;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; What a language server finds wrong (lsp.lisp): kept at marks in each
;;; buffer, from all of its servers; counted in the modeline; underlined;
;;; and gone through with M-n and M-p.

(in-package :heml)


;;;; Errors and warnings.

(defvar *buffer-diagnostics* (make-hash-table :test 'eq :weakness :key)
  "Buffer to ((START-MARK END-MARK SEVERITY MESSAGE DIAGNOSTIC SERVER) ...):
   what its servers say is wrong in it, at marks, so that each stays with
   its text, as the server said it, and the server that did.")

(defparameter *diagnostic-fonts*
  '((1 . (:fg 1 :underline t))          ; an error
    (2 . (:fg 3 :underline t))          ; a warning
    (3 . (:underline t))                ; information
    (4 . (:underline t))))              ; a hint

(defun clear-buffer-diagnostics (buffer)
  (loop for (start end) in (gethash buffer *buffer-diagnostics*)
        do (delete-mark start) (delete-mark end))
  (remhash buffer *buffer-diagnostics*))

(defun file-buffer (file)
  (find file *buffer-list*
        :key (lambda (buffer) (let ((p (buffer-pathname buffer))) (and p (namestring p))))
        :test #'equal))

(defun lsp-note-diagnostics (server uri diagnostics &optional pulled)
  "SERVER says that DIAGNOSTICS, the protocol's, are what is wrong with the
   file at URI; or, if PULLED, answers that they are.  Each replaces what it
   last said or answered, and what is wrong is both."
  (let* ((file (uri-file (or uri "")))
         (buffer (and file (file-buffer file))))
    (when file
      (setf (gethash file (if pulled (lsp-server-pulled server) (lsp-server-pushed server)))
            diagnostics)
      (setf (gethash file (lsp-server-diagnostics server))
            (loop for diagnostic in (append (gethash file (lsp-server-pushed server))
                                            (gethash file (lsp-server-pulled server)))
                  collect (list (jref diagnostic "range" "start" "line")
                                (jref diagnostic "range" "start" "character")
                                (or (jref diagnostic "severity") 1)
                                (or (jref diagnostic "message") ""))))
      (when buffer
        (rebuild-buffer-diagnostics buffer)
        (update-lsp-modeline buffer))
      (incf hi:*decoration-tick*))))

(defun rebuild-buffer-diagnostics (buffer)
  "Make BUFFER's diagnostics again from what each of the servers running
   says of its file: a buffer may have several, and what is wrong is what
   any of them finds."
  (let ((file (let ((pathname (buffer-pathname buffer))) (and pathname (namestring pathname))))
        (lines nil))
    (clear-buffer-diagnostics buffer)
    (flet ((place (number character kind)
             ;; A mark at the protocol's position.  A line is found by its
             ;; number in a vector of the buffer's lines, made once: a long
             ;; file may have thousands of things wrong with it, and
             ;; counting to each from the start would take as long as the
             ;; square of its length.
             (unless lines
               (setf lines (coerce (loop for line = (mark-line (buffer-start-mark buffer))
                                           then (line-next line)
                                         while line collect line)
                                   'vector)))
             (let* ((past (and number (>= number (length lines))))
                    (line (aref lines (max 0 (min (or number 0) (1- (length lines))))))
                    (string (line-string line)))
               (mark line
                     (if past
                         (length string)
                         (min (length string) (unit-charpos string (or character 0))))
                     kind))))
      (when file
        (let ((all (loop for server in *lsp-servers*
                         append
                         (let ((*encoding* (lsp-server-encoding server)))
                           (loop for diagnostic in (append (gethash file (lsp-server-pushed server))
                                                           (gethash file (lsp-server-pulled server)))
                                 collect
                                 (list (place (jref diagnostic "range" "start" "line")
                                              (jref diagnostic "range" "start" "character")
                                              :right-inserting)
                                       (place (jref diagnostic "range" "end" "line")
                                              (jref diagnostic "range" "end" "character")
                                              :left-inserting)
                                       (or (jref diagnostic "severity") 1)
                                       (or (jref diagnostic "message") "")
                                       diagnostic
                                       server))))))
          (when all
            (setf (gethash buffer *buffer-diagnostics*) all)))))))

;;; The modeline says what the server finds: nothing when there is no
;;; server, how many errors and warnings when there are any.

(defun buffer-diagnostic-counts (buffer)
  (let ((errors 0) (warnings 0))
    (loop for (nil nil severity) in (gethash buffer *buffer-diagnostics*)
          do (case severity (1 (incf errors)) (2 (incf warnings))))
    (values errors warnings)))

(make-modeline-field
 :name :lsp
 :function (lambda (buffer window)
             (declare (ignore window))
             (let ((server (ignore-errors (buffer-language-server buffer))))
               (cond ((null server)
                      (if (ignore-errors (buffer-server-failed-p buffer))
                          "(no server)  "
                          ""))
                     ((eq (lsp-server-state server) :starting) "(server starting)  ")
                     (t
                      (multiple-value-bind (errors warnings) (buffer-diagnostic-counts buffer)
                        (format nil "~@[(~A)  ~]~:[(~[~:;~:*~D error~:P~]~:[~; ~]~[~:;~:*~D warning~:P~])  ~;~]"
                                (lsp-progress-text server)
                                (and (zerop errors) (zerop warnings))
                                errors (and (plusp errors) (plusp warnings)) warnings)))))))

(unless (member :lsp hi::*default-modeline-fields* :key #'modeline-field-name)
  (let ((project (member :project hi::*default-modeline-fields* :key #'modeline-field-name)))
    (if project
        (push (modeline-field :lsp) (cdr project))
        (nconc hi::*default-modeline-fields* (list (modeline-field :lsp))))))

(defun update-lsp-modeline (buffer)
  (when (buffer-modeline-field-p buffer :lsp)
    (dolist (window (buffer-windows buffer))
      (ignore-errors (update-modeline-field buffer window (modeline-field :lsp))))))

(defun lsp-line-decorations (line)
  "What the server says is wrong on LINE, as ((START END FONT) ...)."
  (let ((buffer (line-buffer line)))
    (when buffer
      (loop for (start end severity) in (gethash buffer *buffer-diagnostics*)
            for start-line = (mark-line start)
            for end-line = (mark-line end)
            when (and (eq (line-buffer start-line) buffer)
                      (eq (line-buffer end-line) buffer)
                      (line<= start-line line) (line<= line end-line))
              collect (let* ((length (line-length line))
                             (from (if (eq line start-line) (mark-charpos start) 0))
                             (to (if (eq line end-line) (mark-charpos end) length)))
                        ;; A place with no width is the character there.
                        (when (<= to from) (setf to (1+ from)))
                        (list (min from (max 0 (1- length))) (min to (max length 1))
                              (or (cdr (assoc severity *diagnostic-fonts*))
                                  '(:underline t))))))))

(pushnew 'lsp-line-decorations hi:*line-decoration-functions*)

(defun diagnostic-at-mark (mark)
  "What the server says is wrong at MARK, or on its line."
  (let ((diagnostics (gethash (line-buffer (mark-line mark)) *buffer-diagnostics*)))
    (fourth (or (find-if (lambda (d) (and (mark<= (first d) mark) (mark<= mark (second d))))
                         diagnostics)
                (find (mark-line mark) diagnostics :key (lambda (d) (mark-line (first d))))))))


;;;; From one error to the next.

(defun move-to-diagnostic (direction)
  (let* ((point (current-point))
         (diagnostics (sort (copy-list (gethash (current-buffer) *buffer-diagnostics*))
                            #'mark< :key #'first))
         (next (if (plusp direction)
                   (find-if (lambda (d) (mark> (first d) point)) diagnostics)
                   (find-if (lambda (d) (mark< (first d) point)) diagnostics :from-end t))))
    (unless diagnostics (editor-error "Nothing is wrong here, that the server says."))
    ;; Past the last, the first; before the first, the last.
    (unless next
      (setf next (if (plusp direction) (first diagnostics) (first (last diagnostics)))))
    (move-mark point (first next))
    (message "~A" (substitute #\Space #\Newline (fourth next)))))

(defcommand "LSP Next Diagnostic" (p)
  "Go to the next thing the language server finds wrong in this buffer,
   and say what it is; after the last, the first."
  "Go to the next error or warning."
  (declare (ignore p))
  (move-to-diagnostic 1))

(defcommand "LSP Previous Diagnostic" (p)
  "Go to the previous thing the language server finds wrong in this buffer,
   and say what it is; before the first, the last."
  "Go to the previous error or warning."
  (declare (ignore p))
  (move-to-diagnostic -1))
