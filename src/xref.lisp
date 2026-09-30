;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;; This file contains Xref code, for M-. and other commands.
;;;

(in-package :heml)



(defvar *xref-entries* nil)
(defvar *xref-entries-end* nil)
;;;

(defstruct (xref-entry
             (:constructor internal-make-xref-entry (name file position)))
  name
  file
  position)

(defun make-xref-entry (alist)
  (let* ((location (cdr (assoc :location (cdr alist))))
         (file (second (assoc :file location)))
         (position (second (assoc :position location))))
    (internal-make-xref-entry (car alist)
                              file
                              position)))

;;; This is the xref buffer if it exists.
;;;
(defvar *xref-buffer* nil)

;;; This is the cleanup method for deleting *xref-buffer*.
;;;
(defun delete-xref-buffers (buffer)
  (when (eq buffer *xref-buffer*)
    (setf *xref-buffer* nil)
    (setf *xref-entries* nil)))


;;;; Commands.

(defmode "Xref" :major-p t
  :documentation
  "Xref lists Lisp definitions, and the places that call, reference, bind,
   set or expand a name, grouped by file.  Return visits one, n and p show
   the next and previous, and C-x ` visits the next from anywhere.")

(defcommand "Xref Quit" (p)
  "Kill the xref buffer."
  ""
  (declare (ignore p))
  (when *xref-buffer* (delete-buffer-if-possible *xref-buffer*)))

(defcommand "Xref Goto" (p)
  "Change to the entry's buffer."
  "Change to the entry's buffer."
  (result-goto-command p))

(defun xref-location (entry)
  (let ((file (xref-entry-file entry))
        (position (xref-entry-position entry)))
    (when file
      (list (pathname file) :position (or position 1)))))

(defun refresh-xref (buf entries &optional title)
  (setf *xref-entries-end* (length entries))
  (setf *xref-entries* (coerce entries 'vector))
  (with-writable-buffer (buf)
    (delete-region (buffer-region buf))
    (let ((point (buffer-point buf))
          (groups '()))
      (dolist (entry entries)
        (let ((group (assoc (xref-entry-file entry) groups :test #'equal)))
          (if group
              (push entry (cdr group))
              (push (list (xref-entry-file entry) entry) groups))))
      (when title
        (insert-string point (format nil "~A~%" title)))
      (loop for (file . group) in (reverse groups)
            do (insert-string point (format nil "~%~A~%" (or file "(no file)")))
               (dolist (entry (reverse group))
                 (let ((line (mark-line point)))
                   (insert-string point (format nil "  ~A~%" (xref-entry-name entry)))
                   (setf (getf (line-plist line) 'result-location)
                         (xref-location entry))))))))

(defun xref-line-fonts (string)
  (cond ((zerop (length string)) '())
        ((char= (char string 0) #\Space) '())
        ((or (char= (char string 0) #\/) (search "(no file)" string))
         (list (cons 0 '(:fg 5 :bold t))))
        (t (list (cons 0 '(:bold t))))))

(defun xref-highlight-line (line)
  (let ((old (getf (line-plist line) 'xref-marks)))
    (unless (and old (eq (car old) (line-signature line)))
      (dolist (mark (cdr old))
        (hi::delete-font-mark mark))
      (setf (getf (line-plist line) 'xref-marks)
            (cons (line-signature line)
                  (loop for (position . font) in (xref-line-fonts (line-string line))
                        collect (hi::font-mark line position font)))))))

(define-mode-highlighter "Xref" 'xref-highlight-line)

(defun make-xref-buffer (entries &optional title)
  (let ((buf (make-result-buffer "*Xref*" "Xref" 'plist-line-location)))
    (setf *xref-buffer* buf)
    (pushnew 'delete-xref-buffers (buffer-delete-hook buf))
    (refresh-xref buf entries title)
    (buffer-start (buffer-point buf))
    (change-to-buffer buf)
    (next-result-line (current-point) 1)))

(defun xref-write-line (entry s)
  (format s "  ~A ~40T~A~%"
          (shorten-string 36 (xref-entry-name entry))
          (shorten-string 39 (xref-entry-file entry))))

(defun shorten-string (len str)
  (if (<= (length str) len)
      str
      (concat (subseq str 0 (floor (- len 3) 2))
              "..."
              (subseq str (- (length str) (ceiling (- len 3) 2))))))

(defcommand "Xref Help" (p)
  "Show this help."
  "Show this help."
  (declare (ignore p))
  (describe-mode-command nil "Xref"))


;;; Find Definition

(defun change-to-definition (entry)
  (let ((file (xref-entry-file entry))
        (position (xref-entry-position entry)))
    (when file
      (change-to-buffer (find-file-buffer file))
      (when position
        (buffer-start (current-point))
        (character-offset (current-point) (1- position)))
      t)))

(defun %find-definitions (label xref-fun name)
  ;; LABEL is "definition", or the command's name ("Who Calls").
  (let* ((sym (heml::resolve-slave-symbol name nil))
         (data
          (and sym
               (mapcar (lambda (def)
                         (cons (princ-to-string (car def))
                               (cdr def)))
                       (funcall xref-fun sym)))))
    (heml::eval-in-master `(%definitions-found ',label ',name ',data))))

(defun %definitions-found (label name data)
  (let ((entries (mapcar #'make-xref-entry data)))
    (cond
     ((null entries)
      (message "No ~A results for: ~A" label name))
     ((null (cdr entries))
      (change-to-definition (car entries)))
     (t
      (make-xref-buffer entries
                        (if (string-equal label "definition")
                            (format nil "Definitions of ~A" name)
                            (format nil "~A ~A" (string-downcase label :start 1)
                                    name)))))))

(defun find-definitions (name)
  (heml::eval-in-slave
   `(%find-definitions "definition" 'conium:find-definitions ',name)))

(defcommand "Find Definitions" (p)
  "" ""
  (let ((default (heml::symbol-string-at-point)))
    ;; Fixme: MARK-SYMBOL isn't very good, meaning that often we
    ;; will get random forms rather than a symbol.  Let's at least
    ;; catch the case where the result is more than a line long,
    ;; and give up.
    (when (find #\newline default)
      (setf default nil))
    (find-definitions
     (heml::parse-slave-symbol
      (if (or p (not default))
          (heml-interface::prompt-for-string
           :prompt "Name: "
           :default default)
          default)))))

(macrolet
    ((% (name fun conium-fun)
       `(progn
          (defcommand ,name (p)
            "" ""
            (let ((default (heml::symbol-string-at-point)))
              ;; Fixme: MARK-SYMBOL isn't very good, meaning that often we
              ;; will get random forms rather than a symbol.  Let's at least
              ;; catch the case where the result is more than a line long,
              ;; and give up.
              (when (find #\newline default)
                (setf default nil))
              (,fun
               (heml::parse-slave-symbol
                (if (or p (not default))
                    (heml-interface::prompt-for-string
                     :prompt "Name: "
                     :default default)
                    default)))))
          (defun ,fun (name)
            (heml::eval-in-slave
             (list '%find-definitions
                   (list 'quote ',name)
                   (list 'quote ',conium-fun)
                   (list 'quote name)))))))
  (% "Who Calls"        who-calls        conium:who-calls)
  (% "Who References"   who-references   conium:who-references)
  (% "Who Binds"        who-binds        conium:who-binds)
  (% "Who Sets"         who-sets         conium:who-sets)
  (% "Who Macroexpands" who-macroexpands conium:who-macroexpands)
  (% "Who Specializes"  who-specializes  conium:who-specializes))
