;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Slave debugging

(in-package :heml)


(defvar *slave-stack-frames* nil)
(defvar *slave-stack-frames-end* nil)
;;;

(defstruct (slave-stack-frame
             (:constructor make-slave-stack-frame
                 (label remote-frame &optional locals location)))
  label
  remote-frame
  ;; The frame's local variables, as ((NAME . PRINTED-VALUE) ...), and its
  ;; source, as a place (results.lisp) or NIL: both gathered in the slave
  ;; while it is in the debugger, since they cannot be asked for later.
  locals
  location
  (expanded nil))


;;; This is the debug buffer if it exists.
;;;
(defvar *debug-buffer* nil)

;;; This is the cleanup method for deleting *debug-buffer*.
;;;
(defun delete-debug-buffers (buffer)
  (when (eq buffer *debug-buffer*)
    (setf *debug-buffer* nil)
    (setf *slave-stack-frames* nil)))


;;;; Commands.

(defmode "Debug" :major-p t
  :documentation "Debug mode shows a slave's stack, a frame a line.  Return
   shows or hides a frame's local variables, . visits its source, n and p
   move between frames, and q quits.")

(defcommand "Debug Quit" (p)
  "Kill the debug buffer."
  ""
  (declare (ignore p))
  (when *debug-buffer* (delete-buffer-if-possible *debug-buffer*)))

;;; Each of a frame's lines -- its own, and its locals' when they are shown
;;; -- keeps the frame in its plist.
;;;
(defun slave-stack-frame-from-mark (mark)
  (or (getf (line-plist (mark-line mark)) 'stack-frame)
      (editor-error "No frame on this line.")))

(defun frame-line-p (line)
  (and (getf (line-plist line) 'stack-frame)
       (not (getf (line-plist line) 'frame-local))))

(defun frame-line (mark)
  "The line of the frame MARK is in."
  (let ((line (mark-line mark)))
    (loop while (and line (getf (line-plist line) 'frame-local))
          do (setf line (line-previous line)))
    line))

(defun insert-frame-lines (mark index frame)
  "Insert FRAME's line, and its locals' when it is expanded, at MARK."
  (flet ((out (local control &rest args)
           (let ((line (mark-line mark)))
             (insert-string mark (apply #'format nil control args))
             (setf (getf (line-plist line) 'stack-frame) frame)
             (when local (setf (getf (line-plist line) 'frame-local) t)))))
    (out nil "~3D: ~A~%" index (slave-stack-frame-label frame))
    (when (slave-stack-frame-expanded frame)
      (if (slave-stack-frame-locals frame)
          (loop for (name . value) in (slave-stack-frame-locals frame)
                do (out t "       ~A = ~A~%" name value))
          (out t "       (no locals)~%")))))

(defun refresh-debug (buf entries)
  (with-writable-buffer (buf)
    (delete-region (buffer-region buf))
    (setf *slave-stack-frames-end* (length entries))
    (setf *slave-stack-frames* (coerce entries 'vector))
    (let ((point (buffer-point buf)))
      (loop for entry in entries
            for i from 0
            do (insert-frame-lines point i entry)))))

(defcommand "Debug Toggle Locals" (p)
  "Show the local variables of the frame on this line, or hide them."
  "Show or hide the frame's local variables."
  (declare (ignore p))
  (let* ((line (frame-line (current-point)))
         (frame (and line (getf (line-plist line) 'stack-frame)))
         (index (and frame (position frame *slave-stack-frames*))))
    (unless index (editor-error "No frame on this line."))
    (setf (slave-stack-frame-expanded frame) (not (slave-stack-frame-expanded frame)))
    (with-writable-buffer ((current-buffer))
      (with-mark ((start (mark line 0) :left-inserting)
                  (end (mark line 0)))
        (loop do (unless (line-offset end 1) (buffer-end end) (return))
              while (getf (line-plist (mark-line end)) 'frame-local))
        (delete-region (region start end))
        (insert-frame-lines start index frame)))
    (move-to-position (current-point) 0 line)))

(defcommand "Debug Source" (p)
  "Visit the source of the frame on this line, in the other window."
  "Visit the frame's source."
  (declare (ignore p))
  (let ((location (slave-stack-frame-location (slave-stack-frame-from-mark (current-point)))))
    (unless location (editor-error "The frame's source is not known."))
    (show-location location :select t)))

(defun debug-move (count)
  (let ((line (frame-line (current-point))))
    (dotimes (i (abs count))
      (loop (setf line (if (minusp count) (line-previous line) (line-next line)))
            (unless line (editor-error "No more frames."))
            (when (frame-line-p line) (return))))
    (move-to-position (current-point) 0 line)))

(defcommand "Debug Next Frame" (p)
  "Move to the next frame."
  "Move to the next frame."
  (debug-move (or p 1)))

(defcommand "Debug Previous Frame" (p)
  "Move to the previous frame."
  "Move to the previous frame."
  (debug-move (- (or p 1))))

(defun debug-line-fonts (string)
  (let ((colon (position #\: string)))
    (cond ((zerop (length string)) '())
          ((and colon (< colon 5) (digit-char-p (char string (1- colon))))
           (list (cons 0 2) (cons (1+ colon) 0)
                 (cons (min (length string) (+ colon 2)) '(:bold t))
                 (cons (or (position #\Space string :start (min (length string) (+ colon 3)))
                           (length string))
                       0)))
          (t (let ((equals (search " = " string)))
               (when equals
                 (list (cons 7 6) (cons equals 0))))))))

(defun debug-highlight-line (line)
  (let ((old (getf (line-plist line) 'debug-marks)))
    (unless (and old (eq (car old) (line-signature line)))
      (dolist (mark (cdr old))
        (hi::delete-font-mark mark))
      (setf (getf (line-plist line) 'debug-marks)
            (cons (line-signature line)
                  (loop for (position . font) in (debug-line-fonts (line-string line))
                        collect (hi::font-mark line position font)))))))

(define-mode-highlighter "Debug" 'debug-highlight-line :marks 'debug-marks)

(defvar *debug-context* nil)

(defun make-debug-buffer (context entries impl thread)
  (let ((buf (or *debug-buffer*
                 (make-buffer (format nil "Slave Debugger ~A ~A" impl thread)
                              :modes '("Debug")
                              :delete-hook (list 'delete-debug-buffers)))))
    (setf *debug-buffer* buf)
    (setf *debug-context* context)
    (refresh-debug buf
                   (mapcar (lambda (entry)
                             (destructuring-bind (label remote &optional locals location)
                                 entry
                               (make-slave-stack-frame label remote locals location)))
                           entries))
    (let ((fields (buffer-modeline-fields *debug-buffer*)))
      (unless (member :debug-cmds fields :key #'modeline-field-name)
        (setf (cdr (last fields))
              (list (or (modeline-field :debug-cmds)
                        (make-modeline-field
                         :name :debug-cmds :width 18
                         :function
                         #'(lambda (buffer window)
                             (declare (ignore buffer window))
                             "  Type ? for help.")))))
        (setf (buffer-modeline-fields *debug-buffer*) fields)))
    (buffer-start (buffer-point buf))
    (change-to-buffer buf)))

(defcommand "Debug Help" (p)
  "Show this help."
  "Show this help."
  (declare (ignore p))
  (describe-mode-command nil "Debug"))

(defun frame-details (index)
  "Frame INDEX's locals, printed, and its source, as a place; in the slave."
  (list (ignore-errors
         (loop for local in (conium:frame-locals index)
               collect (cons (princ-to-string (getf local :name))
                             (let ((*print-length* 10) (*print-level* 3))
                               (let ((string (prin1-to-string (getf local :value))))
                                 (if (> (length string) 200)
                                     (concatenate 'string (subseq string 0 200) "...")
                                     string))))))
        (ignore-errors
         (let* ((location (conium:frame-source-location-for-emacs index))
                (file (second (assoc :file (cdr location))))
                (position (second (assoc :position (cdr location)))))
           (when (and (eq (car location) :location) file)
             (list file :position (or position 1)))))))

(defun debug-using-master (&optional (start 0) (end 30))
  (if prepl:*debugging-context*
      (let ((frames
              (loop for frame in (conium:compute-backtrace start end)
                    for index from start
                    collect (list* (with-output-to-string (s)
                                     (conium:print-frame frame s))
                                   (heml.wire:make-remote-object frame)
                                   (frame-details index))))
            (context nil
                     #+nil (heml.wire:make-remote-object
                            prepl:*debugging-context*))
            ;; fixme: show the slave name rather than just the impl type
            (impl (lisp-implementation-type))
            (thread (bordeaux-threads:thread-name
                     (bordeaux-threads:current-thread))))
        (heml::eval-in-master
         `(make-debug-buffer ',context ',frames ',impl ',thread)))
      (prepl:debugger nil nil (lambda () (debug-using-master start end)))))
