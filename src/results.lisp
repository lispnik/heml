;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Result lists: buffers that list places, one a line -- Grep's matches, a
;;; compiler's errors, Xref's definitions.  A result buffer has a buffer
;;; variable "Result Location Function", a function of a line that returns
;;; the place the line names, or NIL for a line that names none (a header).
;;; A place is a list (PATHNAME LINE COLUMN), or (PATHNAME :POSITION N) for a
;;; character offset; LINE, COLUMN and N count from 1, and COLUMN may be NIL.
;;;
;;; Return visits the place on the line, in the other window; n and p move to
;;; the next and previous result and show it there; C-x ` moves to the next
;;; result of the result buffer made last, from wherever one is.

(in-package :heml)

(defmode "Outline" :major-p t
  :documentation
  "A buffer's headings or definitions, a line each.  Return visits one, n
   and p show the next and previous, and q quits.")

(defvar *result-buffer* nil
  "The result list \"Next Result\" moves in: the last one made.")

(defun make-result-buffer (name mode location-function)
  "Make, or empty and reuse, the result buffer NAME, in MODE, whose lines'
   places LOCATION-FUNCTION finds."
  (let ((buffer (or (getstring name *buffer-names*)
                    (make-buffer name :modes (list mode)
                                      :delete-hook (list 'forget-result-buffer)))))
    (setf (buffer-major-mode buffer) mode)
    (unless (heml-bound-p 'result-location-function :buffer buffer)
      (defhvar "Result Location Function"
        "A function of a line that returns the place it names, or NIL."
        :buffer buffer))
    (setf (variable-value 'result-location-function :buffer buffer)
          location-function)
    (with-writable-buffer (buffer)
      (delete-region (buffer-region buffer)))
    (setf (buffer-writable buffer) nil)
    (setf *result-buffer* buffer)
    buffer))

;;; A list made once, which cannot change, can keep each line's place in the
;;; line's plist.
;;;
(defun plist-line-location (line)
  (getf (line-plist line) 'result-location))

(defun forget-result-buffer (buffer)
  (when (eq buffer *result-buffer*)
    (setf *result-buffer* nil)))

(defun result-buffer-p (buffer)
  (heml-bound-p 'result-location-function :buffer buffer))

(defun line-location (line)
  "The place LINE names, if it is a result buffer's line that names one."
  (let ((buffer (line-buffer line)))
    (when (and buffer (result-buffer-p buffer))
      (let ((function (variable-value 'result-location-function :buffer buffer)))
        (when function (funcall function line))))))

(defun result-location-at-point ()
  (or (line-location (mark-line (current-point)))
      (editor-error "No result on this line.")))

(defun other-window ()
  "The window after the current one, made by splitting this one if it is the
   only one."
  (if (> (length (remove *echo-area-window* *window-list*)) 1)
      (next-window (current-window))
      (or (make-window (window-display-start (current-window)))
          (editor-error "No room for another window."))))

(defun select-window (window)
  "Make WINDOW current, and its buffer the current buffer, as going to it
   does: setting the current window alone leaves the buffer as it was."
  (setf (current-buffer) (window-buffer window)
        (current-window) window))

(defun visit-location (location)
  "Make the current window show LOCATION's file with point at LOCATION."
  (destructuring-bind (pathname line &optional column) location
    (setf pathname (pathname pathname))
    (unless (probe-file pathname)
      (editor-error "No file ~A." (namestring pathname)))
    (change-to-buffer (find-file-buffer pathname))
    (let ((point (current-point)))
      (buffer-start point)
      (cond ((eq line :position)
             (character-offset point (1- column)))
            (t
             (unless (line-offset point (1- line))
               (buffer-end point))
             (when column
               (character-offset point (min (1- column)
                                            (line-length (mark-line point))))))))))

(defun show-location (location &key select)
  "Show LOCATION in the other window, going there when SELECT."
  (let ((here (current-window)))
    (select-window (other-window))
    (visit-location location)
    (unless select
      (select-window here))))

(defun next-result-line (mark count)
  "Move MARK COUNT results on (back when negative), to a line that names a
   place, and return it; NIL, leaving MARK, when there are not so many."
  (let ((line (mark-line mark))
        (step (if (minusp count) #'line-previous #'line-next)))
    (dotimes (i (abs count))
      (loop (setf line (funcall step line))
            (when (null line) (return-from next-result-line nil))
            (when (line-location line) (return))))
    (move-to-position mark 0 line)))

(defcommand "Result Goto" (p)
  "Visit the place on this line, in the other window."
  "Visit the place on this line, in the other window."
  (declare (ignore p))
  (setf *result-buffer* (current-buffer))
  (show-location (result-location-at-point) :select t))

(defcommand "Result Display" (p)
  "Show the place on this line in the other window, staying here."
  "Show the place on this line in the other window."
  (declare (ignore p))
  (setf *result-buffer* (current-buffer))
  (show-location (result-location-at-point)))

(defcommand "Next Result Line" (p)
  "Move to the next result, and show its place in the other window."
  "Move to the next result, and show it."
  (unless (next-result-line (current-point) (or p 1))
    (editor-error "No more results."))
  (result-display-command nil))

(defcommand "Previous Result Line" (p)
  "Move to the previous result, and show its place in the other window."
  "Move to the previous result, and show it."
  (unless (next-result-line (current-point) (- (or p 1)))
    (editor-error "No earlier results."))
  (result-display-command nil))

(defun result-window (buffer)
  (find buffer (remove *echo-area-window* *window-list*) :key #'window-buffer))

(defcommand "Next Result" (p)
  "Visit the next place in the last result list -- the next match of a
   grep, the next error of a compilation -- with a prefix argument, that
   many on, or back when it is negative.  The list stays in its window."
  "Visit the next place in the last result list."
  (let ((buffer *result-buffer*))
    (unless (and buffer (member buffer *buffer-list*))
      (editor-error "No result list."))
    (let* ((window (result-window buffer))
           (mark (if window (window-point window) (buffer-point buffer))))
      (unless (next-result-line mark (or p 1))
        (editor-error "No more results."))
      (move-mark (buffer-point buffer) mark)
      (let ((location (line-location (mark-line mark))))
        (cond ((eq window (current-window))
               (show-location location :select t))
              (t
               (visit-location location)))))))

(defcommand "Previous Result" (p)
  "Visit the previous place in the last result list."
  "Visit the previous place in the last result list."
  (next-result-command (- (or p 1))))

(defcommand "Result Quit" (p)
  "Bury this result list, showing another buffer in its place."
  "Bury this result list."
  (declare (ignore p))
  (let ((buffer (current-buffer)))
    (change-to-buffer (or (find-if (lambda (b) (and (not (eq b buffer))
                                                    (not (eq b *echo-area-buffer*))))
                                   *buffer-history*)
                          buffer))))
