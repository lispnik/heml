;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Folding: lines put out of sight under the line they belong to -- a
;;; function's body under its first line -- until they are wanted.
;;;
;;; A folded line is hidden: its plist says so, and a window's image goes
;;; from the line before a fold to the line after it (winimage.lisp).
;;; Nothing else knows of folds: the lines are in the buffer as they were,
;;; and are written, searched and counted as ever.  The line before a fold,
;;; its header, says after its end how many lines are under it.
;;;
;;; What can be folded is what the mode's "Fold Ranges Function" says: a
;;; language server's folding ranges where there is one (lsp-features.lisp),
;;; and otherwise each line with the more indented lines after it.

(in-package :heml)

(defhvar "Fold Ranges Function"
  "A function of a buffer that returns what can be folded in it, as
   ((FIRST . LAST) ...), lines numbered from 0: FIRST stays shown and the
   lines after it as far as LAST are hidden.  NIL folds by indentation."
  :value nil)

(defparameter *fold-font* '(:fg 8 :italic t)
  "The font of what a fold's header says is under it.")


;;;; What can be folded.

(defun indentation-fold-ranges (buffer)
  "Each line of BUFFER with more indented lines after it, and the last of
   them: ((FIRST . LAST) ...), lines numbered from 0."
  (let ((ranges '())
        (open '())                      ; ((INDENTATION . LINE) ...), innermost first
        (last-text nil)
        (number 0))
    (flet ((close-to (indentation)
             (loop while (and open (>= (car (first open)) indentation))
                   do (let ((start (cdr (pop open))))
                        (when (and last-text (> last-text start))
                          (push (cons start last-text) ranges))))))
      (do ((line (mark-line (buffer-start-mark buffer)) (line-next line)))
          ((null line))
        (let* ((string (line-string line))
               (indentation (position-if-not (lambda (char) (member char '(#\Space #\Tab)))
                                             string)))
          (when indentation
            (close-to indentation)
            (push (cons indentation number) open)
            (setf last-text number)))
        (incf number))
      (close-to 0))
    (sort ranges #'< :key #'car)))

(defun fold-ranges (buffer)
  "What can be folded in BUFFER, the current buffer."
  (let ((function (value fold-ranges-function)))
    (or (and function (ignore-errors (funcall function buffer)))
        (indentation-fold-ranges buffer))))


;;;; Hiding and showing.

(defun fold-header-p (line)
  "Whether LINE has a fold under it."
  (let ((next (line-next line)))
    (and next (hi:line-hidden-p next) (not (hi:line-hidden-p line)))))

(defun folded-lines (header)
  "How many lines are hidden under HEADER."
  (loop for line = (line-next header) then (line-next line)
        while (and line (hi:line-hidden-p line))
        count t))

(defun buffer-line (buffer number)
  "BUFFER's line NUMBER, from 0, or NIL."
  (with-mark ((mark (buffer-start-mark buffer)))
    (and (line-offset mark number 0) (mark-line mark))))

(defun closing-line-p (line)
  "Whether LINE holds nothing but what closes something: a brace, a bracket,
   a parenthesis, and a semicolon or comma after them."
  (let ((text (string-trim '(#\Space #\Tab) (line-string line))))
    (and (plusp (length text))
         (find (char text 0) "}])")
         (every (lambda (char) (find char "}]);, ")) text))))

(defun fold-last (buffer first last)
  "Where a fold of BUFFER's lines after FIRST as far as LAST ends: with the
   line after LAST too when it holds only what closes the fold, as a C
   function's closing brace does, so that the fold reads as one line."
  (declare (ignore first))
  (let ((after (buffer-line buffer (1+ last))))
    (if (and after (closing-line-p after)) (1+ last) last)))

(defun hide-lines (buffer first last)
  "Fold BUFFER's lines after FIRST as far as LAST, numbered from 0."
  (let ((line (buffer-line buffer first)))
    (when line
      (loop repeat (- last first)
            do (setf line (line-next line))
               (unless line (return))
               (setf (getf (line-plist line) 'hi::hidden) t)))))

(defun unfold-under (header)
  "Show the lines hidden under HEADER."
  (loop for line = (line-next header) then (line-next line)
        while (and line (hi:line-hidden-p line))
        do (remf (line-plist line) 'hi::hidden)))

(defun fold-header (line)
  "The shown line that LINE, hidden, is under."
  (loop for previous = (line-previous line) then (line-previous previous)
        while previous
        unless (hi:line-hidden-p previous) return previous))

(defun fold-annotation (line)
  "How many lines are folded under LINE, and what closes them, when that is
   folded too."
  (when (fold-header-p line)
    (let* ((count (folded-lines line))
           (last (let ((next line))
                   (dotimes (i count next)
                     (setf next (line-next next)))))
           (closing (and last (closing-line-p last) (> count 1))))
      (cons (format nil "... ~D line~:P~@[ ~A~]" (if closing (1- count) count)
                    (and closing (string-trim '(#\Space #\Tab) (line-string last))))
            *fold-font*))))

(pushnew 'fold-annotation hi:*line-annotation-functions*)


;;;; Point is never left where it cannot be seen.

(defvar *last-point-line* nil
  "The line point was on when the last command finished.")

(defun keep-point-shown ()
  "After a command: point on a hidden line goes to the far side of the
   fold if it came from the line next to it, as C-n and C-p do, and
   otherwise -- a search found something there -- the fold is opened."
  (let* ((point (current-point))
         (line (mark-line point)))
    (when (hi:line-hidden-p line)
      (let ((header (fold-header line))
            (after (hi:next-shown-line line)))
        (cond ((null header)
               (remf (line-plist line) 'hi::hidden))
              ((and (eq *last-point-line* header) after)
               (line-start point after))
              ((eq *last-point-line* after)
               (line-end point header))
              (t (unfold-under header)))))
    (setf *last-point-line* (mark-line point))))

(add-hook after-command-hook 'keep-point-shown)


;;;; Commands.

(defcommand "Toggle Fold" (p)
  "Fold what starts on this line, or else what this line is in, under its
   first line; on a line with a fold under it, open the fold.  What can be
   folded is what the language server says, or else a line and the more
   indented lines after it."
  "Fold what is at point, or open the fold there."
  (declare (ignore p))
  (let* ((buffer (current-buffer))
         (point (current-point))
         (line (mark-line point)))
    (cond ((fold-header-p line)
           (unfold-under line))
          (t
           (let* ((number (1- (count-lines (region (buffer-start-mark buffer) point))))
                  (ranges (fold-ranges buffer))
                  (range (or
                          ;; What starts here: the most of it.
                          (first (sort (remove-if-not (lambda (range) (eql (car range) number))
                                                      ranges)
                                       #'> :key #'cdr))
                          ;; What this is in: the least.
                          (first (sort (remove-if-not (lambda (range)
                                                        (< (car range) number (1+ (cdr range))))
                                                      ranges)
                                       #'> :key #'car)))))
             (unless range (editor-error "Nothing to fold here."))
             (hide-lines buffer (car range) (fold-last buffer (car range) (cdr range)))
             (when (hi:line-hidden-p (mark-line point))
               (line-end point (buffer-line buffer (car range)))))))
    (setf *last-point-line* (mark-line point))))

(defcommand "Fold All" (p)
  "Fold everything in this buffer that is not within something else that
   can be folded: each function's body, under its first line."
  "Fold this buffer's outermost foldable things."
  (declare (ignore p))
  (let* ((buffer (current-buffer))
         (point (current-point))
         (end -1)
         (count 0))
    (dolist (range (sort (copy-list (fold-ranges buffer)) #'< :key #'car))
      (when (> (car range) end)
        (let ((last (fold-last buffer (car range) (cdr range))))
          (hide-lines buffer (car range) last)
          (setf end last))
        (incf count)))
    (when (hi:line-hidden-p (mark-line point))
      (line-end point (fold-header (mark-line point))))
    (setf *last-point-line* (mark-line point))
    (message "~D fold~:P." count)))

(defcommand "Unfold All" (p)
  "Open every fold in this buffer."
  "Open every fold in this buffer."
  (declare (ignore p))
  (do ((line (mark-line (buffer-start-mark (current-buffer))) (line-next line)))
      ((null line))
    (remf (line-plist line) 'hi::hidden)))
