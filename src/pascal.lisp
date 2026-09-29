;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;; Just barely enough to be a Pascal/C mode.  Maybe more some day.
;;;
(in-package :heml)

(defmode "Pascal" :major-p t)
(defcommand "Pascal Mode" (p)
  "Put the current buffer into \"Pascal\" mode."
  "Put the current buffer into \"Pascal\" mode."
  (declare (ignore p))
  (setf (buffer-major-mode (current-buffer)) "Pascal"))

(defhvar "Indent Function"
  "Indentation function which is invoked by \"Indent\" command.
   It must take one argument that is the prefix argument."
  :value #'generic-indent
  :mode "Pascal")

(defhvar "Auto Fill Space Indent"
  "When non-nil, uses \"Indent New Comment Line\" to break lines instead of
   \"New Line\"."
  :mode "Pascal" :value t)

(defhvar "Comment Start"
  "String that indicates the start of a comment."
  :mode "Pascal" :value "(*")

(defhvar "Comment End"
  "String that ends comments.  Nil indicates #\newline termination."
  :mode "Pascal" :value " *)")

(defhvar "Comment Begin"
  "String that is inserted to begin a comment."
  :mode "Pascal" :value "(* ")

;;;; Closing brackets show what they close.

(defattribute "Bracket Syntax"
  "The brackets \"Insert Close Bracket\" matches: :OPEN for ( [ { and
   :CLOSE for ) ] }."
  'symbol nil)

(dolist (char '(#\( #\[ #\{))
  (setf (character-attribute :bracket-syntax char) :open))
(dolist (char '(#\) #\] #\}))
  (setf (character-attribute :bracket-syntax char) :close))

(defun opening-bracket (close)
  (ecase close (#\) #\() (#\] #\[) (#\} #\{)))

;;; Move MARK, which is just after a closing bracket, to just before the
;;; bracket it closes.  NIL when there is none.
;;;
(defun balance-bracket (mark)
  (with-mark ((m mark))
    (mark-before m)
    (let ((close (next-character m))
          (depth 1))
      (loop
        (unless (rev-scan-char m :bracket-syntax (or :open :close))
          (return nil))
        (if (test-char (previous-character m) :bracket-syntax :open)
            (decf depth)
            (incf depth))
        (when (zerop depth)
          (unless (char= (previous-character m) (opening-bracket close))
            (editor-error "Mismatched bracket."))
          (mark-before (move-mark mark m))
          (return t))
        (mark-before m)))))

(defcommand "Insert Close Bracket" (p)
  "Insert the closing bracket typed, and show the bracket it closes."
  "Insert the closing bracket typed, and show the bracket it closes."
  (declare (ignore p))
  (let ((point (current-point)))
    (insert-character point (heml-ext:key-event-char *last-key-event-typed*))
    (with-mark ((m point))
      (if (balance-bracket m)
          (when (value paren-pause-period)
            (unless (show-mark m (current-window) (value paren-pause-period))
              (clear-echo-area)
              (message "~A" (line-string (mark-line m)))))
          (editor-error)))))
