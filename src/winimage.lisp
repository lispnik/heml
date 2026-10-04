;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;;    Written by Rob MacLachlan
;;;
;;; This file contains implementation independant functions that
;;; build window images from the buffer structure.
;;;
(in-package :heml-internals)

(defvar the-sentinel
  (list (make-window-dis-line ""))
  "This dis-line, which has several interesting properties, is used to end
  lists of dis-lines.")
(setf (dis-line-line (car the-sentinel))
      (make-line :number most-positive-fixnum :chars ""))
(setf (dis-line-position (car the-sentinel)) most-positive-fixnum)


;;; A popup is a few rows of text drawn over a window's image: a menu of
;;; completions under point (popup.lisp).  It is laid over the dis-lines
;;; each time the image is made, so every device shows it, and it is gone
;;; the next time the image is made without it.
;;;
(defstruct (popup (:constructor make-popup (window x y rows &optional highlights)))
  window                                ; the window it is over
  x y                                   ; its first row's column and line
  rows                                  ; ((TEXT . FONT) ...), from the top
  highlights)                           ; ((ROW START END FONT) ...): parts of rows in another font

(defvar *popup* nil
  "The popup shown, or NIL.")

;;; An annotation is text shown after a line's end that is not the
;;; buffer's: what a language server says of the line.  Each function in
;;; *LINE-ANNOTATION-FUNCTIONS* is called with a line and returns NIL or
;;; (TEXT . FONT); what they return is shown, two columns after the line,
;;; as far as the window's width allows.
;;;
(defvar *line-annotation-functions* '())

(defun annotate-dis-line (dis-line line width)
  (let ((x0 (+ (dis-line-length dis-line) 2)))
    (dolist (function *line-annotation-functions*)
      (let ((annotation (funcall function line)))
        (when annotation
          (let ((x1 (min (1- width) (+ x0 (length (car annotation))))))
            (when (< x0 x1)
              (overlay-dis-line dis-line x0 x1 (car annotation) (cdr annotation))
              (setf x0 (+ x1 2)))))))))

;;; The fringe is a few columns at the left of a window, beside its text
;;; and not in it, as Emacs has: what is drawn there says something of the
;;; line beside it -- a breakpoint, where a program being debugged stopped.
;;; A window has one when its buffer's major mode asks for it (its
;;; MODE-FRINGE-WIDTH), or every window does when *FRINGE-WIDTH* says so.
;;; The window's text is that much narrower: its lines are laid out in the
;;; rest, and the cursor and a click are placed in it (cursor.lisp).  Each
;;; function in *LINE-FRINGE-FUNCTIONS* is called with a line and returns
;;; ((COLUMN TEXT FONT) ...), drawn in the fringe beside the line's first
;;; row.
;;;
(defvar *line-fringe-functions* '())

(defvar *fringe-width* nil
  "The fringe every window has, in columns, or NIL for each mode's own.")

(defvar *mode-fringe-widths* (make-hash-table :test 'equalp)
  "Major mode's name to the fringe its buffers' windows have.")

(defun mode-fringe-width (mode)
  (gethash mode *mode-fringe-widths* 0))

(defun (setf mode-fringe-width) (width mode)
  (setf (gethash mode *mode-fringe-widths*) width))

(defun window-fringe-width (window)
  "How many columns at the left of WINDOW are its fringe."
  (let ((buffer (window-buffer window)))
    (min (max 0 (1- (window-width window)))
         (or *fringe-width*
             (if buffer (mode-fringe-width (buffer-major-mode buffer)) 0)))))

(defun window-text-width (window)
  "How many columns of WINDOW its text has: those right of its fringe."
  (- (window-width window) (window-fringe-width window)))

(defun fringe-dis-line (dis-line line fringe first-row)
  "Move DIS-LINE's image right of a fringe FRINGE columns wide, and draw in
   the fringe what is said of LINE, if this is LINE's FIRST-ROW."
  (let ((chars (dis-line-chars dis-line))
        (length (dis-line-length dis-line)))
    (replace chars chars :start1 fringe :start2 0 :end2 length)
    (fill chars #\Space :end fringe)
    (setf (dis-line-length dis-line) (+ length fringe))
    (do ((change (dis-line-font-changes dis-line) (font-change-next change)))
        ((null change))
      (incf (font-change-x change) fringe))
    (when (and first-row *line-fringe-functions*)
      (dolist (function *line-fringe-functions*)
        (loop for (column text font) in (funcall function line)
              when (< -1 column fringe)
                do (overlay-dis-line dis-line column
                                     (min fringe (+ column (length text)))
                                     text font))))))

;;; A hidden line is one of a fold's (fold.lisp): the image goes from the
;;; line before it to the next that is shown.
;;;
(declaim (inline line-hidden-p))
(defun line-hidden-p (line)
  (getf (line-plist line) 'hidden))

(defun next-shown-line (line)
  (loop
    (setq line (line-next line))
    (unless (and line (line-hidden-p line))
      (return line))))

;;; update-window-image  --  Internal
;;;
;;;    Rebuild Window's image from its display start.  Every dis-line is
;;; computed afresh each time: nothing is carried over from the last image,
;;; so nothing in it can be stale -- a line's text, its font marks and its
;;; syntax highlighting are all read again.  A window is a few dozen lines,
;;; which is no work at all.
;;;
;;;    The old image's dis-lines go back to the window's spare lines, and
;;; the new one is built from them.  A line too long for the width takes
;;; several dis-lines, and the image may start part way through a line.
;;;
(defun update-window-image (window)
  (let* ((first (window-first-line window))
         (height (window-height window))
         (fringe (window-fringe-width window))
         (width (- (window-width window) fringe))
         (start (window-display-start window))
         (line (mark-line start))
         (offset (mark-charpos start))
         (trail first)
         string underhang)
    ;; An image never starts within a fold.
    (when (and line (line-hidden-p line))
      (setq line (next-shown-line line)
            offset 0))
    (unless (eq (cdr first) the-sentinel)
      (shiftf (cdr (window-last-line window))
              (window-spare-lines window)
              (cdr first)
              the-sentinel))
    (do ((pos 0 (1+ pos)))
        ((or (null line) (= pos height)))
      (let* ((cell (window-spare-lines window))
             (dis-line (car cell)))
        (setf (window-spare-lines window) (cdr cell)
              (cdr cell) the-sentinel
              (cdr trail) cell
              trail cell)
        (setf (dis-line-line dis-line) line
              (dis-line-position dis-line) pos)
        (let ((first-row (and (null string) (zerop offset))))
          (multiple-value-setq (string underhang offset)
            (compute-line-image string underhang line offset dis-line width))
          (setf (dis-line-text-length dis-line) (dis-line-length dis-line))
          (when (plusp fringe)
            (fringe-dis-line dis-line line fringe first-row)))
        (unless underhang
          (when *line-annotation-functions*
            (annotate-dis-line dis-line line (+ width fringe)))
          (setq line (next-shown-line line)
                offset 0))))
    (move-mark (window-old-start window) start)
    (cond ((eq trail first)
           (setf (window-last-line window) the-sentinel)
           (move-mark (window-display-end window) start))
          (t
           (setf (window-last-line window) trail)
           (let ((dis-line (car trail)))
             (move-to-position (window-display-end window)
                               (dis-line-end dis-line)
                               (dis-line-line dis-line)))))
    (when (and *popup* (eq (popup-window *popup*) window))
      (overlay-popup window *popup*))))
