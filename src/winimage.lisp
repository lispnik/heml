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
         (width (window-width window))
         (start (window-display-start window))
         (line (mark-line start))
         (offset (mark-charpos start))
         (trail first)
         string underhang)
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
        (multiple-value-setq (string underhang offset)
          (compute-line-image string underhang line offset dis-line width))
        (unless underhang
          (setq line (line-next line)
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
