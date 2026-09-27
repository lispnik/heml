;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :hi)

;;; Terminal hunks.  Their geometry is DEVICE-HUNK's, set by layout.lisp.
;;;
(defclass tty-hunk (device-hunk) ())

(defun make-tty-hunk (&rest initargs)
  (apply #'make-instance 'tty-hunk initargs))

(defmethod device-make-hunk ((device tty-device))
  (make-tty-hunk :device device))
