;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :heml.wire)

(defun unix-gethostid ()
  #.(or
     398792))

(defun unix-getpid ()
  (conium:getpid))

;; fixme: remove this?
(push (cons '*print-readably* nil)
      bt:*default-special-bindings*)
