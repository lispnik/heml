;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Lisp Implementation Dependent Stuff for Heml
;;;   Created: 2002-11-07
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2002 by Gilbert Baumann

(in-package :heml-ext)

(defun getenv (name)
  (uiop:getenv name))

(defmacro without-interrupts (&body body)
  `(#+sbcl sb-sys:without-interrupts #+ecl mp:without-interrupts
    ,@body))

(defmacro fixnump (object)
  `(typep ,object 'fixnum))

(defun file-writable (pathname)
  "File-writable accepts a pathname and returns T if the current
  process can write it, and NIL otherwise. Also if the file does
  not exist return T."
  (handler-case (let ((io (open pathname
                                :direction :output
                                :if-exists :append
                                :if-does-not-exist nil)))
                  (if io
                      (close io :abort t)
                      ;; more complicate situation:
                      ;; we want test if we can create the file.
                      (let ((io (open pathname
                                      :direction :output
                                      :if-exists nil
                                      :if-does-not-exist :create)))
                        (if io
                            (progn
                              (close io)
                              (delete-file io))
                            t))))
    (file-error (err)
                (declare (ignore err))
                nil)) )

(defmacro without-gcing (&body body)
  `(progn ,@body))
