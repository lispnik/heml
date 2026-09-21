;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;;; Running the editor: AppKit's loop on the main thread, Hemlock's on a
;;;; thread of its own, and the entry point of Xoamax.app.

(in-package :hemlock.cocoa)

(defparameter +inherited-variables+
  '(*standard-output* *error-output* *trace-output* *terminal-io* *debug-io*
    *query-io* *standard-input* *package* *readtable* *default-pathname-defaults*)
  "The caller's values of these are the editor thread's too, so that output
from the editor goes where it would have gone from the caller.")

(defun stop-application ()
  "End -[NSApplication run].  Main thread only.

-stop: takes effect when the loop next finishes handling an event, and a
perform delivered through the run loop is not an event, so one is posted."
  (let ((app (objc.runloop:shared-application)))
    (objc:invoke app "stop:" nil)
    (objc:invoke app "postEvent:atStart:"
                 (objc:invoke "NSEvent"
                              "otherEventWithType:location:modifierFlags:timestamp:windowNumber:context:subtype:data1:data2:"
                              15        ; NSEventTypeApplicationDefined
                              (vector 0d0 0d0) 0 0d0 0 nil 0 0 0)
                 t)))

(defmethod hi::invoke-with-editor-thread ((backend (eql :cocoa)) fun)
  (unless (objc.runloop:main-thread-p)
    (error "The Cocoa backend needs the main thread for AppKit, and this is ~
            thread ~A.  Start the editor from the main thread: a terminal ~
            REPL's, or a delivered application's toplevel."
           (bt:thread-name (bt:current-thread))))
  (ensure-display)
  (show-window)
  (let* ((outcome nil)
         (thread
           (bt:make-thread
            (lambda ()
              (setf *editor-running-p* t)
              (unwind-protect
                   (setf outcome (multiple-value-list (funcall fun)))
                (setf *editor-running-p* nil)
                (on-main-thread (stop-application))))
            :name "Hemlock"
            :initial-bindings (mapcar (lambda (symbol) (cons symbol (symbol-value symbol)))
                                      +inherited-variables+))))
    (objc:invoke (objc.runloop:shared-application) "run")
    (bt:join-thread thread)
    (hide-window)
    (objc.runloop:restore-frontmost)
    (values-list outcome)))

;;; The editor's title follows the current buffer.
;;;
(defun update-title (buffer)
  (when (and hi::*in-the-editor* *display*)
    (set-title (format nil "~A — Xoamax" (hi::buffer-name buffer)))))

(hi::add-hook hemlock::set-buffer-hook 'update-title)


;;;; Xoamax.app

(defun main ()
  "The entry point of the application bundle: the editor, with Cocoa, and
the process ends when it does."
  (let ((argv0 (car sb-ext:*posix-argv*)))
    (when argv0
      (setf hemlock::*slave-command* (list argv0 "--slave"))))
  (hi::main (uiop:command-line-arguments))
  (uiop:quit 0))
