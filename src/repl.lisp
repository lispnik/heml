;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(in-package :hi)


;;;; PREPL/background buffer integration

(declaim (special *in-heml-slave-p*
                  heml::*master-machine-and-port*
                  heml::*original-terminal-io*))

(defun need-to-redirect-debugger-io (stream)
  (eq stream heml::*original-terminal-io*))

(defun call-with-typeout-for-thread-debugger (cont)
  (with-new-event-loop ()
    (let ((prepl:*entering-prepl-debugger-hook* nil)
          (*in-heml-slave-p* t)
          (heml.wire:*current-wire* :not-yet))
      (heml::connect-to-editor-for-background-thread
       (car heml::*master-machine-and-port*)
       (cadr heml::*master-machine-and-port*))
      (dispatch-events-no-hang)
      (do ()
          ((not (eq heml.wire:*current-wire* :not-yet)))
        (dispatch-events)
        (write-line "Thread waiting for connection to master..."
                    heml::*original-terminal-io*)
        (force-output heml::*original-terminal-io*))
      (with-typeout-pop-up-in-master
          (*terminal-io* (format nil "Slave thread ~A"
                                 (bt:thread-name (bt:current-thread))))
        (call-with-standard-synonym-streams cont)))))

;;; Setup an a connection to the editor for the current thread, and
;;; create an editor buffer for I/O and return the client stream.
(defun typeout-for-thread ()
  (assert (or (not (boundp '*event-base*)) (not *event-base*)))
  (setf *event-base* (make-event-loop *connection-backend*))
  (setf *in-heml-slave-p* t)
  (let ((heml.wire:*current-wire* :not-yet))
    (heml::connect-to-editor-for-background-thread
     (car heml::*master-machine-and-port*)
     (cadr heml::*master-machine-and-port*))
    (dispatch-events-no-hang)
    (do ()
        ((not (eq heml.wire:*current-wire* :not-yet)))
      (dispatch-events)
      (write-line "Thread waiting for connection to master..."
                  heml::*original-terminal-io*)
      (force-output heml::*original-terminal-io*))
    (let* ((name (format nil "Slave thread ~A"
                         (bt:thread-name (bt:current-thread))))
           (ts-data (heml.wire:remote-value heml.wire:*current-wire*
                     (heml::%make-extra-typescript-buffer name))))
      (heml::connect-stream ts-data heml.wire:*current-wire*))))

