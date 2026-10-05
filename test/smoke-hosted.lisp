;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;;; test/smoke-hosted.lisp -- `make smoke-hosted`: Heml as a guest in another
;;;; program's application.
;;;;
;;;; This file plays the host: it makes the application, gives it a delegate
;;;; and a menu bar of its own, and keeps its event loop turning on the main
;;;; thread -- as a Lisp listener or an IDE would -- and opens Heml in it with
;;;; HEML.COCOA:START-HOSTED.  Then it checks that Heml behaves as a guest:
;;;;
;;;;   - START-HOSTED returns, and the editor runs on its own thread;
;;;;   - the application's delegate stays the host's, and so does the menu bar
;;;;     while Heml's window is not key -- it is Heml's while it is;
;;;;   - a second START-HOSTED visits another file in the same editor;
;;;;   - closing the window hides it, and the editor and its buffers live on;
;;;;   - HEML:*EVALUATE-TEXT-FUNCTION* gets Evaluate Defun's text;
;;;;   - HOSTED-QUIT-OK-P is true with nothing to lose;
;;;;   - when the editor exits, the window goes and the host is as it was.
;;;;
;;;; Exits 0 when every check passes, 1 when one fails, 2 when it hangs.

(asdf:load-system :heml.cocoa)

(defpackage :heml-smoke-hosted (:use :common-lisp))
(in-package :heml-smoke-hosted)

(defvar *checks* 0)
(defvar *failures* '())

(setf heml.cocoa::*activate* nil
      heml.cocoa::*remember-font* nil)

(defun note (format &rest arguments)
  (format t "~&~?~%" format arguments)
  (finish-output))

(defmacro check (name form)
  `(let ((result (ignore-errors ,form)))
     (incf *checks*)
     (cond (result (note "  ok    ~A" ,name))
           (t (push ,name *failures*)
              (note "  FAIL  ~A" ,name)))))

(defun pump-until (predicate &key (seconds 10))
  "Turn the host's event loop until PREDICATE is true, or SECONDS pass.
True if it became true."
  (let ((deadline (+ (get-internal-real-time)
                     (* seconds internal-time-units-per-second))))
    (loop
      (when (ignore-errors (funcall predicate)) (return t))
      (when (> (get-internal-real-time) deadline) (return nil))
      (objc.runloop:pump-events :seconds 0.05d0))))

(defun pump-for (seconds)
  (pump-until (constantly nil) :seconds seconds))

;;; The host -----------------------------------------------------------------------

(objc:define-objc-class host-delegate ()
  ()
  (:objc-class-name "HemlSmokeHostDelegate"))

(defvar *host-delegate* nil)

(defun file-buffer (path)
  (find (namestring path) heml::*buffer-list*
        :key (lambda (buffer)
               (let ((name (heml::buffer-pathname buffer)))
                 (and name (namestring name))))
        :test #'equal))

(defun heml-window ()
  (heml.cocoa::display-window heml.cocoa::*display*))

(defun main-menu ()
  (objc:invoke (objc.runloop:shared-application) "mainMenu"))

(defun same-pointer-p (a b)
  (and a b (cffi:pointer-eq a b)))

(defun run ()
  (objc:ensure-objc-initialized :modules (list heml.cocoa::+appkit-path+))
  (setf *host-delegate* (make-instance 'host-delegate))
  (let* ((app (objc.runloop:shared-application :activation-policy 1))
         (directory (uiop:ensure-directory-pathname
                     (merge-pathnames "build/smoke-hosted/"
                                      (asdf:system-source-directory :heml.cocoa))))
         (one (merge-pathnames "one.lisp" directory))
         (two (merge-pathnames "two.lisp" directory))
         (host-menu (objc:invoke (objc:invoke "NSMenu" "alloc") "initWithTitle:" "Host"))
         (evaluated '()))
    (ensure-directories-exist directory)
    (with-open-file (out one :direction :output :if-exists :supersede)
      (format out "(in-package :cl-user)~%~%(defun one () 1)~%"))
    (with-open-file (out two :direction :output :if-exists :supersede)
      (format out "(defun two () 2)~%"))
    (objc:invoke app "setDelegate:" (objc:objc-object-pointer *host-delegate*))
    (objc:invoke app "setMainMenu:" host-menu)
    (setf heml:*evaluate-text-function*
          (lambda (text package) (push (list text package) evaluated)))

    (note "Hosted: Heml opened in a running application")
    (let ((started (get-internal-real-time)))
      (heml.cocoa:start-hosted one)
      (check "START-HOSTED returns, rather than running the application"
             (< (- (get-internal-real-time) started) (* 5 internal-time-units-per-second))))
    (check "and the editor runs, on a thread of its own"
           (pump-until #'heml.cocoa:hosted-running-p))
    (check "visiting the file it was given"
           (pump-until (lambda () (file-buffer one))))
    (check "the application's delegate is still the host's"
           (same-pointer-p (objc:invoke app "delegate")
                           (objc:objc-object-pointer *host-delegate*)))

    (note "The menu bar follows the key window")
    (let ((delegate (objc:invoke (heml-window) "delegate")))
      (objc:invoke delegate "windowDidResignKey:" nil)
      (check "while Heml's window is not key, the menu bar is the host's"
             (same-pointer-p (main-menu) host-menu))
      (objc:invoke delegate "windowDidBecomeKey:" nil)
      (check "while it is, the menu bar is Heml's"
             (same-pointer-p (main-menu) heml.cocoa::*hosted-menubar*))
      (objc:invoke delegate "windowDidResignKey:" nil)
      (check "and the host's comes back"
             (same-pointer-p (main-menu) host-menu)))

    (note "A second file, in the same editor")
    (heml.cocoa:start-hosted two)
    (check "a second START-HOSTED visits another file"
           (pump-until (lambda () (file-buffer two))))
    (check "and the first is still there" (file-buffer one))

    (note "Closing the window")
    (objc:invoke (objc:invoke (heml-window) "delegate") "windowShouldClose:" (heml-window))
    (pump-for 0.3)
    (check "closing the window hides it"
           (not (objc:invoke-bool (heml-window) "isVisible")))
    (check "and the editor goes on running" (heml.cocoa:hosted-running-p))
    (heml.cocoa:start-hosted)
    (pump-for 0.3)
    (check "asked again, the window comes back"
           (objc:invoke-bool (heml-window) "isVisible"))
    (check "with its buffers" (and (file-buffer one) (file-buffer two)))

    (note "At a line")
    (heml.cocoa:start-hosted one :line 3)
    (check "START-HOSTED with a line puts the point there"
           (pump-until (lambda ()
                         (let ((buffer (file-buffer one)))
                           (and (eq buffer (heml::current-buffer))
                                (search "(defun one"
                                        (heml::line-string
                                         (heml::mark-line
                                          (heml::buffer-point buffer)))))))))

    (note "Evaluating is the host's")
    (heml.cocoa:start-hosted one)
    (pump-for 0.5)
    (heml.cocoa::post-to-editor (list :command "End of Buffer"))
    (heml.cocoa::post-to-editor (list :command "Previous Line"))
    (heml.cocoa::post-to-editor (list :command "Evaluate Defun"))
    (check "Evaluate Defun gives the form to the host's function"
           (pump-until (lambda () evaluated)))
    (check "the form at point: (defun one () 1)"
           (search "(defun one () 1)" (first (first evaluated))))
    (note "    (it got ~S)" (first evaluated))

    (note "Quitting")
    (check "with nothing changed, the host may quit" (heml.cocoa:hosted-quit-ok-p))
    (heml.cocoa::post-to-editor :quit)
    (check "the editor exits when asked"
           (pump-until (lambda () (not (heml.cocoa:hosted-running-p)))))
    (pump-for 0.3)
    (check "and its window goes" (not (objc:invoke-bool (heml-window) "isVisible")))
    (check "the application's delegate is the host's still"
           (same-pointer-p (objc:invoke app "delegate")
                           (objc:objc-object-pointer *host-delegate*)))
    (check "and so is the menu bar" (same-pointer-p (main-menu) host-menu))
    (heml.cocoa:start-hosted one)
    (check "and it can be opened again, the buffers kept"
           (and (pump-until #'heml.cocoa:hosted-running-p)
                (file-buffer two)))
    (heml.cocoa::post-to-editor :quit)
    (pump-until (lambda () (not (heml.cocoa:hosted-running-p))))))

;;; A run that hangs is a failure too.
(bt:make-thread (lambda ()
                  (sleep 180)
                  (note "smoke-hosted: hung")
                  (sb-ext:exit :code 2 :abort t))
                :name "watchdog")

(handler-case (run)
  (error (condition)
    (push (format nil "~A" condition) *failures*)
    (note "  ERROR ~A" condition)))
(note "~&smoke-hosted: ~D check~:P, ~D failure~:P" *checks* (length *failures*))
(sb-ext:exit :code (if *failures* 1 0) :abort t)
