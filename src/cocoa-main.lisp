;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;;; Running the editor: AppKit's loop on the main thread, Heml's on a
;;;; thread of its own, and the entry point of Heml.app.

(in-package :heml.cocoa)

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
  (when *hosted*
    (return-from hi::invoke-with-editor-thread (start-hosted-thread fun)))
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
            :name "Heml"
            :initial-bindings (mapcar (lambda (symbol) (cons symbol (symbol-value symbol)))
                                      +inherited-variables+))))
    (objc:invoke (objc.runloop:shared-application) "run")
    (bt:join-thread thread)
    (hide-window)
    ;; Give the keyboard back to whoever had it -- but only if it was
    ;; taken, or this would pull the user back from wherever they are now.
    (when *activate*
      (objc.runloop:restore-frontmost))
    (values-list outcome)))

;;;; Hosted: a guest in another program's application
;;;
;;; Heml.app owns its process: it runs -[NSApplication run], its delegate is
;;; the application's, its menus are the menu bar, and closing its window is
;;; quitting.  A program that already has an application running -- a Lisp
;;; listener, an IDE -- can have Heml as one more window of its own instead,
;;; by calling START-HOSTED on its main thread.  Then:
;;;
;;;   - the application's run loop is the host's: Heml's thread is started and
;;;     START-HOSTED returns, and when the editor exits its window is hidden;
;;;   - the host's application delegate stays -- Finder's files and Quit are
;;;     the host's, and HOSTED-QUIT-OK-P is the host's to ask before quitting;
;;;   - Heml's menu bar is the menu bar only while its window is key, and the
;;;     host's is put back when it is not;
;;;   - closing the window hides it, and the buffers live on;
;;;   - asking again shows the window, visiting the file if one is named.
;;;
;;; HEML:*EVALUATE-TEXT-FUNCTION* (src/lispeval.lisp) is what the host sets to
;;; have Heml's evaluation commands evaluate in the host's way.

(defvar *hosted-thread* nil
  "The editor's thread, while hosted.")

(defun start-hosted-thread (fun)
  "INVOKE-WITH-EDITOR-THREAD for a guest: the window up, FUN on the Heml
thread, and back to the caller -- whose run loop is already running."
  (ensure-display)
  (show-window)
  (setf *hosted-thread*
        (bt:make-thread
         (lambda ()
           (setf *editor-running-p* t)
           (unwind-protect (funcall fun)
             (setf *editor-running-p* nil)
             (on-main-thread (hide-window))))
         :name "Heml"
         :initial-bindings (mapcar (lambda (symbol) (cons symbol (symbol-value symbol)))
                                   +inherited-variables+)))
  nil)

(defun start-hosted (&optional file &key line)
  "Open Heml in this process's running application, visiting FILE -- a
pathname or a namestring -- if one is given, at LINE, counted from one, if
that is.  Main thread; returns at once.  Called again while the editor runs,
it shows the window and visits FILE."
  (unless (objc.runloop:main-thread-p)
    (error "START-HOSTED is for the main thread, which runs the host's ~
            application; this is thread ~A." (bt:thread-name (bt:current-thread))))
  (setf *hosted* t)
  (cond (*editor-running-p*
         (when file
           (post-to-editor (list :open (namestring file))))
         (show-window))
        (t
         (heml:heml (and file (pathname file)) :backend-type :cocoa)))
  ;; After the file: the inbox is taken in order, and an editor just
  ;; started visits its file before it reads the inbox at all.
  (when (and file line)
    (post-to-editor (list :goto-line line)))
  t)

(defun hosted-running-p ()
  "Whether a hosted Heml's editor is running now."
  (and *hosted* *editor-running-p*))

(defun hosted-quit-ok-p ()
  "For a host about to quit: true when nothing of Heml's would be lost.
Otherwise Heml is asked to exit as C-x C-c would -- offering to save each
changed file -- its window is shown, and the answer is NIL: the host cancels
this quit, and the person quits again once Heml has finished."
  (cond ((not (hosted-running-p)) t)
        ((notany (lambda (buffer)
                   (and (hi:buffer-pathname buffer) (hi:buffer-modified buffer)))
                 hi:*buffer-list*)
         t)
        (t (show-window)
           (post-to-editor :quit)
           nil)))

;;; The window's title follows the current buffer, from each frame
;;; (buffer-title, cocoa-device.lisp).


;;;; Heml.app

(defun main ()
  "The entry point of the application bundle: the editor, Cocoa unless the
command line asks for another, and the process ends when it does.

Launched by Finder or the Dock -- by launchd -- the process has no
terminal, starts in /, and its output goes to the log asdf-macos-app
opens; it goes to the home directory instead, so that a prompt for a file,
a shell and a terminal start there.  Run from a shell, through the bin/heml
launcher, its output goes to the terminal, and files named on the command
line are found from the shell's directory."
  (let ((cwd (uiop:getcwd)))
    (if (equal (uiop:native-namestring cwd) "/")
        (let ((home (user-homedir-pathname)))
          (ignore-errors (uiop:chdir home))
          (setf *default-pathname-defaults* home))
        (setf *default-pathname-defaults* cwd)))
  ;; A slave is this image again, started as the launcher starts it.
  (setf heml::*slave-command*
        (list (uiop:native-namestring sb-ext:*runtime-pathname*)
              "--core" (uiop:native-namestring sb-ext:*core-pathname*)
              "--noinform" "--end-runtime-options" "--slave"))
  (flet ((run () (hi:main (uiop:command-line-arguments))))
    (if (sb-unix:unix-isatty 0)
        (let ((*standard-output* sb-sys:*stdout*)
              (*error-output* sb-sys:*stderr*)
              (*trace-output* sb-sys:*stdout*))
          (run))
        (run)))
  (uiop:quit 0))
