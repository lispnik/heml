;;;; scripts/demo/driver.lisp -- what the Cocoa demos share.
;;;;
;;;; Each demo drives the editor the way test/smoke.lisp does -- keys and
;;;; menu items posted to it, NSEvents sent to its window -- while a thread
;;;; has the view draw itself into a PNG ten times a second, into
;;;; build/demo/NAME-frames/.  CAPTION notes what the video is showing from
;;;; that frame on; scripts/demo/encode.sh makes the frames into
;;;; build/demo/heml-NAME.mp4, with each caption in a band below the editor.
;;;; Like the smoke test, a demo neither takes the keyboard nor touches the
;;;; clipboard, and needs no Screen Recording permission.
;;;;
;;;; A demo loads this, defines its steps in a function, and calls
;;;; (RUN-DEMO "name" #'function).

(handler-bind ((warning #'muffle-warning))
  (asdf:load-system :heml.cocoa))

(defpackage :heml-demo (:use :common-lisp))
(in-package :heml-demo)

(defvar *top* (asdf:system-source-directory :heml.cocoa))
(defvar *demo* (merge-pathnames "build/demo/" *top*))
(defvar *files* (merge-pathnames "files/" *demo*))
(defvar *frames*)

(setf heml.cocoa::*activate* nil
      heml.cocoa::*pasteboard-name* "org.lispnik.heml.demo"
      heml.cocoa::*remember-font* nil
      heml.cocoa:*font-size* 14
      heml.cocoa:*font-name* nil)

;; A plain shell, without anyone's startup files, and no project's saved
;; session or list from the person's own use.
(setf (heml::variable-value 'heml::shell-utility-switches :global)
      "--norc --noprofile")
(let ((state (merge-pathnames "state/" *demo*)))
  (when (probe-file state)
    (uiop:delete-directory-tree state :validate t))
  (setf (uiop:getenv "HEML_STATE_DIRECTORY") (namestring state)))

(ensure-directories-exist *files*)

(defun fresh-directory (name)
  "The directory NAME under build/demo/, made empty."
  (let ((directory (merge-pathnames (uiop:ensure-directory-pathname name) *demo*)))
    (when (probe-file directory)
      (uiop:delete-directory-tree directory :validate t))
    (ensure-directories-exist directory)))

(defun write-file (pathname &rest lines)
  (ensure-directories-exist pathname)
  (with-open-file (out pathname :direction :output :if-exists :supersede
                                :external-format :utf-8)
    (format out "~{~A~%~}" lines))
  pathname)

(defun sh (directory command)
  "Run the shell COMMAND in DIRECTORY, quietly."
  (uiop:run-program (list "/bin/sh" "-c" command) :directory (namestring directory)
                    :output nil :error-output nil :ignore-error-status t))


;;;; Driving the editor, as test/smoke.lisp does

(defmacro main (&body body)
  `(heml.cocoa::call-on-main-thread-and-wait (lambda () ,@body)))

(defun settle ()
  (loop repeat 200 until (heml.cocoa::inbox-empty-p) do (sleep 0.05))
  (sleep 0.3))

(defun post (descriptor) (heml.cocoa::post-to-editor descriptor))
(defun post-key (character &rest modifiers) (post (list :char character modifiers)))
(defun post-named (name &rest modifiers) (post (list :named name modifiers)))

(defun type-text (string &key (pause 0.045))
  "STRING typed a key at a time, as a person would."
  (loop for c across string
        do (if (char= c #\Newline) (post-named "Return") (post-key c))
           (sleep pause)))

(defun type-lines (lines &key (pause 0.035))
  (dolist (line lines)
    (type-text line :pause pause)
    (post-named "Return")))

(defun keys (&rest keys)
  "Each of KEYS pressed in turn: a character, or a list of a character or a
   key's name and its modifiers."
  (dolist (key keys)
    (etypecase key
      (character (post-key key))
      (string (post-named key))
      (cons (if (characterp (first key))
                (apply #'post-key key)
                (apply #'post-named key))))
    (sleep 0.12)))

(defun extended-command (name)
  (post-key #\x "Meta")
  (sleep 0.3)
  (type-text name :pause 0.03)
  (post-named "Return")
  (settle))

(defun display () heml.cocoa::*display*)
(defun window () (heml.cocoa::display-window (display)))
(defun view () (heml.cocoa::display-view (display)))

(defconstant +command+ (ash 1 20))

(defun key-event (characters flags)
  (objc:invoke "NSEvent"
               "keyEventWithType:location:modifierFlags:timestamp:windowNumber:context:characters:charactersIgnoringModifiers:isARepeat:keyCode:"
               10 (vector 0d0 0d0) flags 0d0 (objc:invoke (window) "windowNumber") nil
               characters characters nil 0))

(defun press-menu (character)
  (main (objc:invoke (objc:invoke (objc.runloop:shared-application) "mainMenu")
                     "performKeyEquivalent:"
                     (key-event (string character) +command+))))

(defun choose-menu-item (menu title)
  (main (let* ((submenu (objc:invoke (objc:invoke (objc:invoke (objc.runloop:shared-application)
                                                                "mainMenu")
                                                   "itemWithTitle:" menu)
                                      "submenu"))
               (index (objc:invoke submenu "indexOfItemWithTitle:" title)))
          (objc:invoke submenu "performActionForItemAtIndex:" index))))

(defun mouse-event (type column line clicks)
  (let* ((display (display))
         (point (vector (float (+ heml.cocoa::*margin*
                                  (* (+ column 1/2) (heml.cocoa::display-char-width display)))
                               1d0)
                        (float (+ heml.cocoa::*margin*
                                  (* (+ line 1/2) (heml.cocoa::display-char-height display)))
                               1d0))))
    (objc:invoke "NSEvent"
                 "mouseEventWithType:location:modifierFlags:timestamp:windowNumber:context:eventNumber:clickCount:pressure:"
                 (ecase type (:down 1) (:up 2) (:drag 6))
                 (objc:invoke (view) "convertPoint:toView:" point nil)
                 0 0d0 (objc:invoke (window) "windowNumber") nil 0 clicks 1.0)))

(defun mouse (type column line &key (clicks 1))
  (main (objc:invoke (window) "sendEvent:" (mouse-event type column line clicks))))

(defun open-from-finder (file)
  (main (let ((url (objc:invoke "NSURL" "fileURLWithPath:" (namestring file))))
          (objc:invoke (objc:objc-object-pointer (heml.cocoa::display-app-delegate (display)))
                       "application:openURLs:"
                       (objc.runloop:shared-application)
                       (objc:invoke "NSArray" "arrayWithObject:" url)))))

(defun pause (seconds) (settle) (sleep seconds))

(defun open-file (pathname)
  (open-from-finder pathname)
  (pause 0.8))

(defun screen-text ()
  (map 'list #'heml.cocoa::row-text (heml.cocoa::screen-rows heml.cocoa::*screen*)))

(defun row-p (text)
  (find text (screen-text) :test #'search))

(defun wait-for (text &optional (seconds 30))
  "Wait until TEXT is on the screen, or SECONDS have gone; whether it came."
  (loop repeat (* seconds 10)
        when (row-p text) return t
        do (sleep 0.1)
        finally (format t "~&demo: no ~S on the screen after ~Ds~%" text seconds)
                (return nil)))

(defun point-line ()
  (heml-internals:line-string (heml-internals:mark-line (heml-internals:current-point))))


;;;; Recording, and captions

(defvar *recording* nil)
(defvar *frame* 0)
(defvar *recording-start* nil)
(defvar *recording-end* nil)
(defvar *captions* '()
  "(FRAME . TEXT) for each caption, the latest first.")

(defun capture-frame ()
  (let ((path (namestring (merge-pathnames (format nil "~5,'0D.png" (incf *frame*)) *frames*))))
    (main
      (let* ((bounds (objc:invoke (view) "bounds"))
             (rep (objc:invoke (view) "bitmapImageRepForCachingDisplayInRect:" bounds)))
        (objc:invoke (view) "cacheDisplayInRect:toBitmapImageRep:" bounds rep)
        (objc:invoke (objc:invoke rep "representationUsingType:properties:" 4
                                  (objc:invoke "NSDictionary" "dictionary"))
                     "writeToFile:atomically:" path t)))))

(defun start-recording ()
  (setf *recording* t
        *recording-start* (get-internal-real-time))
  (bt:make-thread
   (lambda ()
     (loop with interval = 1/10
           with next = (get-internal-real-time)
           while *recording*
           do (ignore-errors (capture-frame))
              (incf next (* interval internal-time-units-per-second))
              (let ((wait (/ (- next (get-internal-real-time)) internal-time-units-per-second)))
                (when (plusp wait) (sleep wait)))))
   :name "demo recorder"))

(defun caption (text)
  "Say TEXT below the editor from now until the next caption."
  (push (cons *frame* text) *captions*))

(defun write-captions (name)
  "Each caption's text in a file of its own, and the list of them, frame
   from and to, for encode.sh."
  (let ((directory (merge-pathnames (format nil "~A-captions/" name) *demo*)))
    (when (probe-file directory)
      (uiop:delete-directory-tree directory :validate t))
    (ensure-directories-exist directory)
    (with-open-file (list (merge-pathnames "list.txt" directory) :direction :output
                                                                  :if-exists :supersede)
      (loop for ((from . text) . rest) on (reverse *captions*)
            for i from 1
            for to = (if rest (car (first rest)) *frame*)
            for file = (merge-pathnames (format nil "~2,'0D.txt" i) directory)
            do (with-open-file (out file :direction :output :if-exists :supersede
                                         :external-format :utf-8)
                 (write-string text out))
               (format list "~D ~D ~A~%" from to (namestring file))))))


;;;; Running a demo

(defun run-demo (name function)
  "Start the editor, and on another thread record FUNCTION's steps as the
   demo NAME; leave when they are done."
  (setf *frames* (fresh-directory (format nil "~A-frames" name)))
  (let ((driver
          (bt:make-thread
           (lambda ()
             (handler-case
                 (progn
                   (loop until heml.cocoa::*editor-running-p* do (sleep 0.1))
                   (main (objc:invoke (window) "setFrame:display:"
                                      (vector 80d0 80d0 1200d0 760d0) t))
                   (settle)
                   (start-recording)
                   (pause 1)
                   (funcall function)
                   (pause 1.5))
               (error (condition)
                 (format t "~&demo: ~A~%" condition)))
             (setf *recording* nil
                   *recording-end* (get-internal-real-time))
             (sleep 0.3)
             ;; Out of any prompt, and away without asking about the
             ;; buffers the demo changed.
             (post-key #\g "Control")
             (post-key #\g "Control")
             (post (list :command "Exit Heml")))
           :name "demo")))
    (bt:make-thread (lambda ()
                      (sleep 600)
                      (format t "~&demo: timed out~%")
                      (sb-ext:exit :code 2 :abort t)))
    (heml:heml nil :backend-type :cocoa :load-user-init nil)
    (bt:join-thread driver)
    (write-captions name)
    ;; Frames come as fast as the view can be drawn and written, which may be
    ;; fewer than ten a second: the video's rate is what they came at.
    (with-open-file (out (merge-pathnames (format nil "~A-rate.txt" name) *demo*)
                         :direction :output :if-exists :supersede)
      (format out "~,3F~%" (/ *frame* (max 1/10 (/ (- *recording-end* *recording-start*)
                                                   internal-time-units-per-second)))))
    (format t "~&demo: ~D frames in ~A~%" *frame* (namestring *frames*))
    (finish-output)
    ;; What the editor started -- a language server, a debugger -- goes too,
    ;; or it keeps the output of whatever ran the demo open.
    (ignore-errors
     (uiop:run-program (list "pkill" "-P" (princ-to-string (sb-posix:getpid)))
                       :ignore-error-status t))
    (sb-ext:exit :code 0 :abort t)))
