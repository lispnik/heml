;;;; scripts/demo/cocoa.lisp -- `make demo-cocoa': a video of the Cocoa editor.
;;;;
;;;; Drives the editor the way test/smoke.lisp does -- keys and menu items
;;;; posted to it, NSEvents sent to its window -- while a thread has the
;;;; view draw itself into a PNG ten times a second.  ffmpeg makes the
;;;; frames, in build/demo/cocoa-frames/, into build/demo/xoamax-cocoa.mp4.
;;;; Like the smoke test, it neither takes the keyboard nor touches the
;;;; clipboard, and needs no Screen Recording permission.

(asdf:load-system :hemlock.cocoa)

(defpackage :xoamax-demo (:use :common-lisp))
(in-package :xoamax-demo)

(defvar *top* (asdf:system-source-directory :hemlock.cocoa))
(defvar *files* (merge-pathnames "build/demo/files/" *top*))
(defvar *frames* (merge-pathnames "build/demo/cocoa-frames/" *top*))

(setf hemlock.cocoa::*activate* nil
      hemlock.cocoa::*pasteboard-name* "org.lispnik.xoamax.demo"
      hemlock.cocoa::*remember-font* nil
      hemlock.cocoa:*font-size* 14
      hemlock.cocoa:*font-name* nil)

;; A plain shell, without anyone's startup files.
(setf (hemlock::variable-value 'hemlock::shell-utility-switches :global)
      "--norc --noprofile")

(when (probe-file *frames*)
  (uiop:delete-directory-tree *frames* :validate t))
(ensure-directories-exist *frames*)
(ensure-directories-exist *files*)
(uiop:copy-file (merge-pathnames "src/display.lisp" *top*)
                (merge-pathnames "display.lisp" *files*))
(with-open-file (s (merge-pathnames "fib.lisp" *files*) :direction :output
                   :if-exists :supersede :if-does-not-exist :create))


;;;; Driving the editor, as test/smoke.lisp does

(defmacro main (&body body)
  `(hemlock.cocoa::call-on-main-thread-and-wait (lambda () ,@body)))

(defun settle ()
  (loop repeat 200 until (hemlock.cocoa::inbox-empty-p) do (sleep 0.05))
  (sleep 0.3))

(defun post (descriptor) (hemlock.cocoa::post-to-editor descriptor))
(defun post-key (character &rest modifiers) (post (list :char character modifiers)))
(defun post-named (name) (post (list :named name '())))

(defun type-text (string &key (pause 0.045))
  "STRING typed a key at a time, as a person would."
  (loop for c across string
        do (if (char= c #\Newline) (post-named "Return") (post-key c))
           (sleep pause)))

(defun extended-command (name)
  (post-key #\x "Meta")
  (sleep 0.3)
  (type-text name :pause 0.03)
  (post-named "Return")
  (settle))

(defun display () hemlock.cocoa::*display*)
(defun window () (hemlock.cocoa::display-window (display)))
(defun view () (hemlock.cocoa::display-view (display)))

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
         (point (vector (float (+ hemlock.cocoa::*margin*
                                  (* (+ column 1/2) (hemlock.cocoa::display-char-width display)))
                               1d0)
                        (float (+ hemlock.cocoa::*margin*
                                  (* (+ line 1/2) (hemlock.cocoa::display-char-height display)))
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
          (objc:invoke (objc:objc-object-pointer (hemlock.cocoa::display-app-delegate (display)))
                       "application:openURLs:"
                       (objc.runloop:shared-application)
                       (objc:invoke "NSArray" "arrayWithObject:" url)))))


;;;; Recording

(defvar *recording* nil)
(defvar *frame* 0)

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
  (setf *recording* t)
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


;;;; The demo

(defun pause (seconds) (settle) (sleep seconds))

(defun run ()
  (loop until hemlock.cocoa::*editor-running-p* do (sleep 0.1))
  (main (objc:invoke (window) "setFrame:display:" (vector 80d0 80d0 1200d0 760d0) t))
  (settle)
  (start-recording)
  (pause 1)

  ;; A file opened from Finder, and Lisp typed into it.
  (open-from-finder (merge-pathnames "fib.lisp" *files*))
  (pause 0.8)
  (type-text ";;; Xoamax, native on macOS.")
  (post-named "Return")
  (type-text "(defun fib (n)") (post-key #\j "Control")
  (type-text "\"The Nth Fibonacci number.\"") (post-key #\j "Control")
  (type-text "(if (< n 2)") (post-key #\j "Control")
  (type-text "n") (post-key #\j "Control")
  (type-text "(+ (fib (- n 1)) (fib (- n 2)))))")
  (post-named "Return") (post-named "Return")
  (pause 1)

  ;; Evaluated in the editor's own Lisp.
  (extended-command "Editor Evaluate Buffer")
  (extended-command "Editor Evaluate Expression")
  (type-text "(fib 20)")
  (post-named "Return")
  (pause 2)

  ;; Japanese through the input method: held while composed, then committed.
  (type-text ";; ")
  (main (objc:invoke (view) "setMarkedText:selectedRange:replacementRange:"
                     "にほんご" (cons 4 0) (cons cocoa:ns-not-found 0)))
  (pause 1)
  (main (objc:invoke (view) "insertText:replacementRange:" "日本語"
                     (cons cocoa:ns-not-found 0)))
  (type-text " takes two columns a character.")
  (post-named "Return")
  (pause 1)

  ;; The font, bigger and back.
  (press-menu #\=) (pause 0.6)
  (press-menu #\=) (pause 1)
  (press-menu #\0) (pause 1)

  ;; The mouse: a double click selects a word, a drag a region.
  (mouse :down 8 1 :clicks 1) (mouse :up 8 1 :clicks 1)
  (mouse :down 8 1 :clicks 2) (mouse :up 8 1 :clicks 2)
  (pause 1.2)
  (mouse :down 2 2) (mouse :drag 10 2) (mouse :drag 20 2) (mouse :drag 28 2) (mouse :up 28 2)
  (pause 1.2)
  (mouse :down 0 8) (mouse :up 0 8)
  (pause 0.5)

  ;; Side by side, with a shell on the right.
  (choose-menu-item "View" "Split Window Side by Side")
  (pause 1)
  (extended-command "Shell")
  (pause 1.5)
  (type-text "ls src | head -8")
  (post-named "Return")
  (pause 1.2)
  (type-text "seq 1 500")
  (post-named "Return")
  (pause 1.5)

  ;; The left window split again, a real file in its lower half, paged.
  (post-key #\x "Control") (post-key #\o)
  (pause 0.6)
  (post-key #\x "Control") (post-key #\2)
  (pause 0.8)
  (post-key #\x "Control") (post-key #\f "Control")
  (pause 0.6)
  (type-text "display.lisp" :pause 0.04)
  (post-named "Return")
  (pause 1)
  (post-key #\v "Control") (pause 1)
  (post-key #\v "Control") (pause 1)
  (post-key #\v "Meta") (pause 1)

  ;; Widen the left column, balance them again, and keep only this window.
  (post-key #\u "Control") (type-text "12" :pause 0.1)
  (post-key #\x "Control") (post-key #\})
  (pause 1.5)
  (choose-menu-item "View" "Balance Windows")
  (pause 1.5)
  (post-key #\x "Control") (post-key #\1)
  (pause 2)

  (setf *recording* nil)
  (sleep 0.3)
  (post :quit)
  (loop repeat 10
        while hemlock.cocoa::*editor-running-p*
        do (sleep 1)
           (when hemlock.cocoa::*editor-running-p* (post-key #\n))))

(defvar *driver*
  (bt:make-thread (lambda ()
                    (handler-case (run)
                      (error (condition)
                        (format t "~&demo: ~A~%" condition)
                        (setf *recording* nil)
                        (post :quit))))
                  :name "demo"))

(bt:make-thread (lambda () (sleep 300) (format t "~&demo: timed out~%") (sb-ext:exit :code 2 :abort t)))

(hemlock:hemlock nil :backend-type :cocoa :load-user-init nil)
(bt:join-thread *driver*)
(format t "~&demo: ~D frames in ~A~%" *frame* (namestring *frames*))
(sb-ext:exit :code 0 :abort t)
