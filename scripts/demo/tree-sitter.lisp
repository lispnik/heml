;;;; scripts/demo/tree-sitter.lisp -- `make demo-tree-sitter': a video of
;;;; syntax highlighting, by mode and with tree-sitter.
;;;;
;;;; Drives the editor the way test/smoke.lisp does -- keys and menu items
;;;; posted to it, NSEvents sent to its window -- while a thread has the
;;;; view draw itself into a PNG ten times a second.  ffmpeg makes the
;;;; frames, in build/demo/tree-sitter-frames/, into build/demo/heml-tree-sitter.mp4.
;;;; Like the smoke test, it neither takes the keyboard nor touches the
;;;; clipboard, and needs no Screen Recording permission.

(asdf:load-system :heml.cocoa)

(defpackage :heml-demo (:use :common-lisp))
(in-package :heml-demo)

(defvar *top* (asdf:system-source-directory :heml.cocoa))
(defvar *files* (merge-pathnames "build/demo/files/" *top*))
(defvar *frames* (merge-pathnames "build/demo/tree-sitter-frames/" *top*))

(setf heml.cocoa::*activate* nil
      heml.cocoa::*pasteboard-name* "org.lispnik.heml.demo"
      heml.cocoa::*remember-font* nil
      heml.cocoa:*font-size* 14
      heml.cocoa:*font-name* nil)

;; A plain shell, without anyone's startup files.
(setf (heml::variable-value 'heml::shell-utility-switches :global)
      "--norc --noprofile")

(when (probe-file *frames*)
  (uiop:delete-directory-tree *frames* :validate t))
(ensure-directories-exist *frames*)
(ensure-directories-exist *files*)
(dolist (name '("fib.c" "notes.md" "hello.py" "plain.txt"))
  (with-open-file (s (merge-pathnames name *files*) :direction :output
                     :if-exists :supersede :if-does-not-exist :create)))

;;; The same Lisp twice: once in Lisp mode, coloured by Heml's own parser,
;;; and once in a mode made here for the demo, coloured by tree-sitter's
;;; Common Lisp grammar.
(defparameter *lisp* ";;;; Both highlighters, on the same Lisp.

#| A block comment:
   (defun commented-out () 'not-code) |#

(defun greet (name &key (greeting \"Hello\") loud)
  \"Greet NAME.\"
  (let ((text (format nil \"~A, ~A!\" greeting name)))
    (if loud (string-upcase text) text)))   ; shout?

(defmacro with-greeting ((var name) &body body)
  `(let ((,var (greet ,name)))
     ,@body))

(defvar *greetings* 0)
(defconstant +limit+ 10)
#+sbcl (sb-ext:gc :full t)
(list #\\( :key 3.14 #x1F nil t)
")
(dolist (name '("both.lisp" "both.tslisp"))
  (with-open-file (s (merge-pathnames name *files*) :direction :output
                     :if-exists :supersede :if-does-not-exist :create)
    (write-string *lisp* s)))

(in-package :heml)
(defmode "Lisp/tree-sitter" :major-p t)
(define-file-type-hook ("tslisp") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Lisp/tree-sitter"))
(heml.tree-sitter:define-tree-sitter-language "commonlisp" :mode "Lisp/tree-sitter"
                                              :precedence :last)
(in-package :heml-demo)


;;;; Driving the editor, as test/smoke.lisp does

(defmacro main (&body body)
  `(heml.cocoa::call-on-main-thread-and-wait (lambda () ,@body)))

(defun settle ()
  (loop repeat 200 until (heml.cocoa::inbox-empty-p) do (sleep 0.05))
  (sleep 0.3))

(defun post (descriptor) (heml.cocoa::post-to-editor descriptor))
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

(defun open-file (name)
  (open-from-finder (merge-pathnames name *files*))
  (pause 0.8))

(defun type-lines (lines &key (pause 0.035))
  (dolist (line lines)
    (type-text line :pause pause)
    (post-named "Return")))

(defun run ()
  (loop until heml.cocoa::*editor-running-p* do (sleep 0.1))
  (main (objc:invoke (window) "setFrame:display:" (vector 80d0 80d0 1200d0 760d0) t))
  (settle)
  (start-recording)
  (pause 1)

  ;; C, coloured as it is typed.
  (open-file "fib.c")
  (type-lines '("/* Fibonacci, in C, coloured by tree-sitter. */"
                "#include <stdio.h>"
                ""
                "static int fib(int n) {"
                "    return n < 2 ? n : fib(n - 1) + fib(n - 2);"
                "}"
                ""
                "int main(void) {"
                "    printf(\"fib(20) = %d\\n\", fib(20));"
                "    return 0;"
                "}"))
  (pause 2)

  ;; Markdown.
  (open-file "notes.md")
  (type-lines '("# Heml"
                ""
                "An Emacs-style editor in Common Lisp."
                ""
                "## Highlighting"
                ""
                "- by major mode"
                "- with tree-sitter"
                ""
                "```"
                "make tree-sitter"
                "```"))
  (pause 2)

  ;; Python.
  (open-file "hello.py")
  (type-lines '("# Python, from Homebrew's grammar."
                "def greet(name, loud=False):"
                "    text = f\"Hello, {name}!\""
                "    return text.upper() if loud else text"
                ""
                "print(greet(\"Heml\", loud=True))"))
  (pause 2)

  ;; Text is not Lisp, and is no longer coloured as if it were.
  (open-file "plain.txt")
  (type-lines '("Plain text: it's not code; \"quotes\" and (parens) stay plain."))
  (pause 2)

  ;; The same Lisp, Heml's parser on the left, tree-sitter on the right.
  (open-file "both.lisp")
  (post-key #\x "Control") (post-key #\3)
  (pause 0.8)
  (open-file "both.tslisp")
  (post-key #\< "Meta")
  (pause 5)

  (setf *recording* nil)
  (sleep 0.3)
  (post :quit)
  (loop repeat 10
        while heml.cocoa::*editor-running-p*
        do (sleep 1)
           (when heml.cocoa::*editor-running-p* (post-key #\n))))

(defvar *driver*
  (bt:make-thread (lambda ()
                    (handler-case (run)
                      (error (condition)
                        (format t "~&demo: ~A~%" condition)
                        (setf *recording* nil)
                        (post :quit))))
                  :name "demo"))

(bt:make-thread (lambda () (sleep 300) (format t "~&demo: timed out~%") (sb-ext:exit :code 2 :abort t)))

(heml:heml nil :backend-type :cocoa :load-user-init nil)
(bt:join-thread *driver*)
(format t "~&demo: ~D frames in ~A~%" *frame* (namestring *frames*))
(sb-ext:exit :code 0 :abort t)
