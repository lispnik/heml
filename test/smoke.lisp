;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;;; test/smoke.lisp -- `make smoke`: the Cocoa editor, driven end to end.
;;;;
;;;; A helper thread drives the editor the way AppKit would -- real NSEvents
;;;; sent to the window, the menus' key equivalents, the application
;;;; delegate's messages -- and checks the editor's state after each step:
;;;; the text of the buffer, the region, the windows, the pasteboard.  Each
;;;; step also leaves a picture in build/smoke/, since a check says what the
;;;; editor holds and only a picture says what it drew.
;;;;
;;;; The run does not take the keyboard (HEML.COCOA:*ACTIVATE* is off),
;;;; uses a private pasteboard rather than the clipboard, and does not save
;;;; its font sizes, so it can run while someone works.  It exits 0 when
;;;; every check passes, 1 when one fails, and 2 when the watchdog finds it
;;;; hung.

(asdf:load-system :heml.cocoa)

(defpackage :heml-smoke (:use :common-lisp))
(in-package :heml-smoke)

(defvar *out*
  (merge-pathnames "build/smoke/" (asdf:system-source-directory :heml.cocoa)))

(defvar *failures* '())
(defvar *checks* 0)
(defvar *finished* nil)

;;; Projects' sessions are kept here, not in ~/.heml, and start empty.
(let ((state (merge-pathnames "state/" *out*)))
  (uiop:delete-directory-tree state :validate t :if-does-not-exist :ignore)
  (setf heml::*project-state-directory* state))

(setf heml.cocoa::*activate* nil
      heml.cocoa::*pasteboard-name* "org.lispnik.heml.smoke"
      heml.cocoa::*remember-font* nil
      heml.cocoa:*font-size* 13
      heml.cocoa:*font-name* nil)

(defun note (format &rest arguments)
  (format t "~&~?~%" format arguments)
  (finish-output))

(defmacro check (name form)
  `(let ((result (ignore-errors ,form)))
     (incf *checks*)
     (cond (result (note "  ok    ~A" ,name))
           (t (push ,name *failures*)
              (note "  FAIL  ~A" ,name)))))


;;;; Driving the editor

(defmacro main (&body body)
  "BODY on the main thread, waiting for it."
  `(heml.cocoa::call-on-main-thread-and-wait (lambda () ,@body)))

(defun settle ()
  "Wait until the editor has taken everything posted and redisplayed."
  (loop repeat 200
        until (heml.cocoa::inbox-empty-p)
        do (sleep 0.05))
  (sleep 0.4))

(defun post (descriptor) (heml.cocoa::post-to-editor descriptor))

(defun post-text (string)
  (loop for c across string
        do (post (if (char= c #\Newline)
                     (list :named "Return" '())
                     (list :char c '())))))

(defun post-key (character &rest modifiers)
  (post (list :char character modifiers)))

(defun extended-command (name)
  (post-key #\x "Meta")
  (post-text name)
  (post-text (string #\Newline)))

(defun display () heml.cocoa::*display*)
(defun window () (heml.cocoa::display-window (display)))
(defun view () (heml.cocoa::display-view (display)))

(defparameter *key-codes*
  '((#\a . 0) (#\s . 1) (#\d . 2) (#\f . 3) (#\h . 4) (#\g . 5) (#\z . 6)
    (#\x . 7) (#\c . 8) (#\v . 9) (#\b . 11) (#\q . 12) (#\w . 13) (#\e . 14)
    (#\r . 15) (#\y . 16) (#\t . 17) (#\= . 24) (#\- . 27) (#\0 . 29)
    (#\o . 31) (#\u . 32) (#\i . 34) (#\p . 35) (#\l . 37) (#\j . 38)
    (#\k . 40) (#\n . 45) (#\m . 46) (#\Space . 49)))

(defconstant +option+ (ash 1 19))
(defconstant +command+ (ash 1 20))
(defconstant +left-option+ (logior +option+ #x20))
(defconstant +shift+ (ash 1 17))

(defun key-event (characters unmodified flags)
  (objc:invoke "NSEvent"
               "keyEventWithType:location:modifierFlags:timestamp:windowNumber:context:characters:charactersIgnoringModifiers:isARepeat:keyCode:"
               10 (vector 0d0 0d0) flags 0d0 (objc:invoke (window) "windowNumber") nil
               characters unmodified nil
               (or (cdr (assoc (char-downcase (char unmodified 0)) *key-codes*)) 0)))

(defun press (characters &key (unmodified characters) (flags 0))
  "A key-down NSEvent, sent to the window."
  (main (objc:invoke (window) "sendEvent:" (key-event characters unmodified flags))))

(defun press-menu (character)
  "Cmd-CHARACTER, through the menu bar's key equivalents."
  (main (objc:invoke (objc:invoke (objc.runloop:shared-application) "mainMenu")
                     "performKeyEquivalent:"
                     (key-event (string character) (string character) +command+))))

(defun mouse-event (type column line flags clicks)
  (let* ((display (display))
           (point (vector (float (+ heml.cocoa::*margin*
                                    (* (+ column 1/2) (heml.cocoa::display-char-width display)))
                                 1d0)
                          (float (+ heml.cocoa::*margin*
                                    (* (+ line 1/2) (heml.cocoa::display-char-height display)))
                                 1d0)))
           )
    (objc:invoke "NSEvent"
                 "mouseEventWithType:location:modifierFlags:timestamp:windowNumber:context:eventNumber:clickCount:pressure:"
                 (ecase type (:down 1) (:up 2) (:drag 6) (:right-down 3))
                 (objc:invoke (view) "convertPoint:toView:" point nil)
                 flags 0d0 (objc:invoke (window) "windowNumber") nil 0 clicks 1.0)))

(defun mouse (type column line &key (flags 0) (clicks 1))
  "A mouse NSEvent at the middle of cell COLUMN, LINE, sent to the window."
  (main (objc:invoke (window) "sendEvent:" (mouse-event type column line flags clicks))))

(defun choose-menu-item (menu title)
  "Choose the item TITLE of the menu bar's menu MENU, as a click on it would."
  (main (let* ((submenu (objc:invoke (objc:invoke (objc:invoke (objc.runloop:shared-application)
                                                                "mainMenu")
                                                   "itemWithTitle:" menu)
                                      "submenu"))
               (index (objc:invoke submenu "indexOfItemWithTitle:" title)))
          (assert (>= index 0) () "No item ~S in the ~A menu." title menu)
          (objc:invoke submenu "performActionForItemAtIndex:" index))))

(defvar *shot* 0)

(defun shot (name)
  (let ((path (namestring (merge-pathnames (format nil "~2,'0D-~A.png" (incf *shot*) name)
                                           *out*))))
    (main
      (let* ((bounds (objc:invoke (view) "bounds"))
             (rep (objc:invoke (view) "bitmapImageRepForCachingDisplayInRect:" bounds)))
        (objc:invoke (view) "cacheDisplayInRect:toBitmapImageRep:" bounds rep)
        (objc:invoke (objc:invoke rep "representationUsingType:properties:" 4
                                  (objc:invoke "NSDictionary" "dictionary"))
                     "writeToFile:atomically:" path t)))))

(defun wait-until (predicate &optional (seconds 20))
  (loop repeat (* seconds 10)
        when (ignore-errors (funcall predicate)) return t
        do (sleep 0.1)))


;;;; Reading the editor's state, after SETTLE, while it waits for input

(defun buffer-text (&optional (buffer (hi::current-buffer)))
  (hi::region-to-string (hi::buffer-region buffer)))

(defun region-text () (hi::region-to-string (heml::current-region nil nil)))

(defun pasteboard-text ()
  (main (let ((string (objc:invoke (heml.cocoa::general-pasteboard)
                                   "stringForType:" "public.utf8-plain-text")))
          (and (not (cffi:null-pointer-p string)) (objc:ns-string-to-string string t)))))

(defun point-column () (hi::mark-column (hi::current-point)))

(defun menu-hidden-p (title)
  (main (objc:invoke-bool (objc:invoke (objc:invoke (objc.runloop:shared-application) "mainMenu")
                                       "itemWithTitle:" title)
                          "isHidden")))

(defun row-runs-containing (text)
  "The font runs of the first screen row that shows TEXT, or :NONE."
  (let ((row (find-if (lambda (row) (search text (heml.cocoa::row-text row)))
                      (heml.cocoa::screen-rows heml.cocoa::*screen*))))
    (if row (heml.cocoa::row-runs row) :none)))

(defun run-font-at (text offset)
  "The font drawn at OFFSET characters into TEXT, where TEXT is on screen."
  (let ((row (find-if (lambda (row) (search text (heml.cocoa::row-text row)))
                      (heml.cocoa::screen-rows heml.cocoa::*screen*))))
    (when row
      (let ((column (+ offset (search text (heml.cocoa::row-text row)))))
        (loop for (start end . font) in (heml.cocoa::row-runs row)
              when (and (<= start column) (< column end)) return font)))))


;;;; The run

(defun run ()
  (ensure-directories-exist *out*)
  (sleep 2)
  (note "typing")
  (post-text "(defun hello (name)
  (format t \"Hello, ~A!~%\" name))
café λ 日本語 end")
  (settle)
  (check "posted text is in the buffer"
         (search "café λ 日本語 end" (buffer-text)))
  (check "a wide character takes two columns"
         (= (point-column) (+ (length "café λ ") (* 3 2) (length " end"))))
  (shot "typed")
  (check "a buffer that is not in Lisp mode is not coloured as Lisp"
         (null (row-runs-containing "(format t")))

  (note "key events")
  (post-text (string #\Newline))
  (loop for c across "keys ok" do (press (string c)))
  (settle)
  (check "NSEvent key-downs type through the input context"
         (search "keys ok" (buffer-text)))
  (press "≈" :unmodified "x" :flags +left-option+)
  (settle)
  (check "left Option is Meta: M-x prompts"
         (eq hi::*current-window* hi::*echo-area-window*))
  (shot "meta-x")
  (post-key #\g "Control")
  (settle)
  (main (objc:invoke (view) "setMarkedText:selectedRange:replacementRange:"
                     "にほん" (cons 3 0) (cons cocoa:ns-not-found 0)))
  (settle)
  (check "marked text is held, not inserted"
         (and (heml.cocoa::display-marked-text (display))
              (not (search "にほん" (buffer-text)))))
  (shot "marked-text")
  (main (objc:invoke (view) "insertText:replacementRange:" "日本" (cons cocoa:ns-not-found 0)))
  (settle)
  (check "committed text is inserted"
         (and (null (heml.cocoa::display-marked-text (display)))
              (search "keys ok日本" (buffer-text))))

  (note "windows")
  (post-key #\< "Meta")
  (post-key #\x "Control") (post-key #\2)
  (settle)
  (check "C-x 2 shows the buffer in two windows"
         (= 2 (length (hi::buffer-windows (hi::current-buffer)))))
  (shot "split")
  (let ((columns (heml.cocoa::screen-columns heml.cocoa::*screen*)))
    (main (objc:invoke (window) "setFrame:display:" (vector 100d0 100d0 640d0 520d0) t))
    (settle)
    (check "resizing the window changes the grid"
           (/= columns (heml.cocoa::screen-columns heml.cocoa::*screen*))))
  (shot "resized")
  (post-key #\x "Control") (post-key #\1)
  (main (objc:invoke (window) "setFrame:display:" (vector 100d0 100d0 900d0 700d0) t))
  (settle)

  (note "mouse")
  (mouse :down 7 0)
  (mouse :drag 10 0)
  (mouse :up 12 0)
  (settle)
  (check "a drag marks a region" (heml::region-active-p))
  (check "the region is what was dragged over" (equal (region-text) "hello"))
  (shot "drag")
  (mouse :down 3 1)
  (mouse :up 3 1)
  (mouse :down 3 1 :clicks 2)
  (mouse :up 3 1 :clicks 2)
  (settle)
  (check "a double click selects a word" (equal (region-text) "format"))
  (mouse :down 3 1 :clicks 3)
  (mouse :up 3 1 :clicks 3)
  (settle)
  (check "a triple click selects a line"
         (equal (region-text) (format nil "  (format t \"Hello, ~~A!~~%\" name))~%")))
  (shot "triple-click")
  (let ((menu (main (objc:invoke (view) "menuForEvent:" (mouse-event :right-down 5 1 0 1)))))
    (settle)
    (check "a right click has a menu"
           (and (not (cffi:null-pointer-p menu))
                (main (plusp (objc:invoke menu "numberOfItems"))))))
  (check "a right click in the selection keeps it"
         (and (heml::region-active-p) (search "format" (region-text))))
  (main (objc:invoke (view) "menuForEvent:" (mouse-event :right-down 2 0 0 1)))
  (settle)
  (check "a right click elsewhere moves point" (= (point-column) 2))
  (mouse :down 7 0) (mouse :drag 12 0) (mouse :up 12 0)
  (settle)

  (note "clipboard")
  (press-menu #\c)
  (settle)
  (check "Cmd-C puts the region on the pasteboard" (equal (pasteboard-text) "hello"))
  (main (heml.cocoa::write-pasteboard "from elsewhere")
        ;; As if another application had written it.
        (setf heml.cocoa::*pasteboard-change-count* nil))
  (post-key #\> "Meta")
  (press-menu #\v)
  (settle)
  (check "Cmd-V pastes what another application copied"
         (search "from elsewhere" (buffer-text)))
  (shot "pasted")

  (note "opening files")
  (let ((file (merge-pathnames "opened.lisp" *out*)))
    (with-open-file (out file :direction :output :if-exists :supersede
                              :external-format :utf-8)
      (write-line ";;; Opened from Finder, λ." out))
    (main (let ((url (objc:invoke "NSURL" "fileURLWithPath:" (namestring file))))
            (objc:invoke (objc:objc-object-pointer (heml.cocoa::display-app-delegate (display)))
                         "application:openURLs:"
                         (objc.runloop:shared-application)
                         (objc:invoke "NSArray" "arrayWithObject:" url))))
    (settle)
    (check "application:openURLs: visits the file"
           (and (equal (truename (hi::buffer-pathname (hi::current-buffer)))
                       (truename file))
                (search "Opened from Finder, λ." (buffer-text))))
    (check "a Lisp file is coloured: its comment is red"
           (eql 1 (run-font-at ";;; Opened" 0))))
  (shot "opened")

  (note "tree-sitter")
  (let ((file (merge-pathnames "opened.c" *out*)))
    (with-open-file (out file :direction :output :if-exists :supersede)
      (write-line "/* a C comment */" out)
      (write-line "int main(void) { return 0; }" out))
    (post (list :open (namestring file)))
    (settle)
    (check "a C file is in C mode"
           (equal "C" (hi::buffer-major-mode (hi::current-buffer))))
    (if (and (heml.tree-sitter:tree-sitter-available-p)
             (heml.tree-sitter::find-in-directories "lib/libtree-sitter-c.dylib"))
        (progn
          (check "tree-sitter colours its comment"
                 (eql 1 (run-font-at "/* a C comment" 0)))
          (check "and its type"
                 (eql 2 (run-font-at "int main" 0)))
          (shot "tree-sitter"))
        (note "  skip  tree-sitter colouring: no tree-sitter or C grammar (make tree-sitter)")))
  (let ((file (merge-pathnames "styles.md" *out*)))
    (with-open-file (out file :direction :output :if-exists :supersede)
      (write-line "Some *slanted* and **heavy** text." out))
    (post (list :open (namestring file)))
    (settle)
    (when (heml.tree-sitter::find-in-directories "lib/libtree-sitter-markdown_inline.dylib")
      (check "Markdown's emphasis is italic and its strong emphasis bold"
             (and (getf (run-font-at "*slanted*" 1) :italic)
                  (getf (run-font-at "**heavy**" 2) :bold)))))
  (let ((file (merge-pathnames "links.txt" *out*))
        (target (merge-pathnames "linked.txt" *out*)))
    (with-open-file (out target :direction :output :if-exists :supersede)
      (write-line "the linked file" out))
    (with-open-file (out file :direction :output :if-exists :supersede)
      (write-line "Go [there](linked.txt) now." out))
    (post (list :open (namestring file)))
    (settle)
    (check "a link is underlined, in the link colour"
           (let ((font (run-font-at "[there]" 1)))
             (and (getf font :underline) (getf font :link))))
    (let* ((rows (heml.cocoa::screen-shown-rows heml.cocoa::*screen*))
           (line (position-if (lambda (row) (search "[there]" (heml.cocoa::row-text row))) rows))
           (column (and line (search "[there]" (heml.cocoa::row-text (svref rows line))))))
      (when line
        (mouse :down (+ column 2) line :flags +command+)
        (mouse :up (+ column 2) line :flags +command+)
        (settle)))
    (check "Command-click follows it"
           (let ((pathname (hi::buffer-pathname (hi::current-buffer))))
             (and pathname (equal "linked.txt" (file-namestring pathname))))))
  (let ((file (merge-pathnames "greet" *out*)))
    (with-open-file (out file :direction :output :if-exists :supersede)
      (write-line "#!/bin/sh" out)
      (write-line "# say hello" out)
      (write-line "echo \"hello $1\"" out))
    (post (list :open (namestring file)))
    (settle)
    (check "a script without a type is in the mode its #! line names"
           (equal "Shell Script" (hi::buffer-major-mode (hi::current-buffer))))
    (when (heml.tree-sitter::find-in-directories "lib/libtree-sitter-bash.dylib")
      (check "tree-sitter colours a shell script's comment and string"
             (and (eql 1 (run-font-at "# say hello" 0))
                  (eql 4 (run-font-at "\"hello $1" 0))))))
  (let ((file (merge-pathnames "hello.pas" *out*)))
    (with-open-file (out file :direction :output :if-exists :supersede)
      (write-line "program Hello;" out)
      (write-line "begin WriteLn('hi') end." out))
    (post (list :open (namestring file)))
    (settle)
    (check "a Pascal file is in Pascal mode"
           (equal "Pascal" (hi::buffer-major-mode (hi::current-buffer))))
    (when (heml.tree-sitter::find-in-directories "lib/libtree-sitter-pascal.dylib")
      (check "tree-sitter colours Pascal's keywords"
             (eql 5 (run-font-at "program Hello" 0)))))
  (let ((file (merge-pathnames "block.lisp" *out*)))
    (with-open-file (out file :direction :output :if-exists :supersede)
      (write-line "#| a block comment |#" out)
      (write-line "(defun f (x &key y) (list x y :key))" out))
    (post (list :open (namestring file)))
    (settle)
    (if (heml.tree-sitter::find-in-directories "lib/libtree-sitter-commonlisp.dylib")
        (progn
          (check "tree-sitter colours Lisp: a block comment is red"
                 (eql 1 (run-font-at "#| a block" 0)))
          (check "and &key is a keyword"
                 (eql 5 (run-font-at "&key y" 0))))
        (check "without tree-sitter's grammar, Heml's own parser colours Lisp"
               (run-font-at "(defun f" 1))))

  (note "modes")
  (flet ((file-buffer (name)
           (find name hi::*buffer-list*
                 :key (lambda (b) (and (hi::buffer-pathname b) (file-namestring (hi::buffer-pathname b))))
                 :test #'equal))
         (point-line () (hi::line-string (hi::mark-line (hi::current-point))))
         (write-file (name &rest lines)
           (with-open-file (out (merge-pathnames name *out*) :direction :output :if-exists :supersede)
             (dolist (line lines) (write-line line out)))
           (merge-pathnames name *out*)))
    (post (list :open (namestring (write-file "grep.txt" "alpha one" "beta two" "alpha three"))))
    (settle)
    (extended-command "Grep")
    ;; The prompt holds its default, grep -nH -e, to add to.
    (post-text "alpha grep.txt
")
    (check "Grep lists what it finds, and says how many"
           (wait-until (lambda () (search "Grep finished: 2 results." (buffer-text)))))
    (settle)
    (check "Grep colours the file, the line number and the match"
           (and (eql 5 (run-font-at "grep.txt:1:alpha" 0))
                (eql 2 (run-font-at "grep.txt:1:alpha" 9))
                (equal '(:fg 1 :bold t) (run-font-at "grep.txt:1:alpha" 11))))
    (post-key #\n)
    (post (list :named "Return" '()))
    (settle)
    (check "n and Return visit the first match"
           (and (eq (hi::current-buffer) (file-buffer "grep.txt"))
                (equal "alpha one" (point-line))))
    (post-key #\x "Control") (post-key #\`)
    (settle)
    (check "C-x ` visits the next"
           (equal "alpha three" (point-line)))
    (post-key #\x "Control") (post-key #\o)
    (post-key #\x "Control") (post-key #\q "Control")
    (settle)
    (check "C-x C-q makes the lines editable"
           (equal "Wgrep" (hi::buffer-major-mode (hi::current-buffer))))
    (post-key #\< "Meta")
    (extended-command "Replace String")
    (post-text "alpha
gamma
")
    (post-key #\c "Control") (post-key #\c "Control")
    (settle)
    (check "C-c C-c writes the lines changed to their file's buffer"
           (let ((text (buffer-text (file-buffer "grep.txt"))))
             (and (search "gamma one" text) (search "gamma three" text) (search "beta two" text))))
    (check "and goes back to Grep"
           (equal "Grep" (hi::buffer-major-mode (hi::current-buffer))))
    (post-key #\x "Control") (post-key #\1)
    (post (list :open (namestring (write-file "tb.py" "def f():" "    raise ValueError('x')" "" "f()"))))
    (settle)
    (extended-command "Compile")
    ;; Its default, make -k, goes.
    (post-key #\a "Control") (post-key #\k "Control")
    (post-text "python3 tb.py
")
    (check "Compile lists what a run prints"
           (wait-until (lambda () (search "Compilation finished" (buffer-text)))))
    (post-key #\x "Control") (post-key #\`)
    (settle)
    (check "C-x ` visits the place a traceback names"
           (and (eq (hi::current-buffer) (file-buffer "tb.py"))
                (member (point-line) '("f()" "    raise ValueError('x')") :test #'equal)))
    (post-key #\x "Control") (post-key #\1)
    (when (heml.tree-sitter::find-in-directories "lib/libtree-sitter-python.dylib")
      (extended-command "Outline")
      (settle)
      (check "Outline lists a Python file's definitions"
             (and (equal "Outline" (hi::buffer-major-mode (hi::current-buffer)))
                  (search "def f():" (buffer-text))))
      (post-key #\x "Control") (post-key #\1))
    (when (heml.tree-sitter::find-in-directories "lib/libtree-sitter-c.dylib")
      (post (list :open (namestring (write-file "defs.c" "int a(void) {" "  return 1;" "}"
                                                "int b(void) {" "  return 2;" "}"))))
      (settle)
      (post-key #\> "Meta")
      (post-key #\a "Control" "Meta")
      (settle)
      (check "C-M-a goes to the start of the C function before"
             (equal "int b(void) {" (point-line)))
      (post-key #\a "Control" "Meta")
      (post-key #\e "Control" "Meta")
      (settle)
      (check "C-M-e goes past the end of this one"
             (equal "int b(void) {" (point-line))))
    (when (probe-file "/opt/homebrew/bin/fpc")
      (post (list :open (namestring (write-file "bad.pas" "program bad;" "begin" "  x := 1;" "end."))))
      (settle)
      (post-key #\c "Control") (post-key #\c "Control")
      (check "C-c C-c compiles Pascal with fpc"
             (wait-until (lambda () (search "Compilation finished" (buffer-text)))))
      (post-key #\x "Control") (post-key #\`)
      (settle)
      (check "and C-x ` visits its error"
             (and (eq (hi::current-buffer) (file-buffer "bad.pas"))
                  (equal "  x := 1;" (point-line))))
      (post-key #\x "Control") (post-key #\1))
    (post (list :open (namestring (write-file "heads.md" "# One" "text" "## Two" "more"))))
    (settle)
    (post-key #\c "Control") (post-key #\n "Control")
    (settle)
    (check "C-c C-n goes to Markdown's next heading"
           (equal "## Two" (point-line)))
    (post-key #\c "Control") (post-key #\=)
    (settle)
    (check "C-c = demotes it"
           (equal "### Two" (point-line)))
    (post (list :open (namestring (write-file "fill.md" "Intro." ""
                                              (concatenate 'string "- " (format nil "~{~A~^ ~}" (loop repeat 30 collect "word")))
                                              "- next item"))))
    (settle)
    (post-key #\n "Control") (post-key #\n "Control")
    (post-key #\q "Meta")
    (settle)
    (check "M-q fills a Markdown list item, its lines hung under its text"
           (let ((lines (uiop:split-string (buffer-text) :separator (string #\Newline))))
             (and (every (lambda (l) (<= (length l) 75)) lines)
                  (eql 0 (search "- word" (third lines)))
                  (eql 0 (search "  word" (fourth lines)))
                  (member "- next item" lines :test #'equal))))
    (extended-command "Recursive Grep")
    (post-key #\a "Control") (post-key #\k "Control")
    (post-text "beta two
")
    (post (list :named "Return" '()))
    (check "Recursive Grep searches the directory"
           (wait-until (lambda () (and (search "Grep finished" (buffer-text))
                                       (search "grep.txt:2:beta two" (buffer-text))))))
    (post-key #\x "Control") (post-key #\1)
    ;; Completion at point: a popup under the word, which typing narrows.
    (with-open-file (out (merge-pathnames "comp.txt" *out*) :direction :output :if-exists :supersede)
      (write-line "zebraone zebratwo" out)
      (write-string "zeb" out))
    (post (list :open (namestring (merge-pathnames "comp.txt" *out*))))
    (settle)
    ;; To the end of zeb, on the last line but one.
    (post-key #\> "Meta")
    (post-key #\b "Control")
    (post-key #\i "Control" "Meta")
    (flet ((popup-row-font (text)
             ;; The font of the popup's row showing TEXT, a row that starts
             ;; nothing else, or NIL when there is no such row.
             (let* ((bare (string-trim " " text))
                    (row (find-if (lambda (row)
                                    (string= bare (string-trim " " (heml.cocoa::row-text row))))
                                  (heml.cocoa::screen-rows heml.cocoa::*screen*))))
               (when row
                 (let ((column (search bare (heml.cocoa::row-text row))))
                   (loop for (start end . font) in (heml.cocoa::row-runs row)
                         when (and (<= start column) (< column end)) return font))))))
      (check "C-M-i shows the completions in a popup under the word"
             (wait-until (lambda ()
                           (and (equal heml::*popup-selected-font* (popup-row-font " zebraone "))
                                (equal heml::*popup-font* (popup-row-font " zebratwo "))))
                         10))
      (shot "completion")
      (post-text "rat")
      (settle)
      (check "typing narrows them"
             (and (equal heml::*popup-selected-font* (popup-row-font " zebratwo "))
                  (null (popup-row-font " zebraone "))))
      (post (list :named "Return" '()))
      (settle)
      (check "and Return puts the chosen one in"
             (and (search (format nil "~%zebratwo") (buffer-text))
                  (null hi::*popup*)
                  (null (popup-row-font " zebratwo "))))
      ;; As one types, when that is asked for: Tab puts the choice in.
      (extended-command "Complete as You Type")
      (settle)
      (post-key #\e "Control")
      (post-text " zeb")
      (settle)
      (check "with Complete as You Type on, the popup comes up by itself"
             (wait-until (lambda () (popup-row-font " zebraone ")) 10))
      (post (list :named "Tab" '()))
      (settle)
      (check "and Tab puts the choice in"
             (search "zebratwo zebraone" (buffer-text)))
      (extended-command "Complete as You Type")
      (settle)
      ;; Lisp's symbols say what they name.
      (post (list :open (namestring (write-file "kinds.lisp" "(mapc"))))
      (settle)
      (post-key #\e "Control")
      (post-key #\i "Control" "Meta")
      (check "a Lisp symbol's completions say what each names"
             (wait-until (lambda ()
                           (find-if (lambda (row)
                                      (let ((text (heml.cocoa::row-text row)))
                                        (and (search " mapcar " text) (search "function" text))))
                                    (heml.cocoa::screen-rows heml.cocoa::*screen*)))
                         10))
      (post (list :named "Escape" '()))
      (settle)
      ;; The file prompt: the files that start so, at the foot of the window.
      (post-key #\x "Control") (post-key #\f "Control")
      (post-text "gre")
      (post (list :named "Tab" '()))
      (check "at a file prompt, Tab shows the files that start so in a popup"
             (wait-until (lambda () (and (popup-row-font " greet") (popup-row-font " grep.txt"))) 10))
      (post-text "p")
      (post (list :named "Return" '()))
      (post (list :named "Return" '()))
      (check "and the one chosen is visited"
             (wait-until (lambda ()
                           (equal (hi::buffer-pathname (hi::current-buffer))
                                  (merge-pathnames "grep.txt" *out*)))
                         10)))
    (post (list :open (namestring (write-file "words.txt" "Hello wrold here"))))
    (settle)
    (extended-command "Auto Spell Mode")
    (settle)
    (check "Spell mode underlines a misspelled word in red, and not the others"
           (and (equal '(:fg 1 :underline t) (run-font-at "wrold" 1))
                (null (run-font-at "Hello" 1)))))

  (note "language servers")
  ;; A stand-in server (test/fake-lsp.py), which says the same every time,
  ;; serves Pascal for the run.
  (heml::define-language-server
   "Pascal" (list (list "python3"
                        (namestring (merge-pathnames "test/fake-lsp.py"
                                                     (asdf:system-source-directory :heml.cocoa)))))
   :language-id "pascal")
  (with-open-file (out (merge-pathnames "fake.pas" *out*) :direction :output :if-exists :supersede)
    (write-line "program fake;" out)
    (write-line "  wrongthing here" out)
    (write-line "begin end." out))
  (post (list :open (namestring (merge-pathnames "fake.pas" *out*))))
  (settle)
  (flet ((point-line () (hi::line-string (hi::mark-line (hi::current-point)))))
    (check "a language server's error is underlined where it is"
           (wait-until (lambda ()
                         (and (getf (run-font-at "wrongthing here" 1) :underline)
                              (not (getf (run-font-at "wrongthing here" 7) :underline))))
                       30))
    (shot "language-server")
    (post-key #\n "Control")
    (post-key #\c "Control") (post-key #\d "Control")
    (check "C-c C-d says what is wrong there, and what the server says of it, in a popup"
           (wait-until (lambda ()
                         (let ((rows (map 'list #'heml.cocoa::row-text
                                          (heml.cocoa::screen-rows heml.cocoa::*screen*))))
                           (and (find " fake error" rows :test #'search)
                                (find " fake hover text" rows :test #'search))))
                       10))
    (post (list :named "Escape" '()))
    (settle)
    (check "which the next key puts away"
           (null hi::*popup*))
    (post-key #\. "Meta")
    (check "M-. goes to the definition the server names"
           (wait-until (lambda () (equal "begin end." (point-line))) 10))
    (post-key #\? "Meta")
    (check "M-? lists the references it names"
           (wait-until (lambda ()
                         (and (equal "*References*" (hi::buffer-name (hi::current-buffer)))
                              (search "fake.pas:1: program fake;" (buffer-text))
                              (search "fake.pas:3: begin end." (buffer-text))))
                       10))
    (post (list :named "Return" '()))
    (settle)
    (check "and Return visits one"
           (equal "program fake;" (point-line)))
    (post-key #\x "Control") (post-key #\1)
    (post-key #\> "Meta")
    (post-text "fake_")
    (post-key #\i "Control" "Meta")
    (check "the server's completions are in the popup, each with its kind"
           (wait-until (lambda ()
                         (find-if (lambda (row)
                                    (let ((text (heml.cocoa::row-text row)))
                                      (and (search " fake_function " text) (search "function" text :start2 16))))
                                  (heml.cocoa::screen-rows heml.cocoa::*screen*)))
                       10))
    (post (list :named "Return" '()))
    (settle)
    (check "and the one chosen is put in"
           (search "fake_function" (buffer-text)))
    ;; Only the span that changed is sent: the server's copy keeps up, here
    ;; through a line added at the end with a character past the BMP, then
    ;; one changed at the start.
    (check "the change between two texts is one span of the old"
           (flet ((changed (old new)
                    ;; OLD with the span TEXT-CHANGE names replaced.
                    (multiple-value-bind (sl sc el ec text) (heml::text-change old new)
                      (flet ((index (line character)
                               (let ((i 0) (units 0))
                                 (dotimes (k line) (setf i (1+ (position #\Newline old :start i))))
                                 (loop while (< units character)
                                       do (incf units (if (> (char-code (char old i)) #xFFFF) 2 1))
                                          (incf i))
                                 i)))
                        (concatenate 'string (subseq old 0 (index sl sc)) text
                                     (subseq old (index el ec)))))))
             (let ((face (string (code-char #x1F600))))
               (every (lambda (pair) (string= (second pair) (changed (first pair) (second pair))))
                      (list (list "abc" "abXc")
                            (list (format nil "one~%two~%three") (format nil "one~%2~%three"))
                            (list (format nil "one~%two~%") (format nil "one~%"))
                            (list "" "new")
                            (list "same" "same")
                            (list (format nil "a~Ab~%c" face) (format nil "a~Ab~%Xc" face))
                            (list (format nil "aaa~%aaa") (format nil "aaa~%aaa~%aaa")))))))
    (post-key #\> "Meta")
    (post-text (format nil "~%tail ~A end" (code-char #x1F600)))
    (post-key #\< "Meta")
    (post-text "X")
    (post-key #\c "Control") (post-key #\d "Control")
    (check "after edits, the server has the text the buffer has"
           (wait-until (lambda ()
                         (let ((rows (map 'list #'heml.cocoa::row-text
                                          (heml.cocoa::screen-rows heml.cocoa::*screen*)))
                               (lines (uiop:split-string (buffer-text) :separator (string #\Newline))))
                           (and (find (format nil " first: ~A" (first lines)) rows :test #'search)
                                (find (format nil " length: ~D" (length (buffer-text))) rows
                                      :test #'search))))
                       10))
    (post (list :named "Escape" '()))
    (settle)
    (post-key #\d "Control")
    (settle)
    (extended-command "LSP Rename")
    (post-key #\a "Control") (post-key #\k "Control")
    (post-text "renamed
")
    (check "LSP Rename makes the server's edits"
           (wait-until (lambda () (search "renamedgram fake;" (buffer-text))) 10))
    ;; What the server can do about the error: an edit, and a command of
    ;; its own, which asks for its edit to be made.
    (flet ((popup-row-p (text)
             (find text (map 'list #'heml.cocoa::row-text
                             (heml.cocoa::screen-rows heml.cocoa::*screen*))
                   :test #'search)))
      (post-key #\< "Meta")
      (post-key #\n "Control")
      (post-key #\c "Control") (post-key #\a "Control")
      (check "C-c C-a offers what the server can do, in a popup"
             (wait-until (lambda () (and (popup-row-p " 1  Fix the fake error")
                                         (popup-row-p " 2  Run a command")))
                         10))
      (post (list :named "Return" '()))
      (check "choosing a fix makes its edit"
             (wait-until (lambda () (search "  rightthing here" (buffer-text))) 10))
      (post-key #\c "Control") (post-key #\a "Control")
      (wait-until (lambda () (popup-row-p " 2  Run a command")) 10)
      (post-key #\2)
      (check "and choosing a command makes the edit the server asks for"
             (wait-until (lambda () (eql 0 (search "{ done }" (buffer-text)))) 10)))
    (extended-command "Outline")
    (check "Outline lists the symbols the server names, with their kinds"
           (wait-until (lambda ()
                         (and (equal "*Outline*" (hi::buffer-name (hi::current-buffer)))
                              (search "function fake_symbol" (buffer-text))
                              (search "  variable inner" (buffer-text))))
                       10))
    (post-key #\x "Control") (post-key #\1)
    (post (list :open (namestring (merge-pathnames "fake.pas" *out*))))
    (settle)
    ;; Left unsaved, and not asked about on the way out.
    (setf (hi::buffer-modified (hi::current-buffer)) nil))

  (note "projects")
  (let ((root (merge-pathnames "proj/" *out*)))
    (flet ((file (name &rest lines)
             (let ((path (merge-pathnames name root)))
               (ensure-directories-exist path)
               (with-open-file (out path :direction :output :if-exists :supersede)
                 (dolist (line lines) (write-line line out)))
               path))
           (file-shown-p (name)
             (find name (remove hi::*echo-area-window* hi::*window-list*)
                   :key (lambda (w) (let ((p (hi::buffer-pathname (hi::window-buffer w))))
                                      (and p (file-namestring p))))
                   :test #'equal))
           (line-text () (hi::line-string (hi::mark-line (hi::current-point))))
           (current-file () (let ((p (hi::buffer-pathname (hi::current-buffer))))
                              (and p (file-namestring p)))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)
      (ensure-directories-exist (merge-pathnames ".git/" root))
      (file "src/a.c" "int a(void) {" "  return 1;" "}")
      (file "src/b.c" "int b(void) {" "  return 2;" "}")
      (file "README.md" "# proj")
      (file ".heml-project"
            "(:name \"Proj X\" :ignore (\"README.md\") :variables ((\"Fill Column\" . 42)))")
      (post-key #\x "Control") (post-key #\1)
      (post (list :open (namestring (merge-pathnames "src/a.c" root))))
      (settle)
      (check "a project's file names the project in its modeline, as its settings name it"
             (not (eq :none (row-runs-containing "[Proj X]"))))
      (check "its settings' variables are set in its buffers"
             (eql 42 (hi::variable-value 'heml::fill-column :buffer (hi::current-buffer))))
      ;; Asked of the editor's thread: listing files runs a program, and so
      ;; may the editor at any moment, which two threads must not do at once.
      (setf *project-files* :unknown)
      (post (list :command "Smoke Project Files"))
      (check "and its files are found, less those its settings ignore"
             (wait-until (lambda ()
                           (and (listp *project-files*)
                                (member "src/a.c" *project-files* :test #'equal)
                                (not (member "README.md" *project-files* :test #'equal))))
                         20))
      (check "text finds files with its characters in order, the best first"
             (equal "src/tree-sitter-modes.lisp"
                    (first (heml::fuzzy-file-matches
                            "tsm" '("test/smoke.lisp" "notes/tasks-more.md"
                                    "src/tree-sitter-modes.lisp")))))
      (post-key #\x "Control") (post-key #\p) (post-key #\f)
      (post-text "sbc
")
      (settle)
      (check "C-x p f finds a file from text in its name"
             (wait-until (lambda () (equal "b.c" (current-file))) 10))
      (post-key #\x "Control") (post-key #\p) (post-key #\f)
      (post-text ".c
")
      (settle)
      (check "and lists the files when several have it"
             (wait-until (lambda ()
                           (and (equal "*Project Files*" (hi::buffer-name (hi::current-buffer)))
                                (search "src/a.c" (buffer-text)) (search "src/b.c" (buffer-text))))
                         10))
      (post-key #\q)
      (post-key #\x "Control") (post-key #\p) (post-key #\g)
      (post-key #\a "Control") (post-key #\k "Control")
      (post-text "return
")
      (check "C-x p g searches the project, from its root"
             (wait-until (lambda () (and (search "Grep finished: 2 results." (buffer-text))
                                         (search (namestring root) (buffer-text))))))
      ;; A session: a.c at its second line, beside b.c.
      (post-key #\x "Control") (post-key #\1)
      (post (list :open (namestring (merge-pathnames "src/a.c" root))))
      (settle)
      (post-key #\n "Control")
      (post-key #\x "Control") (post-key #\3)
      (post-key #\x "Control") (post-key #\p) (post-key #\f)
      (post-text "src/b.c
")
      (settle)
      (extended-command "Save Project Session")
      (post-key #\x "Control") (post-key #\1)
      (extended-command "Kill Project Buffers")
      (post-key #\y)
      ;; An :open is taken at once, ahead of keys still queued: wait for them.
      (settle)
      (settle)
      (check "Kill Project Buffers kills them"
             (not (find "a.c" hi::*buffer-list* :key #'hi::buffer-name :test #'search)))
      (post (list :open (namestring (merge-pathnames "README.md" root))))
      (settle)
      (extended-command "Restore Project Session")
      (settle)
      (check "Restore Project Session reopens the files, in their windows"
             (and (file-shown-p "a.c") (file-shown-p "b.c")
                  (= 2 (length (remove hi::*echo-area-window* hi::*window-list*)))))
      (check "with their points"
             (let ((w (file-shown-p "a.c")))
               (and w (equal "  return 1;"
                             (hi::line-string (hi::mark-line (hi::window-point w)))))))
      ;; A window on one of the project's directories comes back too.
      (post-key #\x "Control") (post-key #\p) (post-key #\d)
      (settle)
      (extended-command "Save Project Session")
      (post-key #\x "Control") (post-key #\1)
      (extended-command "Kill Project Buffers")
      (post-key #\y)
      ;; An :open is taken at once, ahead of keys still queued: wait for them.
      (settle)
      (post (list :open (namestring (merge-pathnames "README.md" root))))
      (settle)
      (extended-command "Restore Project Session")
      (settle)
      (check "a Dired window of the project is reopened with its session"
             (find "Dired" (remove hi::*echo-area-window* hi::*window-list*)
                   :key (lambda (w) (hi::buffer-major-mode (hi::window-buffer w)))
                   :test #'equal))
      ;; Back to the two files side by side, for what follows.
      (post-key #\x "Control") (post-key #\1)
      (post (list :open (namestring (merge-pathnames "src/a.c" root))))
      (settle)
      (post-key #\x "Control") (post-key #\3)
      (post-key #\x "Control") (post-key #\p) (post-key #\f)
      (post-text "src/b.c
")
      (settle)
      (extended-command "Save Project Session")
      ;; Switching away and back reopens it without asking.
      (extended-command "Kill Project Buffers")
      (post-key #\y)
      ;; An :open is taken at once, ahead of keys still queued: wait for them.
      (settle)
      (post-key #\x "Control") (post-key #\1)
      (post (list :open (namestring (merge-pathnames "README.org"
                                                     (asdf:system-source-directory :heml.cocoa)))))
      (settle)
      (post-key #\x "Control") (post-key #\p) (post-key #\p)
      (post (list :named "Return" '()))
      (settle)
      (check "C-x p p back to the project reopens its session"
             (and (file-shown-p "a.c") (file-shown-p "b.c")))
      ;; The project's compile commands are remembered, for M-p at the prompt.
      (dolist (command '("echo one" "echo two"))
        (post-key #\x "Control") (post-key #\p) (post-key #\c)
        (post-key #\a "Control") (post-key #\k "Control")
        (post-text (format nil "~A~%" command))
        (wait-until (lambda () (search "Compilation finished" (buffer-text))) 20)
        (settle))
      (check "C-x p c keeps the project's commands"
             (equal '("echo two" "echo one")
                    (heml::project-property (namestring root) :compile-history)))
      (post-key #\x "Control") (post-key #\p) (post-key #\c)
      (post-key #\a "Control") (post-key #\k "Control")
      (post-key #\p "Meta") (post-key #\p "Meta")
      (settle)
      (check "and M-p at its prompt goes back through them"
             (search "echo one" (buffer-text hi::*echo-area-buffer*)))
      (post-key #\g "Control")
      (settle)
      (post-key #\x "Control") (post-key #\1)
      ;; Its shell's inputs come back with its session.
      (post (list :open (namestring (merge-pathnames "src/a.c" root))))
      (settle)
      (post-key #\x "Control") (post-key #\p) (post-key #\s)
      (settle)
      (post-text "echo hist-$((40+2))
")
      (wait-until (lambda () (search "hist-42" (buffer-text))) 20)
      (extended-command "Save Project Session")
      (settle)
      (extended-command "Kill Project Buffers")
      (post-key #\y)
      (settle)
      (post (list :open (namestring (merge-pathnames "src/a.c" root))))
      (settle)
      (post-key #\x "Control") (post-key #\p) (post-key #\s)
      (settle)
      (post-key #\p "Meta")
      (check "a project's shell remembers what was typed to it before"
             (wait-until (lambda () (search "echo hist-$((40+2))" (buffer-text))) 10))
      (post-key #\i "Meta")
      (settle)
      ;; Replacing through the project: both files have a return.
      (post-key #\x "Control") (post-key #\b)
      (post-key #\a "Control") (post-key #\k "Control")
      (post-text "a.c
")
      (settle)
      (post-key #\x "Control") (post-key #\p) (post-key #\r)
      (post-key #\a "Control") (post-key #\k "Control")
      (post-text "return
")
      (post-text "give_back
")
      (post-key #\y)
      (post-key #\y)
      (check "C-x p r replaces a string in every file of the project that has it"
             (wait-until (lambda ()
                           (and (equal "*Project Replace*" (hi::buffer-name (hi::current-buffer)))
                                (search "src/a.c:2: give_back 1;" (buffer-text))
                                (search "src/b.c:2: give_back 2;" (buffer-text))))
                         20))
      (check "and saves them"
             (search "give_back 1;" (uiop:read-file-string (merge-pathnames "src/a.c" root))))
      ;; Recent files: the one before this is offered first.
      (post-key #\x "Control") (post-key #\1)
      (post (list :open (namestring (merge-pathnames "src/b.c" root))))
      (post (list :open (namestring (merge-pathnames "src/a.c" root))))
      (settle)
      (post-key #\x "Control") (post-key #\r "Control")
      (post (list :named "Return" '()))
      (check "C-x C-r offers the files visited lately, the last first"
             (wait-until (lambda () (equal "b.c" (current-file))) 10))
      (extended-command "Kill Project Buffers")
      (post-key #\y)
      ;; An :open is taken at once, ahead of keys still queued: wait for them.
      (settle)
      (post-key #\x "Control") (post-key #\1)
      ;; Back to where the checks after these expect to be.
      (post (list :open (namestring (merge-pathnames "words.txt" *out*))))
      (settle)))

  (note "menus")
  (choose-menu-item "View" "Split Window")
  (settle)
  (check "View > Split Window splits it"
         (= 2 (length (hi::buffer-windows (hi::current-buffer)))))
  (choose-menu-item "View" "Delete Window")
  (settle)
  (check "View > Delete Window deletes one"
         (= 1 (length (hi::buffer-windows (hi::current-buffer)))))

  (note "side by side")
  (choose-menu-item "View" "Split Window Side by Side")
  (settle)
  (let* ((hunks (sort (mapcar #'hi::window-hunk (hi::buffer-windows (hi::current-buffer)))
                      #'< :key #'hi::device-hunk-column))
         (left (first hunks))
         (right (second hunks)))
    (check "View > Split Window Side by Side puts two windows side by side"
           (and (= 2 (length hunks))
                (= (hi::device-hunk-position left) (hi::device-hunk-position right))
                (= (hi::device-hunk-column right)
                   (+ 1 (hi::device-hunk-column left) (hi::device-hunk-width left)))))
    (check "a bar divides them"
           (let ((text (heml.cocoa::row-text
                        (svref (heml.cocoa::screen-rows heml.cocoa::*screen*) 0))))
             (char= #\│ (char text (hi::device-hunk-width left)))))
    (shot "side-by-side")
    (mouse :down 2 0) (mouse :up 2 0)
    (settle)
    (check "the screen is not cleared again after the split has been drawn"
           (not hi::*screen-image-trashed*))
    (check "a click in the left window goes to it"
           (eq (hi::window-hunk hi::*current-window*) left))
    (let ((column (+ 3 (hi::device-hunk-column right))))
      (mouse :down column 0) (mouse :up column 0))
    (settle)
    (check "a click in the right window lands where it is in that window"
           (and (eq (hi::window-hunk hi::*current-window*) right)
                (= (point-column) 3)))
    (post-key #\u "Control") (post-key #\8) (post-key #\x "Control") (post-key #\})
    (settle)
    (check "C-x } widens the window"
           (> (hi::device-hunk-width right) (+ 4 (hi::device-hunk-width left))))
    (choose-menu-item "View" "Balance Windows")
    (settle)
    (check "View > Balance Windows makes them as wide as each other"
           (<= (abs (- (hi::device-hunk-width right) (hi::device-hunk-width left))) 1))
    (check "the bar has a resize cursor"
           (let ((bar (+ (hi::device-hunk-column left) (hi::device-hunk-width left))))
             (find-if (lambda (border)
                        (and (eq (first border) :columns) (= (second border) bar)))
                      (heml.cocoa::screen-shown-borders heml.cocoa::*screen*))))
    (let ((bar (+ (hi::device-hunk-column left) (hi::device-hunk-width left)))
          (before (hi::device-hunk-width left))
          (point (point-column)))
      (mouse :down bar 3) (mouse :drag (+ bar 3) 3) (mouse :drag (+ bar 6) 3) (mouse :up (+ bar 6) 3)
      (settle)
      (check "dragging the bar between them widens the window on its left"
             (= (hi::device-hunk-width left) (+ before 6)))
      (check "and leaves point alone"
             (and (eq (hi::window-hunk hi::*current-window*) right)
                  (= (point-column) point)))))
  (choose-menu-item "View" "Delete Window")
  (settle)
  (check "deleting one of them leaves one window across the screen"
         (let ((windows (hi::buffer-windows (hi::current-buffer))))
           (and (= 1 (length windows))
                (= (hi::device-hunk-width (hi::window-hunk (first windows)))
                   (heml.cocoa::screen-columns heml.cocoa::*screen*)))))
  (check "the view shows the frame the editor finished"
         (let ((screen heml.cocoa::*screen*))
           (equalp (map 'list #'heml.cocoa::row-text (heml.cocoa::screen-shown-rows screen))
                   (map 'list #'heml.cocoa::row-text (heml.cocoa::screen-rows screen)))))
  (choose-menu-item "View" "Split Window")
  (settle)
  (choose-menu-item "View" "Split Window Side by Side")
  (settle)
  (let ((current hi::*current-window*))
    (choose-menu-item "View" "Delete Other Windows")
    (settle)
    (check "View > Delete Other Windows leaves the current window, filling the screen"
           (let ((windows (remove hi::*echo-area-window* hi::*window-list*)))
             (and (equal windows (list current))
                  (= (hi::device-hunk-width (hi::window-hunk current))
                     (heml.cocoa::screen-columns heml.cocoa::*screen*))))))
  (choose-menu-item "View" "Split Window")
  (settle)
  (let* ((bottom (hi::window-hunk hi::*current-window*))
         (top (hi::device-hunk-previous bottom))
         (modeline (hi::device-hunk-position top))
         (before (hi::device-hunk-height top)))
    (mouse :down 5 modeline) (mouse :drag 5 (+ modeline 2)) (mouse :drag 5 (+ modeline 4))
    (mouse :up 5 (+ modeline 4))
    (settle)
    (check "dragging a modeline down makes its window taller"
           (and (= (hi::device-hunk-height top) (+ before 4))
                (= (hi::device-hunk-position top) (+ modeline 4))))
    (check "and selects that window"
           (eq (hi::window-hunk hi::*current-window*) top)))
  (choose-menu-item "View" "Delete Other Windows")
  (settle)
  (press-menu #\b)
  (settle)
  (check "Cmd-B prompts for a buffer" (eq hi::*current-window* hi::*echo-area-window*))
  (shot "switch-buffer")
  (post-key #\g "Control")
  (settle)

  (note "fonts")
  (press-menu #\=) (settle)
  (check "Cmd-= makes the font bigger" (= heml.cocoa:*font-size* 14))
  (shot "bigger")
  (press-menu #\-) (press-menu #\-) (settle)
  (check "Cmd-- makes it smaller" (= heml.cocoa:*font-size* 12))
  (press-menu #\0) (settle)
  (check "Cmd-0 goes back to the default" (= heml.cocoa:*font-size* 13))

  (note "dired")
  ;; A directory of its own: flagging a file must not risk a real one.
  (let ((directory (merge-pathnames "dired/" *out*)))
    (ensure-directories-exist (merge-pathnames "a-directory/" directory))
    (with-open-file (out (merge-pathnames "a-file.txt" directory)
                         :direction :output :if-exists :supersede)
      (write-line "x" out))
    (check "the Dired menu is hidden outside Dired" (menu-hidden-p "Dired"))
    (post (list :command "Dired" (namestring directory)))
    (settle)
    (check "and shown in it" (not (menu-hidden-p "Dired")))
    (choose-menu-item "Dired" "Mark")
    (settle)
    (check "Dired > Mark marks the file under point"
           (not (eq :none (row-runs-containing "* "))))
    (choose-menu-item "Dired" "Unmark All")
    (settle)
    (check "Dired lists a directory, with a header"
           (and (equal "Dired" (hi::buffer-major-mode (hi::current-buffer)))
                (not (eq :none (row-runs-containing "(2 entries, ")))))
    (check "a directory is blue and bold"
           (equal '(:fg 4 :bold t) (run-font-at "a-directory/" 0)))
    ;; C-d flags the file on point's line and C-u clears it, neither moving.
    (post-key #\d "Control")
    (settle)
    (check "a file flagged for deletion is red"
           (eql 1 (run-font-at "D " 0)))
    (post-key #\u "Control")
    (settle)
    ;; A double click on a file visits it.
    (let* ((rows (heml.cocoa::screen-shown-rows heml.cocoa::*screen*))
           (line (position-if (lambda (row) (search "a-file.txt" (heml.cocoa::row-text row))) rows))
           (column (and line (search "a-file.txt" (heml.cocoa::row-text (svref rows line))))))
      (when line
        (mouse :down (+ column 2) line) (mouse :up (+ column 2) line)
        (mouse :down (+ column 2) line :clicks 2) (mouse :up (+ column 2) line :clicks 2)
        (settle))
      (check "a double click in Dired visits the file"
             (let ((pathname (hi::buffer-pathname (hi::current-buffer))))
               (and pathname (equal "a-file.txt" (file-namestring pathname)))))
      (check "and the Dired menu is hidden again there" (menu-hidden-p "Dired")))
    (post-key #\x "Control") (post-key #\k)
    (settle)
    (post (list :named "Return" (quote ())))
    (settle)
    (check "and nothing was deleted"
           (and (probe-file (merge-pathnames "a-file.txt" directory))
                (probe-file (merge-pathnames "a-directory/" directory)))))

  (note "menus from lisp")
  (heml-interface:define-menu "Smoke" ()
    ("Split Side by Side" "Split Window Horizontally"))
  (settle)
  (check "define-menu puts a menu in the menu bar" (not (menu-hidden-p "Smoke")))
  (choose-menu-item "Smoke" "Split Side by Side")
  (settle)
  (check "and its item runs its command"
         (= 2 (length (remove hi::*echo-area-window* hi::*window-list*))))
  (heml-interface:add-menu-item "Smoke" '("One Window" "Delete Other Windows"))
  (settle)
  (choose-menu-item "Smoke" "One Window")
  (settle)
  (check "add-menu-item adds an item that works"
         (= 1 (length (remove hi::*echo-area-window* hi::*window-list*))))
  (heml-interface:remove-menu "Smoke")
  (settle)
  (check "remove-menu takes it away"
         (main (cffi:null-pointer-p
                (objc:invoke (objc:invoke (objc.runloop:shared-application) "mainMenu")
                             "itemWithTitle:" "Smoke"))))

  (note "bufed")
  ;; The list is most recent first: a buffer just visited, then left, is
  ;; near its top, on the screen however many buffers there are.
  (post (list :open (namestring (merge-pathnames "opened.c" *out*))))
  (post (list :open (namestring (merge-pathnames "words.txt" *out*))))
  (settle)
  (post (list :command "Bufed"))
  (settle)
  (let* ((rows (heml.cocoa::screen-shown-rows heml.cocoa::*screen*))
         (line (position-if (lambda (row) (search "opened.c" (heml.cocoa::row-text row))) rows))
         (column (and line (search "opened.c" (heml.cocoa::row-text (svref rows line))))))
    (check "Bufed lists the buffers, under a header"
           (and line (not (eq :none (row-runs-containing "Buffers  (")))))
    (when line
      (mouse :down (+ column 2) line) (mouse :up (+ column 2) line)
      (mouse :down (+ column 2) line :clicks 2) (mouse :up (+ column 2) line :clicks 2)
      (settle))
    (check "a double click in Bufed visits the buffer"
           (let ((pathname (hi::buffer-pathname (hi::current-buffer))))
             (and pathname (equal "opened.c" (file-namestring pathname))))))

  (note "text styles")
  (let ((file (merge-pathnames "styles.txt" *out*)))
    (with-open-file (out file :direction :output :if-exists :supersede)
      (write-line "plain italic under both" out))
    (post (list :open (namestring file)))
    (settle)
    ;; The editor is idle, waiting for input, while the marks are made.
    (let ((line (hi::mark-line (hi::buffer-start-mark (hi::current-buffer)))))
      (hi::font-mark line 6 '(:italic t))
      (hi::font-mark line 12 0)
      (hi::font-mark line 13 '(:underline t))
      (hi::font-mark line 18 0)
      (hi::font-mark line 19 '(:bold t :italic t :underline t)))
    (post-key #\l "Control")
    (settle)
    (check "styles reach the screen"
           (and (equal '(:italic t) (run-font-at "plain italic" 6))
                (equal '(:underline t) (run-font-at "plain italic" 13))))
    (check "italic text is drawn italic, or slanted where the font has none"
           (main (let* ((attributes (heml.cocoa::text-attributes (display) "textColor" nil t))
                        (font (objc:invoke attributes "objectForKey:" "NSFont")))
                   (or (logtest 1 (objc:invoke (objc:invoke font "fontDescriptor") "symbolicTraits"))
                       (not (cffi:null-pointer-p
                             (objc:invoke attributes "objectForKey:" "NSObliqueness")))))))
    (check "an underline is drawn under underlined text, and not under plain"
           (let* ((row (position-if (lambda (row) (search "plain italic under" (heml.cocoa::row-text row)))
                                    (heml.cocoa::screen-shown-rows heml.cocoa::*screen*)))
                  (display (display)))
             (and row
                  (main
                    (let* ((view (view))
                           (bounds (objc:invoke view "bounds"))
                           (rep (objc:invoke view "bitmapImageRepForCachingDisplayInRect:" bounds))
                           (scale (/ (objc:invoke rep "pixelsWide") (aref bounds 2)))
                           (y (floor (* scale (+ (heml.cocoa::cell-y display row)
                                                 (heml.cocoa::display-char-ascent display) 1)))))
                      (objc:invoke view "cacheDisplayInRect:toBitmapImageRep:" bounds rep)
                      (flet ((ink (column)
                               (let* ((x (floor (* scale (+ (heml.cocoa::cell-x display column)
                                                            (/ (heml.cocoa::display-char-width display) 2)))))
                                      (color (objc:invoke (objc:invoke rep "colorAtX:y:" x y)
                                                          "colorUsingColorSpace:"
                                                          (objc:invoke "NSColorSpace" "sRGBColorSpace"))))
                                 (+ (objc:invoke color "redComponent")
                                    (objc:invoke color "greenComponent")
                                    (objc:invoke color "blueComponent")))))
                        ;; A space under "under" (column 15, the "d") and in
                        ;; "plain" (column 2, the "a") at the underline's row.
                        (> (abs (- (ink 15) (ink 2))) 0.5)))))))
    (shot "styles"))

  (note "shell")
  (extended-command "Shell")
  (settle)
  ;; Tab completes a file name, from the shell's directory.
  (post-text "ls gre")
  (post (list :named "Tab" '()))
  (settle)
  (post-text "e")
  (post (list :named "Return" '()))
  (check "in a shell, Tab completes a file's name from a popup"
         (wait-until (lambda () (search "ls greet" (buffer-text))) 10))
  (post (list :named "Return" '()))
  (settle)
  (post-text "echo smoke-$((6*7)); printf '\\033[31mred\\033[0m plain\\n'
")
  (check "a shell runs in a buffer"
         (wait-until (lambda () (search "smoke-42" (buffer-text)))))
  (check "colour codes from a shell are not text"
         (wait-until (lambda () (search (format nil "~%red plain") (buffer-text)))))
  (check "a colour code becomes a font"
         (let ((line (hi::mark-line (hi::buffer-start-mark (hi::current-buffer)))))
           (loop while line
                 thereis (and (search "red plain" (hi::line-string line))
                              (some (lambda (m) (and (hi::fast-font-mark-p m)
                                                     (eql (hi::font-mark-font m) 1)))
                                    (hi::line-marks line)))
                 do (setf line (hi::line-next line)))))
  (post-text "echo first-in
")
  (wait-until (lambda () (<= 2 (count-matches "first-in" (buffer-text)))))
  (post-key #\c "Control") (post-key #\p "Control")
  (settle)
  (check "C-c C-p goes back to the last input"
         (search "echo first-in" (hi::line-string (hi::mark-line (hi::current-point)))))
  (post (list :named "Return" '()))
  (check "Return on it sends it again"
         (wait-until (lambda () (<= 4 (count-matches "first-in" (buffer-text))))))
  (shot "shell")

  (note "slave")
  (extended-command "Start Slave Thread")
  (settle)
  (wait-until (lambda () (search "CL-USER>" (buffer-text))))
  (post-text "(* 6 7)
")
  (check "a slave Lisp evaluates"
         (wait-until (lambda () (search (format nil "~%42") (buffer-text)))))
  (shot "slave")

  (note "lists")
  (extended-command "List Slaves")
  (settle)
  (check "List Slaves shows the slave, idle"
         (and (equal "Slave-List" (hi::buffer-major-mode (hi::current-buffer)))
              (search "idle" (buffer-text))))
  (post-key #\c "Control") (post-key #\?)
  (post-key #\a "Control") (post-key #\k "Control")
  (post-text "mapcar
")
  (check "Slave Apropos lists the symbols"
         (wait-until (lambda () (and (equal "Apropos" (hi::buffer-major-mode (hi::current-buffer)))
                                     (search "MAPCAR" (buffer-text))))))
  (settle)
  (check "with what each names, coloured"
         (and (search "Function:" (buffer-text))
              (eql 6 (run-font-at "  Function:" 2))))
  (post (list :command "Smoke Debug Buffer"))
  (settle)
  (check "the debugger lists frames, coloured"
         (and (equal "Debug" (hi::buffer-major-mode (hi::current-buffer)))
              (eql 2 (run-font-at "  0: (FOO 1)" 2))))
  (post (list :named "Return" '()))
  (settle)
  (check "Return shows a frame's locals"
         (search "X = 1" (buffer-text)))
  (post (list :command "Smoke Completions"))
  (settle)
  (check "a completion list shows what the completions share and the next character"
         (and (eql 7 (run-font-at "mapcar" 0))
              (equal (quote (:fg 4 :bold t)) (run-font-at "mapcar" 4))))

  (note "file names and the environment")
  (let ((home (string-right-trim "/" (namestring (user-homedir-pathname)))))
    (check "~ and ~user start a file name with a home directory"
           (and (equal (heml-ext:expand-file-name "~/notes") (concatenate 'string home "/notes"))
                (equal (heml-ext:expand-file-name (format nil "~~~A/.profile" (uiop:getenv "USER")))
                       (concatenate 'string home "/.profile"))
                (equal (heml-ext:expand-file-name "~") home)))
    (check "a ~ after a /, or a second /, starts the name again"
           (and (equal (heml-ext:expand-file-name "/some/dir/~/notes")
                       (concatenate 'string home "/notes"))
                (equal (heml-ext:expand-file-name "/some/dir//etc/hosts") "/etc/hosts")))
    (check "and other names are left as they are"
           (and (equal (heml-ext:expand-file-name "/tmp/backup~") "/tmp/backup~")
                (equal (heml-ext:expand-file-name "~no-such-user-here/x") "~no-such-user-here/x")
                (equal (heml-ext:expand-file-name "relative/file.txt") "relative/file.txt")))
    ;; Typed at the file prompt, after the directory it offers.
    (let ((out (namestring *out*)))
      (when (eql 0 (search home out))
        (post-key #\x "Control") (post-key #\f "Control")
        (post-text (format nil "~~~Agrep.txt~%" (subseq out (length home))))
        (check "C-x C-f takes a ~/ name typed after the directory offered"
               (wait-until (lambda ()
                             (equal (hi::buffer-pathname (hi::current-buffer))
                                    (merge-pathnames "grep.txt" *out*)))
                           10)))))
  ;; A stand-in for the user's shell, which says what its environment is.
  (let ((shell (merge-pathnames "fake-shell" *out*))
        (real (uiop:getenv "SHELL")))
    (with-open-file (out shell :direction :output :if-exists :supersede)
      (format out "#!/bin/sh~%echo 'noise from a startup file'~%echo 'PATH=/fake/bin:/usr/bin'~%echo 'HEML_SMOKE_VARIABLE=from the shell'~%"))
    (sb-posix:chmod (namestring shell) #o755)
    (setf (uiop:getenv "SHELL") (namestring shell))
    (unwind-protect
         (progn
           (check "the login shell's environment is read"
                  (equal (hi::shell-environment '("PATH" "HEML_SMOKE_VARIABLE"))
                         '(("PATH" . "/fake/bin:/usr/bin")
                           ("HEML_SMOKE_VARIABLE" . "from the shell"))))
           (hi::import-shell-environment '("HEML_SMOKE_VARIABLE"))
           (check "and its variables are set in the editor's process"
                  (equal "from the shell" (uiop:getenv "HEML_SMOKE_VARIABLE")))
           (check "a shell is the user's own, not /bin/bash"
                  (eql 0 (search (namestring shell) (heml::get-command-line)))))
      (setf (uiop:getenv "SHELL") (or real "/bin/sh"))))

  (note "settings")
  ;; A home of its own, so that the real init file is not touched.
  (let ((home (merge-pathnames "home/" *out*)))
    (uiop:delete-directory-tree home :validate t :if-does-not-exist :ignore)
    (ensure-directories-exist home)
    (sb-posix:setenv "HOME" (namestring home) 1)
    (sb-posix:unsetenv "XDG_CONFIG_HOME")
    (choose-menu-item "Heml" "Settings…")
    (settle)
    (check "Settings… opens the init file, ~/.config/heml/init.lisp"
           (wait-until (lambda ()
                         (equal (hi::buffer-pathname (hi::current-buffer))
                                (merge-pathnames ".config/heml/init.lisp" home)))
                       10))
    (check "in a directory made for it"
           (probe-file (merge-pathnames ".config/heml/" home))))

  (note "quitting")
  (post :quit)
  ;; Answer whatever Save All Files and Exit asks.
  (loop repeat 10
        while heml.cocoa::*editor-running-p*
        do (sleep 1)
           (when heml.cocoa::*editor-running-p* (post-key #\n)))
  (check "the editor exits" (not heml.cocoa::*editor-running-p*))
  (setf *finished* t))

(defun count-matches (text string)
  (loop with start = 0
        for at = (search text string :start2 start)
        while at count t do (setf start (1+ at))))

;;; What the lists would show without a slave in the debugger or completing.
(heml::defcommand "Smoke Debug Buffer" (p) "" ""
  (declare (ignore p))
  (heml::make-debug-buffer nil '(("(FOO 1)" nil (("X" . "1")) nil) ("(BAR)" nil nil nil))
                           "SBCL" "smoke"))

(defvar *project-files* :unknown)

(heml::defcommand "Smoke Project Files" (p) "" ""
  (declare (ignore p))
  (setf *project-files*
        (heml::project-files (heml::current-project-root))))

(heml::defcommand "Smoke Completions" (p) "" ""
  (declare (ignore p))
  (heml::make-completelist-buffer
   (mapcar #'heml::make-completelist-entry '("mapcar" "mapcan" "mapc"))))

(defvar *driver*)

(setf *driver*
 (bt:make-thread
 (lambda ()
   (handler-case (run)
     (error (condition)
       (push (format nil "the run itself: ~A" condition) *failures*)
       (note "  FAIL  the run itself: ~A" condition)
       (post :quit))))
 :name "smoke"))

(bt:make-thread
 (lambda ()
   (sleep 600)
   (note "smoke: no result after ten minutes")
   (sb-ext:exit :code 2 :abort t))
 :name "smoke watchdog")

(heml:heml nil :backend-type :cocoa :load-user-init nil)
;; The driver makes its last check after the editor has exited.
(bt:join-thread *driver*)

(note "~&~D check~:P, ~D failed~@[: ~{~A~^; ~}~]~%pictures in ~A"
      *checks* (length *failures*) (reverse *failures*) (namestring *out*))
(sb-ext:exit :code (if (and *finished* (null *failures*)) 0 1) :abort t)
