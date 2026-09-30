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

  (note "bufed")
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

  (note "quitting")
  (post :quit)
  ;; Answer whatever Save All Files and Exit asks.
  (loop repeat 10
        while heml.cocoa::*editor-running-p*
        do (sleep 1)
           (when heml.cocoa::*editor-running-p* (post-key #\n)))
  (check "the editor exits" (not heml.cocoa::*editor-running-p*))
  (setf *finished* t))

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
   (sleep 180)
   (note "smoke: no result after three minutes")
   (sb-ext:exit :code 2 :abort t))
 :name "smoke watchdog")

(heml:heml nil :backend-type :cocoa :load-user-init nil)
;; The driver makes its last check after the editor has exited.
(bt:join-thread *driver*)

(note "~&~D check~:P, ~D failed~@[: ~{~A~^; ~}~]~%pictures in ~A"
      *checks* (length *failures*) (reverse *failures*) (namestring *out*))
(sb-ext:exit :code (if (and *finished* (null *failures*)) 0 1) :abort t)
