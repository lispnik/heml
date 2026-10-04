;;;; scripts/demo/cocoa.lisp -- `make demo-cocoa': a video of the Cocoa
;;;; editor: typing, evaluating, an input method, the font, the mouse,
;;;; windows and a shell.  See driver.lisp for how it is recorded.

(load (merge-pathnames "driver.lisp" *load-truename*))
(in-package :heml-demo)

(uiop:copy-file (merge-pathnames "src/display.lisp" *top*)
                (merge-pathnames "display.lisp" *files*))
(with-open-file (s (merge-pathnames "fib.lisp" *files*) :direction :output
                   :if-exists :supersede :if-does-not-exist :create))

(defun steps ()

  ;; A file opened from Finder, and Lisp typed into it.
  (caption "A file opened from Finder; Lisp coloured and indented as it is typed")
  (open-from-finder (merge-pathnames "fib.lisp" *files*))
  (pause 0.8)
  (type-text ";;; Heml, native on macOS.")
  (post-named "Return")
  (type-text "(defun fib (n)") (post-key #\j "Control")
  (type-text "\"The Nth Fibonacci number.\"") (post-key #\j "Control")
  (type-text "(if (< n 2)") (post-key #\j "Control")
  (type-text "n") (post-key #\j "Control")
  (type-text "(+ (fib (- n 1)) (fib (- n 2)))))")
  (post-named "Return") (post-named "Return")
  (pause 1)

  ;; Evaluated in the editor's own Lisp.
  (caption "Evaluated in the editor's own Lisp")
  (extended-command "Editor Evaluate Buffer")
  (extended-command "Editor Evaluate Expression")
  (type-text "(fib 20)")
  (post-named "Return")
  (pause 2)

  ;; Japanese through the input method: held while composed, then committed.
  (caption "Input methods: Japanese composed, then committed; a wide character takes two columns")
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
  (caption "Command-= and Command-0: the font bigger, and back")
  (press-menu #\=) (pause 0.6)
  (press-menu #\=) (pause 1)
  (press-menu #\0) (pause 1)

  ;; The mouse: a double click selects a word, a drag a region.
  (caption "The mouse: a double click selects a word, a drag a region")
  (mouse :down 8 1 :clicks 1) (mouse :up 8 1 :clicks 1)
  (mouse :down 8 1 :clicks 2) (mouse :up 8 1 :clicks 2)
  (pause 1.2)
  (mouse :down 2 2) (mouse :drag 10 2) (mouse :drag 20 2) (mouse :drag 28 2) (mouse :up 28 2)
  (pause 1.2)
  (mouse :down 0 8) (mouse :up 0 8)
  (pause 0.5)

  ;; Side by side, with a shell on the right.
  (caption "Windows side by side, with a shell on the right")
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
  (caption "The left window split again, and a real file paged through")
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
  (caption "A column widened, the windows balanced again, then one window")
  (post-key #\u "Control") (type-text "12" :pause 0.1)
  (post-key #\x "Control") (post-key #\})
  (pause 1.5)
  (choose-menu-item "View" "Balance Windows")
  (pause 1.5)
  (post-key #\x "Control") (post-key #\1)
  (pause 2))

(run-demo "cocoa" #'steps)
