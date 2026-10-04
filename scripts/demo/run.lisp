;;;; scripts/demo/run.lisp -- `make demo-run': running programs and their
;;;; tests -- Rust, Go and TypeScript -- into the compilation buffer, and
;;;; going to what failed.  The projects are test/fixtures/run/'s.

(load (merge-pathnames "driver.lisp" *load-truename*))
(in-package :heml-demo)

(defvar *projects* (fresh-directory "run"))
(sh *top* (format nil "cp -R test/fixtures/run/. '~A'" (namestring *projects*)))

;; Only what is run is on the screen: no language server's hints.
(setf (heml::variable-value 'heml::language-servers :global) nil)

(defun goto-line (line)
  (keys '(#\< "Meta"))
  (dotimes (i (1- line)) (post-key #\n "Control") (sleep 0.03))
  (settle))

(defun steps ()
  (caption "C-c C-c runs the program -- here cargo run -- into the compilation buffer")
  (open-file (merge-pathnames "rust/src/main.rs" *projects*))
  (keys '(#\c "Control") '(#\c "Control"))
  (wait-for "twice 21 is 42" 180)
  (pause 2.5)

  (caption "C-c t t runs the test point is in, and only it")
  (goto-line 15)
  (keys '(#\c "Control") #\t #\t)
  (wait-for "1 passed; 0 failed" 120)
  (pause 2.5)

  (caption "C-c t f runs them all; C-x ` goes to the one that failed, at its line")
  (keys '(#\c "Control") #\t #\f)
  (wait-for "FAILED" 120)
  (pause 2)
  (keys '(#\x "Control") #\`)
  (pause 3)

  (caption "Go: go test -run for the test at point, and its failure is a place too")
  (open-file (merge-pathnames "go/main_test.go" *projects*))
  (goto-line 12)
  (keys '(#\c "Control") #\t #\t)
  (wait-for "FAIL" 120)
  (pause 2)
  (keys '(#\x "Control") #\`)
  (pause 3)

  (caption "TypeScript runs in Node, which strips its types; its tests with node --test")
  (open-file (merge-pathnames "ts/hello.ts" *projects*))
  (keys '(#\c "Control") '(#\c "Control"))
  (wait-for "twice 21 is 42" 60)
  (pause 2)
  (open-file (merge-pathnames "ts/hello.test.ts" *projects*))
  (keys '(#\c "Control") #\t #\f)
  (wait-for "fail 1" 60)
  (pause 2)
  (keys '(#\x "Control") #\`)
  (pause 3.5))

(run-demo "run" #'steps)
