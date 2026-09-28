;;;; scripts/demo/start.lisp -- starts the TTY editor for `make demo'.
;;;;
;;;; Run from the top of the tree.  The files the demo edits are made in
;;;; build/demo/files/.

(handler-bind ((warning #'muffle-warning))
  (asdf:load-system :heml.tty))

(defparameter *files* (merge-pathnames "build/demo/files/" (uiop:getcwd)))
(ensure-directories-exist *files*)

;; A plain shell in the Shell buffer, without anyone's startup files.
(setf (heml::variable-value 'heml::shell-utility-switches :global)
      "--norc --noprofile")

;; A real source file to page through.
(uiop:copy-file "src/display.lisp" (merge-pathnames "display.lisp" *files*))

;; An empty file to type into: visiting at startup needs it to exist.
(with-open-file (s (merge-pathnames "fib.lisp" *files*) :direction :output
                   :if-exists :supersede :if-does-not-exist :create))

;; A clear screen, which is what comes back when the editor leaves.
(format t "~C[2J~C[H" (code-char 27) (code-char 27))
(finish-output)

(heml:heml (merge-pathnames "fib.lisp" *files*)
                 :backend-type :tty :load-user-init nil)
(uiop:quit)
