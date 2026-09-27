;;;; scripts/demo/repl.lisp -- starts HEMLOCK:REPL for `make demo'.

(handler-bind ((warning #'muffle-warning))
  (asdf:load-system :hemlock.tty))
(format t "~C[2J~C[H" (code-char 27) (code-char 27))
(finish-output)
(uiop:symbol-call :hemlock :repl)
(uiop:quit)
