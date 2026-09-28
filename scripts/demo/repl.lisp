;;;; scripts/demo/repl.lisp -- starts HEML:REPL for `make demo'.

(handler-bind ((warning #'muffle-warning))
  (asdf:load-system :heml.tty))
(format t "~C[2J~C[H" (code-char 27) (code-char 27))
(finish-output)
(uiop:symbol-call :heml :repl)
(uiop:quit)
