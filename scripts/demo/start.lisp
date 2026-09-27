;;;; scripts/demo/start.lisp -- starts the TTY editor for `make demo'.
;;;;
;;;; Run from the top of the tree.  The files the demo edits are made in
;;;; build/demo/files/.

(asdf:load-system :hemlock.tty)

(defparameter *files* (merge-pathnames "build/demo/files/" (uiop:getcwd)))
(ensure-directories-exist *files*)

;; A plain shell in the Shell buffer, without anyone's startup files.
(setf (hemlock::variable-value 'hemlock::shell-utility-switches :global)
      "--norc --noprofile")

;; A real source file to page through.
(uiop:copy-file "src/display.lisp" (merge-pathnames "display.lisp" *files*))

;; An empty file to type into: visiting at startup needs it to exist.
(with-open-file (s (merge-pathnames "fib.lisp" *files*) :direction :output
                   :if-exists :supersede :if-does-not-exist :create))

(hemlock:hemlock (merge-pathnames "fib.lisp" *files*)
                 :backend-type :tty :load-user-init nil)
(uiop:quit)
