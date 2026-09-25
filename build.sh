#!/bin/sh
unset backends

SBCL=${SBCL:-clbuild lisp}

if test $# -eq 0; then
	cat <<eof
Building backend tty.
(Specify backend types at the command line to override this default.)
eof
else
	while test $# -gt 0; do
		case $1 in
			tty|qt)
				backends="$backends :hemlock.$1"
				echo backend $1 enabled
				shift
				;;
			*)
				echo invalid backend type $1
				exit 1
				;;
		esac
	done
fi


$SBCL <<EOF
;; The last backend loaded is the default when $DISPLAY is set.
;;
(dolist (system (or '($backends) '(:hemlock.tty)))
  (asdf:operate 'asdf:load-op system))

(defun hemlock-toplevel ()
  #+ccl (when (find-package :qt) (funcall (find-symbol "REBIRTH" :qt)))
  (let ((argv0 (car (command-line-arguments:get-command-line-arguments)))) 
    (setf hi::*installation-directory*
	  (concatenate 
	   'string
	   (iolib.pathnames:file-path-directory argv0 :namestring t)
	   "/"))
    (setf hemlock::*slave-command* (list argv0 "--slave"))
    (hemlock:main))
  (quit))

(sb-ext:save-lisp-and-die "hemlock"
                          :save-runtime-options t
			  :toplevel 'hemlock-toplevel
			  :executable t)
EOF
