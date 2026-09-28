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
			tty)
				backends="$backends :heml.$1"
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
(dolist (system (or '($backends) '(:heml.tty)))
  (asdf:operate 'asdf:load-op system))

(defun heml-toplevel ()
  ;; The program's own path: GET-COMMAND-LINE-ARGUMENTS leaves it out.
  (let ((argv0 (car sb-ext:*posix-argv*)))
    (setf hi::*installation-directory*
	  (concatenate 
	   'string
	   (iolib.pathnames:file-path-directory argv0 :namestring t)
	   "/"))
    (setf heml::*slave-command* (list argv0 "--slave"))
    ;; With the runtime's options saved, SBCL leaves argv alone, and the
    ;; editor's options are everything after the program's name.
    (heml:main (rest sb-ext:*posix-argv*)))
  (quit))

(sb-ext:save-lisp-and-die "heml"
                          :save-runtime-options t
			  :toplevel 'heml-toplevel
			  :executable t)
EOF
