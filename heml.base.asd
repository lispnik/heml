;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defpackage #:heml-system
  (:use #:cl)
  (:export #:*heml-base-directory*))

(in-package #:heml-system)

(defvar *modern-heml* nil)
(setf *modern-heml* t)

(pushnew :command-bits *features*)

;;; SBCL on macOS has :BSD in *FEATURES*, and osicat and the TTY backend
;;; take it to mean macOS (TIOCGWINSZ, for one, exists only under it).  ECL
;;; on macOS has only :DARWIN, so give it :BSD too.  This must happen before
;;; osicat is compiled, and ASDF does not recompile when features change.
#+(and ecl darwin)
(pushnew :bsd *features*)

;;; ECL 26.5.5's compiler writes calls to ecl_to_cl_index, which its headers
;;; do not declare; ecl_to_index is the function.  A CFFI write at a computed
;;; offset, as in ioconnections and tty-disp-rt, produces one.
#+ecl (require :cmp)
#+ecl
(unless (search "ecl_to_cl_index" c:*user-cc-flags*)
  (setf c:*user-cc-flags*
        (concatenate 'string c:*user-cc-flags* " -Decl_to_cl_index=ecl_to_index")))

(defparameter *heml-base-directory*
  (make-pathname :name nil :type nil :version nil
                 :defaults (parse-namestring *load-truename*)))

;;; vendor/conium is a submodule: lispnik/conium, branch ecl, whose ECL
;;; backend is brought up to date from SLIME's.  The central registry is
;;; searched before ocicl, so this copy is the one loaded.
(pushnew (merge-pathnames "vendor/conium/" *heml-base-directory*)
         asdf:*central-registry* :test #'equal)

(defparameter *binary-pathname*
  (make-pathname :directory
                 (append (pathname-directory *heml-base-directory*)
                         (list "bin"
                               (string-downcase (lisp-implementation-type))))
                 :defaults *heml-base-directory*))

(asdf:defsystem :heml.base
     :pathname #.(make-pathname
                        :directory
                        (pathname-directory *heml-base-directory*)
                        :defaults *heml-base-directory*)
     :depends-on (:alexandria
                  :bordeaux-threads
                  :conium
                  :trivial-gray-streams
                  :iterate
                  :prepl
                  :osicat
                  :iolib
                  :iolib/os
                  :cl-ppcre
                  :command-line-arguments)
    :components
    ((:module core-1
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src"))
                           *heml-base-directory*)
              :depends-on (wire)
              :components
              ((:file "package")
               ;; Lisp implementation specific stuff goes into one of the next
               ;; two files.
               (:file "lispdep" :depends-on ("package"))
               (:file "heml-ext" :depends-on ("package"))

               (:file "decls" :depends-on ("package")) ; early declarations of functions and stuff
               (:file "struct" :depends-on ("package"))
               (:file "charmacs" :depends-on ("package"))
               (:file "key-event" :depends-on ("package" "charmacs"))
               ))
     (:module keysyms
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src"))
                           *heml-base-directory*)
              :depends-on (core-1)
              :components
              ((:file "keysym-defs")))
     (:module core-2
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src"))
                           *heml-base-directory*)
              :depends-on (keysyms core-1)
              :serial t                 ;...
              :components
              ((:file "rompsite")
               (:file "input")
               (:file "macros")
               (:file "line")
               (:file "ring")
               (:file "htext1")
               (:file "buffer" :depends-on (htext1))
               (:file "vars" :depends-on (buffer))
               (:file "interp")
               (:file "syntax")
               (:file "htext2")
               (:file "htext3")
               (:file "htext4")
               (:file "files")
               (:file "search1")
               (:file "search2")
               (:file "table")

               (:file "winimage")
               (:file "window")
               (:file "screen")
               (:file "layout")
               (:file "linimage")
               (:file "cursor")
               (:file "display")
               (:file "exp-syntax")
               (:file "connections")
               (:file "repl" :depends-on ("macros" "rompsite" "connections"))))
     (:module root-1
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src"))
                           *heml-base-directory*)
              :depends-on (core-2 core-1)
              :components
              ((:file "pop-up-stream")))
     (:module root-2
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src"))
                           *heml-base-directory*)
              :depends-on (root-1 core-1 wire)
              :components
              ((:file "font")
               (:file "streams")
               (:file "main")
               (:file "echo")
               (:file "new-undo")))
     (:module core-3
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src"))
                           *heml-base-directory*)
              :depends-on (keysyms core-1 core-2)
              :components
              ((:file "typeout")))
     (:module wire
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src"))
                           *heml-base-directory*)
              :depends-on ()
              :serial t
              :components
              ((:file "wire-package")
               (:file "port")
               (:file "wire")
               (:file "remote")))
     (:module user-1
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src"))
                           *heml-base-directory*)
              :depends-on (root-2 core-1 wire)
              :components
              ((:file "echocoms")

               (:file "command")
               (:file "kbdmac")
               (:file "undo")
               (:file "killcoms")
               (:file "indent" :depends-on ("filecoms"))
               (:file "searchcoms")
               (:file "filecoms")
               (:file "results" :depends-on ("filecoms"))
               (:file "grep" :depends-on ("results"))
               (:file "apropos" :depends-on ("filecoms"))
               (:file "morecoms")
               (:file "doccoms")
               (:file "srccom")
               (:file "group")
               (:file "fill")
               (:file "text")

               (:file "lispmode")
               (:file "ts-buf")
               (:file "ts-stream")
               (:file "request")
               (:file "eval-server")
               (:file "lispbuf" :depends-on ("filecoms"))
               (:file "lispeval" :depends-on ("eval-server"))
               (:file "spell-rt")
               (:file "spell-corr" :depends-on ("spell-rt"))
               (:file "spell-aug" :depends-on ("spell-corr"))
               (:file "spellcoms" :depends-on ("spell-aug" "filecoms"))
               (:file "spell-build" :depends-on ("spell-aug"))

               (:file "comments")
               (:file "overwrite")
               (:file "abbrev")
               (:file "icom")
               (:file "defsyn")
               (:file "pascal")

               (:file "edit-defs")
               (:file "auto-save")
               (:file "register")
               (:file "highlight")
               (:file "dired")
               (:file "diredcoms" :depends-on ("dired"))
               (:file "bufed")
               (:file "coned")
               (:file "xref")
               (:file "completion" :depends-on ("lispmode"))
               (:file "cpc")
               (:file "fuzzy" :depends-on ("cpc"))
               (:file "shell")
               (:file "debug")
               (:file "dabbrev")
               (:file "menus")
               (:file "bindings")
               (:file "slave-list")))
     ;; The iolib event loop and connections, which every backend uses.
     (:module io
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src"))
                           *heml-base-directory*)
              :depends-on (core-2 root-2 user-1)
              :components
              ((:file "ioconnections")))))
