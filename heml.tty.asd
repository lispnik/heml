;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defparameter *heml-base-directory*
  (make-pathname :name nil :type nil :version nil
                 :defaults (parse-namestring *load-truename*)))

(asdf:defsystem :heml.tty
     :pathname #.(make-pathname
                        :directory
                        (pathname-directory *heml-base-directory*)
                        :defaults *heml-base-directory*)
     :depends-on (:heml.base :heml.tree-sitter :heml.lsp)
    :components
    ((:module tty-1
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src"))
                           *heml-base-directory*)
              :components
              ((:file "terminfo")
               (:file "termcap" :depends-on ("terminfo"))
               (:file "tty-disp-rt")
               (:file "tty-display" :depends-on ("terminfo" "tty-disp-rt"))
               (:file "tty-screen" :depends-on ("terminfo" "tty-disp-rt"))
               (:file "tty-stuff")
               (:file "tty-input" :depends-on ("terminfo"))
               (:file "linedit" :depends-on ("tty-display"))))))
