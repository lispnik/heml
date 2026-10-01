;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;;; The native macOS backend: one NSWindow with one NSView, drawn with
;;;; AppKit text drawing through the objc bridge.  SBCL on macOS only.

(defparameter *heml-base-directory*
  (make-pathname :name nil :type nil :version nil
                 :defaults (parse-namestring *load-truename*)))

(asdf:defsystem :heml.cocoa
     :pathname #.(make-pathname
                        :directory
                        (pathname-directory *heml-base-directory*)
                        :defaults *heml-base-directory*)
     :depends-on (:heml.base :heml.tree-sitter :heml.lsp :objc :bordeaux-threads)
    :components
    ((:module cocoa-1
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src"))
                           *heml-base-directory*)
              :serial t
              :components
              ((:file "cocoa-package")
               (:file "cocoa-appkit")
               (:file "cocoa-device")
               (:file "cocoa-main")))))
