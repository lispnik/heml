;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;;; The native macOS backend: one NSWindow with one NSView, drawn with
;;;; AppKit text drawing through the objc bridge.  SBCL on macOS only.

(defparameter *hemlock-base-directory*
  (make-pathname :name nil :type nil :version nil
                 :defaults (parse-namestring *load-truename*)))

(asdf:defsystem :hemlock.cocoa
     :pathname #.(make-pathname
                        :directory
                        (pathname-directory *hemlock-base-directory*)
                        :defaults *hemlock-base-directory*)
     :depends-on (:hemlock.base :objc :bordeaux-threads)
    :components
    ((:module cocoa-1
              :pathname #.(merge-pathnames
                           (make-pathname
                            :directory '(:relative "src"))
                           *hemlock-base-directory*)
              :serial t
              :components
              (;; The iolib event loop, as tty uses it.
               (:file "ioconnections")
               (:file "cocoa-package")
               (:file "cocoa-appkit")
               (:file "cocoa-device")
               (:file "cocoa-main")))))
