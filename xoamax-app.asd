;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;;; Xoamax.app, built by asdf-macos-app:
;;;;
;;;;     (asdf:make "xoamax-app")        ; => build/Xoamax.app
;;;;
;;;; Signed with the Developer ID named by MACOS_SIGNING_IDENTITY, or ad hoc
;;;; when it is unset, which runs on the machine that built it and nowhere
;;;; else.

(defsystem "xoamax-app"
  :defsystem-depends-on ("asdf-macos-app")
  :class :macos-app-system
  :build-operation "macos-app-op"
  :entry-point "hemlock.cocoa:main"
  :description "Xoamax, an Emacs-style editor in Common Lisp, as a macOS application."
  :version "0.1.0"
  :depends-on ("hemlock.cocoa")

  :bundle-identifier "org.lispnik.xoamax"
  :bundle-name "Xoamax"
  :bundle-executable "xoamax"
  :bundle-principal-class "NSApplication"
  :bundle-category "public.app-category.developer-tools"
  :bundle-output-directory #.(merge-pathnames "build/" (uiop:pathname-directory-pathname
                                                        (or *load-truename* *default-pathname-defaults*)))
  :bundle-document-types ((:dict ("CFBundleTypeName" . "Text")
                                 ("LSItemContentTypes" . (:array "public.text" "public.source-code"))
                                 ("CFBundleTypeRole" . "Editor")
                                 ("LSHandlerRank" . "Alternate")))
  :code-signing-identity #.(or (uiop:getenv "MACOS_SIGNING_IDENTITY") "-"))
