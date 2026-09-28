;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;;; Heml.app, built by asdf-macos-app.  `make app' is the way in: it
;;;; tracks freshness and passes the signing identity through.
;;;;
;;;; In a file of its own so that loading heml.cocoa never needs
;;;; asdf-macos-app: :DEFSYSTEM-DEPENDS-ON is resolved when an .asd is read,
;;;; not when its system is built.

(defsystem "heml-app"
  :defsystem-depends-on ("asdf-macos-app")
  :class :macos-app-system
  :build-operation "macos-app-op"
  :entry-point "heml.cocoa:main"
  :description "Heml, an Emacs-style editor in Common Lisp, as a macOS application."
  :version "0.1.0"
  ;; The terminal editor too, for `heml --tty' from a shell.
  :depends-on ("heml.cocoa" "heml.tty")

  :bundle-identifier "org.lispnik.heml"
  :bundle-name "Heml"
  :bundle-executable "heml"
  :bundle-principal-class "NSApplication"
  :bundle-category "public.app-category.developer-tools"
  :bundle-icon "resources/heml.png"
  :bundle-output-directory "build/"
  ;; The command a terminal runs, which starts the runtime with the options
  ;; a shell needs; `make install-cli' links it onto the PATH.
  :bundle-resources (("scripts/heml" . "bin/heml"))
  :bundle-document-types ((:dict ("CFBundleTypeName" . "Text")
                                 ("LSItemContentTypes" . (:array "public.text" "public.source-code"))
                                 ("CFBundleTypeRole" . "Editor")
                                 ("LSHandlerRank" . "Alternate")))
  ;; From the environment, defaulting to ad hoc, which runs on the machine
  ;; that built it and cannot be notarised.  A Developer ID is the deliberate
  ;; act that makes the bundle distributable:
  ;;
  ;;   make app SIGN_IDENTITY="Developer ID Application: You (TEAMID)"
  ;;
  ;; An empty value counts as absent: make exports HEML_SIGN_IDENTITY
  ;; empty when SIGN_IDENTITY is unset, and "" is not NIL.  #. because ASDF
  ;; does not evaluate a defsystem initarg.
  :code-signing-identity #.(let ((identity (uiop:getenv "HEML_SIGN_IDENTITY")))
                             (if (and identity (plusp (length identity)))
                                 identity
                                 "-")))
