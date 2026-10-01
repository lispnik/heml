;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;;; A client for language servers: errors as they are typed, definitions,
;;;; references, completions, renaming and formatting, for the modes whose
;;;; server is installed.  See src/lsp.lisp.
;;;;
;;;; A system of its own because it reads and writes JSON with jzon, which
;;;; as published does not compile under ECL (its NATIVE-NAMESTRING names
;;;; UIOP's functions without their package on Lisps other than SBCL, CCL
;;;; and CMUCL).  The Cocoa editor loads it, and the terminal editor on SBCL.

(asdf:defsystem :heml.lsp
  :depends-on (:heml.base :heml.tree-sitter :babel :com.inuoe.jzon)
  :pathname "src/"
  :components ((:file "lsp")))
