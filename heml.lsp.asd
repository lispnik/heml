;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;;; A client for language servers: errors as they are typed, definitions,
;;;; references, completions, renaming and formatting, for the modes whose
;;;; server is installed.  See src/lsp.lisp.
;;;;
;;;; A system of its own because it reads and writes JSON with jzon, and the
;;;; jzon ocicl has (20251113-f05afbb) does not compile under ECL: its
;;;; NATIVE-NAMESTRING uses UIOP's OS-COND, OS-UNIX-P and UNIX-NAMESTRING
;;;; without their package on Lisps other than SBCL, CCL and CMUCL.  jzon
;;;; fixed that the same day (703cb7e, "fix: compilation under ECL"), and
;;;; with that jzon this system compiles and loads under ECL; until ocicl
;;;; has it, the Cocoa editor loads this, and the terminal editor on SBCL.

(asdf:defsystem :heml.lsp
  :depends-on (:heml.base :heml.tree-sitter :babel :com.inuoe.jzon)
  :pathname "src/"
  :components ((:file "lsp")))
