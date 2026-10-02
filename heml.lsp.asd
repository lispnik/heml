;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;;; A client for language servers: errors as they are typed, definitions,
;;;; references, completions, renaming and formatting, for the modes whose
;;;; server is installed.  See src/lsp.lisp.
;;;;
;;;; It reads and writes JSON with jzon, from vendor/jzon, a submodule of
;;;; Zulu-Inuoe/jzon: the jzon ocicl has (20251113-f05afbb) does not compile
;;;; under ECL -- its NATIVE-NAMESTRING uses UIOP's OS-COND, OS-UNIX-P and
;;;; UNIX-NAMESTRING without their package on Lisps other than SBCL, CCL and
;;;; CMUCL -- and jzon's fix (703cb7e) is later than that.  When ocicl has a
;;;; jzon with it, the submodule can go.  ASDF searches the central registry
;;;; before ocicl; jzon's own dependencies still come from ocicl.

(pushnew (merge-pathnames "vendor/jzon/"
                          (make-pathname :name nil :type nil :version nil
                                         :defaults (or *load-truename* *default-pathname-defaults*)))
         asdf:*central-registry* :test #'equal)

(asdf:defsystem :heml.lsp
  :depends-on (:heml.base :heml.tree-sitter :babel :com.inuoe.jzon)
  :pathname "src/"
  :components ((:file "lsp")
               (:file "lsp-features" :depends-on ("lsp"))))
