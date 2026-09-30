;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;;; Syntax highlighting with tree-sitter, and the modes that use it: C,
;;;; Markdown, Python, shell scripts, Pascal's colours and Lisp's.  SBCL and
;;;; ECL; the terminal and Cocoa editors load it.  It needs nothing to load: the tree-sitter library and grammars
;;;; are looked for when a buffer in one of its modes is first drawn, and
;;;; without them the buffer is not coloured.  See src/tree-sitter.lisp.

(asdf:defsystem :heml.tree-sitter
  :depends-on (:heml.base :babel :cl-ppcre :cffi)
  :pathname "src/"
  :components ((:file "tree-sitter")
               (:file "tree-sitter-modes" :depends-on ("tree-sitter"))))
