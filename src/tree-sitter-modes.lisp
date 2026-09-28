;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;;; Modes highlighted with tree-sitter.  Each is a major mode, chosen by a
;;;; file's type, whose buffers are coloured by the grammar it names.

(in-package :heml)

(defmode "C" :major-p t)

(define-file-type-hook ("c" "h") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "C"))

(heml.tree-sitter:define-tree-sitter-language "c" :mode "C")

(defmode "Markdown" :major-p t)

(define-file-type-hook ("md" "markdown") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Markdown"))

(heml.tree-sitter:define-tree-sitter-language "markdown" :mode "Markdown")

(defmode "Python" :major-p t)

(define-file-type-hook ("py") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Python"))

(heml.tree-sitter:define-tree-sitter-language "python" :mode "Python")

;;; Lisp keeps its own highlighter (exp-syntax.lisp), but tree-sitter's Common
;;; Lisp grammar is here to compare, and to switch to: its query is Neovim's,
;;; and follows Neovim's rule that the later of two patterns wins.
;;;
(heml.tree-sitter:define-tree-sitter-language "commonlisp" :precedence :last)
