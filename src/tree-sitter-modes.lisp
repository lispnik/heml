;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;;; Modes highlighted with tree-sitter.  Each is a major mode, chosen by a
;;;; file's type, or a script's #! line, whose buffers are coloured by the
;;;; grammar it names: C, Markdown, Python, shell scripts, Pascal and Lisp.

(in-package :heml)

(defmacro define-comment-syntax (mode start &optional end)
  "Comments in MODE begin with START and end with END, or at the end of the
line: what \"Indent for Comment\" and its fellows insert and look for."
  `(progn
     (defhvar "Comment Start" "String that indicates the start of a comment."
       :mode ,mode :value ,start)
     (defhvar "Comment End" "String that ends comments.  Nil indicates #\\newline termination."
       :mode ,mode :value ,(and end (concatenate 'string " " end)))
     (defhvar "Comment Begin" "String that is inserted to begin a comment."
       :mode ,mode :value ,(concatenate 'string start " "))))

(defmode "C" :major-p t)

(define-file-type-hook ("c" "h") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "C"))

(define-comment-syntax "C" "/*" "*/")

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

(define-comment-syntax "Python" "#")

(define-interpreter-mode '("python") "Python")

(heml.tree-sitter:define-tree-sitter-language "python" :mode "Python")

(defmode "Shell Script" :major-p t)

(define-file-type-hook ("sh" "bash" "zsh" "ksh") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Shell Script"))

(define-comment-syntax "Shell Script" "#")

(define-interpreter-mode '("sh" "bash" "zsh" "ksh" "dash") "Shell Script")

(heml.tree-sitter:define-tree-sitter-language "bash" :mode "Shell Script")

;;; Pascal mode is Hemlock's own (pascal.lisp); tree-sitter colours it.
;;;
(heml.tree-sitter:define-tree-sitter-language "pascal" :mode "Pascal")

;;; Lisp is coloured by tree-sitter's Common Lisp grammar where it is
;;; installed, and otherwise by Heml's own parser (exp-syntax.lisp), which
;;; misses #| |# comments and colours less.  The query is Neovim's, and
;;; follows Neovim's rule that the later of two patterns wins.
;;;
(heml.tree-sitter:define-tree-sitter-language "commonlisp"
                                              :mode "Lisp"
                                              :precedence :last
                                              :fallback 'hi::line-tag)
