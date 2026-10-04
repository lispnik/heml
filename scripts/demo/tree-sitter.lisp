;;;; scripts/demo/tree-sitter.lisp -- `make demo-tree-sitter': a video of
;;;; syntax highlighting, by mode and with tree-sitter.  See driver.lisp for
;;;; how it is recorded.

(load (merge-pathnames "driver.lisp" *load-truename*))
(in-package :heml-demo)

(dolist (name '("fib.c" "notes.md" "hello.py" "plain.txt"))
  (with-open-file (s (merge-pathnames name *files*) :direction :output
                     :if-exists :supersede :if-does-not-exist :create)))

;;; The same Lisp twice: once in a mode made here for the demo, coloured by
;;; Heml's own parser, and once in Lisp mode, which tree-sitter's Common Lisp
;;; grammar colours.
(defparameter *lisp* ";;;; Both highlighters, on the same Lisp.

#| A block comment:
   (defun commented-out () 'not-code) |#

(defun greet (name &key (greeting \"Hello\") loud)
  \"Greet NAME.\"
  (let ((text (format nil \"~A, ~A!\" greeting name)))
    (if loud (string-upcase text) text)))   ; shout?

(defmacro with-greeting ((var name) &body body)
  `(let ((,var (greet ,name)))
     ,@body))

(defvar *greetings* 0)
(defconstant +limit+ 10)
#+sbcl (sb-ext:gc :full t)
(list #\\( :key 3.14 #x1F nil t)
")
(dolist (name '("both.explisp" "both.lisp"))
  (with-open-file (s (merge-pathnames name *files*) :direction :output
                     :if-exists :supersede :if-does-not-exist :create)
    (write-string *lisp* s)))

(in-package :heml)
(defmode "Lisp/exp-syntax" :major-p t)
(define-file-type-hook ("explisp") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Lisp/exp-syntax"))
(define-mode-highlighter "Lisp/exp-syntax" 'hi::line-tag)
(in-package :heml-demo)

(defun open-file-named (name)
  (open-from-finder (merge-pathnames name *files*))
  (pause 0.8))

(defun steps ()

  ;; C, coloured as it is typed.
  (caption "Highlighting belongs to a mode: tree-sitter colours C as it is typed")
  (open-file-named "fib.c")
  (type-lines '("/* Fibonacci, in C, coloured by tree-sitter. */"
                "#include <stdio.h>"
                ""
                "static int fib(int n) {"
                "    return n < 2 ? n : fib(n - 1) + fib(n - 2);"
                "}"
                ""
                "int main(void) {"
                "    printf(\"fib(20) = %d\\n\", fib(20));"
                "    return 0;"
                "}"))
  (pause 2)

  ;; Markdown.
  (caption "Markdown: two grammars, the block one and the inline one")
  (open-file-named "notes.md")
  (type-lines '("# Heml"
                ""
                "An Emacs-style editor in Common Lisp."
                ""
                "## Highlighting"
                ""
                "- by major mode"
                "- with tree-sitter"
                ""
                "```"
                "make tree-sitter"
                "```"))
  (pause 2)

  ;; Python.
  (caption "Python, from Homebrew's grammar; indented by Neovim's queries")
  (open-file-named "hello.py")
  (type-lines '("# Python, from Homebrew's grammar."
                "def greet(name, loud=False):"
                "    text = f\"Hello, {name}!\""
                "    return text.upper() if loud else text"
                ""
                "print(greet(\"Heml\", loud=True))"))
  (pause 2)

  ;; Text is not Lisp, and is no longer coloured as if it were.
  (caption "Plain text is not code, and is not coloured as if it were")
  (open-file-named "plain.txt")
  (type-lines '("Plain text: it's not code; \"quotes\" and (parens) stay plain."))
  (pause 2)

  ;; The same Lisp, Heml's parser on the left, Lisp mode's tree-sitter on
  ;; the right.
  (caption "The same Lisp: Heml's own parser on the left, tree-sitter's grammar on the right")
  (open-file-named "both.explisp")
  (post-key #\x "Control") (post-key #\3)
  (pause 0.8)
  (open-file-named "both.lisp")
  (post-key #\< "Meta")
  (pause 5))

(run-demo "tree-sitter" #'steps)
