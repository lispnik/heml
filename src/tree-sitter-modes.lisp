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

(heml.tree-sitter:define-tree-sitter-language
 "c" :mode "C" :indent t
 :definitions '("function_definition")
 :opens "[{(\\[]\\s*(//.*|/\\*.*\\*/\\s*)?$"
 :closes "^\\s*([})\\]]|else\\b)")

(defmode "Markdown" :major-p t)

(define-file-type-hook ("md" "markdown") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Markdown"))

(heml.tree-sitter:define-tree-sitter-language "markdown" :mode "Markdown"
                                              :inline "markdown_inline")

;;; A Markdown line keeps the one before's indentation, as Neovim's query
;;; says, and with spaces.
(defhvar "Indent Function" "Indentation function which is invoked by \"Indent\" command."
  :mode "Markdown" :value #'generic-indent)
(defhvar "Indent with Tabs" "Whether indentation uses tabs." :mode "Markdown" :value nil)

(defmode "Python" :major-p t)

(define-file-type-hook ("py") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Python"))

(define-comment-syntax "Python" "#")

(define-interpreter-mode '("python") "Python")

(heml.tree-sitter:define-tree-sitter-language
 "python" :mode "Python" :indent t
 :definitions '("function_definition" "class_definition")
 :opens "(:|[({\\[])\\s*(#.*)?$"
 :closes "^\\s*((else|elif|except|finally)\\b|[)}\\]])"
 :finishes "^\\s*(return|pass|raise|break|continue)\\b")

(defmode "Shell Script" :major-p t)

(define-file-type-hook ("sh" "bash" "zsh" "ksh") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Shell Script"))

(define-comment-syntax "Shell Script" "#")

(define-interpreter-mode '("sh" "bash" "zsh" "ksh" "dash") "Shell Script")

(heml.tree-sitter:define-tree-sitter-language
 "bash" :mode "Shell Script" :indent t
 :definitions '("function_definition")
 :opens "(\\b(then|do|else|in)|[{(]|[^;(]\\))\\s*(#.*)?$"
 :closes "^\\s*((fi|done|esac|elif|else)\\b|[})])"
 :finishes "^\\s*;;")

;;; Pascal mode is Hemlock's own (pascal.lisp); tree-sitter colours and
;;; indents it, with Neovim's query.  Pascal's words are any case.
;;;
(heml.tree-sitter:define-tree-sitter-language
 "pascal" :mode "Pascal" :indent t :indent-width 2
 :definitions '("defProc")
 :opens "(?i)(\\b(begin|then|do|else|of|repeat|record|try|except|finally|var|const|type|class|object|interface|implementation)|[(\\[])\\s*(\\{[^}]*\\}|\\(\\*.*\\*\\)|//.*)?$"
 :closes "(?i)^\\s*((end|until|else|except|finally)\\b|[)\\]])")

;;; Lisp is coloured by tree-sitter's Common Lisp grammar where it is
;;; installed, and otherwise by Heml's own parser (exp-syntax.lisp), which
;;; misses #| |# comments and colours less.  The query is Neovim's, and
;;; follows Neovim's rule that the later of two patterns wins.
;;;
(heml.tree-sitter:define-tree-sitter-language "commonlisp"
                                              :mode "Lisp"
                                              :precedence :last
                                              :fallback 'hi::line-tag)


;;; Return indents the new line in the modes that know how, as Emacs's
;;; electric indentation does, and in C a closing brace goes back out.

(defcommand "New Line and Indent" (p)
  "Indent the line, which may have just become one that goes back out, such
   as a closing brace or an else, then start a new one, indented."
  "Indent this line, start another and indent it."
  (declare (ignore p))
  (indent-command nil)
  (indent-new-line-command nil))

(defcommand "Insert and Indent" (p)
  "Insert the character typed, then indent the line."
  "Insert the character typed, then indent the line."
  (self-insert-command p)
  (indent-command nil))

(dolist (mode '("C" "Python" "Shell Script"))
  (bind-key "New Line and Indent" #k"return" :mode mode))
(bind-key "Insert and Indent" #k"}" :mode "C")

(bind-key "New Line and Indent" #k"return" :mode "Pascal")


;;;; Moving by definition: a language's functions and classes, as its
;;;; grammar's tree has them.

(defun current-definitions ()
  (let ((language (heml.tree-sitter:buffer-language (current-buffer))))
    (unless language
      (editor-error "This buffer's grammar is not installed."))
    (heml.tree-sitter:definition-spans language (current-buffer))))

(defcommand "Beginning of Definition" (p)
  "Move to the start of the function or class point is in, or of the one
   before; with an argument, that many back."
  "Move to the start of this definition, or the one before."
  (let ((point (current-point)))
    (dotimes (i (or p 1))
      (let ((start nil))
        (loop for (line charpos) in (current-definitions)
              for mark = (mark line charpos)
              when (mark< mark point) do (setf start mark))
        (unless start (editor-error "No earlier definition."))
        (move-mark point start)))))

(defcommand "End of Definition" (p)
  "Move past the end of the function or class point is in, or of the next
   one; with an argument, that many on."
  "Move past the end of this definition, or the next one."
  (let ((point (current-point)))
    (dotimes (i (or p 1))
      (let ((end nil))
        (loop for (nil nil line charpos) in (current-definitions)
              for mark = (mark line charpos)
              when (and (mark> mark point) (or (null end) (mark< mark end)))
                do (setf end mark))
        (unless end (editor-error "No later definition."))
        (move-mark point end)
        (when (and (= (mark-charpos point) (line-length (mark-line point)))
                   (line-next (mark-line point)))
          (line-offset point 1 0))))))

(defcommand "Mark Definition" (p)
  "Put point at the start of the function or class it is in, and the mark
   at its end."
  "Mark this definition."
  (declare (ignore p))
  (let ((point (current-point)) (found nil))
    (loop for span in (current-definitions)
          for (start-line start end-line end) = span
          when (and (mark<= (mark start-line start) point)
                    (mark< point (mark end-line end)))
            do (setf found span))
    (unless found (editor-error "Not in a definition."))
    (destructuring-bind (start-line start end-line end) found
      (push-buffer-mark (copy-mark (mark end-line end)) t)
      (move-mark point (mark start-line start)))))

(dolist (mode '("C" "Python" "Shell Script" "Pascal"))
  (bind-key "Beginning of Definition" #k"control-meta-a" :mode mode)
  (bind-key "End of Definition" #k"control-meta-e" :mode mode)
  (bind-key "Mark Definition" #k"control-meta-h" :mode mode))


;;;; An outline: a buffer's headings or definitions, a line each, in a
;;;; result buffer (results.lisp) whose lines visit them.

(defun outline-entries (buffer)
  "(LINE DEPTH TEXT) for each heading or definition in BUFFER."
  (if (string= (buffer-major-mode buffer) "Markdown")
      (loop for (line level) in (markdown-headings buffer)
            collect (list line (1- level) (string-trim " #" (line-string line))))
      (let ((language (heml.tree-sitter:buffer-language buffer)))
        (unless language
          (editor-error "No outline for this buffer."))
        (let ((open '()))
          (loop for (line nil end-line) in (heml.tree-sitter:definition-spans language buffer)
                do (loop while (and open (line> line (first open)))
                         do (pop open))
                collect (list line (length open) (string-trim '(#\Space #\Tab #\{) (line-string line)))
                do (push end-line open))))))

(defcommand "Outline" (p)
  "List this buffer's headings, or its functions and classes, in the buffer
   *Outline*, indented by how deep each is.  Return on one goes there."
  "List this buffer's headings or definitions."
  (declare (ignore p))
  (let* ((source (current-buffer))
         (entries (outline-entries source))
         (pathname (buffer-pathname source))
         (buffer (make-result-buffer "*Outline*" "Outline" 'plist-line-location)))
    (unless entries (editor-error "Nothing to outline."))
    (with-writable-buffer (buffer)
      (let ((point (buffer-point buffer)))
        (insert-string point (format nil "Outline of ~A~%~%" (buffer-name source)))
        (loop for (line depth text) in entries
              for number = (count-lines (region (buffer-start-mark source) (mark line 0)))
              do (let ((out (mark-line point)))
                   (insert-string point (format nil "~vT~A~%" (* 2 depth) text))
                   (setf (getf (line-plist out) 'result-location)
                         (if pathname
                             (list pathname number)
                             nil))))))
    (unless pathname
      (message "The buffer has no file, so its outline cannot visit it."))
    (select-window (other-window))
    (change-to-buffer buffer)
    (buffer-start (current-point))
    (next-result-line (current-point) 1)))

(defun outline-highlight-line (line)
  (let ((old (getf (line-plist line) 'outline-marks)))
    (unless (and old (eq (car old) (line-signature line)))
      (dolist (mark (cdr old))
        (hi::delete-font-mark mark))
      (let ((string (line-string line)))
        (setf (getf (line-plist line) 'outline-marks)
              (cons (line-signature line)
                    (cond ((zerop (length string)) '())
                          ((not (getf (line-plist line) 'result-location))
                           (list (hi::font-mark line 0 '(:bold t))))
                          ((char/= (char string 0) #\Space)
                           (list (hi::font-mark line 0 '(:fg 4 :bold t))))
                          (t (list (hi::font-mark line 0 4))))))))))

(define-mode-highlighter "Outline" 'outline-highlight-line)


;;;; C: the header and the source.

(defparameter *other-file-types*
  '((("c" "cc" "cpp" "cxx" "m" "mm") . ("h" "hh" "hpp" "hxx"))
    (("h" "hh" "hpp" "hxx") . ("c" "cc" "cpp" "cxx" "m" "mm"))))

(defcommand "Find Other File" (p)
  "Visit the header of this C source, or the source of this header: the
   file of the same name with the other type, in the same directory, or a
   new one if there is none."
  "Visit this file's header or source."
  (declare (ignore p))
  (let* ((pathname (or (buffer-pathname (current-buffer))
                       (editor-error "The buffer has no file.")))
         (types (cdr (assoc (pathname-type pathname) *other-file-types*
                            :test (lambda (type types)
                                    (member type types :test #'string-equal))))))
    (unless types (editor-error "Not a C source or header."))
    (let ((other (or (find-if #'probe-file
                              (mapcar (lambda (type) (make-pathname :type type :defaults pathname))
                                      types))
                     (make-pathname :type (first types) :defaults pathname))))
      (change-to-buffer (find-file-buffer other)))))

(bind-key "Find Other File" #k"control-c o" :mode "C")


;;;; Shifting lines, as Python's blocks are.

(defun map-region-lines (function)
  "Call FUNCTION on each line the region covers, or on point's line when
   the region is not active; a line the region ends at the start of is not
   covered."
  (if (region-active-p)
      (let* ((region (current-region))
             (end (region-end region)))
        (do ((line (mark-line (region-start region)) (line-next line)))
            ((or (null line)
                 (and (eq line (mark-line end)) (zerop (mark-charpos end))
                      (not (eq line (mark-line (region-start region)))))))
          (funcall function line)
          (when (eq line (mark-line end)) (return))))
      (funcall function (mark-line (current-point)))))

(defun shift-lines (columns)
  (map-region-lines
   (lambda (line)
     (let ((string (line-string line)))
       (unless (zerop (length (string-trim '(#\Space #\Tab) string)))
         (with-mark ((mark (mark line 0)))
           (if (plusp columns)
               (insert-string mark (make-string columns :initial-element #\Space))
               (let ((spaces (min (- columns)
                                  (or (position #\Space string :test-not #'char=)
                                      (length string)))))
                 (with-mark ((end mark))
                   (character-offset end spaces)
                   (delete-region (region mark end))))))))))
  (when (region-active-p)
    (setf (last-command-type) :ephemerally-active)))

(defcommand "Shift Region Right" (p)
  "Indent the lines of the region, or this line, a level more (four columns,
   or the argument's number)."
  "Indent the region's lines a level more."
  (shift-lines (or p 4)))

(defcommand "Shift Region Left" (p)
  "Indent the lines of the region, or this line, a level less (four columns,
   or the argument's number)."
  "Indent the region's lines a level less."
  (shift-lines (- (or p 4))))

(bind-key "Shift Region Right" #k"control-c \>" :mode "Python")
(bind-key "Shift Region Left" #k"control-c \<" :mode "Python")


;;;; Running a script, into the compilation buffer, whose lines visit the
;;;; places a traceback or an error names.

(defun run-buffer-file (program)
  (let* ((buffer (current-buffer))
         (pathname (or (buffer-pathname buffer) (editor-error "The buffer has no file."))))
    (when (buffer-modified buffer)
      (save-file-command nil))
    (compile-command nil
                     (format nil "~A ~A" program (shell-quote (file-namestring pathname)))
                     (directory-namestring pathname))))

(defcommand "Python Run File" (p)
  "Save this file and run it with python3, showing what it prints in the
   compilation buffer; Return on a traceback's line visits it."
  "Run this file with python3."
  (declare (ignore p))
  (run-buffer-file "python3"))

(defcommand "Shell Script Run File" (p)
  "Save this file and run it with sh -- or what its #! line names -- showing
   what it prints in the compilation buffer."
  "Run this shell script."
  (declare (ignore p))
  (let* ((first (line-string (mark-line (buffer-start-mark (current-buffer)))))
         (program (if (and (> (length first) 2) (string= "#!" first :end2 2))
                      (string-trim " " (subseq first 2))
                      "sh")))
    (run-buffer-file program)))

(defcommand "Pascal Compile File" (p)
  "Save this file and compile it with Free Pascal (fpc), showing what the
   compiler says in the compilation buffer; Return on an error visits it."
  "Compile this file with fpc."
  (declare (ignore p))
  (run-buffer-file "fpc"))

(bind-key "Pascal Compile File" #k"control-c control-c" :mode "Pascal")
(bind-key "Python Run File" #k"control-c control-c" :mode "Python")
(bind-key "Shell Script Run File" #k"control-c control-c" :mode "Shell Script")

(defcommand "Send Region to Shell" (p)
  "Send the region, or this line, to the current shell, as if typed there,
   and show the shell."
  "Send the region, or this line, to the shell."
  (declare (ignore p))
  (let ((text (if (region-active-p)
                  (region-to-string (current-region))
                  (line-string (mark-line (current-point)))))
        (here (current-window)))
    (unless (and (value current-shell) (member (value current-shell) *buffer-list*))
      (make-new-shell nil)
      (select-window here))
    (let* ((shell (value current-shell))
           (connection (variable-value 'process-connection :buffer shell)))
      (connection-write (string-right-trim '(#\Newline) text) connection)
      (connection-write (string #\Newline) connection)
      (unless (find shell *window-list* :key #'window-buffer)
        (select-window (other-window))
        (change-to-buffer shell)
        (select-window here)))))

(bind-key "Send Region to Shell" #k"control-c control-r" :mode "Shell Script")


;;;; Markdown's headings.

(defun markdown-headings (buffer)
  "((LINE LEVEL) ...) for BUFFER's # headings, outside fenced code."
  (let ((fence nil) (headings '()))
    (do ((line (mark-line (buffer-start-mark buffer)) (line-next line)))
        ((null line) (nreverse headings))
      (let* ((string (line-string line))
             (trimmed (string-left-trim " " string)))
        (cond ((or (eql 0 (search "```" trimmed)) (eql 0 (search "~~~" trimmed)))
               (setf fence (not fence)))
              ((and (not fence) (plusp (length string)) (char= (char string 0) #\#))
               (let ((level (or (position #\# string :test-not #'char=) (length string))))
                 (when (and (<= level 6)
                            (or (= level (length string))
                                (char= (char string level) #\Space)))
                   (push (list line level) headings)))))))))

(defun markdown-move (count)
  (let* ((point (current-point))
         (lines (mapcar #'first (markdown-headings (current-buffer))))
         (target (if (plusp count)
                     (nth (1- count) (remove-if-not (lambda (l) (line> l (mark-line point)))
                                                    lines))
                     (nth (1- (- count))
                          (reverse (remove-if-not (lambda (l) (line< l (mark-line point)))
                                                  lines))))))
    (unless target (editor-error "No ~:[later~;earlier~] heading." (minusp count)))
    (move-to-position point 0 target)))

(defcommand "Markdown Next Heading" (p)
  "Move to the next heading."
  "Move to the next heading."
  (markdown-move (or p 1)))

(defcommand "Markdown Previous Heading" (p)
  "Move to the previous heading."
  "Move to the previous heading."
  (markdown-move (- (or p 1))))

(defun markdown-heading-level (line)
  (second (find line (markdown-headings (line-buffer line)) :key #'first)))

(defun markdown-set-level (line level)
  (let ((old (or (markdown-heading-level line) (editor-error "Not on a heading."))))
    (setf level (max 1 (min 6 level)))
    (with-mark ((mark (mark line 0)) (end (mark line 0)))
      (character-offset end old)
      (delete-region (region mark end))
      (insert-string mark (make-string level :initial-element #\#)))))

(defcommand "Markdown Promote Heading" (p)
  "Make this heading a level higher: one # fewer."
  "Make this heading a level higher."
  (let ((line (mark-line (current-point))))
    (markdown-set-level line (- (or (markdown-heading-level line) 1) (or p 1)))))

(defcommand "Markdown Demote Heading" (p)
  "Make this heading a level lower: one # more."
  "Make this heading a level lower."
  (let ((line (mark-line (current-point))))
    (markdown-set-level line (+ (or (markdown-heading-level line) 1) (or p 1)))))

(bind-key "Markdown Next Heading" #k"control-c control-n" :mode "Markdown")
(bind-key "Markdown Previous Heading" #k"control-c control-p" :mode "Markdown")
(bind-key "Markdown Promote Heading" #k"control-c \-" :mode "Markdown")
(bind-key "Markdown Demote Heading" #k"control-c =" :mode "Markdown")


;;;; Menus for these modes.

(define-menu "C" (:mode "C")
  ("Beginning of Function" "Beginning of Definition")
  ("End of Function" "End of Definition")
  ("Mark Function" "Mark Definition")
  ("Outline" "Outline")
  :separator
  ("Header or Source" "Find Other File")
  ("Compile…" "Compile")
  ("Next Error" "Next Result"))

(define-menu "Python" (:mode "Python")
  ("Beginning of Definition" "Beginning of Definition")
  ("End of Definition" "End of Definition")
  ("Mark Definition" "Mark Definition")
  ("Outline" "Outline")
  :separator
  ("Shift Right" "Shift Region Right")
  ("Shift Left" "Shift Region Left")
  :separator
  ("Run File" "Python Run File")
  ("Next Error" "Next Result"))

(define-menu "Shell Script" (:mode "Shell Script")
  ("Beginning of Function" "Beginning of Definition")
  ("End of Function" "End of Definition")
  ("Outline" "Outline")
  :separator
  ("Send Region to Shell" "Send Region to Shell")
  ("Run File" "Shell Script Run File"))

(define-menu "Pascal" (:mode "Pascal")
  ("Beginning of Procedure" "Beginning of Definition")
  ("End of Procedure" "End of Definition")
  ("Outline" "Outline")
  :separator
  ("Compile with fpc" "Pascal Compile File")
  ("Next Error" "Next Result"))

(define-menu "Markdown" (:mode "Markdown")
  ("Next Heading" "Markdown Next Heading")
  ("Previous Heading" "Markdown Previous Heading")
  ("Promote Heading" "Markdown Promote Heading")
  ("Demote Heading" "Markdown Demote Heading")
  ("Outline" "Outline")
  :separator
  ("Open Link" "Open Link"))
