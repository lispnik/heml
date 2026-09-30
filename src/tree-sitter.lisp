;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;;; Syntax highlighting with tree-sitter.
;;;;
;;;; tree-sitter parses a buffer into a syntax tree with a grammar, a shared
;;;; library for each language, and a query, highlights.scm, names the parts
;;;; of the tree to colour: @comment, @string, @keyword and so on.  A mode
;;;; highlighted this way names its language with DEFINE-TREE-SITTER-LANGUAGE,
;;;; and its buffers are coloured when the library, the grammar and the query
;;;; can be found; otherwise they are not coloured, and nothing complains.
;;;;
;;;; A buffer is parsed whole, again whenever its signature changes, and a
;;;; line is coloured when redisplay draws it, from the captures of the query
;;;; within the line's bytes.  Redisplay draws only what is visible, so only
;;;; visible lines are coloured.
;;;;
;;;; SBCL only.  Much of tree-sitter's interface passes a node, a 32-byte
;;;; struct, by value, which sb-alien does and CFFI cannot without libffi.

(defpackage :heml.tree-sitter
  (:use :common-lisp)
  (:export #:define-tree-sitter-language
           #:*tree-sitter-directories*
           #:tree-sitter-available-p))

(in-package :heml.tree-sitter)


;;;; Finding the library, grammars and queries

(defvar *tree-sitter-directories*
  (remove nil
          (append
           (let ((path (uiop:getenv "HEML_TREE_SITTER_DIR")))
             (when (plusp (length path))
               (uiop:split-string path :separator ":")))
           (list (ignore-errors
                  (namestring (asdf:system-relative-pathname :heml.base "build/tree-sitter/")))
                 (namestring (merge-pathnames ".local/share/heml/tree-sitter/"
                                              (user-homedir-pathname)))
                 "/opt/homebrew/"
                 "/usr/local/")))
  "Where tree-sitter things are looked for, each laid out as Homebrew lays
them out: lib/libtree-sitter.dylib, lib/libtree-sitter-<language>.dylib and
share/tree-sitter/queries/<language>/highlights.scm.  `make tree-sitter'
builds Heml's grammars into build/tree-sitter/, and `make install-tree-sitter'
copies them to ~/.local/share/heml/tree-sitter/.")

(defun find-in-directories (relative)
  (loop for directory in *tree-sitter-directories*
        for file = (probe-file (merge-pathnames relative (uiop:ensure-directory-pathname directory)))
        when file return file))

(defvar *library-state* nil
  "NIL until the library is looked for, then :LOADED or :MISSING.")

(defun tree-sitter-available-p ()
  "Load the tree-sitter library if it can be found.  True when it is loaded."
  (unless *library-state*
    (setf *library-state*
          (let ((library (find-in-directories "lib/libtree-sitter.dylib")))
            (if (and library
                     (ignore-errors
                      (sb-alien:load-shared-object library :dont-save t)
                      t))
                :loaded
                :missing))))
  (eq *library-state* :loaded))


;;;; Calling it

(sb-alien:define-alien-type nil
    (sb-alien:struct ts-node
      (context (sb-alien:array (sb-alien:unsigned 32) 4))
      (id sb-sys:system-area-pointer)
      (tree sb-sys:system-area-pointer)))

(defconstant +node-size+ 32)
(defconstant +capture-size+ 40)         ; a node, a uint32, and padding
(defconstant +match-size+ 16)           ; id, pattern, count, captures

(defvar *addresses* (make-hash-table :test 'equal))

(defun address (name)
  (or (gethash name *addresses*)
      (setf (gethash name *addresses*)
            (or (sb-sys:find-dynamic-foreign-symbol-address name)
                (error "No ~A in the loaded tree-sitter libraries." name)))))

;;; (TS NAME RESULT-TYPE (TYPE ARGUMENT)*) calls the C function NAME.
;;;
(defmacro ts (name result-type &rest arguments)
  `(sb-alien:alien-funcall
    (sb-alien:sap-alien (sb-sys:int-sap (address ,name))
                        (function ,result-type ,@(mapcar #'first arguments)))
    ,@(mapcar #'second arguments)))

(defmacro node-at (sap)
  "The node stored at SAP, as an argument passed by value."
  `(sb-alien:deref (sb-alien:sap-alien ,sap (* (sb-alien:struct ts-node)))))

(defun foreign-string (sap length)
  (let ((octets (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (i length)
      (setf (aref octets i) (sb-sys:sap-ref-8 sap i)))
    (babel:octets-to-string octets :encoding :utf-8)))

(defun call-with-foreign-memory (size function)
  (let ((sap (sb-alien:alien-sap (sb-alien:make-alien (sb-alien:unsigned 8) size))))
    (unwind-protect (funcall function sap)
      (sb-alien:free-alien (sb-alien:sap-alien sap (* (sb-alien:unsigned 8)))))))

(defmacro with-foreign-memory ((sap size) &body body)
  `(call-with-foreign-memory ,size (lambda (,sap) ,@body)))


;;;; Languages

(defstruct (language (:constructor %make-language))
  name                 ; as in libtree-sitter-<name>.dylib and tree_sitter_<name>
  precedence           ; :FIRST or :LAST, which pattern wins on the same text
  state                ; NIL, :READY or :MISSING
  pointer              ; the TSLanguage
  query                ; the TSQuery
  fonts                ; capture index to font, or NIL for none
  predicates           ; pattern index to its predicates
  problem              ; why it could not be loaded, when it could not
  (indent-query :unloaded) ; the indentation query, NIL when there is none
  indent-kinds         ; its capture index to :BEGIN, :END and the like
  indent-predicates    ; its pattern index to its predicates
  (indent-width 4)     ; columns a level of indentation is
  opens                ; a regex: a line ending so opens a block
  closes               ; a regex: a line starting so closes one
  finishes)            ; a regex: a line starting so ends its block

(defvar *languages* (make-hash-table :test 'equal))

(defun define-tree-sitter-language (name &key mode (precedence :first) fallback
                                                indent (indent-width 4) opens closes finishes)
  "Highlight buffers whose major mode is MODE with tree-sitter's grammar NAME
and its highlight query.  PRECEDENCE says which pattern wins when two capture
the same text: :FIRST, tree-sitter's own rule, followed by the queries that
come with grammars, or :LAST, Neovim's, followed by its queries.  FALLBACK,
when given, is MODE's highlighter instead, from the first time it is needed,
if the grammar or its query cannot be loaded.  INDENT makes MODE's lines
indented as the language's indents.scm says, INDENT-WIDTH columns a level,
with spaces."
  (let ((language (%make-language :name name :precedence precedence
                                  :indent-width indent-width
                                  :opens (and opens (ppcre:create-scanner opens))
                                  :closes (and closes (ppcre:create-scanner closes))
                                  :finishes (and finishes (ppcre:create-scanner finishes)))))
    (setf (gethash name *languages*) language)
    (when (and mode indent)
      (heml-interface:defhvar "Indent Function"
        "Indentation function which is invoked by \"Indent\" command."
        :mode mode :value (lambda (mark) (tree-sitter-indent-line language mark)))
      (heml-interface:defhvar "Indent with Tabs"
        "Whether indentation uses tabs."
        :mode mode :value nil))
    (when mode
      (heml-interface:define-mode-highlighter
       mode (lambda (line)
              (cond ((language-ready-p language)
                     (highlight-line language line))
                    (fallback
                     (heml-interface:define-mode-highlighter mode fallback)
                     (funcall fallback line))))))
    language))

(defun language-ready-p (language)
  "Load LANGUAGE's grammar and query the first time.  True when they are."
  (unless (language-state language)
    (setf (language-state language)
          (handler-case (progn (load-language language) :ready)
            ;; Said nowhere: the terminal editor's error output is its
            ;; screen, and a mode without tree-sitter is simply not coloured.
            ;; The reason stays here for anyone who asks.
            (error (condition)
              (setf (language-problem language) (princ-to-string condition))
              :missing))))
  (eq (language-state language) :ready))

(defun load-language (language)
  (let* ((name (language-name language))
         (grammar (find-in-directories (format nil "lib/libtree-sitter-~A.dylib" name)))
         (query-file (find-in-directories
                      (format nil "share/tree-sitter/queries/~A/highlights.scm" name))))
    (unless (tree-sitter-available-p)
      (error "the tree-sitter library is not installed"))
    (unless grammar (error "its grammar is not installed"))
    (unless query-file (error "its highlight query is not installed"))
    (sb-alien:load-shared-object grammar :dont-save t)
    (let ((pointer (ts (format nil "tree_sitter_~A" (substitute #\_ #\- name))
                       sb-sys:system-area-pointer)))
      (setf (language-pointer language) pointer
            (language-query language) (make-query pointer (uiop:read-file-string query-file)))
      (read-captures language)
      (read-predicates language))))

(defun make-query (language source)
  (let ((octets (babel:string-to-octets source :encoding :utf-8)))
    (with-foreign-memory (out 8)
      (let ((query (sb-sys:with-pinned-objects (octets)
                     (ts "ts_query_new" sb-sys:system-area-pointer
                         (sb-sys:system-area-pointer language)
                         (sb-sys:system-area-pointer (sb-sys:vector-sap octets))
                         ((sb-alien:unsigned 32) (length octets))
                         (sb-sys:system-area-pointer out)
                         (sb-sys:system-area-pointer (sb-sys:sap+ out 4))))))
        (when (zerop (sb-sys:sap-int query))
          (error "its highlight query does not suit the grammar (error ~D at byte ~D)"
                 (sb-sys:sap-ref-32 out 4) (sb-sys:sap-ref-32 out 0)))
        query))))

(defun query-string (function query id)
  (with-foreign-memory (length 4)
    (let ((sap (sb-alien:alien-funcall
                (sb-alien:sap-alien (sb-sys:int-sap (address function))
                                    (function sb-sys:system-area-pointer
                                              sb-sys:system-area-pointer
                                              (sb-alien:unsigned 32)
                                              sb-sys:system-area-pointer))
                query id length)))
      (foreign-string sap (sb-sys:sap-ref-32 length 0)))))

(defun read-captures (language)
  (let* ((query (language-query language))
         (count (ts "ts_query_capture_count" (sb-alien:unsigned 32)
                    (sb-sys:system-area-pointer query)))
         (fonts (make-array count)))
    (dotimes (i count)
      (setf (aref fonts i)
            (capture-font (query-string "ts_query_capture_name_for_id" query i))))
    (setf (language-fonts language) fonts)))

;;; A pattern's predicates are (NAME ARGUMENT*), each argument a capture's
;;; index, as (:CAPTURE . INDEX), or a string.
;;;
(defun read-predicates (language)
  (let* ((query (language-query language))
         (count (ts "ts_query_pattern_count" (sb-alien:unsigned 32)
                    (sb-sys:system-area-pointer query)))
         (predicates (make-array count)))
    (dotimes (pattern count)
      (with-foreign-memory (step-count 4)
        (let ((steps (ts "ts_query_predicates_for_pattern" sb-sys:system-area-pointer
                         (sb-sys:system-area-pointer query)
                         ((sb-alien:unsigned 32) pattern)
                         (sb-sys:system-area-pointer step-count)))
              (current '())
              (all '()))
          (dotimes (i (sb-sys:sap-ref-32 step-count 0))
            (let ((type (sb-sys:sap-ref-32 steps (* i 8)))
                  (value (sb-sys:sap-ref-32 steps (+ 4 (* i 8)))))
              (ecase type
                (0 (push (nreverse current) all)
                   (setf current '()))
                (1 (push (cons :capture value) current))
                (2 (push (query-string "ts_query_string_value_for_id" query value)
                         current)))))
          (setf (aref predicates pattern) (nreverse all)))))
    (setf (language-predicates language) predicates)))


;;;; Colours

(defparameter *capture-fonts*
  '(("comment" . 1)
    ("string.special" . 5) ("string" . 4) ("character" . 4)
    ("escape" . 6) ("embedded" . 6)
    ("number" . 3) ("float" . 3) ("boolean" . 3) ("constant" . 3)
    ("keyword" . 5) ("conditional" . 5) ("repeat" . 5) ("include" . 5)
    ("exception" . 5) ("label" . 5) ("module" . 5)
    ("function.macro" . 5) ("function" . 6) ("method" . 6) ("constructor" . 6)
    ("type" . 2) ("attribute" . 6) ("variable.builtin" . 3)
    ("text.title" . (:fg 4 :bold t)) ("markup.heading" . (:fg 4 :bold t))
    ("text.literal" . 2) ("markup.raw" . 2)
    ("text.uri" . 6) ("text.reference" . 6) ("markup.link" . 6)
    ("punctuation.special" . 5) ("markup.list" . 5))
  "Capture names, most specific first, to the fonts, which are colours, they
are drawn in.  A name is matched by its leading components: @function.builtin
by \"function\".  A capture that matches none, such as @variable, is not
coloured.")

(defun capture-font (name)
  (cdr (find-if (lambda (entry)
                  (let ((prefix (car entry)))
                    (and (uiop:string-prefix-p prefix name)
                         (or (= (length prefix) (length name))
                             (char= (char name (length prefix)) #\.)))))
                *capture-fonts*)))


;;;; Parsing a buffer

(defstruct (parse (:constructor %make-parse))
  signature            ; the buffer's signature when parsed
  tree                 ; the TSTree
  root                 ; foreign memory holding the root node
  octets               ; the buffer as UTF-8, which the tree indexes
  line-starts          ; line to the byte its text starts at
  lines                ; row to line
  (indents :unmade))   ; node id to its indentation captures, made when needed

(defvar *parses* (make-hash-table :test 'eq :weakness :key)
  "Buffer to its latest parse.")

(defvar *parser* nil)

(defun buffer-octets (buffer)
  "BUFFER's text as UTF-8, and a table of where each of its lines starts."
  (let ((starts (make-hash-table :test 'eq))
        (chunks '())
        (lines '())
        (offset 0))
    (do ((line (heml-interface:mark-line (heml-interface:buffer-start-mark buffer))
               (heml-interface:line-next line)))
        ((null line))
      (let ((octets (babel:string-to-octets (heml-interface:line-string line)
                                            :encoding :utf-8)))
        (setf (gethash line starts) offset)
        (push line lines)
        (push octets chunks)
        (incf offset (1+ (length octets)))))
    (let ((all (make-array offset :element-type '(unsigned-byte 8)
                                  :initial-element 10))
          (position 0))
      (dolist (octets (nreverse chunks))
        (replace all octets :start1 position)
        (incf position (1+ (length octets))))
      (values all starts (coerce (nreverse lines) 'simple-vector)))))

(defun free-parse (parse)
  (ts "ts_tree_delete" sb-alien:void (sb-sys:system-area-pointer (parse-tree parse)))
  (sb-alien:free-alien (sb-alien:sap-alien (parse-root parse) (* (sb-alien:unsigned 8)))))

(defun buffer-parse (language buffer)
  "BUFFER parsed with LANGUAGE, parsing it again if it has changed."
  (let ((parse (gethash buffer *parses*))
        (signature (heml-interface:buffer-signature buffer)))
    (if (and parse (eql (parse-signature parse) signature))
        parse
        (progn
          (when parse (free-parse parse))
          (unless *parser*
            (setf *parser* (ts "ts_parser_new" sb-sys:system-area-pointer)))
          (ts "ts_parser_set_language" (sb-alien:unsigned 8)
              (sb-sys:system-area-pointer *parser*)
              (sb-sys:system-area-pointer (language-pointer language)))
          (multiple-value-bind (octets starts lines) (buffer-octets buffer)
            (let* ((tree (sb-sys:with-pinned-objects (octets)
                           (ts "ts_parser_parse_string" sb-sys:system-area-pointer
                               (sb-sys:system-area-pointer *parser*)
                               (sb-sys:system-area-pointer (sb-sys:int-sap 0))
                               (sb-sys:system-area-pointer (sb-sys:vector-sap octets))
                               ((sb-alien:unsigned 32) (length octets)))))
                   (root (sb-alien:alien-sap
                          (sb-alien:make-alien (sb-alien:unsigned 8) +node-size+))))
              (setf (node-at root)
                    (ts "ts_tree_root_node" (sb-alien:struct ts-node)
                        (sb-sys:system-area-pointer tree)))
              (setf (gethash buffer *parses*)
                    (%make-parse :signature signature :tree tree :root root
                                 :octets octets :line-starts starts
                                 :lines lines))))))))


;;;; Colouring a line

(defvar *cursor* nil)

(defun node-bytes (sap)
  (values (ts "ts_node_start_byte" (sb-alien:unsigned 32) ((sb-alien:struct ts-node) (node-at sap)))
          (ts "ts_node_end_byte" (sb-alien:unsigned 32) ((sb-alien:struct ts-node) (node-at sap)))))

(defun match-texts (parse captures count index)
  "The text of each capture numbered INDEX in a match."
  (loop for k below count
        for capture = (sb-sys:sap+ captures (* k +capture-size+))
        when (= index (sb-sys:sap-ref-32 capture +node-size+))
          collect (multiple-value-bind (start end) (node-bytes capture)
                    (babel:octets-to-string (parse-octets parse)
                                            :start start :end end :encoding :utf-8))))

(defun lua-pattern-regex (pattern)
  "Neovim's queries write some patterns as Lua patterns: the common classes
become their regular-expression equivalents."
  (with-output-to-string (out)
    (loop with i = 0
          while (< i (length pattern))
          do (let ((c (char pattern i)))
               (if (and (char= c #\%) (< (1+ i) (length pattern)))
                   (let ((class (char pattern (1+ i))))
                     (write-string (case class
                                     (#\a "[A-Za-z]") (#\d "[0-9]") (#\s "\\s")
                                     (#\w "[A-Za-z0-9]") (#\l "[a-z]") (#\u "[A-Z]")
                                     (#\p "[!-/:-@\\[-`{-~]")
                                     (t (format nil "\\~C" class)))
                                   out)
                     (incf i 2))
                   (progn (write-char c out) (incf i)))))))

(defun match-kinds (captures count index)
  "The node type of each capture numbered INDEX in a match."
  (loop for k below count
        for capture = (sb-sys:sap+ captures (* k +capture-size+))
        when (= index (sb-sys:sap-ref-32 capture +node-size+))
          collect (ts "ts_node_type" sb-alien:c-string ((sb-alien:struct ts-node) (node-at capture)))))

(defun predicates-hold-p (language parse pattern captures count
                          &optional (predicates (language-predicates language)))
  (every (lambda (predicate)
           (destructuring-bind (name &rest arguments) predicate
             (flet ((texts (argument)
                      (if (consp argument)
                          (match-texts parse captures count (cdr argument))
                          (list argument))))
               (let* ((negated (uiop:string-prefix-p "not-" name))
                      (name (if negated (subseq name 4) name))
                      (subject (texts (first arguments)))
                      (holds
                        (cond
                          ((string= name "eq?")
                           (let ((other (texts (second arguments))))
                             (every (lambda (text) (member text other :test #'string=)) subject)))
                          ((or (string= name "match?") (string= name "lua-match?"))
                           (let ((regex (if (string= name "lua-match?")
                                            (lua-pattern-regex (second arguments))
                                            (second arguments))))
                             (every (lambda (text) (ppcre:scan regex text)) subject)))
                          ((string= name "any-of?")
                           (every (lambda (text) (member text (rest arguments) :test #'string=))
                                  subject))
                          ((string= name "kind-eq?")
                           (every (lambda (kind) (member kind (rest arguments) :test #'string=))
                                  (match-kinds captures count (cdr (first arguments)))))
                          ;; #set!, #offset! and the like say nothing about
                          ;; whether the pattern applies.
                          (t t))))
                 (if negated (not holds) holds)))))
         (aref predicates pattern)))

(defun line-char-index (string byte)
  "The index in STRING of the character that starts BYTE bytes into its UTF-8."
  (let ((bytes 0))
    (dotimes (i (length string) (length string))
      (when (>= bytes byte) (return i))
      (let ((code (char-code (char string i))))
        (incf bytes (cond ((< code #x80) 1) ((< code #x800) 2) ((< code #x10000) 3) (t 4)))))))

(defun line-fonts (language parse line)
  "A vector of the font each character of LINE is drawn in, NIL for none."
  (let* ((string (heml-interface:line-string line))
         (fonts (make-array (length string) :initial-element nil))
         (line-start (gethash line (parse-line-starts parse)))
         (line-end (+ line-start (babel:string-size-in-octets string :encoding :utf-8)))
         (ascii (= (- line-end line-start) (length string)))
         (spans '()))
    (unless *cursor*
      (setf *cursor* (ts "ts_query_cursor_new" sb-sys:system-area-pointer)))
    (ts "ts_query_cursor_set_byte_range" (sb-alien:unsigned 8)
        (sb-sys:system-area-pointer *cursor*)
        ((sb-alien:unsigned 32) line-start)
        ((sb-alien:unsigned 32) (1+ line-end)))
    (ts "ts_query_cursor_exec" sb-alien:void
        (sb-sys:system-area-pointer *cursor*)
        (sb-sys:system-area-pointer (language-query language))
        ((sb-alien:struct ts-node) (node-at (parse-root parse))))
    (with-foreign-memory (match (+ +match-size+ 4))
      (let ((capture-index (sb-sys:sap+ match +match-size+)))
        (loop while (plusp (ts "ts_query_cursor_next_capture" (sb-alien:unsigned 8)
                               (sb-sys:system-area-pointer *cursor*)
                               (sb-sys:system-area-pointer match)
                               (sb-sys:system-area-pointer capture-index)))
              do (let* ((pattern (sb-sys:sap-ref-16 match 4))
                        (count (sb-sys:sap-ref-16 match 6))
                        (captures (sb-sys:sap-ref-sap match 8))
                        (capture (sb-sys:sap+ captures (* (sb-sys:sap-ref-32 capture-index 0)
                                                          +capture-size+)))
                        (font (aref (language-fonts language)
                                    (sb-sys:sap-ref-32 capture +node-size+))))
                   (when font
                     (multiple-value-bind (start end) (node-bytes capture)
                       (let ((start (max start line-start))
                             (end (min end line-end)))
                         (when (and (< start end)
                                    (predicates-hold-p language parse pattern captures count))
                           (let ((from (- start line-start))
                                 (to (- end line-start)))
                             (unless ascii
                               (setf from (line-char-index string from)
                                     to (line-char-index string to)))
                             (push (list from to pattern font) spans))))))))))
    ;; Wider spans first, so that what is inside them shows through, and of
    ;; two on the same text, the one whose pattern wins last.
    (setf spans (sort spans (if (eq (language-precedence language) :last)
                                (lambda (a b)
                                  (let ((wa (- (second a) (first a)))
                                        (wb (- (second b) (first b))))
                                    (or (> wa wb) (and (= wa wb) (< (third a) (third b))))))
                                (lambda (a b)
                                  (let ((wa (- (second a) (first a)))
                                        (wb (- (second b) (first b))))
                                    (or (> wa wb) (and (= wa wb) (> (third a) (third b)))))))))
    (dolist (span spans fonts)
      (destructuring-bind (from to pattern font) span
        (declare (ignore pattern))
        (fill fonts font :start from :end to)))))

(defun highlight-line (language line)
  "Bring LINE's font marks up to date with LANGUAGE's highlighting."
  (let ((buffer (heml-interface:line-buffer line)))
    (when (and buffer (language-ready-p language))
      (let* ((parse (buffer-parse language buffer))
             (plist (heml-interface:line-plist line))
             (old (getf plist 'tree-sitter)))
        (unless (and old (eq (car old) parse))
          (dolist (mark (cdr old))
            (hi:delete-font-mark mark))
          (let ((fonts (line-fonts language parse line))
                (marks '())
                (last nil))
            (dotimes (i (length fonts))
              (let ((font (aref fonts i)))
                (unless (equal font last)
                  (push (hi:font-mark line i (or font 0)) marks)
                  (setf last font))))
            (setf (getf (heml-interface:line-plist line) 'tree-sitter)
                  (cons parse marks))))))))


;;;; Indentation.

;;; A language's indents.scm marks the nodes that indent the lines inside
;;; them (@indent.begin), a line that starting with them goes back out a
;;; level (@indent.branch, @indent.end, @indent.dedent), ones whose lines
;;; keep their indentation (@indent.auto), and more, as Neovim's queries do;
;;; and a line's indentation is worked out as Neovim works it out, from the
;;; node the line starts with -- for a blank line, the one the line before
;;; ends with -- and each node enclosing it.

(sb-alien:define-alien-type nil
    (sb-alien:struct ts-point
      (row (sb-alien:unsigned 32))
      (column (sb-alien:unsigned 32))))

;;; A node, while Lisp holds it, is its 32 bytes in a vector, pinned while
;;; they are passed by value.

(defun make-node () (make-array +node-size+ :element-type '(unsigned-byte 8)))

(defun node-copy (sap)
  (let ((node (make-node)))
    (dotimes (i +node-size+ node)
      (setf (aref node i) (sb-sys:sap-ref-8 sap i)))))

(defmacro with-node ((sap node) &body body)
  `(let ((%node ,node))
     (sb-sys:with-pinned-objects (%node)
       (let ((,sap (sb-sys:vector-sap %node)))
         ,@body))))

(defun node-null-p (node)
  (with-node (sap node)
    (plusp (ts "ts_node_is_null" (sb-alien:unsigned 8) ((sb-alien:struct ts-node) (node-at sap))))))

(defun node-returning (name node &rest more)
  "The node the C function NAME returns for NODE, or NIL for its null node."
  (let ((result (make-node)))
    (with-node (in node)
      (with-node (out result)
        (setf (node-at out)
              (if more
                  (ts name (sb-alien:struct ts-node) ((sb-alien:struct ts-node) (node-at in))
                      ((sb-alien:unsigned 32) (first more)))
                  (ts name (sb-alien:struct ts-node) ((sb-alien:struct ts-node) (node-at in)))))))
    (unless (node-null-p result) result)))

(defun node-parent (node) (node-returning "ts_node_parent" node))

(defun node-children (node)
  (let ((count (with-node (sap node)
                 (ts "ts_node_child_count" (sb-alien:unsigned 32) ((sb-alien:struct ts-node) (node-at sap))))))
    (loop for i below count
          for child = (node-returning "ts_node_child" node i)
          when child collect child)))

(defun node-point (name node)
  (let ((point (make-array 8 :element-type '(unsigned-byte 8))))
    (with-node (sap node)
      (sb-sys:with-pinned-objects (point)
        (let ((psap (sb-sys:vector-sap point)))
          (setf (sb-alien:deref (sb-alien:sap-alien psap (* (sb-alien:struct ts-point))))
                (ts name (sb-alien:struct ts-point) ((sb-alien:struct ts-node) (node-at sap))))
          (values (sb-sys:sap-ref-32 psap 0) (sb-sys:sap-ref-32 psap 4)))))))

(defun node-start (node) (node-point "ts_node_start_point" node))
(defun node-end (node) (node-point "ts_node_end_point" node))

(defun node-type (node)
  (with-node (sap node) (ts "ts_node_type" sb-alien:c-string ((sb-alien:struct ts-node) (node-at sap)))))

(defun node-has-error-p (node)
  (with-node (sap node) (plusp (ts "ts_node_has_error" (sb-alien:unsigned 8) ((sb-alien:struct ts-node) (node-at sap))))))

(defun node-id (node)
  (with-node (sap node) (sb-sys:sap-int (sb-sys:sap-ref-sap sap 16))))

(defun descendant-at (root row column)
  "The smallest node of ROOT's tree at ROW and COLUMN, a byte in that row."
  (let ((point (make-array 8 :element-type '(unsigned-byte 8)))
        (result (make-node)))
    (sb-sys:with-pinned-objects (point)
      (let ((psap (sb-sys:vector-sap point)))
        (setf (sb-sys:sap-ref-32 psap 0) row
              (sb-sys:sap-ref-32 psap 4) column)
        (with-node (in root)
          (with-node (out result)
            (setf (node-at out)
                  (ts "ts_node_descendant_for_point_range" (sb-alien:struct ts-node)
                      ((sb-alien:struct ts-node) (node-at in))
                      ((sb-alien:struct ts-point)
                       (sb-alien:deref (sb-alien:sap-alien psap (* (sb-alien:struct ts-point)))))
                      ((sb-alien:struct ts-point)
                       (sb-alien:deref (sb-alien:sap-alien psap (* (sb-alien:struct ts-point)))))))))))
    result))

;;; The query, read the first time a line is indented.

(defun query-predicates (query)
  (let* ((count (ts "ts_query_pattern_count" (sb-alien:unsigned 32)
                    (sb-sys:system-area-pointer query)))
         (predicates (make-array count)))
    (dotimes (pattern count predicates)
      (with-foreign-memory (step-count 4)
        (let ((steps (ts "ts_query_predicates_for_pattern" sb-sys:system-area-pointer
                         (sb-sys:system-area-pointer query)
                         ((sb-alien:unsigned 32) pattern)
                         (sb-sys:system-area-pointer step-count)))
              (current '())
              (all '()))
          (dotimes (i (sb-sys:sap-ref-32 step-count 0))
            (let ((type (sb-sys:sap-ref-32 steps (* i 8)))
                  (value (sb-sys:sap-ref-32 steps (+ 4 (* i 8)))))
              (ecase type
                (0 (push (nreverse current) all)
                   (setf current '()))
                (1 (push (cons :capture value) current))
                (2 (push (query-string "ts_query_string_value_for_id" query value)
                         current)))))
          (setf (aref predicates pattern) (nreverse all)))))))

(defun language-indent-ready-p (language)
  "Load LANGUAGE's indents.scm the first time.  True when there is one."
  (when (eq (language-indent-query language) :unloaded)
    (setf (language-indent-query language)
          (let ((file (find-in-directories
                       (format nil "share/tree-sitter/queries/~A/indents.scm"
                               (language-name language)))))
            (when file
              (handler-case
                  (let* ((query (make-query (language-pointer language)
                                            (uiop:read-file-string file)))
                         (count (ts "ts_query_capture_count" (sb-alien:unsigned 32)
                                    (sb-sys:system-area-pointer query)))
                         (kinds (make-array count)))
                    (dotimes (i count)
                      (let ((name (query-string "ts_query_capture_name_for_id" query i)))
                        (setf (aref kinds i)
                              (and (uiop:string-prefix-p "indent." name)
                                   (intern (string-upcase (subseq name 7)) :keyword)))))
                    (setf (language-indent-kinds language) kinds
                          (language-indent-predicates language) (query-predicates query))
                    query)
                (error (condition)
                  (setf (language-problem language) (princ-to-string condition))
                  nil))))))
  (language-indent-query language))

(defun pattern-settings (predicates)
  "What a pattern's #set! directives say, as an alist of key to value."
  (loop for (name . arguments) in predicates
        when (string= name "set!")
          collect (let ((strings (remove-if-not #'stringp arguments)))
                    (cons (first strings) (or (second strings) t)))))

(defun parse-indent-map (language parse)
  "Node id to ((KIND . SETTINGS) ...), from LANGUAGE's indents.scm over the
whole of PARSE's tree."
  (when (eq (parse-indents parse) :unmade)
    (let ((map (make-hash-table))
          (cursor (ts "ts_query_cursor_new" sb-sys:system-area-pointer))
          (kinds (language-indent-kinds language))
          (predicates (language-indent-predicates language)))
      (unwind-protect
           (progn
             (ts "ts_query_cursor_exec" sb-alien:void
                 (sb-sys:system-area-pointer cursor)
                 (sb-sys:system-area-pointer (language-indent-query language))
                 ((sb-alien:struct ts-node) (node-at (parse-root parse))))
             (with-foreign-memory (match (+ +match-size+ 4))
               (let ((capture-index (sb-sys:sap+ match +match-size+)))
                 (loop while (plusp (ts "ts_query_cursor_next_capture" (sb-alien:unsigned 8)
                                        (sb-sys:system-area-pointer cursor)
                                        (sb-sys:system-area-pointer match)
                                        (sb-sys:system-area-pointer capture-index)))
                       do (let* ((pattern (sb-sys:sap-ref-16 match 4))
                                 (count (sb-sys:sap-ref-16 match 6))
                                 (captures (sb-sys:sap-ref-sap match 8))
                                 (capture (sb-sys:sap+ captures (* (sb-sys:sap-ref-32 capture-index 0)
                                                                   +capture-size+)))
                                 (kind (aref kinds (sb-sys:sap-ref-32 capture +node-size+))))
                            (when (and kind
                                       (predicates-hold-p language parse pattern captures count
                                                          predicates))
                              (push (cons kind (pattern-settings (aref predicates pattern)))
                                    (gethash (sb-sys:sap-int (sb-sys:sap-ref-sap capture 16)) map))))))))
        (ts "ts_query_cursor_delete" sb-alien:void (sb-sys:system-area-pointer cursor)))
      (setf (parse-indents parse) map)))
  (parse-indents parse))

(defun indent-kind (map node kind)
  "The settings of NODE's KIND capture, T when it has none, or NIL."
  (let ((entry (assoc kind (gethash (node-id node) map))))
    (and entry (or (cdr entry) t))))

(defun setting (settings key)
  (and (consp settings) (cdr (assoc key settings :test #'equal))))

(defun leading-columns (string)
  "How far STRING's first non-blank character is indented, tabs every eight."
  (let ((column 0))
    (loop for c across string
          do (case c
               (#\Space (incf column))
               (#\Tab (setf column (* 8 (1+ (floor column 8)))))
               (t (return))))
    column))

(defun blank-string-p (string)
  (every (lambda (c) (member c '(#\Space #\Tab))) string))

(defun string-byte (string index)
  (babel:string-size-in-octets string :end index :encoding :utf-8))

(defun find-delimiter (parse node delimiter)
  "NODE's child that is DELIMITER, and whether nothing but blanks and more
of it follow it on its line."
  (dolist (child (node-children node))
    (when (string= (node-type child) delimiter)
      (multiple-value-bind (row column) (node-end child)
        (let* ((string (heml-interface:line-string (svref (parse-lines parse) row)))
               (after (subseq string (line-char-index string column))))
          (return (values child
                          (every (lambda (c) (or (member c '(#\Space #\Tab))
                                                 (find c delimiter)))
                                 after))))))))

(defun indent-column (language parse line)
  "The column LINE should be indented to, or -1 to keep the previous line's
indentation."
  (let* ((map (parse-indent-map language parse))
         (lines (parse-lines parse))
         (row (position line lines))
         (width (language-indent-width language))
         (root (node-copy (parse-root parse)))
         (string (heml-interface:line-string line))
         (node
           (if (blank-string-p string)
               (let ((previous (loop for r from (1- row) downto 0
                                     unless (blank-string-p
                                             (heml-interface:line-string (svref lines r)))
                                       return r)))
                 (unless previous (return-from indent-column 0))
                 (let* ((text (heml-interface:line-string (svref lines previous)))
                        (last (position-if-not (lambda (c) (member c '(#\Space #\Tab)))
                                               text :from-end t))
                        (node (descendant-at root previous (string-byte text last))))
                   ;; A comment ending the line says nothing: the node
                   ;; before it does.
                   (when (search "comment" (node-type node))
                     (let ((first (descendant-at root previous
                                                 (string-byte text (position-if-not
                                                                    (lambda (c) (member c '(#\Space #\Tab)))
                                                                    text)))))
                       (unless (= (node-id first) (node-id node))
                         (let* ((start (line-char-index text (nth-value 1 (node-start node))))
                                (before (position-if-not (lambda (c) (member c '(#\Space #\Tab)))
                                                         text :end start :from-end t)))
                           (when before
                             (setf node (descendant-at root previous (string-byte text before))))))))
                   (if (indent-kind map node :end)
                       (descendant-at root row 0)
                       node)))
               (descendant-at root row
                              (string-byte string (position-if-not
                                                   (lambda (c) (member c '(#\Space #\Tab)))
                                                   string)))))
         (indent 0)
         (processed (make-hash-table)))
    (when (indent-kind map node :zero)
      (return-from indent-column 0))
    (loop while node
          do (let ((begin (indent-kind map node :begin))
                   (align (indent-kind map node :align))
                   (is-processed nil))
               (multiple-value-bind (srow) (node-start node)
                 (let ((erow (node-end node)))
                   (when (and (not begin) (not align) (indent-kind map node :auto)
                              (< srow row) (<= row erow))
                     (return-from indent-column -1))
                   (when (and (not begin) (indent-kind map node :ignore)
                              (< srow row) (<= row erow))
                     (return-from indent-column 0))
                   (when (and (not (gethash srow processed))
                              (or (and (indent-kind map node :branch) (= srow row))
                                  (and (indent-kind map node :dedent) (/= srow row))))
                     (decf indent width)
                     (setf is-processed t))
                   (let* ((should-process (not (gethash srow processed)))
                          (parent (node-parent node))
                          (in-error (and should-process parent (node-has-error-p parent))))
                     (when (and should-process begin
                                (or (/= srow erow) in-error (setting begin "indent.immediate"))
                                (or (/= srow row) (setting begin "indent.start_at_same_line")))
                       (incf indent width)
                       (setf is-processed t))
                     ;; In an error, a child's aligned indent is the node's.
                     (when (and in-error (not align))
                       (dolist (child (node-children node))
                         (let ((child-align (indent-kind map child :align)))
                           (when child-align (setf align child-align) (return)))))
                     (when (and should-process align (or (/= srow erow) in-error) (/= srow row))
                       (multiple-value-bind (open open-last)
                           (if (setting align "indent.open_delimiter")
                               (find-delimiter parse node (setting align "indent.open_delimiter"))
                               node)
                         (multiple-value-bind (close close-last)
                             (if (setting align "indent.close_delimiter")
                                 (find-delimiter parse node (setting align "indent.close_delimiter"))
                                 node)
                           (when open
                             (multiple-value-bind (orow ocol) (node-start open)
                               (let ((crow (and close (node-start close)))
                                     (absolute nil))
                                 (if open-last
                                     ;; The delimiter ended its line: a hanging indent.
                                     (progn
                                       (incf indent width)
                                       (when (and close-last crow (< crow row))
                                         (setf indent (max (- indent width) 0))))
                                     (if (and close-last crow (/= orow crow) (< crow row))
                                         (setf indent (max (- indent width) 0))
                                         (let ((text (heml-interface:line-string (svref lines orow))))
                                           (setf indent (+ (line-char-index text ocol)
                                                           (let ((increment (setting align "indent.increment")))
                                                             (if (stringp increment)
                                                                 (or (parse-integer increment :junk-allowed t) 1)
                                                                 1)))
                                                 absolute t))))
                                 (when (and crow (/= crow orow) (= crow row)
                                            (setting align "indent.avoid_last_matching_next")
                                            (<= indent (+ (leading-columns
                                                           (heml-interface:line-string (svref lines orow)))
                                                          width)))
                                   (incf indent width))
                                 (setf is-processed t)
                                 (when absolute
                                   (return-from indent-column indent))))))))
                     (setf (gethash srow processed) (or (gethash srow processed) is-processed))
                     (setf node parent))))))
    (max indent 0)))

;;; Code being typed is unfinished, and parses as errors: the tree cannot
;;; say how to indent within an ERROR.  There, and on a blank line after a
;;; line that finishes its block (Python's return), the line before says: a
;;; level more if it OPENS a block, a level less if it FINISHES one, and a
;;; level less again if this line CLOSES one.
;;;
(defun in-error-p (node)
  (loop for n = node then (node-parent n)
        while n
        thereis (string= (node-type n) "ERROR")))

(defun bracket-balance (string)
  "Parentheses and square brackets STRING opens less those it closes: a
statement continued over lines.  Braces open blocks, and do not count."
  (- (count-if (lambda (c) (find c "([")) string)
     (count-if (lambda (c) (find c ")]")) string)))

(defun statement-start (lines row)
  "The row where the line ROW's statement began: a line that closes brackets
opened on lines before belongs to the line that opened them."
  (let ((balance (bracket-balance (heml-interface:line-string (svref lines row)))))
    (loop while (and (minusp balance) (plusp row))
          do (decf row)
             (incf balance (bracket-balance (heml-interface:line-string (svref lines row)))))
    row))

(defun textual-indent (language lines row)
  (let* ((previous (loop for r from (1- row) downto 0
                         unless (blank-string-p (heml-interface:line-string (svref lines r)))
                           return r))
         (text (and previous (heml-interface:line-string (svref lines previous))))
         (width (language-indent-width language))
         ;; From the line the previous line's statement began on.
         (indent (if text
                     (leading-columns (heml-interface:line-string
                                       (svref lines (statement-start lines previous))))
                     0)))
    (when text
      (cond ((and (language-opens language) (ppcre:scan (language-opens language) text))
             (incf indent width))
            ((and (language-finishes language) (ppcre:scan (language-finishes language) text))
             (decf indent width))))
    (when (and (language-closes language)
               (ppcre:scan (language-closes language)
                           (heml-interface:line-string (svref lines row))))
      (decf indent width))
    (max indent 0)))

(defun textual-indent-p (language parse line)
  "True when LINE is better indented from the line before than from the tree."
  (when (language-opens language)
    (let* ((lines (parse-lines parse))
           (row (position line lines))
           (string (heml-interface:line-string line))
           (root (node-copy (parse-root parse)))
           (probe (if (blank-string-p string)
                      (let ((previous (loop for r from (1- row) downto 0
                                            unless (blank-string-p
                                                    (heml-interface:line-string (svref lines r)))
                                              return r)))
                        (when previous
                          (let ((text (heml-interface:line-string (svref lines previous))))
                            (when (and (language-finishes language)
                                       (ppcre:scan (language-finishes language) text))
                              (return-from textual-indent-p t))
                            (descendant-at root previous
                                           (string-byte text (position-if-not
                                                              (lambda (c) (member c '(#\Space #\Tab)))
                                                              text :from-end t))))))
                      (descendant-at root row
                                     (string-byte string (position-if-not
                                                          (lambda (c) (member c '(#\Space #\Tab)))
                                                          string))))))
      (and probe (in-error-p probe)))))

(defun tree-sitter-indent-line (language mark)
  "Indent the line MARK is on as LANGUAGE's indents.scm says, or as the line
before is when there is no query to say."
  (let* ((line (heml-interface:mark-line mark))
         (buffer (heml-interface:line-buffer line)))
    (if (and buffer (language-ready-p language) (language-indent-ready-p language))
        (let* ((parse (buffer-parse language buffer))
               (column (if (textual-indent-p language parse line)
                           (textual-indent language (parse-lines parse)
                                           (position line (parse-lines parse)))
                           (indent-column language parse line))))
          (when (eql column -1)
            (setf column (let ((previous (loop for l = (heml-interface:line-previous line)
                                                 then (heml-interface:line-previous l)
                                               while l
                                               unless (blank-string-p (heml-interface:line-string l))
                                                 return l)))
                           (if previous (leading-columns (heml-interface:line-string previous)) 0))))
          (heml-interface:line-start mark)
          (heml::delete-horizontal-space mark)
          (heml::indent-to-column mark column)
          ;; Point, in the old indentation, goes to the new.
          (let ((point (heml-interface:current-point)))
            (when (and (eq (heml-interface:mark-line point) line)
                       (< (heml-interface:mark-column point) column))
              (heml-interface:move-to-column point column))))
        (heml::generic-indent mark))))
