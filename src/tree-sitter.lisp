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
  problem)             ; why it could not be loaded, when it could not

(defvar *languages* (make-hash-table :test 'equal))

(defun define-tree-sitter-language (name &key mode (precedence :first) fallback)
  "Highlight buffers whose major mode is MODE with tree-sitter's grammar NAME
and its highlight query.  PRECEDENCE says which pattern wins when two capture
the same text: :FIRST, tree-sitter's own rule, followed by the queries that
come with grammars, or :LAST, Neovim's, followed by its queries.  FALLBACK,
when given, is MODE's highlighter instead, from the first time it is needed,
if the grammar or its query cannot be loaded."
  (let ((language (%make-language :name name :precedence precedence)))
    (setf (gethash name *languages*) language)
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
  line-starts)         ; line to the byte its text starts at

(defvar *parses* (make-hash-table :test 'eq :weakness :key)
  "Buffer to its latest parse.")

(defvar *parser* nil)

(defun buffer-octets (buffer)
  "BUFFER's text as UTF-8, and a table of where each of its lines starts."
  (let ((starts (make-hash-table :test 'eq))
        (chunks '())
        (offset 0))
    (do ((line (heml-interface:mark-line (heml-interface:buffer-start-mark buffer))
               (heml-interface:line-next line)))
        ((null line))
      (let ((octets (babel:string-to-octets (heml-interface:line-string line)
                                            :encoding :utf-8)))
        (setf (gethash line starts) offset)
        (push octets chunks)
        (incf offset (1+ (length octets)))))
    (let ((all (make-array offset :element-type '(unsigned-byte 8)
                                  :initial-element 10))
          (position 0))
      (dolist (octets (nreverse chunks))
        (replace all octets :start1 position)
        (incf position (1+ (length octets))))
      (values all starts))))

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
          (multiple-value-bind (octets starts) (buffer-octets buffer)
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
                                 :octets octets :line-starts starts))))))))


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

(defun predicates-hold-p (language parse pattern captures count)
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
                          ;; #set!, #offset! and the like say nothing about
                          ;; whether the pattern applies.
                          (t t))))
                 (if negated (not holds) holds)))))
         (aref (language-predicates language) pattern)))

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
