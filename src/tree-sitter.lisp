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
;;;; A buffer is parsed when its signature changes: the old tree is told what
;;;; span of the text changed, found by comparing the old text with the new,
;;;; and tree-sitter reuses what it can of it.  A line is coloured when
;;;; redisplay draws it, from the captures of the query within the line's
;;;; bytes.  Redisplay draws only what is visible, so only visible lines are
;;;; coloured.
;;;;
;;;; Much of tree-sitter's interface passes a node, a 32-byte struct, by
;;;; value, which CFFI cannot do without libffi.  TS calls a function either
;;;; way: through sb-alien on SBCL, and through a C function pointer in
;;;; inline C on ECL, which compiles through C.  Memory is CFFI's.

(defpackage :heml.tree-sitter
  (:use :common-lisp)
  (:export #:define-tree-sitter-language
           #:*tree-sitter-directories*
           #:tree-sitter-available-p
           #:buffer-language
           #:definition-spans))

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
                 (namestring (merge-pathnames "tree-sitter/" (hi::heml-data-directory)))
                 "/opt/homebrew/"
                 "/usr/local/")))
  "Where tree-sitter things are looked for, each laid out as Homebrew lays
them out: lib/libtree-sitter.dylib, lib/libtree-sitter-<language>.dylib and
share/tree-sitter/queries/<language>/highlights.scm.  `make tree-sitter'
builds Heml's grammars into build/tree-sitter/, and `make install-tree-sitter'
copies them to $XDG_DATA_HOME/heml/tree-sitter/ (~/.local/share/heml/).")

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
                     (ignore-errors (load-library library) t))
                :loaded
                :missing))))
  (eq *library-state* :loaded))


;;;; Calling it

(defconstant +node-size+ 32)
(defconstant +point-size+ 8)
(defconstant +capture-size+ 40)         ; a node, a uint32, and padding
(defconstant +match-size+ 16)           ; id, pattern, count, captures

#+sbcl
(progn
  (sb-alien:define-alien-type nil
      (sb-alien:struct ts-node
        (context (sb-alien:array (sb-alien:unsigned 32) 4))
        (id sb-sys:system-area-pointer)
        (tree sb-sys:system-area-pointer)))
  (sb-alien:define-alien-type nil
      (sb-alien:struct ts-point
        (row (sb-alien:unsigned 32))
        (column (sb-alien:unsigned 32)))))

(defvar *addresses* (make-hash-table :test 'equal))

(defun address (name)
  (or (gethash name *addresses*)
      (setf (gethash name *addresses*)
            (let ((pointer (cffi:foreign-symbol-pointer name)))
              (if (and pointer (not (cffi:null-pointer-p pointer)))
                  pointer
                  (error "No ~A in the loaded tree-sitter libraries." name))))))

(defun load-library (pathname)
  #+sbcl (sb-alien:load-shared-object pathname :dont-save t)
  #-sbcl (cffi:load-foreign-library pathname))

;;; (TS NAME RESULT (TYPE ARGUMENT)*) calls the C function NAME.  A TYPE is
;;; :POINTER, :UINT32, or :NODE or :POINT, whose ARGUMENT is a pointer to the
;;; struct, passed by value.  RESULT is :VOID, :POINTER, :UINT32, :BOOL,
;;; :STRING, or (:NODE POINTER) or (:POINT POINTER), a struct returned into
;;; the memory at POINTER.
;;;
#+sbcl
(defmacro ts (name result &rest arguments)
  (labels ((alien-type (type)
             (ecase type
               (:pointer 'sb-sys:system-area-pointer)
               (:uint32 '(sb-alien:unsigned 32))
               (:node '(sb-alien:struct ts-node))
               (:point '(sb-alien:struct ts-point))))
           (deref (type pointer)
             `(sb-alien:deref (sb-alien:sap-alien ,pointer (* ,(alien-type type))))))
    (let* ((kind (if (consp result) (first result) result))
           (call `(sb-alien:alien-funcall
                   (sb-alien:sap-alien (address ,name)
                                       (function ,(ecase kind
                                                    (:void 'sb-alien:void)
                                                    (:pointer 'sb-sys:system-area-pointer)
                                                    (:uint32 '(sb-alien:unsigned 32))
                                                    (:bool '(sb-alien:unsigned 8))
                                                    (:string 'sb-alien:c-string)
                                                    ((:node :point) (alien-type kind)))
                                                 ,@(mapcar (lambda (a) (alien-type (first a)))
                                                           arguments)))
                   ,@(mapcar (lambda (a)
                               (destructuring-bind (type value) a
                                 (if (member type '(:node :point)) (deref type value) value)))
                             arguments))))
      (case kind
        (:bool `(plusp ,call))
        ((:node :point) `(setf ,(deref kind (second result)) ,call))
        (t call)))))

#+ecl
(defmacro ts (name result &rest arguments)
  (let* ((kind (if (consp result) (first result) result))
         (out (and (consp result) (second result)))
         (c-types (mapcar (lambda (a) (ecase (first a)
                                        (:pointer "void*") (:uint32 "unsigned int")
                                        (:node "TSNode") (:point "TSPoint")))
                          arguments))
         (c-result (ecase kind
                     (:void "void") ((:pointer :string) "void*") (:uint32 "unsigned int")
                     (:bool "unsigned char") (:node "TSNode") (:point "TSPoint")))
         ;; #0 is the function, #1 the result's memory if any, then the arguments.
         (first-argument (if out 2 1))
         (call (format nil "((~A (*)(~{~A~^, ~}))#0)(~{~A~^, ~})"
                       c-result c-types
                       (loop for a in arguments
                             for i from first-argument
                             collect (let ((code (string-downcase (format nil "#~36R" i))))
                                       (case (first a)
                                         (:node (format nil "*(TSNode*)~A" code))
                                         (:point (format nil "*(TSPoint*)~A" code))
                                         (t code))))))
         (code (format nil "{ typedef struct { unsigned int context[4]; const void *id; const void *tree; } TSNode; typedef struct { unsigned int row; unsigned int column; } TSPoint; ~A }"
                       (case kind
                         (:void (format nil "~A;" call))
                         ((:node :point) (format nil "*(~A*)#1 = ~A;" c-result call))
                         (t (format nil "@(return) = ~A;" call)))))
         (form `(ffi:c-inline ((address ,name) ,@(and out (list out)) ,@(mapcar #'second arguments))
                              (:pointer-void ,@(and out '(:pointer-void))
                                             ,@(mapcar (lambda (a) (if (eq (first a) :uint32)
                                                                       :unsigned-int
                                                                       :pointer-void))
                                                       arguments))
                              ,(ecase kind
                                 ((:void :node :point) :void)
                                 ((:pointer :string) :pointer-void)
                                 (:uint32 :unsigned-int)
                                 ;; An unsigned char would come back a character.
                                 (:bool :int))
                              ,code
                              :one-liner nil :side-effects t)))
    (case kind
      (:bool `(plusp ,form))
      (:string `(let ((p ,form)) (if (cffi:null-pointer-p p) nil (cffi:foreign-string-to-lisp p))))
      ((:node :point) `(progn ,form nil))
      (t form))))

;;; Foreign memory, which the calls take and fill.

(defmacro u16 (pointer offset) `(cffi:mem-ref ,pointer :uint16 ,offset))
(defmacro u32 (pointer offset) `(cffi:mem-ref ,pointer :uint32 ,offset))
(defmacro pointer-at (pointer offset) `(cffi:mem-ref ,pointer :pointer ,offset))
(defmacro ptr+ (pointer offset) `(cffi:inc-pointer ,pointer ,offset))

(defmacro with-foreign-memory ((pointer size) &body body)
  `(cffi:with-foreign-pointer (,pointer ,size) ,@body))

(defun allocate (size) (cffi:foreign-alloc :uint8 :count size))
(defun deallocate (pointer) (cffi:foreign-free pointer))

(defmacro with-vector-pointer ((pointer vector) &body body)
  "POINTER to the bytes of VECTOR, an (unsigned-byte 8) vector, while BODY runs."
  `(cffi:with-pointer-to-vector-data (,pointer ,vector) ,@body))

(defun foreign-string (pointer length)
  (let ((octets (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (i length)
      (setf (aref octets i) (cffi:mem-aref pointer :uint8 i)))
    (babel:octets-to-string octets :encoding :utf-8)))


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
  finishes             ; a regex: a line starting so ends its block
  single               ; a regex: a line ending so governs just one statement
  opens-always         ; true when a new line after one that OPENS is always indented
  inline               ; the language of what its (inline) nodes hold, or NIL
  inline-node-query    ; the query finding those nodes
  code-blocks          ; true when its fenced code blocks are coloured by their languages
  code-block-query     ; the query finding those blocks, with their languages' names
  mode                 ; the major mode it colours
  definitions)         ; node types "Beginning of Definition" moves among

(defvar *languages* (make-hash-table :test 'equal))

(defun define-tree-sitter-language (name &key mode (precedence :first) fallback
                                                indent (indent-width 4) opens closes finishes
                                                single opens-always inline code-blocks
                                                definitions)
  "Highlight buffers whose major mode is MODE with tree-sitter's grammar NAME
and its highlight query.  PRECEDENCE says which pattern wins when two capture
the same text: :FIRST, tree-sitter's own rule, followed by the queries that
come with grammars, or :LAST, Neovim's, followed by its queries.  FALLBACK,
when given, is MODE's highlighter instead, from the first time it is needed,
if the grammar or its query cannot be loaded.  OPENS-ALWAYS says a new
line after one that OPENS a block is indented whatever the tree says.
INDENT makes MODE's lines
indented as the language's indents.scm says, INDENT-WIDTH columns a level,
with spaces.  DEFINITIONS are the types of the nodes -- functions, classes
-- that \"Beginning of Definition\" and its fellows move among."
  (let ((language (%make-language :name name :precedence precedence
                                  :mode mode :definitions definitions
                                  :indent-width indent-width
                                  :opens (and opens (ppcre:create-scanner opens))
                                  :closes (and closes (ppcre:create-scanner closes))
                                  :finishes (and finishes (ppcre:create-scanner finishes))
                                  :single (and single (ppcre:create-scanner single))
                                  :opens-always opens-always
                                  :code-blocks code-blocks
                                  :inline (and inline (%make-language :name inline :precedence :first)))))
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
    (load-library grammar)
    (let ((pointer (ts (format nil "tree_sitter_~A" (substitute #\_ #\- name)) :pointer)))
      (setf (language-pointer language) pointer
            (language-query language) (make-query pointer (uiop:read-file-string query-file)))
      (read-captures language)
      (read-predicates language))))

(defun make-query (language source)
  (let ((octets (babel:string-to-octets source :encoding :utf-8)))
    (with-foreign-memory (out 8)
      (let ((query (with-vector-pointer (text octets)
                     (ts "ts_query_new" :pointer
                         (:pointer language) (:pointer text) (:uint32 (length octets))
                         (:pointer out) (:pointer (ptr+ out 4))))))
        (when (cffi:null-pointer-p query)
          (error "its highlight query does not suit the grammar (error ~D at byte ~D)"
                 (u32 out 4) (u32 out 0)))
        query))))

(defun query-string (function query id)
  (with-foreign-memory (length 4)
    (let ((pointer (ts function :pointer (:pointer query) (:uint32 id) (:pointer length))))
      (foreign-string pointer (u32 length 0)))))

(defun read-captures (language)
  (let* ((query (language-query language))
         (count (ts "ts_query_capture_count" :uint32 (:pointer query)))
         (fonts (make-array count)))
    (dotimes (i count)
      (setf (aref fonts i)
            (capture-font (query-string "ts_query_capture_name_for_id" query i))))
    (setf (language-fonts language) fonts)))

;;; A pattern's predicates are (NAME ARGUMENT*), each argument a capture's
;;; index, as (:CAPTURE . INDEX), or a string.
;;;
(defun read-predicates (language)
  (setf (language-predicates language) (query-predicates (language-query language))))


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
    ("text.emphasis" . (:italic t)) ("markup.italic" . (:italic t))
    ("text.strong" . (:bold t)) ("markup.strong" . (:bold t))
    ("text.uri" . (:fg 6 :underline t)) ("markup.link.url" . (:fg 6 :underline t))
    ("text.reference" . 6) ("markup.link" . 6)
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
  language             ; the language it was parsed with
  tree                 ; the TSTree
  root                 ; foreign memory holding the root node
  octets               ; the buffer as UTF-8, which the tree indexes
  line-starts          ; line to its row
  row-starts           ; row to the byte its text starts at, and the length at the end
  lines                ; row to line
  strings              ; row to the line's characters as they were parsed
  (indents :unmade)    ; node id to its indentation captures, made when needed
  inline-tree          ; the inline language's tree, over the (inline) nodes
  inline-root          ; foreign memory holding its root node, or NIL
  injections)          ; ((LANGUAGE TREE ROOT RANGES) ...): its code blocks, by language

(defvar *parses* (make-hash-table :test 'eq :weakness :key)
  "Buffer to its latest parse.")

(defvar *parser* nil)

;;; The octets are made again each time a buffer has changed, but not from
;;; the start: a line whose characters are the same string as when it was
;;; last parsed -- they are a new string whenever they change (the open
;;; line's a number that changes) -- is the same, and what the lines that
;;; are the same at each end were is kept.  Only the lines between are
;;; encoded again, and the edit that tree-sitter is told of is theirs.
;;; Encoding every line and comparing the whole of the old octets with the
;;; new, as was done, took a fifth of a second for a file of ten megabytes,
;;; each time it changed.

(defun line-chars-of (line)
  (heml-internals::line-chars line))

(defun line-octets (line)
  (babel:string-to-octets (heml-interface:line-string line) :encoding :utf-8))

(defun buffer-octets (buffer)
  "BUFFER's text as UTF-8, a table of each line's row, each row's start in
   the text and its length at the end, the rows' lines and their
   characters."
  (let ((rows (make-hash-table :test 'eq))
        (chunks '())
        (lines '())
        (offset 0))
    (do ((line (heml-interface:mark-line (heml-interface:buffer-start-mark buffer))
               (heml-interface:line-next line)))
        ((null line))
      (let ((octets (line-octets line)))
        (push line lines)
        (push octets chunks)
        (incf offset (1+ (length octets)))))
    (let* ((count (length lines))
           (all (make-array offset :element-type '(unsigned-byte 8) :initial-element 10))
           (starts (make-array (1+ count)))
           (lines (coerce (nreverse lines) 'simple-vector))
           (strings (make-array count))
           (position 0))
      (loop for octets in (nreverse chunks)
            for row from 0
            do (setf (svref starts row) position
                     (gethash (svref lines row) rows) row
                     (svref strings row) (line-chars-of (svref lines row)))
               (replace all octets :start1 position)
               (incf position (1+ (length octets))))
      (setf (svref starts count) position)
      (values all rows starts lines strings))))

(defun changed-octets (old buffer)
  "As BUFFER-OCTETS, from OLD, the parse of the buffer before it changed;
   and, as a sixth value, the edit -- (START OLD-END NEW-END START-ROW
   OLD-END-ROW NEW-END-ROW), in bytes and rows -- or NIL when no line
   changed."
  (let* ((old-lines (parse-lines old))
         (old-strings (parse-strings old))
         (old-starts (parse-row-starts old))
         (old-octets (parse-octets old))
         (rows (parse-line-starts old))
         (n (length old-lines))
         (last (heml-interface:mark-line (heml-interface:buffer-end-mark buffer)))
         (p 0)
         (line (heml-interface:mark-line (heml-interface:buffer-start-mark buffer)))
         (before nil))
    (flet ((same (line row)
             (and (eq line (svref old-lines row))
                  (eq (line-chars-of line) (svref old-strings row)))))
      (loop while (and line (< p n) (same line p))
            do (setf before line
                     line (heml-interface:line-next line))
               (incf p))
      (let ((q 0) (back last))
        (loop while (and back line (not (eq back before)) (< (+ p q) n)
                         (same back (- n 1 q)))
              do (incf q)
                 (setf back (heml-interface:line-previous back)))
        (let* ((middle (when (and line (not (eq back before)))
                         (loop for x = line then (heml-interface:line-next x)
                               collect x
                               until (or (eq x back) (null (heml-interface:line-next x))))))
               (k (length middle))
               (j (- n p q)))
          (if (and (zerop j) (zerop k))
              (values old-octets rows old-starts old-lines old-strings nil)
              (let* ((chunks (mapcar #'line-octets middle))
                     (middle-length (reduce #'+ chunks :key (lambda (c) (1+ (length c)))))
                     (start (svref old-starts p))
                     (old-end (svref old-starts (- n q)))
                     (new-end (+ start middle-length))
                     (delta (- new-end old-end))
                     (m (+ p k q))
                     (octets (make-array (+ (length old-octets) delta)
                                         :element-type '(unsigned-byte 8) :initial-element 10))
                     (starts (make-array (1+ m)))
                     (lines (make-array m))
                     (strings (make-array m)))
                (replace octets old-octets :end2 start)
                (replace octets old-octets :start1 new-end :start2 old-end)
                (replace starts old-starts :end2 p)
                (replace lines old-lines :end2 p)
                (replace strings old-strings :end2 p)
                ;; The lines that are gone are no row's.
                (loop for row from p below (- n q)
                      do (remhash (svref old-lines row) rows))
                (let ((position start))
                  (loop for x in middle
                        for chunk in chunks
                        for row from p
                        do (setf (svref starts row) position
                                 (svref lines row) x
                                 (svref strings row) (line-chars-of x)
                                 (gethash x rows) row)
                           (replace octets chunk :start1 position)
                           (incf position (1+ (length chunk)))))
                ;; The lines after are where they were, moved by what changed.
                (loop for old-row from (- n q) below n
                      for row from (+ p k)
                      do (setf (svref starts row) (+ (svref old-starts old-row) delta)
                               (svref lines row) (svref old-lines old-row)
                               (svref strings row) (svref old-strings old-row))
                         (unless (= row old-row)
                           (setf (gethash (svref lines row) rows) row)))
                (setf (svref starts m) (length octets))
                (values octets rows starts lines strings
                        (list start old-end new-end p (- n q) (+ p k))))))))))

(defun free-inline (parse)
  (when (parse-inline-tree parse)
    (ts "ts_tree_delete" :void (:pointer (parse-inline-tree parse)))
    (deallocate (parse-inline-root parse))
    (setf (parse-inline-tree parse) nil (parse-inline-root parse) nil)))

(defun free-parse (parse)
  (ts "ts_tree_delete" :void (:pointer (parse-tree parse)))
  (deallocate (parse-root parse))
  (free-inline parse)
  (loop for (nil tree root) in (parse-injections parse)
        do (ts "ts_tree_delete" :void (:pointer tree))
           (deallocate root))
  (setf (parse-injections parse) '()))

;;; Markdown is two grammars: the block grammar finds headings, lists and
;;; paragraphs, whose text it leaves in (inline) nodes, and the inline
;;; grammar parses that text for emphasis, code and links.  The inline
;;; grammar parses only those nodes' ranges (ts_parser_set_included_ranges),
;;; of the same text, so its tree indexes the buffer as the block tree does.

(defvar *inline-parser* nil)

(defconstant +range-size+ 24)           ; two points and two byte offsets

(defun parse-inline (language octets root)
  "The inline language's tree over the (inline) nodes of the tree whose root
is at ROOT, and foreign memory holding its root; or NIL."
  (let ((inline (language-inline language)))
    (when (and inline (language-ready-p inline))
      (unless (language-inline-node-query language)
        (setf (language-inline-node-query language)
              (make-query (language-pointer language) "(inline) @inline")))
      (let ((ranges '())
            (cursor (ts "ts_query_cursor_new" :pointer)))
        (unwind-protect
             (progn
               (ts "ts_query_cursor_exec" :void
                   (:pointer cursor)
                   (:pointer (language-inline-node-query language))
                   (:node root))
               (with-foreign-memory (match (+ +match-size+ 4))
                 (let ((capture-index (ptr+ match +match-size+)))
                   (loop while (ts "ts_query_cursor_next_capture" :bool
                                   (:pointer cursor) (:pointer match) (:pointer capture-index))
                         do (let* ((capture (ptr+ (pointer-at match 8)
                                                  (* (u32 capture-index 0) +capture-size+)))
                                   (node (node-copy capture)))
                              (multiple-value-bind (start end) (node-bytes capture)
                                (multiple-value-bind (srow scol) (node-start node)
                                  (multiple-value-bind (erow ecol) (node-end node)
                                    (push (list srow scol erow ecol start end) ranges)))))))))
          (ts "ts_query_cursor_delete" :void (:pointer cursor)))
        (when ranges
          (parse-ranges inline octets (nreverse ranges)))))))

(defun parse-ranges (language octets ranges)
  "LANGUAGE's tree over RANGES of the text OCTETS, each (START-ROW
START-COLUMN END-ROW END-COLUMN START-BYTE END-BYTE), and foreign memory
holding its root."
  (unless *inline-parser*
    (setf *inline-parser* (ts "ts_parser_new" :pointer)))
  (ts "ts_parser_set_language" :bool
      (:pointer *inline-parser*) (:pointer (language-pointer language)))
  (with-foreign-memory (memory (* +range-size+ (length ranges)))
    (loop for (srow scol erow ecol start end) in ranges
          for offset from 0 by +range-size+
          do (setf (u32 memory offset) srow
                   (u32 memory (+ offset 4)) scol
                   (u32 memory (+ offset 8)) erow
                   (u32 memory (+ offset 12)) ecol
                   (u32 memory (+ offset 16)) start
                   (u32 memory (+ offset 20)) end))
    (ts "ts_parser_set_included_ranges" :bool
        (:pointer *inline-parser*) (:pointer memory) (:uint32 (length ranges)))
    (let ((tree (with-vector-pointer (text octets)
                  (ts "ts_parser_parse_string" :pointer
                      (:pointer *inline-parser*) (:pointer (cffi:null-pointer))
                      (:pointer text) (:uint32 (length octets)))))
          (root (allocate +node-size+)))
      (ts "ts_tree_root_node" (:node root) (:pointer tree))
      (values tree root))))

;;; A fenced code block that names its language is coloured as that
;;; language: its text is parsed again with the language's grammar, all the
;;; blocks of one language together, as the inline grammar parses the
;;; (inline) nodes.

(defparameter *code-block-languages*
  '(("c" . "c") ("h" . "c")
    ("python" . "python") ("py" . "python") ("python3" . "python")
    ("bash" . "bash") ("sh" . "bash") ("shell" . "bash") ("zsh" . "bash") ("console" . "bash")
    ("lisp" . "commonlisp") ("commonlisp" . "commonlisp") ("common-lisp" . "commonlisp")
    ("cl" . "commonlisp") ("elisp" . "commonlisp") ("emacs-lisp" . "commonlisp")
    ("pascal" . "pascal") ("delphi" . "pascal") ("objectpascal" . "pascal")
    ("rust" . "rust") ("rs" . "rust")
    ("go" . "go") ("golang" . "go")
    ("javascript" . "javascript") ("js" . "javascript") ("jsx" . "javascript")
    ("node" . "javascript")
    ("typescript" . "typescript") ("ts" . "typescript") ("tsx" . "tsx")
    ("json" . "json") ("jsonc" . "json")
    ("yaml" . "yaml") ("yml" . "yaml"))
  "What a code block may call its language, and the grammar that is.")

(defun code-block-language (name)
  "The language a code block calls NAME, when its grammar is installed."
  (let* ((grammar (cdr (assoc (string-downcase name) *code-block-languages* :test #'string=)))
         (language (and grammar (gethash grammar *languages*))))
    (and language (language-ready-p language) language)))

(defun capture-range (capture)
  "The node at CAPTURE as a range: (START-ROW START-COLUMN END-ROW END-COLUMN
START-BYTE END-BYTE)."
  (let ((node (node-copy capture)))
    (multiple-value-bind (start end) (node-bytes capture)
      (multiple-value-bind (start-row start-column) (node-start node)
        (multiple-value-bind (end-row end-column) (node-end node)
          (list start-row start-column end-row end-column start end))))))

(defun match-capture (captures count index)
  "The capture numbered INDEX among a match's COUNT CAPTURES, or NIL."
  (loop for k below count
        for capture = (ptr+ captures (* k +capture-size+))
        when (= index (u32 capture +node-size+))
          return capture))

(defun code-block-ranges (language octets root)
  "The fenced code blocks in the tree whose root is at ROOT that name a
language Heml has, as ((LANGUAGE RANGE ...) ...), the ranges in order."
  (let ((blocks '())
        (cursor (ts "ts_query_cursor_new" :pointer)))
    (unwind-protect
         (progn
           (ts "ts_query_cursor_exec" :void
               (:pointer cursor) (:pointer (language-code-block-query language)) (:node root))
           (with-foreign-memory (match (+ +match-size+ 4))
             (let ((capture-index (ptr+ match +match-size+)))
               (loop while (ts "ts_query_cursor_next_capture" :bool
                               (:pointer cursor) (:pointer match) (:pointer capture-index))
                     do (let* ((count (u16 match 6))
                               (captures (pointer-at match 8))
                               (capture (ptr+ captures (* (u32 capture-index 0) +capture-size+)))
                               ;; @language is the query's first capture, @content its second.
                               (name (match-capture captures count 0)))
                          ;; At the block's text, the match holds its language's name too.
                          (when (and name (= 1 (u32 capture +node-size+)))
                            (let* ((range (capture-range capture))
                                   (block-language
                                     (multiple-value-bind (start end) (node-bytes name)
                                       (code-block-language
                                        (babel:octets-to-string octets :start start :end end
                                                                       :encoding :utf-8
                                                                       :errorp nil)))))
                              (when (and block-language (< (fifth range) (sixth range)))
                                (let ((entry (assoc block-language blocks)))
                                  (unless entry
                                    (setf entry (list block-language))
                                    (push entry blocks))
                                  (push range (cdr entry)))))))))))
      (ts "ts_query_cursor_delete" :void (:pointer cursor)))
    (loop for (block-language . ranges) in blocks
          collect (cons block-language (reverse ranges)))))

(defun parse-code-blocks (language octets root)
  "The trees of the fenced code blocks in the tree whose root is at ROOT,
one for each language named, as ((LANGUAGE TREE ROOT RANGES) ...), RANGES the
blocks' bytes as (START . END)."
  (when (language-code-blocks language)
    (unless (language-code-block-query language)
      (setf (language-code-block-query language)
            (make-query (language-pointer language)
                        "(fenced_code_block (info_string (language) @language) (code_fence_content) @content)")))
    (loop for (block-language . ranges) in (code-block-ranges language octets root)
          collect (multiple-value-bind (tree block-root)
                      (parse-ranges block-language octets ranges)
                    (list block-language tree block-root
                          (loop for range in ranges
                                collect (cons (fifth range) (sixth range))))))))

;;; The span of text an edit changed: where the old and new text first
;;; differ, and where each ends before what they share at the end.  A
;;; point is a row and a byte in the row.
;;;
(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(defun text-point (octets offset)
  (declare (type octets octets) (fixnum offset) (optimize speed))
  (let ((rows 0) (newline -1))
    (declare (fixnum rows newline))
    (dotimes (i offset)
      (when (= (aref octets i) 10)
        (incf rows)
        (setf newline i)))
    (values rows (- offset newline 1))))

(defun edit-tree (tree old new)
  "Tell TREE, parsed from the text OLD, that it is now NEW."
  (declare (type octets old new))
  (let* ((old-length (length old))
         (new-length (length new))
         (shorter (min old-length new-length))
         (start (let ((i 0))
                  (declare (fixnum i) (optimize speed))
                  (loop while (and (< i shorter) (= (aref old i) (aref new i)))
                        do (incf i))
                  i))
         (common (- shorter start))
         (suffix (let ((k 0))
                   (declare (fixnum k) (optimize speed))
                   (loop while (and (< k common)
                                    (= (aref old (- old-length k 1))
                                       (aref new (- new-length k 1))))
                         do (incf k))
                   k))
         (old-end (- old-length suffix))
         (new-end (- new-length suffix)))
    (with-foreign-memory (edit 36)
      (setf (u32 edit 0) start (u32 edit 4) old-end (u32 edit 8) new-end)
      (loop for (octets offset at) in (list (list new start 12)
                                            (list old old-end 20)
                                            (list new new-end 28))
            do (multiple-value-bind (row column) (text-point octets offset)
                 (setf (u32 edit at) row (u32 edit (+ at 4)) column)))
      (ts "ts_tree_edit" :void (:pointer tree) (:pointer edit)))))

(defun buffer-parse (language buffer)
  "BUFFER parsed with LANGUAGE, parsing it again if it has changed, from the
last parse when it was parsed with LANGUAGE too."
  (let ((parse (gethash buffer *parses*))
        (signature (heml-interface:buffer-signature buffer)))
    (if (and parse (eql (parse-signature parse) signature)
             (eq (parse-language parse) language))
        parse
        (progn
          (unless *parser*
            (setf *parser* (ts "ts_parser_new" :pointer)))
          (ts "ts_parser_set_language" :bool
              (:pointer *parser*) (:pointer (language-pointer language)))
          (multiple-value-bind (octets starts row-starts lines strings edit)
              (if (and parse (eq (parse-language parse) language) (parse-strings parse))
                  (changed-octets parse buffer)
                  (buffer-octets buffer))
            (let* ((old (and parse (eq (parse-language parse) language) parse))
                   (tree (progn
                           (when (and old edit)
                             (destructuring-bind (start old-end new-end
                                                  start-row old-row new-row)
                                 edit
                               (with-foreign-memory (change 36)
                                 (setf (u32 change 0) start (u32 change 4) old-end
                                       (u32 change 8) new-end
                                       (u32 change 12) start-row (u32 change 16) 0
                                       (u32 change 20) old-row (u32 change 24) 0
                                       (u32 change 28) new-row (u32 change 32) 0)
                                 (ts "ts_tree_edit" :void (:pointer (parse-tree old))
                                     (:pointer change)))))
                           (with-vector-pointer (text octets)
                             (ts "ts_parser_parse_string" :pointer
                                 (:pointer *parser*)
                                 (:pointer (if old (parse-tree old) (cffi:null-pointer)))
                                 (:pointer text) (:uint32 (length octets))))))
                   (root (allocate +node-size+)))
              (when parse (free-parse parse))
              (ts "ts_tree_root_node" (:node root) (:pointer tree))
              (multiple-value-bind (inline-tree inline-root) (parse-inline language octets root)
                (setf (gethash buffer *parses*)
                      (%make-parse :signature signature :language language
                                   :tree tree :root root
                                   :octets octets :line-starts starts
                                   :row-starts row-starts
                                   :lines lines :strings strings
                                   :inline-tree inline-tree :inline-root inline-root
                                   :injections (parse-code-blocks language octets root))))))))))


;;;; Colouring a line

(defvar *cursor* nil)

(defun node-bytes (sap)
  (values (ts "ts_node_start_byte" :uint32 (:node sap))
          (ts "ts_node_end_byte" :uint32 (:node sap))))

(defun match-texts (parse captures count index)
  "The text of each capture numbered INDEX in a match."
  (loop for k below count
        for capture = (ptr+ captures (* k +capture-size+))
        when (= index (u32 capture +node-size+))
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
        for capture = (ptr+ captures (* k +capture-size+))
        when (= index (u32 capture +node-size+))
          collect (ts "ts_node_type" :string (:node capture))))

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

(defun merge-fonts (outer inner)
  "INNER drawn within OUTER: its colour, and both's other styles."
  (flet ((as-plist (font)
           (cond ((null font) '())
                 ((integerp font) (if (zerop font) '() (list :fg font)))
                 (t font))))
    (if (or (null outer) (integerp inner))
        ;; A colour within replaces the colour without.
        inner
        (let ((merged (copy-list (as-plist outer))))
          (loop for (key value) on (as-plist inner) by #'cddr
                do (setf (getf merged key) value))
          ;; Just a colour is a colour index.
          (if (and (= (length merged) 2) (eq (first merged) :fg))
              (second merged)
              merged)))))

(defun query-spans (language parse root string line-start line-end)
  "LANGUAGE's highlight query's spans within one line, from the tree whose
root is at ROOT, as (FROM TO PATTERN FONT) in characters, sorted to be laid
down in order: wider first, so what is inside shows through, and of two on
the same text, the one whose pattern wins last."
  (let ((ascii (= (- line-end line-start) (length string)))
        (spans '()))
    (unless *cursor*
      (setf *cursor* (ts "ts_query_cursor_new" :pointer)))
    (ts "ts_query_cursor_set_byte_range" :bool
        (:pointer *cursor*) (:uint32 line-start) (:uint32 (1+ line-end)))
    (ts "ts_query_cursor_exec" :void
        (:pointer *cursor*) (:pointer (language-query language)) (:node root))
    (with-foreign-memory (match (+ +match-size+ 4))
      (let ((capture-index (ptr+ match +match-size+)))
        (loop while (ts "ts_query_cursor_next_capture" :bool
                        (:pointer *cursor*) (:pointer match) (:pointer capture-index))
              do (let* ((pattern (u16 match 4))
                        (count (u16 match 6))
                        (captures (pointer-at match 8))
                        (capture (ptr+ captures (* (u32 capture-index 0) +capture-size+)))
                        (font (aref (language-fonts language) (u32 capture +node-size+))))
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
    (sort spans (if (eq (language-precedence language) :last)
                    (lambda (a b)
                      (let ((wa (- (second a) (first a)))
                            (wb (- (second b) (first b))))
                        (or (> wa wb) (and (= wa wb) (< (third a) (third b))))))
                    (lambda (a b)
                      (let ((wa (- (second a) (first a)))
                            (wb (- (second b) (first b))))
                        (or (> wa wb) (and (= wa wb) (> (third a) (third b))))))))))

(defun line-fonts (language parse line)
  "A vector of the font each character of LINE is drawn in, NIL for none:
the language's highlighting, then its inline language's within that, and in
a code block the block's language's instead."
  (let* ((string (heml-interface:line-string line))
         (fonts (make-array (length string) :initial-element nil))
         (line-start (svref (parse-row-starts parse) (gethash line (parse-line-starts parse))))
         (line-end (+ line-start (babel:string-size-in-octets string :encoding :utf-8))))
    (flet ((lay-down (spans)
             (dolist (span spans)
               (destructuring-bind (from to pattern font) span
                 (declare (ignore pattern))
                 (loop for i from from below to
                       do (setf (aref fonts i) (merge-fonts (aref fonts i) font)))))))
      (lay-down (query-spans language parse (parse-root parse) string line-start line-end))
      (when (parse-inline-root parse)
        (lay-down (query-spans (language-inline language) parse (parse-inline-root parse)
                               string line-start line-end)))
      ;; A code block's line is its own language's: what the block grammar
      ;; made of it goes, and that language's colours are laid down.
      (loop for (block-language nil block-root ranges) in (parse-injections parse)
            when (some (lambda (range) (and (< (car range) line-end) (> (cdr range) line-start)))
                       ranges)
              do (loop for (start . end) in ranges
                       do (let ((from (max start line-start)) (to (min end line-end)))
                            (when (< from to)
                              (fill fonts nil
                                    :start (line-char-index string (- from line-start))
                                    :end (line-char-index string (- to line-start))))))
                 (lay-down (query-spans block-language parse block-root
                                        string line-start line-end))))
    ;; Links, over whatever colours them.
    (loop for (start end target) in (hi:line-links string)
          do (loop for i from start below (min end (length fonts))
                   do (setf (aref fonts i) (merge-fonts (aref fonts i) (hi:link-font target)))))
    ;; And what else there is to draw on the line: a language server's errors.
    (loop for (start end font) in (hi:line-decorations line)
          do (loop for i from (max 0 start) below (min end (length fonts))
                   do (setf (aref fonts i) (merge-fonts (aref fonts i) font))))
    fonts))

(defun highlight-line (language line)
  "Bring LINE's font marks up to date with LANGUAGE's highlighting."
  (let ((buffer (heml-interface:line-buffer line)))
    (when (and buffer (language-ready-p language))
      (let* ((parse (buffer-parse language buffer))
             (plist (heml-interface:line-plist line))
             (old (getf plist 'tree-sitter)))
        (unless (and old (eq (car old) parse)
                     (eql (getf plist 'decoration-tick) hi:*decoration-tick*))
          (setf (getf (heml-interface:line-plist line) 'decoration-tick) hi:*decoration-tick*)
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

;;; A node, while Lisp holds it, is its 32 bytes in a vector, pointed to
;;; while they are passed by value.

(defun make-node () (make-array +node-size+ :element-type '(unsigned-byte 8)))

(defun node-copy (pointer)
  (let ((node (make-node)))
    (dotimes (i +node-size+ node)
      (setf (aref node i) (cffi:mem-aref pointer :uint8 i)))))

(defmacro with-node ((pointer node) &body body)
  `(with-vector-pointer (,pointer ,node) ,@body))

(defun node-null-p (node)
  (with-node (p node) (ts "ts_node_is_null" :bool (:node p))))

(defun node-returning (name node &rest more)
  "The node the C function NAME returns for NODE, or NIL for its null node."
  (let ((result (make-node)))
    (with-node (in node)
      (with-node (out result)
        (if more
            (ts name (:node out) (:node in) (:uint32 (first more)))
            (ts name (:node out) (:node in)))))
    (unless (node-null-p result) result)))

(defun node-parent (node) (node-returning "ts_node_parent" node))

(defun node-children (node)
  (let ((count (with-node (p node) (ts "ts_node_child_count" :uint32 (:node p)))))
    (loop for i below count
          for child = (node-returning "ts_node_child" node i)
          when child collect child)))

(defun node-point (name node)
  (with-foreign-memory (point +point-size+)
    (with-node (p node)
      (ts name (:point point) (:node p)))
    (values (u32 point 0) (u32 point 4))))

(defun node-start (node) (node-point "ts_node_start_point" node))
(defun node-end (node) (node-point "ts_node_end_point" node))

(defun node-type (node)
  (with-node (p node) (ts "ts_node_type" :string (:node p))))

(defun node-has-error-p (node)
  (with-node (p node) (ts "ts_node_has_error" :bool (:node p))))

(defun node-id (node)
  (with-node (p node) (cffi:pointer-address (pointer-at p 16))))

(defun descendant-at (root row column)
  "The smallest node of ROOT's tree at ROW and COLUMN, a byte in that row."
  (let ((result (make-node)))
    (with-foreign-memory (point +point-size+)
      (setf (u32 point 0) row (u32 point 4) column)
      (with-node (in root)
        (with-node (out result)
          (ts "ts_node_descendant_for_point_range" (:node out)
              (:node in) (:point point) (:point point)))))
    result))

;;; The query, read the first time a line is indented.

(defun query-predicates (query)
  "A query's patterns' predicates, each (NAME ARGUMENT*), an argument a
capture's index, as (:CAPTURE . INDEX), or a string."
  (let* ((count (ts "ts_query_pattern_count" :uint32 (:pointer query)))
         (predicates (make-array count)))
    (dotimes (pattern count predicates)
      (with-foreign-memory (step-count 4)
        (let ((steps (ts "ts_query_predicates_for_pattern" :pointer
                         (:pointer query) (:uint32 pattern) (:pointer step-count)))
              (current '())
              (all '()))
          (dotimes (i (u32 step-count 0))
            (let ((type (u32 steps (* i 8)))
                  (value (u32 steps (+ 4 (* i 8)))))
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
                         (count (ts "ts_query_capture_count" :uint32 (:pointer query)))
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
          (cursor (ts "ts_query_cursor_new" :pointer))
          (kinds (language-indent-kinds language))
          (predicates (language-indent-predicates language)))
      (unwind-protect
           (progn
             (ts "ts_query_cursor_exec" :void
                 (:pointer cursor) (:pointer (language-indent-query language))
                 (:node (parse-root parse)))
             (with-foreign-memory (match (+ +match-size+ 4))
               (let ((capture-index (ptr+ match +match-size+)))
                 (loop while (ts "ts_query_cursor_next_capture" :bool
                                 (:pointer cursor) (:pointer match) (:pointer capture-index))
                       do (let* ((pattern (u16 match 4))
                                 (count (u16 match 6))
                                 (captures (pointer-at match 8))
                                 (capture (ptr+ captures (* (u32 capture-index 0) +capture-size+)))
                                 (kind (aref kinds (u32 capture +node-size+))))
                            (when (and kind
                                       (predicates-hold-p language parse pattern captures count
                                                          predicates))
                              (push (cons kind (pattern-settings (aref predicates pattern)))
                                    (gethash (cffi:pointer-address (pointer-at capture 16)) map))))))))
        (ts "ts_query_cursor_delete" :void (:pointer cursor)))
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
      (let* ((start (statement-start lines previous))
             (governor (loop for r from (1- start) downto 0
                             unless (blank-string-p (heml-interface:line-string (svref lines r)))
                               return (heml-interface:line-string (svref lines r)))))
        (cond ((and (language-opens language) (ppcre:scan (language-opens language) text))
               (incf indent width))
              ((and (language-finishes language) (ppcre:scan (language-finishes language) text))
               (decf indent width))
              ;; The one statement a then or a do governed is over: back to
              ;; the line that governed it.
              ((and (language-single language) governor
                    (ppcre:scan (language-single language) governor))
               (setf indent (leading-columns governor))))))
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
                            ;; Where indentation is the block, as in YAML,
                            ;; the tree of what is typed so far cannot say
                            ;; that a block was meant.
                            (when (and (language-opens-always language)
                                       (ppcre:scan (language-opens language) text))
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


;;;; Definitions: the functions and classes C-M-a, C-M-e and C-M-h move by.

(defun buffer-language (buffer)
  "The language that colours BUFFER's major mode, when it is loaded."
  (let ((mode (heml-interface:buffer-major-mode buffer)))
    (loop for language being the hash-values of *languages*
          when (and (equal (language-mode language) mode)
                    (language-ready-p language))
            return language)))

(defun definition-spans (language buffer)
  "Where each of BUFFER's definitions starts and ends, as ((START-LINE
START-CHARPOS END-LINE END-CHARPOS) ...), in the order they start."
  (let* ((parse (buffer-parse language buffer))
         (lines (parse-lines parse))
         (types (language-definitions language))
         (spans '()))
    (labels ((place (row byte)
               (if (< row (length lines))
                   (let ((line (svref lines row)))
                     (values line (line-char-index (heml-interface:line-string line) byte)))
                   (let ((last (svref lines (1- (length lines)))))
                     (values last (length (heml-interface:line-string last))))))
             (walk (node)
               (when (member (node-type node) types :test #'string=)
                 (multiple-value-bind (start-row start-byte) (node-start node)
                   (multiple-value-bind (end-row end-byte) (node-end node)
                     (multiple-value-bind (start-line start) (place start-row start-byte)
                       (multiple-value-bind (end-line end) (place end-row end-byte)
                         (push (list start-line start end-line end) spans))))))
               (mapc #'walk (node-children node))))
      (walk (node-copy (parse-root parse))))
    (nreverse spans)))
