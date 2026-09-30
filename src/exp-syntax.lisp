;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;; ---------------------------------------------------------------------------
;;;     Title: experimental syntax highlighting
;;;   Created: 2004-07-09
;;;    Author: Gilbert Baumann <gilbert@base-electronic.de>
;;;       $Id: exp-syntax.lisp,v 1.1 2004-07-09 15:16:14 gbaumann Exp $
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2004 by Gilbert Baumann

(in-package :hi)

;;;; ------------------------------------------------------------------------------------------
;;;; Syntax Highlighting
;;;;

;; This still is only proof of concept.

;; We define highlighting by parsing the buffer content with a simple
;; recursive descend parser. The font attributes for each character are
;; then derived from the parser state. Each line remembers the start and
;; end parser state for caching. If the start parser state is the same as
;; the end parser state of the previous line no reparsing needs to be done.
;; Lines can change and if a line changes its end parser state is
;; considered to be unknown. So if you change a line syntax highlighting of
;; all following lines is potentially invalid. We avoid reparsing all of
;; the rest of the buffer by three means: First we access syntax markup in
;; a lazy fashion; if a line isn't displayed we don't need its syntax
;; markup. Second when while doing reparsing the newly computed end state
;; is the same as the old end state reparsing stops, because this end state
;; then matches the start state of the next line. Third when seeing an open
;; paren in the very first column, we assume that a new top-level
;; expression starts.

;; These recursive descend parsers are written in a mini language which
;; subsequently is compiled to some "byte" code and interpreted by a
;; virtual machine. For now we don't allow for parameters or return values
;; of procedures and so a state boils down to the current procedure, the
;; instruction pointer and the stack of saved activations.

;; This mini language allows to define procedures. Within a body of a
;; procedure the following syntax applies:

;; stmt -> (IF <cond> <tag>)     If <cond> evaluates to true, goto <tag>.
;;                               <cond> can be any lisp expression and has
;;                               the current look-ahead character available
;;                               in the variable 'ch'.
;;         <tag>                 A symbol serving as the target for GOs.
;;         (GO <tag>)            Continue execution at the indicated label.
;;         (CONSUME)             Consume the current lookahead character and
;;                               read the next one putting it into 'ch'.
;;         (CALL <proc>)         Call another procedure
;;         (RETURN)              Return from the current procedure

;; What the user sees is a little different. The function ME expands its
;; input to the above language. Added features are:

;; (IF <cond> <cons> [<alt>])    IF is modified to take statements instead
;;                               of branch targets
;; (PROGN {<stmt>}*)             Mainly because of IF, PROGN is introduced.
;;                               Note that the body can defined new branch
;;                               targets, which also are available from outside
;;                               of it.
;; (WHILE <cond> {<stmt>}*)
;; (COND {(<cond> {<stmt>}*)}*)

;; This mini-language for now is enough to write interesting recursive
;; descend parsers.

(defun line-syntax-info (line)
  (tag-syntax-info (line-tag line)))

(defun (setf line-syntax-info) (value line)
  (setf (getf (line-plist line) 'syntax-info-4) value))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun me (form)
    (cond ((atom form)
           (list form))
          (t
           (ecase (car form)
             ((if)
              (destructuring-bind (cond cons &optional alt) (cdr form)
                (let ((l1 (gensym "L."))
                      (l2 (gensym "L.")))
                  (append (list `(if (not ,cond) ,l1))
                          (me cons)
                          (list `(go ,l2))
                          (list l1)
                          (and alt (me alt))
                          (list l2)))))
             ((while)
              (destructuring-bind (cond &rest body) (cdr form)
                (let ((exit (gensym "EXIT."))
                      (loop (gensym "LOOP.")))
                  (append (list loop)
                          (list `(if (not ,cond) ,exit))
                          (me `(progn ,@body))
                          (list `(go ,loop))
                          (list exit)))))
             ((cond)
              (cond ((null (cdr form)) nil)
                    (t
                     (me
                      `(if ,(caadr form) (progn ,@(cdadr form))
                        (cond ,@(cddr form)))))))
             ((consume return) (list form))
             ((progn) (mapcan #'me (cdr form)))
             ((go) (list form))
             ((call) (list form))))))

  (defun ass (stmts)
    (let ((ip 0)
          (labels nil)
          (fixups nil)
          (code (make-array 0 :fill-pointer 0 :adjustable t)))
      (loop for stmt in stmts
            do
            (cond ((atom stmt)
                   (push (cons stmt ip) labels))
                  ((eq (car stmt) 'go)
                   (vector-push-extend :go code) (incf ip)
                   (push ip fixups)
                   (vector-push-extend (cadr stmt) code) (incf ip))
                  ((eq (car stmt) 'if)
                   (vector-push-extend :if code) (incf ip)
                   (vector-push-extend `(lambda (ch) (declare (ignorable ch)) ,(cadr stmt))
                                       code)
                   (incf ip)
                   (push ip fixups)
                   (vector-push-extend (caddr stmt) code) (incf ip))
                  ((eq (car stmt) 'call)
                   (vector-push-extend :call code) (incf ip)
                   (vector-push-extend `',(cadr stmt) code) (incf ip))
                  ((eq (car stmt) 'consume)
                   (vector-push-extend :consume code) (incf ip))
                  ((eq (car stmt) 'return)
                   (vector-push-extend :return code) (incf ip))
                  (t
                   (incf ip)
                   (vector-push-extend stmt code))))
      (loop for fixup in fixups do
            (let ((q (cdr (assoc (aref code fixup) labels))))
              (unless q
                (error "Undefined label ~S." (aref code fixup)))
              (setf (aref code fixup) q)))
      code)))

(defmacro defstate (name stuff &rest body)
  stuff
  `(setf (gethash ',name *parsers*)
         (vector ,@(coerce (ass (append (me `(progn ,@body))
                                        (list '(return))))
                           'list))))

(defvar *parsers* (make-hash-table))

(defstate initial ()
  (while t
    (call sexp)))

(defstate comment ()
  loop
  (cond ((char= ch #\newline)
         (consume)
         (return))
        (t
         (consume)
         (go loop))))

(defstate bq ()
  (consume)                             ;consume `
  (call sexp))

(defstate rq ()
  (consume)                             ;consume '
  (call sexp))

(defstate uq ()
  (consume)                             ;consume `
  (call sexp))

(defstate sexp ()
  loop
  (call skip-white*)                    ;skip possible white space and comments
  (cond ((char= ch #\() (call list))
        ((char= ch #\`) (call bq))
        ((char= ch #\') (call rq))
        ((char= ch #\,) (call uq))
        ((char= ch #\;) (call comment))
        ((char= ch #\") (call string))
        ((char= ch #\#) (call hash))
        ((char= ch #\:) (call keyword))
        ((or (alphanumericp ch) (find ch "-+*/"))
         (call atom))
        (t
         ;; hmm
         (consume)
         (go loop))))

(defstate hash ()
  (consume)
  (cond ((char= ch #\\) (call char-const))
        ((char= ch #\|) (call block-comment))
        ((char= ch #\+) (call hash-plus))
        ((char= ch #\-) (call hash-minus))
        ((char= ch #\')
         (consume)
         (call sexp))
        (t
         (call sexp))))

;;; #| ... |#, which may hold another.
(defstate block-comment ()
  (consume)                             ;consume |
  loop
  (cond ((char= ch #\|)
         (consume)
         (if (char= ch #\#)
             (progn (consume) (return))
             (go loop)))
        ((char= ch #\#)
         (consume)
         (if (char= ch #\|)
             (progn (call block-comment) (go loop))
             (go loop)))
        (t
         (consume)
         (go loop))))

(defstate keyword ()
  (consume)                             ;consume :
  (while (or (alphanumericp ch) (find ch "-+*/"))
    (consume)))

(defstate char-const ()
  (consume)                             ;\\
  (cond ((or (alphanumericp ch) (find ch "-+*/"))
         (call atom))
        (t
         (consume))))

(defstate string ()
  (consume)
  (while t
    (cond ((char= ch #\\)
           (consume)
           (consume))
          ((char= ch #\")
           (consume)
           (return))
          (t
           (consume)))))

(defstate atom ()
  (while (or (alphanumericp ch) (find ch "-+*/"))
    (consume)))

(defstate list ()
  (consume)                             ;consume open-paren
  (while t
    (call skip-white*)                  ;skip possible white space
    (cond ((char= ch #\))
           (consume)
           (return))
          (t
           (call sexp)))))

(defstate skip-white* ()
  loop
  (while (member ch '(#\space #\tab #\newline #\return #\page))
    (consume))
  (cond ((char= ch #\;)
         (call comment)
         (go loop))
        (t
         (return))))

(defstate hash-plus ()
  (consume)                             ;#\+
  (call sexp)                                ;cond
  (call sexp)                                ;form
  )

(defstate hash-minus ()
  (consume)                             ;#\-
  (call sexp)                                ;cond
  (call sexp)                                ;form
  )

;; --------------------

(defun step** (state char)
  (let* (fun ip code)
    (labels ((fetch ()
               (prog1 (aref code ip) (incf ip)))
             (sync (fun* ip*)
               (setf fun fun*
                     ip  ip*
                     code (or (gethash fun *parsers*)
                              (error "No such fun: ~S." fun))))
             (exit ()
               (sync (pop state) (pop state)))
             (save ()
               (push ip state)
               (push fun state)))
      (exit)
      (loop
          (ecase (fetch)
            (:if
             (let ((cond (fetch))
                   (target (fetch)))
               (when (funcall cond char)
                 (setf ip target))))
            (:consume
             (save)
             (return-from step** state))
            (:return
             '(print (list :return state) *trace-output*)
             (exit)
             ;;(print (list :dada state))
             )
            (:call
             (let ((new-fun (fetch)))
               '(print (list :call new-fun) *trace-output*)
               (save)
               (sync new-fun 0)))
            (:go
             (setf ip (fetch))))))))

(defun dodo (string)
  (let ((state (list 'initial 0)))
    (loop for c across string do
          (setf state (step** state c))
          (let ((q (member-if (lambda (x) (member x '(string rq bq uq comment))) state)))
            (case (car q)
              (comment (format t "/~A" c))
              ((rq bq) (princ (char-upcase c)))
              (uq (princ c))
              ((nil) (princ c)))))
    state))

;;;;;;;;;;;;;

(defun initial-syntax-state ()
  (list 'initial 0))

(defun empty-syntax-info ()
  (make-syntax-info :frob nil (initial-syntax-state) nil))

(declaim (special *mode-highlighters* *buffer-list*))

(defun lisp-highlighted-p (line)
  "True when LINE's buffer is coloured by this file's Lisp parser.  Tags are
also computed for their package, which the modeline shows in any buffer, and
a buffer in another mode must not be coloured as Lisp on that account."
  (let ((buffer (line-buffer line)))
    (and buffer
         (eq (gethash (buffer-major-mode buffer) *mode-highlighters*) 'line-tag))))

(defun recompute-syntax-marks (line tag)
  (unless (lisp-highlighted-p line)
    (return-from recompute-syntax-marks (empty-syntax-info)))
  (let* ((sy (or (tag-syntax-info tag)
                 (empty-syntax-info)))
         (prev (line-previous line))
         (prev-to (if prev
                      (sy-to-state (tag-syntax-info (%line-tag prev)))
                      (initial-syntax-state)))
         (font-marks (sy-font-marks sy)))
    (cond ((and (eq (sy-signature sy) (line-signature line))
                (equal (sy-from-state sy) prev-to))
           ;; no work
           sy)
          (t
           ;; work to do, but first remove old font marks
           (dolist (fm font-marks)
             (hi::delete-font-mark fm))
           (setf font-marks nil)
           ;; now do the highlighting
           (let ((state prev-to)
                 (last-font 0))
             ;;(print `(:begin ,state) *trace-output*)
             (loop for p from 0 below (line-length line) do
                  (let ((ch (line-character line p)))
                    (setf state (step** state ch))
                    (let ((font (state-font state)))
                      (unless (eq font last-font)
                        (push (hi::font-mark line p font) font-marks)
                        (setf last-font font)))))
             (setf state (step** state #\newline))
             ;; hack
             (let ((s (line-string line)) p1 p2)
               (when (and (eql 0 (search "(def" s))
                          (setf p1 (position #\space s))
                          (setf p2 (position #\space s :start (1+ p1))))
                 (push (hi::font-mark line (1+ p1) 5) font-marks)
                 (push (hi::font-mark line p2 0) font-marks)))
             (make-syntax-info (line-signature line)
                               prev-to
                               state
                               font-marks) )))))

(defun state-font (state)
  (cond ((member 'hash-plus state)
         6)
        (t
         (let ((q (member-if (lambda (x) (member x '(string rq bq uq comment block-comment
                                                       keyword hash-plus hash-minus)))
                             state)))
           (case (car q)
             ((comment block-comment) 1)
             (keyword 6)
             (rq 5)
             (bq 2)
             (uq 3)
             (string 4)
             (hash-plus 6)
             (hash-minus 7)
             ((nil) 0))))))


;;;; Highlighters by mode

;;; A buffer's major mode decides how its lines are coloured: a mode names a
;;; function of a line, which brings the line's font marks up to date before
;;; redisplay reads them.  A mode that names none gets no colouring.  The
;;; parser above understands Lisp, so it is Lisp mode's; buffers in other
;;; modes are no longer coloured as if they held Lisp.

(defvar *mode-highlighters* (make-hash-table :test 'equal)
  "Major mode name to the function that brings a line's highlighting up to
date.")

(defun define-mode-highlighter (mode function)
  "Make FUNCTION, of a line, the highlighter of lines in buffers whose major
mode is MODE.  NIL removes it."
  (if function
      (setf (gethash mode *mode-highlighters*) function)
      (remhash mode *mode-highlighters*))
  ;; Tags already computed made font marks, or not, by the highlighter
  ;; before: they are computed again as they are drawn.
  (dolist (buffer *buffer-list*)
    (setf (buffer-tag-line-number buffer) 0))
  mode)

(defun highlight-line (line)
  (let ((buffer (line-buffer line)))
    (when buffer
      (let ((function (gethash (buffer-major-mode buffer) *mode-highlighters*)))
        (if function
            (funcall function line)
            (highlight-links line))))))

(define-mode-highlighter "Lisp" 'line-tag)

;;; Links.  A line's links are found in its text: a Markdown link
;;; [text](url), an autolink <url>, and a bare URL.  Each is drawn in
;;; LINK-FONT, whose :LINK says where it goes -- the terminal makes it a
;;; hyperlink, and "Open Link" follows it.  A mode's highlighter lays links
;;; into its colouring (tree-sitter's does); a buffer whose mode has none has
;;; its links shown on their own, on lines without colours of their own.

(defparameter *link-scanners*
  (list (cons (cl-ppcre:create-scanner "\\[[^\\]\\n]+\\]\\(([^)\\s]+)\\)") 1)
        (cons (cl-ppcre:create-scanner "<((?:https?|ftp|file|mailto):[^>\\s]+)>") 1)
        (cons (cl-ppcre:create-scanner "(?:https?|ftp|file)://[^\\s<>\"'`]+") 0))
  "Each scanner, and the register that is the link's target (0: the match).")

(defun trim-url (url)
  "URL without the punctuation that ends the sentence it is in."
  (let ((end (length url)))
    (loop while (and (plusp end)
                     (or (find (char url (1- end)) ".,;:!?'\"")
                         (and (char= (char url (1- end)) #\))
                              (> (count #\) url :end end) (count #\( url :end end)))))
          do (decf end))
    (subseq url 0 end)))

(defun line-links (string)
  "The links in STRING, as ((START END TARGET) ...), in order and apart."
  (let ((links '()))
    (loop for (scanner . register) in *link-scanners*
          do (cl-ppcre:do-scans (start end rstarts rends scanner string)
               (let* ((target (if (zerop register)
                                  (trim-url (subseq string start end))
                                  (subseq string (aref rstarts (1- register))
                                          (aref rends (1- register)))))
                      (end (if (zerop register) (+ start (length target)) end)))
                 (unless (find-if (lambda (link)
                                    (and (< start (second link)) (< (first link) end)))
                                  links)
                   (push (list start end target) links)))))
    (sort links #'< :key #'first)))

(defun link-font (target)
  "The font a link to TARGET is drawn in."
  (list :fg 6 :underline t :link target))

(defun link-at-mark (mark)
  "The target of the link MARK is on, or NIL."
  (let ((position (mark-charpos mark)))
    (third (find-if (lambda (link) (and (<= (first link) position) (< position (second link))))
                    (line-links (line-string (mark-line mark)))))))

(defun highlight-links (line)
  "Show LINE's links, unless the line has colours of its own."
  (let ((old (getf (line-plist line) 'link-marks)))
    (unless (and old (eq (car old) (line-signature line)))
      (dolist (mark (cdr old))
        (delete-font-mark mark))
      (setf (getf (line-plist line) 'link-marks)
            (cons (line-signature line)
                  (unless (some (lambda (m) (fast-font-mark-p m)) (line-marks line))
                    (loop for (start end target) in (line-links (line-string line))
                          collect (font-mark line start (link-font target))
                          collect (font-mark line end 0))))))))


;;;; Tag computation

;; This should probably go into a different file

(defun line-tag (line)
  (let ((buffer (line-buffer line)))
    (cond
     ((null buffer)
      nil)
     (t
      (unless (< (line-number line)
                 (buffer-tag-line-number buffer))
        (recompute-tags-up-to line)
        (setf (buffer-tag-line-number buffer) (1+ (line-number line))))
      (%line-tag line)))))

(defun recompute-tags-up-to (end-line)
  (let* ((level (buffer-tag-line-number (line-buffer end-line)))
         (start-line
          (iter (for line initially end-line then prev)
                (for prev = (line-previous line))
                (let ((validp (< (line-number line) level)))
                  (finding line such-that (or validp (null prev)))))))
    (unless (line-previous start-line)
      (let ((tag (make-tag :syntax-info (empty-syntax-info))))
        (setf (%line-tag start-line) tag)
        (setf (tag-syntax-info tag) (recompute-syntax-marks start-line tag)))
      (setf start-line (line-next start-line)))
    (iter (for line initially start-line then (line-next line))
          (while line)
          (recompute-line-tag line)
          (until (eq line end-line)))))

(defmacro cache-scanner (regex)
  ;; help compilers that don't support compiler macros
  `(load-time-value (ppcre:create-scanner ,regex)))

(defun recompute-line-tag (line)
  (let ((ptag (%line-tag (line-previous line)))
        (tag (or (%line-tag line)
                 (setf (%line-tag line) (make-tag)))))
    (setf (tag-syntax-info tag) (recompute-syntax-marks line tag))
    (setf (tag-package tag)
          (or (cl-ppcre:register-groups-bind
                  (package)
                  ((cache-scanner
                    "^\\((?:[a-zA-Z]+:)?in-package (?:[^)]*::?)([^)]*)\\)")
                   (line-string line))
                (when package
                  (heml::canonicalize-slave-package-name package)))
              (cl-ppcre:register-groups-bind
                  (package)
                  ((cache-scanner "^\\(in-package \"([^)]*)\"\\)")
                   (line-string line))
                (when package
                  (heml::canonicalize-slave-package-name package)))
              (tag-package ptag)))))

;; $Log: exp-syntax.lisp,v $
;; Revision 1.1  2004-07-09 15:16:14  gbaumann
;; moved syntax highlighting out to another file.
;;
;;
