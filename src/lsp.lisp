;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; A client for language servers (the Language Server Protocol): clangd for
;;; C, a Python server, bash-language-server for shell scripts, pasls for
;;; Pascal, and whatever DEFINE-LANGUAGE-SERVER names for a mode.
;;;
;;; A server is a program, one for a mode and a project, started the first
;;; time a file of that mode is edited in that project, when its program is
;;; installed.  Heml talks to it over its standard input and output: JSON
;;; messages (read and written with jzon), each after a Content-Length
;;; header.  The buffer's text is sent when it is opened, and after that
;;; what has changed in it -- the span that changed, for a server that takes
;;; that, or else the whole text -- before each request, and, so that the
;;; server's errors keep up, a moment after typing stops.
;;;
;;; A request waits for its answer while events are dispatched, as an
;;; evaluation in a slave does, and gives up after a few seconds.
;;;
;;; The client is in six files, loaded in this order:
;;;
;;;   lsp.lisp              JSON; what a server is, and the settings it is
;;;                         given; messages; files and positions
;;;   lsp-server.lisp       what a server asks and says unasked; files it
;;;                         wants to hear of; starting and stopping
;;;   lsp-sync.lisp         keeping a server's copy of each buffer up to date
;;;   lsp-diagnostics.lisp  errors and warnings, shown and gone through
;;;   lsp-commands.lisp     going places, describing, fixing, renaming,
;;;                         formatting; the outline; signatures; completion
;;;   lsp-features.lisp     calls, highlighting, colours, hints, lenses and
;;;                         folds; DEFINE-LANGUAGE-SERVER and the servers
;;;                         Heml knows

(in-package :heml)


;;;; JSON, as jzon has it: an object is a hash table whose keys are strings,
;;;; an array a vector, false NIL, and null the symbol NULL.

(defun json (&rest keys-and-values)
  (let ((object (make-hash-table :test 'equal)))
    (loop for (key value) on keys-and-values by #'cddr
          do (setf (gethash key object) value))
    object))

(defun jref (object &rest keys)
  "The value under KEYS, each within the last, in OBJECT; NIL where there
   is none, or it is null."
  (dolist (key keys (if (eq object 'null) nil object))
    (setf object (if (hash-table-p object) (gethash key object) nil))))

(defun jlist (value)
  "VALUE, a JSON array or nothing, as a list."
  (if (vectorp value) (coerce value 'list) '()))


;;;; Servers.

(defvar *language-servers* '()
  "((MODE COMMANDS LANGUAGE-ID GROUP) ...): for a major mode, the commands
   that run a server for it, the first whose program is installed and which
   starts being used; what the protocol calls the language; and the group of
   modes that have one server between them in a project, which is the mode
   itself when it shares with none.")

(defun mode-language-id (mode)
  (third (assoc mode *language-servers* :test #'string=)))

(defun language-server-group (mode)
  "What MODE's server is known by: modes of one group, in one project, have
   one server between them."
  (fourth (assoc mode *language-servers* :test #'string=)))

(defhvar "Language Servers"
  "When true, a language server is started for a file whose mode has one
   installed."
  :value t)

(defstruct (lsp-server (:constructor %make-lsp-server))
  mode                                  ; the major mode it was started for
  group                                 ; and that mode's group, whose modes it serves
  root                                  ; the directory it was started in
  commands                              ; what to run instead, if it cannot start
  connection
  (state :starting)                     ; :STARTING, :READY or :DEAD
  (next-id 0)
  (pending (make-hash-table))           ; request id to the function its answer goes to
  (input (make-array 4096 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
  capabilities
  (encoding :utf-16)                    ; what a position's character counts: :UTF-16 or :UTF-32
  (progress '())                        ; ((TOKEN . TEXT) ...): what it says it is doing
  (watchers '())                        ; ((ID (SCANNER . KINDS) ...) ...): files it wants to hear of
  watched                               ; file to write date, as last looked at
  settings-date                         ; the .heml-project's date when its settings were sent
  (documents (make-hash-table :test 'eq)) ; buffer to a DOCUMENT
  (diagnostics (make-hash-table :test 'equal)) ; file to ((LINE COLUMN SEVERITY MESSAGE) ...)
  ;; A server says what is wrong with a file, or is asked, or both, each for
  ;; different things: what it said and what it answered are kept apart, a
  ;; file to the protocol's diagnostics, and DIAGNOSTICS is both.
  (pushed (make-hash-table :test 'equal))
  (pulled (make-hash-table :test 'equal)))

;;; What the server has of a buffer: the version last sent, the buffer's
;;; signature then, and, for a server that takes changes, the buffer's
;;; lines and their strings as they were, so that the next change can be
;;; told as the lines that changed (lsp-sync.lisp).  Here, before anything
;;; sets a slot: a structure's SETF is not a function on ECL.
;;;
(defstruct (document (:constructor make-document (version signature)))
  version signature
  lines strings                         ; vectors: the lines when last told, and their text
  (pull t)                              ; whether to ask what is wrong with it
  extras)                               ; the signature its colours, hints and lenses are of

(defvar *lsp-servers* '()
  "The servers running, or starting.")

(defparameter *token-types*
  #("namespace" "type" "class" "enum" "interface" "struct" "typeParameter" "parameter"
    "variable" "property" "enumMember" "event" "function" "method" "macro" "keyword"
    "modifier" "comment" "string" "number" "regexp" "operator" "decorator")
  "The kinds of token a server is told Heml knows.")

(defvar *encoding* :utf-16
  "What a position's character counts, for the server being talked to:
   :UTF-16 code units, or :UTF-32, characters.")

(defvar *additional-language-servers* '()
  "((MODE NAME COMMANDS) ...): servers a mode's buffers have as well as the
   mode's own -- a linter beside the language's server -- each known, as a
   server's group, by its NAME.")

(defun language-server-commands (mode &optional (group (language-server-group mode)))
  "The commands that might run MODE's language server, or the additional
   one called GROUP: those whose programs are installed."
  (remove-if-not (lambda (command) (find-program (first command)))
                 (if (equal group (language-server-group mode))
                     (second (assoc mode *language-servers* :test #'string=))
                     (third (find-if (lambda (entry)
                                       (and (string= (first entry) mode)
                                            (string= (second entry) group)))
                                     *additional-language-servers*)))))

(defun mode-server-groups (mode)
  "The groups of the servers MODE's buffers have: the mode's own first."
  (when (assoc mode *language-servers* :test #'string=)
    (cons (language-server-group mode)
          (loop for (other name) in *additional-language-servers*
                when (string= other mode) collect name))))

(defvar *lsp-failures* '()
  "((GROUP ROOT) ...): where no server could be started, or one kept dying,
   so that none is tried again until \"LSP Restart\" asks.")

(defun lsp-failed-p (group root)
  (member (list group root) *lsp-failures* :test #'equal))

;;; A server that dies once it is ready is started again, by LSP-IDLE; one
;;; that keeps dying is not.

(defvar *lsp-crashes* '()
  "((GROUP ROOT) . TIMES): when the servers that died by themselves did.")

(defparameter *lsp-crash-limit* 3
  "A server that has died this many times within a minute is not started
   again.")

(defun note-lsp-crash (group root)
  "A ready server for GROUP in ROOT died.  True when that is once too often."
  (let* ((key (list group root))
         (now (get-universal-time))
         (entry (or (assoc key *lsp-crashes* :test #'equal)
                    (first (push (list key) *lsp-crashes*)))))
    (setf (cdr entry) (cons now (remove-if (lambda (time) (> (- now time) 60)) (cdr entry))))
    (>= (length (cdr entry)) *lsp-crash-limit*)))


;;;; Settings.

;;; A server asks for its settings (workspace/configuration) by section,
;;; "yaml" or "python.analysis".  They are JSON written as Lisp: an object is
;;; an alist whose keys are strings, an array a vector or a list that is no
;;; alist, and :TRUE, :FALSE and :NULL are themselves.  So
;;;
;;;   (("yaml" ("schemas" ("file:///x/schema.json" . "*.yaml"))))
;;;
;;; is {"yaml": {"schemas": {"file:///x/schema.json": "*.yaml"}}}.

(defvar *language-server-settings* '()
  "Settings for language servers, as JSON written in Lisp: an alist of
   section names and their values, each an alist in turn, a string, a
   number, a vector, or :TRUE, :FALSE or :NULL.  A project's own, the
   :settings of its .heml-project, are laid over these.")

(defun lisp-json (value)
  "VALUE, JSON written in Lisp, as jzon has it."
  (cond ((eq value :true) t)
        ((eq value :false) nil)
        ((eq value :null) 'null)
        ((stringp value) value)
        ((vectorp value) (map 'vector #'lisp-json value))
        ((and (consp value)
              (every (lambda (pair) (and (consp pair) (stringp (car pair)))) value))
         (let ((object (make-hash-table :test 'equal)))
           (loop for (key . member) in value
                 do (setf (gethash key object) (lisp-json member)))
           object))
        ((listp value) (map 'vector #'lisp-json value))
        (t value)))

(defun merge-json (under over)
  "OVER laid on UNDER: objects are merged, member by member, and anything
   else of OVER's replaces UNDER's."
  (cond ((and (hash-table-p under) (hash-table-p over))
         (let ((merged (make-hash-table :test 'equal)))
           (maphash (lambda (key value) (setf (gethash key merged) value)) under)
           (maphash (lambda (key value)
                      (setf (gethash key merged)
                            (multiple-value-bind (old found) (gethash key merged)
                              (if found (merge-json old value) value))))
                    over)
           merged))
        (t over)))

(defun language-server-settings (root)
  "The settings for a server in ROOT, a JSON object: *LANGUAGE-SERVER-SETTINGS*
   under the :settings of ROOT's .heml-project."
  (flet ((object (settings)
           (let ((json (ignore-errors (lisp-json settings))))
             (if (hash-table-p json) json (make-hash-table :test 'equal)))))
    (merge-json (object *language-server-settings*)
                (object (getf (ignore-errors (project-settings root)) :settings)))))

(defun settings-date (root)
  "When ROOT's .heml-project was last written, or NIL when it has none."
  (ignore-errors (file-write-date (project-settings-file root))))

(defun lsp-send-changed-settings (server)
  "Tell SERVER its settings again, if its project's .heml-project has
   changed since it was told."
  (let ((date (settings-date (lsp-server-root server))))
    (unless (eql date (lsp-server-settings-date server))
      (setf (lsp-server-settings-date server) date)
      (lsp-notify server "workspace/didChangeConfiguration"
                  (json "settings" (language-server-settings (lsp-server-root server)))))))

(defun settings-section (settings section)
  "What SETTINGS has for SECTION, a dotted path or nothing for all of it;
   null when it has nothing."
  (let ((value settings))
    (when (and (stringp section) (plusp (length section)))
      (dolist (key (uiop:split-string section :separator "."))
        (setf value (if (hash-table-p value)
                        (multiple-value-bind (member found) (gethash key value)
                          (if found member :missing))
                        :missing))))
    (if (eq value :missing) 'null value)))


;;;; Messages.

(defvar *lsp-log* (let ((file (uiop:getenv "HEML_LSP_LOG")))
                    (and file (plusp (length file)) file))
  "A file every message to and from a language server is added to, or NIL:
   set from HEML_LSP_LOG, for finding out what a server says.")

(defun lsp-log (direction text)
  (when *lsp-log*
    (ignore-errors
     (with-open-file (out *lsp-log* :direction :output :if-exists :append
                                    :if-does-not-exist :create :external-format :utf-8)
       (format out "~A ~A~%" direction text)))))

(defun lsp-send (server object)
  (let ((connection (lsp-server-connection server)))
    (when (and connection (not (eq (lsp-server-state server) :dead)))
      (let* ((text (com.inuoe.jzon:stringify object))
             (body (progn (lsp-log "->" text)
                          (babel:string-to-octets text :encoding :utf-8)))
             (header (babel:string-to-octets
                      (format nil "Content-Length: ~D~C~C~C~C" (length body)
                              #\Return #\Linefeed #\Return #\Linefeed)
                      :encoding :utf-8)))
        ;; What goes wrong is in the log, when there is one.
        (handler-case
            (connection-write (concatenate '(simple-array (unsigned-byte 8) (*)) header body)
                              connection)
          (error (condition)
            (lsp-log "!!" (format nil "not sent: ~A" condition))))))))

(defun lsp-notify (server method params)
  (lsp-send server (json "jsonrpc" "2.0" "method" method "params" params)))

(defun lsp-request-async (server method params function)
  "Ask SERVER; FUNCTION is called with its result and its error, one NIL."
  (let ((id (incf (lsp-server-next-id server))))
    (setf (gethash id (lsp-server-pending server)) function)
    (lsp-send server (json "jsonrpc" "2.0" "id" id "method" method "params" params))
    id))

(defun lsp-wait (predicate seconds)
  "Dispatch events until PREDICATE is true, or SECONDS have passed.  True
   if it became true."
  (let ((deadline (+ (get-internal-real-time) (* seconds internal-time-units-per-second))))
    (loop
      (when (funcall predicate) (return t))
      (when (> (get-internal-real-time) deadline) (return nil))
      (hi::dispatch-events-for 0.05))))

(defun lsp-request (server method params &key (timeout 5))
  "Ask SERVER and wait for its answer: the result, or NIL if there is none
   in TIMEOUT seconds or the server says it cannot."
  (let* ((done nil) (answer nil)
         (id (lsp-request-async server method params
                                (lambda (result error)
                                  (declare (ignore error))
                                  (setf answer result done t)))))
    (unless (lsp-wait (lambda () (or done (eq (lsp-server-state server) :dead))) timeout)
      ;; Given up on: the server is told, and its answer is no one's.
      (remhash id (lsp-server-pending server))
      (lsp-notify server "$/cancelRequest" (json "id" id)))
    (if (eq answer 'null) nil answer)))

;;; What arrives: bytes, a message at a time once its header and all its
;;; body are there.

(defun lsp-receive (server bytes)
  (let ((input (lsp-server-input server)))
    (loop for byte across bytes do (vector-push-extend byte input))
    (loop
      (let ((header-end (search #(13 10 13 10) input)))
        (unless header-end (return))
        (let* ((header (babel:octets-to-string input :end header-end :encoding :utf-8 :errorp nil))
               (at (search "content-length:" header :test #'char-equal))
               (length (and at (parse-integer header :start (+ at 15) :junk-allowed t)))
               (start (+ header-end 4)))
          (unless length
            ;; Not a message: nothing of what is here can be trusted.
            (setf (fill-pointer input) 0)
            (return))
          (when (< (length input) (+ start length))
            (return))
          (let ((body (babel:octets-to-string input :start start :end (+ start length)
                                                    :encoding :utf-8 :errorp nil)))
            (replace input input :start2 (+ start length))
            (decf (fill-pointer input) (+ start length))
            (lsp-log "<-" body)
            (handler-case (lsp-dispatch server (com.inuoe.jzon:parse body))
              (error (condition) (lsp-log "!!" (princ-to-string condition))))))))))

;;;; Files and places, as the protocol writes them.

(defun file-uri (pathname)
  (with-output-to-string (s)
    (write-string "file://" s)
    (loop for byte across (babel:string-to-octets (namestring pathname) :encoding :utf-8)
          for char = (code-char byte)
          do (if (and (< byte 128)
                      (or (alphanumericp char) (find char "/-._~")))
                 (write-char char s)
                 (format s "%~2,'0X" byte)))))

(defun uri-file (uri)
  "The file a file: URI names, or NIL for another kind."
  (when (uiop:string-prefix-p "file://" uri)
    (let ((octets (make-array (length uri) :element-type '(unsigned-byte 8) :fill-pointer 0))
          (i 7))
      (loop while (< i (length uri))
            do (let ((char (char uri i)))
                 (cond ((and (char= char #\%) (< (+ i 2) (length uri)))
                        (vector-push (parse-integer uri :start (1+ i) :end (+ i 3) :radix 16)
                                     octets)
                        (incf i 3))
                       (t
                        (loop for byte across (babel:string-to-octets (string char) :encoding :utf-8)
                              do (vector-push-extend byte octets))
                        (incf i)))))
      (babel:octets-to-string (coerce octets '(simple-array (unsigned-byte 8) (*)))
                              :encoding :utf-8))))

;;; A position is a line and a character, both from 0.  What a character
;;; counts is agreed with the server when it starts: characters themselves
;;; (the protocol's utf-32), which Heml asks for, or else UTF-16 code units,
;;; of which a character past #xFFFF is two.  *ENCODING* is that of the
;;; server being talked to: LSP-CURRENT-SERVER sets it for the command
;;; under way, and what handles a server's messages binds it.

(defun utf16-offset (string charpos)
  (loop for i below (min charpos (length string))
        sum (if (> (char-code (char string i)) #xFFFF) 2 1)))

(defun utf16-charpos (string offset)
  (let ((units 0))
    (dotimes (i (length string) (length string))
      (when (>= units offset) (return i))
      (incf units (if (> (char-code (char string i)) #xFFFF) 2 1)))))

(defun unit-offset (string charpos)
  "How far CHARPOS is into STRING, as the server counts."
  (if (eq *encoding* :utf-32)
      (min charpos (length string))
      (utf16-offset string charpos)))

(defun unit-charpos (string offset)
  "The index in STRING of what the server counts as OFFSET."
  (if (eq *encoding* :utf-32)
      (min offset (length string))
      (utf16-charpos string offset)))

(defun lsp-position (mark)
  (json "line" (1- (count-lines (region (buffer-start-mark (line-buffer (mark-line mark))) mark)))
        "character" (unit-offset (line-string (mark-line mark)) (mark-charpos mark))))

(defun lsp-move-mark (mark line character)
  "Move MARK, in its buffer, to the protocol's position LINE and CHARACTER."
  (buffer-start mark)
  (unless (line-offset mark line)
    (buffer-end mark))
  (let ((string (line-string (mark-line mark))))
    (character-offset mark (unit-charpos string character)))
  mark)

(defun lsp-document (buffer)
  (json "uri" (file-uri (buffer-pathname buffer))))

(defun lsp-position-params (buffer mark)
  (json "textDocument" (lsp-document buffer) "position" (lsp-position mark)))

(defun lsp-symbol-params (buffer mark)
  "As LSP-POSITION-PARAMS, for asking about the name at MARK: just after a
   name's last character, the place asked about is that character."
  (with-mark ((at mark))
    (let ((next (next-character at))
          (previous (previous-character at)))
      (when (and previous (word-char-p previous)
                 (not (and next (word-char-p next))))
        (mark-before at)))
    (lsp-position-params buffer at)))
