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
;;; What it gives: the errors and warnings it finds, underlined where they
;;; are and listed; the definition of what is at point, and its references;
;;; what the server says of it; completions, for the popup; renaming; and
;;; formatting.
;;;
;;; A request waits for its answer while events are dispatched, as an
;;; evaluation in a slave does, and gives up after a few seconds.

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
;;; signature then, and, for a server that takes changes, the text, so that
;;; the next change can be told as a span of it.  Here, before anything
;;; sets a slot: a structure's SETF is not a function on ECL.
;;;
(defstruct (document (:constructor make-document (version signature text)))
  version signature text
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

(defun lsp-dispatch (server message)
  (let ((id (jref message "id"))
        (method (jref message "method"))
        (*encoding* (lsp-server-encoding server)))
    (cond ((and method id)
           ;; The server asks something.
           (lsp-send server
                     (json "jsonrpc" "2.0" "id" id
                           "result" (or (ignore-errors
                                         (lsp-answer server method (jref message "params")))
                                        'null))))
          (method
           (lsp-told server method (jref message "params")))
          (id
           (let ((function (gethash id (lsp-server-pending server))))
             (when function
               (remhash id (lsp-server-pending server))
               (funcall function (gethash "result" message) (jref message "error"))))))))

(defun lsp-answer (server method params)
  "The answer to what SERVER asks: an edit it wants made is made, its
   settings are given, what it wants to hear of is noted, and anything else
   is answered with null."
  (cond ((string= method "workspace/applyEdit")
         (json "applied" (and (ignore-errors (apply-workspace-edit (jref params "edit")) t) t)))
        ;; What it would answer has changed: its documents are asked about
        ;; again.
        ((string= method "workspace/diagnostic/refresh")
         (loop for document being the hash-values of (lsp-server-documents server)
               do (setf (document-pull document) t))
         'null)
        ((member method '("workspace/semanticTokens/refresh" "workspace/inlayHint/refresh"
                          "workspace/codeLens/refresh")
                 :test #'string=)
         (loop for document being the hash-values of (lsp-server-documents server)
               do (setf (document-extras document) nil))
         'null)
        ((string= method "workspace/configuration")
         (let ((settings (language-server-settings (lsp-server-root server))))
           (map 'vector
                (lambda (item) (settings-section settings (jref item "section")))
                (jlist (jref params "items")))))
        ((string= method "workspace/workspaceFolders")
         (vector (json "uri" (file-uri (string-right-trim "/" (namestring (lsp-server-root server))))
                       "name" (project-name (namestring (lsp-server-root server))))))
        ((string= method "client/registerCapability")
         (dolist (registration (jlist (jref params "registrations")))
           (when (equal (jref registration "method") "workspace/didChangeWatchedFiles")
             (push (cons (jref registration "id")
                         (file-watchers server (jlist (jref registration "registerOptions"
                                                            "watchers"))))
                   (lsp-server-watchers server))))
         'null)
        ((string= method "client/unregisterCapability")
         (dolist (registration (jlist (or (jref params "unregisterations")
                                          (jref params "unregistrations"))))
           (setf (lsp-server-watchers server)
                 (remove (jref registration "id") (lsp-server-watchers server)
                         :key #'car :test #'equal)))
         'null)
        ;; Asked to choose among things it offers, Heml chooses none, and
        ;; shows what was said.
        ((string= method "window/showMessageRequest")
         (lsp-say server (jref params "type")
                  (format nil "~A~@[  (~{~A~^, ~})~]" (or (jref params "message") "")
                          (mapcar (lambda (action) (jref action "title"))
                                  (jlist (jref params "actions")))))
         'null)
        ((string= method "window/showDocument")
         (json "success" nil))
        (t 'null)))

(defun lsp-told (server method params)
  "What SERVER says unasked."
  (cond ((string= method "textDocument/publishDiagnostics")
         (lsp-note-diagnostics server (jref params "uri") (jlist (jref params "diagnostics"))))
        ((string= method "window/showMessage")
         (lsp-say server (jref params "type") (or (jref params "message") "")))
        ((string= method "window/logMessage")
         (lsp-say server 4 (or (jref params "message") "")))
        ((string= method "$/progress")
         (lsp-note-progress server (jref params "token") (jref params "value")))))


;;;; What a server says to the user, and what it says it is doing.

;;; A server's messages arrive while events are handled, when nothing may be
;;; drawn: they wait in *LSP-SAID* for LSP-IDLE, which puts every one in the
;;; buffer "Language Servers" and shows errors, warnings and what the server
;;; meant to be seen in the echo area.

(defvar *lsp-said* '()
  "((TYPE TEXT) ...), the latest first: what servers have said that is yet
   to be shown.  TYPE is the protocol's: 1 an error, 2 a warning, 3 for the
   user's information, 4 for the log.")

(defparameter *lsp-log-lines* 2000
  "The most lines the buffer of what servers say keeps.")

(defun lsp-say (server type text)
  (push (list (if (integerp type) type 4)
              (format nil "~A: ~A" (lsp-server-group server) text))
        *lsp-said*))

(defun lsp-show-said ()
  "Put what servers have said in their buffer, and the last thing meant for
   the user in the echo area."
  (when *lsp-said*
    (let* ((said (nreverse (shiftf *lsp-said* '())))
           (buffer (or (getstring "Language Servers" *buffer-names*)
                       (make-buffer "Language Servers" :modes '("Fundamental"))))
           (shown (find-if (lambda (type) (<= type 3)) said :key #'first :from-end t)))
      (when buffer
        (with-writable-buffer (buffer)
          (let ((end (buffer-end-mark buffer)))
            (loop for (type text) in said
                  do (insert-string end (format nil "~[~;error  ~;warning  ~:;~]~A~%" type
                                                (substitute #\Space #\Newline text)))))
          ;; The oldest lines go.
          (let ((extra (- (count-lines (buffer-region buffer)) *lsp-log-lines*)))
            (when (plusp extra)
              (with-mark ((from (buffer-start-mark buffer))
                          (to (buffer-start-mark buffer)))
                (line-offset to extra 0)
                (delete-region (region from to))))))
        (setf (buffer-modified buffer) nil))
      (when shown
        (message "~A" (substitute #\Space #\Newline (second shown)))))))

(defun lsp-note-progress (server token value)
  "SERVER has begun something, got further with it, or finished it: the
   modeline of its buffers says what, and how far."
  (let ((kind (jref value "kind"))
        (entry (assoc token (lsp-server-progress server) :test #'equal)))
    (cond ((equal kind "end")
           (setf (lsp-server-progress server)
                 (remove token (lsp-server-progress server) :key #'car :test #'equal)))
          ((or (equal kind "begin") (equal kind "report"))
           (unless entry
             (setf entry (list token "" nil))
             (push entry (lsp-server-progress server)))
           (when (stringp (jref value "title"))
             (setf (second entry) (jref value "title")))
           (setf (third entry)
                 (format nil "~A~@[ ~A~]~@[ ~D%~]" (second entry)
                         (let ((message (jref value "message")))
                           (and (stringp message) (plusp (length message)) message))
                         (let ((percentage (jref value "percentage")))
                           (and (realp percentage) (round percentage)))))))
    (loop for buffer being the hash-keys of (lsp-server-documents server)
          do (ignore-errors (update-lsp-modeline buffer)))))

(defun lsp-progress-text (server)
  "What SERVER says it is doing, the thing it began last, or NIL."
  (let ((text (third (first (lsp-server-progress server)))))
    (and text (subseq text 0 (min 40 (length text))))))


;;;; Files a server wants to hear of.

;;; A server registers patterns (client/registerCapability, for
;;; workspace/didChangeWatchedFiles): files that, made, changed or deleted
;;; by anything -- a checkout, a build, another editor -- it should be told
;;; of.  Heml looks at the project's files every few seconds and tells it
;;; what is different from the last look.

(defun glob-scanner (glob)
  "A scanner matching the paths the protocol's GLOB does: * is anything
   within a name, ** any number of directories, ? a character, {a,b} either,
   and [...] one of some characters."
  (let ((out (make-string-output-stream))
        (i 0) (depth 0) (length (length glob)))
    (write-string "^" out)
    (loop while (< i length)
          do (let ((char (char glob i)))
               (cond ((and (char= char #\*) (< (1+ i) length) (char= (char glob (1+ i)) #\*))
                      (incf i)
                      (cond ((and (< (1+ i) length) (char= (char glob (1+ i)) #\/))
                             (incf i)
                             (write-string "(?:.*/)?" out))
                            (t (write-string ".*" out))))
                     ((char= char #\*) (write-string "[^/]*" out))
                     ((char= char #\?) (write-string "[^/]" out))
                     ((char= char #\{) (incf depth) (write-string "(?:" out))
                     ((and (char= char #\}) (plusp depth)) (decf depth) (write-string ")" out))
                     ((and (char= char #\,) (plusp depth)) (write-string "|" out))
                     ((char= char #\[)
                      (let ((close (position #\] glob :start i)))
                        (cond (close
                               (write-char #\[ out)
                               (when (and (< (1+ i) close) (char= (char glob (1+ i)) #\!))
                                 (write-char #\^ out)
                                 (incf i))
                               (write-string (subseq glob (1+ i) close) out)
                               (write-char #\] out)
                               (setf i close))
                              (t (write-string "\\[" out)))))
                     ((find char ".+()|^$\\") (write-char #\\ out) (write-char char out))
                     (t (write-char char out))))
             (incf i))
    (write-string "$" out)
    (ignore-errors (cl-ppcre:create-scanner (get-output-stream-string out)))))

(defun file-watchers (server watchers)
  "The protocol's WATCHERS as ((SCANNER . KINDS) ...): a scanner for a file's
   whole name, and the kinds of change wanted, 1 for made, 2 changed, 4
   deleted, added together."
  (loop for watcher in watchers
        for pattern = (jref watcher "globPattern")
        for glob = (cond ((stringp pattern) pattern)
                         ((hash-table-p pattern)
                          (let ((base (jref pattern "baseUri")))
                            (format nil "~A/~A"
                                    (string-right-trim
                                     "/" (or (uri-file (if (hash-table-p base)
                                                           (or (jref base "uri") "")
                                                           (or base "")))
                                             ""))
                                    (or (jref pattern "pattern") "")))))
        for scanner = (and glob
                           (glob-scanner
                            ;; A pattern that names no place is one within the
                            ;; server's directory.
                            (if (or (uiop:string-prefix-p "/" glob) (uiop:string-prefix-p "**" glob))
                                glob
                                (concatenate 'string (namestring (lsp-server-root server)) glob))))
        when scanner
          collect (cons scanner (or (jref watcher "kind") 7))))

(defparameter *lsp-watch-ticks* 10
  "How many of LSP-IDLE's half seconds pass between looks at the files
   servers want to hear of.")

(defparameter *lsp-watch-limit* 20000
  "A project with more files than this is not looked at.")

(defun watched-kinds (server file)
  "The kinds of change to FILE, a whole name, that SERVER wants to hear of,
   or NIL."
  (let ((kinds 0))
    (loop for (nil . watchers) in (lsp-server-watchers server)
          do (loop for (scanner . wanted) in watchers
                   when (cl-ppcre:scan scanner file)
                     do (setf kinds (logior kinds wanted))))
    (and (plusp kinds) kinds)))

(defun lsp-look-at-files (server)
  "Tell SERVER which of the files it wants to hear of have been made,
   changed or deleted since the last look.  The first look tells it nothing."
  (when (and (lsp-server-watchers server) (eq (lsp-server-state server) :ready))
    (let* ((root (namestring (lsp-server-root server)))
           (files (ignore-errors (project-files root)))
           (old (lsp-server-watched server))
           (new (make-hash-table :test 'equal))
           (changes '()))
      (when (<= (length files) *lsp-watch-limit*)
        (dolist (relative files)
          (let* ((file (concatenate 'string root relative))
                 (kinds (watched-kinds server file)))
            (when kinds
              (let ((date (ignore-errors (file-write-date file))))
                (when date
                  (setf (gethash file new) date)
                  (when old
                    (let ((before (gethash file old)))
                      (cond ((null before)
                             (when (logtest kinds 1) (push (cons file 1) changes)))
                            ((/= before date)
                             (when (logtest kinds 2) (push (cons file 2) changes)))))))))))
        (when old
          (loop for file being the hash-keys of old
                unless (gethash file new)
                  do (let ((kinds (watched-kinds server file)))
                       (when (and kinds (logtest kinds 4))
                         (push (cons file 3) changes)))))
        (setf (lsp-server-watched server) new)
        (when changes
          (lsp-notify server "workspace/didChangeWatchedFiles"
                      (json "changes"
                            (map 'vector (lambda (change)
                                           (json "uri" (file-uri (car change))
                                                 "type" (cdr change)))
                                 changes))))))))


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


;;;; Starting and stopping.

(defun start-language-server (mode root &key (group (language-server-group mode))
                                             (commands (language-server-commands mode group)))
  "Start MODE's server in ROOT, or the additional one called GROUP: the
   first of COMMANDS, by default those of its commands that are installed.
   One that cannot start, or will not be initialized, gives way to the next
   (LSP-SERVER-DIED), and when there is no next the place is remembered in
   *LSP-FAILURES*."
  (let ((installed commands))
    (let ((command (first installed))
          (others (rest installed)))
     (when command
      (let ((server (%make-lsp-server :mode mode :group group
                                      :root root :commands others)))
        (setf (lsp-server-connection server)
              (make-process-connection
               ;; What the server says on its error output is no one's to read.
               (list "/bin/sh" "-c" (format nil "exec ~{~A~^ ~} 2>/dev/null"
                                            (mapcar #'shell-quote command)))
               :directory root
               :filter (lambda (connection bytes)
                         (declare (ignore connection))
                         (handler-case (lsp-receive server bytes)
                           (error (condition)
                             (lsp-log "!!" (format nil "not read: ~A" condition))))
                         nil)
               :sentinel (lambda (connection event)
                           (declare (ignore connection))
                           (when (member event '(:disconnected :error))
                             (lsp-server-died server)))))
        (push server *lsp-servers*)
        (lsp-request-async
         server "initialize"
         (json "processId" (ignore-errors (isys:getpid))
               "clientInfo" (json "name" "Heml")
               "rootPath" (string-right-trim "/" (namestring root))
               "rootUri" (file-uri (string-right-trim "/" (namestring root)))
               "workspaceFolders" (vector (json "uri" (file-uri (string-right-trim "/" (namestring root)))
                                                "name" (project-name (namestring root))))
               "capabilities"
               (json "general" (json "positionEncodings" (vector "utf-32" "utf-16"))
                     "window" (json "workDoneProgress" t
                                    "showMessage" (json)
                                    "showDocument" (json "support" nil))
                     "textDocument"
                     (json "synchronization" (json "didSave" t)
                           "publishDiagnostics" (json)
                           "diagnostic" (json "dynamicRegistration" nil
                                              "relatedDocumentSupport" nil)
                           "hover" (json "contentFormat" (vector "plaintext" "markdown"))
                           "completion"
                           (json "completionItem"
                                 (json "snippetSupport" t
                                       "insertReplaceSupport" t
                                       "documentationFormat" (vector "plaintext" "markdown")
                                       "resolveSupport"
                                       (json "properties"
                                             (vector "documentation" "detail"
                                                     "additionalTextEdits"))))
                           "definition" (json "linkSupport" t)
                           "declaration" (json "linkSupport" t)
                           "typeDefinition" (json "linkSupport" t)
                           "implementation" (json "linkSupport" t)
                           "references" (json)
                           "documentHighlight" (json)
                           "callHierarchy" (json)
                           "rename" (json "prepareSupport" t)
                           "formatting" (json)
                           "rangeFormatting" (json)
                           "onTypeFormatting" (json)
                           "documentSymbol" (json "hierarchicalDocumentSymbolSupport" t)
                           "semanticTokens"
                           (json "requests" (json "full" t)
                                 "tokenTypes" *token-types*
                                 "tokenModifiers" (vector)
                                 "formats" (vector "relative"))
                           "inlayHint" (json)
                           "codeLens" (json)
                           "foldingRange" (json "lineFoldingOnly" t)
                           "signatureHelp"
                           (json "signatureInformation"
                                 (json "parameterInformation" (json "labelOffsetSupport" t)
                                       "activeParameterSupport" t))
                           "codeAction"
                           (json "dataSupport" t
                                 "resolveSupport" (json "properties" (vector "edit" "command"))
                                 "codeActionLiteralSupport"
                                 (json "codeActionKind"
                                       (json "valueSet"
                                             (vector "quickfix" "refactor" "refactor.extract"
                                                     "refactor.inline" "refactor.rewrite"
                                                     "source" "source.organizeImports")))))
                     "workspace" (json "workspaceFolders" t "configuration" t
                                       "applyEdit" t "symbol" (json)
                                       "workspaceEdit"
                                       (json "documentChanges" t
                                             "resourceOperations"
                                             (vector "create" "rename" "delete"))
                                       "didChangeWatchedFiles"
                                       (json "dynamicRegistration" t
                                             "relativePatternSupport" t)
                                       "executeCommand" (json)
                                       "semanticTokens" (json "refreshSupport" t)
                                       "inlayHint" (json "refreshSupport" t)
                                       "codeLens" (json "refreshSupport" t)
                                       "diagnostics" (json "refreshSupport" t))))
         (lambda (result error)
           (cond ((or error (not (hash-table-p result)))
                  (lsp-server-died server))
                 (t
                  (setf (lsp-server-capabilities server) (jref result "capabilities"))
                  (when (equal (jref result "capabilities" "positionEncoding") "utf-32")
                    (setf (lsp-server-encoding server) :utf-32))
                  (lsp-notify server "initialized" (json))
                  ;; Some servers take their settings only when told of them.
                  (setf (lsp-server-settings-date server) (settings-date root))
                  (let ((settings (language-server-settings root)))
                    (when (plusp (hash-table-count settings))
                      (lsp-notify server "workspace/didChangeConfiguration"
                                  (json "settings" settings))))
                  (setf (lsp-server-state server) :ready)))))
        server)))))

(defun lsp-server-died (server &optional stopped)
  "SERVER is gone.  One that went before it was ready, unless Heml STOPPED
   it, could not be started: the next command for its mode is tried, and
   when there is none, none is tried there again.  One that went once it
   was ready is started again by LSP-IDLE, unless it keeps going."
  (unless (eq (lsp-server-state server) :dead)
    (let ((mode (lsp-server-mode server))
          (group (lsp-server-group server))
          (root (lsp-server-root server)))
      (cond (stopped)
            ((eq (lsp-server-state server) :starting)
             (unless (and (lsp-server-commands server)
                          (ignore-errors
                           (start-language-server mode root :group group
                                                  :commands (lsp-server-commands server))))
               (pushnew (list group root) *lsp-failures* :test #'equal)))
            ((note-lsp-crash group root)
             (pushnew (list group root) *lsp-failures* :test #'equal))))
    (setf (lsp-server-state server) :dead)
    (setf *lsp-servers* (remove server *lsp-servers*))
    ;; What its buffers' other servers say is wrong is still wrong.
    (loop for buffer being the hash-keys of (lsp-server-documents server)
          do (ignore-errors (rebuild-buffer-diagnostics buffer))
             (ignore-errors (update-lsp-modeline buffer)))
    (incf hi:*decoration-tick*)
    (let ((connection (lsp-server-connection server)))
      (when connection
        (setf (lsp-server-connection server) nil)
        (ignore-errors (delete-connection connection))))))

(defun stop-language-server (server)
  (when (eq (lsp-server-state server) :ready)
    (lsp-request server "shutdown" 'null :timeout 1)
    (lsp-notify server "exit" 'null)
    (lsp-wait (lambda () nil) 0.1))
  (lsp-server-died server t))

(defun buffer-language-servers (buffer &optional start)
  "The language servers for BUFFER, its mode's own first, each started if
   START and it can be."
  (let ((pathname (buffer-pathname buffer))
        (mode (buffer-major-mode buffer)))
    (when (and pathname (value language-servers))
      (let ((root (or (buffer-project-root buffer) (directory-namestring pathname))))
        (loop for group in (mode-server-groups mode)
              for server = (or (find-if (lambda (server)
                                          (and (string= (lsp-server-group server) group)
                                               (string= (lsp-server-root server) root)))
                                        *lsp-servers*)
                               (and start (not (lsp-failed-p group root))
                                    (start-language-server mode root :group group)))
              when server collect server)))))

(defun buffer-language-server (buffer &optional start)
  "The language server for BUFFER, its mode's own, started if START and it
   can be."
  (find (language-server-group (buffer-major-mode buffer))
        (buffer-language-servers buffer start)
        :key #'lsp-server-group :test #'equal))

(defun buffer-server-with (buffer capability)
  "The first of BUFFER's servers, ready, that has CAPABILITY, a name among
   the capabilities a server says it has."
  (find-if (lambda (server)
             (and (eq (lsp-server-state server) :ready)
                  (jref (lsp-server-capabilities server) capability)))
           (buffer-language-servers buffer)))

(defun buffer-server-failed-p (buffer)
  "Whether BUFFER's server could not be started, or kept dying."
  (let ((pathname (buffer-pathname buffer))
        (group (language-server-group (buffer-major-mode buffer))))
    (and pathname group
         (lsp-failed-p group (or (buffer-project-root buffer)
                                 (directory-namestring pathname))))))

(defun stop-language-servers ()
  (dolist (server (copy-list *lsp-servers*))
    (ignore-errors (stop-language-server server))))

(add-hook exit-hook 'stop-language-servers)


;;;; Keeping the server's copy of a buffer up to date.

(defun incremental-sync-p (server)
  "Whether SERVER takes a change as the span that changed (the protocol's
   TextDocumentSyncKind.Incremental) rather than the whole text again."
  (let ((sync (jref (lsp-server-capabilities server) "textDocumentSync")))
    (eql 2 (if (hash-table-p sync) (jref sync "change") sync))))

(defun text-position (text index)
  "The protocol's position of the character at INDEX in TEXT: its line, and
   how many UTF-16 code units into the line it is."
  (let ((line 0) (line-start 0))
    (loop for i below index
          when (char= (char text i) #\Newline)
            do (incf line) (setf line-start (1+ i)))
    (values line
            (if (eq *encoding* :utf-32)
                (- index line-start)
                (loop for i from line-start below index
                      sum (if (> (char-code (char text i)) #xFFFF) 2 1))))))

(defun text-change (old new)
  "The one span of OLD that must be replaced to make it NEW: where it
   starts and ends in OLD, as lines and characters, and the text that
   replaces it.  What OLD and NEW share at their start and at their end is
   left out."
  (let* ((old-length (length old))
         (new-length (length new))
         (prefix (or (mismatch old new) old-length))
         (room (- (min old-length new-length) prefix))
         (suffix (let ((k 0))
                   (loop while (and (< k room)
                                    (char= (char old (- old-length k 1))
                                           (char new (- new-length k 1))))
                         do (incf k))
                   k)))
    (multiple-value-bind (start-line start-character) (text-position old prefix)
      (multiple-value-bind (end-line end-character) (text-position old (- old-length suffix))
        (values start-line start-character end-line end-character
                (subseq new prefix (- new-length suffix)))))))

(defun lsp-sync (server buffer)
  "Tell SERVER of BUFFER, or, if its text has changed, of the change: the
   span that changed, for a server that takes that, and the whole text for
   one that does not."
  (when (eq (lsp-server-state server) :ready)
    (let ((document (gethash buffer (lsp-server-documents server)))
          (signature (buffer-signature buffer))
          (incremental (incremental-sync-p server))
          (*encoding* (lsp-server-encoding server)))
      (cond ((null document)
             (let ((text (region-to-string (buffer-region buffer))))
               (setf (gethash buffer (lsp-server-documents server))
                     (make-document 1 signature (and incremental text)))
               (lsp-notify server "textDocument/didOpen"
                           (json "textDocument"
                                 (json "uri" (file-uri (buffer-pathname buffer))
                                       "languageId" (or (mode-language-id (buffer-major-mode buffer))
                                                        (mode-language-id (lsp-server-mode server)))
                                       "version" 1
                                       "text" text)))))
            ((not (eql (document-signature document) signature))
             (let ((text (region-to-string (buffer-region buffer)))
                   (old (document-text document)))
               (setf (document-signature document) signature)
               ;; A signature changes for more than text: say nothing then.
               (unless (and old (string= old text))
                 (lsp-notify
                  server "textDocument/didChange"
                  (json "textDocument" (json "uri" (file-uri (buffer-pathname buffer))
                                             "version" (incf (document-version document)))
                        "contentChanges"
                        (vector
                         (if old
                             (multiple-value-bind (start-line start-character
                                                   end-line end-character inserted)
                                 (text-change old text)
                               (json "range" (json "start" (json "line" start-line
                                                                 "character" start-character)
                                                   "end" (json "line" end-line
                                                               "character" end-character))
                                     "text" inserted))
                             (json "text" text)))))
                 (setf (document-pull document) t)
                 ;; What is wrong with the server's other files may have
                 ;; changed with this one.
                 (when (jref (lsp-server-capabilities server)
                             "diagnosticProvider" "interFileDependencies")
                   (loop for other being the hash-values of (lsp-server-documents server)
                         do (setf (document-pull other) t)))
                 (when incremental
                   (setf (document-text document) text))))))
      (let ((document (gethash buffer (lsp-server-documents server))))
        (when (and document (document-pull document))
          (setf (document-pull document) nil)
          (lsp-pull-diagnostics server buffer document))))))

(defun lsp-pull-diagnostics (server buffer document)
  "Ask SERVER what is wrong in BUFFER, if it is a server that is asked (the
   protocol's pull diagnostics).  One that was busy, or whose answer the
   buffer's change overtook, is asked again at the next LSP-SYNC."
  (let ((provider (jref (lsp-server-capabilities server) "diagnosticProvider")))
    (when provider
      (let ((uri (file-uri (buffer-pathname buffer)))
            (identifier (jref provider "identifier")))
        (lsp-request-async
         server "textDocument/diagnostic"
         (if (stringp identifier)
             (json "textDocument" (json "uri" uri) "identifier" identifier)
             (json "textDocument" (json "uri" uri)))
         (lambda (result error)
           (cond (error
                  ;; ServerCancelled and ContentModified.
                  (when (member (jref error "code") '(-32802 -32801))
                    (setf (document-pull document) t)))
                 ((equal (jref result "kind") "full")
                  (lsp-note-diagnostics server uri (jlist (jref result "items")) t)))))))))

(defun lsp-buffer-closed (buffer)
  (dolist (server *lsp-servers*)
    (when (gethash buffer (lsp-server-documents server))
      (remhash buffer (lsp-server-documents server))
      (when (buffer-pathname buffer)
        (lsp-notify server "textDocument/didClose" (json "textDocument" (lsp-document buffer))))))
  (clear-buffer-diagnostics buffer))

(defun lsp-buffer-saved (buffer)
  (dolist (server *lsp-servers*)
    (when (gethash buffer (lsp-server-documents server))
      (lsp-sync server buffer)
      (lsp-notify server "textDocument/didSave" (json "textDocument" (lsp-document buffer))))))

(add-hook delete-buffer-hook 'lsp-buffer-closed)
(add-hook write-file-hook 'lsp-buffer-saved)

;;; Twice a second, the current buffer's server is started if it is not, and
;;; told of what has been typed, so that its errors keep up.

(defvar *lsp-ticks* 0)

(defvar *lsp-idle-functions* '()
  "Functions of a server and a buffer, called twice a second with the
   current buffer and its server, once it is ready and knows the buffer as
   it is.")

(defun lsp-idle (&optional elapsed)
  (declare (ignore elapsed))
  (ignore-errors (lsp-show-said))
  (incf *lsp-ticks*)
  (dolist (server *lsp-servers*)
    (when (eq (lsp-server-state server) :ready)
      (ignore-errors (lsp-send-changed-settings server))
      (when (zerop (mod *lsp-ticks* *lsp-watch-ticks*))
        (ignore-errors (lsp-look-at-files server)))))
  (ignore-errors
   (let ((buffer (current-buffer)))
     ;; Its additional servers too.
     (dolist (other (buffer-language-servers buffer t))
       (lsp-sync other buffer))
     (let ((server (buffer-language-server buffer)))
       (when server
         (when (eq (lsp-server-state server) :ready)
           (let ((*encoding* (lsp-server-encoding server)))
             (dolist (function *lsp-idle-functions*)
               (ignore-errors (funcall function server buffer)))))))
     ;; The other buffers the servers know: one changed from elsewhere is
     ;; told of, and one whose server is to be asked about again is.
     (dolist (server *lsp-servers*)
       (dolist (other (loop for other being the hash-keys of (lsp-server-documents server)
                            collect other))
         (unless (eq other buffer)
           (ignore-errors (lsp-sync server other))))))))

(defun start-lsp-idle ()
  (remove-scheduled-event 'lsp-idle)
  (schedule-event 0.5 'lsp-idle))

(add-hook entry-hook 'start-lsp-idle)

(defun lsp-current-server (&optional capability)
  "The current buffer's server, ready and knowing the buffer as it is: its
   mode's own, or with CAPABILITY, the first of its servers that has it."
  (let* ((buffer (current-buffer))
         (server (or (buffer-language-server (progn (buffer-language-servers buffer t) buffer))
                     (first (buffer-language-servers buffer))
                     (editor-error "No language server for this buffer."))))
    (unless (lsp-wait (lambda () (not (eq (lsp-server-state server) :starting))) 10)
      (editor-error "The language server has not started."))
    ;; One that could not start may have given way to another.
    (when (eq (lsp-server-state server) :dead)
      (setf server (or (buffer-language-server buffer)
                       (editor-error "The language server could not be started.")))
      (unless (lsp-wait (lambda () (not (eq (lsp-server-state server) :starting))) 10)
        (editor-error "The language server has not started."))
      (when (eq (lsp-server-state server) :dead)
        (editor-error "The language server could not be started.")))
    (lsp-sync server buffer)
    ;; Another of its servers, when this one cannot do what is wanted.
    (when (and capability (not (jref (lsp-server-capabilities server) capability)))
      (let ((other (buffer-server-with buffer capability)))
        (when other
          (setf server other)
          (lsp-sync server buffer))))
    (setf *encoding* (lsp-server-encoding server))
    server))


;;;; Errors and warnings.

(defvar *buffer-diagnostics* (make-hash-table :test 'eq :weakness :key)
  "Buffer to ((START-MARK END-MARK SEVERITY MESSAGE DIAGNOSTIC SERVER) ...):
   what its servers say is wrong in it, at marks, so that each stays with
   its text, as the server said it, and the server that did.")

(defparameter *diagnostic-fonts*
  '((1 . (:fg 1 :underline t))          ; an error
    (2 . (:fg 3 :underline t))          ; a warning
    (3 . (:underline t))                ; information
    (4 . (:underline t))))              ; a hint

(defun clear-buffer-diagnostics (buffer)
  (loop for (start end) in (gethash buffer *buffer-diagnostics*)
        do (delete-mark start) (delete-mark end))
  (remhash buffer *buffer-diagnostics*))

(defun file-buffer (file)
  (find file *buffer-list*
        :key (lambda (buffer) (let ((p (buffer-pathname buffer))) (and p (namestring p))))
        :test #'equal))

(defun lsp-note-diagnostics (server uri diagnostics &optional pulled)
  "SERVER says that DIAGNOSTICS, the protocol's, are what is wrong with the
   file at URI; or, if PULLED, answers that they are.  Each replaces what it
   last said or answered, and what is wrong is both."
  (let* ((file (uri-file (or uri "")))
         (buffer (and file (file-buffer file))))
    (when file
      (setf (gethash file (if pulled (lsp-server-pulled server) (lsp-server-pushed server)))
            diagnostics)
      (setf (gethash file (lsp-server-diagnostics server))
            (loop for diagnostic in (append (gethash file (lsp-server-pushed server))
                                            (gethash file (lsp-server-pulled server)))
                  collect (list (jref diagnostic "range" "start" "line")
                                (jref diagnostic "range" "start" "character")
                                (or (jref diagnostic "severity") 1)
                                (or (jref diagnostic "message") ""))))
      (when buffer
        (rebuild-buffer-diagnostics buffer)
        (update-lsp-modeline buffer))
      (incf hi:*decoration-tick*))))

(defun rebuild-buffer-diagnostics (buffer)
  "Make BUFFER's diagnostics again from what each of the servers running
   says of its file: a buffer may have several, and what is wrong is what
   any of them finds."
  (let ((file (let ((pathname (buffer-pathname buffer))) (and pathname (namestring pathname)))))
    (clear-buffer-diagnostics buffer)
    (when file
      (let ((all (loop for server in *lsp-servers*
                       append (let ((*encoding* (lsp-server-encoding server)))
                                (loop for diagnostic in (append (gethash file (lsp-server-pushed server))
                                                                (gethash file (lsp-server-pulled server)))
                                      collect
                                      (let ((start (copy-mark (buffer-start-mark buffer)
                                                              :right-inserting))
                                            (end (copy-mark (buffer-start-mark buffer)
                                                            :left-inserting)))
                                        (lsp-move-mark start (jref diagnostic "range" "start" "line")
                                                       (jref diagnostic "range" "start" "character"))
                                        (lsp-move-mark end (jref diagnostic "range" "end" "line")
                                                       (jref diagnostic "range" "end" "character"))
                                        (list start end
                                              (or (jref diagnostic "severity") 1)
                                              (or (jref diagnostic "message") "")
                                              diagnostic
                                              server)))))))
        (when all
          (setf (gethash buffer *buffer-diagnostics*) all))))))

;;; The modeline says what the server finds: nothing when there is no
;;; server, how many errors and warnings when there are any.

(defun buffer-diagnostic-counts (buffer)
  (let ((errors 0) (warnings 0))
    (loop for (nil nil severity) in (gethash buffer *buffer-diagnostics*)
          do (case severity (1 (incf errors)) (2 (incf warnings))))
    (values errors warnings)))

(make-modeline-field
 :name :lsp
 :function (lambda (buffer window)
             (declare (ignore window))
             (let ((server (ignore-errors (buffer-language-server buffer))))
               (cond ((null server)
                      (if (ignore-errors (buffer-server-failed-p buffer))
                          "(no server)  "
                          ""))
                     ((eq (lsp-server-state server) :starting) "(server starting)  ")
                     (t
                      (multiple-value-bind (errors warnings) (buffer-diagnostic-counts buffer)
                        (format nil "~@[(~A)  ~]~:[(~[~:;~:*~D error~:P~]~:[~; ~]~[~:;~:*~D warning~:P~])  ~;~]"
                                (lsp-progress-text server)
                                (and (zerop errors) (zerop warnings))
                                errors (and (plusp errors) (plusp warnings)) warnings)))))))

(unless (member :lsp hi::*default-modeline-fields* :key #'modeline-field-name)
  (let ((project (member :project hi::*default-modeline-fields* :key #'modeline-field-name)))
    (if project
        (push (modeline-field :lsp) (cdr project))
        (nconc hi::*default-modeline-fields* (list (modeline-field :lsp))))))

(defun update-lsp-modeline (buffer)
  (when (buffer-modeline-field-p buffer :lsp)
    (dolist (window (buffer-windows buffer))
      (ignore-errors (update-modeline-field buffer window (modeline-field :lsp))))))

(defun lsp-line-decorations (line)
  "What the server says is wrong on LINE, as ((START END FONT) ...)."
  (let ((buffer (line-buffer line)))
    (when buffer
      (loop for (start end severity) in (gethash buffer *buffer-diagnostics*)
            for start-line = (mark-line start)
            for end-line = (mark-line end)
            when (and (eq (line-buffer start-line) buffer)
                      (eq (line-buffer end-line) buffer)
                      (line<= start-line line) (line<= line end-line))
              collect (let* ((length (line-length line))
                             (from (if (eq line start-line) (mark-charpos start) 0))
                             (to (if (eq line end-line) (mark-charpos end) length)))
                        ;; A place with no width is the character there.
                        (when (<= to from) (setf to (1+ from)))
                        (list (min from (max 0 (1- length))) (min to (max length 1))
                              (or (cdr (assoc severity *diagnostic-fonts*))
                                  '(:underline t))))))))

(pushnew 'lsp-line-decorations hi:*line-decoration-functions*)

(defun diagnostic-at-mark (mark)
  "What the server says is wrong at MARK, or on its line."
  (let ((diagnostics (gethash (line-buffer (mark-line mark)) *buffer-diagnostics*)))
    (fourth (or (find-if (lambda (d) (and (mark<= (first d) mark) (mark<= mark (second d))))
                         diagnostics)
                (find (mark-line mark) diagnostics :key (lambda (d) (mark-line (first d))))))))


(defparameter *symbol-kinds*
  #(nil "file" "module" "namespace" "package" "class" "method" "property" "field"
    "constructor" "enum" "interface" "function" "variable" "constant" "string" "number"
    "boolean" "array" "object" "key" "null" "enum member" "struct" "event" "operator"
    "type parameter"))


;;;; From one error to the next.

(defun move-to-diagnostic (direction)
  (let* ((point (current-point))
         (diagnostics (sort (copy-list (gethash (current-buffer) *buffer-diagnostics*))
                            #'mark< :key #'first))
         (next (if (plusp direction)
                   (find-if (lambda (d) (mark> (first d) point)) diagnostics)
                   (find-if (lambda (d) (mark< (first d) point)) diagnostics :from-end t))))
    (unless diagnostics (editor-error "Nothing is wrong here, that the server says."))
    ;; Past the last, the first; before the first, the last.
    (unless next
      (setf next (if (plusp direction) (first diagnostics) (first (last diagnostics)))))
    (move-mark point (first next))
    (message "~A" (substitute #\Space #\Newline (fourth next)))))

(defcommand "LSP Next Diagnostic" (p)
  "Go to the next thing the language server finds wrong in this buffer,
   and say what it is; after the last, the first."
  "Go to the next error or warning."
  (declare (ignore p))
  (move-to-diagnostic 1))

(defcommand "LSP Previous Diagnostic" (p)
  "Go to the previous thing the language server finds wrong in this buffer,
   and say what it is; before the first, the last."
  "Go to the previous error or warning."
  (declare (ignore p))
  (move-to-diagnostic -1))


;;;; Places the server names.

(defun lsp-locations (result)
  "RESULT, a location or several, as ((FILE LINE CHARACTER) ...)."
  (loop for location in (if (hash-table-p result) (list result) (jlist result))
        for uri = (or (jref location "uri") (jref location "targetUri"))
        for range = (or (jref location "range") (jref location "targetSelectionRange")
                        (jref location "targetRange"))
        for file = (and uri (uri-file uri))
        when (and file range)
          collect (list file (jref range "start" "line") (jref range "start" "character"))))

(defun lsp-visit (location)
  (destructuring-bind (file line character) location
    (change-to-buffer (find-file-buffer file))
    (lsp-move-mark (current-point) line character)))

(defun file-line-text (file line)
  "The text of FILE's line LINE, from 0: from its buffer when it has one."
  (let ((buffer (file-buffer file)))
    (or (if buffer
            (with-mark ((mark (buffer-start-mark buffer)))
              (and (line-offset mark line) (line-string (mark-line mark))))
            (ignore-errors
             (with-open-file (in file :external-format :utf-8)
               (loop repeat line do (read-line in))
               (read-line in))))
        "")))

(defun list-lsp-locations (name title locations)
  "List LOCATIONS in the result buffer NAME, a line each, under TITLE."
  (let ((buffer (make-result-buffer name "Outline" 'plist-line-location))
        (root (ignore-errors (current-project-root))))
    (with-writable-buffer (buffer)
      (let ((point (buffer-point buffer)))
        (insert-string point (format nil "~A: ~D~%~%" title (length locations)))
        (loop for (file line character text) in locations
              do (let ((out (mark-line point)))
                   (insert-string point
                                  (format nil "  ~A:~D: ~A~%"
                                          (if (and root (eql 0 (search root file)))
                                              (subseq file (length root))
                                              file)
                                          (1+ line)
                                          (string-trim '(#\Space #\Tab)
                                                       (or text (file-line-text file line)))))
                   (setf (getf (line-plist out) 'result-location)
                         (list (pathname file) (1+ line) (1+ (or character 0))))))))
    (select-window (other-window))
    (change-to-buffer buffer)
    (buffer-start (current-point))
    (next-result-line (current-point) 1)))


;;;; Commands.

(defun lsp-go-to (method what)
  "Go to what the server answers METHOD with for what is at point; several
   places are listed.  WHAT says what they are, as \"definition\"."
  (let* ((server (lsp-current-server))
         (locations (lsp-locations
                     (lsp-request server method
                                  (lsp-symbol-params (current-buffer) (current-point))))))
    (cond ((null locations) (editor-error "No ~A found." what))
          ((null (rest locations))
           (push-buffer-mark (copy-mark (current-point)))
           (lsp-visit (first locations)))
          (t (list-lsp-locations (format nil "*~:(~A~)s*" what)
                                 (format nil "~:(~A~)s" what)
                                 locations)))))

(defcommand "LSP Find Definition" (p)
  "Go to the definition of what is at point, as the language server finds
   it; several are listed."
  "Go to the definition of what is at point."
  (declare (ignore p))
  (lsp-go-to "textDocument/definition" "definition"))

(defcommand "LSP Find Declaration" (p)
  "Go to the declaration of what is at point, as the language server finds
   it: in C, a function's prototype rather than its body."
  "Go to the declaration of what is at point."
  (declare (ignore p))
  (lsp-go-to "textDocument/declaration" "declaration"))

(defcommand "LSP Find Type Definition" (p)
  "Go to the definition of the type of what is at point, as the language
   server finds it."
  "Go to the definition of the type of what is at point."
  (declare (ignore p))
  (lsp-go-to "textDocument/typeDefinition" "type definition"))

(defcommand "LSP Find Implementation" (p)
  "Go to what implements the interface, or the method of one, at point, as
   the language server finds it; several are listed."
  "Go to the implementations of what is at point."
  (declare (ignore p))
  (lsp-go-to "textDocument/implementation" "implementation"))

(defcommand "LSP Find References" (p)
  "List the places that refer to what is at point, as the language server
   finds them."
  "List the references to what is at point."
  (declare (ignore p))
  (let* ((server (lsp-current-server))
         (params (lsp-symbol-params (current-buffer) (current-point))))
    (setf (gethash "context" params) (json "includeDeclaration" t))
    (let ((locations (lsp-locations (lsp-request server "textDocument/references" params
                                                 :timeout 15))))
      (unless locations (editor-error "No references found."))
      (list-lsp-locations "*References*" "References" locations))))

(defun hover-text (contents)
  (cond ((stringp contents) contents)
        ((hash-table-p contents) (or (jref contents "value") ""))
        ((vectorp contents)
         (format nil "~{~A~^~%~}" (remove "" (map 'list #'hover-text contents) :test #'string=)))
        (t "")))

(defcommand "LSP Describe" (p)
  "Show what the language server says of what is at point, in a popup under
   it until the next key: what is wrong there, when something is, and what
   it is."
  "Show what the language server says of what is at point."
  (declare (ignore p))
  (let* ((server (lsp-current-server))
         (wrong (diagnostic-at-mark (current-point)))
         (hover (hover-text
                 (jref (lsp-request server "textDocument/hover"
                                    (lsp-symbol-params (current-buffer) (current-point)))
                       "contents")))
         (text (string-trim '(#\Space #\Newline)
                            (format nil "~@[~A~%~%~]~A" wrong hover))))
    (if (zerop (length text))
        (message "Nothing is said of this.")
        (show-text-popup text))))

(defun apply-text-edits (buffer edits)
  "Make the server's EDITS to BUFFER, the last first so that the places of
   the others stand."
  (let ((edits (sort (loop for edit in edits
                           collect (list (jref edit "range" "start" "line")
                                         (jref edit "range" "start" "character")
                                         (jref edit "range" "end" "line")
                                         (jref edit "range" "end" "character")
                                         (or (jref edit "newText") "")))
                     (lambda (a b)
                       (or (> (first a) (first b))
                           (and (= (first a) (first b)) (> (second a) (second b))))))))
    (loop for (start-line start-character end-line end-character text) in edits
          do (with-mark ((start (buffer-start-mark buffer) :left-inserting)
                         (end (buffer-start-mark buffer) :left-inserting))
               (lsp-move-mark start start-line start-character)
               (lsp-move-mark end end-line end-character)
               (delete-region (region start end))
               (insert-string start (remove #\Return text))))
    (length edits)))

(defun apply-workspace-edit (edit)
  "Make the server's EDIT, changes to any number of files, in their
   buffers.  How many changes, and in how many buffers."
  (let ((edits 0) (buffers 0))
    (flet ((apply-to (uri edits-there)
             (let ((file (and uri (uri-file uri))))
               (when file
                 (incf edits (apply-text-edits (find-file-buffer file) (jlist edits-there)))
                 (incf buffers)))))
      (let ((changes (jref edit "changes")))
        (when (hash-table-p changes)
          (maphash #'apply-to changes)))
      ;; In order: a file may be made and then written in.
      (dolist (change (jlist (jref edit "documentChanges")))
        (cond ((jref change "kind")
               (apply-file-operation change))
              ((jref change "edits")
               (apply-to (jref change "textDocument" "uri") (jref change "edits"))))))
    (values edits buffers)))

(defun apply-file-operation (change)
  "Make, rename or delete a file, as a server's edit says to."
  (let* ((kind (jref change "kind"))
         (options (jref change "options"))
         (file (uri-file (or (jref change "uri") (jref change "oldUri") "")))
         (new (uri-file (or (jref change "newUri") ""))))
    (flet ((free-p (file)
             ;; Whether FILE may be written: it is not there, or the edit
             ;; says to write over it.
             (or (not (probe-file file))
                 (and (jref options "overwrite") (not (jref options "ignoreIfExists"))))))
      (cond ((null file))
            ((string= kind "create")
             (when (free-p file)
               (ensure-directories-exist file)
               (with-open-file (out file :direction :output :if-exists :supersede)
                 (declare (ignorable out)))))
            ((string= kind "rename")
             (when (and new (probe-file file) (free-p new))
               (ensure-directories-exist new)
               (rename-file file new)
               ;; A buffer on the file follows it.
               (let ((buffer (file-buffer file)))
                 (when buffer
                   (lsp-buffer-closed buffer)
                   (setf (buffer-pathname buffer) (pathname new))))))
            ((string= kind "delete")
             (when (probe-file file)
               (if (uiop:directory-pathname-p (probe-file file))
                   (when (jref options "recursive")
                     (uiop:delete-directory-tree (probe-file file) :validate t))
                   (delete-file file))))))))

(defcommand "LSP Code Action" (p)
  "Offer what the language server can do about what is at point -- a fix
   for what is wrong there, a refactoring -- in a popup, and do the one
   chosen."
  "Offer the language server's fixes for what is at point."
  (declare (ignore p))
  (let* ((server (lsp-current-server))
         (buffer (current-buffer))
         (point (current-point))
         (here (find-if (lambda (d) (and (mark<= (first d) point) (mark<= point (second d))))
                        (gethash buffer *buffer-diagnostics*)))
         (on-line (remove-if-not (lambda (d) (eq (mark-line (first d)) (mark-line point)))
                                 (gethash buffer *buffer-diagnostics*)))
         ;; About what is wrong at point, or on its line, or just point.
         (about (cond (here (list here)) (t on-line)))
         (range (lambda ()
                  ;; As the server being asked counts.
                  (if about
                      (json "start" (lsp-position (first (first about)))
                            "end" (lsp-position (second (first about))))
                      (json "start" (lsp-position point) "end" (lsp-position point)))))
         ;; Each of the buffer's servers is asked, about what it found.
         (actions
           (loop for server in (remove-if-not
                                (lambda (server)
                                  (and (eq (lsp-server-state server) :ready)
                                       (jref (lsp-server-capabilities server)
                                             "codeActionProvider")))
                                (cons server (remove server (buffer-language-servers buffer))))
                 append (let ((*encoding* (lsp-server-encoding server)))
                          (lsp-sync server buffer)
                          (mapcar (lambda (action) (cons action server))
                                  (jlist (lsp-request
                                          server "textDocument/codeAction"
                                          (json "textDocument" (lsp-document buffer)
                                                "range" (funcall range)
                                                "context"
                                                (json "diagnostics"
                                                      (map 'vector #'fifth
                                                           (remove server about
                                                                   :key #'sixth
                                                                   :test-not #'eq)))))))))))
    (unless actions (editor-error "The server has nothing to offer here."))
    (let ((choice (popup-select (mapcar (lambda (action) (or (jref (car action) "title") "?"))
                                        actions))))
      (when choice
        (let* ((server (cdr (nth choice actions)))
               (*encoding* (lsp-server-encoding server))
               (action (let ((action (car (nth choice actions))))
                         ;; One whose edit the server has yet to work out.
                         (or (and (not (jref action "edit"))
                                  (not (jref action "command"))
                                  (jref (lsp-server-capabilities server)
                                        "codeActionProvider" "resolveProvider")
                                  (let ((resolved (lsp-request server "codeAction/resolve" action
                                                               :timeout 15)))
                                    (and (hash-table-p resolved) resolved)))
                             action)))
               (edit (jref action "edit"))
               (command (jref action "command")))
          (when (hash-table-p edit)
            (apply-workspace-edit edit))
          ;; A command of the server's own, which makes its edits by asking
          ;; for them (workspace/applyEdit): the action's, or the action itself.
          (let ((command (cond ((hash-table-p command) command)
                               ((stringp command) action))))
            (when command
              (lsp-request server "workspace/executeCommand"
                           (json "command" (jref command "command")
                                 "arguments" (or (gethash "arguments" command) (vector)))
                           :timeout 15)))
          (message "~A" (or (jref action "title") "Done.")))))))

(defcommand "LSP Rename" (p)
  "Rename what is at point everywhere the language server finds it, in
   every file; the buffers changed are left to save."
  "Rename what is at point, everywhere."
  (declare (ignore p))
  (let* ((server (lsp-current-server))
         (old
           ;; A server that says what can be renamed is asked first: what is
           ;; here may be nothing with a name, or a name that is not this
           ;; project's to change.
           (or (when (jref (lsp-server-capabilities server) "renameProvider" "prepareProvider")
                 (let ((there (lsp-request server "textDocument/prepareRename"
                                           (lsp-symbol-params (current-buffer) (current-point)))))
                   (unless there (editor-error "This cannot be renamed."))
                   (or (jref there "placeholder")
                       (let ((range (or (jref there "range")
                                        (and (jref there "start") there))))
                         (when range
                           (with-mark ((start (current-point))
                                       (end (current-point)))
                             (lsp-move-mark start (jref range "start" "line")
                                            (jref range "start" "character"))
                             (lsp-move-mark end (jref range "end" "line")
                                            (jref range "end" "character"))
                             (region-to-string (region start end))))))))
               (word-at-point)))
         (name (prompt-for-string :prompt "Rename to: "
                                  :default (and old (plusp (length old)) old)))
         (params (lsp-symbol-params (current-buffer) (current-point))))
    (setf (gethash "newName" params) name)
    (let ((edit (lsp-request server "textDocument/rename" params :timeout 15)))
      (unless (hash-table-p edit) (editor-error "The server would not rename this."))
      (multiple-value-bind (edits buffers) (apply-workspace-edit edit)
        (message "Renamed in ~D place~:P, in ~D buffer~:P." edits buffers)))))

(defcommand "LSP Format Buffer" (p)
  "Lay this buffer out as the language server's formatter does."
  "Format this buffer with the language server."
  (declare (ignore p))
  (let* ((server (lsp-current-server "documentFormattingProvider"))
         (edits (lsp-request server "textDocument/formatting"
                             (json "textDocument" (lsp-document (current-buffer))
                                   "options" (json "tabSize" 4 "insertSpaces" t))
                             :timeout 15)))
    (if (plusp (length edits))
        (message "~D change~:P." (apply-text-edits (current-buffer) (jlist edits)))
        (message "Nothing to change."))))

(defcommand "LSP Find Symbol" (p)
  "List the symbols of this project whose names have some text in them, as
   the language server finds them, with their kinds."
  "List the project's symbols matching some text."
  (declare (ignore p))
  (let* ((server (lsp-current-server))
         (query (prompt-for-string :prompt "Symbols matching: "
                                   :default (let ((word (word-at-point)))
                                              (and (plusp (length word)) word))))
         (symbols (jlist (lsp-request server "workspace/symbol" (json "query" query)
                                      :timeout 15)))
         (locations
           (loop for symbol in symbols
                 for file = (let ((uri (jref symbol "location" "uri"))) (and uri (uri-file uri)))
                 for kind = (jref symbol "kind")
                 when file
                   collect (list file
                                 (or (jref symbol "location" "range" "start" "line") 0)
                                 (or (jref symbol "location" "range" "start" "character") 0)
                                 (format nil "~@[~A ~]~A~@[  in ~A~]"
                                         (and (integerp kind) (< 0 kind (length *symbol-kinds*))
                                              (aref *symbol-kinds* kind))
                                         (jref symbol "name")
                                         (let ((container (jref symbol "containerName")))
                                           (and container (plusp (length container)) container)))))))
    (unless locations (editor-error "No symbol matches ~A." query))
    (list-lsp-locations "*Symbols*" (format nil "Symbols matching ~S" query) locations)))

;;; Formatting as a file is saved.

(defhvar "LSP Format on Save"
  "When true, a buffer with a language server that formats is laid out by
   it each time it is saved."
  :value nil)

(defun lsp-format-before-saving (buffer)
  (when (value lsp-format-on-save)
    (ignore-errors
     (let ((server (buffer-server-with buffer "documentFormattingProvider")))
       (when server
         (setf *encoding* (lsp-server-encoding server))
         (lsp-sync server buffer)
         (let ((edits (lsp-request server "textDocument/formatting"
                                   (json "textDocument" (lsp-document buffer)
                                         "options" (json "tabSize" 4 "insertSpaces" t))
                                   :timeout 5)))
           (when (plusp (length edits))
             (apply-text-edits buffer (jlist edits)))))))))

(add-hook before-write-file-hook 'lsp-format-before-saving)

(defcommand "LSP Diagnostics" (p)
  "List what the language server finds wrong, in this buffer's project."
  "List what the language server finds wrong."
  (declare (ignore p))
  (let* ((first (lsp-current-server))
         (locations
           (loop for server in (cons first (remove first (buffer-language-servers
                                                          (current-buffer))))
                 append
                 (loop for file being the hash-keys of (lsp-server-diagnostics server)
                   using (hash-value diagnostics)
                 append (loop for (line character severity message) in diagnostics
                              collect (list file line character
                                            (format nil "~A: ~A"
                                                    (case severity
                                                      (1 "error") (2 "warning") (t "note"))
                                                    (substitute #\Space #\Newline message))))))))
    (unless locations (editor-error "Nothing is wrong, that the server says."))
    (list-lsp-locations "*Diagnostics*" "Errors and warnings"
                        (sort locations (lambda (a b)
                                          (or (string< (first a) (first b))
                                              (and (string= (first a) (first b))
                                                   (< (second a) (second b)))))))))

(defcommand "LSP Restart" (p)
  "Stop this buffer's language server; it is started again a moment later."
  "Restart this buffer's language server."
  (declare (ignore p))
  (let* ((buffer (current-buffer))
         (pathname (or (buffer-pathname buffer)
                       (editor-error "No language server for this buffer.")))
         (group (language-server-group (buffer-major-mode buffer)))
         (root (or (buffer-project-root buffer) (directory-namestring pathname)))
         (servers (buffer-language-servers buffer))
         (groups (mode-server-groups (buffer-major-mode buffer))))
    (declare (ignorable group))
    (unless (or servers (some (lambda (group) (lsp-failed-p group root)) groups))
      (editor-error "No language server for this buffer."))
    ;; One that could not be started, or kept dying, is tried again.
    (dolist (group groups)
      (setf *lsp-failures* (remove (list group root) *lsp-failures* :test #'equal))
      (setf *lsp-crashes* (remove (list group root) *lsp-crashes* :key #'car :test #'equal)))
    (dolist (server servers)
      (stop-language-server server))
    (message "The language server is starting again.")))


;;;; The server's outline of a buffer.

(defun lsp-outline-entries (buffer)
  "BUFFER's symbols as its language server names them, as ((LINE DEPTH
   TEXT) ...), each with its kind; NIL when there is no server ready that
   gives them."
  (let ((server (buffer-language-server buffer)))
    (when (and server (eq (lsp-server-state server) :ready)
               (jref (lsp-server-capabilities server) "documentSymbolProvider"))
      (lsp-sync server buffer)
      (let ((symbols (jlist (lsp-request server "textDocument/documentSymbol"
                                         (json "textDocument" (lsp-document buffer))
                                         :timeout 3)))
            (lines (coerce (loop for line = (mark-line (buffer-start-mark buffer))
                                   then (line-next line)
                                 while line collect line)
                           'vector))
            (entries '()))
        (labels ((walk (symbol depth)
                   (let* ((number (or (jref symbol "selectionRange" "start" "line")
                                      (jref symbol "range" "start" "line")
                                      (jref symbol "location" "range" "start" "line")))
                          (kind (jref symbol "kind"))
                          (name (or (jref symbol "name") "")))
                     (when (and number (< -1 number (length lines)))
                       (push (list (aref lines number) depth
                                   (format nil "~@[~A ~]~A"
                                           (and (integerp kind) (< 0 kind (length *symbol-kinds*))
                                                (aref *symbol-kinds* kind))
                                           name))
                             entries))
                     (dolist (child (jlist (jref symbol "children")))
                       (walk child (1+ depth))))))
          (dolist (symbol symbols)
            (walk symbol 0)))
        (nreverse entries)))))

(pushnew 'lsp-outline-entries *outline-functions*)


;;;; The signature of the call being typed.

(defun lsp-signature (point)
  "Ask the server what the call POINT is in takes, and show it over the
   call when the answer comes, the argument being typed marked."
  (let* ((buffer (line-buffer (mark-line point)))
         (server (buffer-language-server buffer))
         (start (call-start point)))
    (when (and server start (eq (lsp-server-state server) :ready)
               (jref (lsp-server-capabilities server) "signatureHelpProvider")
               (not (eql (previous-character point) #\Space)))
      (lsp-sync server buffer)
      (let ((anchor (copy-mark start :right-inserting))
            (tick (incf *signature-tick*)))
        (lsp-request-async
         server "textDocument/signatureHelp" (lsp-position-params buffer point)
         (lambda (result error)
           (declare (ignore error))
           (unwind-protect
                (let* ((signatures (jlist (jref result "signatures")))
                       (signature (and signatures
                                       (nth (min (or (jref result "activeSignature") 0)
                                                 (1- (length signatures)))
                                            signatures))))
                  ;; Not if the call has been closed, or another asked
                  ;; about, since this was asked.
                  (when (and signature (= tick *signature-tick*)
                             (eq (current-buffer) buffer)
                             (eq (mark-line anchor) (mark-line (current-point))))
                    (let* ((label (or (jref signature "label") ""))
                           (parameters (jlist (jref signature "parameters")))
                           (active (or (jref signature "activeParameter")
                                       (jref result "activeParameter") 0))
                           (parameter (and (integerp active) (nth active parameters)))
                           (name (and parameter (gethash "label" parameter))))
                      (multiple-value-bind (from to)
                          ;; A parameter is named by its text, or by where
                          ;; it is in the label.
                          (cond ((stringp name)
                                 (let ((at (search name label)))
                                   (and at (values at (+ at (length name))))))
                                ((vectorp name)
                                 (values (utf16-charpos label (aref name 0))
                                         (utf16-charpos label (aref name 1)))))
                        (show-signature anchor label from to)))))
             (delete-mark anchor))))))))


;;;; Completions, for the popup.

(defparameter *completion-kinds*
  #(nil "text" "method" "function" "constructor" "field" "variable" "class" "interface"
    "module" "property" "unit" "value" "enum" "keyword" "snippet" "color" "file"
    "reference" "folder" "enum member" "constant" "struct" "event" "operator" "type"))

(defvar *lsp-completion-items* '()
  "((TEXT ITEM SERVER) ...): the completions last offered, as the server
   gave them, by the text each is known by in the popup.")

(defun completion-item-text (item)
  "What ITEM, a server's completion, is known by: what is typed to choose
   it, without what a snippet would add."
  (let* ((insert (or (jref item "textEdit" "newText") (jref item "insertText")))
         (text (string-trim " " (or (jref item "filterText")
                                    (and insert (not (find #\$ insert)) insert)
                                    (jref item "label")
                                    ""))))
    ;; A label may be a whole signature.
    (subseq text 0 (or (position-if (lambda (char) (find char "( <")) text) (length text)))))

(defun lsp-completions (point)
  "The language server's completions of the word before POINT, each with
   its kind and what the server says it is, and where the word starts; the
   buffers' words when the server has none to give."
  (let* ((buffer (line-buffer (mark-line point)))
         (server (ignore-errors (buffer-language-server buffer)))
         (items (when (and server (eq (lsp-server-state server) :ready))
                  (let ((*encoding* (lsp-server-encoding server)))
                    (lsp-sync server buffer)
                    (let ((result (lsp-request server "textDocument/completion"
                                               (lsp-position-params buffer point)
                                               :timeout 2)))
                      (jlist (if (hash-table-p result) (jref result "items") result))))))
         (start (token-start point #'word-char-p))
         (typed (region-to-string (region start point)))
         (found '())
         (candidates
           (remove-duplicates
            (loop for item in items
                  for text = (completion-item-text item)
                  for kind = (jref item "kind")
                  for detail = (jref item "detail")
                  when (and (plusp (length text))
                            (>= (length text) (length typed))
                            (string-equal typed text :end2 (length typed))
                            (string/= text typed))
                    collect (progn
                              (push (list text item server) found)
                              (cons text
                                    (let ((kind (and (integerp kind)
                                                     (< 0 kind (length *completion-kinds*))
                                                     (aref *completion-kinds* kind)))
                                          (detail (and (stringp detail)
                                                       (substitute #\Space #\Newline detail))))
                                      (when (or kind detail)
                                        (let ((note (format nil "~@[~A~]~:[~;  ~]~@[~A~]"
                                                            kind (and kind detail) detail)))
                                          (subseq note 0 (min 44 (length note)))))))))
            :key #'car :test #'string= :from-end t)))
    (cond (candidates
           (setf *lsp-completion-items* (nreverse found))
           (values (subseq candidates 0 (min 200 (length candidates))) start))
          (t
           (setf *lsp-completion-items* '())
           (delete-mark start)
           (word-completions point)))))

(defun resolve-completion-item (entry)
  "The item of ENTRY, one of *LSP-COMPLETION-ITEMS*, with what its server
   left to be asked for: what it is, and what else it changes."
  (destructuring-bind (text item server) entry
    (declare (ignore text))
    (when (and (jref (lsp-server-capabilities server) "completionProvider" "resolveProvider")
               (not (gethash "heml-resolved" item)))
      (setf (gethash "heml-resolved" item) t)
      (let ((resolved (let ((*encoding* (lsp-server-encoding server)))
                        (lsp-request server "completionItem/resolve"
                                     (let ((asked (make-hash-table :test 'equal)))
                                       (maphash (lambda (key value)
                                                  (unless (string= key "heml-resolved")
                                                    (setf (gethash key asked) value)))
                                                item)
                                       asked)
                                     :timeout 1))))
        (when (hash-table-p resolved)
          (maphash (lambda (key value) (setf (gethash key item) value)) resolved))))
    item))

(defun lsp-describe-completion (text)
  "What the server says of the completion known by TEXT."
  (let ((entry (assoc text *lsp-completion-items* :test #'string=)))
    (when entry
      (let* ((item (resolve-completion-item entry))
             (detail (jref item "detail"))
             (documentation (hover-text (jref item "documentation"))))
        (string-trim '(#\Space #\Newline)
                     (format nil "~@[~A~%~%~]~A" (and (stringp detail) detail) documentation))))))

(defun lsp-accept-completion (text start point)
  "Put in the completion known by TEXT, in place of what is between START
   and POINT: its own text, a snippet's with its places to fill in, and
   whatever else the server says it needs -- the line that imports it."
  (let ((entry (assoc text *lsp-completion-items* :test #'string=)))
    (cond ((null entry)
           (delete-region (region start point))
           (insert-string point text))
          (t
           (let* ((item (resolve-completion-item entry))
                  (server (third entry))
                  (buffer (line-buffer (mark-line point)))
                  (*encoding* (lsp-server-encoding server))
                  (edit (jref item "textEdit"))
                  (new (or (jref edit "newText") (jref item "insertText") (jref item "label")
                           text))
                  (range (or (jref edit "range") (jref edit "insert"))))
             ;; What else it changes, first: the places are the server's,
             ;; of the text as it was, and these are other lines.
             (let ((others (jlist (jref item "additionalTextEdits"))))
               (when others
                 (apply-text-edits buffer others)))
             ;; Its edit may start before the word: at a dot, say.
             (when (and range (eql (jref range "start" "line")
                                   (1- (count-lines (region (buffer-start-mark buffer) start)))))
               (let ((string (line-string (mark-line start))))
                 (line-offset start 0 (min (mark-charpos start)
                                           (unit-charpos string
                                                         (jref range "start" "character"))))))
             (delete-region (region start point))
             (if (eql (jref item "insertTextFormat") 2)
                 (insert-snippet point (remove #\Return new))
                 (insert-string point (remove #\Return new))))))))
