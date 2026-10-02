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
  "((MODE COMMANDS LANGUAGE-ID) ...): for a major mode, the commands that
   run a server for it, the first whose program is installed being used,
   and what the protocol calls the language.")

(defhvar "Language Servers"
  "When true, a language server is started for a file whose mode has one
   installed."
  :value t)

(defstruct (lsp-server (:constructor %make-lsp-server))
  mode                                  ; the major mode it serves
  root                                  ; the directory it was started in
  language-id
  connection
  (state :starting)                     ; :STARTING, :READY or :DEAD
  (next-id 0)
  (pending (make-hash-table))           ; request id to the function its answer goes to
  (input (make-array 4096 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
  capabilities
  (documents (make-hash-table :test 'eq)) ; buffer to a DOCUMENT
  (diagnostics (make-hash-table :test 'equal))) ; file to ((LINE COLUMN SEVERITY MESSAGE) ...)

(defvar *lsp-servers* '()
  "The servers running, or starting.")

(defun find-language-server-command (mode)
  "The command to run MODE's language server, and its language's name; NIL
   when none is installed."
  (let ((entry (assoc mode *language-servers* :test #'string=)))
    (when entry
      (let ((command (find-if (lambda (command) (find-program (first command)))
                              (second entry))))
        (when command (values command (third entry)))))))


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
        (ignore-errors
         (connection-write (concatenate '(simple-array (unsigned-byte 8) (*)) header body)
                           connection))))))

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
  (let ((done nil) (answer nil))
    (lsp-request-async server method params
                       (lambda (result error)
                         (declare (ignore error))
                         (setf answer result done t)))
    (lsp-wait (lambda () (or done (eq (lsp-server-state server) :dead))) timeout)
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
        (method (jref message "method")))
    (cond ((and method id)
           ;; The server asks something.  An edit it wants made is made; a
           ;; configuration it asks for is a null for each thing asked; and
           ;; anything else is answered with null.
           (lsp-send server
                     (json "jsonrpc" "2.0" "id" id
                           "result"
                           (cond ((string= method "workspace/applyEdit")
                                  (json "applied"
                                        (and (ignore-errors
                                              (apply-workspace-edit (jref message "params" "edit"))
                                              t)
                                             t)))
                                 ((string= method "workspace/configuration")
                                  (map 'vector (constantly 'null)
                                       (jlist (jref message "params" "items"))))
                                 (t 'null)))))
          (method
           (when (string= method "textDocument/publishDiagnostics")
             (lsp-note-diagnostics server (jref message "params"))))
          (id
           (let ((function (gethash id (lsp-server-pending server))))
             (when function
               (remhash id (lsp-server-pending server))
               (funcall function (gethash "result" message) (jref message "error"))))))))


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

;;; A position is a line and a character, both from 0, the character counted
;;; in UTF-16 code units: a character past #xFFFF is two.

(defun utf16-offset (string charpos)
  (loop for i below (min charpos (length string))
        sum (if (> (char-code (char string i)) #xFFFF) 2 1)))

(defun utf16-charpos (string offset)
  (let ((units 0))
    (dotimes (i (length string) (length string))
      (when (>= units offset) (return i))
      (incf units (if (> (char-code (char string i)) #xFFFF) 2 1)))))

(defun lsp-position (mark)
  (json "line" (1- (count-lines (region (buffer-start-mark (line-buffer (mark-line mark))) mark)))
        "character" (utf16-offset (line-string (mark-line mark)) (mark-charpos mark))))

(defun lsp-move-mark (mark line character)
  "Move MARK, in its buffer, to the protocol's position LINE and CHARACTER."
  (buffer-start mark)
  (unless (line-offset mark line)
    (buffer-end mark))
  (let ((string (line-string (mark-line mark))))
    (character-offset mark (utf16-charpos string character)))
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

(defun start-language-server (mode root)
  (multiple-value-bind (command language-id) (find-language-server-command mode)
    (when command
      (let ((server (%make-lsp-server :mode mode :root root :language-id language-id)))
        (setf (lsp-server-connection server)
              (make-process-connection
               ;; What the server says on its error output is no one's to read.
               (list "/bin/sh" "-c" (format nil "exec ~{~A~^ ~} 2>/dev/null"
                                            (mapcar #'shell-quote command)))
               :directory root
               :filter (lambda (connection bytes)
                         (declare (ignore connection))
                         (ignore-errors (lsp-receive server bytes))
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
               (json "textDocument"
                     (json "synchronization" (json "didSave" t)
                           "publishDiagnostics" (json)
                           "hover" (json "contentFormat" (vector "plaintext" "markdown"))
                           "completion" (json "completionItem" (json "snippetSupport" nil))
                           "definition" (json)
                           "references" (json)
                           "rename" (json)
                           "formatting" (json)
                           "documentSymbol" (json "hierarchicalDocumentSymbolSupport" t)
                           "signatureHelp"
                           (json "signatureInformation"
                                 (json "parameterInformation" (json "labelOffsetSupport" t)
                                       "activeParameterSupport" t))
                           "codeAction"
                           (json "codeActionLiteralSupport"
                                 (json "codeActionKind"
                                       (json "valueSet"
                                             (vector "quickfix" "refactor" "refactor.extract"
                                                     "refactor.inline" "refactor.rewrite"
                                                     "source" "source.organizeImports")))))
                     "workspace" (json "workspaceFolders" t "configuration" t
                                       "applyEdit" t "symbol" (json))))
         (lambda (result error)
           (cond ((or error (not (hash-table-p result)))
                  (lsp-server-died server))
                 (t
                  (setf (lsp-server-capabilities server) (jref result "capabilities"))
                  (lsp-notify server "initialized" (json))
                  (setf (lsp-server-state server) :ready)))))
        server))))

(defun lsp-server-died (server)
  (unless (eq (lsp-server-state server) :dead)
    (setf (lsp-server-state server) :dead)
    (setf *lsp-servers* (remove server *lsp-servers*))
    (loop for buffer being the hash-keys of (lsp-server-documents server)
          do (clear-buffer-diagnostics buffer))
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
  (lsp-server-died server))

(defun buffer-language-server (buffer &optional start)
  "The language server for BUFFER, started if START and it can be."
  (let ((pathname (buffer-pathname buffer))
        (mode (buffer-major-mode buffer)))
    (when (and pathname (value language-servers)
               (assoc mode *language-servers* :test #'string=))
      (let ((root (or (buffer-project-root buffer) (directory-namestring pathname))))
        (or (find-if (lambda (server)
                       (and (string= (lsp-server-mode server) mode)
                            (string= (lsp-server-root server) root)))
                     *lsp-servers*)
            (and start (start-language-server mode root)))))))

(defun stop-language-servers ()
  (dolist (server (copy-list *lsp-servers*))
    (ignore-errors (stop-language-server server))))

(add-hook exit-hook 'stop-language-servers)


;;;; Keeping the server's copy of a buffer up to date.

;;; What the server has of a buffer: the version last sent, the buffer's
;;; signature then, and, for a server that takes changes, the text, so that
;;; the next change can be told as a span of it.
;;;
(defstruct (document (:constructor make-document (version signature text)))
  version signature text)

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
            (loop for i from line-start below index
                  sum (if (> (char-code (char text i)) #xFFFF) 2 1)))))

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
          (incremental (incremental-sync-p server)))
      (cond ((null document)
             (let ((text (region-to-string (buffer-region buffer))))
               (setf (gethash buffer (lsp-server-documents server))
                     (make-document 1 signature (and incremental text)))
               (lsp-notify server "textDocument/didOpen"
                           (json "textDocument"
                                 (json "uri" (file-uri (buffer-pathname buffer))
                                       "languageId" (lsp-server-language-id server)
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
                 (when incremental
                   (setf (document-text document) text)))))))))

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

(defun lsp-idle (&optional elapsed)
  (declare (ignore elapsed))
  (ignore-errors
   (let ((buffer (current-buffer)))
     (let ((server (buffer-language-server buffer t)))
       (when server
         (lsp-sync server buffer))))))

(defun start-lsp-idle ()
  (remove-scheduled-event 'lsp-idle)
  (schedule-event 0.5 'lsp-idle))

(add-hook entry-hook 'start-lsp-idle)

(defun lsp-current-server ()
  "The current buffer's server, ready and knowing the buffer as it is."
  (let* ((buffer (current-buffer))
         (server (or (buffer-language-server buffer t)
                     (editor-error "No language server for this buffer."))))
    (unless (lsp-wait (lambda () (not (eq (lsp-server-state server) :starting))) 10)
      (editor-error "The language server has not started."))
    (when (eq (lsp-server-state server) :dead)
      (editor-error "The language server could not be started."))
    (lsp-sync server buffer)
    server))


;;;; Errors and warnings.

(defvar *buffer-diagnostics* (make-hash-table :test 'eq :weakness :key)
  "Buffer to ((START-MARK END-MARK SEVERITY MESSAGE DIAGNOSTIC) ...): what
   its server says is wrong in it, at marks, so that each stays with its
   text, and as the server said it.")

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

(defun lsp-note-diagnostics (server params)
  (let* ((file (uri-file (or (jref params "uri") "")))
         (diagnostics (jlist (jref params "diagnostics")))
         (buffer (and file (file-buffer file))))
    (when file
      (setf (gethash file (lsp-server-diagnostics server))
            (loop for diagnostic in diagnostics
                  collect (list (jref diagnostic "range" "start" "line")
                                (jref diagnostic "range" "start" "character")
                                (or (jref diagnostic "severity") 1)
                                (or (jref diagnostic "message") ""))))
      (when buffer
        (clear-buffer-diagnostics buffer)
        (setf (gethash buffer *buffer-diagnostics*)
              (loop for diagnostic in diagnostics
                    collect (let ((start (copy-mark (buffer-start-mark buffer) :right-inserting))
                                  (end (copy-mark (buffer-start-mark buffer) :left-inserting)))
                              (lsp-move-mark start (jref diagnostic "range" "start" "line")
                                             (jref diagnostic "range" "start" "character"))
                              (lsp-move-mark end (jref diagnostic "range" "end" "line")
                                             (jref diagnostic "range" "end" "character"))
                              (list start end
                                    (or (jref diagnostic "severity") 1)
                                    (or (jref diagnostic "message") "")
                                    diagnostic)))))
      (when buffer (update-lsp-modeline buffer))
      (incf hi:*decoration-tick*))))

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
               (cond ((null server) "")
                     ((eq (lsp-server-state server) :starting) "(server starting)  ")
                     (t
                      (multiple-value-bind (errors warnings) (buffer-diagnostic-counts buffer)
                        (if (and (zerop errors) (zerop warnings))
                            ""
                            (format nil "(~[~:;~:*~D error~:P~]~:[~; ~]~[~:;~:*~D warning~:P~])  "
                                    errors (and (plusp errors) (plusp warnings)) warnings))))))))

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

(defcommand "LSP Find Definition" (p)
  "Go to the definition of what is at point, as the language server finds
   it; several are listed."
  "Go to the definition of what is at point."
  (declare (ignore p))
  (let* ((server (lsp-current-server))
         (locations (lsp-locations
                     (lsp-request server "textDocument/definition"
                                  (lsp-symbol-params (current-buffer) (current-point))))))
    (cond ((null locations) (editor-error "No definition found."))
          ((null (rest locations))
           (push-buffer-mark (copy-mark (current-point)))
           (lsp-visit (first locations)))
          (t (list-lsp-locations "*Definitions*" "Definitions" locations)))))

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
      ;; A change that makes, renames or deletes a file has no edits.
      (dolist (change (jlist (jref edit "documentChanges")))
        (when (jref change "edits")
          (apply-to (jref change "textDocument" "uri") (jref change "edits")))))
    (values edits buffers)))

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
         (range (if about
                    (json "start" (lsp-position (first (first about)))
                          "end" (lsp-position (second (first about))))
                    (json "start" (lsp-position point) "end" (lsp-position point))))
         (actions (jlist (lsp-request
                          server "textDocument/codeAction"
                          (json "textDocument" (lsp-document buffer)
                                "range" range
                                "context" (json "diagnostics"
                                                (map 'vector #'fifth about)))))))
    (unless actions (editor-error "The server has nothing to offer here."))
    (let ((choice (popup-select (mapcar (lambda (action) (or (jref action "title") "?")) actions))))
      (when choice
        (let* ((action (nth choice actions))
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
         (name (prompt-for-string :prompt "Rename to: "
                                  :default (let ((word (word-at-point)))
                                             (and (plusp (length word)) word))))
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
  (let* ((server (lsp-current-server))
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
     (let ((server (buffer-language-server buffer)))
       (when (and server (eq (lsp-server-state server) :ready)
                  (jref (lsp-server-capabilities server) "documentFormattingProvider"))
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
  (let* ((server (lsp-current-server))
         (locations
           (loop for file being the hash-keys of (lsp-server-diagnostics server)
                   using (hash-value diagnostics)
                 append (loop for (line character severity message) in diagnostics
                              collect (list file line character
                                            (format nil "~A: ~A"
                                                    (case severity
                                                      (1 "error") (2 "warning") (t "note"))
                                                    (substitute #\Space #\Newline message)))))))
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
  (let ((server (or (buffer-language-server (current-buffer))
                    (editor-error "No language server for this buffer."))))
    (stop-language-server server)
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

(defun lsp-completions (point)
  "The language server's completions of the word before POINT, each with
   its kind, and where the word starts; the buffers' words when the server
   has none to give."
  (let* ((buffer (line-buffer (mark-line point)))
         (server (ignore-errors (buffer-language-server buffer)))
         (items (when (and server (eq (lsp-server-state server) :ready))
                  (lsp-sync server buffer)
                  (let ((result (lsp-request server "textDocument/completion"
                                             (lsp-position-params buffer point)
                                             :timeout 2)))
                    (jlist (if (hash-table-p result) (jref result "items") result)))))
         (start (token-start point #'word-char-p))
         (typed (region-to-string (region start point)))
         (candidates
           (remove-duplicates
            (loop for item in items
                  for text = (let ((insert (or (jref item "textEdit" "newText")
                                               (jref item "insertText"))))
                               (string-trim " " (if (and insert (not (find #\$ insert)))
                                                    insert
                                                    (or (jref item "label") ""))))
                  for kind = (jref item "kind")
                  when (and (plusp (length text))
                            (>= (length text) (length typed))
                            (string-equal typed text :end2 (length typed))
                            (string/= text typed))
                    collect (cons text (and (integerp kind) (< 0 kind (length *completion-kinds*))
                                            (aref *completion-kinds* kind))))
            :key #'car :test #'string= :from-end t)))
    (cond (candidates
           (values (subseq candidates 0 (min 200 (length candidates))) start))
          (t
           (delete-mark start)
           (word-completions point)))))


;;;; Naming a mode's server.

(defun define-language-server (mode commands &key language-id)
  "MODE's files are served by a language server: COMMANDS is the command
   lines that run one, a list of a program and its arguments each, the first
   whose program is installed being used; LANGUAGE-ID is the protocol's name
   for the language.  M-. goes to a definition there, M-? lists references,
   C-c C-d describes, C-c C-a offers fixes, C-c C-s finds a symbol in the
   project, M-n and M-p go to the next and previous error, a call's
   signature is shown as it is typed, and completions come from the server."
  (setf *language-servers*
        (cons (list mode commands (or language-id (string-downcase mode)))
              (remove mode *language-servers* :key #'car :test #'string=)))
  (defhvar "Completions Function"
    "A function of a mark, point, that returns the completions of what is
     before it and a mark where what they complete starts."
    :mode mode :value 'lsp-completions)
  (bind-key "LSP Find Definition" #k"meta-." :mode mode)
  (bind-key "LSP Find References" #k"meta-?" :mode mode)
  (bind-key "LSP Describe" #k"control-c control-d" :mode mode)
  (bind-key "LSP Code Action" #k"control-c control-a" :mode mode)
  (bind-key "LSP Find Symbol" #k"control-c control-s" :mode mode)
  (bind-key "LSP Next Diagnostic" #k"meta-n" :mode mode)
  (bind-key "LSP Previous Diagnostic" #k"meta-p" :mode mode)
  (defhvar "Signature Function"
    "A function of a mark, point, that shows the signature of the call point
     is in with SHOW-SIGNATURE, or does nothing."
    :mode mode :value 'lsp-signature)
  (when (find-menu mode)
    (add-menu-item mode :separator)
    (dolist (entry '(("Go to Definition" "LSP Find Definition")
                     ("Find References" "LSP Find References")
                     ("Describe" "LSP Describe")
                     ("Fix or Refactor…" "LSP Code Action")
                     ("Find Symbol…" "LSP Find Symbol")
                     ("Next Error" "LSP Next Diagnostic")
                     ("Previous Error" "LSP Previous Diagnostic")
                     ("Rename…" "LSP Rename")
                     ("Format Buffer" "LSP Format Buffer")
                     ("Errors and Warnings" "LSP Diagnostics")))
      (add-menu-item mode entry)))
  mode)

(define-language-server "C" '(("clangd")) :language-id "c")
(define-language-server "Python" '(("pyright-langserver" "--stdio")
                                   ("basedpyright-langserver" "--stdio")
                                   ("pylsp")
                                   ("jedi-language-server"))
  :language-id "python")
(define-language-server "Shell Script" '(("bash-language-server" "start"))
  :language-id "shellscript")
(define-language-server "Pascal" '(("pasls")) :language-id "pascal")
(define-language-server "Rust" '(("rust-analyzer")) :language-id "rust")
(define-language-server "Go" '(("gopls")) :language-id "go")
(define-language-server "JavaScript" '(("typescript-language-server" "--stdio"))
  :language-id "javascript")
(define-language-server "TS" '(("typescript-language-server" "--stdio"))
  :language-id "typescript")
(define-language-server "TSX" '(("typescript-language-server" "--stdio"))
  :language-id "typescriptreact")
(define-language-server "JSON" '(("vscode-json-language-server" "--stdio")
                                 ("vscode-json-languageserver" "--stdio"))
  :language-id "json")
(define-language-server "YAML" '(("yaml-language-server" "--stdio")) :language-id "yaml")
