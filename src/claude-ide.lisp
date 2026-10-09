;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Heml as Claude Code's IDE.  Claude Code, run in a terminal, connects to
;;; the editor it was started from as it does to VS Code's and JetBrains'
;;; extensions: the editor serves MCP over a WebSocket on localhost and
;;; says where in a lock file, ~/.claude/ide/<port>.lock; Claude Code reads
;;; the selection, the open files and the diagnostics through the server's
;;; tools, and shows the edits it proposes as diffs in the editor, which the
;;; user accepts or rejects there.
;;;
;;; Claude Code documents the lock file's place, the transport, the
;;; X-Claude-Code-Ide-Authorization header and getDiagnostics; the rest --
;;; the lock file's fields, the other tools' arguments and answers, the
;;; notifications -- is taken from two Emacs packages that implement it,
;;; claude-code-ide.el (manzaltu) and monet.el (stevemolitor):
;;;
;;;   lock file  {pid, workspaceFolders, ideName, transport: "ws", authToken}
;;;   terminal   CLAUDE_CODE_SSE_PORT=<port>, ENABLE_IDE_INTEGRATION=true
;;;   protocol   MCP 2024-11-05, WebSocket subprotocol "mcp"
;;;   tools      answer {content: [{type: "text", text}]}, text often JSON
;;;   openDiff   answered when the user decides: FILE_SAVED and the final
;;;              text (Claude Code writes the file), or DIFF_REJECTED
;;;   notified   selection_changed {text, filePath, fileUrl, selection},
;;;              at_mentioned {filePath, lineStart, lineEnd}, lines from 0
;;;              as claude-code-ide.el sends them (monet.el counts from 1)
;;;
;;; Claude Code 2.1.294, run in a Heml terminal, connected so: the token in
;;; the header, MCP 2025-11-25, ide_connected, tools/list; /ide shows Heml
;;; connected, and a selection shows in its prompt.

(in-package :heml)

(defhvar "Claude IDE Server"
  "Whether the Claude terminal starts the server through which Claude Code
   uses Heml as its IDE."
  :value t)

(defvar *ide-listener* nil "The listening connection, while there is one.")
(defvar *ide-port* nil)
(defvar *ide-token* nil "What Claude Code must say it has, from the lock file.")
(defvar *ide-clients* '() "The connected clients.")
(defvar *ide-roots* '() "The workspace folders the lock file names.")
(defstruct ide-diff
  "A change Claude Code proposed with openDiff, shown in BUFFER, waiting
   for the user: the request ID of CLIENT is answered when they decide."
  tab client id path contents buffer)

(defvar *ide-diffs* '() "The proposed changes shown.")

(defvar *ide-lock-directories* '()
  "Each Claude configuration directory a Claude was started with, whose ide/
   holds a lock file for this server: projects may use different ones.")


;;;; SHA-1 (RFC 3174) and base64, for the WebSocket handshake: nothing Heml
;;;; loads has them.

(defun rotl32 (x n)
  (logand #xFFFFFFFF (logior (ash x n) (ash x (- n 32)))))

(defun sha1 (message)
  "The SHA-1 digest of the octets MESSAGE, as 20 octets."
  (let* ((length (length message))
         (padded (* 64 (ceiling (+ length 9) 64)))
         (data (make-array padded :element-type '(unsigned-byte 8) :initial-element 0))
         (w (make-array 80 :element-type '(unsigned-byte 32)))
         (h0 #x67452301) (h1 #xEFCDAB89) (h2 #x98BADCFE) (h3 #x10325476) (h4 #xC3D2E1F0))
    (replace data message)
    (setf (aref data length) #x80)
    (loop for i below 8
          do (setf (aref data (- padded 1 i)) (ldb (byte 8 (* 8 i)) (* 8 length))))
    (loop for chunk from 0 below padded by 64
          do (dotimes (i 16)
               (setf (aref w i) (logior (ash (aref data (+ chunk (* 4 i))) 24)
                                        (ash (aref data (+ chunk (* 4 i) 1)) 16)
                                        (ash (aref data (+ chunk (* 4 i) 2)) 8)
                                        (aref data (+ chunk (* 4 i) 3)))))
             (loop for i from 16 below 80
                   do (setf (aref w i) (rotl32 (logxor (aref w (- i 3)) (aref w (- i 8))
                                                       (aref w (- i 14)) (aref w (- i 16)))
                                               1)))
             (let ((a h0) (b h1) (c h2) (d h3) (e h4))
               (dotimes (i 80)
                 (multiple-value-bind (f k)
                     (cond ((< i 20) (values (logior (logand b c) (logand (logxor b #xFFFFFFFF) d))
                                             #x5A827999))
                           ((< i 40) (values (logxor b c d) #x6ED9EBA1))
                           ((< i 60) (values (logior (logand b c) (logand b d) (logand c d))
                                             #x8F1BBCDC))
                           (t (values (logxor b c d) #xCA62C1D6)))
                   (let ((temp (logand #xFFFFFFFF (+ (rotl32 a 5) f e k (aref w i)))))
                     (setf e d d c c (rotl32 b 30) b a a temp))))
               (setf h0 (logand #xFFFFFFFF (+ h0 a)) h1 (logand #xFFFFFFFF (+ h1 b))
                     h2 (logand #xFFFFFFFF (+ h2 c)) h3 (logand #xFFFFFFFF (+ h3 d))
                     h4 (logand #xFFFFFFFF (+ h4 e)))))
    (let ((digest (make-array 20 :element-type '(unsigned-byte 8))))
      (loop for h in (list h0 h1 h2 h3 h4)
            for i from 0 by 4
            do (dotimes (j 4)
                 (setf (aref digest (+ i j)) (ldb (byte 8 (- 24 (* 8 j))) h))))
      digest)))

(defun base64-encode (octets)
  (let ((alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))
    (with-output-to-string (out)
      (loop for i from 0 below (length octets) by 3
            do (let* ((n (length octets))
                      (b0 (aref octets i))
                      (b1 (if (< (+ i 1) n) (aref octets (+ i 1)) 0))
                      (b2 (if (< (+ i 2) n) (aref octets (+ i 2)) 0))
                      (word (logior (ash b0 16) (ash b1 8) b2)))
                 (write-char (char alphabet (ldb (byte 6 18) word)) out)
                 (write-char (char alphabet (ldb (byte 6 12) word)) out)
                 (write-char (if (< (+ i 1) n) (char alphabet (ldb (byte 6 6) word)) #\=) out)
                 (write-char (if (< (+ i 2) n) (char alphabet (ldb (byte 6 0) word)) #\=) out))))))

(defun random-token ()
  "32 hex digits from /dev/urandom."
  (with-open-file (in "/dev/urandom" :element-type '(unsigned-byte 8))
    (format nil "~(~{~2,'0X~}~)" (loop repeat 16 collect (read-byte in)))))


;;;; WebSocket connections.

(defstruct (ide-client (:constructor make-ide-client (connection)))
  connection
  (state :handshake)
  (input (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
  (message (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
  (initialized nil)
  (deleted nil))

(defun ide-drop (client)
  "Close CLIENT's connection and forget it, once: closing a descriptor
   twice can close the one the next connection was given."
  (setf *ide-clients* (remove client *ide-clients*))
  (unless (ide-client-deleted client)
    (setf (ide-client-deleted client) t
          (ide-client-state client) :closed)
    (ignore-errors (delete-connection (ide-client-connection client)))))

(defun octets (string)
  (babel:string-to-octets string :encoding :utf-8))

(defun ide-write (client octets)
  (handler-case (connection-write (coerce octets '(simple-array (unsigned-byte 8) (*)))
                                  (ide-client-connection client))
    (error (condition) (lsp-log "!!" (format nil "claude ide: not sent: ~A" condition)))))

(defun ide-send-frame (client opcode payload)
  "Send PAYLOAD, octets, in one unmasked frame of OPCODE, as a server does."
  (let* ((length (length payload))
         (header (cond ((< length 126) (list (logior #x80 opcode) length))
                       ((< length 65536) (list (logior #x80 opcode) 126
                                               (ldb (byte 8 8) length) (ldb (byte 8 0) length)))
                       (t (list* (logior #x80 opcode) 127
                                 (loop for i from 7 downto 0
                                       collect (ldb (byte 8 (* 8 i)) length)))))))
    (ide-write client (concatenate '(vector (unsigned-byte 8)) header payload))))

(defun ide-send (client object)
  "Send OBJECT, JSON as jzon has it, to CLIENT."
  (let ((text (com.inuoe.jzon:stringify object)))
    (lsp-log "claude>" text)
    (ide-send-frame client 1 (octets text))))

(defun header-end (octets)
  (search #(13 10 13 10) octets))

(defun parse-headers (text)
  "The request's headers, as an alist of downcased names and values."
  (loop for line in (rest (uiop:split-string text :separator '(#\Newline)))
        for trimmed = (string-right-trim '(#\Return) line)
        for colon = (position #\: trimmed)
        when colon
          collect (cons (string-downcase (subseq trimmed 0 colon))
                        (string-trim " " (subseq trimmed (1+ colon))))))

(defun ide-handshake (client)
  "Answer the HTTP request that opens the WebSocket, once it is all here:
   with the switch to the protocol, or 401 without the lock file's token."
  (let* ((input (ide-client-input client))
         (end (header-end input)))
    (when end
      (let* ((headers (parse-headers (babel:octets-to-string input :end end :encoding :latin-1
                                                                       :errorp nil)))
             (key (cdr (assoc "sec-websocket-key" headers :test #'string=)))
             (token (cdr (assoc "x-claude-code-ide-authorization" headers :test #'string=)))
             (protocols (cdr (assoc "sec-websocket-protocol" headers :test #'string=)))
             (rest (subseq input (+ end 4))))
        (cond ((or (null key) (not (equal token *ide-token*)))
               (lsp-log "!!" "claude ide: refused a connection without the token")
               (ide-write client (octets (format nil "HTTP/1.1 401 Unauthorized~C~CContent-Length: 0~C~C~C~C"
                                                 #\Return #\Newline #\Return #\Newline
                                                 #\Return #\Newline)))
               (setf (ide-client-state client) :closed)
               ;; Closed once the answer has gone: what is written is queued.
               (schedule-event 0.5 (lambda (elapsed)
                                     (declare (ignore elapsed))
                                     (ide-drop client))
                               nil))
              (t
               (ide-write client
                          (octets (format nil "HTTP/1.1 101 Switching Protocols~@{~A~}"
                                          (crlf) "Upgrade: websocket" (crlf) "Connection: Upgrade" (crlf)
                                          "Sec-WebSocket-Accept: "
                                          (base64-encode
                                           (sha1 (octets (concatenate 'string key
                                                                      "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"))))
                                          (crlf)
                                          (if (and protocols (search "mcp" protocols))
                                              (format nil "Sec-WebSocket-Protocol: mcp~A" (crlf))
                                              "")
                                          (crlf))))
               (setf (ide-client-state client) :open
                     (fill-pointer (ide-client-input client)) 0)
               (ide-receive client rest)))))))

(defun crlf () (format nil "~C~C" #\Return #\Newline))

(defun ide-receive (client bytes)
  "BYTES from CLIENT: the handshake's, or frames', each acted on once whole."
  (loop for byte across bytes do (vector-push-extend byte (ide-client-input client)))
  (case (ide-client-state client)
    (:handshake (ide-handshake client))
    (:open (ide-read-frames client))))

(defun ide-read-frames (client)
  (loop
    (let* ((input (ide-client-input client))
           (available (length input)))
      (when (< available 2) (return))
      (let* ((b0 (aref input 0))
             (b1 (aref input 1))
             (fin (logbitp 7 b0))
             (opcode (logand b0 #x0F))
             (masked (logbitp 7 b1))
             (length (logand b1 #x7F))
             (at 2))
        (cond ((= length 126)
               (when (< available 4) (return))
               (setf length (logior (ash (aref input 2) 8) (aref input 3)) at 4))
              ((= length 127)
               (when (< available 10) (return))
               (setf length (loop for i from 2 below 10
                                  for n = (aref input i) then (logior (ash n 8) (aref input i))
                                  finally (return n))
                     at 10)))
        (let ((mask (when masked (subseq input at (+ at 4)))))
          (when masked (incf at 4))
          (when (< available (+ at length)) (return))
          (let ((payload (subseq input at (+ at length))))
            (when mask
              (dotimes (i length)
                (setf (aref payload i) (logxor (aref payload i) (aref mask (mod i 4))))))
            ;; What is left is the next frame's.
            (let ((rest (subseq input (+ at length))))
              (setf (fill-pointer input) 0)
              (loop for byte across rest do (vector-push-extend byte input)))
            (ide-frame client fin opcode payload)
            (when (eq (ide-client-state client) :closed) (return))))))))

(defun ide-frame (client fin opcode payload)
  (case opcode
    ((0 1 2)
     (loop for byte across payload do (vector-push-extend byte (ide-client-message client)))
     (when fin
       (let ((text (babel:octets-to-string (coerce (ide-client-message client)
                                                   '(simple-array (unsigned-byte 8) (*)))
                                           :encoding :utf-8 :errorp nil)))
         (setf (fill-pointer (ide-client-message client)) 0)
         (ide-message client text))))
    (8 (ide-send-frame client 8 #())
       (setf (ide-client-state client) :closed)
       (schedule-event 0.5 (lambda (elapsed)
                             (declare (ignore elapsed))
                             (ide-drop client))
                       nil))
    (9 (ide-send-frame client 10 payload))
    (t nil)))


;;;; MCP.

(defun ide-result (client id result)
  (ide-send client (json "jsonrpc" "2.0" "id" id "result" result)))

(defun ide-error (client id code message)
  (ide-send client (json "jsonrpc" "2.0" "id" id
                         "error" (json "code" code "message" message))))

(defun ide-notify (client method params)
  (ide-send client (json "jsonrpc" "2.0" "method" method "params" params)))

(defun ide-notify-all (method params)
  (dolist (client *ide-clients*)
    (when (ide-client-initialized client)
      (ide-notify client method params))))

(defun text-content (&rest texts)
  "A tool's answer: each of TEXTS, a string or JSON to be one."
  (json "content" (map 'vector (lambda (text)
                                 (json "type" "text"
                                       "text" (if (stringp text)
                                                  text
                                                  (com.inuoe.jzon:stringify text))))
                       texts)))

(defun ide-message (client text)
  (lsp-log "claude<" text)
  (let* ((message (handler-case (com.inuoe.jzon:parse text)
                    (error () (return-from ide-message nil))))
         (id (jref message "id"))
         (method (jref message "method"))
         (params (jref message "params")))
    (cond ((equal method "initialize")
           (setf (ide-client-initialized client) t)
           (ide-result client id
                       (json "protocolVersion" (or (jref params "protocolVersion") "2024-11-05")
                             "capabilities" (json "tools" (json "listChanged" t))
                             "serverInfo" (json "name" "Heml" "version" "1"))))
          ((equal method "tools/list")
           (ide-result client id (json "tools" (ide-tool-list))))
          ((equal method "tools/call")
           ;; On the command loop, where buffers may be looked at and
           ;; changed and windows shown.
           (queue-command (lambda () (ide-call-tool client id params))))
          ((member method '("prompts/list") :test #'equal)
           (ide-result client id (json "prompts" #())))
          ((member method '("resources/list") :test #'equal)
           (ide-result client id (json "resources" #())))
          ((member method '("resources/templates/list") :test #'equal)
           (ide-result client id (json "resourceTemplates" #())))
          ((equal method "ide_connected")
           (queue-command (lambda () (ide-send-selection))))
          ((and id method)
           (ide-error client id -32601 (format nil "Method not found: ~A" method)))
          (t nil))))


;;;; The tools.

(defparameter *ide-tools*
  '(("getCurrentSelection" "The text selected, or the cursor's place, in the active editor" ())
    ("getLatestSelection" "The latest text selected in any file" ())
    ("openFile" "Open a file in Heml, selecting text or lines in it"
     (("filePath" "string") ("uri" "string") ("startText" "string") ("endText" "string")
      ("startLine" "integer") ("endLine" "integer")))
    ("getOpenEditors" "The files open in Heml" ())
    ("getWorkspaceFolders" "The projects of the files open in Heml" ())
    ("getDiagnostics" "Errors and warnings from the language servers, for a file or all"
     (("uri" "string")))
    ("checkDocumentDirty" "Whether a file has changes not saved" (("uri" "string")))
    ("saveDocument" "Save a file" (("uri" "string")))
    ("openDiff" "Show a proposed change to a file, for the user to accept or reject"
     (("old_file_path" "string") ("new_file_path" "string") ("new_file_contents" "string")
      ("tab_name" "string")))
    ("close_tab" "Close a diff" (("tab_name" "string")))
    ("closeAllDiffTabs" "Close every diff" ()))
  "(NAME DESCRIPTION ((ARGUMENT TYPE) ...)) for each tool.")

(defun ide-tool-list ()
  (map 'vector (lambda (tool)
                 (destructuring-bind (name description arguments) tool
                   (json "name" name "description" description
                         "inputSchema"
                         (json "type" "object"
                               "properties" (let ((properties (json)))
                                              (loop for (argument type) in arguments
                                                    do (setf (gethash argument properties)
                                                             (json "type" type)))
                                              properties)))))
       *ide-tools*))

(defun ide-call-tool (client id params)
  (let ((name (jref params "name"))
        (arguments (or (jref params "arguments") (json))))
    (handler-case
        (let ((result (ide-tool name arguments client id)))
          (unless (eq result :later)
            (ide-result client id result)))
      (error (condition)
        (ide-error client id -32603 (format nil "~A: ~A" name condition))))))

(defun argument-path (arguments &rest keys)
  "The file the first of KEYS in ARGUMENTS names, a path or a file: URI."
  (let ((value (some (lambda (key) (jref arguments key)) keys)))
    (when value
      (or (uri-file value) value))))

(defun ide-file-buffer (path)
  "The buffer visiting PATH, or NIL."
  (let ((truename (probe-file path)))
    (and truename
         (find-if (lambda (buffer) (equal (buffer-pathname buffer) truename)) *buffer-list*))))

(defun place-json (mark)
  (json "line" (line-number-in-buffer mark) "character" (mark-charpos mark)))

(defun buffer-selection (buffer)
  "BUFFER's selection, as Claude Code is told of it, or NIL when BUFFER has
   no file."
  (let ((pathname (buffer-pathname buffer)))
    (when pathname
      (let* ((point (buffer-point buffer))
             (active (and (eq buffer (current-buffer)) (region-active-p)))
             (region (and active (current-region nil nil)))
             (start (if region (region-start region) point))
             (end (if region (region-end region) point)))
        (json "text" (if region (region-to-string region) "")
              "filePath" (namestring pathname)
              "fileUrl" (file-uri pathname)
              "selection" (json "start" (place-json start) "end" (place-json end)
                                "isEmpty" (not region)))))))

(defvar *ide-latest-selection* nil "The last selection that was not empty.")

(defun empty-selection ()
  (json "text" "" "filePath" ""
        "selection" (json "start" (json "line" 0 "character" 0)
                          "end" (json "line" 0 "character" 0) "isEmpty" t)))

(defun ide-tool (name arguments client id)
  (declare (ignorable client id))
  (cond
    ((equal name "getCurrentSelection")
     (text-content (or (buffer-selection (current-buffer)) (empty-selection))))
    ((equal name "getLatestSelection")
     (text-content (or *ide-latest-selection* (buffer-selection (current-buffer)) (empty-selection))))
    ((equal name "openFile")
     (let ((path (argument-path arguments "filePath" "uri")))
       (unless (and path (probe-file path))
         (editor-error "No file ~A." path))
       (change-to-buffer (find-file-buffer path))
       (ide-select (current-buffer) arguments)
       (text-content "FILE_OPENED")))
    ((equal name "getOpenEditors")
     (text-content
      (json "editors"
            (map 'vector (lambda (buffer)
                           (let ((pathname (buffer-pathname buffer)))
                             (json "uri" (file-uri pathname) "name" (file-namestring pathname)
                                   "path" (namestring pathname)
                                   "isActive" (eq buffer (current-buffer))
                                   "isDirty" (and (buffer-modified buffer) t))))
                 (remove-if-not #'buffer-pathname *buffer-list*)))))
    ((equal name "getWorkspaceFolders")
     (text-content
      (json "folders"
            (map 'vector (lambda (root)
                           (json "uri" (file-uri root) "name" (car (last (pathname-directory root)))
                                 "path" (namestring root)))
                 (ide-workspace-folders)))))
    ((equal name "getDiagnostics")
     (text-content (ide-diagnostics (argument-path arguments "uri"))))
    ((equal name "checkDocumentDirty")
     (let ((buffer (ide-file-buffer (argument-path arguments "uri" "filePath"))))
       (text-content (json "isDirty" (and buffer (buffer-modified buffer) t)))))
    ((equal name "saveDocument")
     (let ((buffer (ide-file-buffer (argument-path arguments "uri" "filePath"))))
       (cond (buffer
              (save-file-command nil buffer)
              (text-content (json "saved" t)))
             (t (text-content (json "saved" nil "error" "File not open"))))))
    ((equal name "openDiff")
     (ide-open-diff client id arguments)
     :later)
    ((equal name "close_tab")
     (let ((diff (find (jref arguments "tab_name") *ide-diffs* :key #'ide-diff-tab :test #'equal)))
       (cond (diff (close-ide-diff diff nil) (text-content "TAB_CLOSED"))
             (t (text-content "TAB_NOT_DIFF")))))
    ((equal name "closeAllDiffTabs")
     (let ((count (length *ide-diffs*)))
       (dolist (diff *ide-diffs*) (close-ide-diff diff nil))
       (text-content (format nil "CLOSED_~D_DIFF_TABS" count))))
    (t (error "No tool ~A." name))))

(defun ide-select (buffer arguments)
  "Point, and the region, where ARGUMENTS of openFile say: between
   startText and endText, or the lines from startLine to endLine (from 1)."
  (let ((point (buffer-point buffer))
        (start-text (jref arguments "startText"))
        (end-text (jref arguments "endText"))
        (start-line (jref arguments "startLine"))
        (end-line (jref arguments "endLine")))
    (flet ((select (start end)
             (move-mark point start)
             (when end
               (push-buffer-mark (copy-mark end) t))))
      (cond ((and start-text (plusp (length start-text)))
             (let ((text (region-to-string (buffer-region buffer))))
               (let ((from (search start-text text)))
                 (when from
                   (let* ((to (and end-text (plusp (length end-text))
                                   (search end-text text :start2 from)))
                          (start (copy-mark (buffer-start-mark buffer) :temporary)))
                     (character-offset start from)
                     (if to
                         (let ((end (copy-mark (buffer-start-mark buffer) :temporary)))
                           (character-offset end (+ to (length end-text)))
                           (select start end))
                         (select start nil)))))))
            (start-line
             (let ((start (copy-mark (buffer-start-mark buffer) :temporary)))
               (move-to-line start start-line 0)
               (if end-line
                   (let ((end (copy-mark start :temporary)))
                     (move-to-line end end-line 0)
                     (line-end end)
                     (select start end))
                   (select start nil))))))))

(defun ide-workspace-folders ()
  (remove-duplicates
   (remove nil (append *ide-roots*
                       (mapcar (lambda (buffer)
                                 (let ((pathname (buffer-pathname buffer)))
                                   (and pathname
                                        (let ((root (project-root (directory-namestring pathname))))
                                          (and root (uiop:ensure-directory-pathname root))))))
                               *buffer-list*)))
   :test #'equal))

(defun ide-diagnostics (path)
  "What the language servers say is wrong with the file PATH, or with every
   file open: ((uri diagnostics) ...)."
  (let ((only (and path (probe-file path))))
    (coerce
     (loop for buffer in *buffer-list*
           for pathname = (buffer-pathname buffer)
           for entries = (and pathname (or (null only) (equal pathname only))
                              (gethash buffer *buffer-diagnostics*))
           when entries
             collect (json "uri" (file-uri pathname)
                           "diagnostics"
                           (map 'vector
                                (lambda (entry)
                                  (destructuring-bind (start end severity message &rest more) entry
                                    (declare (ignore more))
                                    (json "range" (json "start" (place-json start) "end" (place-json end))
                                          "severity" (case severity
                                                       (1 "Error") (2 "Warning") (3 "Information")
                                                       (t "Hint"))
                                          "message" message)))
                                entries)))
     'vector)))


;;;; Proposed changes, as diffs.

(defmode "Claude Diff" :documentation
  "A change Claude Code proposes: C-c C-c accepts it, C-c C-k rejects it.")

(defun ide-open-diff (client id arguments)
  (let* ((path (argument-path arguments "old_file_path"))
         (new-path (or (argument-path arguments "new_file_path") path))
         (contents (or (jref arguments "new_file_contents") ""))
         (tab (or (jref arguments "tab_name") (file-namestring new-path)))
         (old (if (and path (probe-file path)) (namestring path) "/dev/null"))
         (proposed (uiop:with-temporary-file (:pathname temp :keep t :type "proposed")
                     (with-open-file (out temp :direction :output :if-exists :supersede
                                               :external-format :utf-8)
                       (write-string contents out))
                     temp))
         (output (unwind-protect
                      (uiop:run-program (list "diff" "-u" "--label" (format nil "a/~A" (file-namestring new-path))
                                              "--label" (format nil "b/~A" (file-namestring new-path))
                                              old (namestring proposed))
                                        :output :string :ignore-error-status t)
                   (delete-file proposed)))
         (root (uiop:pathname-directory-pathname new-path))
         (buffer (make-git-buffer (format nil "*claude diff: ~A*" tab) "Diff" root))
         (diff (make-ide-diff :tab tab :client client :id id :path new-path
                              :contents contents :buffer buffer)))
    (fill-git-buffer buffer (or (git-lines output) (list "No changes.")))
    (setf (buffer-minor-mode buffer "Claude Diff") t)
    (defhvar "Claude Diff" "The proposed change this buffer shows." :buffer buffer :value diff)
    (push diff *ide-diffs*)
    (let ((here (current-window)))
      (select-window (other-window))
      (change-to-buffer buffer)
      (buffer-start (current-point))
      (select-window here))
    (message "Claude proposes a change to ~A: C-c C-c in its diff accepts, C-c C-k rejects."
             (file-namestring new-path))))

(defun close-ide-diff (diff answer)
  "Close DIFF, answering Claude Code's openDiff with ANSWER: :accept,
   :reject, or NIL when it closed the diff itself."
  (setf *ide-diffs* (remove diff *ide-diffs*))
  (let ((client (ide-diff-client diff)))
    (case answer
      (:accept
       (ide-result client (ide-diff-id diff)
                   (text-content "FILE_SAVED" (ide-diff-contents diff)))
       (revert-when-written (ide-diff-path diff)))
      (:reject
       (ide-result client (ide-diff-id diff)
                   (text-content "DIFF_REJECTED" (ide-diff-tab diff))))))
  (let ((buffer (ide-diff-buffer diff)))
    (when (member buffer *buffer-list*)
      (dolist (window (buffer-windows buffer))
        (unless (rest *window-list*) (return))
        (when (member window *window-list*)
          (delete-window window)))
      (delete-buffer-if-possible buffer))))

(defun revert-when-written (path)
  "Claude Code writes an accepted change itself: read the file again into
   its buffer, unless that has changes of its own, once it is written."
  (let ((buffer (ide-file-buffer path))
        (date (and (probe-file path) (file-write-date path)))
        (waited 0))
    (when (and buffer (not (buffer-modified buffer)))
      (let (event)
        (setf event
              (lambda (elapsed)
                (declare (ignore elapsed))
                (incf waited)
                (let ((now (and (probe-file path) (file-write-date path))))
                  (when (or (and now (not (eql now date))) (> waited 40))
                    (remove-scheduled-event event)
                    (when (and now (not (eql now date)) (member buffer *buffer-list*)
                               (not (buffer-modified buffer)))
                      (let ((line (line-number-in-buffer (buffer-point buffer))))
                        (read-buffer-file path buffer)
                        (move-to-line (buffer-point buffer) (1+ line) 0)))))))
        (schedule-event 0.25 event)))))

(defun current-ide-diff ()
  (or (and (heml-bound-p 'claude-diff :buffer (current-buffer))
           (variable-value 'claude-diff :buffer (current-buffer)))
      (editor-error "Not a diff Claude Code proposed.")))

(defcommand "Claude Accept Diff" (p)
  "Accept the change Claude Code proposes in this diff: it writes it."
  "Accept the proposed change."
  (declare (ignore p))
  (close-ide-diff (current-ide-diff) :accept)
  (message "Accepted."))

(defcommand "Claude Reject Diff" (p)
  "Reject the change Claude Code proposes in this diff."
  "Reject the proposed change."
  (declare (ignore p))
  (close-ide-diff (current-ide-diff) :reject)
  (message "Rejected."))

(bind-key "Claude Accept Diff" #k"control-c control-c" :mode "Claude Diff")
(bind-key "Claude Reject Diff" #k"control-c control-k" :mode "Claude Diff")


;;;; The selection, as it changes.

(defvar *ide-last-selection-sent* nil)

(defhvar "Claude Send Selection"
  "Whether Claude Code is told what is selected as the selection changes,
   as VS Code's extension tells it.  It may still ask."
  :value t)

(defun ide-send-selection ()
  (when (and *ide-clients* (value claude-send-selection))
    (let ((selection (buffer-selection (current-buffer))))
      (when selection
        (unless (jref selection "selection" "isEmpty")
          (setf *ide-latest-selection* selection))
        (let ((key (list (jref selection "filePath")
                         (jref selection "selection" "start" "line")
                         (jref selection "selection" "start" "character")
                         (jref selection "selection" "end" "line")
                         (jref selection "selection" "end" "character"))))
          (unless (equal key *ide-last-selection-sent*)
            (setf *ide-last-selection-sent* key)
            (ide-notify-all "selection_changed" selection)))))))

(add-hook after-command-hook 'ide-send-selection)


;;;; The server.

(defun default-claude-config-directory ()
  "Claude Code's own default: $CLAUDE_CONFIG_DIR, or ~/.claude/."
  (uiop:ensure-directory-pathname
   (or (let ((dir (uiop:getenv "CLAUDE_CONFIG_DIR")))
         (and dir (plusp (length dir)) dir))
       (merge-pathnames ".claude/" (user-homedir-pathname)))))

(defun lock-file (config-directory port)
  (merge-pathnames (format nil "ide/~D.lock" port) config-directory))

(defun write-lock-files ()
  (dolist (directory *ide-lock-directories*)
    (write-lock-file (lock-file directory *ide-port*))))

(defun write-lock-file (file)
  "Say in FILE where the server is and what a client must show it, readable
   by this user alone, since it holds the token."
  (ensure-directories-exist file)
  (progn
    (with-open-file (out file :direction :output :if-exists :supersede :external-format :utf-8)
      (write-string (com.inuoe.jzon:stringify
                     (json "pid" (isys:getpid)
                           "workspaceFolders" (map 'vector (lambda (root)
                                                             (string-right-trim "/" (namestring root)))
                                                   *ide-roots*)
                           "ideName" "Heml"
                           "transport" "ws"
                           "authToken" *ide-token*))
                    out))
    (isys:chmod (namestring file) #o600)))

(defun remove-stale-lock-files (config-directory)
  "Take away the lock files earlier Hemls left in CONFIG-DIRECTORY, whose
   process is gone."
  (dolist (file (ignore-errors (directory (merge-pathnames "ide/*.lock" config-directory))))
    (ignore-errors
     (let* ((object (com.inuoe.jzon:parse (uiop:read-file-string file)))
            (pid (jref object "pid")))
       (when (and (equal (jref object "ideName") "Heml") (integerp pid)
                  (not (zerop (cffi:foreign-funcall "kill" :int pid :int 0 :int))))
         (delete-file file))))))

(defun ensure-ide-server (root config-directory)
  "The server, started the first time, with ROOT among its folders and a
   lock file in CONFIG-DIRECTORY, Claude Code's own; its port."
  (unless (member config-directory *ide-lock-directories* :test #'equal)
    (remove-stale-lock-files config-directory))
  (unless *ide-listener*
    (setf *ide-token* (random-token)
          *ide-listener*
          (make-tcp-listener "claude ide" "127.0.0.1" 0
                             :acceptor (lambda (connection)
                                         (push (make-ide-client connection) *ide-clients*))
                             :initargs (list :filter #'ide-filter :sentinel #'ide-sentinel))
          *ide-port* (connection-port *ide-listener*)))
  (when root
    (pushnew (uiop:ensure-directory-pathname root) *ide-roots* :test #'equal))
  (pushnew config-directory *ide-lock-directories* :test #'equal)
  (write-lock-files)
  *ide-port*)

(defun ide-client-of (connection)
  (find connection *ide-clients* :key #'ide-client-connection))

(defun ide-filter (connection bytes)
  (let ((client (ide-client-of connection)))
    (when client
      (handler-case (ide-receive client bytes)
        (error (condition) (lsp-log "!!" (format nil "claude ide: ~A" condition))))))
  nil)

(defun ide-sentinel (connection event)
  (when (eq event :disconnected)
    (let ((client (ide-client-of connection)))
      (when client
        (setf *ide-clients* (remove client *ide-clients*))
        ;; Its descriptors closed, from the command loop: deleting a
        ;; connection inside its own event handler is not safe.
        (queue-command (lambda () (ide-drop client)))
        ;; Its diffs have no one to answer.
        (dolist (diff *ide-diffs*)
          (when (eq (ide-diff-client diff) client)
            (queue-command (lambda () (close-ide-diff diff nil)))))))))

(defun stop-ide-server ()
  (when *ide-listener*
    (dolist (directory *ide-lock-directories*)
      (ignore-errors (delete-file (lock-file directory *ide-port*))))
    (dolist (client *ide-clients*)
      (ide-drop client))
    (ignore-errors (delete-connection *ide-listener*))
    (setf *ide-listener* nil *ide-port* nil *ide-clients* '() *ide-roots* '()
          *ide-lock-directories* '())))

(add-hook exit-hook 'stop-ide-server)

(defun ide-connected-p ()
  (some #'ide-client-initialized *ide-clients*))

(defun ide-mention (pathname start-line end-line)
  "Tell Claude Code the lines START-LINE to END-LINE (from 0) of PATHNAME
   are what the user means."
  (ide-notify-all "at_mentioned" (json "filePath" (namestring pathname)
                                       "lineStart" start-line "lineEnd" end-line)))
