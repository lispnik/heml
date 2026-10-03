;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; A language server's side of the conversation (lsp.lisp): what it asks
;;; -- edits, settings, the user's choice among things -- and what it says
;;; unasked: what is wrong, messages for the user, how far it has got.
;;; The files it wants to hear of when they change.  And starting a server,
;;; giving way to the next command when one will not start, and stopping.

(in-package :heml)


;;;; What a server asks, and says unasked.

(defvar *lsp-questions* '()
  "((SERVER ID PARAMS) ...): what servers have asked the user to choose
   among (window/showMessageRequest), waiting for the command loop.")

(defun lsp-dispatch (server message)
  (let ((id (jref message "id"))
        (method (jref message "method"))
        (*encoding* (lsp-server-encoding server)))
    (cond ((and method id)
           ;; The server asks something.
           (let ((answer (or (ignore-errors (lsp-answer server id method
                                                        (jref message "params")))
                             'null)))
             ;; What the user is to answer is answered when they have.
             (unless (eq answer :later)
               (lsp-send server (json "jsonrpc" "2.0" "id" id "result" answer)))))
          (method
           (lsp-told server method (jref message "params")))
          (id
           (let ((function (gethash id (lsp-server-pending server))))
             (when function
               (remhash id (lsp-server-pending server))
               (funcall function (gethash "result" message) (jref message "error"))))))))

(defun lsp-answer (server id method params)
  "The answer to what SERVER asks, in the request ID: an edit it wants made
   is made, its settings are given, what it wants to hear of is noted, and
   anything else is answered with null.  :LATER when the user is to answer."
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
        ;; Something for the user to choose among: asked of them by the
        ;; command loop, where a popup can be shown.
        ((string= method "window/showMessageRequest")
         (cond ((jlist (jref params "actions"))
                (setf *lsp-questions*
                      (append *lsp-questions* (list (list server id params))))
                (queue-command 'lsp-ask-question)
                :later)
               (t
                (lsp-say server (jref params "type") (or (jref params "message") ""))
                'null)))
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

(defun lsp-ask-question ()
  "Ask the user the next thing a server asked: what it says, in the echo
   area, and its choices in a popup.  The server is told the choice, or
   that there was none."
  (let ((question (pop *lsp-questions*)))
    (when question
      (destructuring-bind (server id params) question
        (let* ((actions (jlist (jref params "actions")))
               (text (substitute #\Space #\Newline (or (jref params "message") "")))
               (choice (unless (eq (lsp-server-state server) :dead)
                         (message "~A: ~A" (lsp-server-group server) text)
                         (ignore-errors
                          (popup-select (mapcar (lambda (action)
                                                  (or (jref action "title") "?"))
                                                actions))))))
          (lsp-say server 4 (format nil "~A  (~:[no answer~;~:*~A~])" text
                                    (and choice (jref (nth choice actions) "title"))))
          (lsp-send server (json "jsonrpc" "2.0" "id" id
                                 "result" (if choice (nth choice actions) 'null))))))))

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
                           (and (stringp message)
                                (plusp (length (string-trim " " message)))
                                (string-trim " " message)))
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
                     ;; The commands of a server's own that only the editor
                     ;; can do and Heml does (RUN-LSP-COMMAND): rust-analyzer
                     ;; offers its Run lens only to an editor that says so.
                     "experimental" (json "commands"
                                          (json "commands"
                                                (vector "rust-analyzer.runSingle"
                                                        "rust-analyzer.showReferences"
                                                        "rust-analyzer.gotoLocation")))
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
