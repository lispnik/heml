;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; What a language server is asked for by a command (lsp.lisp): where a
;;; thing is defined and used, what it is, fixes and refactorings, a new
;;; name, a layout, the project's symbols, a buffer's outline, a call's
;;; signature, and completions.

(in-package :heml)


(defparameter *symbol-kinds*
  #(nil "file" "module" "namespace" "package" "class" "method" "property" "field"
    "constructor" "enum" "interface" "function" "variable" "constant" "string" "number"
    "boolean" "array" "object" "key" "null" "enum member" "struct" "event" "operator"
    "type parameter"))


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
  (let* ((server (lsp-current-server
                  ;; "textDocument/typeDefinition" is "typeDefinitionProvider".
                  (format nil "~AProvider" (subseq method (1+ (position #\/ method))))))
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
  (let* ((server (lsp-current-server "referencesProvider"))
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
  (let* ((server (lsp-current-server "hoverProvider"))
         (buffer (current-buffer))
         (wrong (diagnostic-at-mark (current-point)))
         ;; What each of the buffer's servers says of it.
         (hover (format nil "~{~A~^~%~%~}"
                        (loop for server in (cons server (remove server (buffer-language-servers
                                                                         buffer)))
                              for text = (and (eq (lsp-server-state server) :ready)
                                              (jref (lsp-server-capabilities server) "hoverProvider")
                                              (let ((*encoding* (lsp-server-encoding server)))
                                                (lsp-sync server buffer)
                                                (string-trim
                                                 '(#\Space #\Newline)
                                                 (hover-text
                                                  (jref (lsp-request
                                                         server "textDocument/hover"
                                                         (lsp-symbol-params buffer (current-point)))
                                                        "contents")))))
                              when (and text (plusp (length text)))
                                collect text)))
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
  (let* ((server (lsp-current-server "renameProvider"))
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
  (let* ((server (lsp-current-server "workspaceSymbolProvider"))
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
  (let ((server (buffer-server-with buffer "documentSymbolProvider")))
    (when server
      (setf *encoding* (lsp-server-encoding server))
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
         (server (buffer-server-with buffer "signatureHelpProvider"))
         (start (call-start point)))
    (when (and server start
               (not (eql (previous-character point) #\Space)))
      (setf *encoding* (lsp-server-encoding server))
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
         ;; Each of the buffer's servers that completes is asked: (ITEM .
         ;; SERVER) for each thing any offers, the mode's own server's first.
         (items (loop for server in (ignore-errors (buffer-language-servers buffer))
                      when (and (eq (lsp-server-state server) :ready)
                                (jref (lsp-server-capabilities server) "completionProvider"))
                        append (let ((*encoding* (lsp-server-encoding server)))
                                 (lsp-sync server buffer)
                                 (let ((result (lsp-request server "textDocument/completion"
                                                            (lsp-position-params buffer point)
                                                            :timeout 2)))
                                   (mapcar (lambda (item) (cons item server))
                                           (jlist (if (hash-table-p result)
                                                      (jref result "items")
                                                      result)))))))
         (start (token-start point #'word-char-p))
         (typed (region-to-string (region start point)))
         (found '())
         (candidates
           (remove-duplicates
            (loop for (item . server) in items
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
