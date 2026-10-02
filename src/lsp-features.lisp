;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; More of what a language server gives, over lsp.lisp's client: who calls
;;; what; formatting a region, and as one types; the other uses of the name
;;; at point; colours from the server's own reading of the text; the types
;;; it infers and the things it offers to do, shown after a line's end; and
;;; what can be folded.  And DEFINE-LANGUAGE-SERVER, which gives a mode all
;;; of it, with the servers Heml knows.

(in-package :heml)


;;;; Who calls this, and what this calls.

(defun lsp-calls (incoming)
  (let* ((server (lsp-current-server))
         (item (first (jlist (lsp-request server "textDocument/prepareCallHierarchy"
                                          (lsp-symbol-params (current-buffer)
                                                             (current-point))))))
         (name (and item (jref item "name"))))
    (unless item (editor-error "Nothing here calls or is called."))
    (let ((locations
            (loop for call in (jlist (lsp-request server
                                                  (if incoming
                                                      "callHierarchy/incomingCalls"
                                                      "callHierarchy/outgoingCalls")
                                                  (json "item" item)
                                                  :timeout 15))
                  for other = (jref call (if incoming "from" "to"))
                  for file = (and other (uri-file (or (jref other "uri") "")))
                  for kind = (and other (jref other "kind"))
                  for text = (and other
                                  (format nil "~@[~A ~]~A"
                                          (and (integerp kind) (< 0 kind (length *symbol-kinds*))
                                               (aref *symbol-kinds* kind))
                                          (jref other "name")))
                  when file
                    ;; A caller is listed where it calls; what is called,
                    ;; where it is defined.
                    append (let ((places (and incoming (jlist (jref call "fromRanges")))))
                             (if places
                                 (loop for place in places
                                       collect (list file (jref place "start" "line")
                                                     (jref place "start" "character") text))
                                 (list (list file
                                             (or (jref other "selectionRange" "start" "line") 0)
                                             (or (jref other "selectionRange" "start" "character")
                                                 0)
                                             text)))))))
      (unless locations
        (editor-error (if incoming "Nothing calls ~A." "~A calls nothing.") name))
      (list-lsp-locations "*Calls*"
                          (format nil (if incoming "Calls of ~A" "What ~A calls") name)
                          locations))))

(defcommand "LSP Incoming Calls" (p)
  "List the places that call the function at point, as the language server
   finds them, each with the function it is in."
  "List the callers of the function at point."
  (declare (ignore p))
  (lsp-calls t))

(defcommand "LSP Outgoing Calls" (p)
  "List the functions that the function at point calls, as the language
   server finds them, each where it is defined."
  "List what the function at point calls."
  (declare (ignore p))
  (lsp-calls nil))


;;;; Formatting a region, and as one types.

(defun lsp-format-options ()
  (json "tabSize" 4 "insertSpaces" t))

(defcommand "LSP Format Region" (p)
  "Lay the region out as the language server's formatter does."
  "Format the region with the language server."
  (declare (ignore p))
  (let* ((server (lsp-current-server "documentRangeFormattingProvider"))
         (region (current-region))
         (edits (lsp-request server "textDocument/rangeFormatting"
                             (json "textDocument" (lsp-document (current-buffer))
                                   "range" (json "start" (lsp-position (region-start region))
                                                 "end" (lsp-position (region-end region)))
                                   "options" (lsp-format-options))
                             :timeout 15)))
    (if (plusp (length edits))
        (message "~D change~:P." (apply-text-edits (current-buffer) (jlist edits)))
        (message "Nothing to change."))))

(defhvar "LSP Format on Type"
  "When true, a language server that lays code out as it is typed -- after
   a closing brace, a semicolon, a new line: the server says after what --
   is asked to."
  :value nil)

(defvar *typed-in* nil
  "(BUFFER . SIGNATURE): the buffer the last command finished in, as it was
   then.")

(defun lsp-format-after-typing ()
  "After a command that changed the buffer: if what is before point is
   something the server formats after, ask it to, and make its edits when
   they come if nothing more has been typed."
  (let* ((buffer (current-buffer))
         (signature (buffer-signature buffer))
         (changed (and *typed-in* (eq (car *typed-in*) buffer)
                       (not (eql (cdr *typed-in*) signature)))))
    (setf *typed-in* (cons buffer signature))
    (when (and changed (value lsp-format-on-type))
      (let* ((server (ignore-errors (buffer-language-server buffer)))
             (provider (and server (eq (lsp-server-state server) :ready)
                            (jref (lsp-server-capabilities server)
                                  "documentOnTypeFormattingProvider"))))
        (when (hash-table-p provider)
          (let* ((point (current-point))
                 (typed (if (zerop (mark-charpos point))
                            (and (line-previous (mark-line point)) #\Newline)
                            (previous-character point)))
                 (text (and typed (string typed))))
            (when (and text
                       (member text (cons (jref provider "firstTriggerCharacter")
                                          (jlist (jref provider "moreTriggerCharacter")))
                               :test #'equal))
              (let ((*encoding* (lsp-server-encoding server)))
                (lsp-sync server buffer)
                (lsp-request-async
                 server "textDocument/onTypeFormatting"
                 (json "textDocument" (lsp-document buffer)
                       "position" (lsp-position point)
                       "ch" text
                       "options" (lsp-format-options))
                 (lambda (result error)
                   (declare (ignore error))
                   (when (and (vectorp result) (plusp (length result))
                              (eql (buffer-signature buffer) signature))
                     (apply-text-edits buffer (jlist result))
                     (when (eq (car *typed-in*) buffer)
                       (setf *typed-in* (cons buffer (buffer-signature buffer)))))))))))))))

(add-hook after-command-hook 'lsp-format-after-typing)


;;;; The other uses of the name at point.

(defhvar "LSP Highlight Symbol"
  "When true, the other uses of the name point rests on are shown, as the
   language server finds them."
  :value t)

(defparameter *lsp-highlight-font* '(:bold t :underline t))

(defvar *lsp-highlights* '()
  "((LINE START END) ...): the uses shown.")

(defvar *lsp-highlight-place* nil
  "(BUFFER SIGNATURE LINE CHARPOS): where point was when the uses were last
   asked for.")

(defun clear-lsp-highlights ()
  (when *lsp-highlights*
    (setf *lsp-highlights* '())
    (incf hi:*decoration-tick*)))

(defun lsp-highlight-uses (server buffer)
  "Twice a second: when point has come to rest somewhere new, ask SERVER
   for the uses of what is there."
  (let* ((point (buffer-point buffer))
         (place (list buffer (buffer-signature buffer) (mark-line point) (mark-charpos point))))
    (unless (equal place *lsp-highlight-place*)
      (setf *lsp-highlight-place* place)
      (let ((next (next-character point))
            (previous (previous-character point)))
        (cond ((and (value lsp-highlight-symbol)
                    (jref (lsp-server-capabilities server) "documentHighlightProvider")
                    (or (and next (word-char-p next)) (and previous (word-char-p previous))))
               (lsp-request-async
                server "textDocument/documentHighlight" (lsp-symbol-params buffer point)
                (lambda (result error)
                  (declare (ignore error))
                  ;; Not if point has moved on since.
                  (when (eq place *lsp-highlight-place*)
                    (setf *lsp-highlights*
                          (loop for highlight in (jlist result)
                                for range = (jref highlight "range")
                                when (and range
                                          (eql (jref range "start" "line") (jref range "end" "line")))
                                  collect (with-mark ((mark (buffer-start-mark buffer)))
                                            (lsp-move-mark mark (jref range "start" "line")
                                                           (jref range "start" "character"))
                                            (let ((string (line-string (mark-line mark))))
                                              (list (mark-line mark)
                                                    (mark-charpos mark)
                                                    (unit-charpos string
                                                                  (jref range "end" "character")))))))
                    (incf hi:*decoration-tick*)))))
              (t (clear-lsp-highlights)))))))

(defun lsp-highlight-decorations (line)
  (loop for (there start end) in *lsp-highlights*
        when (eq there line)
          collect (list start end *lsp-highlight-font*)))

(pushnew 'lsp-highlight-uses *lsp-idle-functions*)


;;;; Colours, hints and lenses: what the server says of the whole text.

;;; Each is asked for when the buffer has changed since it was last asked
;;; for (the document's EXTRAS is the signature they are of), a moment after
;;; typing stops, and kept by line: a line is the same line however the
;;; lines before it change.

(defhvar "LSP Semantic Highlighting"
  "When true, a language server's own reading of the text colours it, over
   the colours the grammar gives: names it knows to be types, functions,
   macros and namespaces, and parameters, which are in italics."
  :value t)

(defhvar "LSP Inlay Hints"
  "When true, what a language server infers -- the type of a variable
   declared without one -- is shown after the end of the line it is on."
  :value nil)

(defhvar "LSP Code Lenses"
  "When true, what a language server offers to do with a line -- run the
   test defined there, show what refers to it -- is shown after its end;
   \"LSP Code Lens\" does one."
  :value nil)

(defvar *buffer-tokens* (make-hash-table :test 'eq :weakness :key)
  "Buffer to a table of its lines' colours: line to ((START END FONT) ...).")

(defvar *buffer-hints* (make-hash-table :test 'eq :weakness :key)
  "Buffer to a table of its lines' hints: line to the text.")

(defvar *buffer-lenses* (make-hash-table :test 'eq :weakness :key)
  "Buffer to a table of its lines' lenses: line to the protocol's lenses.")

(defparameter *lsp-annotation-font* '(:fg 8 :italic t))

(defparameter *token-fonts*
  '(("namespace" . 5) ("type" . 2) ("class" . 2) ("enum" . 2) ("interface" . 2)
    ("struct" . 2) ("typeParameter" . 2) ("parameter" . (:italic t))
    ("enumMember" . 3) ("function" . 6) ("method" . 6) ("macro" . 5) ("decorator" . 6))
  "The protocol's kinds of token to the fonts they are drawn in, as the
   grammars' captures are (*CAPTURE-FONTS*).  A kind not here -- a variable,
   and what any grammar can tell, a keyword, a string, a comment -- is left
   as the grammar coloured it.")

(defun buffer-lines-from (buffer)
  (mark-line (buffer-start-mark buffer)))

(defun note-semantic-tokens (server buffer data)
  "DATA is the protocol's tokens: five numbers each, the lines and
   characters relative to the token before."
  (let* ((legend (jlist (jref (lsp-server-capabilities server)
                              "semanticTokensProvider" "legend" "tokenTypes")))
         (fonts (map 'vector (lambda (type) (cdr (assoc type *token-fonts* :test #'equal)))
                     legend))
         (table (make-hash-table :test 'eq))
         (line (buffer-lines-from buffer))
         (character 0)
         (string nil))
    (loop for i from 0 to (- (length data) 5) by 5
          do (let ((lines (aref data i))
                   (type (aref data (+ i 3))))
               (when (plusp lines)
                 (loop repeat lines while line do (setf line (line-next line)))
                 (setf character 0 string nil))
               (unless line (return))
               (incf character (aref data (+ i 1)))
               (let ((font (and (< -1 type (length fonts)) (aref fonts type))))
                 (when font
                   (unless string (setf string (line-string line)))
                   (push (list (unit-charpos string character)
                               (unit-charpos string (+ character (aref data (+ i 2))))
                               font)
                         (gethash line table))))))
    (setf (gethash buffer *buffer-tokens*) table)
    (incf hi:*decoration-tick*)))

(defun lsp-token-decorations (line)
  (let* ((buffer (line-buffer line))
         (table (and buffer (gethash buffer *buffer-tokens*))))
    (and table (gethash line table))))

(defun hint-text (hint string)
  "What the protocol's HINT says, for the end of the line STRING: a type,
   which the server would have after a name, has the name before it."
  (let* ((label (jref hint "label"))
         (text (string-trim " " (if (stringp label)
                                    label
                                    (format nil "~{~A~}"
                                            (mapcar (lambda (part) (or (jref part "value") ""))
                                                    (jlist label)))))))
    (if (and (plusp (length text)) (char= (char text 0) #\:))
        (let* ((end (min (length string) (unit-charpos string (jref hint "position" "character"))))
               (start (or (position-if-not #'word-char-p string :end end :from-end t) -1)))
          (concatenate 'string (subseq string (1+ start) end) text))
        text)))

(defun note-inlay-hints (buffer hints)
  (let ((table (make-hash-table :test 'eq))
        (lines (coerce (loop for line = (buffer-lines-from buffer) then (line-next line)
                             while line collect line)
                       'vector)))
    (dolist (hint (sort (copy-list hints)
                        (lambda (a b)
                          (let ((la (jref a "position" "line")) (lb (jref b "position" "line")))
                            (or (< la lb)
                                (and (= la lb) (< (jref a "position" "character")
                                                  (jref b "position" "character"))))))))
      (let ((number (jref hint "position" "line")))
        ;; Not the names of arguments, which mean nothing away from them.
        (when (and (< -1 number (length lines)) (not (eql (jref hint "kind") 2)))
          (let* ((line (aref lines number))
                 (text (hint-text hint (line-string line))))
            (when (plusp (length text))
              (setf (gethash line table)
                    (format nil "~@[~A, ~]~A" (gethash line table) text)))))))
    (setf (gethash buffer *buffer-hints*) table)))

(defun lens-title (lens)
  (jref lens "command" "title"))

(defun note-code-lenses (server buffer lenses)
  (let ((table (make-hash-table :test 'eq))
        (lines (coerce (loop for line = (buffer-lines-from buffer) then (line-next line)
                             while line collect line)
                       'vector))
        (unresolved 0))
    (dolist (lens lenses)
      (let ((number (jref lens "range" "start" "line")))
        (when (and number (< -1 number (length lines)))
          (let ((line (aref lines number)))
            (setf (gethash line table) (append (gethash line table) (list lens)))
            ;; One whose command the server has yet to work out is asked
            ;; about, a few of them, and changed in place when it answers.
            (when (and (not (jref lens "command"))
                       (jref (lsp-server-capabilities server) "codeLensProvider" "resolveProvider")
                       (< (incf unresolved) 40))
              (lsp-request-async
               server "codeLens/resolve" lens
               (lambda (result error)
                 (declare (ignore error))
                 (when (hash-table-p result)
                   (setf (gethash "command" lens) (gethash "command" result))))))))))
    (setf (gethash buffer *buffer-lenses*) table)))

(defun lsp-line-annotation (line)
  "What the server says of LINE, for after its end: what it infers there,
   and what it offers to do."
  (let ((buffer (line-buffer line)))
    (when buffer
      (let* ((hints (gethash buffer *buffer-hints*))
             (lenses (gethash buffer *buffer-lenses*))
             (hint (and hints (gethash line hints)))
             (titles (and lenses (remove nil (mapcar #'lens-title (gethash line lenses))))))
        (when (or hint titles)
          (cons (format nil "~@[~A~]~:[~;  ~]~@[[~{~A~^ | ~}]~]" hint (and hint titles) titles)
                *lsp-annotation-font*))))))

(defun whole-buffer-range (buffer)
  (json "start" (json "line" 0 "character" 0)
        "end" (lsp-position (buffer-end-mark buffer))))

(defun lsp-refresh-extras (server buffer)
  "Twice a second: ask SERVER for BUFFER's colours, hints and lenses again,
   those that are wanted and it gives, if the buffer has changed since they
   were asked for."
  (let ((document (gethash buffer (lsp-server-documents server)))
        (signature (buffer-signature buffer))
        (capabilities (lsp-server-capabilities server)))
    (when (and document (not (eql (document-extras document) signature)))
      (setf (document-extras document) signature)
      (flet ((current-p () (eql (buffer-signature buffer) signature)))
        (cond ((and (value lsp-semantic-highlighting)
                    (let ((provider (jref capabilities "semanticTokensProvider")))
                      (and (hash-table-p provider) (jref provider "full"))))
               (lsp-request-async
                server "textDocument/semanticTokens/full"
                (json "textDocument" (lsp-document buffer))
                (lambda (result error)
                  (declare (ignore error))
                  (when (and (current-p) (vectorp (jref result "data")))
                    (note-semantic-tokens server buffer (jref result "data"))))))
              ((gethash buffer *buffer-tokens*)
               (remhash buffer *buffer-tokens*)
               (incf hi:*decoration-tick*)))
        (cond ((and (value lsp-inlay-hints) (jref capabilities "inlayHintProvider"))
               (lsp-request-async
                server "textDocument/inlayHint"
                (json "textDocument" (lsp-document buffer)
                      "range" (whole-buffer-range buffer))
                (lambda (result error)
                  (declare (ignore error))
                  (when (current-p)
                    (note-inlay-hints buffer (jlist result))))))
              (t (remhash buffer *buffer-hints*)))
        (cond ((and (value lsp-code-lenses) (jref capabilities "codeLensProvider"))
               (lsp-request-async
                server "textDocument/codeLens"
                (json "textDocument" (lsp-document buffer))
                (lambda (result error)
                  (declare (ignore error))
                  (when (current-p)
                    (note-code-lenses server buffer (jlist result))))))
              (t (remhash buffer *buffer-lenses*)))))))

(pushnew 'lsp-refresh-extras *lsp-idle-functions*)

;;; Uses over colours, and what is wrong (lsp.lisp) over both.
(pushnew 'lsp-highlight-decorations hi:*line-decoration-functions*)
(pushnew 'lsp-token-decorations hi:*line-decoration-functions*)
(pushnew 'lsp-line-annotation hi:*line-annotation-functions*)

(defun lsp-ask-again ()
  "Have every buffer's colours, hints and lenses asked for again."
  (dolist (server *lsp-servers*)
    (loop for document being the hash-values of (lsp-server-documents server)
          do (setf (document-extras document) nil))))

(defun toggle-lsp-variable (name what)
  (let ((on (not (variable-value name :global))))
    (setf (variable-value name :global) on)
    (lsp-ask-again)
    (message "~A are ~:[not shown~;shown~]." what on)))

(defcommand "LSP Inlay Hints" (p)
  "Show what the language server infers -- the types of variables declared
   without them -- after the ends of their lines, or stop showing it."
  "Show or hide the language server's hints."
  (declare (ignore p))
  (toggle-lsp-variable 'lsp-inlay-hints "Hints"))

(defcommand "LSP Code Lenses" (p)
  "Show what the language server offers to do with each line after its
   end, or stop showing it."
  "Show or hide the language server's lenses."
  (declare (ignore p))
  (toggle-lsp-variable 'lsp-code-lenses "Lenses"))

(defun run-lsp-command (server command)
  "Do COMMAND, the protocol's: the server's own is the server's to carry
   out, and showing references is something Heml can do."
  (let ((name (or (jref command "command") ""))
        (arguments (jlist (jref command "arguments"))))
    (cond ((member name (jlist (jref (lsp-server-capabilities server)
                                     "executeCommandProvider" "commands"))
                   :test #'equal)
           (lsp-request server "workspace/executeCommand"
                        (json "command" name
                              "arguments" (or (gethash "arguments" command) (vector)))
                        :timeout 15)
           (message "~A" (or (jref command "title") "Done.")))
          ((search "showReferences" name :test #'char-equal)
           (let ((locations (lsp-locations (third arguments))))
             (unless locations (editor-error "None."))
             (list-lsp-locations "*References*" (or (jref command "title") "References")
                                 locations)))
          (t
           (editor-error "~A is for an editor to do, and Heml does not know how."
                         (if (plusp (length name)) name (jref command "title")))))))

(defcommand "LSP Code Lens" (p)
  "Do what the language server offers to do with this line: one of the
   things, chosen from a popup, when it offers several."
  "Do one of the things the language server offers for this line."
  (declare (ignore p))
  (let* ((server (lsp-current-server))
         (buffer (current-buffer))
         (lenses (or (let ((table (gethash buffer *buffer-lenses*)))
                       (and table (gethash (mark-line (current-point)) table)))
                     ;; Not shown, so not yet asked for.
                     (let ((line (lsp-position (current-point))))
                       (loop for lens in (jlist (lsp-request server "textDocument/codeLens"
                                                             (json "textDocument"
                                                                   (lsp-document buffer))))
                             when (eql (jref lens "range" "start" "line") (jref line "line"))
                               collect lens))))
         (lenses (loop for lens in lenses
                       collect (or (and (not (jref lens "command"))
                                        (let ((resolved (lsp-request server "codeLens/resolve"
                                                                     lens)))
                                          (and (hash-table-p resolved) resolved)))
                                   lens)))
         (lenses (remove-if-not #'lens-title lenses)))
    (unless lenses (editor-error "The server offers nothing for this line."))
    (let ((choice (if (rest lenses)
                      (popup-select (mapcar #'lens-title lenses))
                      0)))
      (when choice
        (run-lsp-command server (jref (nth choice lenses) "command"))))))


;;;; What can be folded.

(defun lsp-fold-ranges (buffer)
  "What BUFFER's language server says can be folded, as ((FIRST . LAST)
   ...), lines from 0; NIL when there is no server that says."
  (let ((server (ignore-errors (buffer-language-server buffer))))
    (when (and server (eq (lsp-server-state server) :ready)
               (jref (lsp-server-capabilities server) "foldingRangeProvider"))
      (let ((*encoding* (lsp-server-encoding server)))
        (lsp-sync server buffer)
        (loop for range in (jlist (lsp-request server "textDocument/foldingRange"
                                               (json "textDocument" (lsp-document buffer))
                                               :timeout 3))
              for first = (jref range "startLine")
              for last = (jref range "endLine")
              when (and (integerp first) (integerp last) (> last first))
                collect (cons first last))))))


;;;; Naming a mode's server.

(defun define-language-server (mode commands &key language-id group)
  "MODE's files are served by a language server: COMMANDS is the command
   lines that run one, a list of a program and its arguments each, the first
   whose program is installed and which starts being used; LANGUAGE-ID is
   the protocol's name for the language; and GROUP, when given, names the
   modes that have one server between them in a project, those defined with
   the same GROUP.  M-. goes to a definition there, M-? lists references,
   C-c C-d describes, C-c C-a offers fixes, C-c C-s finds a symbol in the
   project, C-c C-t goes to a type's definition, C-c C-u lists callers, C-c
   C-l does what the server offers for a line, M-n and M-p go to the next
   and previous error, a call's signature is shown as it is typed, and
   completions and what can be folded come from the server."
  (setf *language-servers*
        (cons (list mode commands (or language-id (string-downcase mode)) (or group mode))
              (remove mode *language-servers* :key #'car :test #'string=)))
  (defhvar "Completions Function"
    "A function of a mark, point, that returns the completions of what is
     before it and a mark where what they complete starts."
    :mode mode :value 'lsp-completions)
  (defhvar "Completion Accept Function"
    "A function that puts a completion in: its text, and what else the
     server says it needs."
    :mode mode :value 'lsp-accept-completion)
  (defhvar "Completion Describe Function"
    "A function of a completion's text that returns what the server says of
     it."
    :mode mode :value 'lsp-describe-completion)
  (bind-key "LSP Find Definition" #k"meta-." :mode mode)
  (bind-key "LSP Find References" #k"meta-?" :mode mode)
  (bind-key "LSP Describe" #k"control-c control-d" :mode mode)
  (bind-key "LSP Code Action" #k"control-c control-a" :mode mode)
  (bind-key "LSP Find Symbol" #k"control-c control-s" :mode mode)
  (bind-key "LSP Next Diagnostic" #k"meta-n" :mode mode)
  (bind-key "LSP Previous Diagnostic" #k"meta-p" :mode mode)
  (bind-key "LSP Find Type Definition" #k"control-c control-t" :mode mode)
  (bind-key "LSP Incoming Calls" #k"control-c control-u" :mode mode)
  (bind-key "LSP Code Lens" #k"control-c control-l" :mode mode)
  (defhvar "Fold Ranges Function"
    "A function of a buffer that returns what can be folded in it."
    :mode mode :value 'lsp-fold-ranges)
  (defhvar "Signature Function"
    "A function of a mark, point, that shows the signature of the call point
     is in with SHOW-SIGNATURE, or does nothing."
    :mode mode :value 'lsp-signature)
  (when (find-menu mode)
    (add-menu-item mode :separator)
    (dolist (entry '(("Go to Definition" "LSP Find Definition")
                     ("Go to Declaration" "LSP Find Declaration")
                     ("Go to Type Definition" "LSP Find Type Definition")
                     ("Go to Implementation" "LSP Find Implementation")
                     ("Find References" "LSP Find References")
                     ("Callers" "LSP Incoming Calls")
                     ("What This Calls" "LSP Outgoing Calls")
                     ("Describe" "LSP Describe")
                     ("Fix or Refactor…" "LSP Code Action")
                     ("Find Symbol…" "LSP Find Symbol")
                     ("Next Error" "LSP Next Diagnostic")
                     ("Previous Error" "LSP Previous Diagnostic")
                     ("Rename…" "LSP Rename")
                     ("Format Buffer" "LSP Format Buffer")
                     ("Format Region" "LSP Format Region")
                     ("Do What Is Offered Here…" "LSP Code Lens")
                     ("Show or Hide Hints" "LSP Inlay Hints")
                     ("Show or Hide Lenses" "LSP Code Lenses")
                     ("Errors and Warnings" "LSP Diagnostics")))
      (add-menu-item mode entry)))
  mode)

(defun define-additional-language-server (mode name commands)
  "MODE's buffers have another server as well as the mode's own, called
   NAME and run by the first of COMMANDS that is installed and starts: a
   linter, say.  What it finds wrong is shown with what the mode's server
   finds, what it offers to do about it is offered with what that offers,
   and it is asked for what the mode's server cannot do, as formatting."
  (setf *additional-language-servers*
        (append (remove-if (lambda (entry)
                             (and (string= (first entry) mode) (string= (second entry) name)))
                           *additional-language-servers*)
                (list (list mode name commands))))
  name)

(define-language-server "C" '(("clangd")) :language-id "c")
(define-language-server "Python" '(("pyright-langserver" "--stdio")
                                   ("basedpyright-langserver" "--stdio")
                                   ("pylsp")
                                   ("jedi-language-server"))
  :language-id "python")
;;; Ruff, which lints and formats, beside whichever server knows the types.
(define-additional-language-server "Python" "ruff" '(("ruff" "server")))
(define-language-server "Shell Script" '(("bash-language-server" "start"))
  :language-id "shellscript")
(define-language-server "Pascal" '(("pasls")) :language-id "pascal")
(define-language-server "Rust" '(("rust-analyzer")) :language-id "rust")
(define-language-server "Go" '(("gopls")) :language-id "go")
;;; TypeScript's compiler, from version 7, is a server too, and is tried when
;;; typescript-language-server is missing or finds no TypeScript it can use
;;; (it wants one older than 7).  A project's JavaScript, TypeScript and TSX
;;; files have one server between them.
(defvar *typescript-servers* '(("typescript-language-server" "--stdio")
                               ("tsc" "--lsp" "--stdio")))
(define-language-server "JavaScript" *typescript-servers*
  :language-id "javascript" :group "TypeScript")
(define-language-server "TS" *typescript-servers*
  :language-id "typescript" :group "TypeScript")
(define-language-server "TSX" *typescript-servers*
  :language-id "typescriptreact" :group "TypeScript")
(define-language-server "JSON" '(("vscode-json-language-server" "--stdio")
                                 ("vscode-json-languageserver" "--stdio"))
  :language-id "json")
(define-language-server "YAML" '(("yaml-language-server" "--stdio")) :language-id "yaml")
