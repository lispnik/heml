;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Keeping a language server's copy of each buffer the same as the buffer
;;; (lsp.lisp): opening, changes, saving, closing; asking a server that
;;; waits to be asked what is wrong; and LSP-IDLE, twice a second, which
;;; does all that for the buffers, and LSP-CURRENT-SERVER, which every
;;; command starts with.

(in-package :heml)


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

;;; A change is told as the lines that changed.  What the server was last
;;; told is the buffer's lines then, and the string of each: a line's
;;; characters are a new string whenever they change (the open line's are a
;;; number that changes with it), so a line whose string is the same object
;;; is unchanged.  Finding the change is going through the lines from each
;;; end while they are the same -- no text is copied or compared -- and
;;; telling it needs no column but the end of the last line.  Copying the
;;; whole buffer and comparing it with the last copy, as was done, took a
;;; tenth of a second and more, each time typing paused, in a long file.

(defun line-text (line)
  "LINE's characters, as a string: the open line's are closed into one."
  (let ((chars (hi::line-chars line)))
    (if (stringp chars) chars (progn (line-string line) (hi::line-chars line)))))

(defun buffer-snapshot (buffer)
  "BUFFER's lines, and their strings, as two vectors."
  (let ((count (count-lines (buffer-region buffer))))
    (let ((lines (make-array count))
          (strings (make-array count)))
      (loop for line = (mark-line (buffer-start-mark buffer)) then (line-next line)
            for i from 0
            while (and line (< i count))
            do (setf (svref lines i) line
                     (svref strings i) (line-text line)))
      (values lines strings))))

(defun line-change (document buffer)
  "How BUFFER differs from what DOCUMENT says the server was last told: the
   span of the old text, as lines and characters, and the text that
   replaces it -- or NIL, when it does not.  DOCUMENT is then the buffer as
   it is."
  (let* ((old-lines (document-lines document))
         (old-strings (document-strings document))
         (n (length old-lines))
         (last (mark-line (buffer-end-mark buffer)))
         (p 0)
         (line (mark-line (buffer-start-mark buffer)))
         (before nil))
    (flet ((same (line i)
             (and (eq line (svref old-lines i))
                  (eq (hi::line-chars line) (svref old-strings i))))
           (old-end (i)
             (unit-offset (svref old-strings i) (length (svref old-strings i)))))
      ;; The same at the start...
      (loop while (and line (< p n) (same line p))
            do (setf before line
                     line (line-next line))
               (incf p))
      ;; ... and at the end, not into what is the same at the start.
      (let ((q 0) (back last))
        (loop while (and back line (not (eq back before)) (< (+ p q) n)
                         (same back (- n 1 q)))
              do (incf q)
                 (setf back (line-previous back)))
        (let* ((middle (when (and line (not (eq back before)))
                         (loop for x = line then (line-next x)
                               collect x
                               until (or (eq x back) (null (line-next x))))))
               (k (length middle))
               (j (- n p q))
               (texts (mapcar #'line-text middle)))
          (when (or (plusp j) (plusp k))
            ;; What the server is now told.
            (setf (document-lines document)
                  (concatenate 'simple-vector (subseq old-lines 0 p) middle
                               (subseq old-lines (- n q)))
                  (document-strings document)
                  (concatenate 'simple-vector (subseq old-strings 0 p)
                               (mapcar #'hi::line-chars middle)
                               (subseq old-strings (- n q))))
            (flet ((joined () (format nil "~{~A~^~%~}" texts)))
              (cond ((plusp q)
                     (values p 0 (+ p j) 0 (format nil "~{~A~%~}" texts)))
                    ((and (plusp j) (plusp k))
                     (values p 0 (1- n) (old-end (1- n)) (joined)))
                    ((plusp j)
                     ;; Lines gone from the end, and the newline before them.
                     (values (1- p) (old-end (1- p)) (1- n) (old-end (1- n)) ""))
                    (t
                     ;; Lines added at the end.
                     (values (1- n) (old-end (1- n)) (1- n) (old-end (1- n))
                             (format nil "~%~A" (joined))))))))))))

(defun lsp-sync (server buffer)
  "Tell SERVER of BUFFER, or, if its text has changed, of the change: the
   lines that changed, for a server that takes changes, and the whole text
   for one that does not."
  (when (eq (lsp-server-state server) :ready)
    (let ((document (gethash buffer (lsp-server-documents server)))
          (signature (buffer-signature buffer))
          (incremental (incremental-sync-p server))
          (*encoding* (lsp-server-encoding server)))
      (cond ((null document)
             (let ((text (region-to-string (buffer-region buffer)))
                   (document (make-document 1 signature)))
               (when incremental
                 (multiple-value-bind (lines strings) (buffer-snapshot buffer)
                   (setf (document-lines document) lines
                         (document-strings document) strings)))
               (setf (gethash buffer (lsp-server-documents server)) document)
               (lsp-notify server "textDocument/didOpen"
                           (json "textDocument"
                                 (json "uri" (file-uri (buffer-pathname buffer))
                                       "languageId" (or (mode-language-id (buffer-major-mode buffer))
                                                        (mode-language-id (lsp-server-mode server)))
                                       "version" 1
                                       "text" text)))))
            ((not (eql (document-signature document) signature))
             (setf (document-signature document) signature)
             (let ((change
                     (if incremental
                         (multiple-value-bind (start-line start-character
                                               end-line end-character inserted)
                             (line-change document buffer)
                           ;; A signature changes for more than text: say
                           ;; nothing then.
                           (when inserted
                             (json "range" (json "start" (json "line" start-line
                                                               "character" start-character)
                                                 "end" (json "line" end-line
                                                             "character" end-character))
                                   "text" inserted)))
                         (json "text" (region-to-string (buffer-region buffer))))))
               (when change
                 (lsp-notify
                  server "textDocument/didChange"
                  (json "textDocument" (json "uri" (file-uri (buffer-pathname buffer))
                                             "version" (incf (document-version document)))
                        "contentChanges" (vector change)))
                 (setf (document-pull document) t)
                 ;; What is wrong with the server's other files may have
                 ;; changed with this one.
                 (when (jref (lsp-server-capabilities server)
                             "diagnosticProvider" "interFileDependencies")
                   (loop for other being the hash-values of (lsp-server-documents server)
                         do (setf (document-pull other) t)))))))
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
   current buffer and each of its servers that is ready and knows the
   buffer as it is.")

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
     (dolist (server (buffer-language-servers buffer))
       (when (eq (lsp-server-state server) :ready)
         (let ((*encoding* (lsp-server-encoding server)))
           (dolist (function *lsp-idle-functions*)
             (ignore-errors (funcall function server buffer))))))
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
