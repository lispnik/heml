;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Claude Code in a terminal of its own, at the project's root, with Heml as
;;; its IDE (claude-ide.lisp): "Claude" (C-c a a) runs it, or goes back to
;;; it; "Claude Send Region" (C-c a r) and "Claude Send File" (C-c a f) tell
;;; it what is meant -- as a mention through the IDE's connection when it
;;; has one, or else typed into its prompt as an @ reference -- and leave the
;;; rest of the prompt to be written there.

(in-package :heml)

(defhvar "Claude Program"
  "The command \"Claude\" runs: Claude Code's, found in the PATH."
  :value "claude")

(defhvar "Claude Config Directory"
  "Claude Code's user directory, as CLAUDE_CONFIG_DIR names it, for the
   Claude Heml runs and the lock file it writes for it there: NIL for
   Claude Code's own default, $CLAUDE_CONFIG_DIR or ~/.claude/.  Set it in
   the init file for every project, or in a project's .heml-project, as
   (:variables ((\"Claude Config Directory\" . \"~/.claude-work/\"))), for one."
  :value nil)

(defun claude-config-directory (root)
  "The Claude user directory for the project at ROOT: its .heml-project's,
   else the variable's, else Claude Code's default."
  (let ((value (or (cdr (assoc "Claude Config Directory"
                               (getf (ignore-errors (project-settings root)) :variables)
                               :test #'string-equal))
                   (value claude-config-directory))))
    (if (and value (plusp (length (string value))))
        (uiop:ensure-directory-pathname (heml-ext:expand-file-name (string value)))
        (default-claude-config-directory))))

(defun claude-root ()
  "Where Claude runs for the current buffer: its project's root, or its
   directory."
  (let ((directory (default-directory)))
    (uiop:ensure-directory-pathname (or (project-root directory) directory))))

(defun claude-buffer-name (root)
  (format nil "*claude ~A*" (car (last (pathname-directory root)))))

(defun claude-term-buffer (root)
  "The buffer of the Claude running at ROOT, when one still runs."
  (let ((buffer (getstring (claude-buffer-name root) *buffer-names*)))
    (when buffer
      (let ((term (buffer-term buffer)))
        (if (and term (null (term-exit-code term)))
            buffer
            nil)))))

(defun show-in-other-window (buffer)
  (let ((window (car (buffer-windows buffer))))
    (if window
        (select-window window)
        (progn
          (when (null (rest (remove *echo-area-window* *window-list*)))
            (split-window-command nil))
          (select-window (other-window))
          (change-to-buffer buffer)))))

(defcommand "Claude" (p)
  "Run Claude Code in a terminal beside this window, at this buffer's
   project's root, with Heml as its IDE; or go back to the one running
   there.  With an argument, ask what to run."
  "Run Claude Code."
  (let* ((root (claude-root))
         (running (claude-term-buffer root)))
    (if running
        (show-in-other-window running)
        (let* ((command (if p
                            (prompt-for-string :prompt "Run: " :default (value claude-program))
                            (value claude-program)))
               (words (cl-ppcre:split "\\s+" (string-trim " " command))))
          (unless (find-program (first words))
            (editor-error "~A is not installed." (first words)))
          (let* ((config (claude-config-directory root))
                 (environment
                   `(("CLAUDE_CONFIG_DIR" . ,(string-right-trim "/" (namestring config)))
                     ,@(when (value claude-ide-server)
                         (let ((port (ensure-ide-server root config)))
                           `(("CLAUDE_CODE_SSE_PORT" . ,(princ-to-string port))
                             ("ENABLE_IDE_INTEGRATION" . "true")))))))
            (when (null (rest (remove *echo-area-window* *window-list*)))
              (split-window-command nil))
            (select-window (other-window))
            (make-term words (namestring root)
                       :environment environment
                       :name (let ((name (claude-buffer-name root)))
                               (if (getstring name *buffer-names*)
                                   (loop for i from 2
                                         for candidate = (format nil "~A<~D>" name i)
                                         unless (getstring candidate *buffer-names*)
                                           return candidate)
                                   name))))))))

(defun claude-for-sending ()
  "The terminal of the Claude running at this buffer's project's root."
  (let ((buffer (claude-term-buffer (claude-root))))
    (unless buffer
      (editor-error "No Claude runs here: C-c a a starts one."))
    (buffer-term buffer)))

(defun mention-text (pathname root start-line end-line)
  "The @ reference Claude Code reads for the lines START-LINE to END-LINE,
   from 1, of PATHNAME, relative to ROOT."
  (let ((name (enough-namestring pathname root)))
    (cond ((null start-line) (format nil "@~A " name))
          ((= start-line end-line) (format nil "@~A#L~D " name start-line))
          (t (format nil "@~A#L~D-~D " name start-line end-line)))))

(defun claude-mention (start-line end-line)
  "Tell the Claude running here about the current buffer's lines START-LINE
   to END-LINE (from 0), or the whole file when they are NIL."
  (let* ((buffer (current-buffer))
         (pathname (buffer-pathname buffer))
         (term (claude-for-sending)))
    (cond ((and pathname start-line (ide-connected-p))
           (ide-mention pathname start-line end-line)
           (message "Lines ~D to ~D of ~A told to Claude." (1+ start-line) (1+ end-line)
                    (file-namestring pathname)))
          (pathname
           (term-paste-string term (mention-text pathname (claude-root)
                                                 (and start-line (1+ start-line))
                                                 (and end-line (1+ end-line))))
           (message "~A put in Claude's prompt." (file-namestring pathname)))
          ((region-active-p)
           (term-paste-string term (region-to-string (current-region)))
           (message "The region put in Claude's prompt."))
          (t (editor-error "This buffer has no file.")))))

(defcommand "Claude Send Region" (p)
  "Tell the Claude running at this project's root about the region's lines,
   or point's line: through its connection to Heml when it has one, or
   else as an @ reference in its prompt.  A buffer without a file sends the
   region's text."
  "Tell Claude about the region."
  (declare (ignore p))
  (let ((region (and (region-active-p) (current-region nil nil))))
    (if region
        (claude-mention (line-number-in-buffer (region-start region))
                        (let ((end (region-end region)))
                          ;; A region ending at a line's start ends on the line before.
                          (max (line-number-in-buffer (region-start region))
                               (- (line-number-in-buffer end)
                                  (if (and (zerop (mark-charpos end))
                                           (not (mark= end (region-start region))))
                                      1 0)))))
        (let ((line (line-number-in-buffer (current-point))))
          (claude-mention line line)))))

(defcommand "Claude Send File" (p)
  "Put an @ reference to this buffer's file in the prompt of the Claude
   running at this project's root."
  "Tell Claude about this file."
  (declare (ignore p))
  (claude-mention nil nil))

(bind-key "Claude" #k"control-c a a")
(bind-key "Claude Send Region" #k"control-c a r")
(bind-key "Claude Send File" #k"control-c a f")

(add-menu-item "Tools" :separator)
(add-menu-item "Tools" '("Claude" "Claude"))
(add-menu-item "Tools" '("Send Region to Claude" "Claude Send Region"))
(add-menu-item "Tools" '("Send File to Claude" "Claude Send File"))
