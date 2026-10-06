;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Structural editing and indentation in Lisp mode, from sexp-edit
;;; (vendor/sexp-edit), the library the Lisp Listener's REPL and its iOS
;;; editor use too, so that a key does the same thing in each.
;;;
;;; The library's commands take a string and an offset into it and answer a
;;; new string and offset, or NIL to decline.  RUN-SEXP-COMMAND gives one the
;;; text around point -- the outermost form point is in, or else what is
;;; before point back to the previous top-level form, and the form after it --
;;; and makes the difference as one deletion and one insertion, so that marks,
;;; undo and font marks see an ordinary edit.  The indenter is given the text
;;; from the same start to the line being indented.
;;;
(in-package :heml)


;;;; The text around a mark.

(defun sexp-window-start (mark)
  "A new right-inserting mark where the text the structural commands and the
   indenter are given begins: the outermost open parenthesis around MARK, or,
   at top level, the start of the top-level form before it, never further
   back than Lisp mode parses, nor before a REPL buffer's input."
  (let ((start (copy-mark mark :right-inserting)))
    (pre-command-parse-check start)
    (with-mark ((m mark))
      (cond ((backward-up-list m)
             (loop (move-mark start m)
                   (unless (backward-up-list m) (return)))
             ;; With what is written before it: '(a b) is data.
             (loop while (find (previous-character start) "'`,@#")
                   do (mark-before start)))
            (t
             (with-mark ((parse mark))
               (funcall (value parse-start-function) parse)
               (move-mark start parse)
               (when (and (top-level-offset (move-mark m mark) -1)
                          (mark> m start))
                 (move-mark start m))))))
    (when (heml-bound-p 'buffer-input-mark)
      (let ((input (value buffer-input-mark)))
        (when (and (eq (line-buffer (mark-line input)) (line-buffer (mark-line mark)))
                   (mark<= input mark)
                   (mark< start input))
          (move-mark start input))))
    start))

(defun sexp-window-end (start mark)
  "A new mark at the end of the text from START that a command at MARK is
   given: past the form START begins and the form after MARK, to the end of
   that line, or as far as Lisp mode parses when the form is not closed."
  (let ((end (copy-mark mark :left-inserting)))
    (with-mark ((m start))
      (cond ((form-offset m 1)
             (when (mark> m end) (move-mark end m)))
            (t (funcall (value parse-end-function) end))))
    (with-mark ((m mark))
      (when (and (form-offset m 1) (mark> m end))
        (move-mark end m)))
    (line-end end)))

(defun mark-text-column (mark)
  "MARK's column as sexp-edit counts it."
  (let ((sexp-edit:*indent-first-column* 0))
    (sexp-edit:text-column (line-string (mark-line mark)) (mark-charpos mark))))

(defmacro with-sexp-settings ((start) &body body)
  "Run BODY with sexp-edit's settings as the current buffer's: the column
   START is in, the tab width, the package to look operators up in."
  `(let* ((sexp-edit:*indent-first-column* (mark-text-column ,start))
          (sexp-edit:*tab-width* (value spaces-per-tab))
          (sexp-edit:*local-definers* (value lisp-indentation-local-definers))
          (sexp-edit:*indent-package*
            (let ((name (and (heml-bound-p 'current-package) (value current-package))))
              (and (stringp name) (find-package (canonical-case name))))))
     ,@body))

(defun replace-sexp-text (start old new)
  "Make the text OLD after START into NEW, as one deletion and one insertion
   of what differs."
  (let ((prefix (mismatch old new)))
    (when prefix
      (let* ((old-length (length old))
             (new-length (length new))
             (suffix (min (- old-length (or (mismatch old new :from-end t) 0))
                          (- (min old-length new-length) prefix))))
        (with-mark ((from start :right-inserting)
                    (to start :left-inserting))
          (character-offset from prefix)
          (move-mark to from)
          (character-offset to (- old-length prefix suffix))
          (delete-region (region from to))
          (insert-string from (subseq new prefix (- new-length suffix))))))))

(defun run-sexp-command (command &optional (point (current-point)))
  "Run the sexp-edit COMMAND on the text around POINT, make the change it
   answers and put POINT where it says.  True when it did; NIL, with nothing
   changed, when it declined."
  (let* ((start (sexp-window-start point))
         (end (sexp-window-end start point)))
    (unwind-protect
         (let ((text (region-to-string (region start end)))
               (offset (count-characters (region start point))))
           (multiple-value-bind (new new-offset)
               (with-sexp-settings (start)
                 (sexp-edit:run-paredit-command command text offset))
             (when new
               (replace-sexp-text start text new)
               (move-mark point start)
               (character-offset point new-offset)
               t)))
      (delete-mark start)
      (delete-mark end))))


;;;; Indentation.

(defhvar "Lisp Indentation Local Definers"
  "Forms with syntax like LABELS, MACROLET, etc.: the functions they define
   are indented as a DEFUN is."
  :value '("LABELS" "MACROLET" "FLET"))

;;; DEFINDENT -- Public.
;;;
;;; The table is sexp-edit's, which the Lisp Listener reads too.
;;;
(defun defindent (fname args)
  "Define Fname to have Args special arguments.  If args is null then remove
   any special arguments information."
  (check-type fname string)
  (check-type args (or null (integer 0)))
  (sexp-edit:defindent fname args))

;;; Heml's own forms.  Common Lisp's are in sexp-edit's table, and a macro
;;; loaded here says itself where its body starts, with &BODY.
;;;
(defindent "with-mark" 1)
(defindent "with-random-typeout" 1)
(defindent "with-pop-up-display" 1)
(defindent "defhvar" 1)
(defindent "hlet" 1)
(defindent "defcommand" 2)
(defindent "defattribute" 1)
(defindent "command-case" 1)
(defindent "with-input-from-region" 1)
(defindent "with-output-to-mark" 1)
(defindent "with-output-to-window" 1)
(defindent "do-strings" 1)
(defindent "save-for-undo" 1)
(defindent "do-alpha-chars" 1)
(defindent "do-headers-buffers" 1)
(defindent "do-headers-lines" 1)
(defindent "with-headers-mark" 1)
(defindent "frob" 1) ;cover silly FLET and MACROLET names for Rob and Bill.
(defindent "with-writable-buffer" 1)
(defindent "remote" 1)
(defindent "remote-value" 1)
(defindent "remote-value-bind" 3)
(defindent "with-lock-held" 1)
(defindent "with-gensyms" 1)

;;; LISP-INDENTATION -- Internal Interface.
;;;
(defun lisp-indentation (mark)
  "The column the line MARK begins should be indented to, by sexp-edit's
   rules, from the text back to the outermost form around it.  The second
   value is true when MARK is inside a string, a |symbol| or a block comment."
  (let ((start (sexp-window-start mark)))
    (unwind-protect
         (with-sexp-settings (start)
           (let ((text (region-to-string (region start mark)))
                 (offset (count-characters (region start mark))))
             (values (sexp-edit:indentation-at text offset)
                     (nth-value 1 (sexp-edit:innermost-open-paren text offset)))))
      (delete-mark start))))

;;; INDENT-FOR-LISP -- Internal.
;;;
;;; This is the value of "Indent Function" for "Lisp" mode.
;;;
(defun indent-for-lisp (mark)
  (line-start mark)
  (insert-lisp-indentation mark))

(defun insert-lisp-indentation (m)
  (delete-horizontal-space m)
  (indent-to-column m (lisp-indentation m)))

;;; LISP-INDENT-REGION -- Internal.
;;;
;;; Each line is indented from the text as the lines before it now are, so a
;;; line follows the one before it as that one was just indented.  A line
;;; that begins inside a string is part of the string, and is left alone, as
;;; Emacs leaves it.
;;;
(defun lisp-indent-region (region &optional (undo-text "Lisp region indenting"))
  (check-region-query-size region)
  (let* ((start (region-start region))
         (end (region-end region))
         (first-line (mark-line start))
         (last-line (mark-line end))
         (save1 (line-start (copy-mark start :right-inserting)))
         (save2 (line-end (copy-mark end :left-inserting)))
         (buf-region (region save1 save2))
         (undo-region (copy-region buf-region)))
    (with-mark ((bol start :left-inserting))
      (do ((line first-line (line-next line)))
          (nil)
        (line-start bol line)
        (multiple-value-bind (column quoted) (lisp-indentation bol)
          (unless quoted
            (delete-horizontal-space bol)
            (indent-to-column bol column)))
        (when (eq line last-line) (return nil))))
    (make-region-undo :twiddle undo-text buf-region undo-region)))

(defcommand "Defindent" (p)
  "Define the Lisp indentation for the current function.
  The indentation is a non-negative integer which is the number
  of special arguments for the form.  Examples: 2 for Do, 1 for Dolist.
  If a prefix argument is supplied, then delete the indentation information."
  "Do a defindent, man!"
  (with-mark ((m (current-point)))
    (pre-command-parse-check m)
    (unless (backward-up-list m) (editor-error))
    (mark-after m)
    (with-mark ((n m))
      (scan-char n :lisp-syntax (not :constituent))
      (let ((s (region-to-string (region m n))))
        (declare (simple-string s))
        (when (zerop (length s)) (editor-error))
        (if p
            (defindent s nil)
            (let ((i (prompt-for-integer
                      :prompt (format nil "Indentation for ~A: " s)
                      :help "Number of special arguments.")))
              (when (minusp i)
                (editor-error "Indentation must be non-negative."))
              (defindent s i))))))
  (indent-command nil))

(defcommand "Indent Form" (p)
  "Indent Lisp code in the next form."
  "Indent Lisp code in the next form."
  (declare (ignore p))
  (let ((point (current-point)))
    (pre-command-parse-check point)
    (with-mark ((m point))
      (unless (form-offset m 1) (editor-error))
      (lisp-indent-region (region point m) "Indent Form"))))


;;;; Balanced insertion and deletion.

(defhvar "Lisp Structural Editing"
  "When true, ( and \" in Lisp mode put in a pair, ) steps over the closing
   parenthesis already there, and the structural commands (\"Wrap Form\",
   \"Splice Form\", \"Slurp Forward\" and the rest) work, as in the Lisp
   Listener and as Emacs's paredit does.  Inside a string or a comment each is
   just a character."
  :mode "Lisp" :value t)

(defhvar "Lisp Keep Parens Balanced"
  "When true, Backspace and Delete in Lisp mode leave a list's parentheses
   with it, as Emacs's paredit does: Backspace after a closing parenthesis
   moves inside the list and after an opening one out of it, Delete the other
   way, and in an empty list either takes both away; a string's quotes are
   kept the same way.  An unmatched parenthesis, or one in a string or a
   comment, is a character like any other.  When NIL, they delete a
   character."
  :mode "Lisp" :value t)

(defun structural-p (p)
  (and (not p) (value lisp-structural-editing)))

(defcommand "Lisp Insert (" (p)
  "Insert a pair of parentheses, point between them, while \"Lisp Structural
   Editing\" is true; in a string or a comment, or with an argument, just the
   character typed."
  "Insert a pair of parentheses."
  (if (and (structural-p p) (run-sexp-command 'sexp-edit:insert-pair))
      (invoke-hook self-insert-hook)
      (self-insert-command p)))

(defcommand "Lisp Insert \"" (p)
  "Insert a pair of double quotes, point between them, while \"Lisp
   Structural Editing\" is true; before a string's closing quote, step over
   it; inside a string, an escaped quote.  In a comment, or with an argument,
   just the character typed."
  "Insert a pair of double quotes."
  (if (and (structural-p p) (run-sexp-command 'sexp-edit:insert-quote))
      (invoke-hook self-insert-hook)
      (self-insert-command p)))

;;; "Paren Pause Period" is defined in lispmode.lisp.
;;;
(defcommand "Lisp Insert )" (p)
  "Step over the closing parenthesis after point while \"Lisp Structural
   Editing\" is true; otherwise insert a \")\" and briefly position the cursor
   at the matching \"(\"."
  "Step over or insert a \")\"."
  (let ((point (current-point)))
    (cond ((and (structural-p p) (run-sexp-command 'sexp-edit:close-or-skip))
           (invoke-hook self-insert-hook))
          (t
           (insert-character point #\))
           (invoke-hook self-insert-hook)
           (pre-command-parse-check point)
           (when (valid-spot point nil)
             (with-mark ((m point))
               (if (list-offset m -1)
                   (let ((pause (value paren-pause-period))
                         (win (current-window)))
                     (if pause
                         (unless (show-mark m win pause)
                           (clear-echo-area)
                           (message "~A" (line-string (mark-line m))))
                         (unless (displayed-p m (current-window))
                           (clear-echo-area)
                           (message "~A" (line-string (mark-line m))))))
                   (editor-error))))))))

(defcommand "Lisp Delete Previous Character" (p)
  "Delete the character before point, but for a list's parentheses and a
   string's quotes while \"Lisp Keep Parens Balanced\" is true: after a
   closing one move inside, after an opening one move out, and in an empty
   list or string take both away.  With an argument, delete that many
   characters."
  "Delete the character before point, keeping parentheses balanced."
  (unless (and (not p) (value lisp-keep-parens-balanced)
               (run-sexp-command 'sexp-edit:delete-pair-backward))
    (delete-previous-character-expanding-tabs-command p)))

(defcommand "Lisp Delete Next Character" (p)
  "Delete the character after point, but for a list's parentheses and a
   string's quotes while \"Lisp Keep Parens Balanced\" is true: before an
   opening one move inside, before a closing one move out, and in an empty
   list or string take both away.  With an argument, delete that many
   characters."
  "Delete the character after point, keeping parentheses balanced."
  (unless (and (not p) (value lisp-keep-parens-balanced)
               (run-sexp-command 'sexp-edit:delete-pair-forward))
    (delete-next-character-command p)))


;;;; Structure.

(macrolet ((define-structural (name command documentation)
             `(defcommand ,name (p)
                ,documentation ,documentation
                (declare (ignore p))
                (unless (value lisp-structural-editing)
                  (editor-error "\"Lisp Structural Editing\" is off."))
                (unless (run-sexp-command ',command)
                  (editor-error)))))
  (define-structural "Wrap Form" sexp-edit:wrap-round
    "Put the form at point in a new list, point after its open parenthesis.")
  (define-structural "Splice Form" sexp-edit:splice
    "Take away the parentheses of the list point is in, leaving its forms.")
  (define-structural "Raise Form" sexp-edit:raise-sexp
    "Put the form at point in place of the list it is in.")
  (define-structural "Slurp Forward" sexp-edit:slurp-forward
    "Bring the form after the list point is in into it, at its end.")
  (define-structural "Barf Forward" sexp-edit:barf-forward
    "Put the last form of the list point is in out after it.")
  (define-structural "Slurp Backward" sexp-edit:slurp-backward
    "Bring the form before the list point is in into it, at its start.")
  (define-structural "Barf Backward" sexp-edit:barf-backward
    "Put the first form of the list point is in out before it."))


;;;; The corpus of edits.

(defun sexp-corpus-failures (cases)
  "Replay sexp-edit's corpus of edits (sexp-edit-tests:*edit-cases*), each
   through RUN-SEXP-COMMAND in a Lisp-mode buffer made current for it, and say how many differ
   from what the library answers on a string: the smoke tests' check that
   Heml edits as the Lisp Listener does.  A string, for the echo area."
  (let ((buffer (make-buffer "  *sexp corpus*" :modes '("Lisp")))
        (previous (current-buffer))
        (failures '()))
    (flet ((render (text offset)
             (substitute #\/ #\Newline
                         (concatenate 'string (subseq text 0 offset) "|"
                                      (subseq text offset)))))
      (unwind-protect
           (dolist (case cases)
             (setf (current-buffer) buffer)
             (destructuring-bind (command before after label) case
               (let ((point (buffer-point buffer)))
                 (delete-region (buffer-region buffer))
                 (insert-string point (substitute #\Newline #\/ (remove #\| before)))
                 (buffer-start point)
                 (character-offset point (position #\| before))
                 (let* ((done (run-sexp-command command point))
                        (got (if done
                                 (render (region-to-string (buffer-region buffer))
                                         (count-characters
                                          (region (buffer-start-mark buffer) point)))
                                 :declined)))
                   (unless (equal got after)
                     (push (format nil "~A: ~S => ~S" label before got) failures))))))
        (setf (current-buffer) previous)
        (delete-buffer buffer)))
    (format nil "sexp corpus: ~D cases, ~D failed~{; ~A~}"
            (length cases) (length failures) (reverse failures))))
