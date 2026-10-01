;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Completion at point.  The completions of what is before point are shown
;;; in a popup under it, a few rows laid over the window (winimage.lisp), in
;;; every device alike.  C-n and C-p or the arrows choose, Return or Tab
;;; puts the choice in, typing narrows the list, and any other key puts the
;;; popup away and does what it does.
;;;
;;; What completes a word is its mode's: Lisp's symbols come from the slave,
;;; or the editor's own image, and from the buffers; a shell's are file
;;; names; elsewhere the words of the buffers do.  A completion is a string,
;;; or (STRING . NOTE), the note shown beside it: a symbol's kind.

(in-package :heml)

(defparameter *popup-font* '(:fg 0 :bg 7))
(defparameter *popup-selected-font* '(:fg 7 :bg 4 :bold t))
(defparameter *popup-rows* 10
  "The most completions the popup shows at once.")
(defparameter *popup-width* 70)

(defun candidate-text (candidate)
  (if (consp candidate) (car candidate) candidate))

(defun candidate-note (candidate)
  (and (consp candidate) (cdr candidate)))


;;;; The popup.

(defun popup-rows (items selected first width name-width)
  (loop for item in (nthcdr first items)
        for index from first
        repeat *popup-rows*
        collect (cons (let ((text (make-string width :initial-element #\Space))
                            (note (candidate-note item)))
                        (replace text (candidate-text item) :start1 1 :end1 (1- width))
                        (when note
                          (replace text note :start1 (min (1- width) (+ 3 name-width))
                                             :end1 (1- width)))
                        text)
                      (if (= index selected) *popup-selected-font* *popup-font*))))

(defun bottom-window ()
  "The window, other than the echo area's, lowest and leftmost on the screen."
  (let ((best nil))
    (dolist (window *window-list* best)
      (unless (eq window *echo-area-window*)
        (let ((hunk (window-hunk window)))
          (when (or (null best)
                    (> (hi::device-hunk-position hunk)
                       (hi::device-hunk-position (window-hunk best)))
                    (and (= (hi::device-hunk-position hunk)
                            (hi::device-hunk-position (window-hunk best)))
                         (< (hi::device-hunk-column hunk)
                            (hi::device-hunk-column (window-hunk best)))))
            (setf best window)))))))

(defun show-popup (items selected mark &optional window)
  "Show ITEMS in a popup, the SELECTED one marked, as many as fit and the
   popup's rows allow: under MARK in the current window, or, given WINDOW,
   at its foot."
  (let* ((over (or window (current-window)))
         (height (window-height over))
         (count (min (length items) *popup-rows*))
         (name-width (reduce #'max items :key (lambda (i) (length (candidate-text i)))))
         (note-width (reduce #'max items :key (lambda (i) (length (candidate-note i)))))
         (width (min *popup-width* (window-width over)
                     (+ 2 name-width (if (plusp note-width) (+ 2 note-width) 0)))))
    (multiple-value-bind (x y)
        (if window
            (values 1 nil)
            (hi::mark-to-cursorpos mark over))
      (when x
        (let* ((below (if window 0 (- height y 1)))
               (above (if window height y))
               ;; Under the line, or over it where there is more room.
               (under (and (not window) (or (>= below count) (>= below above))))
               (rows (min count (if under below above)))
               (first (max 0 (min (- selected (floor rows 2)) (- (length items) rows)))))
          (when (plusp rows)
            (let ((*popup-rows* rows))
              (setf hi::*popup*
                    (hi::make-popup over
                                    (max 0 (min (1- x) (- (window-width over) width)))
                                    (cond (window (- height rows))
                                          (under (1+ y))
                                          (t (- y rows)))
                                    (popup-rows items selected first width name-width))))))))))

(defun popup-choose (candidates start &key window (accept (list #k"return" #k"tab" #k"control-i")))
  "Let the user choose among CANDIDATES, completions of the text from the
   mark START to point, which typing narrows.  Returns the text of the one
   chosen, or NIL when the popup is put away without one.  The popup is
   under START, or at the foot of WINDOW; the keys ACCEPT put the choice in."
  (let ((point (current-point))
        (selected 0))
    (unwind-protect
         (loop
           (let* ((typed (region-to-string (region start point)))
                  (items (remove-if-not
                          (lambda (c)
                            (let ((text (candidate-text c)))
                              (and (>= (length text) (length typed))
                                   (string-equal typed text :end2 (length typed)))))
                          candidates)))
             (when (or (null items) (mark< point start))
               (return nil))
             (setf selected (max 0 (min selected (1- (length items)))))
             ;; Once without it, so that the place it goes is on the screen.
             (setf hi::*popup* nil)
             (redisplay)
             (show-popup items selected start window)
             (redisplay)
             (let* ((key (get-key-event hi::*editor-input*))
                    (char (heml-ext:key-event-char key)))
               (cond ((member key (list #k"control-n" #k"downarrow"))
                      (setf selected (mod (1+ selected) (length items))))
                     ((member key (list #k"control-p" #k"uparrow"))
                      (setf selected (mod (1- selected) (length items))))
                     ((member key accept)
                      (return (candidate-text (nth selected items))))
                     ((member key (list #k"control-g" #k"escape"))
                      (return nil))
                     ((member key (list #k"backspace" #k"delete"))
                      (when (mark<= point start) (return nil))
                      (delete-characters point -1))
                     ((and char (graphic-char-p char) (not (char= char #\Space))
                           (zerop (logandc2 (heml-ext:key-event-bits key)
                                            (heml-ext:key-event-modifier-mask "Shift"))))
                      (insert-character point char))
                     (t
                      ;; Anything else is not for the popup.
                      (unget-key-event key hi::*editor-input*)
                      (return nil))))))
      (setf hi::*popup* nil))))


;;;; What completes a word.

(defun lisp-symbol-char-p (char)
  (not (or (member char '(#\Space #\Tab #\Newline #\( #\) #\' #\` #\, #\; #\" #\#))
           (char= char #\|))))

(defun word-char-p (char)
  (or (alphanumericp char) (char= char #\_)))

(defun token-start (point char-p)
  "A mark where the token ending at POINT starts."
  (let ((start (copy-mark point :right-inserting)))
    (loop for char = (previous-character start)
          while (and char (funcall char-p char))
          do (mark-before start))
    start))

(defparameter *completion-line-limit* 20000
  "The most lines of other buffers searched for words to complete with.")

(defun buffer-tokens (prefix char-p)
  "The tokens in the buffers that start with PREFIX, the current buffer's
   first, without PREFIX itself."
  (let ((found (make-hash-table :test 'equal))
        (tokens '())
        (budget *completion-line-limit*)
        (length (length prefix)))
    (flet ((scan (buffer limited)
             (do ((line (mark-line (buffer-start-mark buffer)) (line-next line)))
                 ((or (null line) (and limited (minusp (decf budget)))))
               (let ((string (line-string line)) (at 0))
                 (loop
                   (let ((hit (search prefix string :start2 at :test #'char-equal)))
                     (unless hit (return))
                     (if (and (plusp hit) (funcall char-p (char string (1- hit))))
                         (setf at (1+ hit))
                         (let* ((end (or (position-if-not char-p string :start hit)
                                         (length string)))
                                (token (subseq string hit end)))
                           (when (and (> (length token) length)
                                      (not (gethash token found)))
                             (setf (gethash token found) t)
                             (push token tokens))
                           (setf at (max end (1+ hit)))))))))))
      (when (plusp length)
        (scan (current-buffer) nil)
        (dolist (buffer *buffer-list*)
          (unless (or (eq buffer (current-buffer)) (eq buffer *echo-area-buffer*))
            (scan buffer t)))))
    (nreverse tokens)))

(defun word-completions (point)
  "Completions of the word before POINT, and where it starts."
  (let ((start (token-start point #'word-char-p)))
    (values (sort (buffer-tokens (region-to-string (region start point)) #'word-char-p)
                  #'string-lessp)
            start)))

;;; Lisp's symbols, with what each names.  %SYMBOL-COMPLETIONS runs in the
;;; slave, which is this program too, or in the editor.

(defun symbol-kind (symbol)
  (cond ((keywordp symbol) "keyword")
        ((special-operator-p symbol) "special form")
        ((macro-function symbol) "macro")
        ((fboundp symbol)
         (if (typep (fdefinition symbol) 'generic-function) "generic" "function"))
        ((find-class symbol nil) "class")
        ((boundp symbol) (if (constantp symbol) "constant" "variable"))
        (t nil)))

(defun %symbol-completions (package-name prefix)
  "The symbols accessible in the package named that start with PREFIX, as
   ((NAME . KIND) ...), names in lower case."
  (let ((package (or (find-package (canonical-case package-name))
                     (find-package :common-lisp-user)))
        (prefix (string-downcase prefix))
        (seen (make-hash-table :test 'equal))
        (found '()))
    (do-symbols (symbol package)
      (let ((name (string-downcase (symbol-name symbol))))
        (when (and (starts-with-p name prefix) (not (gethash name seen)))
          (setf (gethash name seen) t)
          (push (cons name (symbol-kind symbol)) found))))
    found))

(defun lisp-symbol-completions (point)
  "Completions of the symbol before POINT -- the slave's or the editor's
   symbols, each with its kind, and the buffers' -- and where its name
   starts, after any package prefix."
  (let* ((token-start (token-start point #'lisp-symbol-char-p))
         (token (region-to-string (region token-start point)))
         (colon (position #\: token :from-end t))
         (package (and colon (string-right-trim ":" (subseq token 0 colon))))
         (name (string-downcase (if colon (subseq token (1+ colon)) token)))
         (start (copy-mark token-start :right-inserting)))
    (when colon (character-offset start (1+ colon)))
    (delete-mark token-start)
    (let* ((package-name (if (and package (plusp (length package)))
                             package
                             (or (ignore-errors (package-at-point)) "COMMON-LISP-USER")))
           (symbols
             (when (plusp (length name))
               (or (let ((info (value current-eval-server)))
                     (when info
                       (ignore-errors
                        (eval-form-in-server-1
                         info
                         (format nil "(heml::%symbol-completions ~S ~S)" package-name name)))))
                   (ignore-errors (%symbol-completions package-name name)))))
           (symbols (remove name symbols :key #'car :test #'string=))
           (tokens (unless colon
                     (remove-if (lambda (token) (assoc token symbols :test #'string=))
                                (remove-duplicates
                                 (mapcar #'string-downcase
                                         (buffer-tokens name #'lisp-symbol-char-p))
                                 :test #'string=)))))
      (values (sort (append symbols tokens) #'string-lessp :key #'candidate-text)
              start))))

(defun editor-symbol-completions (point)
  "As LISP-SYMBOL-COMPLETIONS, of the editor's own symbols whatever slave
   there is."
  (hlet ((current-eval-server nil))
    (lisp-symbol-completions point)))

;;; File names, in a shell's buffer: what is before point names a file in
;;; the shell's directory, or wherever its directories say.

(defun file-name-char-p (char)
  (not (member char '(#\Space #\Tab #\" #\' #\< #\> #\| #\; #\& #\( #\) #\= #\:))))

(defun directory-entries (directory)
  "The names in DIRECTORY, a directory's with a / after it."
  (ignore-errors
   (append (mapcar (lambda (d) (concatenate 'string (car (last (pathname-directory d))) "/"))
                   (uiop:subdirectories directory))
           (mapcar #'file-namestring (uiop:directory-files directory)))))

(defun file-name-completions (point)
  "Completions of the file name before POINT, and where its last component
   starts; the words of the buffers when it names no file."
  (let* ((token-start (token-start point #'file-name-char-p))
         (token (region-to-string (region token-start point)))
         (expanded (heml-ext:expand-file-name token))
         (slash (position #\/ token :from-end t))
         (base (if slash (subseq token (1+ slash)) token))
         (directory (uiop:ensure-directory-pathname
                     (merge-pathnames
                      (let ((eslash (position #\/ expanded :from-end t)))
                        (if eslash (subseq expanded 0 (1+ eslash)) ""))
                      (uiop:ensure-directory-pathname
                       (if (heml-bound-p 'current-working-directory)
                           (value current-working-directory)
                           (default-directory))))))
         (names (sort (remove-if-not (lambda (name)
                                       (and (> (length name) (length base))
                                            (string= base name :end2 (length base))))
                                     (directory-entries directory))
                      #'string<)))
    (cond (names
           (let ((start (copy-mark token-start :right-inserting)))
             (when slash (character-offset start (1+ slash)))
             (delete-mark token-start)
             (values names start)))
          (t
           (delete-mark token-start)
           (word-completions point)))))

(defhvar "Completions Function"
  "A function of a mark, point, that returns the completions of what is
   before it and a mark where what they complete starts."
  :value 'word-completions)

(defhvar "Completions Function"
  "A function of a mark, point, that returns the completions of what is
   before it and a mark where what they complete starts."
  :mode "Lisp" :value 'lisp-symbol-completions)

(defhvar "Completions Function"
  "A function of a mark, point, that returns the completions of what is
   before it and a mark where what they complete starts."
  :mode "Editor" :value 'editor-symbol-completions)

(defhvar "Completions Function"
  "A function of a mark, point, that returns the completions of what is
   before it and a mark where what they complete starts."
  :mode "Process" :value 'file-name-completions)

(defun complete-at-point (&optional p automatic)
  "Complete what is before point from a popup of its completions.  When
   AUTOMATIC, the popup has come up unasked: only Tab puts a choice in,
   nothing is said when there are no completions, and one alone is offered
   rather than put in."
  (declare (ignore p))
  (let ((point (current-point)))
    (multiple-value-bind (candidates start) (funcall (value completions-function) point)
      (unwind-protect
           (let ((typed (region-to-string (region start point))))
             (cond ((null candidates)
                    (unless automatic (message "No completions of ~S." typed)))
                   (t
                    (let ((choice (cond ((or automatic (rest candidates))
                                         (if automatic
                                             (popup-choose candidates start
                                                           :accept (list #k"tab" #k"control-i"))
                                             (popup-choose candidates start)))
                                        (t (candidate-text (first candidates))))))
                      (when choice
                        (delete-region (region start point))
                        (insert-string point choice))))))
        (delete-mark start)))))

(defcommand "Complete at Point" (p)
  "Complete what is before point: its completions are shown in a popup
   under it, where C-n and C-p or the arrows choose, Return or Tab puts the
   choice in, typing narrows them, and anything else puts the popup away.
   One completion alone is put in at once."
  "Complete what is before point, from a popup."
  (complete-at-point p))


;;;; Completions as one types.

(defhvar "Complete as You Type"
  "When true, the popup of completions comes up by itself once a word is
   \"Complete as You Type Length\" characters long.  Tab puts the choice in;
   Return, and every other key, does what it does without the popup."
  :value nil)

(defhvar "Complete as You Type Length"
  "How many characters of a word are typed before its completions are
   offered, when \"Complete as You Type\" is on."
  :value 3)

(defun maybe-complete-as-typed ()
  "After a character is typed: offer the completions of the word it is in."
  (when (and (value complete-as-you-type)
             (not (eq (current-buffer) *echo-area-buffer*))
             (null hi::*popup*)
             (not (listen-editor-input hi::*editor-input*)))
    (let* ((point (current-point))
           (char (previous-character point)))
      (when (and char (word-char-p char))
        (let ((start (token-start point #'word-char-p)))
          (unwind-protect
               (when (>= (- (mark-charpos point) (mark-charpos start))
                         (value complete-as-you-type-length))
                 (ignore-errors (complete-at-point nil t)))
            (delete-mark start)))))))

(add-hook self-insert-hook 'maybe-complete-as-typed)

(defcommand "Complete as You Type" (p)
  "Turn the offering of completions as one types on or off: with a positive
   argument on, with zero or a negative one off, and without one the other
   way."
  "Toggle the offering of completions as one types."
  (setf (value complete-as-you-type)
        (if p (plusp p) (not (value complete-as-you-type))))
  (message "Completions are ~:[asked for with Tab~;offered as you type~]."
           (value complete-as-you-type)))


;;;; The file prompt.

(defun choose-file-completion (typein defaults)
  "At a file prompt whose input TYPEIN is the start of several files' names:
   show them in a popup at the foot of the window above, and put the one
   chosen in.  True when one was."
  (let* ((files (heml-ext:ambiguous-files typein defaults))
         (names (sort (remove-duplicates
                       (loop for file in files
                             for name = (namestring file)
                             for directory-p = (and (plusp (length name))
                                                    (or (char= (char name (1- (length name))) #\/)
                                                        (ignore-errors (uiop:directory-exists-p name))))
                             for trimmed = (string-right-trim "/" name)
                             for slash = (position #\/ trimmed :from-end t)
                             collect (concatenate 'string
                                                  (if slash (subseq trimmed (1+ slash)) trimmed)
                                                  (if directory-p "/" "")))
                       :test #'string=)
                      #'string<)))
    (when (rest names)
      (let* ((point (current-point))
             (start (copy-mark point :right-inserting)))
        ;; The name being typed starts after the last / before point.
        (loop for char = (previous-character start)
              while (and char (char/= char #\/)
                         (mark> start (region-start hi::*parse-input-region*)))
              do (mark-before start))
        (unwind-protect
             (let ((choice (popup-choose names start :window (bottom-window))))
               (when choice
                 (delete-region (region start point))
                 (insert-string point choice)
                 t))
          (delete-mark start))))))
