;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Completion at point.  The completions of what is before point are shown
;;; in a popup under it, a few rows laid over the window (winimage.lisp), in
;;; every device alike.  C-n and C-p or the arrows choose, Return or Tab
;;; puts the choice in, typing narrows the list, and any other key puts the
;;; popup away and does what it does.
;;;
;;; What completes a word is its mode's: Lisp's symbols come from the slave,
;;; or the editor's own image, and from the buffers; elsewhere the words of
;;; the buffers do.

(in-package :heml)

(defparameter *popup-font* '(:fg 0 :bg 7))
(defparameter *popup-selected-font* '(:fg 7 :bg 4 :bold t))
(defparameter *popup-rows* 10
  "The most completions the popup shows at once.")
(defparameter *popup-width* 60)


;;;; The popup.

(defun popup-rows (items selected first width)
  (loop for item in (nthcdr first items)
        for index from first
        repeat *popup-rows*
        collect (cons (let ((text (make-string width :initial-element #\Space)))
                        (replace text item :start1 1 :end1 (1- width))
                        text)
                      (if (= index selected) *popup-selected-font* *popup-font*))))

(defun show-popup (items selected mark)
  "Show ITEMS in a popup under MARK in the current window, the SELECTED one
   marked, as many as fit and the popup's rows allow."
  (let ((window (current-window)))
    (multiple-value-bind (x y) (hi::mark-to-cursorpos mark window)
      (when x
        (let* ((height (window-height window))
               (count (min (length items) *popup-rows*))
               (below (- height y 1))
               ;; Under the line, or over it where there is more room.
               (under (or (>= below count) (>= below y)))
               (rows (min count (if under below y)))
               (first (max 0 (min (- selected (floor rows 2)) (- (length items) rows))))
               (width (min *popup-width* (window-width window)
                           (+ 2 (reduce #'max items :key #'length)))))
          (when (plusp rows)
            (let ((*popup-rows* rows))
              (setf hi::*popup*
                    (hi::make-popup window (max 0 (min (1- x) (- (window-width window) width)))
                                    (if under (1+ y) (- y rows))
                                    (popup-rows items selected first width))))))))))

(defun popup-choose (candidates start)
  "Let the user choose among CANDIDATES, completions of the text from the
   mark START to point, which typing narrows.  Returns the one chosen, or
   NIL when the popup is put away without one."
  (let ((point (current-point))
        (selected 0))
    (unwind-protect
         (loop
           (let* ((typed (region-to-string (region start point)))
                  (items (remove-if-not (lambda (c)
                                          (and (>= (length c) (length typed))
                                               (string-equal typed c :end2 (length typed))))
                                        candidates)))
             (when (or (null items) (mark< point start))
               (return nil))
             (setf selected (max 0 (min selected (1- (length items)))))
             ;; Once without it, so that the place it goes is on the screen.
             (setf hi::*popup* nil)
             (redisplay)
             (show-popup items selected start)
             (redisplay)
             (let* ((key (get-key-event hi::*editor-input*))
                    (char (heml-ext:key-event-char key)))
               (cond ((member key (list #k"control-n" #k"downarrow"))
                      (setf selected (mod (1+ selected) (length items))))
                     ((member key (list #k"control-p" #k"uparrow"))
                      (setf selected (mod (1- selected) (length items))))
                     ((member key (list #k"return" #k"tab" #k"control-i"))
                      (return (nth selected items)))
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

(defun lisp-symbol-completions (point)
  "Completions of the symbol before POINT -- the slave's or the editor's
   symbols, and the buffers' -- and where its name starts, after any
   package prefix."
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
                         (format nil "(heml::%find-symbol-completion-matches ~S ~S)"
                                 package-name name)))))
                   (ignore-errors (%find-symbol-completion-matches package-name name)))))
           (tokens (unless colon
                     (mapcar #'string-downcase (buffer-tokens name #'lisp-symbol-char-p)))))
      (values (sort (remove-duplicates (append (remove name symbols :test #'string=) tokens)
                                       :test #'string=)
                    #'string-lessp)
              start))))

(defhvar "Completions Function"
  "A function of a mark, point, that returns the completions of what is
   before it and a mark where what they complete starts."
  :value 'word-completions)

(defhvar "Completions Function"
  "A function of a mark, point, that returns the completions of what is
   before it and a mark where what they complete starts."
  :mode "Lisp" :value 'lisp-symbol-completions)

(defun editor-symbol-completions (point)
  "As LISP-SYMBOL-COMPLETIONS, of the editor's own symbols whatever slave
   there is."
  (hlet ((current-eval-server nil))
    (lisp-symbol-completions point)))

(defhvar "Completions Function"
  "A function of a mark, point, that returns the completions of what is
   before it and a mark where what they complete starts."
  :mode "Editor" :value 'editor-symbol-completions)

(defun complete-at-point (&optional p)
  (declare (ignore p))
  (let ((point (current-point)))
    (multiple-value-bind (candidates start) (funcall (value completions-function) point)
      (unwind-protect
           (let ((typed (region-to-string (region start point))))
             (cond ((null candidates)
                    (message "No completions of ~S." typed))
                   (t
                    (let ((choice (if (rest candidates)
                                      (popup-choose candidates start)
                                      (first candidates))))
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
