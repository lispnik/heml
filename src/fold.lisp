;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Folding: lines put out of sight under the line they belong to -- a
;;; function's body under its first line -- until they are wanted.
;;;
;;; A folded line is hidden: its plist says so, and a window's image goes
;;; from the line before a fold to the line after it (winimage.lisp).
;;; Nothing else knows of folds: the lines are in the buffer as they were,
;;; and are written, searched and counted as ever.  The line before a fold,
;;; its header, says after its end how many lines are under it.
;;;
;;; What can be folded is the file's own sections -- form feeds, ;;;; titles
;;; and dashed comment headers -- and what the mode's "Fold Ranges Function"
;;; says: a language server's folding ranges where there is one
;;; (lsp-features.lisp), and otherwise each line with the more indented lines
;;; after it.  A window of a buffer with sections or folds has a column in
;;; its fringe for their markers, which a click toggles.

(in-package :heml)

(defhvar "Fold Ranges Function"
  "A function of a buffer that returns what can be folded in it, as
   ((FIRST . LAST) ...), lines numbered from 0: FIRST stays shown and the
   lines after it as far as LAST are hidden.  NIL folds by indentation."
  :value nil)

(defparameter *fold-font* '(:fg 8 :italic t)
  "The font of what a fold's header says is under it.")


;;;; What can be folded.

(defun indentation-fold-ranges (buffer)
  "Each line of BUFFER with more indented lines after it, and the last of
   them: ((FIRST . LAST) ...), lines numbered from 0."
  (let ((ranges '())
        (open '())                      ; ((INDENTATION . LINE) ...), innermost first
        (last-text nil)
        (number 0))
    (flet ((close-to (indentation)
             (loop while (and open (>= (car (first open)) indentation))
                   do (let ((start (cdr (pop open))))
                        (when (and last-text (> last-text start))
                          (push (cons start last-text) ranges))))))
      (do ((line (mark-line (buffer-start-mark buffer)) (line-next line)))
          ((null line))
        (let* ((string (line-string line))
               (indentation (position-if-not (lambda (char) (member char '(#\Space #\Tab)))
                                             string)))
          (when indentation
            (close-to indentation)
            (push (cons indentation number) open)
            (setf last-text number)))
        (incf number))
      (close-to 0))
    (sort ranges #'< :key #'car)))

;;;; Sections.

;;; A file's own sections: a form feed, a Lisp file's ;;;; title (the first
;;; of a run of such lines), and a dashed comment, such as
;;;   ;;; --- Intel HEX -----------------------------------------
;;; in any mode with comment syntax ("Comment Start").  A section runs from
;;; its header to before the next header as high or higher, less the blank
;;; lines at its end: a form feed's is the highest, a dashed comment's the
;;; lowest.

(defun dashed-header-scanner (comment-start comment-end)
  "A scanner for a dashed section header in comments that start so, or
   with // where they start with /*."
  (let* ((start (string-trim '(#\Space #\Tab) comment-start))
         (end (and comment-end (string-trim '(#\Space #\Tab) comment-end)))
         (opener (if (string= start "/*")
                     "/\\*|//"
                     (ppcre:quote-meta-chars start))))
    (ppcre:create-scanner
     (format nil "^\\s*(?:~A)+\\s*-{2,}\\s*[^-\\s].*?-{2,}\\s*~@[(?:~A)?\\s*~]$"
             opener
             (and end (plusp (length end)) (ppcre:quote-meta-chars end))))))

(defparameter *title-scanner* (ppcre:create-scanner "^;;;;\\s+\\S")
  "A Lisp file's ;;;; section title.")

(defun section-header-level (string previous scanner lisp-p)
  "0, 1 or 2 when STRING, after the line PREVIOUS, heads a section, or NIL."
  (cond ((and (plusp (length string)) (char= (char string 0) #\Page)) 0)
        ((and scanner (ppcre:scan scanner string)) 2)
        ((and lisp-p (title-line-p string) (not (and previous (title-line-p previous))))
         1)))

(defun title-line-p (string)
  "Whether STRING is a ;;;; title's line: not the file's -*- line."
  (and (ppcre:scan *title-scanner* string) (not (search "-*-" string))))

(defun buffer-variable (name buffer)
  "The value of the Heml variable NAME as BUFFER sees it -- its own, its
   major mode's, or the global one -- whether or not it is current: the
   fringe is drawn for every window's buffer."
  (let ((mode (buffer-major-mode buffer)))
    (cond ((heml-bound-p name :buffer buffer) (variable-value name :buffer buffer))
          ((heml-bound-p name :mode mode) (variable-value name :mode mode))
          ((heml-bound-p name :global) (variable-value name :global)))))

;;;; Regions marked in comments.

;;; As IntelliJ and Visual Studio mark them: Visual Studio's
;;;   // region Name ... // endregion   (#region, # region, #pragma region)
;;; and NetBeans's
;;;   // <editor-fold desc="Name" defaultstate="collapsed"> ... // </editor-fold>
;;; in the mode's comments.  A region folds from its first line through its
;;; last, and one whose defaultstate is collapsed is folded as its file is
;;; read.

(defun comment-opener-regex (comment-start)
  "A regex for what starts a comment in a mode whose \"Comment Start\" is
   COMMENT-START, or NIL."
  (let ((start (and comment-start (string-trim '(#\Space #\Tab) comment-start))))
    (when (plusp (length start))
      (if (string= start "/*")
          "/\\*+|//+"
          (format nil "(?:~A)+" (ppcre:quote-meta-chars start))))))

(defun region-scanners (comment-start)
  "Scanners for a region's start, its end, an editor-fold's start and its
   end, in comments that start so; the first's register is the region's
   name, the third's the editor-fold's attributes."
  (let* ((opener (comment-opener-regex comment-start))
         (in-comment (if opener (format nil "(?:~A)\\s*" opener) "(?!)")))
    (list (ppcre:create-scanner
           (format nil "^\\s*(?:~A#?|#\\s*(?:pragma\\s+)?)region\\b\\s*(.*?)\\s*$" in-comment))
          (ppcre:create-scanner
           (format nil "^\\s*(?:~A#?|#\\s*(?:pragma\\s+)?)endregion\\b" in-comment))
          (ppcre:create-scanner
           (format nil "^\\s*~A<editor-fold\\b([^>]*)>" in-comment))
          (ppcre:create-scanner
           (format nil "^\\s*~A</editor-fold\\s*>" in-comment)))))

(defun buffer-regions (buffer comment-start)
  "BUFFER's marked regions, as ((FIRST . LAST) ...) and a list of those to
   be folded at first, lines numbered from 0."
  (destructuring-bind (start end fold-start fold-end) (region-scanners comment-start)
    (let ((open '())                    ; ((KIND FIRST COLLAPSED) ...)
          (ranges '())
          (collapsed '())
          (number 0))
      (do ((line (mark-line (buffer-start-mark buffer)) (line-next line)))
          ((null line))
        (let ((string (line-string line)))
          (flet ((close-region (kind)
                   (let ((entry (find kind open :key #'first)))
                     (when entry
                       (setf open (cdr (member entry open)))
                       (let ((range (cons (second entry) number)))
                         (when (> number (second entry))
                           (push range ranges)
                           (when (third entry) (push range collapsed))))))))
            (cond ((ppcre:scan end string) (close-region :region))
                  ((ppcre:scan fold-end string) (close-region :fold))
                  ((ppcre:scan start string)
                   (push (list :region number nil) open))
                  (t (ppcre:register-groups-bind (attributes) (fold-start string)
                       (push (list :fold number
                                   (and attributes
                                        (ppcre:scan "defaultstate\\s*=\\s*\"collapsed\"" attributes)))
                             open))))))
        (incf number))
      (values (sort ranges #'< :key #'car) collapsed))))

(defvar *buffer-sections* (make-hash-table :test 'eq :weakness :key)
  "Buffer to (SIGNATURE RANGES HEADERS MODE COLLAPSED): its sections and
   regions when last looked for, HEADERS a table of their first lines.")

(defun buffer-sections (buffer)
  "BUFFER's sections and marked regions, ((FIRST . LAST) ...) lines
   numbered from 0, a table of their first lines, and the regions to be
   folded at first."
  (let ((cached (gethash buffer *buffer-sections*)))
    (if (and cached (eql (first cached) (buffer-signature buffer))
             (equal (fourth cached) (buffer-major-mode buffer)))
        (values (second cached) (third cached) (fifth cached))
        (let* ((comment-start (buffer-variable 'comment-start buffer))
               (comment-end (buffer-variable 'comment-end buffer))
               (scanner (and comment-start (plusp (length (string-trim " " comment-start)))
                             (dashed-header-scanner comment-start comment-end)))
               (lisp-p (and comment-start (string= (string-trim " " comment-start) ";")))
               (headers (make-hash-table :test 'eq))
               (open '())               ; ((LEVEL FIRST) ...), innermost first
               (ranges '())
               (last-text -1)
               (number 0)
               (previous nil))
          (flet ((close-to (level)
                   (loop while (and open (>= (first (first open)) level))
                         do (let ((first (second (pop open))))
                              (when (> last-text first)
                                (push (cons first last-text) ranges))))))
            (do ((line (mark-line (buffer-start-mark buffer)) (line-next line)))
                ((null line))
              (let* ((string (line-string line))
                     (level (section-header-level string previous scanner lisp-p)))
                (when level
                  (close-to level)
                  (push (list level number) open)
                  (setf (gethash line headers) t))
                (unless (every (lambda (c) (member c '(#\Space #\Tab #\Page))) string)
                  (setf last-text number))
                (setf previous string)
                (incf number)))
            (close-to -1))
          ;; And the regions marked in comments, whose first lines are
          ;; headers too.
          (multiple-value-bind (regions collapsed) (buffer-regions buffer comment-start)
            (dolist (range regions)
              (let ((line (buffer-line buffer (car range))))
                (when line (setf (gethash line headers) t))))
            (setf ranges (stable-sort (append ranges regions)
                                      (lambda (a b)
                                        (or (< (car a) (car b))
                                            (and (= (car a) (car b)) (> (cdr a) (cdr b)))))))
            (setf (gethash buffer *buffer-sections*)
                  (list (buffer-signature buffer) ranges headers (buffer-major-mode buffer)
                        collapsed))
            (values ranges headers collapsed))))))

(defun section-header-line-p (line)
  "Whether LINE heads a section of its buffer."
  (let ((buffer (line-buffer line)))
    (and buffer
         (gethash line (nth-value 1 (buffer-sections buffer))))))


(defun fold-ranges (buffer)
  "What can be folded in BUFFER, the current buffer: its sections, and what
   the mode's function says or else its indentation; by their first lines,
   and of two that start together, the longer first."
  (let ((function (value fold-ranges-function)))
    (stable-sort (copy-list
                  (append (buffer-sections buffer)
                          (or (and function (ignore-errors (funcall function buffer)))
                              (indentation-fold-ranges buffer))))
                 (lambda (a b)
                   (or (< (car a) (car b))
                       (and (= (car a) (car b)) (> (cdr a) (cdr b))))))))


;;;; Hiding and showing.

(defun fold-header-p (line)
  "Whether LINE has a fold under it."
  (let ((next (line-next line)))
    (and next (hi:line-hidden-p next) (not (hi:line-hidden-p line)))))

(defun folded-lines (header)
  "How many lines are hidden under HEADER."
  (loop for line = (line-next header) then (line-next line)
        while (and line (hi:line-hidden-p line))
        count t))

(defun buffer-line (buffer number)
  "BUFFER's line NUMBER, from 0, or NIL."
  (with-mark ((mark (buffer-start-mark buffer)))
    (and (line-offset mark number 0) (mark-line mark))))

(defun closing-line-p (line)
  "Whether LINE holds nothing but what closes something: a brace, a bracket,
   a parenthesis, and a semicolon or comma after them."
  (let ((text (string-trim '(#\Space #\Tab) (line-string line))))
    (and (plusp (length text))
         (find (char text 0) "}])")
         (every (lambda (char) (find char "}]);, ")) text))))

(defun fold-last (buffer first last)
  "Where a fold of BUFFER's lines after FIRST as far as LAST ends: with the
   line after LAST too when it holds only what closes the fold, as a C
   function's closing brace does, so that the fold reads as one line."
  (declare (ignore first))
  (let ((after (buffer-line buffer (1+ last))))
    (if (and after (closing-line-p after)) (1+ last) last)))

(defun hide-lines (buffer first last)
  "Fold BUFFER's lines after FIRST as far as LAST, numbered from 0."
  (let ((line (buffer-line buffer first)))
    (when line
      (loop repeat (- last first)
            do (setf line (line-next line))
               (unless line (return))
               (setf (getf (line-plist line) 'hi::hidden) t)))))

(defun unfold-under (header)
  "Show the lines hidden under HEADER."
  (loop for line = (line-next header) then (line-next line)
        while (and line (hi:line-hidden-p line))
        do (remf (line-plist line) 'hi::hidden)))

(defun fold-header (line)
  "The shown line that LINE, hidden, is under."
  (loop for previous = (line-previous line) then (line-previous previous)
        while previous
        unless (hi:line-hidden-p previous) return previous))

(defun fold-annotation (line)
  "How many lines are folded under LINE, and what closes them, when that is
   folded too."
  (when (fold-header-p line)
    (let* ((count (folded-lines line))
           (last (let ((next line))
                   (dotimes (i count next)
                     (setf next (line-next next)))))
           (closing (and last (closing-line-p last) (> count 1))))
      (cons (format nil "... ~D line~:P~@[ ~A~]" (if closing (1- count) count)
                    (and closing (string-trim '(#\Space #\Tab) (line-string last))))
            *fold-font*))))

(pushnew 'fold-annotation hi:*line-annotation-functions*)


;;;; Point is never left where it cannot be seen.

(defvar *last-point-line* nil
  "The line point was on when the last command finished.")

(defun keep-point-shown ()
  "After a command: point on a hidden line goes to the far side of the
   fold if it came from the line next to it, as C-n and C-p do, and
   otherwise -- a search found something there -- the fold is opened."
  (let* ((point (current-point))
         (line (mark-line point)))
    (when (hi:line-hidden-p line)
      (let ((header (fold-header line))
            (after (hi:next-shown-line line)))
        (cond ((null header)
               (remf (line-plist line) 'hi::hidden))
              ((and (eq *last-point-line* header) after)
               (line-start point after))
              ((eq *last-point-line* after)
               (line-end point header))
              (t (unfold-under header)))))
    (setf *last-point-line* (mark-line point))))

(add-hook after-command-hook 'keep-point-shown)


;;;; Commands.

(defcommand "Toggle Fold" (p)
  "Fold what starts on this line, or else what this line is in, under its
   first line; on a line with a fold under it, open the fold; with the
   region active, fold its lines (\"Fold Selection\").  What can be
   folded is a section or a marked region, what the language server says,
   or else a line and the more indented lines after it."
  "Fold what is at point, or open the fold there."
  (declare (ignore p))
  (when (region-active-p)
    (return-from toggle-fold-command (fold-selection-command nil)))
  (let* ((buffer (current-buffer))
         (point (current-point))
         (line (mark-line point)))
    (cond ((fold-header-p line)
           (unfold-under line))
          (t
           (let* ((number (1- (count-lines (region (buffer-start-mark buffer) point))))
                  (ranges (fold-ranges buffer))
                  (range (or
                          ;; What starts here: the most of it.
                          (first (sort (remove-if-not (lambda (range) (eql (car range) number))
                                                      ranges)
                                       #'> :key #'cdr))
                          ;; What this is in: the least.
                          (first (sort (remove-if-not (lambda (range)
                                                        (< (car range) number (1+ (cdr range))))
                                                      ranges)
                                       #'> :key #'car)))))
             (unless range (editor-error "Nothing to fold here."))
             (hide-lines buffer (car range) (fold-last buffer (car range) (cdr range)))
             (when (hi:line-hidden-p (mark-line point))
               (line-end point (buffer-line buffer (car range)))))))
    (update-fold-column buffer)
    (setf *last-point-line* (mark-line point))))

(defcommand "Fold All" (p)
  "Fold everything in this buffer that is not within something else that
   can be folded: each function's body, under its first line."
  "Fold this buffer's outermost foldable things."
  (declare (ignore p))
  (let* ((buffer (current-buffer))
         (point (current-point))
         (end -1)
         (count 0))
    (dolist (range (fold-ranges buffer))
      (when (> (car range) end)
        (let ((last (fold-last buffer (car range) (cdr range))))
          (hide-lines buffer (car range) last)
          (setf end last))
        (incf count)))
    (when (hi:line-hidden-p (mark-line point))
      (line-end point (fold-header (mark-line point))))
    (update-fold-column buffer)
    (setf *last-point-line* (mark-line point))
    (message "~D fold~:P." count)))

(defcommand "Unfold All" (p)
  "Open every fold in this buffer."
  "Open every fold in this buffer."
  (declare (ignore p))
  (do ((line (mark-line (buffer-start-mark (current-buffer))) (line-next line)))
      ((null line))
    (remf (line-plist line) 'hi::hidden))
  (update-fold-column (current-buffer)))

(defun line-number-in-buffer (mark)
  "MARK's line's number, from 0."
  (1- (count-lines (region (buffer-start-mark (line-buffer (mark-line mark))) mark))))

(defun section-at (buffer number)
  "The innermost of BUFFER's sections that holds its line NUMBER."
  (let ((best nil))
    (dolist (range (buffer-sections buffer) best)
      (when (and (<= (car range) number (cdr range))
                 (or (null best) (> (car range) (car best))))
        (setf best range)))))

(defun toggle-section (buffer header)
  "Fold the section HEADER heads, or open it when it is folded; whether
   HEADER heads one."
  (cond ((fold-header-p header)
         (unfold-under header)
         t)
        (t
         (let* ((number (line-number-in-buffer (mark header 0)))
                (range (find number (buffer-sections buffer) :key #'car)))
           (when range
             (hide-lines buffer (car range) (cdr range))
             t)))))

(defcommand "Fold Section" (p)
  "Fold the section point is in -- a form feed's page, a ;;;; title's, or a
   dashed comment header's, such as ;;; --- Intel HEX --- -- under its
   header; on a folded section's header, open it."
  "Fold the section point is in, or open the one at point."
  (declare (ignore p))
  (let* ((buffer (current-buffer))
         (point (current-point))
         (line (mark-line point)))
    (if (fold-header-p line)
        (unfold-under line)
        (let ((range (section-at buffer (line-number-in-buffer point))))
          (unless range (editor-error "Not in a section."))
          (hide-lines buffer (car range) (cdr range))
          (line-end point (buffer-line buffer (car range)))))
    (update-fold-column buffer)
    (setf *last-point-line* (mark-line point))))


;;;; The fold column, in the fringe.

(defparameter *fold-marker-open* "▼"
  "Drawn in the fringe beside an open section's header.")

(defparameter *fold-marker-closed* "►"
  "Drawn in the fringe beside a line with a fold under it.")

(defparameter *fold-marker-font* '(:fg 8 :shape :fold-open)
  "The font of an open section's marker.")

(defparameter *fold-marker-closed-font* '(:fg 6 :bold t :shape :fold-closed)
  "The font of a fold's marker.")

(defun buffer-has-folds-p (buffer)
  (do ((line (mark-line (buffer-start-mark buffer)) (line-next line)))
      ((null line) nil)
    (when (hi:line-hidden-p line) (return t))))

(defun update-fold-column (buffer)
  "Give BUFFER's windows a fringe column for fold markers when it has
   sections or folds, and take it away when it has neither."
  (setf (hi:buffer-fringe-columns buffer :fold)
        (if (or (buffer-sections buffer) (buffer-has-folds-p buffer)) 1 0)))

(defun fold-line-fringe (line)
  (let ((buffer (line-buffer line)))
    (when (and buffer (plusp (hi:buffer-fringe-columns buffer :fold)))
      (let ((column (hi:buffer-fringe-column buffer :fold)))
        (cond ((fold-header-p line)
               (list (list column *fold-marker-closed* *fold-marker-closed-font*)))
              ((section-header-line-p line)
               (list (list column *fold-marker-open* *fold-marker-font*))))))))

(pushnew 'fold-line-fringe hi:*line-fringe-functions*)

(defvar *fold-signatures* (make-hash-table :test 'eq :weakness :key)
  "Each buffer's signature when its fold column was last decided.")

(defun fold-idle (&optional elapsed)
  "Once a second: the fold column of each buffer shown that has changed."
  (declare (ignore elapsed))
  (dolist (window *window-list*)
    (let ((buffer (window-buffer window)))
      (when (and buffer
                 (not (eql (gethash buffer *fold-signatures*) (buffer-signature buffer))))
        (setf (gethash buffer *fold-signatures*) (buffer-signature buffer))
        (ignore-errors (update-fold-column buffer))))))

(defun start-fold-idle ()
  (remove-scheduled-event 'fold-idle)
  (schedule-event 1 'fold-idle))

(add-hook entry-hook 'start-fold-idle)

;;; A click in the fold column beside a header folds or opens it.
;;;
(defun fold-fringe-click (line column window)
  (declare (ignore window))
  (let ((buffer (line-buffer line)))
    (when (and buffer
               (plusp (hi:buffer-fringe-columns buffer :fold))
               (= column (hi:buffer-fringe-column buffer :fold))
               (toggle-section buffer line))
      (let ((point (current-point)))
        (when (hi:line-hidden-p (mark-line point))
          (line-end point line)))
      (update-fold-column buffer)
      (setf *last-point-line* (mark-line (current-point)))
      t)))

(pushnew 'fold-fringe-click *fringe-click-functions*)


;;;; Folding the selection.

(defun fold-lines (buffer first last)
  "Fold BUFFER's lines after FIRST as far as LAST, from 0, point taken to
   FIRST's end when it is folded away."
  (hide-lines buffer first last)
  (let ((point (current-point)))
    (when (hi:line-hidden-p (mark-line point))
      (line-end point (buffer-line buffer first))))
  (update-fold-column buffer)
  (setf *last-point-line* (mark-line (current-point))))

(defcommand "Fold Selection" (p)
  "Fold the lines the region covers under its first line, as IntelliJ's
   Fold Selection does."
  "Fold the region's lines under its first."
  (declare (ignore p))
  (let* ((region (current-region))
         (start (region-start region))
         (end (region-end region))
         (first (line-number-in-buffer start))
         (last (- (line-number-in-buffer end)
                  ;; A region that ends at a line's start ends before it.
                  (if (and (zerop (mark-charpos end)) (mark< start end)) 1 0))))
    (unless (> last first)
      (editor-error "Select more than one line to fold."))
    (fold-lines (current-buffer) first last)
    (deactivate-region)))


;;;; Regions that start folded.

(defun fold-collapsed-regions (buffer &optional existed)
  "Fold BUFFER's regions marked defaultstate=\"collapsed\": on Read File
   Hook, as its file is read."
  (declare (ignore existed))
  (let ((collapsed (nth-value 2 (buffer-sections buffer))))
    (dolist (range collapsed)
      (hide-lines buffer (car range) (cdr range)))
    (when collapsed
      (update-fold-column buffer))))

(add-hook read-file-hook 'fold-collapsed-regions)


;;;; Folds in a project's session.

(defun buffer-folds (buffer)
  "BUFFER's folds, ((FIRST . LAST) ...): each a shown line and the last of
   the hidden lines after it, from 0."
  (let ((folds '()) (number 0) (first nil))
    (do ((line (mark-line (buffer-start-mark buffer)) (line-next line)))
        ((null line))
      (cond ((hi:line-hidden-p line)
             (unless first (setf first (1- number))))
            (first
             (push (cons first (1- number)) folds)
             (setf first nil)))
      (incf number))
    (when first (push (cons first (1- number)) folds))
    (nreverse folds)))

(defun restore-buffer-folds (buffer folds)
  "Fold BUFFER as FOLDS, from BUFFER-FOLDS, says, as far as it still can."
  (let ((count (count-lines (buffer-region buffer))))
    (loop for (first . last) in folds
          when (and (<= 0 first) (< first last count))
            do (hide-lines buffer first last))
    (update-fold-column buffer)))
