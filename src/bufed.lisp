;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;; This file contains Bufed (Buffer Editing) code.
;;;

(in-package :heml)

;;; The Bufed buffer lists the other buffers, one a line, under a header:
;;;
;;;   Buffers  (5, by recency)
;;;    .  * notes.md        1.2K  Markdown  ~/notes/notes.md
;;;         Makefile         10K  Fundamental  ~/Projects/heml/Makefile
;;;
;;; A line's first column is its mark (* marked, D flagged for deletion),
;;; then . for the current buffer, % for a read-only one and * for a
;;; modified one.  With grouping on, a line naming a directory, in
;;; brackets, starts each group of the buffers visiting files there.
;;; *BUFED-LINES* holds what each line after the header shows: an entry for
;;; a buffer, or NIL for a group's line.  The list is made again from the
;;; buffers whenever it is shown, and when they change while it is; marks
;;; and flags belong to the buffers, and are kept.



;;;; State.

(defstruct (bufed-entry (:constructor make-bufed-entry (buffer)))
  buffer
  (marked nil)
  (deleted nil))

(defvar *bufed-buffer* nil
  "The Bufed buffer, when there is one.")

(defvar *bufed-lines* #()
  "What each line after the header shows: a BUFED-ENTRY, or NIL for a
group's heading.")

(defvar *bufed-marks* (make-hash-table :test 'eq :weakness :key)
  "Buffer to its entry, so that marks and flags outlive a listing.")

(defvar *bufed-sort* :recency
  "How the buffers are ordered: :RECENCY, :NAME, :SIZE or :MODE.")

(defvar *bufed-filter* nil
  "When not NIL, only the buffers it names are listed: a mode's name, * for
the modified ones, a directory (starting with /) their files are under, or
text their names contain.")

(defvar *bufed-grouped* nil
  "When true, the buffers visiting files are grouped by directory, and the
others follow.")

(defvar *bufed-stale* nil
  "True when a buffer has changed since the listing was made.")

(defconstant +bufed-header-lines+ 1)

(defmode "Bufed" :major-p t
  :documentation
  "Bufed lists the buffers, to visit, save, kill and otherwise work on them,
   one at a time or marked.")

(defhvar "Virtual Buffer Deletion"
  "When set, \"Bufed Delete\" flags a buffer for deletion, to be killed by
   \"Bufed Expunge\", instead of killing it at once."
  :value t)

(defhvar "Bufed Delete Confirm"
  "When set, Bufed asks before it kills buffers."
  :value t)



;;;; Making the listing.

(defun bufed-listed-buffers ()
  "The buffers to list, in the order *BUFED-SORT* says, as the filter lets."
  (let* ((all (remove-if (lambda (buffer)
                           (or (eq buffer *echo-area-buffer*) (eq buffer *bufed-buffer*)))
                         ;; Most recently current first: the history, then
                         ;; the rest in the order they were made.
                         (remove-duplicates (append *buffer-history* *buffer-list*)
                                            :from-end t)))
         (shown (if *bufed-filter* (remove-if-not #'bufed-filter-shows-p all) all)))
    (ecase *bufed-sort*
      (:recency shown)
      (:name (sort (copy-list shown) #'string-lessp :key #'buffer-name))
      (:size (stable-sort (copy-list shown) #'> :key #'bufed-buffer-size))
      (:mode (stable-sort (copy-list shown) #'string-lessp :key #'buffer-major-mode)))))

(defun bufed-filter-shows-p (buffer)
  (let ((filter *bufed-filter*))
    (cond ((string= filter "*") (buffer-modified buffer))
          ;; A directory: the buffers visiting files under it.
          ((and (plusp (length filter)) (char= (char filter 0) #\/))
           (let ((pathname (buffer-pathname buffer)))
             (and pathname (eql 0 (search filter (namestring pathname))))))
          ((getstring filter *mode-names*)
           (if (mode-major-p filter)
               (string-equal filter (buffer-major-mode buffer))
               (buffer-minor-mode buffer filter)))
          (t (search filter (buffer-name buffer) :test #'char-equal)))))

(defun bufed-buffer-size (buffer)
  (count-characters (buffer-region buffer)))

(defun bufed-abbreviate (namestring)
  "NAMESTRING with the home directory shown as ~."
  (let ((home (namestring (user-homedir-pathname))))
    (if (uiop:string-prefix-p home namestring)
        (concatenate 'string "~/" (subseq namestring (length home)))
        namestring)))

(defun bufed-entry-for (buffer)
  (or (gethash buffer *bufed-marks*)
      (setf (gethash buffer *bufed-marks*) (make-bufed-entry buffer))))

(defun bufed-line-fields (entry)
  "The columns of ENTRY's line: flags, name, size, mode and file."
  (let ((buffer (bufed-entry-buffer entry)))
    (list (format nil "~C~C~C~C"
                  (cond ((bufed-entry-deleted entry) #\D)
                        ((bufed-entry-marked entry) #\*)
                        (t #\Space))
                  (if (eq buffer (bufed-previous-buffer)) #\. #\Space)
                  (if (buffer-writable buffer) #\Space #\%)
                  (if (buffer-modified buffer) #\* #\Space))
          (bufed-display-name buffer)
          (human-size (bufed-buffer-size buffer))
          (buffer-major-mode buffer)
          (let ((pathname (buffer-pathname buffer)))
            (if pathname (bufed-abbreviate (namestring pathname)) "")))))

(defun bufed-display-name (buffer)
  "BUFFER's name, but only its file's name when the buffer is named as a
file's buffer is by default (\"notes.md /Users/me/notes/\"): the file
column shows the directory."
  (let ((pathname (buffer-pathname buffer))
        (name (buffer-name buffer)))
    (if (and pathname (string= name (pathname-to-buffer-name pathname)))
        (file-namestring pathname)
        name)))

(defun bufed-previous-buffer ()
  "The buffer that was current before Bufed: the one . marks."
  (find-if (lambda (buffer) (not (eq buffer *bufed-buffer*)))
           (if (eq (current-buffer) *bufed-buffer*)
               *buffer-history*
               (cons (current-buffer) *buffer-history*))))

(defun bufed-groups (buffers)
  "BUFFERS as ((HEADING . BUFFERS) ...): those visiting files by directory,
then the others under no heading."
  (let ((groups '())
        (others '()))
    (dolist (buffer buffers)
      (let ((pathname (buffer-pathname buffer)))
        (if pathname
            (let* ((directory (bufed-abbreviate (directory-namestring pathname)))
                   (group (assoc directory groups :test #'string=)))
              (if group
                  (push buffer (cdr group))
                  (push (list directory buffer) groups)))
            (push buffer others))))
    (append (mapcar (lambda (group) (cons (car group) (reverse (cdr group))))
                    (sort groups #'string< :key #'car))
            (when others (list (cons "no file" (reverse others)))))))

(defun bufed-fill ()
  "Write the listing into the Bufed buffer, keeping point on its buffer."
  (let* ((buffer *bufed-buffer*)
         (point (buffer-point buffer))
         (old-entry (ignore-errors (bufed-entry-at point)))
         (old-index (bufed-line-index point))
         (buffers (bufed-listed-buffers))
         (layout (if *bufed-grouped*
                     (bufed-groups buffers)
                     (list (cons nil buffers))))
         (lines '()))
    ;; What each line shows, in order.
    (dolist (group layout)
      (when (car group) (push (car group) lines))
      (dolist (b (cdr group)) (push (bufed-entry-for b) lines)))
    (setf lines (nreverse lines))
    (setf *bufed-lines* (map 'simple-vector (lambda (line) (if (stringp line) nil line)) lines))
    (let* ((fields (mapcar #'bufed-line-fields (remove-if #'stringp lines)))
           (name-width (reduce #'max fields :key (lambda (f) (length (second f))) :initial-value 4))
           (size-width (reduce #'max fields :key (lambda (f) (length (third f))) :initial-value 4))
           (mode-width (reduce #'max fields :key (lambda (f) (length (fourth f))) :initial-value 4)))
      (with-writable-buffer (buffer)
        (delete-region (buffer-region buffer))
        (with-mark ((mark (buffer-start-mark buffer) :left-inserting))
          (insert-string
           mark
           (string-right-trim
            '(#\Newline)
            (with-output-to-string (s)
              (format s "Buffers  (~D, by ~(~A~)~@[, showing ~S~]~:[~;, grouped~])~%"
                      (length buffers) *bufed-sort* *bufed-filter* *bufed-grouped*)
              (dolist (line lines)
                (if (stringp line)
                    (format s "[~A]~%" line)
                    (destructuring-bind (flags name size mode file) (bufed-line-fields line)
                      (format s "~A ~vA  ~v@A  ~vA  ~A~%"
                              flags name-width name size-width size mode-width mode file))))))))
        (setf (buffer-modified buffer) nil)))
    (setf *bufed-stale* nil)
    ;; Point stays on its buffer, or where it was.
    (let ((index (or (and old-entry (position old-entry *bufed-lines*))
                     (min (max 0 old-index) (max 0 (1- (length *bufed-lines*)))))))
      (buffer-start point)
      (line-offset point (+ index +bufed-header-lines+) 0))))

(defun bufed-line-index (mark)
  (- (count-lines (region (buffer-start-mark (line-buffer (mark-line mark))) mark))
     1 +bufed-header-lines+))

(defun bufed-entry-at (mark)
  (let ((index (bufed-line-index mark)))
    (or (and (< -1 index (length *bufed-lines*)) (svref *bufed-lines* index))
        (editor-error "Not on a buffer's line."))))

(defun bufed-check ()
  (unless (and *bufed-buffer* (eq (current-buffer) *bufed-buffer*))
    (editor-error "Not in the Bufed buffer.")))

(defun bufed-entries ()
  (remove nil (coerce *bufed-lines* 'list)))

(defun bufed-targets ()
  "The marked buffers' entries, or the one under point when none is marked."
  (or (remove-if-not #'bufed-entry-marked (bufed-entries))
      (list (bufed-entry-at (current-point)))))

(defun bufed-down-line ()
  (let ((point (current-point)))
    (line-offset point 1 0)
    (when (blank-line-p (mark-line point))
      (line-offset point -1 0))))



;;;; Showing it.

(defcommand "Bufed" (p)
  "List the buffers, to visit, save, kill and otherwise work on them."
  "List the buffers."
  (declare (ignore p))
  (unless *bufed-buffer*
    (setf *bufed-buffer*
          (make-buffer "Bufed" :modes '("Bufed")
                               :delete-hook (list #'delete-bufed-buffers)))
    (setf (buffer-writable *bufed-buffer*) nil)
    (let ((fields (buffer-modeline-fields *bufed-buffer*)))
      (setf (cdr (last fields))
            (list (or (modeline-field :bufed-cmds)
                      (make-modeline-field
                       :name :bufed-cmds :width 18
                       :function #'(lambda (buffer window)
                                     (declare (ignore buffer window))
                                     "  Type ? for help.")))))
      (setf (buffer-modeline-fields *bufed-buffer*) fields))
    (start-bufed-watch))
  (bufed-fill)
  (change-to-buffer *bufed-buffer*)
  (let ((point (current-point)))
    (buffer-start point)
    (line-offset point +bufed-header-lines+ 0)))

(defun delete-bufed-buffers (buffer)
  (when (eq buffer *bufed-buffer*)
    (setf *bufed-buffer* nil
          *bufed-lines* #())))

(defcommand "Bufed Update" (p)
  "List the buffers again."
  "List the buffers again."
  (declare (ignore p))
  (bufed-check)
  (bufed-fill))

(defcommand "Bufed Help" (p)
  "Show this help."
  "Show this help."
  (declare (ignore p))
  (describe-mode-command nil "Bufed"))



;;;; Visiting.

(defcommand "Bufed Goto" (p)
  "Change to the buffer under point."
  "Change to the buffer under point."
  (declare (ignore p))
  (bufed-check)
  (change-to-buffer (bufed-entry-buffer (bufed-entry-at (current-point)))))

(defcommand "Bufed Goto Other Window" (p)
  "Show the buffer under point in the other window, splitting this one if it
   is the only one, and go there."
  "Show the buffer under point in the other window."
  (declare (ignore p))
  (bufed-check)
  (let ((buffer (bufed-entry-buffer (bufed-entry-at (current-point)))))
    (setf (current-window) (bufed-other-window))
    (change-to-buffer buffer)))

(defcommand "Bufed Display" (p)
  "Show the buffer under point in the other window, staying in Bufed."
  "Show the buffer under point in the other window."
  (declare (ignore p))
  (bufed-check)
  (let ((buffer (bufed-entry-buffer (bufed-entry-at (current-point))))
        (here (current-window)))
    (setf (current-window) (bufed-other-window))
    (change-to-buffer buffer)
    (select-window here)))

(defun bufed-other-window ()
  (if (> (length (remove *echo-area-window* *window-list*)) 1)
      (next-window (current-window))
      (or (make-window (window-display-start (current-window)))
          (editor-error "No room for another window."))))

(defcommand "Bufed Mouse Goto" (p)
  "Change to the buffer clicked."
  "Change to the buffer clicked."
  (mouse-set-point-command p)
  (bufed-goto-command nil))

(defcommand "Bufed Quit" (p)
  "Kill the buffers flagged for deletion, and leave Bufed."
  "Leave Bufed, killing the flagged buffers."
  (declare (ignore p))
  (bufed-check)
  (expunge-bufed-buffers)
  (when *bufed-buffer* (delete-buffer-if-possible *bufed-buffer*)))



;;;; Marks and flags.

(defun bufed-set (entry &key (marked nil markedp) (deleted nil deletedp))
  (when markedp (setf (bufed-entry-marked entry) marked))
  (when deletedp (setf (bufed-entry-deleted entry) deleted))
  (bufed-fill))

(defcommand "Bufed Mark" (p)
  "Mark the buffer under point, or the next P, and move down."
  "Mark the buffer under point and move down."
  (bufed-check)
  (dotimes (i (or p 1))
    (bufed-set (bufed-entry-at (current-point)) :marked t)
    (bufed-down-line)))

(defcommand "Bufed Unmark" (p)
  "Clear the mark or flag of the buffer under point, or the next P, and move
   down."
  "Unmark the buffer under point and move down."
  (bufed-check)
  (dotimes (i (or p 1))
    (bufed-set (bufed-entry-at (current-point)) :marked nil :deleted nil)
    (bufed-down-line)))

(defcommand "Bufed Unmark All" (p)
  "Clear every mark and flag."
  "Clear every mark and flag."
  (declare (ignore p))
  (bufed-check)
  (dolist (entry (bufed-entries))
    (setf (bufed-entry-marked entry) nil (bufed-entry-deleted entry) nil))
  (bufed-fill))

(defcommand "Bufed Toggle Marks" (p)
  "Mark the unmarked buffers and unmark the marked ones, leaving flagged
   ones alone."
  "Toggle the marks."
  (declare (ignore p))
  (bufed-check)
  (dolist (entry (bufed-entries))
    (unless (bufed-entry-deleted entry)
      (setf (bufed-entry-marked entry) (not (bufed-entry-marked entry)))))
  (bufed-fill))

(defcommand "Bufed Mark Matching" (p)
  "Mark the buffers whose names contain some text."
  "Mark the buffers whose names contain some text."
  (declare (ignore p))
  (bufed-check)
  (let ((text (prompt-for-string :prompt "Mark buffers whose names contain: "
                                 :help "Text the names contain, in any case."))
        (count 0))
    (dolist (entry (bufed-entries))
      (when (search text (buffer-name (bufed-entry-buffer entry)) :test #'char-equal)
        (setf (bufed-entry-marked entry) t)
        (incf count)))
    (bufed-fill)
    (message "~D buffer~:P marked." count)))

(defcommand "Bufed Delete" (p)
  "Flag the buffer under point for killing, and move down; \"Bufed Expunge\"
   kills the flagged ones.  Without \"Virtual Buffer Deletion\", kill it now."
  "Flag the buffer under point for killing."
  (declare (ignore p))
  (bufed-check)
  (let ((entry (bufed-entry-at (current-point))))
    (if (and (not (value virtual-buffer-deletion))
             (or (not (value bufed-delete-confirm))
                 (prompt-for-y-or-n :prompt "Kill buffer? " :default t
                                    :must-exist t :default-string "Y")))
        (progn (delete-bufed-buffer (bufed-entry-buffer entry)) (bufed-fill))
        (progn (bufed-set entry :deleted t) (bufed-down-line)))))

(defcommand "Bufed Undelete" (p)
  "Clear the flag on the buffer under point."
  "Clear the flag on the buffer under point."
  (declare (ignore p))
  (bufed-check)
  (bufed-set (bufed-entry-at (current-point)) :deleted nil))

(defcommand "Bufed Expunge" (p)
  "Kill the buffers flagged for deletion."
  "Kill the flagged buffers."
  (declare (ignore p))
  (bufed-check)
  (expunge-bufed-buffers)
  (when *bufed-buffer* (bufed-fill)))

(defun expunge-bufed-buffers ()
  "Kill the flagged buffers, after asking.  True if any was killed."
  (let ((buffers (mapcar #'bufed-entry-buffer
                         (remove-if-not #'bufed-entry-deleted (bufed-entries)))))
    (when (and buffers
               (or (not (value bufed-delete-confirm))
                   (prompt-for-y-or-n :prompt (format nil "Kill ~D buffer~:P? " (length buffers))
                                      :default t :must-exist t :default-string "Y")))
      (dolist (buffer buffers t) (delete-bufed-buffer buffer)))))

(defun delete-bufed-buffer (buffer)
  (when (and (buffer-modified buffer)
             (buffer-pathname buffer)
             (prompt-for-y-or-n :prompt (list "~A is modified.  Save it first? "
                                              (buffer-name buffer))))
    (save-file-command nil buffer))
  (remhash buffer *bufed-marks*)
  (delete-buffer-if-possible buffer))



;;;; Operations on the marked buffers, or the one under point.

(defcommand "Bufed Kill" (p)
  "Kill the marked buffers, or the one under point, after asking."
  "Kill the marked buffers, or the one under point."
  (declare (ignore p))
  (bufed-check)
  (let ((buffers (mapcar #'bufed-entry-buffer (bufed-targets))))
    (when (or (not (value bufed-delete-confirm))
              (prompt-for-y-or-n :prompt (format nil "Kill ~D buffer~:P? " (length buffers))
                                 :default t :must-exist t :default-string "Y"))
      (dolist (buffer buffers) (delete-bufed-buffer buffer))))
  (bufed-fill))

(defcommand "Bufed Save File" (p)
  "Save the marked buffers, or the one under point, that visit files."
  "Save the marked buffers, or the one under point."
  (declare (ignore p))
  (bufed-check)
  (let ((saved 0))
    (dolist (entry (bufed-targets))
      (let ((buffer (bufed-entry-buffer entry)))
        (when (and (buffer-pathname buffer) (buffer-modified buffer))
          (save-file-command nil buffer)
          (incf saved))))
    (bufed-fill)
    (message "~D buffer~:P saved." saved)))

(defcommand "Bufed Not Modified" (p)
  "Say the marked buffers, or the one under point, are not modified."
  "Clear the modified flag."
  (declare (ignore p))
  (bufed-check)
  (dolist (entry (bufed-targets))
    (setf (buffer-modified (bufed-entry-buffer entry)) nil))
  (bufed-fill))

(defcommand "Bufed Toggle Read Only" (p)
  "Make the marked buffers, or the one under point, read-only, or writable
   again."
  "Toggle read-only."
  (declare (ignore p))
  (bufed-check)
  (dolist (entry (bufed-targets))
    (let ((buffer (bufed-entry-buffer entry)))
      (setf (buffer-writable buffer) (not (buffer-writable buffer)))))
  (bufed-fill))

(defcommand "Bufed Revert" (p)
  "Read the marked buffers, or the one under point, from their files again."
  "Revert buffers from their files."
  (declare (ignore p))
  (bufed-check)
  (let ((buffers (remove-if-not #'buffer-pathname
                                (mapcar #'bufed-entry-buffer (bufed-targets)))))
    ;; "Revert File" works on the current buffer.
    (dolist (buffer buffers)
      (change-to-buffer buffer)
      (revert-file-command nil))
    (change-to-buffer *bufed-buffer*)
    (bufed-fill)
    (message "~D buffer~:P reverted." (length buffers))))



;;;; Sorting, filtering and grouping.

(defcommand "Bufed Sort" (p)
  "Order the buffers by recency, then name, then size, then mode, each time
   this is run."
  "Change the order buffers are listed in."
  (declare (ignore p))
  (bufed-check)
  (setf *bufed-sort* (ecase *bufed-sort*
                       (:recency :name) (:name :size) (:size :mode) (:mode :recency)))
  (bufed-fill)
  (message "Sorted by ~(~A~)." *bufed-sort*))

(defcommand "Bufed Filter" (p)
  "List only some buffers: those in a mode, the modified ones (*), or those
   whose names contain some text.  Nothing lists them all again."
  "List only some buffers."
  (declare (ignore p))
  (bufed-check)
  (let ((filter (prompt-for-string
                 :prompt "Show buffers (a mode, * for modified, text in their names): "
                 :help "Empty shows every buffer."
                 :default "" :trim t)))
    (setf *bufed-filter* (if (zerop (length filter)) nil filter))
    (bufed-fill)))

(defcommand "Bufed Toggle Groups" (p)
  "Group the buffers visiting files by directory, the others after them, or
   stop grouping."
  "Toggle grouping."
  (declare (ignore p))
  (bufed-check)
  (setf *bufed-grouped* (not *bufed-grouped*))
  (bufed-fill))



;;;; Keeping up.

;;; Buffers made, killed, renamed, modified or visited mark the listing
;;; stale, and while it is shown it is made again within a second.  The
;;; hooks only mark it: they run in the middle of what changes the buffers.

(defun bufed-note-change (&rest arguments)
  (unless (and arguments (eq (first arguments) *bufed-buffer*))
    (setf *bufed-stale* t)))

(add-hook make-buffer-hook 'bufed-note-change)
(add-hook delete-buffer-hook 'bufed-note-change)
(add-hook buffer-name-hook 'bufed-note-change)
(add-hook buffer-pathname-hook 'bufed-note-change)
(add-hook buffer-modified-hook 'bufed-note-change)

(defvar *bufed-watching* nil)

(defun bufed-watch (elapsed)
  (declare (ignore elapsed))
  (when (and *bufed-stale* *bufed-buffer* (buffer-windows *bufed-buffer*))
    (ignore-errors (bufed-fill))))

(defun start-bufed-watch ()
  (unless *bufed-watching*
    (schedule-event 1 #'bufed-watch)
    (setf *bufed-watching* t)))



;;;; Colours.

;;; The header is bold, a group's line blue; a flagged buffer is red all
;;; along, a marked one bold yellow; a modified buffer's name is bold, and
;;; a buffer that visits no file is cyan.
;;;
(defun bufed-line-fonts (string)
  (cond ((zerop (length string)) '())
        ((uiop:string-prefix-p "Buffers  (" string) (list (cons 0 '(:bold t))))
        ((char= (char string 0) #\[) (list (cons 0 '(:fg 4 :bold t))))
        ((< (length string) 6) '())
        ((char= (char string 0) #\D) (list (cons 0 1)))
        ((char= (char string 0) #\*) (list (cons 0 '(:fg 3 :bold t))))
        (t
         (let* ((name-end (or (position #\Space string :start 5) (length string)))
                (modified (char= (char string 3) #\*))
                (file-less (let ((last-space (position #\Space (string-right-trim " " string)
                                                       :from-end t)))
                             (not (and last-space
                                       (find (char string (min (1+ last-space) (1- (length string))))
                                             "~/"))))))
           (when (or modified file-less)
             (list (cons 5 (cond ((and modified file-less) '(:fg 6 :bold t))
                                 (modified '(:bold t))
                                 (t 6)))
                   (cons name-end 0)))))))

(defun bufed-highlight-line (line)
  (let ((old (getf (line-plist line) 'bufed-marks)))
    (unless (and old (eq (car old) (line-signature line)))
      (dolist (mark (cdr old))
        (delete-font-mark mark))
      (setf (getf (line-plist line) 'bufed-marks)
            (cons (line-signature line)
                  (loop for (position . font) in (bufed-line-fonts (line-string line))
                        collect (font-mark line position font)))))))

(define-mode-highlighter "Bufed" 'bufed-highlight-line :marks 'bufed-marks)
