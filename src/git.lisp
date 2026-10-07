;;; -*- Log: heml.log; Package: heml -*-
;;;
;;; Git: a status buffer that stages and unstages files and hunks and
;;; commits, as Emacs's Magit does; Diff mode, whose lines visit the places
;;; they show; a log and a blame, whose lines show their commits; and marks
;;; in the fringe beside a file's lines that differ from the last commit, as
;;; Emacs's diff-hl draws.  Everything asks the git program, run in the
;;; repository, and waits for it, but pushing and pulling, which may take a
;;; while, write to a buffer as they go.

(in-package :heml)


;;;; Running git.

(defun git (directory &rest arguments)
  "Run git with ARGUMENTS in DIRECTORY: what it printed, its exit code, and
   what it said on its error output.  Its colours, quoting of file names
   and diffs' own prefixes are turned off, whatever the user's settings."
  (multiple-value-bind (output errors code)
      (uiop:run-program (list* "git" "-c" "color.ui=never" "-c" "core.quotepath=false"
                               "-c" "diff.noprefix=false" "-c" "diff.mnemonicprefix=false"
                               arguments)
                        :directory (namestring directory)
                        :output :string :error-output :string
                        :external-format :utf-8
                        :ignore-error-status t)
    (values output code errors)))

(defun git-ok (directory &rest arguments)
  "What git with ARGUMENTS printed in DIRECTORY; an editor error saying
   what went wrong when it failed."
  (multiple-value-bind (output code errors) (apply #'git directory arguments)
    (unless (zerop code)
      (editor-error "git ~A: ~A" (first arguments)
                    (let ((text (string-trim '(#\Newline #\Space) errors)))
                      (subseq text 0 (or (position #\Newline text) (length text))))))
    output))

(defun git-lines (string)
  (let ((lines (uiop:split-string (string-right-trim '(#\Newline) string)
                                  :separator '(#\Newline))))
    (if (equal lines '("")) '() lines)))

(defun git-root (directory)
  "The top of the Git repository DIRECTORY is in, or NIL."
  (when (and directory (probe-file directory))
    (multiple-value-bind (output code)
        (ignore-errors (git directory "rev-parse" "--show-toplevel"))
      (when (eql code 0)
        (uiop:ensure-directory-pathname (string-trim '(#\Newline) output))))))

(defun current-git-root ()
  (or (git-root (default-directory))
      (editor-error "Not in a Git repository.")))

(defun git-has-head-p (root)
  (eql 0 (nth-value 1 (git root "rev-parse" "--verify" "-q" "HEAD"))))

(defun call-with-patch-file (patch function)
  "Call FUNCTION with the name of a file holding PATCH."
  (uiop:with-temporary-file (:pathname pathname :prefix "heml-git-")
    (with-open-file (out pathname :direction :output :if-exists :supersede
                                  :external-format :utf-8)
      (write-string patch out))
    (funcall function (uiop:native-namestring pathname))))

(defvar *git-root* nil
  "The repository of the Git buffer being made.")

(defun buffer-git-root (buffer)
  (and (heml-bound-p 'git-root :buffer buffer)
       (variable-value 'git-root :buffer buffer)))

(defun make-git-buffer (name mode root)
  "The buffer NAME, made if there is none, in MODE, emptied, knowing it is
   about ROOT."
  (let ((buffer (or (getstring name *buffer-names*)
                    (make-buffer name :modes (list mode)))))
    (setf (buffer-major-mode buffer) mode)
    (unless (heml-bound-p 'git-root :buffer buffer)
      (defhvar "Git Root" "The repository this buffer is about." :buffer buffer))
    (setf (variable-value 'git-root :buffer buffer) root)
    (with-writable-buffer (buffer)
      (delete-region (buffer-region buffer)))
    buffer))

(defun fill-git-buffer (buffer lines)
  "Put LINES in BUFFER, each a string or (STRING . PLIST), the plist kept in
   the line's own; BUFFER is left read-only and unmodified."
  (with-writable-buffer (buffer)
    (delete-region (buffer-region buffer))
    (let ((mark (copy-mark (buffer-start-mark buffer) :left-inserting))
          (first t))
      (dolist (entry lines)
        (unless first (insert-character mark #\Newline))
        (setf first nil)
        (insert-string mark (if (consp entry) (car entry) entry))
        (when (consp entry)
          (setf (line-plist (mark-line mark)) (copy-list (cdr entry)))))
      (delete-mark mark)))
  (setf (buffer-modified buffer) nil
        (buffer-writable buffer) nil)
  buffer)

(defun git-entry (line)
  (getf (line-plist line) 'git-entry))


;;;; Colours.  A line of a Git buffer keeps its colours in its plist, as
;;;; ((POSITION . FONT) ...), put there when it was made; a diff's are
;;;; worked out from its text.

(defparameter *git-section-font* '(:fg 4 :bold t))
(defparameter *git-label-font* '(:bold t))
(defparameter *git-hash-font* 3)
(defparameter *git-date-font* 2)
(defparameter *git-author-font* 6)
(defparameter *git-comment-font* 8)
(defparameter *diff-added-font* 2)
(defparameter *diff-removed-font* 1)
(defparameter *diff-hunk-font* 6)
(defparameter *diff-file-font* '(:bold t))

(defun diff-line-fonts (string)
  "The colours of a line of a diff."
  (cond ((zerop (length string)) '())
        ((or (uiop:string-prefix-p "diff " string) (uiop:string-prefix-p "+++ " string)
             (uiop:string-prefix-p "--- " string) (uiop:string-prefix-p "index " string)
             (uiop:string-prefix-p "new file" string) (uiop:string-prefix-p "deleted file" string)
             (uiop:string-prefix-p "rename " string) (uiop:string-prefix-p "similarity " string))
         (list (cons 0 *diff-file-font*)))
        ((uiop:string-prefix-p "commit " string)
         (list (cons 0 *git-hash-font*)))
        ((uiop:string-prefix-p "@@" string) (list (cons 0 *diff-hunk-font*)))
        ((char= (char string 0) #\+) (list (cons 0 *diff-added-font*)))
        ((char= (char string 0) #\-) (list (cons 0 *diff-removed-font*)))
        (t '())))

(defun git-line-fonts (line)
  (let ((plist (line-plist line)))
    (if (getf plist 'git-diff)
        (diff-line-fonts (line-string line))
        (getf plist 'git-fonts))))

(defun git-highlight-line (line)
  (let ((old (getf (line-plist line) 'git-marks)))
    (unless (and old (eq (car old) (line-signature line)))
      (dolist (mark (cdr old))
        (hi::delete-font-mark mark))
      (setf (getf (line-plist line) 'git-marks)
            (cons (line-signature line)
                  (loop for (position . font) in (git-line-fonts line)
                        when (<= position (line-length line))
                          collect (hi::font-mark line position font)))))))

(defun diff-highlight-line (line)
  (unless (getf (line-plist line) 'git-diff)
    (setf (getf (line-plist line) 'git-diff) t))
  (git-highlight-line line))


;;;; Diff mode: a diff, whose lines visit the places they show, in the file
;;;; as it is now.

(defmode "Diff" :major-p t
  :documentation "A diff: Return on a line visits the place in the file it
   shows, n and p go to the next and previous hunk, q buries it.")

(define-mode-highlighter "Diff" 'diff-highlight-line :marks 'git-marks)

(defparameter *hunk-header-scanner*
  (cl-ppcre:create-scanner "^@@ -\\d+(?:,\\d+)? \\+(\\d+)(?:,(\\d+))? @@"))

(defun diff-line-location (line)
  "The file and line a line of a diff shows: for a line taken out, the line
   after it."
  (let ((count 0) (start nil))
    ;; On the hunk's own @@ line, its first line.
    (multiple-value-bind (match groups)
        (cl-ppcre:scan-to-strings *hunk-header-scanner* (line-string line))
      (when match
        (setf start (parse-integer (aref groups 0)))))
    (do ((l (if start nil (line-previous line)) (line-previous l)))
        ((null l))
      (let ((string (line-string l)))
        (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings *hunk-header-scanner* string)
          (when match
            (setf start (parse-integer (aref groups 0)))
            (return))
          (when (or (uiop:string-prefix-p "diff " string) (uiop:string-prefix-p "commit " string))
            (return-from diff-line-location nil))
          (unless (or (uiop:string-prefix-p "-" string) (uiop:string-prefix-p "\\" string))
            (incf count)))))
    (when start
      (let ((file nil))
        (do ((l line (line-previous l)))
            ((null l))
          (let ((string (line-string l)))
            (cond ((and (uiop:string-prefix-p "+++ b/" string) (not file))
                   (setf file (subseq string 6)))
                  ((and (uiop:string-prefix-p "--- a/" string) (not file))
                   (setf file (subseq string 6)))
                  ((uiop:string-prefix-p "diff " string) (return)))))
        (let ((root (buffer-git-root (line-buffer line))))
          (when (and file root)
            (list (merge-pathnames file root) (max 1 (+ start count)))))))))

(defun show-diff (root title arguments)
  "Run git with ARGUMENTS, a command printing a diff, in ROOT, and show what
   it prints in the buffer *git diff*, in the other window."
  (let* ((output (apply #'git-ok root arguments))
         (buffer (make-git-buffer "*git diff*" "Diff" root)))
    (unless (heml-bound-p 'result-location-function :buffer buffer)
      (defhvar "Result Location Function"
        "A function of a line that returns the place it names, or NIL."
        :buffer buffer))
    (setf (variable-value 'result-location-function :buffer buffer) 'diff-line-location)
    (fill-git-buffer buffer (or (git-lines output) (list (format nil "No differences: ~A." title))))
    (let ((here (current-window)))
      (select-window (other-window))
      (change-to-buffer buffer)
      (buffer-start (current-point))
      (select-window here))
    buffer))

(defun next-hunk (count)
  (let ((line (mark-line (current-point))))
    (dotimes (i (abs count))
      (loop (setf line (if (minusp count) (line-previous line) (line-next line)))
            (unless line (editor-error "No more hunks."))
            (when (uiop:string-prefix-p "@@" (line-string line)) (return))))
    (move-to-position (current-point) 0 line)))

(defcommand "Diff Next Hunk" (p)
  "Go to the next hunk of the diff."
  "Go to the next hunk."
  (next-hunk (or p 1)))

(defcommand "Diff Previous Hunk" (p)
  "Go to the previous hunk of the diff."
  "Go to the previous hunk."
  (next-hunk (- (or p 1))))


;;;; Commits: showing one, the log, and the blame.

(defun show-commit (root hash)
  (when (every (lambda (c) (char= c #\0)) hash)
    (editor-error "Not committed yet."))
  (show-diff root hash (list "show" "--stat" "-p" "--format=commit %H%nAuthor: %an <%ae>%nDate:   %ad%n%n%w(0,4,4)%B"
                             "--src-prefix=a/" "--dst-prefix=b/" hash)))

(defun line-commit (line)
  "The commit a line of a log or a blame is about."
  (or (getf (git-entry line) :commit)
      (editor-error "No commit on this line.")))

(defcommand "Git Show Commit" (p)
  "Show the commit this line is about -- its message and its diff -- in the
   other window."
  "Show this line's commit."
  (declare (ignore p))
  (let ((buffer (current-buffer)))
    (show-commit (or (buffer-git-root buffer) (current-git-root))
                 (line-commit (mark-line (current-point))))))

(defmode "Git Log" :major-p t
  :documentation "Commits, the latest first: Return shows one, q buries
   the list.")

(define-mode-highlighter "Git Log" 'git-highlight-line :marks 'git-marks)

(defparameter *git-log-limit* 500
  "How many commits a log shows.")

(defun show-log (root title &rest paths)
  (let* ((output (apply #'git-ok root "log" (format nil "-~D" *git-log-limit*) "--date=short"
                        "--format=%h%x09%ad%x09%an%x09%s"
                        (when paths (cons "--" paths))))
         (rows (mapcar (lambda (line) (uiop:split-string line :separator '(#\Tab)))
                       (git-lines output)))
         (author-width (min 20 (reduce #'max rows :key (lambda (row) (length (third row)))
                                                  :initial-value 0)))
         (buffer (make-git-buffer (format nil "*git log: ~A*" title) "Git Log" root)))
    (fill-git-buffer
     buffer
     (or (loop for (hash date author subject) in rows
               for author-text = (subseq author 0 (min (length author) author-width))
               for date-start = (1+ (length hash))
               for author-start = (+ date-start (length date) 1)
               collect (list (format nil "~A ~A ~vA  ~A" hash date author-width author-text subject)
                             'git-entry (list :commit hash)
                             'git-fonts (list (cons 0 *git-hash-font*)
                                              (cons date-start *git-date-font*)
                                              (cons author-start *git-author-font*)
                                              (cons (+ author-start author-width) 0))))
         (list "No commits.")))
    (change-to-buffer buffer)
    (buffer-start (current-point))
    buffer))

(defcommand "Git Log" (p)
  "List the commits of this repository, the latest first; Return shows one."
  "List this repository's commits."
  (declare (ignore p))
  (let ((root (current-git-root)))
    (show-log root (car (last (pathname-directory root))))))

(defcommand "Git Log File" (p)
  "List the commits that changed this buffer's file, the latest first;
   Return shows one."
  "List the commits that changed this file."
  (declare (ignore p))
  (let* ((pathname (or (buffer-pathname (current-buffer))
                       (editor-error "The buffer has no file.")))
         (root (or (git-root (directory-namestring pathname))
                   (editor-error "Not in a Git repository."))))
    (show-log root (file-namestring pathname) (enough-namestring pathname root))))

(defmode "Git Blame" :major-p t
  :documentation "A file's lines, each with the commit that last changed
   it: Return shows the commit, q buries the list.")

(define-mode-highlighter "Git Blame" 'git-highlight-line :marks 'git-marks)

(defconstant +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0))

(defun short-date (seconds)
  (multiple-value-bind (s m h day month year) (decode-universal-time (+ seconds +unix-epoch+))
    (declare (ignore s m h))
    (format nil "~4,'0D-~2,'0D-~2,'0D" year month day)))

(defun parse-blame (output)
  "What git blame --line-porcelain printed, as (HASH DATE AUTHOR TEXT) for
   each line of the file."
  (let ((rows '()) (hash nil) (author "") (date ""))
    (dolist (line (git-lines output))
      (cond ((and (>= (length line) 41) (char= (char line 40) #\Space)
                  (every (lambda (c) (digit-char-p c 16)) (subseq line 0 40)))
             (setf hash (subseq line 0 40)))
            ((uiop:string-prefix-p "author " line) (setf author (subseq line 7)))
            ((uiop:string-prefix-p "author-time " line)
             (setf date (short-date (parse-integer line :start 12 :junk-allowed t))))
            ((uiop:string-prefix-p (string #\Tab) line)
             (push (list hash date author (subseq line 1)) rows))))
    (nreverse rows)))

(defcommand "Git Blame" (p)
  "Show this buffer's file, as last saved, with the commit that last
   changed each line, its date and author, point on the line it is on
   here; Return on a line shows its commit."
  "Show who last changed each line."
  (declare (ignore p))
  (let* ((buffer (current-buffer))
         (pathname (or (buffer-pathname buffer) (editor-error "The buffer has no file.")))
         (root (or (git-root (directory-namestring pathname))
                   (editor-error "Not in a Git repository.")))
         (number (count-lines (region (buffer-start-mark buffer) (current-point))))
         (rows (parse-blame (git-ok root "blame" "--line-porcelain" "--"
                                    (enough-namestring pathname root))))
         (width (min 16 (reduce #'max rows :key (lambda (row) (length (third row)))
                                           :initial-value 0)))
         (blame (make-git-buffer (format nil "*git blame: ~A*" (file-namestring pathname))
                                 "Git Blame" root)))
    (fill-git-buffer
     blame
     (loop for (hash date author text) in rows
           for short = (subseq hash 0 8)
           for prefix = (format nil "~A ~A ~vA │ " short date width
                                (subseq author 0 (min width (length author))))
           collect (list (concatenate 'string prefix text)
                         'git-entry (list :commit hash)
                         'git-fonts (list (cons 0 *git-hash-font*)
                                          (cons 9 *git-date-font*)
                                          (cons 20 *git-author-font*)
                                          (cons (+ 20 width) *git-comment-font*)
                                          (cons (length prefix) 0)))))
    (change-to-buffer blame)
    (buffer-start (current-point))
    (line-offset (current-point) (1- (min number (max 1 (length rows)))) 0)
    (when (buffer-modified buffer)
      (message "Blame of ~A as last saved." (file-namestring pathname)))
    blame))


;;;; Diffs of a file and of the repository.

(defun save-if-modified (buffer)
  (when (and (buffer-modified buffer) (buffer-pathname buffer))
    (save-file-command nil)))

(defcommand "Git Diff File" (p)
  "Save this buffer and show how its file differs from the last commit,
   in the other window; Return on a line visits it."
  "Show how this file differs from the last commit."
  (declare (ignore p))
  (let* ((buffer (current-buffer))
         (pathname (or (buffer-pathname buffer) (editor-error "The buffer has no file.")))
         (root (or (git-root (directory-namestring pathname))
                   (editor-error "Not in a Git repository."))))
    (save-if-modified buffer)
    (show-diff root (file-namestring pathname)
               (append (list "diff" "--src-prefix=a/" "--dst-prefix=b/")
                       (when (git-has-head-p root) (list "HEAD"))
                       (list "--" (enough-namestring pathname root))))))

(defcommand "Git Diff" (p)
  "Show how the repository's files differ from the last commit, in the
   other window; Return on a line visits it."
  "Show how the repository differs from the last commit."
  (declare (ignore p))
  (let ((root (current-git-root)))
    (show-diff root "the repository"
               (append (list "diff" "--src-prefix=a/" "--dst-prefix=b/")
                       (when (git-has-head-p root) (list "HEAD"))))))


;;;; The status buffer.

(defmode "Git Status" :major-p t
  :documentation "A repository's state: the files changed, staged and not,
   and the latest commits.  s stages what point is on -- a file, a hunk, or
   a section's files -- u unstages it, k discards it; Tab shows or hides a
   file's hunks; c commits (with an argument, amends); Return visits; d
   shows a diff; l the log; g makes the list again; P pushes, F pulls.")

(define-mode-highlighter "Git Status" 'git-highlight-line :marks 'git-marks)

(defvar *git-expanded* (make-hash-table :test 'equal)
  "(ROOT SECTION FILE) to T for each file whose hunks the status shows.")

(defparameter *git-section-titles*
  '((:unmerged . "Unmerged") (:untracked . "Untracked files")
    (:unstaged . "Unstaged changes") (:staged . "Staged changes")))

(defun git-change-word (char)
  (case char
    (#\M "modified") (#\A "new file") (#\D "deleted") (#\R "renamed")
    (#\C "copied") (#\T "typechange") (#\U "unmerged") (t "changed")))

(defun git-status-files (root)
  "The repository's changed files, as ((SECTION WORD FILE) ...)."
  (let ((fields (uiop:split-string (git-ok root "status" "--porcelain=v1" "-z"
                                           "--untracked-files=all")
                                   :separator (list (code-char 0))))
        (files '()))
    (loop while fields
          do (let ((field (pop fields)))
               (when (> (length field) 3)
                 (let ((x (char field 0)) (y (char field 1)) (file (subseq field 3)))
                   ;; A rename or copy, in either column, is followed by
                   ;; the path it came from.
                   (when (or (find x "RC") (find y "RC")) (pop fields))
                   (cond ((and (char= x #\?) (char= y #\?))
                          (push (list :untracked "" file) files))
                         ((or (char= x #\U) (char= y #\U)
                              (and (char= x #\A) (char= y #\A))
                              (and (char= x #\D) (char= y #\D)))
                          (push (list :unmerged "unmerged" file) files))
                         (t
                          (unless (char= x #\Space)
                            (push (list :staged (git-change-word x) file) files))
                          (unless (char= y #\Space)
                            (push (list :unstaged (git-change-word y) file) files))))))))
    (nreverse files)))

(defun split-diff (output)
  "A file's diff as its header's lines and its hunks, each a list of lines
   starting with its @@ line."
  (let ((header '()) (hunks '()))
    (dolist (line (git-lines output))
      (cond ((uiop:string-prefix-p "@@" line) (push (list line) hunks))
            (hunks (push line (first hunks)))
            (t (push line header))))
    (values (nreverse header) (nreverse (mapcar #'reverse hunks)))))

(defun file-diff (root section file)
  (git-ok root "diff" "--src-prefix=a/" "--dst-prefix=b/"
          (if (eq section :staged) "--cached" "--no-ext-diff")
          "--" file))

(defun git-status-header (root)
  (let* ((branch (string-trim '(#\Newline) (git root "symbolic-ref" "--short" "-q" "HEAD")))
         (last (and (git-has-head-p root)
                    (string-trim '(#\Newline) (git root "log" "-1" "--format=%h %s"))))
         (upstream (multiple-value-bind (output code)
                       (git root "rev-parse" "--abbrev-ref" "@{upstream}")
                     (and (eql code 0) (string-trim '(#\Newline) output))))
         (counts (and upstream
                      (uiop:split-string (string-trim '(#\Newline)
                                                      (git root "rev-list" "--left-right" "--count"
                                                           "@{upstream}...HEAD"))
                                         :separator '(#\Tab)))))
    (remove nil
            (list (list (format nil "Head:     ~A~@[  ~A~]"
                                (if (string= branch "") "(detached)" branch) last)
                        'git-entry (list :header t)
                        'git-fonts (list (cons 0 *git-label-font*) (cons 10 *git-section-font*)
                                         (cons (+ 10 (length (if (string= branch "") "(detached)" branch)))
                                               0)))
                  (when upstream
                    (list (format nil "Upstream: ~A~@[  (ahead ~A, behind ~A)~]" upstream
                                  (and (= (length counts) 2) (second counts))
                                  (and (= (length counts) 2) (first counts)))
                          'git-entry (list :header t)
                          'git-fonts (list (cons 0 *git-label-font*) (cons 10 0))))))))

(defun git-status-lines (root)
  "The status buffer's lines, each (STRING . PLIST)."
  (let ((files (git-status-files root))
        (lines (git-status-header root)))
    (flet ((add (string &rest plist) (push (cons string plist) lines)))
      (if (null files)
          (progn (add "")
                 (add "Nothing to commit: the working tree is clean."))
          (loop for (section . title) in *git-section-titles*
                for in-section = (remove section files :key #'first :test-not #'eq)
                when in-section
                  do (add "")
                     (add (format nil "~A (~D)" title (length in-section))
                          'git-entry (list :section section)
                          'git-fonts (list (cons 0 *git-section-font*)))
                     (loop for (nil word file) in in-section
                           do (add (format nil "  ~11A~A" word file)
                                   'git-entry (list :file file :section section)
                                   'git-fonts (list (cons 2 (cond ((string= word "new file") *diff-added-font*)
                                                                  ((string= word "deleted") *diff-removed-font*)
                                                                  (t 3)))
                                                    (cons 13 0)))
                              (when (and (gethash (list (namestring root) section file) *git-expanded*)
                                         (not (eq section :untracked)))
                                (multiple-value-bind (header hunks) (split-diff (file-diff root section file))
                                  (if (null hunks)
                                      (dolist (line header)
                                        (unless (or (uiop:string-prefix-p "diff " line)
                                                    (uiop:string-prefix-p "index " line))
                                          (add (concatenate 'string "    " line)
                                               'git-entry (list :file file :section section))))
                                      (dolist (hunk hunks)
                                        (loop for line in hunk
                                              for offset from 0
                                              do (add line
                                                      'git-diff t
                                                      'git-entry (list :hunk hunk :header header
                                                                       :offset offset
                                                                       :file file :section section))))))))))
      (when (git-has-head-p root)
        (add "")
        (add "Recent commits" 'git-entry (list :section :commits)
             'git-fonts (list (cons 0 *git-section-font*)))
        (dolist (line (git-lines (git root "log" "-10" "--format=%h %s")))
          (let ((space (or (position #\Space line) (length line))))
            (add (concatenate 'string "  " line)
                 'git-entry (list :commit (subseq line 0 space))
                 'git-fonts (list (cons 2 *git-hash-font*) (cons (+ 2 space) 0)))))))
    (nreverse lines)))

(defun git-status-buffer-name (root)
  (format nil "*git: ~A*" (car (last (pathname-directory root)))))

(defun git-status-fill (buffer)
  "Make BUFFER's status again, keeping point on what it was on."
  (let* ((root (buffer-git-root buffer))
         (point (buffer-point buffer))
         (old (git-entry (mark-line point)))
         (old-number (count-lines (region (buffer-start-mark buffer) point)))
         (lines (git-status-lines root)))
    (fill-git-buffer buffer lines)
    (let* ((key (and old (list (getf old :file) (getf old :section) (getf old :commit))))
           (target (or (and key (some #'identity key)
                            (do ((line (mark-line (buffer-start-mark buffer)) (line-next line)))
                                ((null line) nil)
                              (let ((entry (git-entry line)))
                                (when (and entry (not (eq (first entry) :hunk))
                                           (equal key (list (getf entry :file) (getf entry :section)
                                                            (getf entry :commit))))
                                  (return line)))))
                       nil)))
      (if target
          (move-to-position point 0 target)
          (progn (buffer-start point)
                 (line-offset point (min (1- old-number) (max 0 (1- (length lines)))) 0))))))

(defun git-status-buffers (root)
  (remove-if-not (lambda (buffer)
                   (and (string= (buffer-major-mode buffer) "Git Status")
                        (equal (buffer-git-root buffer) root)))
                 *buffer-list*))

(defun git-changed (root)
  "The repository ROOT has changed: make its status buffers again, and its
   files' marks."
  (dolist (buffer (git-status-buffers root))
    (git-status-fill buffer))
  (forget-git-bases root))

(defcommand "Git Status" (p)
  "Show the state of this buffer's repository: what is changed, staged and
   not, and the latest commits.  In it, s stages, u unstages, c commits."
  "Show the state of this repository."
  (declare (ignore p))
  (let* ((root (current-git-root))
         (buffer (or (find-if (lambda (b) (string= (buffer-major-mode b) "Git Status"))
                              (git-status-buffers root))
                     (make-git-buffer (git-status-buffer-name root) "Git Status" root))))
    (git-status-fill buffer)
    (change-to-buffer buffer)))

(defcommand "Git Status Refresh" (p)
  "Make the status again."
  "Make the status again."
  (declare (ignore p))
  (git-changed (status-root)))

(defun status-root ()
  (or (buffer-git-root (current-buffer)) (editor-error "Not a Git buffer.")))

(defun status-entry ()
  (git-entry (mark-line (current-point))))

(defun hunk-patch (entry &optional (lines (getf entry :hunk)))
  (format nil "~{~A~%~}~{~A~%~}" (getf entry :header) lines))

(defun section-files (section)
  (let ((files '()))
    (do ((line (mark-line (buffer-start-mark (current-buffer))) (line-next line)))
        ((null line))
      (let ((entry (git-entry line)))
        (when (and (eq (first entry) :file) (eq (getf entry :section) section))
          (pushnew (getf entry :file) files :test #'string=))))
    (nreverse files)))

(defun unstage-files (root files)
  (if (git-has-head-p root)
      (apply #'git-ok root "restore" "--staged" "--" files)
      (apply #'git-ok root "rm" "--cached" "-q" "-r" "--" files)))

(defcommand "Git Stage" (p)
  "Stage what point is on: a file, a hunk, or every file of the section
   whose title it is on."
  "Stage what point is on."
  (declare (ignore p))
  (let ((root (status-root)) (entry (status-entry)))
    (case (first entry)
      (:file
       (when (eq (getf entry :section) :staged) (editor-error "Already staged."))
       (git-ok root "add" "-A" "--" (getf entry :file)))
      (:hunk
       (unless (eq (getf entry :section) :unstaged) (editor-error "Not an unstaged hunk."))
       (call-with-patch-file (hunk-patch entry)
                             (lambda (file) (git-ok root "apply" "--cached" file))))
      (:section
       (let ((files (and (member (getf entry :section) '(:unstaged :untracked :unmerged))
                         (section-files (getf entry :section)))))
         (unless files (editor-error "Nothing here to stage."))
         (apply #'git-ok root "add" "-A" "--" files)))
      (t (editor-error "Nothing here to stage.")))
    (git-changed root)))

(defcommand "Git Unstage" (p)
  "Unstage what point is on: a file, a hunk, or every staged file."
  "Unstage what point is on."
  (declare (ignore p))
  (let ((root (status-root)) (entry (status-entry)))
    (unless (eq (getf entry :section) :staged)
      (editor-error "Not staged."))
    (case (first entry)
      (:file (unstage-files root (list (getf entry :file))))
      (:hunk (call-with-patch-file (hunk-patch entry)
                                   (lambda (file) (git-ok root "apply" "--cached" "--reverse" file))))
      (:section (unstage-files root (section-files :staged)))
      (t (editor-error "Nothing here to unstage.")))
    (git-changed root)))

(defcommand "Git Stage All" (p)
  "Stage every change, untracked files too."
  "Stage every change."
  (declare (ignore p))
  (let ((root (status-root)))
    (git-ok root "add" "-A")
    (git-changed root)))

(defcommand "Git Unstage All" (p)
  "Unstage everything staged."
  "Unstage everything."
  (declare (ignore p))
  (let* ((root (status-root))
         (files (section-files :staged)))
    (when files (unstage-files root files))
    (git-changed root)))

(defcommand "Git Discard" (p)
  "Throw away the unstaged change point is on, after asking: a file's
   changes, a hunk, or an untracked file, which is deleted."
  "Discard the change point is on."
  (declare (ignore p))
  (let* ((root (status-root)) (entry (status-entry))
         (file (getf entry :file)) (section (getf entry :section)))
    (when (eq section :staged)
      (editor-error "Unstage it first."))
    (case (first entry)
      (:file
       (case section
         (:untracked
          (when (prompt-for-y-or-n :prompt (format nil "Delete ~A? " file) :default nil)
            (delete-file (merge-pathnames file root))))
         (t
          (when (prompt-for-y-or-n :prompt (format nil "Discard the changes to ~A? " file) :default nil)
            (git-ok root "restore" "--" file)))))
      (:hunk
       (when (prompt-for-y-or-n :prompt "Discard this hunk? " :default nil)
         (call-with-patch-file (hunk-patch entry)
                               (lambda (patch) (git-ok root "apply" "--reverse" patch)))))
      (t (editor-error "Nothing here to discard.")))
    (revert-git-buffers root)
    (git-changed root)))

(defun revert-git-buffers (root)
  "Read again each unmodified buffer of ROOT's files whose file changed."
  (dolist (buffer *buffer-list*)
    (let ((pathname (buffer-pathname buffer)))
      (when (and pathname (not (buffer-modified buffer))
                 (uiop:subpathp pathname root)
                 (probe-file pathname)
                 (buffer-write-date buffer)
                 (/= (buffer-write-date buffer) (file-write-date pathname)))
        (let ((number (count-lines (region (buffer-start-mark buffer) (buffer-point buffer)))))
          (read-buffer-file pathname buffer)
          (line-offset (buffer-point buffer) (1- number) 0))))))

(defcommand "Git Status Toggle" (p)
  "Show or hide the hunks of the file point is on."
  "Show or hide this file's hunks."
  (declare (ignore p))
  (let* ((root (status-root)) (entry (status-entry))
         (key (list (namestring root) (getf entry :section) (getf entry :file))))
    (unless (member (first entry) '(:file :hunk))
      (editor-error "Not on a file."))
    (when (eq (getf entry :section) :untracked)
      (editor-error "An untracked file has no hunks."))
    (if (gethash key *git-expanded*)
        (remhash key *git-expanded*)
        (setf (gethash key *git-expanded*) t))
    (let ((buffer (current-buffer)))
      ;; Point goes to the file's own line.
      (do ((line (mark-line (current-point)) (line-previous line)))
          ((null line))
        (when (eq (first (git-entry line)) :file)
          (move-to-position (current-point) 0 line)
          (return)))
      (git-status-fill buffer))))

(defun hunk-location (root entry)
  (multiple-value-bind (match groups)
      (cl-ppcre:scan-to-strings *hunk-header-scanner* (first (getf entry :hunk)))
    (declare (ignore match))
    (let ((number (parse-integer (aref groups 0))))
      (loop for line in (rest (getf entry :hunk))
            repeat (max 0 (1- (getf entry :offset)))
            unless (uiop:string-prefix-p "-" line) do (incf number))
      (list (merge-pathnames (getf entry :file) root) (max 1 number)))))

(defcommand "Git Status Visit" (p)
  "Visit what point is on in the other window: a file, the line of a hunk
   it is on, or a commit."
  "Visit what point is on."
  (declare (ignore p))
  (let ((root (status-root)) (entry (status-entry)))
    (case (first entry)
      (:file (show-location (list (merge-pathnames (getf entry :file) root) 1) :select t))
      (:hunk (show-location (hunk-location root entry) :select t))
      (:commit (show-commit root (getf entry :commit)))
      (t (editor-error "Nothing here to visit.")))))

(defcommand "Git Status Diff" (p)
  "Show the diff of the file point is on, staged or not, or else of
   everything changed since the last commit."
  "Show a diff."
  (declare (ignore p))
  (let ((root (status-root)) (entry (status-entry)))
    (if (and (member (first entry) '(:file :hunk)) (not (eq (getf entry :section) :untracked)))
        (show-diff root (getf entry :file)
                   (list "diff" "--src-prefix=a/" "--dst-prefix=b/"
                         (if (eq (getf entry :section) :staged) "--cached" "--no-ext-diff")
                         "--" (getf entry :file)))
        (git-diff-command nil))))

(defun next-status-item (count)
  (let ((line (mark-line (current-point))))
    (dotimes (i (abs count))
      (loop (setf line (if (minusp count) (line-previous line) (line-next line)))
            (unless line (editor-error "No more."))
            (let ((entry (git-entry line)))
              (when (or (member (first entry) '(:file :commit :section))
                        (and (eq (first entry) :hunk) (zerop (getf entry :offset))))
                (return)))))
    (move-to-position (current-point) 0 line)))

(defcommand "Git Status Next" (p)
  "Go to the next file, hunk, section or commit."
  "Go to the next item."
  (next-status-item (or p 1)))

(defcommand "Git Status Previous" (p)
  "Go to the previous file, hunk, section or commit."
  "Go to the previous item."
  (next-status-item (- (or p 1))))

(defun run-git-process (root what &rest arguments)
  "Run git with ARGUMENTS in ROOT, writing what it says to the buffer
   *git output* in the other window, as it goes."
  (let ((here (current-window))
        (output (getstring "*git output*" *buffer-names*)))
    (unless (and output (result-window output))
      (select-window (other-window)))
    (start-result-command "*git output*" "Compilation"
                          (format nil "git~{ ~A~}" (mapcar #'shell-quote arguments))
                          (namestring root) what)
    (when (member here *window-list*)
      (select-window here))))

(defcommand "Git Push" (p)
  "Push the current branch to its upstream, showing what git says; with an
   argument, the command is offered to change first."
  "Push the current branch."
  (let ((root (current-git-root)))
    (if p
        (start-result-command "*git output*" "Compilation"
                              (prompt-for-string :prompt "Command: " :default "git push")
                              (namestring root) "Git")
        (run-git-process root "Git push" "push"))))

(defcommand "Git Pull" (p)
  "Pull into the current branch from its upstream, showing what git says;
   with an argument, the command is offered to change first."
  "Pull into the current branch."
  (let ((root (current-git-root)))
    (if p
        (start-result-command "*git output*" "Compilation"
                              (prompt-for-string :prompt "Command: " :default "git pull")
                              (namestring root) "Git")
        (run-git-process root "Git pull" "pull"))))


;;;; Committing: the message is written in a buffer of its own.

(defmode "Git Commit" :major-p t
  :documentation "A commit's message: C-c C-c commits, C-c C-k gives up.
   Lines starting with # are left out.")

(defun git-commit-highlight-line (line)
  (let ((string (line-string line)))
    (setf (getf (line-plist line) 'git-fonts)
          (cond ((uiop:string-prefix-p "#" string) (list (cons 0 *git-comment-font*)))
                ;; A summary longer than 72 characters, past them.
                ((and (null (line-previous line)) (> (length string) 72))
                 (list (cons 72 *diff-removed-font*)))
                (t '())))
    (git-highlight-line line)))

(define-mode-highlighter "Git Commit" 'git-commit-highlight-line :marks 'git-marks)

(defun open-commit-buffer (root amend)
  (let* ((staged (git-ok root "diff" "--cached" "--stat"))
         (buffer (or (getstring "*git commit*" *buffer-names*)
                     (make-buffer "*git commit*" :modes '("Git Commit")))))
    (when (and (not amend) (string= (string-trim '(#\Newline #\Space) staged) ""))
      (editor-error "Nothing is staged to commit."))
    (setf (buffer-major-mode buffer) "Git Commit")
    (unless (heml-bound-p 'git-root :buffer buffer)
      (defhvar "Git Root" "The repository this buffer is about." :buffer buffer)
      (defhvar "Git Amend" "Whether the commit amends the last." :buffer buffer))
    (setf (variable-value 'git-root :buffer buffer) root
          (variable-value 'git-amend :buffer buffer) amend)
    (with-writable-buffer (buffer)
      (delete-region (buffer-region buffer))
      (let ((mark (copy-mark (buffer-start-mark buffer) :left-inserting)))
        (insert-string mark (format nil "~A~%~%# ~:[Write~;Change~] the commit's message, then C-c C-c to commit~:*~:[~; (amending the last)~], or C-c C-k to give up.~%# Lines starting with # are left out.~%#~%~{# ~A~%~}"
                                    (if amend
                                        (string-right-trim '(#\Newline)
                                                           (git-ok root "log" "-1" "--format=%B"))
                                        "")
                                    amend
                                    (git-lines staged)))
        (delete-mark mark)))
    ;; WITH-WRITABLE-BUFFER leaves it read-only; this one is for writing.
    (setf (buffer-writable buffer) t
          (buffer-modified buffer) nil)
    (change-to-buffer buffer)
    (buffer-start (current-point))
    buffer))

(defcommand "Git Commit" (p)
  "Commit what is staged, with a message written in a buffer of its own;
   with an argument, amend the last commit.  When nothing is staged and
   this buffer's file has changed, it is staged first."
  "Commit what is staged."
  (let* ((root (or (buffer-git-root (current-buffer)) (current-git-root)))
         (pathname (buffer-pathname (current-buffer))))
    (when (and (not p) pathname
               (string= (string-trim '(#\Newline #\Space) (git-ok root "diff" "--cached" "--stat")) ""))
      (save-if-modified (current-buffer))
      (when (string/= "" (git root "status" "--porcelain" "--" (enough-namestring pathname root)))
        (git-ok root "add" "-A" "--" (enough-namestring pathname root))
        (git-changed root)))
    (open-commit-buffer root (and p t))))

(defcommand "Git Commit Finish" (p)
  "Commit, with this buffer's message, leaving out the lines that start
   with #."
  "Commit with this message."
  (declare (ignore p))
  (let* ((buffer (current-buffer))
         (root (buffer-git-root buffer))
         (amend (variable-value 'git-amend :buffer buffer))
         (message (region-to-string (buffer-region buffer))))
    (let ((output (call-with-patch-file
                   message
                   (lambda (file)
                     (apply #'git-ok root "commit" "--cleanup=strip" "-F" file
                            (when amend (list "--amend")))))))
      (bury-git-buffer buffer)
      (git-changed root)
      (message "~A" (or (first (git-lines output)) "Committed.")))))

(defun bury-git-buffer (buffer)
  (change-to-buffer (or (find-if (lambda (b) (and (not (eq b buffer))
                                                  (not (eq b *echo-area-buffer*))))
                                 *buffer-history*)
                        buffer))
  (unless (eq buffer (current-buffer))
    (delete-buffer buffer)))

(defcommand "Git Commit Abort" (p)
  "Give up the commit."
  "Give up the commit."
  (declare (ignore p))
  (bury-git-buffer (current-buffer))
  (message "No commit."))


;;;; Marks in the fringe beside the lines of a file Git tracks that differ
;;;; from the last commit: a green bar beside lines added, a blue one beside
;;;; lines changed, and a red edge where lines were taken out.  Once a
;;;; second, each buffer shown that has changed since is compared with the
;;;; file as last committed (diff -U0 on the two), and the marks kept by
;;;; line; the commit's text is asked for once, and again when the
;;;; repository's index changes, as a commit or a checkout changes it.

(defhvar "Git Fringe"
  "When true, the lines of a file Git tracks that differ from the last
   commit are marked in the fringe."
  :value t)

(defparameter *git-fringe-line-limit* 100000
  "A buffer with more lines than this is not compared.")

(defstruct (git-file (:constructor make-git-file (root name directory)))
  root name directory
  (base nil)                            ; the file as committed, or NIL
  (signature nil)                       ; the buffer's when compared
  (marks nil))                          ; line to :added, :changed or :deleted

(defvar *git-files* (make-hash-table :test 'eq)
  "Each buffer of a file Git tracks, to its git-file.")

(defvar *git-index-dates* (make-hash-table :test 'equal)
  "Each repository's index file's date when last looked at.")

(defun note-git-buffer (buffer pathname)
  (remhash buffer *git-files*)
  (setf (buffer-fringe-columns buffer :git) 0)
  (when (and pathname (value git-fringe) (probe-file pathname))
    (let* ((directory (directory-namestring pathname))
           (root (git-root directory)))
      (when (and root
                 (eql 0 (nth-value 1 (ignore-errors
                                      (git directory "ls-files" "--error-unmatch" "--"
                                           (file-namestring pathname))))))
        (setf (gethash buffer *git-files*)
              (make-git-file root (file-namestring pathname) directory)
              (buffer-fringe-columns buffer :git) 1)))))

(defun forget-git-buffer (buffer)
  (remhash buffer *git-files*)
  (setf (buffer-fringe-columns buffer :git) 0))

(defun forget-git-bases (root)
  (maphash (lambda (buffer file)
             (declare (ignore buffer))
             (when (equal (git-file-root file) root)
               (setf (git-file-base file) nil
                     (git-file-signature file) nil)))
           *git-files*))

(defun git-file-base-text (file)
  (or (git-file-base file)
      (setf (git-file-base file)
            (multiple-value-bind (output code)
                (git (git-file-directory file) "show"
                     (format nil "HEAD:./~A" (git-file-name file)))
              ;; Not in the last commit: every line is new.
              (if (eql code 0) output "")))))

(defun write-buffer-text (buffer pathname)
  (with-open-file (out pathname :direction :output :if-exists :supersede
                                :external-format :utf-8)
    (do ((line (mark-line (buffer-start-mark buffer)) (line-next line)))
        ((null line))
      (write-string (line-string line) out)
      ;; A file ending in a newline is a buffer whose last line is empty.
      (when (line-next line) (terpri out)))))

(defparameter *diff-range-scanner*
  (cl-ppcre:create-scanner "^@@ -\\d+(?:,(\\d+))? \\+(\\d+)(?:,(\\d+))? @@"))

(defun diff-line-kinds (output)
  "From diff -U0's OUTPUT, what each line of the new file is: ((NUMBER .
   KIND) ...), KIND :added, :changed, :deleted (lines taken out after it)
   or :deleted-before (lines taken out before the first)."
  (let ((kinds '()))
    (dolist (line (git-lines output))
      (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings *diff-range-scanner* line)
        (when match
          (let ((old (if (aref groups 0) (parse-integer (aref groups 0)) 1))
                (start (parse-integer (aref groups 1)))
                (new (if (aref groups 2) (parse-integer (aref groups 2)) 1)))
            (cond ((zerop new)
                   (push (if (zerop start) (cons 1 :deleted-before) (cons start :deleted)) kinds))
                  (t
                   (loop for number from start below (+ start new)
                         do (push (cons number (if (zerop old) :added :changed)) kinds))))))))
    kinds))

(defun compare-git-buffer (buffer file)
  (let ((base (git-file-base-text file))
        (lines (count-lines (buffer-region buffer))))
    (setf (git-file-signature file) (buffer-signature buffer))
    (if (> lines *git-fringe-line-limit*)
        (setf (git-file-marks file) nil)
        (uiop:with-temporary-file (:pathname old :prefix "heml-git-old-")
          (uiop:with-temporary-file (:pathname new :prefix "heml-git-new-")
            (with-open-file (out old :direction :output :if-exists :supersede
                                     :external-format :utf-8)
              (write-string base out))
            (write-buffer-text buffer new)
            (let ((output (uiop:run-program (list "diff" "-U0" (uiop:native-namestring old)
                                                  (uiop:native-namestring new))
                                            :output :string :external-format :utf-8
                                            :ignore-error-status t))
                  (marks (make-hash-table :test 'eq)))
              (let ((kinds (sort (diff-line-kinds output) #'< :key #'car))
                    (number 1)
                    (line (mark-line (buffer-start-mark buffer))))
                (dolist (kind kinds)
                  (loop while (and line (< number (car kind)))
                        do (setf line (line-next line)) (incf number))
                  (when line
                    (setf (gethash line marks) (cdr kind)))))
              (setf (git-file-marks file) marks)
              (incf hi:*decoration-tick*)))))))

(defvar *git-index-files* (make-hash-table :test 'equal)
  "Each repository's index file: in a linked worktree or a submodule .git is
   a file, and the index is elsewhere.")

(defun git-index-file (root)
  (let ((key (namestring root)))
    (or (gethash key *git-index-files*)
        (setf (gethash key *git-index-files*)
              (let ((path (string-trim '(#\Newline)
                                       (or (ignore-errors (git root "rev-parse" "--git-path" "index"))
                                           ""))))
                (if (plusp (length path))
                    (merge-pathnames path root)
                    (merge-pathnames ".git/index" root)))))))

(defun git-index-changed-p (root)
  (let* ((index (git-index-file root))
         (date (and (probe-file index) (file-write-date index)))
         (key (namestring root)))
    (unless (eql date (gethash key *git-index-dates*))
      (setf (gethash key *git-index-dates*) date)
      t)))

(defun git-idle (elapsed)
  (declare (ignore elapsed))
  (when (value git-fringe)
    (let ((roots '()))
      (dolist (window *window-list*)
        (let* ((buffer (window-buffer window))
               (file (and buffer (gethash buffer *git-files*))))
          (when file
            (let ((root (git-file-root file)))
              (unless (member root roots :test #'equal)
                (push root roots)
                (when (git-index-changed-p root)
                  (forget-git-bases root))))
            (unless (eql (git-file-signature file) (buffer-signature buffer))
              (ignore-errors (compare-git-buffer buffer file)))))))))

(defparameter *git-mark-fonts*
  '((:added "▎" (:fg 2)) (:changed "▎" (:fg 4))
    (:deleted "▁" (:fg 1)) (:deleted-before "▔" (:fg 1))))

(defun git-line-fringe (line)
  (let* ((buffer (line-buffer line))
         (file (and buffer (gethash buffer *git-files*)))
         (marks (and file (git-file-marks file)))
         (kind (and marks (gethash line marks))))
    (when kind
      (destructuring-bind (text font) (cdr (assoc kind *git-mark-fonts*))
        (list (list (buffer-fringe-column buffer :git) text font))))))

(pushnew 'git-line-fringe hi:*line-fringe-functions*)

(defun start-git-idle ()
  (remove-scheduled-event 'git-idle)
  (schedule-event 1 'git-idle))

(add-hook buffer-pathname-hook 'note-git-buffer)
(add-hook delete-buffer-hook 'forget-git-buffer)
(add-hook entry-hook 'start-git-idle)


;;;; The Git menu.

(define-menu "Git" (:after "Project")
  ("Status" "Git Status")
  ("Diff File" "Git Diff File")
  ("Diff Repository" "Git Diff")
  :separator
  ("Commit…" "Git Commit")
  ("Log" "Git Log")
  ("Log of File" "Git Log File")
  ("Blame" "Git Blame")
  :separator
  ("Pull" "Git Pull")
  ("Push" "Git Push"))
