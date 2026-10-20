;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Dired's colours.
;;;
;;; Each column of a file's line has its own: the permissions a character at
;;; a time, the size by how large it is, the date by how old the file is,
;;; and the name by what Git says of the file, else by $LS_COLORS when it is
;;; set, else by what kind of file it is.  What a line's colours need to
;;; know beyond its text -- the file's date, whether a link is broken, what
;;; Git says -- is found when the listing is made and kept in the line's
;;; plist as DIRED-FACTS, so redisplay never asks the file system or Git.

(in-package :heml)

(defhvar "Dired Git Status"
  "When true, Dired colours a file's name by what Git says of it: changed,
   new or ignored."
  :value t)

(defhvar "Dired Use LS_COLORS"
  "When true and $LS_COLORS is set, Dired colours names as ls does with it."
  :value t)


;;;; Git.

(defun dired-git-statuses (directory)
  "What Git says of the files in DIRECTORY's repository, as a hash table
   from a file's namestring (a directory's ending in /) to :MODIFIED, :NEW,
   :IGNORED, or :INSIDE for a directory holding a changed or new file; NIL
   outside a repository."
  (let ((root (ignore-errors (git-root directory))))
    (when root
      (let* ((root (namestring root))
             (output (ignore-errors
                      (git root "status" "--porcelain=v1" "-z"
                           "--untracked-files=all" "--ignored=matching")))
             (statuses (make-hash-table :test #'equal)))
        (when output
          (let ((entries (uiop:split-string output :separator (string (code-char 0)))))
            (loop while entries
                  do (let ((entry (pop entries)))
                       (when (> (length entry) 3)
                         ;; A rename or a copy is followed by the old name.
                         (when (find (char entry 0) "RC")
                           (pop entries))
                         (let ((path (concatenate 'string root (subseq entry 3)))
                               (kind (cond ((string= entry "??" :end1 2) :new)
                                           ((string= entry "!!" :end1 2) :ignored)
                                           (t :modified))))
                           (setf (gethash path statuses) kind)
                           (unless (eq kind :ignored)
                             ;; Each directory from the file up to the root.
                             (loop for end = (position #\/ path :from-end t
                                                                :end (1- (length path)))
                                     then (position #\/ path :from-end t :end end)
                                   while (and end (>= end (length root)))
                                   do (let ((directory (subseq path 0 (1+ end))))
                                        (unless (gethash directory statuses)
                                          (setf (gethash directory statuses) :inside)))))))))))
        statuses))))

(defun dired-git-status (namestring statuses top)
  "What STATUSES say of NAMESTRING: its own state, or :IGNORED when a
   directory above it and below TOP, the directory listed, is ignored.  So a
   listing of an ignored directory is not all grey."
  (when statuses
    (or (gethash namestring statuses)
        (loop for end = (position #\/ namestring :from-end t
                                                 :end (max 0 (1- (length namestring))))
                then (position #\/ namestring :from-end t :end end)
              while (and end (>= end (length top)))
              when (eq (gethash (subseq namestring 0 (1+ end)) statuses) :ignored)
                return :ignored))))


;;;; Facts.

;;; Git names files by where they really are: a directory reached through a
;;; link (/tmp is /private/tmp) is listed under the name it was given.
;;;
(defun dired-real-top (directory)
  "DIRECTORY's namestring, and where it really is, as (GIVEN . REAL)."
  (let ((given (directory-namestring directory)))
    (cons given (or (ignore-errors (namestring (truename given))) given))))

(defun dired-real-name (name top)
  (destructuring-bind (given . real) top
    (if (and (string/= given real) (uiop:string-prefix-p given name))
        (concatenate 'string real (subseq name (length given)))
        name)))

(defun dired-file-facts (pathname line statuses top)
  "What colouring LINE, PATHNAME's listing, needs that its text does not say."
  (let ((name (namestring pathname)))
    (list :mtime (ignore-errors (file-write-date name))
          :broken (and (plusp (length line))
                       (char= (char line 0) #\l)
                       (not (ignore-errors (probe-file name))))
          :git (dired-git-status (dired-real-name name top) statuses (cdr top)))))

(defun dired-file-lines (buffer)
  "The first file line of the Dired buffer BUFFER."
  (let ((line (mark-line (buffer-start-mark buffer))))
    (loop repeat +dired-header-lines+
          while line
          do (setf line (line-next line)))
    line))

(defun dired-note-line-facts (buffer directory entries)
  "Keep each file's facts in its line of BUFFER, ENTRIES being the listing's
   ((PATHNAME . LINE) ...) in order."
  (let ((statuses (and (value dired-git-status) (dired-git-statuses directory)))
        (top (dired-real-top directory)))
    (loop for line = (dired-file-lines buffer) then (line-next line)
          for (pathname . text) in entries
          while line
          do (setf (getf (line-plist line) 'dired-facts)
                   (dired-file-facts pathname text statuses top)))))

(defun dired-refresh-git (buffer)
  "Ask Git again about the files the Dired buffer BUFFER lists."
  (let ((info (variable-value 'dired-information :buffer buffer)))
    (when info
      (let ((statuses (and (value dired-git-status)
                           (dired-git-statuses (dired-info-pathname info))))
            (top (dired-real-top (dired-info-pathname info))))
        (loop for line = (dired-file-lines buffer) then (line-next line)
              for file across (dired-info-files info)
              while line
              do (let ((facts (getf (line-plist line) 'dired-facts))
                       (git (dired-git-status (dired-real-name
                                               (namestring (dired-file-pathname file)) top)
                                              statuses (cdr top))))
                   ;; A new list, so that the highlighter sees the change.
                   (unless (eq git (getf facts :git))
                     (setf (getf (line-plist line) 'dired-facts)
                           (list* :git git facts)))))))))

(defun dired-buffers-under (namestring)
  "The Dired buffers whose directory holds NAMESTRING, or is held by it."
  (loop for (directory . buffer) in *pathnames-to-dired-buffers*
        when (and (member buffer *buffer-list*)
                  (let ((length (min (length directory) (length namestring))))
                    (string= directory namestring :end1 length :end2 length)))
          collect buffer))

(defun dired-note-file-written (buffer)
  (let ((pathname (buffer-pathname buffer)))
    (when pathname
      (mapc #'dired-refresh-git (dired-buffers-under (namestring pathname))))))

(defun dired-note-git-changed (root)
  (mapc #'dired-refresh-git (dired-buffers-under (namestring root))))

(add-hook write-file-hook 'dired-note-file-written)
(pushnew 'dired-note-git-changed *git-change-functions*)


;;;; $LS_COLORS.

(defun sgr-plist (parameters)
  "The font SGR PARAMETERS (\"01;34\") describe, as a property list, or NIL
   when they describe none."
  (let ((codes (mapcar (lambda (p) (or (parse-integer p :junk-allowed t) 0))
                       (cl-ppcre:split ";" parameters)))
        (font '()))
    (flet ((basic (n) (if (zerop n) nil n))
           (indexed (n)
             (cond ((< n 8) (if (zerop n) nil n))
                   ((< n 16) (if (= n 8) 8 (- n 8)))
                   (t n))))
      (flet ((colour (key)
               (case (pop codes)
                 (5 (setf (getf font key) (indexed (or (pop codes) 0))))
                 (2 (setf (getf font key) (list (or (pop codes) 0) (or (pop codes) 0)
                                                (or (pop codes) 0)))))))
        (loop while codes
              do (let ((code (pop codes)))
                   (cond ((= code 0) (setf font '()))
                         ((= code 1) (setf (getf font :bold) t))
                         ((= code 3) (setf (getf font :italic) t))
                         ((= code 4) (setf (getf font :underline) t))
                         ((<= 30 code 37) (setf (getf font :fg) (basic (- code 30))))
                         ((<= 90 code 97) (setf (getf font :fg) (basic (- code 90))))
                         ((<= 40 code 47) (setf (getf font :bg) (basic (- code 40))))
                         ((<= 100 code 107) (setf (getf font :bg) (basic (- code 100))))
                         ((= code 38) (colour :fg))
                         ((= code 48) (colour :bg)))))))
    (loop for (key value) on font by #'cddr
          when value append (list key value))))

(defvar *ls-colors* (cons nil nil)
  "The last $LS_COLORS read, and its table.")

(defun ls-colors-table (string)
  "STRING, as $LS_COLORS has it, as (CODES . PATTERNS): an alist of
   two-letter codes and fonts, and one of name endings and fonts, the
   longest first."
  (let ((codes '()) (patterns '()))
    (dolist (entry (cl-ppcre:split ":" string))
      (let ((equals (position #\= entry)))
        (when equals
          (let ((key (subseq entry 0 equals))
                (font (sgr-plist (subseq entry (1+ equals)))))
            (when font
              (if (and (plusp (length key)) (char= (char key 0) #\*))
                  (push (cons (subseq key 1) font) patterns)
                  (push (cons key font) codes)))))))
    (cons codes (stable-sort (nreverse patterns) #'> :key (lambda (p) (length (car p)))))))

(defun ls-colors ()
  "The table of $LS_COLORS, or NIL when it is not set or not to be used."
  (let ((string (uiop:getenv "LS_COLORS")))
    (when (and string (plusp (length string)) (value dired-use-ls-colors))
      (unless (equal string (car *ls-colors*))
        (setf *ls-colors* (cons string (ls-colors-table string))))
      (cdr *ls-colors*))))

(defun ls-colors-font (table type permissions name broken)
  "The font TABLE gives a file whose type letter is TYPE, with PERMISSIONS
   (as ls shows them, less the type) and NAME, or NIL."
  (destructuring-bind (codes . patterns) table
    (flet ((code (key) (cdr (assoc key codes :test #'string=))))
      (case type
        (#\d (or (and (char= (char permissions 7) #\w)
                      (code (if (find (char permissions 8) "tT") "tw" "ow")))
                 (code "di")))
        (#\l (if broken (or (code "or") (code "ln")) (code "ln")))
        (#\p (code "pi"))
        (#\s (code "so"))
        (#\b (code "bd"))
        (#\c (code "cd"))
        (t
         (or (and (find (char permissions 2) "sS") (code "su"))
             (and (find (char permissions 5) "sS") (code "sg"))
             (and (find #\x permissions) (code "ex"))
             (cdr (find-if (lambda (pattern)
                             (let ((suffix (car pattern)))
                               (and (>= (length name) (length suffix))
                                    (string-equal suffix name
                                                  :start2 (- (length name) (length suffix))))))
                           patterns))
             (code "fi")))))))


;;;; Heml's own colours.

(defparameter *dired-grey* 8
  "Heml's grey: the system's in Cocoa, a terminal's bright black.")

(defparameter *dired-dark-grey* '(:fg 242)
  "An older date's grey.  A font that is a number must be under 16.")

(defparameter *dired-kind-types*
  '((:image "png" "jpg" "jpeg" "gif" "bmp" "tif" "tiff" "webp" "svg" "ico" "heic"
     "psd" "icns")
    (:media "mp3" "wav" "flac" "ogg" "m4a" "aac" "aiff" "mp4" "mkv" "mov" "avi"
     "webm" "m4v")
    (:faded "bak" "orig" "swp" "tmp" "fasl" "o" "a" "pyc" "class" "elc" "dx64fsl"
     "fas"))
  "The kinds of file Dired colours by their type.")

(defparameter *dired-build-names*
  '("Makefile" "makefile" "GNUmakefile" "CMakeLists.txt" "package.json"
    "Cargo.toml" "go.mod" "Dockerfile" "build.sh" "pyproject.toml" "ocicl.csv")
  "Names of files that build a project, which Dired underlines.")

(defparameter *dired-kind-fonts*
  `((:directory . (:fg 4 :bold t))
    (:link . 6)
    (:broken . (:fg 1 :italic t))
    (:special . 3)
    (:executable . 2)
    (:archive . 1)
    (:image . 5)
    (:media . (:fg 5 :bold t))
    (:build . (:fg 3 :bold t :underline t))
    (:faded . ,*dired-grey*)
    (:hidden . ,*dired-grey*))
  "The font for each kind of file Dired tells apart.")

(defun dired-name-kind (type permissions name broken)
  "What kind of file Dired is to colour NAME as, or NIL for none."
  (let* ((base (let ((slash (position #\/ name :from-end t :end (max 0 (1- (length name))))))
                 (if slash (subseq name (1+ slash)) name)))
         (dot (position #\. base :from-end t))
         (extension (and dot (plusp dot) (subseq base (1+ dot)))))
    (case (if (zerop (length base)) #\- type)
      (#\d :directory)
      (#\l (if broken :broken :link))
      ((#\p #\s #\b #\c) :special)
      (t
       (cond ((zerop (length base)) nil)
             ((find #\x permissions) :executable)
             ((or (archive-suffix base) (compressed-entry base)) :archive)
             ((member base *dired-build-names* :test #'string=) :build)
             ((and extension (string-equal extension "asd")) :build)
             ((or (char= (char base (1- (length base))) #\~)
                  (and (> (length base) 1)
                       (char= (char base 0) #\#)
                       (char= (char base (1- (length base))) #\#)))
              :faded)
             ((and extension
                   (car (find-if (lambda (kind)
                                   (member extension (cdr kind) :test #'string-equal))
                                 *dired-kind-types*))))
             ((char= (char base 0) #\.) :hidden))))))


;;;; A line's colours.

;;; The columns of a file's line: flag, type, permissions, links, owner,
;;; size and date, and then the name.
;;;
(defparameter *dired-columns-scanner*
  (cl-ppcre:create-scanner
   "^(.) ([dlpscb-])(\\S{9})(\\s+\\d+)(\\s+\\S+)(\\s+\\S+)(\\s+\\w{3}\\s+\\d+\\s+(?:\\d\\d:\\d\\d|\\d{4})) "))

(defun permission-font (char)
  (case char
    (#\r 3)
    (#\w 1)
    (#\x 2)
    ((#\s #\S #\t #\T) 5)
    (t *dired-grey*)))

(defun dired-size-font (size directoryp)
  (let ((unit (char size (1- (length size)))))
    (cond (directoryp *dired-grey*)
          ((digit-char-p unit) *dired-grey*)
          ((char= unit #\K) 0)
          ((char= unit #\M) 3)
          (t '(:fg 1 :bold t)))))

(defun dired-date-font (mtime now)
  (if (null mtime)
      0
      (let ((age (- now mtime)))
        (cond ((< age 3600) '(:fg 2 :bold t))
              ((< age 86400) 2)
              ((< age (* 7 86400)) 0)
              ((< age (* 365 86400)) *dired-grey*)
              (t *dired-dark-grey*)))))

(defun dired-name-font (type permissions name facts)
  (let ((broken (getf facts :broken)))
    (or (case (getf facts :git)
          (:modified (if (char= type #\d) '(:fg 208 :bold t) '(:fg 208)))
          (:new (if (char= type #\d) '(:fg 2 :bold t) '(:fg 2)))
          (:inside (and (char= type #\d) '(:fg 208 :bold t)))
          (:ignored *dired-grey*))
        (let ((table (ls-colors)))
          (and table (ls-colors-font table type permissions name broken)))
        (cdr (assoc (dired-name-kind type permissions name broken) *dired-kind-fonts*))
        0)))

(defun dired-header-fonts (string)
  (let ((open (search "  (" string :from-end t)))
    (if open
        (list (cons 0 '(:fg 4 :bold t)) (cons open *dired-grey*))
        (list (cons 0 '(:bold t))))))

(defun dired-line-fonts (string &optional facts (now (get-universal-time)))
  "Where the Dired line STRING's colours start, as ((POSITION . FONT) ...),
   FACTS being what its listing knew of the file."
  (multiple-value-bind (start end starts ends) (cl-ppcre:scan *dired-columns-scanner* string)
    (cond
      ((zerop (length string)) '())
      ((not start)
       (if (member (char string 0) '(#\Space #\D)) '() (dired-header-fonts string)))
      ((char= (char string 0) #\D) (list (cons 0 1)))
      ((char= (char string 0) #\*) (list (cons 0 '(:fg 3 :bold t))))
      (t
       (let* ((type (char string 2))
              (permissions (subseq string 3 12))
              (owner (string-left-trim " " (subseq string (aref starts 4) (aref ends 4))))
              (size (string-left-trim " " (subseq string (aref starts 5) (aref ends 5))))
              (arrow (search " -> " string :start2 end))
              (name (subseq string end (or arrow (length string))))
              (fonts '()))
         (flet ((font (position font)
                  ;; A font the same as the one before needs no mark.
                  (unless (and fonts (equal (cdar fonts) font))
                    (push (cons position font) fonts))))
           (font 2 (case type
                     (#\d '(:fg 4 :bold t))
                     (#\l 6)
                     (#\- *dired-grey*)
                     (t 3)))
           (loop for i from 3 below 12
                 do (font i (permission-font (char string i))))
           (font (aref starts 3) *dired-grey*)
           (font (aref starts 4) (if (equal owner (uiop:getenv "USER")) *dired-grey* 0))
           (font (aref starts 5) (dired-size-font size (char= type #\d)))
           (font (aref starts 6) (dired-date-font (getf facts :mtime) now))
           (font (aref ends 6) 0)
           (font end (dired-name-font type permissions name facts))
           (when arrow (font arrow 0)))
         (nreverse fonts))))))

(defun dired-highlight-line (line)
  (let ((old (getf (line-plist line) 'dired-marks))
        (facts (getf (line-plist line) 'dired-facts)))
    (unless (and old
                 (eq (car (car old)) (line-signature line))
                 (eq (cdr (car old)) facts))
      (dolist (mark (cdr old))
        (delete-font-mark mark))
      (setf (getf (line-plist line) 'dired-marks)
            (cons (cons (line-signature line) facts)
                  (loop for (position . font) in (dired-line-fonts (line-string line) facts)
                        collect (font-mark line position font)))))))

(define-mode-highlighter "Dired" 'dired-highlight-line :marks 'dired-marks)
(define-mode-highlighter "Wdired" 'dired-highlight-line :marks 'dired-marks)
