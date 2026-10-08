;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Projects.  A file's project is the tree it is in: the nearest directory
;;; above it holding a version-control marker (.git and the like), or failing
;;; that a build file (Makefile, an .asd, package.json and the like).  Nothing
;;; is declared.  C-x p finds a file in the project, searches, compiles, runs
;;; a shell or Dired at its root, and switches to another project known from
;;; the files visited.
;;;
;;; Sessions: what a project had open -- its files, their points, and the
;;; window layout -- is saved when the editor leaves the project or exits,
;;; and reopened when it switches to the project, or starts in it with no
;;; files to edit.  The projects known and their sessions are kept in
;;; *PROJECT-STATE-DIRECTORY*: $XDG_STATE_HOME/heml/ (~/.local/state/heml/)
;;; unless HEML_STATE_DIRECTORY says.

(in-package :heml)


;;;; Finding a project.

(defvar *project-markers* '(".heml-project" ".git" ".hg" ".svn" "_darcs" ".fossil")
  "What makes a directory a project's root, nearest first.")

(defvar *project-build-markers*
  '("Makefile" "*.asd" "package.json" "Cargo.toml" "pyproject.toml" "go.mod"
    "CMakeLists.txt" "*.lpi" "*.lpr")
  "What makes a directory a project's root when no directory above holds one
   of *PROJECT-MARKERS*: the nearest holding one.")

(defvar *project-roots* (make-hash-table :test 'equal)
  "Directory to its project's root, or NIL, as found.")

(defun directory-holds-p (directory marker)
  (if (find #\* marker)
      (directory (merge-pathnames marker directory))
      (probe-file (concatenate 'string directory marker))))

(defun find-project-root (directory)
  (let ((home (namestring (user-homedir-pathname)))
        (build nil))
    (loop for pathname = (pathname directory)
            then (uiop:pathname-parent-directory-pathname pathname)
          for name = (namestring pathname)
          until (or (string= name "/") (string= name home))
          do (when (some (lambda (marker) (directory-holds-p name marker)) *project-markers*)
               (return-from find-project-root name))
             (when (and (not build)
                        (some (lambda (marker) (directory-holds-p name marker))
                              *project-build-markers*))
               (setf build name))
          ;; At the top of a relative or odd pathname, stop.
          until (equal pathname (uiop:pathname-parent-directory-pathname pathname)))
    build))

(defun project-root (&optional (directory (default-directory)))
  "The root of DIRECTORY's project, as a directory's namestring, or NIL."
  (let ((directory (namestring (uiop:ensure-directory-pathname directory))))
    (multiple-value-bind (root found) (gethash directory *project-roots*)
      (if found
          root
          (setf (gethash directory *project-roots*) (find-project-root directory))))))

(defun buffer-project-root (buffer)
  "The root of the project BUFFER's file is in, or NIL."
  (let ((pathname (buffer-pathname buffer)))
    (and pathname (project-root (directory-namestring pathname)))))

(defun project-name (root)
  (or (getf (project-settings root) :name)
      (car (last (pathname-directory (pathname root))))))

(defun current-project-root ()
  (or (buffer-project-root (current-buffer))
      (project-root (default-directory))
      (editor-error "Not in a project.")))

(defun under-root-p (pathname root)
  (and pathname (eql 0 (search root (namestring pathname)))))

(defun project-buffers (root)
  "The buffers visiting files in ROOT's project, most recent first."
  (remove-if-not (lambda (buffer) (under-root-p (buffer-pathname buffer) root))
                 (remove-duplicates (append *buffer-history* *buffer-list*) :from-end t)))


;;;; A project's settings.

;;; A file .heml-project at a project's root says things about it, as a
;;; property list, read as data and never evaluated:
;;;
;;;   (:name "Heml"                        ; what the modeline calls it
;;;    :compile "make smoke-tty"           ; what C-x p c offers at first
;;;    :ignore ("build/" "*.fasl")         ; files C-x p f leaves out
;;;    :variables (("Fill Column" . 100)   ; Heml variables, set in each of
;;;                ("Indent with Tabs" . nil)) ; the project's file buffers
;;;    :settings (("yaml" ("schemas" ("file:///x/schema.json" . "*.yaml")))))
;;;                                        ; its language servers' settings:
;;;                                        ; JSON, an object an alist (lsp.lisp)
;;;
;;; Its presence also makes its directory a project's root.

(defvar *project-settings* (make-hash-table :test 'equal)
  "Root to (WRITE-DATE . SETTINGS), as last read.")

(defun project-settings-file (root)
  (concatenate 'string root ".heml-project"))

(defun project-settings (root)
  "ROOT's project's settings, a property list, read again when its file
   has changed."
  (let* ((file (project-settings-file root))
         (date (and (probe-file file) (ignore-errors (file-write-date file))))
         (cached (gethash root *project-settings*)))
    (cond ((null date) nil)
          ((and cached (eql (car cached) date)) (cdr cached))
          (t (cdr (setf (gethash root *project-settings*)
                        (cons date
                              (let ((form (ignore-errors
                                           (with-open-file (in file :external-format :utf-8)
                                             (with-standard-io-syntax
                                               (let ((*read-eval* nil)
                                                     (*package* (find-package :keyword)))
                                                 (read in nil nil)))))))
                                (and (listp form) (evenp (length form)) form)))))))))

(defun ignored-file-p (file patterns)
  "Whether FILE, relative to its root, is one PATTERNS leave out: a pattern
   ending in / is a directory anywhere in the path, one starting with * the
   end of the name, and any other the start of the path."
  (some (lambda (pattern)
          (let ((length (length pattern)))
            (cond ((zerop length) nil)
                  ((char= (char pattern 0) #\*)
                   (let ((tail (subseq pattern 1)))
                     (and (>= (length file) (length tail))
                          (string= tail file :start2 (- (length file) (length tail))))))
                  ((char= (char pattern (1- length)) #\/)
                   (or (eql 0 (search pattern file))
                       (search (concatenate 'string "/" pattern) file)))
                  (t (eql 0 (search pattern file))))))
        patterns))

(defun apply-project-variables (buffer root)
  "Give BUFFER, a buffer of ROOT's project, the project's :VARIABLES."
  (loop for (name . value) in (getf (project-settings root) :variables)
        do (ignore-errors
            (let ((symbol (string-to-variable name)))
              (when (heml-bound-p symbol)
                (defhvar name (variable-documentation symbol)
                  :buffer buffer :value value))))))

(defcommand "Edit Project Settings" (p)
  "Visit this project's settings, the file .heml-project at its root, made
   with an example the first time."
  "Visit this project's settings file."
  (declare (ignore p))
  (let* ((root (current-project-root))
         (file (project-settings-file root))
         (new (not (probe-file file))))
    (find-file-command nil file)
    (when (and new (zerop (count-characters (buffer-region (current-buffer)))))
      (insert-string (current-point)
                     (format nil ";;; Settings for this project: a property list, read as data.~%~
                                  (:name ~S~%~
                                  ~1T:compile \"make -k \"~%~
                                  ~1T:ignore ()                 ; (\"build/\" \"*.o\")~%~
                                  ~1T:variables ()              ; ((\"Fill Column\" . 100))~%~
                                  ~1T:settings ())              ; language servers': ((\"yaml\" (\"schemas\" (\"file:///x.json\" . \"*.yaml\"))))~%"
                             (project-name root))))))


;;;; A project's files.

(defvar *project-file-limit* 50000
  "The most files a walk of a project's tree lists.")

(defvar *project-skipped-directories*
  '(".git" ".hg" ".svn" "_darcs" "node_modules" "build" "dist" ".cache")
  "Directories a walk of a project's tree does not enter.")

(defvar *project-file-walks* (make-hash-table :test 'equal)
  "Root to (TIME . FILES), a walk's result, kept a while.")

(defun run-for-lines (command directory)
  "The lines COMMAND, a shell command, prints in DIRECTORY, or NIL if it
   fails."
  (ignore-errors
   (uiop:run-program (list "/bin/sh" "-c"
                           (format nil "cd ~A && ~A" (shell-quote directory) command))
                     :output :lines :error-output nil)))

(defun walk-project-files (root)
  (let ((files '()) (count 0))
    (labels ((walk (directory)
               (dolist (file (uiop:directory-files directory))
                 (when (>= (incf count) *project-file-limit*)
                   (return-from walk-project-files (nreverse files)))
                 (push (enough-namestring file root) files))
               (dolist (sub (uiop:subdirectories directory))
                 (unless (member (car (last (pathname-directory sub)))
                                 *project-skipped-directories* :test #'string=)
                   (walk sub)))))
      (walk (pathname root)))
    (nreverse files)))

(defun project-files (root &key fresh)
  "The files in ROOT's project, relative to ROOT, less those its settings
   :IGNORE.  FRESH, for a walk of the tree, is a walk now, not one of the
   last half minute."
  (let ((files (all-project-files root :fresh fresh))
        (ignore (getf (project-settings root) :ignore)))
    (if ignore
        (remove-if (lambda (file) (ignored-file-p file ignore)) files)
        files)))

(defun all-project-files (root &key fresh)
  "The files in ROOT's project, relative to ROOT: those git knows of, or
   ripgrep finds, or a walk of the tree does."
  (or (and (probe-file (concatenate 'string root ".git"))
           (run-for-lines "git ls-files -co --exclude-standard" root))
      (and (find-program "rg")
           (run-for-lines "rg --files" root))
      (let ((walk (gethash root *project-file-walks*)))
        (if (and walk (not fresh) (< (- (get-universal-time) (car walk)) 30))
            (cdr walk)
            (cdr (setf (gethash root *project-file-walks*)
                       (cons (get-universal-time) (walk-project-files root))))))))


(defcommand "Forget Project Caches" (p)
  "Find each directory's project afresh, as after a .git is made or removed."
  "Find each directory's project afresh."
  (declare (ignore p))
  (clrhash *project-roots*)
  (clrhash *project-file-walks*)
  (message "Projects will be found again."))


;;;; The projects known, and where they and their sessions are kept.

(defvar *project-state-directory* nil
  "Where the projects known and their sessions are kept: NIL for
   HEML_STATE_DIRECTORY, or $XDG_STATE_HOME/heml/ without it.")

(defhvar "Project Sessions"
  "When true, a project's open files and windows are saved when the editor
   leaves it or exits, and reopened when it switches to it or starts in it."
  :value t)

(defun project-state-directory ()
  (uiop:ensure-directory-pathname
   (or *project-state-directory*
       (let ((variable (uiop:getenv "HEML_STATE_DIRECTORY")))
         (and (plusp (length variable)) variable))
       (let ((directory (hi::heml-state-directory)))
         (move-old-state directory)
         directory))))

(defun move-old-state (directory)
  "Move what Heml kept in ~/.heml/ before it followed XDG into DIRECTORY,
   the first time DIRECTORY is wanted and does not exist."
  (let* ((old (merge-pathnames ".heml/" (user-homedir-pathname)))
         (projects (merge-pathnames "projects.lisp" old)))
    (when (and (probe-file projects) (not (probe-file directory)))
      (ignore-errors
       (ensure-directories-exist directory)
       (rename-file projects (merge-pathnames "projects.lisp" directory))
       (let ((sessions (merge-pathnames "sessions/" old)))
         (when (probe-file sessions)
           (rename-file (uiop:ensure-directory-pathname sessions)
                        (merge-pathnames "sessions/" directory))))
       ;; ~/.heml/ goes too, if nothing else was in it.
       (uiop:delete-empty-directory old)))))

(defun state-file (name)
  (merge-pathnames name (project-state-directory)))

(defun read-state (name)
  (let ((file (state-file name)))
    (when (probe-file file)
      (ignore-errors
       (with-open-file (in file :external-format :utf-8)
         (with-standard-io-syntax
           (let ((*read-eval* nil)
                 (*package* (find-package :keyword)))
             (read in nil nil))))))))

(defun write-state (name form)
  "Write FORM to the state file NAME, through a new file renamed over it."
  (let* ((file (state-file name))
         (new (make-pathname :type "new" :defaults file)))
    (ensure-directories-exist new)
    (with-open-file (out new :direction :output :if-exists :supersede
                             :external-format :utf-8)
      (with-standard-io-syntax
        ;; Not readably: that writes a base string in #A notation.
        (let ((*package* (find-package :keyword))
              (*print-readably* nil))
          (prin1 form out)
          (terpri out))))
    (rename-file new file)))

(defvar *known-projects* :unread
  "((ROOT . PLIST) ...), most recently visited first; PLIST has :VISITED and
   :COMPILE.")

(defun known-projects ()
  (when (eq *known-projects* :unread)
    (setf *known-projects*
          (remove-if-not (lambda (entry) (and (consp entry) (stringp (car entry))))
                         (read-state "projects.lisp"))))
  *known-projects*)

(defun save-known-projects ()
  (ignore-errors (write-state "projects.lisp" (known-projects))))

(defun project-property (root key &optional default)
  (getf (cdr (assoc root (known-projects) :test #'string=)) key default))

(defun (setf project-property) (value root key &optional default)
  (declare (ignore default))
  (let ((entry (assoc root (known-projects) :test #'string=)))
    (unless entry
      (setf entry (list root))
      (push entry *known-projects*))
    (setf (getf (cdr entry) key) value)
    (save-known-projects)
    value))

(defun note-project (root)
  "Make ROOT the project most recently visited."
  (let ((entry (or (assoc root (known-projects) :test #'string=) (list root))))
    (setf (getf (cdr entry) :visited) (get-universal-time))
    (setf *known-projects* (cons entry (remove entry (known-projects))))
    (save-known-projects)))

(defun note-buffer-project (buffer pathname)
  (when pathname
    (let ((root (project-root (directory-namestring pathname))))
      (when root
        (apply-project-variables buffer root)
        (unless (equal root (car (first (known-projects))))
          (note-project root))))))

(add-hook buffer-pathname-hook 'note-buffer-project)


;;;; Commands.

(defun visit-project-file (root file)
  (find-file-command nil (merge-pathnames file root)))

(defun project-find-file (root)
  (let* ((files (or (project-files root) (editor-error "No files in ~A." root)))
         (table (make-string-table :separator #\/
                                   :initial-contents (mapcar (lambda (f) (cons f f)) files))))
    (multiple-value-bind (input exact)
        (prompt-for-keyword (list table)
                            :must-exist nil
                            :prompt (format nil "Find file in ~A: " (project-name root))
                            :help "A file of the project, or text in the names of several.")
      (if exact
          (visit-project-file root exact)
          (let ((matches (fuzzy-file-matches input files)))
            (cond ((null matches) (editor-error "No file in ~A matches ~A." (project-name root) input))
                  ((null (rest matches)) (visit-project-file root (first matches)))
                  (t (list-project-files root input matches))))))))

;;; Text that is no file's name finds the files it is in, its characters in
;;; order though not together, the best first: a file is better the more of
;;; the characters are together, start a word, and are in its name rather
;;; than its directories, and the shorter it is.
;;;
(defun fuzzy-file-score (input file)
  "How well INPUT matches FILE, or NIL if its characters are not in FILE in
   order."
  (let* ((name-start (let ((slash (position #\/ file :from-end t))) (if slash (1+ slash) 0)))
         (score 0)
         (at 0)
         (last -2))
    (flet ((match-from (start)
             (setf score 0 at start last -2)
             (loop for char across input
                   do (let ((hit (position char file :start at :test #'char-equal)))
                        (unless hit (return nil))
                        (incf score (cond ((= hit (1+ last)) 12)
                                          ((or (zerop hit)
                                               (find (char file (1- hit)) "/-_. "))
                                           8)
                                          (t 1)))
                        (when (>= hit name-start) (incf score 3))
                        (setf last hit at (1+ hit)))
                   finally (return t))))
      (when (or (match-from name-start) (match-from 0))
        (- (* 10 score) (length file))))))

(defun fuzzy-file-matches (input files)
  "The FILES that INPUT matches, the best first."
  (mapcar #'cdr
          (stable-sort (loop for file in files
                             for score = (fuzzy-file-score input file)
                             when score collect (cons score file))
                       #'> :key #'car)))

(defun list-project-files (root input files)
  (let ((buffer (make-result-buffer "*Project Files*" "Outline" 'plist-line-location)))
    (with-writable-buffer (buffer)
      (let ((point (buffer-point buffer)))
        (insert-string point (format nil "Files in ~A matching ~S, the best first: ~D~%~%"
                                     (project-name root) input (length files)))
        (dolist (file files)
          (let ((line (mark-line point)))
            (insert-string point (format nil "  ~A~%" file))
            (setf (getf (line-plist line) 'result-location)
                  (list (merge-pathnames file root) 1))))))
    (change-to-buffer buffer)
    (buffer-start (current-point))
    (next-result-line (current-point) 1)))

(defvar *project-history-length* 20
  "How many of a project's compile commands are kept.")

(defvar *project-shell-history-length* 200
  "How many of a project's shell's inputs are kept with its session.")

(defcommand "Project Find File" (p)
  "Visit a file of this project, completing its name from the project's
   files; text that is no file's name lists the files whose names hold it."
  "Visit a file of this project."
  (declare (ignore p))
  (project-find-file (current-project-root)))

(defcommand "Project Grep" (p)
  "Search the files of this project for a regular expression, from its root."
  "Search this project's files."
  (declare (ignore p))
  (let* ((root (current-project-root))
         (pattern (prompt-for-string :prompt (format nil "Search ~A for: " (project-name root))
                                     :default (let ((word (word-at-point)))
                                                (and (plusp (length word)) word)))))
    (grep-command nil (recursive-grep-command-line pattern) root)))

(defcommand "Project Compile" (p)
  "Compile this project, from its root.  Its last command is remembered."
  "Compile this project, from its root."
  (declare (ignore p))
  (let* ((root (current-project-root))
         (history (project-property root :compile-history))
         (command
           ;; M-p at the prompt goes back through this project's commands.
           (let ((*echo-area-history* (make-ring (max 10 *project-history-length*))))
             (dolist (old (reverse history))
               (ring-push old *echo-area-history*))
             (prompt-for-string :prompt (format nil "Compile ~A: " (project-name root))
                                :default (project-property
                                          root :compile
                                          (or (getf (project-settings root) :compile)
                                              "make -k "))))))
    (setf (project-property root :compile) command
          (project-property root :compile-history)
          (let ((all (cons command (remove command history :test #'string=))))
            (subseq all 0 (min (length all) *project-history-length*))))
    (compile-command nil command root)))

(defcommand "Project Shell Command" (p)
  "Run a shell command in this project's root, its output in a buffer."
  "Run a shell command in this project's root."
  (declare (ignore p))
  (let ((root (current-project-root)))
    (shell-command-command nil (prompt-for-string
                                :prompt (format nil "Shell command in ~A: " (project-name root)))
                           root)))

(defvar *project-shells* (make-hash-table :test 'equal)
  "Root to its project's shell buffer.")

(defun project-shell (root)
  "Go to ROOT's project's shell, started in ROOT the first time."
  (let ((shell (gethash root *project-shells*)))
    (if (and shell (member shell *buffer-list*))
        (change-to-buffer shell)
        (progn
          (make-new-shell nil t (format nil "cd ~A && exec ~A" (shell-quote root) (get-command-line)))
          (let ((buffer (current-buffer)))
            (when (heml-bound-p 'current-working-directory :buffer buffer)
              (setf (variable-value 'current-working-directory :buffer buffer) root))
            ;; What was typed to this project's shell before, for M-p.
            (when (heml-bound-p 'interactive-history :buffer buffer)
              (let ((ring (variable-value 'interactive-history :buffer buffer)))
                (dolist (input (reverse (getf (read-state (session-file-name root)) :shell-history)))
                  (ring-push (string-to-region input) ring))))
            (setf (gethash root *project-shells*) buffer))))))

(defun project-shell-history (root)
  "What has been typed to ROOT's project's shell, the latest first: this
   session's, or, when it has no shell now, what was saved before."
  (let ((shell (gethash root *project-shells*)))
    (if (and shell (member shell *buffer-list*)
             (heml-bound-p 'interactive-history :buffer shell))
        (let ((ring (variable-value 'interactive-history :buffer shell)))
          (loop for i below (min (ring-length ring) *project-shell-history-length*)
                collect (region-to-string (ring-ref ring i))))
        (getf (read-state (session-file-name root)) :shell-history))))

(defcommand "Project Shell" (p)
  "Go to this project's shell, started in its root the first time."
  "Go to this project's shell."
  (declare (ignore p))
  (project-shell (current-project-root)))

(defun dired-buffer-directory (buffer)
  "The directory BUFFER edits in Dired, or NIL."
  (and (heml-bound-p 'dired-information :buffer buffer)
       (namestring (dired-info-pathname (variable-value 'dired-information :buffer buffer)))))

(defcommand "Project Dired" (p)
  "Edit this project's root directory in Dired."
  "Edit this project's root in Dired."
  (declare (ignore p))
  (dired-command nil (current-project-root)))

(defcommand "Project Switch Buffer" (p)
  "Go to another of this project's buffers."
  "Go to another of this project's buffers."
  (declare (ignore p))
  (let* ((root (current-project-root))
         (buffers (or (remove (current-buffer) (project-buffers root))
                      (editor-error "No other buffer in ~A." (project-name root))))
         (table (make-string-table
                 :initial-contents (mapcar (lambda (b) (cons (buffer-name b) b)) buffers))))
    (multiple-value-bind (name buffer)
        (prompt-for-keyword (list table) :must-exist t
                            :default (buffer-name (first buffers))
                            :prompt (format nil "Buffer in ~A: " (project-name root)))
      (declare (ignore name))
      (change-to-buffer buffer))))

(defcommand "List Project Buffers" (p)
  "List this project's buffers in Bufed."
  "List this project's buffers."
  (declare (ignore p))
  (setf *bufed-filter* (current-project-root))
  (bufed-command nil))

(defcommand "Kill Project Buffers" (p)
  "Kill the buffers visiting this project's files, and its shell, asking
   first, and asking whether to save each modified one."
  "Kill this project's buffers."
  (declare (ignore p))
  (let* ((root (current-project-root))
         (buffers (project-buffers root))
         (shell (gethash root *project-shells*)))
    (when (and shell (member shell *buffer-list*))
      (push shell buffers))
    (dolist (buffer *buffer-list*)
      (when (under-root-p (dired-buffer-directory buffer) root)
        (push buffer buffers)))
    (unless buffers (editor-error "No buffers in ~A." (project-name root)))
    (when (prompt-for-y-or-n :prompt (format nil "Kill ~D buffer~:P of ~A? "
                                             (length buffers) (project-name root))
                             :default nil :must-exist t)
      (dolist (buffer buffers)
        (when (and (buffer-modified buffer) (buffer-pathname buffer)
                   (prompt-for-y-or-n :prompt (format nil "Save ~A first? " (buffer-name buffer))
                                      :default t :must-exist t))
          (save-file-command nil buffer))
        (delete-buffer-if-possible buffer))
      (message "Killed ~D buffer~:P." (length buffers)))))

(defcommand "Forget Project" (p)
  "Forget this project: take it off the list Switch Project offers, and
   delete its saved session."
  "Forget this project."
  (declare (ignore p))
  (let ((root (current-project-root)))
    (setf *known-projects* (remove root (known-projects) :key #'car :test #'string=))
    (save-known-projects)
    (let ((file (state-file (session-file-name root))))
      (when (probe-file file) (delete-file file)))
    (message "Forgot ~A." (project-name root))))

(defun abbreviate-root (root)
  (let ((home (namestring (user-homedir-pathname))))
    (if (eql 0 (search home root))
        (concatenate 'string "~/" (subseq root (length home)))
        root)))

(defcommand "Switch Project" (p)
  "Go to another project: its session, the files and windows it had open,
   is reopened; a project without one offers its files."
  "Go to another project."
  (declare (ignore p))
  (let* ((here (or (buffer-project-root (current-buffer)) (project-root (default-directory))))
         (projects (or (remove-if-not (lambda (root) (probe-file root))
                                      (mapcar #'car (known-projects)))
                       (editor-error "No projects known yet: visit a file in one.")))
         (table (make-string-table
                 :initial-contents (mapcar (lambda (root)
                                             (cons (format nil "~A ~A" (project-name root)
                                                           (abbreviate-root root))
                                                   root))
                                           projects)))
         (other (find here projects :test-not #'equal)))
    (multiple-value-bind (name root)
        (prompt-for-keyword (list table) :must-exist t
                            :default (and other (format nil "~A ~A" (project-name other)
                                                        (abbreviate-root other)))
                            :prompt "Switch to project: ")
      (declare (ignore name))
      (switch-to-project root here))))

(defun switch-to-project (root &optional here)
  (when (and here (not (equal here root)) (value project-sessions))
    (save-project-session here))
  (note-project root)
  (unless (and (value project-sessions) (restore-project-session root))
    (project-find-file root)))


;;;; Replacing through a project.

(defun replace-in-line (line old new)
  "Replace each OLD in LINE's text with NEW.  How many there were."
  (let ((string (line-string line)) (count 0) (at 0) (parts '()))
    (loop
      (let ((hit (search old string :start2 at)))
        (unless hit (return))
        (push (subseq string at hit) parts)
        (push new parts)
        (incf count)
        (setf at (+ hit (length old)))))
    (when (plusp count)
      (push (subseq string at) parts)
      (with-mark ((start (mark line 0) :left-inserting)
                  (end (mark line (length string))))
        (delete-region (region start end))
        (insert-string start (apply #'concatenate 'string (nreverse parts)))))
    count))

(defcommand "Project Replace" (p)
  "Replace a string with another in every file of this project that has
   it, exactly as written.  The files are found, how many places there are
   is said and asked about, the changes are made in the files' buffers, and
   the changed lines are listed; then the buffers are saved, if wanted.
   Each buffer's changes can be undone there."
  "Replace a string through this project."
  (declare (ignore p))
  (let* ((root (current-project-root))
         (old (prompt-for-string :prompt (format nil "Replace in ~A: " (project-name root))
                                 :default (let ((word (word-at-point)))
                                            (and (plusp (length word)) word))))
         (new (prompt-for-string :prompt (format nil "Replace ~A with: " old)
                                 :default "" :trim nil))
         (ignore (getf (project-settings root) :ignore))
         (files (remove-if (lambda (file) (ignored-file-p file ignore))
                           (mapcar (lambda (file) (string-left-trim "./" file))
                                   (run-for-lines
                                    (if (find-program "rg")
                                        (format nil "rg -l -F -e ~A ." (shell-quote old))
                                        (format nil "grep -rlI -F --exclude-dir=.git --exclude-dir=.hg -e ~A ."
                                                (shell-quote old)))
                                    root)))))
    (when (or (zerop (length old)) (find #\Newline old))
      (editor-error "What is replaced must be some text on one line."))
    (unless files (editor-error "No file in ~A has ~A." (project-name root) old))
    (let* ((buffers (mapcar (lambda (file) (find-file-buffer (merge-pathnames file root))) files))
           (places (loop for buffer in buffers
                         sum (let ((count 0))
                               (do ((line (mark-line (buffer-start-mark buffer)) (line-next line)))
                                   ((null line) count)
                                 (let ((at 0) (string (line-string line)))
                                   (loop for hit = (search old string :start2 at)
                                         while hit do (incf count) (setf at (+ hit (length old))))))))))
      (when (zerop places) (editor-error "No file in ~A has ~A." (project-name root) old))
      (unless (prompt-for-y-or-n
               :prompt (format nil "Replace ~D place~:P in ~D file~:P? " places (length buffers))
               :default nil :must-exist t)
        (editor-error "Nothing replaced."))
      (let ((changed '()))
        (dolist (buffer buffers)
          (let ((number 0))
            (do ((line (mark-line (buffer-start-mark buffer)) (line-next line)))
                ((null line))
              (incf number)
              (when (plusp (replace-in-line line old new))
                (push (list (buffer-pathname buffer) number (line-string line)) changed)))))
        (setf changed (nreverse changed))
        (when (prompt-for-y-or-n
               :prompt (format nil "Save the ~D buffer~:P changed? " (length buffers))
               :default t :must-exist t)
          (dolist (buffer buffers)
            (when (buffer-modified buffer) (save-file-command nil buffer))))
        ;; The lines changed, to look over and go to.
        (let ((buffer (make-result-buffer "*Project Replace*" "Outline" 'plist-line-location)))
          (with-writable-buffer (buffer)
            (let ((point (buffer-point buffer)))
              (insert-string point (format nil "~A replaced with ~A in ~A: ~D line~:P~%~%"
                                           old new (project-name root) (length changed)))
              (loop for (pathname number text) in changed
                    do (let ((line (mark-line point)))
                         (insert-string point (format nil "  ~A:~D: ~A~%"
                                                      (enough-namestring pathname root) number
                                                      (string-trim '(#\Space #\Tab) text)))
                         (setf (getf (line-plist line) 'result-location)
                               (list pathname number))))))
          (change-to-buffer buffer)
          (buffer-start (current-point))
          (next-result-line (current-point) 1))
        (message "Replaced ~D place~:P in ~D file~:P." places (length buffers))))))


;;;; Recent files.

(defvar *recent-files* :unread
  "The files visited, the latest first, as namestrings.")

(defvar *recent-files-kept* 200)

(defun recent-files ()
  (when (eq *recent-files* :unread)
    (setf *recent-files* (remove-if-not #'stringp (read-state "recent-files.lisp"))))
  *recent-files*)

(defun note-recent-file (buffer pathname)
  (declare (ignore buffer))
  (when pathname
    (let ((name (namestring pathname)))
      (unless (equal name (first (recent-files)))
        (let ((all (cons name (remove name (recent-files) :test #'string=))))
          (setf *recent-files* (subseq all 0 (min (length all) *recent-files-kept*))))
        (ignore-errors (write-state "recent-files.lisp" *recent-files*))))))

(add-hook buffer-pathname-hook 'note-recent-file)

(defcommand "Find Recent File" (p)
  "Visit one of the files visited lately, in this session or before it,
   completing its name; text that is no file's name finds the files with its
   characters in order, the best first."
  "Visit one of the files visited lately."
  (declare (ignore p))
  (let* ((here (let ((pathname (buffer-pathname (current-buffer))))
                 (and pathname (namestring pathname))))
         (files (or (remove-if-not #'probe-file (remove here (recent-files) :test #'equal))
                    (editor-error "No files have been visited.")))
         (names (mapcar (lambda (file) (cons (abbreviate-root file) file)) files))
         (table (make-string-table :separator #\/ :initial-contents names)))
    (multiple-value-bind (input file)
        (prompt-for-keyword (list table) :must-exist nil
                                         :default (car (first names))
                                         :prompt "Recent file: "
                                         :help "A file visited lately, or text in the names of several.")
      (if file
          (find-file-command nil file)
          (let ((matches (fuzzy-file-matches input (mapcar #'car names))))
            (cond ((null matches) (editor-error "No recent file matches ~A." input))
                  ((null (rest matches))
                   (find-file-command nil (cdr (assoc (first matches) names :test #'string=))))
                  (t
                   (let ((buffer (make-result-buffer "*Recent Files*" "Outline"
                                                     'plist-line-location)))
                     (with-writable-buffer (buffer)
                       (let ((point (buffer-point buffer)))
                         (insert-string point (format nil "Recent files matching ~S, the best first: ~D~%~%"
                                                      input (length matches)))
                         (dolist (name matches)
                           (let ((line (mark-line point)))
                             (insert-string point (format nil "  ~A~%" name))
                             (setf (getf (line-plist line) 'result-location)
                                   (list (pathname (cdr (assoc name names :test #'string=))) 1))))))
                     (change-to-buffer buffer)
                     (buffer-start (current-point))
                     (next-result-line (current-point) 1)))))))))


;;;; Sessions.

;;; A session is (:FILES ((FILE LINE COLUMN FOLDS) ...) :CURRENT FILE
;;; :LAYOUT NODE), FILE relative to the root, LINE and START counting from 1,
;;; FOLDS the file's folds as BUFFER-FOLDS gives them (fold.lisp);
;;; a NODE is (:WINDOW FILE LINE COLUMN START), (:DIRED DIRECTORY) for a
;;; window on a directory of the project, (:SHELL) for one on its shell, or
;;; (:SPLIT DIRECTION SIZES NODE ...), as the device's layout tree is
;;; (layout.lisp).

(defun session-file-name (root)
  (format nil "sessions/~A.lisp"
          (substitute-if #\_ (lambda (c) (not (or (alphanumericp c) (find c "-."))))
                         (string-trim "/" root))))

(defun mark-line-number (mark)
  (count-lines (region (buffer-start-mark (line-buffer (mark-line mark))) mark)))

(defun window-session-point (window)
  "WINDOW's point: the buffer's own in the current window."
  (if (eq window (current-window))
      (buffer-point (window-buffer window))
      (window-point window)))

(defun session-window (window root)
  (let ((buffer (window-buffer window)))
    (cond ((under-root-p (buffer-pathname buffer) root)
           (let ((point (window-session-point window)))
             (list :window (enough-namestring (buffer-pathname buffer) root)
                   (mark-line-number point) (mark-charpos point)
                   (mark-line-number (window-display-start window)))))
          ((under-root-p (dired-buffer-directory buffer) root)
           (list :dired (enough-namestring (dired-buffer-directory buffer) root)))
          ((eq buffer (gethash root *project-shells*))
           (list :shell)))))

(defun session-layout (node root)
  (if (hi::layout-split-p node)
      (let ((children '()) (sizes '()))
        (loop for child in (hi::layout-split-children node)
              for size in (hi::layout-split-sizes node)
              for saved = (session-layout child root)
              when saved do (push saved children) (push size sizes))
        (cond ((null children) nil)
              ((null (rest children)) (first children))
              (t (list* :split (hi::layout-split-direction node) (nreverse sizes)
                        (nreverse children)))))
      (session-window (device-hunk-window node) root)))

(defun current-layout-root ()
  (hi::layout-root (hi::device-layout (device-hunk-device (window-hunk (current-window))))))

(defun save-project-session (root &key (layout t))
  "Save what ROOT's project has open, and the windows if LAYOUT."
  (let ((buffers (project-buffers root)))
    (ignore-errors
     (write-state
      (session-file-name root)
      (list :files (loop for buffer in buffers
                         for point = (buffer-point buffer)
                         collect (list (enough-namestring (buffer-pathname buffer) root)
                                       (mark-line-number point) (mark-charpos point)
                                       (buffer-folds buffer)))
            :current (let ((pathname (buffer-pathname (current-buffer))))
                       (and (under-root-p pathname root) (enough-namestring pathname root)))
            :layout (and layout (session-layout (current-layout-root) root))
            :shell-history (project-shell-history root))))))

(defun move-to-line (mark line column)
  (buffer-start mark)
  (unless (line-offset mark (max 0 (1- line)))
    (buffer-end mark))
  (character-offset mark (min column (line-length (mark-line mark)))))

(defun session-buffer (root file)
  (let ((pathname (merge-pathnames file root)))
    (when (probe-file pathname)
      (find-file-buffer pathname))))

(defun restore-session-window (window node root)
  (select-window window)
  (ecase (first node)
    (:window
     (destructuring-bind (file line column start) (rest node)
       (let ((buffer (session-buffer root file)))
         (when buffer
           (change-to-buffer buffer)
           (move-to-line (current-point) line column)
           (move-to-line (window-display-start window) start 0)))))
    (:dired
     (let ((directory (merge-pathnames (second node) root)))
       (when (probe-file directory)
         (dired-command nil (namestring directory)))))
    (:shell
     (project-shell root))))

(defun restore-session-layout (window node root)
  (if (member (first node) '(:window :dired :shell))
      (restore-session-window window node root)
      (destructuring-bind (direction sizes &rest children) (rest node)
        (let ((windows (list window))
              (remaining (reduce #'+ sizes)))
          ;; Each split leaves the window its first share, and gives the new
          ;; one the rest, which the next split divides again.
          (loop for size in (butlast sizes)
                for current = (first windows)
                do (select-window current)
                   (let ((new (and (plusp remaining)
                                   (make-window (window-display-start current)
                                                :direction direction
                                                :proportion (/ (- remaining size) remaining)))))
                     (unless new (return))
                     (decf remaining size)
                     (push new windows)))
          (loop for child in children
                for child-window in (reverse windows)
                do (restore-session-layout child-window child root))))))

(defun restore-project-session (root)
  "Reopen what ROOT's project had open when its session was saved.  True
   when there was a session."
  (let ((session (read-state (session-file-name root))))
    (when (getf session :files)
      (loop for (file line column folds) in (getf session :files)
            for buffer = (session-buffer root file)
            when buffer
              do (move-to-line (buffer-point buffer) line column)
                 ;; Its folds as they were, unless it is folded already.
                 (when (and folds (not (buffer-folds buffer)))
                   (restore-buffer-folds buffer folds)))
      (let ((layout (getf session :layout))
            (current (getf session :current)))
        (when layout
          (delete-other-windows-command nil)
          (restore-session-layout (current-window) layout root))
        (when current
          (let* ((buffer (session-buffer root current))
                 (window (and buffer (find buffer *window-list* :key #'window-buffer))))
            (cond (window (select-window window))
                  (buffer (change-to-buffer buffer))))))
      (message "Reopened ~A." (project-name root))
      t)))

(defcommand "Save Project Session" (p)
  "Save the files and windows this project has open, to reopen later."
  "Save this project's session."
  (declare (ignore p))
  (let ((root (current-project-root)))
    (save-project-session root)
    (message "Saved ~A's session." (project-name root))))

(defcommand "Restore Project Session" (p)
  "Reopen the files and windows this project had open when its session was
   last saved."
  "Reopen this project's session."
  (declare (ignore p))
  (let ((root (current-project-root)))
    (unless (restore-project-session root)
      (editor-error "No session saved for ~A." (project-name root)))))

;;; On exit, each project with files open is saved, and the windows with the
;;; current buffer's.  On entry, when nothing was named to edit, the project
;;; the editor started in is reopened.

(defun save-sessions-on-exit ()
  (when (value project-sessions)
    (let ((current (buffer-project-root (current-buffer)))
          (roots (remove-duplicates (remove nil (mapcar #'buffer-project-root *buffer-list*))
                                    :test #'string=)))
      (dolist (root roots)
        (save-project-session root :layout (equal root current))))))

(defvar *startup-session-restored* nil)

(defun restore-session-on-entry ()
  (unless *startup-session-restored*
    (setf *startup-session-restored* t)
    (when (and (value project-sessions)
               (notany #'buffer-pathname *buffer-list*))
      (let ((root (ignore-errors (project-root (default-directory)))))
        (when root
          (ignore-errors (restore-project-session root)))))))

(add-hook exit-hook 'save-sessions-on-exit)
(add-hook entry-hook 'restore-session-on-entry)


;;;; The modeline.

(make-modeline-field
 :name :project
 :function (lambda (buffer window)
             (declare (ignore window))
             (let ((root (buffer-project-root buffer)))
               (if root (format nil "[~A]  " (project-name root)) ""))))

;;; After the modes, where a narrow window still shows it.
(unless (member :project hi::*default-modeline-fields* :key #'modeline-field-name)
  (let ((modes (member :modes hi::*default-modeline-fields* :key #'modeline-field-name)))
    (if modes
        (push (modeline-field :project) (cdr modes))
        (nconc hi::*default-modeline-fields* (list (modeline-field :project))))))
