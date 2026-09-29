;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;; Simple directory editing support.
;;; This file contains site dependent calls.
;;;
;;; Written by Blaine Burks and Bill Chiles.
;;;

(in-package :heml)


(defmode "Dired" :major-p t
  :documentation
  "Dired permits convenient directory browsing and file operations including
   viewing, deleting, copying, renaming, and wildcard specifications.")


(defstruct (dired-information (:print-function print-dired-information)
                              (:conc-name dired-info-))
  pathname              ; Pathname of directory.
  pattern               ; FILE-NAMESTRING with wildcard possibly.
  dot-files-p           ; Whether to include UNIX dot files.
  (sort :name)          ; :NAME, :DATE or :SIZE.
  write-date            ; Write date of directory.
  files                 ; Simple-vector of dired-file structures.
  file-list)            ; List of pathnames for files, excluding directories.

(defun print-dired-information (obj str n)
  (declare (ignore n))
  (format str "#<Dired Info ~S>" (namestring (dired-info-pathname obj))))


(defstruct (dired-file (:print-function print-dired-file)
                       (:constructor make-dired-file (pathname)))
  pathname
  (deleted-p nil)                       ; flagged for deletion, shown as D
  (marked-p nil)                        ; marked for an operation, shown as *
  (write-date nil))

;;; A Dired buffer is a header line and then a line for each file, in the
;;; order of the files vector.
;;;
(defconstant +dired-header-lines+ 1)

(defun print-dired-file (obj str n)
  (declare (ignore n))
  (format str "#<Dired-file ~A>" (namestring (dired-file-pathname obj))))



;;;; "Dired" command.

;;; *pathnames-to-dired-buffers* is an a-list mapping directory namestrings to
;;; buffers that display their contents.
;;;
(defvar *pathnames-to-dired-buffers* ())

(make-modeline-field
 :name :dired-cmds :width 20
 :function
 #'(lambda (buffer window)
     (declare (ignore buffer window))
     "  Type ? for help.  "))

(defcommand "Dired" (p &optional directory)
  "Prompts for a directory and edits it.  If a dired for that directory already
   exists, go to that buffer, otherwise create one.  With an argument, include
   UNIX dot files."
  "Prompts for a directory and edits it.  If a dired for that directory already
   exists, go to that buffer, otherwise create one.  With an argument, include
   UNIX dot files."
  (let ((info (if (heml-bound-p 'dired-information)
                  (value dired-information))))
    (dired-guts nil
                ;; Propagate dot-files property to subdirectory edits.
                (or (and info (dired-info-dot-files-p info))
                    p)
                directory)))

(defcommand "Dired with Pattern" (p)
  "Do a dired, prompting for a pattern which may include a single *.  With an
   argument, include UNIX dit files."
  "Do a dired, prompting for a pattern which may include a single *.  With an
   argument, include UNIX dit files."
  (dired-guts t p nil))

(defun dired-guts (patternp dot-files-p directory)
  (start-dired-watch)
  (let* ((dpn (value pathname-defaults))
         (directory (or directory
                        (prompt-for-file
                         :prompt "Edit Directory: "
                         :help "Pathname to edit."
                         :default (make-pathname
                                   :device (pathname-device dpn)
                                   :directory (pathname-directory dpn))
                         :must-exist nil)))
         (pattern (if patternp
                      (prompt-for-string
                       :prompt "Filename pattern: "
                       :help "Type a filename with a single asterisk."
                       :trim t)))
         ;; On ECL a namestring is not a simple string.
         (full-name (coerce (namestring (if pattern
                                            (merge-pathnames directory pattern)
                                            directory))
                            'simple-string))
         (name (concatenate 'simple-string "Dired " full-name))
         (buffer (cdr (assoc full-name *pathnames-to-dired-buffers*
                             :test #'string=))))
    (declare (simple-string full-name))
    (setf (value pathname-defaults) (merge-pathnames directory dpn))
    (change-to-buffer
     (cond (buffer
            (when (and dot-files-p
                       (not (dired-info-dot-files-p
                             (variable-value 'dired-information
                                             :buffer buffer))))
              (setf (dired-info-dot-files-p (variable-value 'dired-information
                                                            :buffer buffer))
                    t)
              (update-dired-buffer directory pattern buffer))
            buffer)
           (t
            (let ((buffer (make-buffer
                           name :modes '("Dired")
                           :modeline-fields
                           (append (value default-modeline-fields)
                                   (list (modeline-field :dired-cmds)))
                           :delete-hook (list 'dired-buffer-delete-hook))))
              (unless (initialize-dired-buffer directory pattern
                                               dot-files-p buffer)
                (delete-buffer-if-possible buffer)
                (editor-error "No entries for ~A." full-name))
              (push (cons full-name buffer) *pathnames-to-dired-buffers*)
              buffer))))))

;;; INITIALIZE-DIRED-BUFFER gets a dired in the buffer and defines some
;;; variables to make it usable as a dired buffer.  If there are no file
;;; satisfying directory, then this returns nil, otherwise t.
;;;
(defun initialize-dired-buffer (directory pattern dot-files-p buffer)
  (multiple-value-bind (pathnames dired-files)
                       (dired-in-buffer directory pattern dot-files-p buffer)
    (if (zerop (length dired-files))
        nil
        (defhvar "Dired Information"
          "Contains the information neccessary to manipulate dired buffers."
          :buffer buffer
          :value (make-dired-information :pathname directory
                                         :pattern pattern
                                         :dot-files-p dot-files-p
                                         :write-date (dired-directory-signature directory)
                                         :files dired-files
                                         :file-list pathnames)))))

;;; CALL-PRINT-DIRECTORY gives us a nice way to report PRINT-DIRECTORY errors
;;; to the user and to clean up the dired buffer.
;;;
(defun call-print-directory (directory mark dot-files-p)
  (handler-case (with-output-to-mark (s mark :full)
                  (print-directory directory s
                                   :all dot-files-p
                                   :verbose t
                                   :return-list t))
    (error (condx)
      (delete-buffer-if-possible (line-buffer (mark-line mark)))
      (editor-error "~A" condx))))

;;; DIRED-BUFFER-DELETE-HOOK is called on dired buffers upon deletion.  This
;;; removes the buffer from the pathnames mapping, and it deletes and buffer
;;; local variables referring to it.
;;;
(defun dired-buffer-delete-hook (buffer)
  (setf *pathnames-to-dired-buffers*
        (delete buffer *pathnames-to-dired-buffers* :test #'eq :key #'cdr)))



;;;; Dired deletion and undeletion.

(defcommand "Dired Delete File" (p)
  "Marks a file for deletion; signals an error if not in a dired buffer.
   With an argument, this prompts for a pattern that may contain at most one
   wildcard, an asterisk, and all names matching the pattern will be flagged
   for deletion."
  "Marks a file for deletion; signals an error if not in a dired buffer."
  (dired-frob-deletion p t))

(defcommand "Dired Undelete File" (p)
  "Removes a mark for deletion; signals and error if not in a dired buffer.
   With an argument, this prompts for a pattern that may contain at most one
   wildcard, an asterisk, and all names matching the pattern will be unflagged
   for deletion."
  "Removes a mark for deletion; signals and error if not in a dired buffer."
  (dired-frob-deletion p nil))

(defcommand "Dired Delete File and Down Line" (p)
  "Marks file for deletion and moves down a line.
   See \"Dired Delete File\"."
  "Marks file for deletion and moves down a line.
   See \"Dired Delete File\"."
  (declare (ignore p))
  (dired-frob-deletion nil t)
  (dired-down-line (current-point)))

(defcommand "Dired Undelete File and Down Line" (p)
  "Marks file undeleted and moves down a line.
   See \"Dired Delete File\"."
  "Marks file undeleted and moves down a line.
   See \"Dired Delete File\"."
  (declare (ignore p))
  (dired-frob-deletion nil nil)
  (dired-down-line (current-point)))

(defcommand "Dired Delete File with Pattern" (p)
  "Prompts for a pattern and marks matching files for deletion.
   See \"Dired Delete File\"."
  "Prompts for a pattern and marks matching files for deletion.
   See \"Dired Delete File\"."
  (declare (ignore p))
  (dired-frob-deletion t t)
  (dired-down-line (current-point)))

(defcommand "Dired Undelete File with Pattern" (p)
  "Prompts for a pattern and marks matching files undeleted.
   See \"Dired Delete File\"."
  "Prompts for a pattern and marks matching files undeleted.
   See \"Dired Delete File\"."
  (declare (ignore p))
  (dired-frob-deletion t nil)
  (dired-down-line (current-point)))

;;; DIRED-FROB-DELETION takes arguments indicating whether to prompt for a
;;; pattern and whether to mark the file deleted or undeleted.  This uses
;;; CURRENT-POINT and CURRENT-BUFFER, and if not in a dired buffer, signal
;;; an error.
;;;
(defun dired-frob-deletion (patternp deletep)
  (unless (heml-bound-p 'dired-information)
    (editor-error "Not in Dired buffer."))
  (with-mark ((mark (current-point) :left-inserting))
    (let* ((dir-info (value dired-information))
           (files (dired-info-files dir-info))
           (del-files
            (if patternp
                (dired:pathnames-from-pattern
                 (prompt-for-string
                  :prompt "Filename pattern: "
                  :help "Type a filename with a single asterisk."
                  :trim t)
                 (dired-info-file-list dir-info))
                (list (dired-file-pathname
                       (dired-file-at mark files)))))
           (note-char (if deletep #\D #\space)))
      (with-writable-buffer ((current-buffer))
        (dolist (f del-files)
          (let* ((pos (position f files :test #'equal
                                :key #'dired-file-pathname))
                 (dired-file (svref files pos)))
            (dired-file-line mark pos)
            (setf (dired-file-deleted-p dired-file) deletep)
            (if deletep
                (setf (dired-file-write-date dired-file)
                      (file-write-date (dired-file-pathname dired-file)))
                (setf (dired-file-write-date dired-file) nil))
            (setf (next-character mark) note-char)))))))

(defun dired-down-line (point)
  (line-offset point 1)
  (when (blank-line-p (mark-line point))
    (line-offset point -1)))



;;;; Dired file finding and going to dired buffers.

(defcommand "Dired Edit File" (p)
  "Read in file or recursively \"Dired\" a directory."
  "Read in file or recursively \"Dired\" a directory."
  (declare (ignore p))
  (let ((point (current-point)))
    (when (blank-line-p (mark-line point)) (editor-error "Not on a file line."))
    (let ((pathname (dired-file-pathname
                     (dired-file-at
                      point (dired-info-files (value dired-information))))))
      (if (directoryp pathname)
          (dired-command nil (directory-namestring pathname))
          (change-to-buffer (find-file-buffer pathname))))))

(defcommand "Dired View File" (p)
  "Read in file as if by \"View File\" or recursively \"Dired\" a directory.
   This associates the file's buffer with the dired buffer."
  "Read in file as if by \"View File\".
   This associates the file's buffer with the dired buffer."
  (declare (ignore p))
  (let ((point (current-point)))
    (when (blank-line-p (mark-line point)) (editor-error "Not on a file line."))
    (let ((pathname (dired-file-pathname
                     (dired-file-at
                      point (dired-info-files (value dired-information))))))
      (if (directoryp pathname)
          (dired-command nil (directory-namestring pathname))
          (let* ((dired-buf (current-buffer))
                 (buffer (view-file-command nil pathname)))
            (push #'(lambda (buffer)
                      (declare (ignore buffer))
                      (setf dired-buf nil))
                  (buffer-delete-hook dired-buf))
            (setf (variable-value 'view-return-function :buffer buffer)
                  #'(lambda ()
                      (if dired-buf
                          (change-to-buffer dired-buf)
                          (dired-from-buffer-pathname-command nil)))))))))

(defcommand "Dired from Buffer Pathname" (p)
  "Invokes \"Dired\" on the directory part of the current buffer's pathname.
   With an argument, also prompt for a file pattern within that directory."
  "Invokes \"Dired\" on the directory part of the current buffer's pathname.
   With an argument, also prompt for a file pattern within that directory."
  (let ((pathname (buffer-pathname (current-buffer))))
    (if pathname
        (dired-command p (directory-namestring pathname))
        (editor-error "No pathname associated with buffer."))))

(defcommand "Dired Up Directory" (p)
  "Invokes \"Dired\" on the directory up one level from the current Dired
   buffer."
  "Invokes \"Dired\" on the directory up one level from the current Dired
   buffer."
  (declare (ignore p))
  (unless (heml-bound-p 'dired-information)
    (editor-error "Not in Dired buffer."))
  (let ((dirs (or (pathname-directory
                   (dired-info-pathname (value dired-information)))
                  '(:relative))))
    (dired-command nil
                   (truename (make-pathname :directory (nconc dirs '(:up)))))))



;;;; Dired misc. commands -- update, help, line motion.

(defcommand "Dired Toggle Hidden Files" (p)
  "Show the files whose names start with a dot, or hide them again."
  "Show or hide dot files."
  (declare (ignore p))
  (unless (heml-bound-p 'dired-information)
    (editor-error "Not in Dired buffer."))
  (let ((info (value dired-information)))
    (setf (dired-info-dot-files-p info) (not (dired-info-dot-files-p info)))
    (update-dired-buffer (dired-info-pathname info) (dired-info-pattern info)
                         (current-buffer))
    (message "~:[Hiding~;Showing~] hidden files." (dired-info-dot-files-p info))))

(defcommand "Dired Update Buffer" (p)
  "Recompute the contents of a dired buffer.
   This maintains delete flags for files that have not been modified."
  "Recompute the contents of a dired buffer.
   This maintains delete flags for files that have not been modified."
  (declare (ignore p))
  (unless (heml-bound-p 'dired-information)
    (editor-error "Not in Dired buffer."))
  (let ((buffer (current-buffer))
        (dir-info (value dired-information)))
    (update-dired-buffer (dired-info-pathname dir-info)
                         (dired-info-pattern dir-info)
                         buffer)))

;;; UPDATE-DIRED-BUFFER updates buffer with a dired of directory, deleting
;;; whatever is in the buffer already.  This assumes buffer was previously
;;; used as a dired buffer having necessary variables bound.  The new files
;;; are compared to the old ones propagating any deleted flags if the name
;;; and the write date is the same for both specifications.
;;;
(defun update-dired-buffer (directory pattern buffer)
  (let* ((dir-info (variable-value 'dired-information :buffer buffer))
         (point (buffer-point buffer))
         (old-index (max 0 (- (count-lines (region (buffer-start-mark buffer) point))
                              1 +dired-header-lines+)))
         (old-files (dired-info-files dir-info))
         (old-pathname (and (< old-index (length old-files))
                            (dired-file-pathname (svref old-files old-index)))))
    (%update-dired-buffer directory pattern buffer)
    ;; Point stays on its file, or where it was if that has gone.
    (let* ((files (dired-info-files dir-info))
           (index (or (and old-pathname
                           (position old-pathname files :test #'equal
                                                        :key #'dired-file-pathname))
                      (min old-index (max 0 (1- (length files)))))))
      (dired-file-line point index))))

(defun %update-dired-buffer (directory pattern buffer)
  (with-writable-buffer (buffer)
    (delete-region (buffer-region buffer))
    (let ((dir-info (variable-value 'dired-information :buffer buffer)))
      (multiple-value-bind (pathnames new-dired-files)
                           (dired-in-buffer directory pattern
                                            (dired-info-dot-files-p dir-info)
                                            buffer
                                            (dired-info-sort dir-info))
        (let ((point (buffer-point buffer))
              (old-dired-files (dired-info-files dir-info)))
          (declare (simple-vector old-dired-files))
          (dotimes (i (length old-dired-files))
            (let ((old-file (svref old-dired-files i)))
              (when (dired-file-deleted-p old-file)
                (let ((pos (position (dired-file-pathname old-file)
                                     new-dired-files :test #'equal
                                     :key #'dired-file-pathname)))
                  (when pos
                    (let* ((new-file (svref new-dired-files pos))
                           (write-date (file-write-date
                                        (dired-file-pathname new-file))))
                      (when (= (dired-file-write-date old-file) write-date)
                        (setf (dired-file-deleted-p new-file) t)
                        (setf (dired-file-write-date new-file) write-date)
                        (setf (next-character (dired-file-line (copy-mark point :temporary) pos))
                              #\D))))))))
          ;; Marks stay on files that are still there.
          (dotimes (i (length old-dired-files))
            (let ((old-file (svref old-dired-files i)))
              (when (dired-file-marked-p old-file)
                (let ((pos (position (dired-file-pathname old-file)
                                     new-dired-files :test #'equal
                                     :key #'dired-file-pathname)))
                  (when pos
                    (let ((new-file (svref new-dired-files pos)))
                      (setf (dired-file-marked-p new-file) t)
                      (unless (dired-file-deleted-p new-file)
                        (setf (next-character (dired-file-line (copy-mark point :temporary) pos))
                              #\*))))))))
          (setf (dired-info-files dir-info) new-dired-files)
          (setf (dired-info-file-list dir-info) pathnames)
          (setf (dired-info-write-date dir-info)
                (dired-directory-signature directory))
          (dired-file-line point 0))))))

;;; DIRED-IN-BUFFER inserts a dired listing of directory in buffer returning
;;; two values: a list of pathnames of files only, and an array of dired-file
;;; structures.  This uses FILTER-REGION to insert a space for the indication
;;; of whether the file is flagged for deletion.  Then we clean up extra header
;;; and trailing lines known to be in the output (into every code a little
;;; slime must fall).
;;;
(defun dired-in-buffer (directory pattern dot-files-p buffer &optional (sort :name))
  (let ((point (buffer-point buffer))
        (*directory-sort* sort))
    (with-writable-buffer (buffer)
      (let* ((pathnames (call-print-directory
			 (if pattern
			     (merge-pathnames directory pattern)
			     (merge-pathnames directory "*.*.~*~"))
                         point
                         dot-files-p))
             (dired-files (make-array (length pathnames))))
        (declare (list pathnames) (simple-vector dired-files))
        (filter-region #'(lambda (str)
                           (concatenate 'simple-string "  " str))
                       (buffer-region buffer))
        (delete-characters point -2)
        (delete-region (line-to-region (mark-line (buffer-start point))))
        (delete-characters point)
        ;; The header: which directory, and how much is in it.  It starts in
        ;; the first column, where a file line has its deletion flag, so line
        ;; motion over files never stops on it.
        (with-mark ((start (buffer-start-mark buffer) :left-inserting))
          (insert-string start (format nil "~A  (~D entr~:@P, ~A~@[, by ~(~A~)~])~%"
                                       (namestring (if pattern
                                                       (merge-pathnames directory pattern)
                                                       directory))
                                       (length pathnames)
                                       (human-size *directory-total-size*)
                                       (unless (eq sort :name) sort))))
        (do ((p pathnames (cdr p))
             (i 0 (1+ i)))
            ((null p))
          (setf (svref dired-files i) (make-dired-file (car p))))
        (dired-file-line point 0)
        (values (delete-if #'directoryp pathnames) dired-files)))))


(defun dired-file-at (mark files)
  "The dired-file whose line MARK is on."
  (let ((index (- (count-lines (region (buffer-start-mark (line-buffer (mark-line mark)))
                                       mark))
                  1 +dired-header-lines+)))
    (when (or (blank-line-p (mark-line mark)) (not (< -1 index (length files))))
      (editor-error "Not on a file line."))
    (aref files index)))

(defun dired-file-line (mark index)
  "Move MARK to the start of the INDEXth file's line, and return it."
  (buffer-start mark)
  (line-offset mark (+ index +dired-header-lines+) 0)
  mark)


(defcommand "Dired Help" (p)
  "How to use dired."
  "How to use dired."
  (declare (ignore p))
  (describe-mode-command nil "Dired"))

(defcommand "Dired Next File" (p)
  "Moves to next undeleted file."
  "Moves to next undeleted file."
  (unless (dired-line-offset (current-point) (or p 1))
    (editor-error "Not enough lines.")))

(defcommand "Dired Previous File" (p)
  "Moves to previous undeleted file."
  "Moves to next undeleted file."
  (unless (dired-line-offset (current-point) (or p -1))
    (editor-error "Not enough lines.")))

;;; DIRED-LINE-OFFSET moves mark n undeleted file lines, returning mark.  If
;;; there are not enough lines, mark remains unmoved, this returns nil.
;;;
(defun dired-line-offset (mark n)
  (with-mark ((m mark))
    (let ((step (if (plusp n) 1 -1)))
      (dotimes (i (abs n) (move-mark mark m))
        (loop
          (unless (line-offset m step 0)
            (return-from dired-line-offset nil))
          (when (blank-line-p (mark-line m))
            (return-from dired-line-offset nil))
          (when (member (next-character m) '(#\space #\*))
            (return)))))))



;;;; Dired user interaction functions.

(defun dired-error-function (string &rest args)
  (apply #'editor-error string args))

(defun dired-report-function (string &rest args)
  (clear-echo-area)
  (apply #'message string args))

(defun dired-yesp-function (string &rest args)
  (prompt-for-y-or-n :prompt (cons string args) :default t))



;;;; Dired expunging and quitting.

(defcommand "Dired Expunge Files" (p)
  "Expunges files marked for deletion.
   Query the user if value of \"Dired File Expunge Confirm\" is non-nil.  Do
   the same with directories and the value of \"Dired Directory Expunge
   Confirm\"."
  "Expunges files marked for deletion.
   Query the user if value of \"Dired File Expunge Confirm\" is non-nil.  Do
   the same with directories and the value of \"Dired Directory Expunge
   Confirm\"."
  (declare (ignore p))
  (when (expunge-dired-files)
    (dired-update-buffer-command nil))
  (maintain-dired-consistency))

(defcommand "Dired Quit" (p)
  "Expunges the files in a dired buffer and then exits."
  "Expunges the files in a dired buffer and then exits."
  (declare (ignore p))
  (expunge-dired-files)
  (delete-buffer-if-possible (current-buffer)))

(defhvar "Dired File Expunge Confirm"
  "When set (the default), \"Dired Expunge Files\" and \"Dired Quit\" will ask
   for confirmation before deleting the marked files."
  :value t)

(defhvar "Dired Directory Expunge Confirm"
  "When set (the default), \"Dired Expunge Files\" and \"Dired Quit\" will ask
   for confirmation before deleting each marked directory."
  :value t)

(defun expunge-dired-files ()
  (multiple-value-bind (marked-files marked-dirs) (get-marked-dired-files)
    (let ((dired:*error-function* #'dired-error-function)
          (dired:*report-function* #'dired-report-function)
          (dired:*yesp-function* #'dired-yesp-function)
          (we-did-something nil))
      (when (and marked-files
                 (or (not (value dired-file-expunge-confirm))
                     (prompt-for-y-or-n :prompt (if (dired-trash-p)
                                                    (format nil "Move ~D file~:P to the Trash? "
                                                            (length marked-files))
                                                    "Really delete files? ")
                                        :default t
                                        :must-exist t
                                        :default-string "Y")))
        (setf we-did-something t)
        (dolist (file-info marked-files)
          (let ((pathname (car file-info))
                (write-date (cdr file-info)))
            (if (= write-date (file-write-date pathname))
                (dired-remove pathname nil)
                (message "~A has been modified, it remains unchanged."
                         (namestring pathname))))))
      (when marked-dirs
        (dolist (dir-info marked-dirs)
          (let ((dir (car dir-info))
                (write-date (cdr dir-info)))
            (if (= write-date (file-write-date dir))
                (when (or (not (value dired-directory-expunge-confirm))
                          (prompt-for-y-or-n
                           :prompt (list (if (dired-trash-p)
                                             "~a is a directory. Move it to the Trash? "
                                             "~a is a directory. Delete it? ")
                                         (directory-namestring dir))
                           :default t
                           :must-exist t
                           :default-string "Y"))
                  (dired-remove dir t)
                  (setf we-did-something t))
                (message "~A has been modified, it remains unchanged.")))))
      we-did-something)))



;;;; Dired copying and renaming.

(defhvar "Dired Copy File Confirm"
  "Can be either t, nil, or :update.  T means always query before clobbering an
   existing file, nil means don't query before clobbering an existing file, and
   :update means only ask if the existing file is newer than the source."
  :value t)

(defhvar "Dired Rename File Confirm"
  "When non-nil, dired will query before clobbering an existing file."
  :value t)

(defcommand "Dired Copy File" (p)
  "Copy the file under the point"
  "Copy the file under the point"
  (declare (ignore p))
  (let* ((point (current-point))
         (confirm (value dired-copy-file-confirm))
         (source (dired-file-pathname
                  (dired-file-at
                   point (dired-info-files (value dired-information)))))
         (dest (prompt-for-file
                :prompt (if (directoryp source)
                            "Destination Directory Name: "
                            "Destination Filename: ")
                :help "Name of new file."
                :default source
                :must-exist nil))
         (dired:*error-function* #'dired-error-function)
         (dired:*report-function* #'dired-report-function)
         (dired:*yesp-function* #'dired-yesp-function))
    (dired:copy-file source dest :update (if (eq confirm :update) t nil)
                     :clobber (not confirm)))
  (maintain-dired-consistency))

(defcommand "Dired Rename File" (p)
  "Rename the file or directory under the point"
  "Rename the file or directory under the point"
  (declare (ignore p))
  (let* ((point (current-point))
         (source (dired-namify (dired-file-pathname
                                (dired-file-at
                                 point
                                 (dired-info-files (value dired-information))))))
         (dest (prompt-for-file
                :prompt "New Filename: "
                :help "The new name for this file."
                :default source
                :must-exist nil))
         (dired:*error-function* #'dired-error-function)
         (dired:*report-function* #'dired-report-function)
         (dired:*yesp-function* #'dired-yesp-function))
    ;; ARRAY-ELEMENT-FROM-MARK moves mark to line start.
    (dired:rename-file source dest :clobber (value dired-rename-file-confirm)))
  (maintain-dired-consistency))

(defcommand "Dired Copy with Wildcard" (p)
  "Copy files that match a pattern containing ONE wildcard."
  "Copy files that match a pattern containing ONE wildcard."
  (declare (ignore p))
  (let* ((dir-info (value dired-information))
         (confirm (value dired-copy-file-confirm))
         (pattern (prompt-for-string
                   :prompt "Filename pattern: "
                   :help "Type a filename with a single asterisk."
                   :trim t))
         (destination (namestring
                       (prompt-for-file
                        :prompt "Destination Spec: "
                        :help "Destination spec.  May contain ONE asterisk."
                        :default (dired-info-pathname dir-info)
                        :must-exist nil)))
         (dired:*error-function* #'dired-error-function)
         (dired:*yesp-function* #'dired-yesp-function)
         (dired:*report-function* #'dired-report-function))
    (dired:copy-file pattern destination :update (if (eq confirm :update) t nil)
                     :clobber (not confirm)
                     :directory (dired-info-file-list dir-info)))
  (maintain-dired-consistency))

(defcommand "Dired Rename with Wildcard" (p)
  "Rename files that match a pattern containing ONE wildcard."
  "Rename files that match a pattern containing ONE wildcard."
  (declare (ignore p))
  (let* ((dir-info (value dired-information))
         (pattern (prompt-for-string
                   :prompt "Filename pattern: "
                   :help "Type a filename with a single asterisk."
                   :trim t))
         (destination (namestring
                       (prompt-for-file
                        :prompt "Destination Spec: "
                        :help "Destination spec.  May contain ONE asterisk."
                        :default (dired-info-pathname dir-info)
                        :must-exist nil)))
         (dired:*error-function* #'dired-error-function)
         (dired:*yesp-function* #'dired-yesp-function)
         (dired:*report-function* #'dired-report-function))
    (dired:rename-file pattern destination
                       :clobber (not (value dired-rename-file-confirm))
                       :directory (dired-info-file-list dir-info)))
  (maintain-dired-consistency))

(defcommand "Delete File" (p)
  "Delete a file.  Specify directories with a trailing slash."
  "Delete a file.  Specify directories with a trailing slash."
  (declare (ignore p))
  (let* ((spec (namestring
                (prompt-for-file
                 :prompt "Delete File: "
                 :help '("Name of File or Directory to delete.  ~
                          One wildcard is permitted.")
                 :must-exist nil)))
         (directoryp (directoryp spec))
         (dired:*error-function* #'dired-error-function)
         (dired:*report-function* #'dired-report-function)
         (dired:*yesp-function* #'dired-yesp-function))
    (when (or (not directoryp)
              (not (value dired-directory-expunge-confirm))
              (prompt-for-y-or-n
               :prompt (list "~A is a directory. Delete it? "
                             (directory-namestring spec))
               :default t :must-exist t :default-string "Y")))
    (dired:delete-file spec :recursive t
                       :clobber (or directoryp
                                    (value dired-file-expunge-confirm))))
  (maintain-dired-consistency))

(defcommand "Copy File" (p)
  "Copy a file, allowing ONE wildcard."
  "Copy a file, allowing ONE wildcard."
  (declare (ignore p))
  (let* ((confirm (value dired-copy-file-confirm))
         (source (namestring
                  (prompt-for-file
                   :prompt "Source Filename: "
                   :help "Name of File to copy.  One wildcard is permitted."
                   :must-exist nil)))
         (dest (namestring
                (prompt-for-file
                 :prompt (if (directoryp source)
                             "Destination Directory Name: "
                             "Destination Filename: ")
                 :help "Name of new file."
                 :default source
                 :must-exist nil)))
         (dired:*error-function* #'dired-error-function)
         (dired:*report-function* #'dired-report-function)
         (dired:*yesp-function* #'dired-yesp-function))
    (dired:copy-file source dest :update (if (eq confirm :update) t nil)
                     :clobber (not confirm)))
  (maintain-dired-consistency))

(defcommand "Rename File" (p)
  "Rename a file, allowing ONE wildcard."
  "Rename a file, allowing ONE wildcard."
  (declare (ignore p))
  (let* ((source (namestring
                  (prompt-for-file
                   :prompt "Source Filename: "
                   :help "Name of file to rename.  One wildcard is permitted."
                   :must-exist nil)))
         (dest (namestring
                (prompt-for-file
                 :prompt (if (directoryp source)
                             "Destination Directory Name: "
                             "Destination Filename: ")
                 :help "Name of new file."
                 :default source
                 :must-exist nil)))
         (dired:*error-function* #'dired-error-function)
         (dired:*report-function* #'dired-report-function)
         (dired:*yesp-function* #'dired-yesp-function))
    (dired:rename-file source dest
                       :clobber (not (value dired-rename-file-confirm))))
  (maintain-dired-consistency))

;;; What changes when a directory's entries do: its modification time, which
;;; is kept to the second, and its size, which on APFS follows the number of
;;; entries, so that two changes in one second are not missed.  NIL when the
;;; directory cannot be read.
;;;
(defun dired-directory-signature (directory)
  (ignore-errors
   (let ((stat (isys:stat (string-right-trim "/" (directory-namestring directory)))))
     (list (isys:stat-mtime stat) (isys:stat-size stat)))))

(defun maintain-dired-consistency ()
  (dolist (info *pathnames-to-dired-buffers*)
    (let* ((directory (directory-namestring (car info)))
           (buffer (cdr info))
           (dir-info (variable-value 'dired-information :buffer buffer))
           (write-date (dired-directory-signature directory)))
      ;; A directory that has gone is left as it was last listed.
      (when (and write-date
                 (not (equal (dired-info-write-date dir-info) write-date)))
        (update-dired-buffer directory (dired-info-pattern dir-info) buffer)))))



;;;; Dired utilities.

;;; GET-MARKED-DIRED-FILES returns as multiple values a list of file specs
;;; and a list of directory specs that have been marked for deletion.  This
;;; assumes the current buffer is a "Dired" buffer.
;;;
(defun get-marked-dired-files ()
  (let* ((files (dired-info-files (value dired-information)))
         (length (length files))
         (marked-files ())
         (marked-dirs ()))
    (unless files (editor-error "Not in Dired buffer."))
    (do ((i 0 (1+ i)))
        ((= i length) (values (nreverse marked-files) (nreverse marked-dirs)))
      (let* ((thing (svref files i))
             (pathname (dired-file-pathname thing)))
        (when (and (dired-file-deleted-p thing) ; file marked for delete
                   (probe-file pathname))       ; file still exists
          (if (directoryp pathname)
              (push (cons pathname (file-write-date pathname)) marked-dirs)
              (push (cons pathname (file-write-date pathname))
                    marked-files)))))))

;;; ARRAY-ELEMENT-FROM-MARK -- Internal Interface.
;;;
;;; This counts the lines between it and the beginning of the buffer.  The
;;; number is used to index vector as if each line mapped to an element
;;; starting with the zero'th element (lines are numbered starting at 1).
;;; This must use AREF since some modes use this with extendable vectors.
;;;
(defun array-element-from-mark (mark vector
                                &optional (error-msg "Invalid line."))
  (when (blank-line-p (mark-line mark)) (editor-error error-msg))
  (aref vector
         (1- (count-lines (region
                           (buffer-start-mark (line-buffer (mark-line mark)))
                           mark)))))

;;; DIRED-NAMIFY and DIRED-DIRECTORIFY are implementation dependent slime.
;;;
(defun dired-namify (pathname)
  (let* ((string (namestring pathname))
         (last (1- (length string))))
    (if (char= (schar string last) #\/)
        (subseq string 0 last)
        string)))

;;;; View Mode.

(defmode "View" :major-p nil
  :setup-function 'setup-view-mode
  :cleanup-function 'cleanup-view-mode
  :precedence 5.0
  :documentation
  "View mode scrolls forwards and backwards in a file with the buffer read-only.
   Scrolling off the end optionally deletes the buffer.")

(defun setup-view-mode (buffer)
  (defhvar "View Return Function"
    "Function that gets called when quitting or returning from view mode."
    :value nil
    :buffer buffer)
  (setf (buffer-writable buffer) nil))
;;;
(defun cleanup-view-mode (buffer)
  (delete-variable 'view-return-function :buffer buffer)
  (setf (buffer-writable buffer) t))

(defcommand "View File" (p &optional pathname)
  "Reads a file in as if by \"Find File\", but read-only.  Commands exist
   for scrolling convenience."
  "Reads a file in as if by \"Find File\", but read-only.  Commands exist
   for scrolling convenience."
  (declare (ignore p))
  (let* ((pn (or pathname
                 (prompt-for-file
                  :prompt "View File: " :must-exist t
                  :help "Name of existing file to read into its own buffer."
                  :default (buffer-default-pathname (current-buffer)))))
         (buffer (make-buffer (format nil "View File ~A" (gensym)))))
    (visit-file-command nil pn buffer)
    (setf (buffer-minor-mode buffer "View") t)
    (change-to-buffer buffer)
    buffer))

(defcommand "View Return" (p)
  "Return to a parent buffer, if it exists."
  "Return to a parent buffer, if it exists."
  (declare (ignore p))
  (unless (call-view-return-fun)
    (editor-error "No View return method for this buffer.")))

(defcommand "View Quit" (p)
  "Delete a buffer in view mode."
  "Delete a buffer in view mode, invoking VIEW-RETURN-FUNCTION if it exists for
   this buffer."
  (declare (ignore p))
  (let* ((buf (current-buffer))
         (funp (call-view-return-fun)))
    (delete-buffer-if-possible buf)
    (unless funp (editor-error "No View return method for this buffer."))))

;;; CALL-VIEW-RETURN-FUN returns nil if there is no current
;;; view-return-function.  If there is one, it calls it and returns t.
;;;
(defun call-view-return-fun ()
  (if (heml-bound-p 'view-return-function)
      (let ((fun (value view-return-function)))
        (cond (fun
               (funcall fun)
               t)))))


(defhvar "View Scroll Deleting Buffer"
  "When this is set, \"View Scroll Down\" deletes the buffer when the end
   of the file is visible."
  :value t)

(defcommand "View Scroll Down" (p)
  "Scroll the current window down through its buffer.
   If the end of the file is visible, then delete the buffer if \"View Scroll
   Deleting Buffer\" is set.  If the buffer is associated with a dired buffer,
   this returns there instead of to the previous buffer."
  "Scroll the current window down through its buffer.
   If the end of the file is visible, then delete the buffer if \"View Scroll
   Deleting Buffer\" is set.  If the buffer is associated with a dired buffer,
   this returns there instead of to the previous buffer."
  (if (and (not p)
           (displayed-p (buffer-end-mark (current-buffer))
                        (current-window))
           (value view-scroll-deleting-buffer))
      (view-quit-command nil)
      (scroll-window-down-command p)))

(defcommand "View Edit File" (p)
  "Turn off \"View\" mode in this buffer."
  "Turn off \"View\" mode in this buffer."
  (declare (ignore p))
  (let ((buf (current-buffer)))
    (setf (buffer-minor-mode buf "View") nil)
    (warn-about-visit-file-buffers buf)))

(defcommand "View Help" (p)
  "Shows \"View\" mode help message."
  "Shows \"View\" mode help message."
  (declare (ignore p))
  (describe-mode-command nil "View"))



;;;; Colours.

;;; The header is bold; a directory is blue, a symbolic link cyan and an
;;; executable green; a file flagged for deletion is red all along, and a
;;; marked one bold yellow.
;;;
(defparameter *dired-line-scanner*
  (cl-ppcre:create-scanner
   "^(.) ([dlpscb-])\\S{9}\\s+\\d+\\s+\\S+\\s+\\S+\\s+\\w{3}\\s+\\d+\\s+(?:\\d\\d:\\d\\d|\\d{4}) "))

(defun dired-line-fonts (string)
  "Where LINE's colours start, as ((POSITION . FONT) ...)."
  (multiple-value-bind (start end) (cl-ppcre:scan *dired-line-scanner* string)
    (cond
      ((zerop (length string)) '())
      ((not start)
       (if (member (char string 0) '(#\Space #\D)) '() (list (cons 0 '(:bold t)))))
      ((char= (char string 0) #\D) (list (cons 0 1)))
      ((char= (char string 0) #\*) (list (cons 0 '(:fg 3 :bold t))))
      (t
       (let ((font (case (char string 2)
                     (#\d '(:fg 4 :bold t))
                     (#\l 6)
                     (t (and (find #\x string :start 3 :end 12) 2)))))
         (when font
           (let ((arrow (search " -> " string :start2 end)))
             (list* (cons end font)
                    (when arrow (list (cons arrow 0)))))))))))

(defun dired-highlight-line (line)
  (let ((old (getf (line-plist line) 'dired-marks)))
    (unless (and old (eq (car old) (line-signature line)))
      (dolist (mark (cdr old))
        (hi::delete-font-mark mark))
      (setf (getf (line-plist line) 'dired-marks)
            (cons (line-signature line)
                  (loop for (position . font) in (dired-line-fonts (line-string line))
                        collect (hi::font-mark line position font)))))))

(define-mode-highlighter "Dired" 'dired-highlight-line)



;;;; Marks, and operations on the marked files.

;;; A file is marked with *, and an operation acts on the marked files, or
;;; on the file under point when none is marked.  D, a deletion flag, is
;;; separate: x (or q) deletes what is flagged.

(defun dired-files ()
  (unless (heml-bound-p 'dired-information)
    (editor-error "Not in Dired buffer."))
  (dired-info-files (value dired-information)))

(defun dired-show-state (index)
  "Put the INDEXth file's flag in the first column of its line."
  (let ((file (svref (dired-files) index)))
    (with-writable-buffer ((current-buffer))
      (with-mark ((mark (current-point)))
        (dired-file-line mark index)
        (setf (next-character mark)
              (cond ((dired-file-deleted-p file) #\D)
                    ((dired-file-marked-p file) #\*)
                    (t #\space)))))))

(defun dired-index-at-point ()
  (let ((files (dired-files)))
    (position (dired-file-at (current-point) files) files)))

(defun dired-targets ()
  "The marked files' pathnames, or the one under point when none is marked."
  (let ((marked (loop for file across (dired-files)
                      when (dired-file-marked-p file)
                        collect (dired-file-pathname file))))
    (or marked
        (list (dired-file-pathname (svref (dired-files) (dired-index-at-point)))))))

(defun dired-set-mark (index value)
  (let ((file (svref (dired-files) index)))
    (setf (dired-file-marked-p file) value)
    (unless value
      (setf (dired-file-deleted-p file) nil
            (dired-file-write-date file) nil))
    (dired-show-state index)))

(defcommand "Dired Mark" (p)
  "Mark the file under point, or the next P files, and move down."
  "Mark the file under point and move down."
  (dotimes (i (or p 1))
    (dired-set-mark (dired-index-at-point) t)
    (dired-down-line (current-point))))

(defcommand "Dired Unmark" (p)
  "Remove the mark or deletion flag from the file under point, or the next P
   files, and move down."
  "Unmark the file under point and move down."
  (dotimes (i (or p 1))
    (dired-set-mark (dired-index-at-point) nil)
    (dired-down-line (current-point))))

(defcommand "Dired Unmark All" (p)
  "Remove every mark and deletion flag."
  "Remove every mark and deletion flag."
  (declare (ignore p))
  (dotimes (i (length (dired-files)))
    (let ((file (svref (dired-files) i)))
      (when (or (dired-file-marked-p file) (dired-file-deleted-p file))
        (dired-set-mark i nil)))))

(defcommand "Dired Toggle Marks" (p)
  "Mark the unmarked files and unmark the marked ones.  Files flagged for
   deletion are left as they are."
  "Toggle the marks."
  (declare (ignore p))
  (dotimes (i (length (dired-files)))
    (let ((file (svref (dired-files) i)))
      (unless (dired-file-deleted-p file)
        (dired-set-mark i (not (dired-file-marked-p file)))))))

(defcommand "Dired Mark with Pattern" (p)
  "Mark the files whose names match a pattern with a single *."
  "Mark the files matching a pattern."
  (declare (ignore p))
  (let* ((matches (dired:pathnames-from-pattern
                   (prompt-for-string :prompt "Mark files matching: "
                                      :help "A file name with a single asterisk."
                                      :trim t)
                   (dired-info-file-list (value dired-information))))
         (files (dired-files)))
    (dolist (pathname matches)
      (let ((index (position pathname files :test #'equal :key #'dired-file-pathname)))
        (when index (dired-set-mark index t))))
    (message "~D file~:P marked." (length matches))))

(defun dired-refresh ()
  "Show the directory as it now is, keeping marks and flags."
  (let ((info (value dired-information)))
    (update-dired-buffer (dired-info-pathname info) (dired-info-pattern info)
                         (current-buffer))
    (maintain-dired-consistency)))

(defun dired-directory ()
  (dired-info-pathname (value dired-information)))

(defun dired-file-name (pathname)
  "PATHNAME's last component: a file's name, or a directory's with its /."
  (if (directoryp pathname)
      (format nil "~A/" (car (last (pathname-directory pathname))))
      (file-namestring pathname)))

(defun dired-each-into-directory (verb targets function)
  "Prompt for a directory and call FUNCTION with each target and where it goes
   there; or, for one target, prompt for the whole new name."
  (if (rest targets)
      (let ((directory (prompt-for-file
                        :prompt (format nil "~A ~D files to directory: " verb (length targets))
                        :help "The directory they go into."
                        :default (dired-directory)
                        :must-exist nil)))
        (unless (directoryp directory)
          (setf directory (pathname (concatenate 'string (namestring directory) "/"))))
        (dolist (target targets)
          (funcall function target (merge-pathnames (dired-file-name target) directory))))
      (let ((target (first targets)))
        (funcall function target
                 (prompt-for-file :prompt (format nil "~A ~A to: " verb (dired-file-name target))
                                  :help "The new name."
                                  :default target
                                  :must-exist nil)))))

(defmacro with-dired-functions (&body body)
  `(let ((dired:*error-function* #'dired-error-function)
         (dired:*report-function* #'dired-report-function)
         (dired:*yesp-function* #'dired-yesp-function))
     ,@body))

(defcommand "Dired Copy" (p)
  "Copy the marked files, or the file under point, to a directory or a new
   name."
  "Copy the marked files or the file under point."
  (declare (ignore p))
  (with-dired-functions
    (dired-each-into-directory
     "Copy" (dired-targets)
     (lambda (from to)
       (dired:copy-file (namestring from) (namestring to)
                        :clobber (not (value dired-copy-file-confirm))))))
  (dired-refresh))

(defcommand "Dired Rename" (p)
  "Rename the marked files, or the file under point, or move them into a
   directory."
  "Rename the marked files or the file under point."
  (declare (ignore p))
  (with-dired-functions
    (dired-each-into-directory
     "Move" (dired-targets)
     (lambda (from to)
       (dired:rename-file (namestring from) (namestring to)
                          :clobber (not (value dired-rename-file-confirm))))))
  (dired-refresh))

(defcommand "Dired Delete" (p)
  "Delete the marked files, or the file under point, after asking."
  "Delete the marked files or the file under point."
  (declare (ignore p))
  (let ((targets (dired-targets))
        (files (dired-files)))
    (dolist (target targets)
      (let ((index (position target files :test #'equal :key #'dired-file-pathname)))
        (setf (dired-file-deleted-p (svref files index)) t
              (dired-file-write-date (svref files index)) (file-write-date target))))
    (expunge-dired-files)
    (dired-refresh)))

(defcommand "Dired Create Directory" (p)
  "Make a directory, named relative to this one."
  "Make a directory."
  (declare (ignore p))
  (let ((name (prompt-for-string :prompt "Create directory: "
                                 :help "The new directory's name, relative to this one."
                                 :trim t)))
    (ensure-directories-exist
     (merge-pathnames (if (and (plusp (length name))
                               (char= (char name (1- (length name))) #\/))
                          name
                          (concatenate 'string name "/"))
                      (dired-directory))))
  (dired-refresh))

(defcommand "Dired Symlink" (p)
  "Make symbolic links to the marked files, or the file under point."
  "Make symbolic links."
  (declare (ignore p))
  (dired-each-into-directory
   "Link" (dired-targets)
   (lambda (from to)
     (isys:symlink (string-right-trim "/" (namestring from))
                   (string-right-trim "/" (namestring to)))))
  (dired-refresh))

(defcommand "Dired Change Mode" (p)
  "Change the permissions of the marked files, or the file under point, to an
   octal mode such as 644."
  "Change permissions."
  (declare (ignore p))
  (let* ((targets (dired-targets))
         (text (prompt-for-string :prompt (format nil "Mode (octal) for ~:[~A~;~*~D files~]: "
                                                  (rest targets) (dired-file-name (first targets))
                                                  (length targets))
                                  :help "Permissions in octal, such as 644 or 755."
                                  :trim t))
         (mode (or (ignore-errors (parse-integer text :radix 8))
                   (editor-error "~S is not an octal mode." text))))
    (dolist (target targets)
      (isys:chmod (string-right-trim "/" (namestring target)) mode)))
  (dired-refresh))

(defcommand "Dired Compress" (p)
  "Compress the marked files, or the file under point, with gzip; one that is
   already compressed (.gz) is uncompressed."
  "Compress or uncompress with gzip."
  (declare (ignore p))
  (dolist (target (dired-targets))
    (unless (directoryp target)
      (let ((result (uiop:run-program (list (if (equalp (pathname-type target) "gz") "gunzip" "gzip")
                                            (namestring target))
                                      :ignore-error-status t
                                      :error-output :string)))
        (declare (ignore result)))))
  (dired-refresh))

(defcommand "Dired Shell Command" (p)
  "Run a shell command on the marked files, or the file under point.  A * in
   the command stands for the files; without one, they go at the end."
  "Run a shell command on files."
  (declare (ignore p))
  (let* ((targets (dired-targets))
         (names (format nil "~{~A~^ ~}"
                        (mapcar (lambda (target)
                                  (uiop:escape-sh-token
                                   (string-right-trim "/" (dired-file-name target))))
                                targets)))
         (command (prompt-for-string :prompt (format nil "! on ~:[~A~;~*~D files~]: "
                                                     (rest targets)
                                                     (dired-file-name (first targets))
                                                     (length targets))
                                     :help "A shell command; * stands for the files."
                                     :trim t))
         (words (cl-ppcre:split "\\s+" command))
         (line (if (member "*" words :test #'string=)
                   (format nil "~{~A~^ ~}"
                           (substitute names "*" words :test #'string=))
                   (concatenate 'string command " " names)))
         (output (uiop:run-program (list "/bin/sh" "-c" line)
                                   :directory (dired-directory)
                                   :output :string :error-output :output
                                   :ignore-error-status t)))
    ;; The refresh says how many files it read: the output is shown after.
    (dired-refresh)
    (if (find #\Newline (string-right-trim '(#\Newline) output))
        (with-pop-up-display (s)
          (write-string output s))
        (message "~A" (string-right-trim '(#\Newline) output)))))



;;;; The Trash.

(defhvar "Dired Delete to Trash"
  "When true, and macOS's trash command is there, files Dired deletes go to
   the Trash, whence Finder can put them back, rather than being deleted for
   good."
  :value t)

(defparameter *trash-program* "/usr/bin/trash")

(defun dired-trash-p ()
  (and (value dired-delete-to-trash) (probe-file *trash-program*) t))

(defun dired-remove (pathname directoryp)
  "Move PATHNAME to the Trash, or delete it, as \"Dired Delete to Trash\" says."
  (let ((name (if directoryp
                  (string-right-trim "/" (directory-namestring pathname))
                  (namestring pathname))))
    (if (dired-trash-p)
        (multiple-value-bind (output error status)
            (uiop:run-program (list *trash-program* name)
                              :ignore-error-status t :error-output :string)
          (declare (ignore output))
          (unless (zerop status)
            (message "Could not move ~A to the Trash: ~A" name error)))
        (dired:delete-file (if directoryp (directory-namestring pathname) name)
                           :clobber t :recursive directoryp))))



;;;; Sorting, opening, and keeping up.

(defcommand "Dired Sort" (p)
  "Sort by name, then by date (newest first), then by size (largest first),
   each time this is run."
  "Change the order files are listed in."
  (declare (ignore p))
  (let ((info (value dired-information)))
    (setf (dired-info-sort info)
          (ecase (dired-info-sort info) (:name :date) (:date :size) (:size :name)))
    (update-dired-buffer (dired-info-pathname info) (dired-info-pattern info)
                         (current-buffer))
    (message "Sorted by ~(~A~)." (dired-info-sort info))))

(defun dired-pathname-at-point ()
  (dired-file-pathname (svref (dired-files) (dired-index-at-point))))

(defcommand "Dired Edit File Other Window" (p)
  "Visit the file under point in the other window, splitting this one if it
   is the only one.  A directory is edited in Dired there."
  "Visit the file under point in the other window."
  (declare (ignore p))
  (let* ((pathname (dired-pathname-at-point))
         (window (if (> (length (remove *echo-area-window* *window-list*)) 1)
                     (next-window (current-window))
                     (or (make-window (window-display-start (current-window)))
                         (editor-error "No room for another window.")))))
    (setf (current-window) window)
    (if (directoryp pathname)
        (dired-command nil (directory-namestring pathname))
        (change-to-buffer (find-file-buffer pathname)))))

(defcommand "Dired Open Externally" (p)
  "Open the marked files, or the file under point, with the application the
   Mac opens them with."
  "Open files with their default application."
  (declare (ignore p))
  (dolist (target (dired-targets))
    (uiop:launch-program (list "/usr/bin/open" (string-right-trim "/" (namestring target))))))

(defcommand "Dired Mouse Edit File" (p)
  "Visit the file double-clicked, or Dired the directory."
  "Visit the file double-clicked."
  (mouse-set-point-command p)
  (dired-edit-file-command nil))

;;; A listing is brought up to date when its directory changes: every two
;;; seconds, each Dired buffer whose directory's write date has moved is
;;; listed again, keeping its marks, flags and point.
;;;
(defparameter +dired-watch-interval+ 2)

(defvar *dired-watching* nil)

(defun dired-watch (elapsed)
  (declare (ignore elapsed))
  (handler-case (maintain-dired-consistency)
    (error () nil)))

(defun start-dired-watch ()
  (unless *dired-watching*
    (schedule-event +dired-watch-interval+ #'dired-watch)
    (setf *dired-watching* t)))
