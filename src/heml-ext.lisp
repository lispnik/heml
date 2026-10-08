;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :heml-ext)

(defconstant hi::char-code-limit 256)
(defconstant char-code-limit 256)

(defun skip-whitespace (&optional (stream *standard-input*))
  (peek-char t stream))


;;; These are just stubs for now:

(defun quit ()
  )

(defvar hi::*command-line-switches* nil)

(defun hi::get-terminal-name ()
  "vt100")

(defun default-directory ()
  (let* ((p (heml::buffer-default-directory (current-buffer)))
         (p (and p (namestring p))))
    (if (and p
             (handler-case
                 (eq (iolib.os:file-kind p) :directory)
               (iolib.pathnames:invalid-file-path () nil)))
        p
        (isys:getcwd))))


(defun find-buffer (name)
  (getstring name hi:*buffer-names*))

(defun maybe-rename-buffer (buffer new-name)
  (unless (find-buffer new-name)
    (setf (buffer-name buffer) new-name)))

(defun rename-buffer-uniquely (buffer new-name)
  (or (maybe-rename-buffer buffer new-name)
      (iter:iter
       (iter:for i from 2)
       (iter:until
        (maybe-rename-buffer buffer (format nil "~A<~D>" new-name i))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hi::%sp-byte-blt (src start dest dstart end)
  (loop for s from start
        for d from dstart below end
        do
        (setf (aref dest d) (aref src s))))


(defun delq (item list)
  (delete item list))

(defun memq (item list)
  (member item list))

(defun assq (item alist)
  (assoc item alist))

(defun concat (&rest args)
  (apply #'concatenate 'string args))


;;;; File names as a shell writes them.

;;; ~ is the user's home directory and ~NAME is NAME's, at the start of a
;;; file name.  A file prompt starts with a directory already in it, so, as
;;; in Emacs, a ~ after a / starts the name again from there, and so does a
;;; second / after one: /some/dir/~/notes is ~/notes, and /some/dir//etc is
;;; /etc.

(defvar *home-directories* (make-hash-table :test 'equal)
  "User's name to its home directory, as found.")

(defun user-home-directory (name)
  "The home directory of the user NAME, the current user for \"\", without
   a / at its end; or NIL if there is no such user."
  (if (zerop (length name))
      (string-right-trim "/" (namestring (user-homedir-pathname)))
      (multiple-value-bind (home found) (gethash name *home-directories*)
        (if found
            home
            (setf (gethash name *home-directories*)
                  (ignore-errors
                   (nth 5 (multiple-value-list (isys:getpwnam name)))))))))

(defun expand-file-name (name)
  "NAME, a file name as typed, with a ~ or ~USER that starts it replaced by
   the home directory, and with what comes before a later start of a name --
   a ~ after a /, or a second / -- left out.  A ~USER of no user is left."
  (let* ((name (if (pathnamep name) (namestring name) name))
         (tilde (loop for i from (1- (length name)) downto 0
                      when (and (char= (char name i) #\~)
                                (or (zerop i) (char= (char name (1- i)) #\/)))
                        return i))
         (double (search "//" name :from-end t)))
    (cond ((and tilde (or (null double) (> tilde double)))
           (let* ((slash (position #\/ name :start tilde))
                  (home (user-home-directory (subseq name (1+ tilde) slash))))
             (if home
                 (concatenate 'string home (if slash (subseq name slash) ""))
                 (subseq name tilde))))
          (double (subseq name (1+ double)))
          (t name))))


;;;; complete-file

(defun complete-file (pathname &key (defaults *default-pathname-defaults*)
                      ignore-types)
  (setf pathname (expand-file-name pathname))
  (let ((files (complete-file-directory pathname defaults)))
    (cond ((null files)
           (values nil nil))
          ((null (cdr files))
           (values (car files)
                   t))
          (t
           (let ((good-files
                  (delete-if #'(lambda (pathname)
                                 (and (simple-string-p
                                       (pathname-type pathname))
                                      (member (pathname-type pathname)
                                              ignore-types
                                              :test #'string=)))
                             files)))
             (cond ((null good-files))
                   ((null (cdr good-files))
                    (return-from complete-file
                      (values (car good-files)
                              t)))
                   (t
                    (setf files good-files)))
             (let ((common (file-namestring (car files))))
               (dolist (file (cdr files))
                 (let ((name (file-namestring file)))
                   (dotimes (i (min (length common) (length name))
                             (when (< (length name) (length common))
                               (setf common name)))
                     (unless (char= (schar common i) (schar name i))
                       (setf common (subseq common 0 i))
                       (return)))))
               (values (merge-pathnames common pathname)
                       nil)))))))

;;; COMPLETE-FILE-DIRECTORY-ARG -- Internal.
;;;
(defun complete-file-directory (pathname defaults)
  (let* ((namestring
          (namestring
           (merge-pathnames pathname (directory-namestring defaults))))
         (directory
          (if (eq (iolib.os::get-file-kind namestring t) :directory)
              namestring
              (iolib.pathnames:file-path-directory namestring :namestring t))))
    (delete-if-not (lambda (candidate)
                     (search namestring candidate))
                   (append
                    (when (probe-file namestring)
                      (list namestring))
                    (mapcar (lambda (f)
                              (iolib.pathnames:file-path-namestring
                               (iolib.pathnames:merge-file-paths
                                f directory)))
                            (iolib.os:list-directory directory))))))

;;; Ambiguous-Files  --  Public
;;;
(defun ambiguous-files (pathname
                        &optional (defaults *default-pathname-defaults*))
  "Return a list of all files which are possible completions of Pathname.
   We look in the directory specified by Defaults as well as looking down
   the search list."
  (complete-file-directory (expand-file-name pathname) defaults))


;;;; CLISP fixage

;;;;;;

(defun set-file-permissions (pathname access)
  (declare (ignore pathname access))
  nil)
