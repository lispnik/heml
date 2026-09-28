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
  (getstring name hi::*buffer-names*))

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


;;;; complete-file

(defun complete-file (pathname &key (defaults *default-pathname-defaults*)
                      ignore-types)
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
  (complete-file-directory pathname defaults))


;;;; CLISP fixage

;;;;;;

(defun set-file-permissions (pathname access)
  (declare (ignore pathname access))
  nil)
