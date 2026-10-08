;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Compressing and unpacking in Dired.  "Dired Compress" (Z) does to each
;;; of the marked files, or the one under point, what it needs: an archive
;;; is unpacked, a compressed file uncompressed, a directory put in a tar
;;; compressed as "Dired Compression" says, and any other file compressed
;;; so.  "Dired Compress To" (z) puts them all in one archive, whose name's
;;; type says what kind.  The programs run in the background, each with its
;;; own sentinel; what is left to do when one ends -- moving what an archive
;;; held to where it belongs, listing the directory again, saying what went
;;; wrong -- is queued for the command loop, since a sentinel runs inside an
;;; event handler.
;;;
;;; An archive is unpacked into a directory of its own made beside it, and
;;; then: when all it held is under one name, that is moved out, a file as
;;; it is and a directory named as the archive is less its type (foo.tar.gz
;;; to foo/), so there is never foo/foo/; when it held several, the
;;; directory it was unpacked in becomes foo/.  A name already there is
;;; never written over: foo-2/ and so on are used instead.

(in-package :heml)

(defhvar "Dired Compression"
  "How \"Dired Compress\" compresses a file, and with what it compresses the
   tar it makes of a directory: :gzip, :bzip2, :xz, :zstd or :lz4."
  :value :gzip)

(defparameter *compressors*
  '((:gzip "gz" ("gzip") ("gzip" "-d"))
    (:bzip2 "bz2" ("bzip2") ("bzip2" "-d"))
    (:xz "xz" ("xz") ("xz" "-d"))
    (:zstd "zst" ("zstd" "-q" "--rm") ("zstd" "-q" "-d" "--rm"))
    (:lz4 "lz4" ("lz4" "-q" "--rm") ("lz4" "-q" "-d" "--rm"))
    (nil "Z" nil ("gzip" "-d"))
    (nil "br" nil ("brotli" "-q" "-d" "--rm")))
  "(KEY TYPE COMPRESS UNCOMPRESS): a compressor, the type it gives a file,
   and the commands, the file's name after them, that compress a file and
   uncompress one, each in place of the file.")

(defparameter *archive-types*
  '((".tar.gz" :tar) (".tgz" :tar) (".tar.bz2" :tar) (".tbz" :tar) (".tbz2" :tar)
    (".tar.xz" :tar) (".txz" :tar) (".tar.zst" :tar) (".tzst" :tar) (".tar.lz4" :tar)
    (".tar.Z" :tar) (".tar" :tar) (".zip" :zip) (".7z" :7z))
  "The ends of an archive's name, longest first among those that share an
   end, and its kind.")

(defun archive-suffix (name)
  "The end of NAME that says it is an archive, and the archive's kind."
  (loop for (suffix kind) in *archive-types*
        when (and (> (length name) (length suffix))
                  (string-equal suffix name :start2 (- (length name) (length suffix))))
          return (values suffix kind)))

(defun compressed-entry (name)
  "The compressor that made the file NAME, by its type, or NIL."
  (find-if (lambda (entry)
             (let ((suffix (concatenate 'string "." (second entry))))
               (and (> (length name) (length suffix))
                    (string= suffix name :start2 (- (length name) (length suffix))))))
           *compressors*))

(defun compression-entry ()
  (or (assoc (value dired-compression) *compressors*)
      (editor-error "Dired Compression is ~S, not one of ~{~S~^, ~}."
                    (value dired-compression)
                    (remove nil (mapcar #'first *compressors*)))))

(defun need-program (name)
  (unless (find-program name)
    (editor-error "~A is not installed." name)))

(defun unused-name (directory name &optional (type ""))
  "NAME (with TYPE after it) in DIRECTORY, or NAME-2, NAME-3 and so on, the
   first that is not there."
  (loop for i from 1
        for candidate = (if (= i 1)
                            (concatenate 'string name type)
                            (format nil "~A-~D~A" name i type))
        unless (probe-file (concatenate 'string (namestring directory) candidate))
          return candidate))

(defun target-name (pathname)
  "A target's name, without a directory's slash."
  (string-right-trim "/" (dired-file-name pathname)))


;;;; Running the programs.

(defun run-in-background (arguments directory what finish)
  "Run ARGUMENTS in DIRECTORY, collecting what it says; when it ends, the
   command loop calls FINISH with whether it succeeded, and says what it
   said when it did not."
  (let ((output '())
        (done nil))
    (make-process-connection
     (list "/bin/sh" "-c" (format nil "exec 2>&1; exec~{ ~A~}" (mapcar #'shell-quote arguments)))
     :directory (namestring directory)
     :filter (lambda (connection bytes)
               (declare (ignore connection))
               (push (copy-seq bytes) output)
               nil)
     :sentinel (lambda (connection event)
                 (when (and (eq event :disconnected) (not done))
                   (setf done t)
                   (hi::reap-process connection)
                   (let ((code (hi::connection-exit-code connection)))
                     (queue-command
                      (lambda ()
                        ;; Its descriptors closed and itself forgotten.
                        (ignore-errors (delete-connection connection))
                        (let ((ok (or (null code) (zerop code))))
                          (unwind-protect (funcall finish ok)
                            (refresh-dired-buffers directory))
                          (if ok
                              (message "~A." what)
                              (message "~A failed: ~A" what
                                       (string-trim '(#\Space #\Newline)
                                                    (babel:octets-to-string
                                                     (apply #'concatenate
                                                            '(simple-array (unsigned-byte 8) (*))
                                                            (reverse output))
                                                     :encoding :utf-8 :errorp nil))))))))))))
  (message "~A..." what))

(defun refresh-dired-buffers (directory)
  "List DIRECTORY again in every Dired buffer showing it."
  (let ((name (namestring directory)))
    (dolist (entry *pathnames-to-dired-buffers*)
      (let ((buffer (cdr entry)))
        (when (and (member buffer *buffer-list*)
                   (string= name (directory-namestring (car entry)))
                   (not (equal (buffer-major-mode buffer) "Wdired")))
          (let ((info (variable-value 'dired-information :buffer buffer)))
            (update-dired-buffer name (dired-info-pattern info) buffer)))))))


;;;; Each file.

(defun unpack-command (archive kind directory)
  "The command that unpacks ARCHIVE, of KIND, into DIRECTORY."
  (let ((archive (namestring archive))
        (directory (namestring directory)))
    (ecase kind
      (:tar (need-program "tar") (list "tar" "-xf" archive "-C" directory))
      (:zip (need-program "unzip") (list "unzip" "-q" archive "-d" directory))
      (:7z (need-program "7z") (list "7z" "x" "-y" "-bd" (format nil "-o~A" directory) archive)))))

(defun unpacked-roots (directory)
  "What an archive unpacked into DIRECTORY holds at its top, less the
   resource forks a Mac's zip adds."
  (remove-if (lambda (pathname) (equal (target-name pathname) "__MACOSX"))
             (unpacked-entries directory)))

(defun unpacked-entries (directory)
  "What DIRECTORY holds, files and directories, as absolute pathnames: UIOP
   gives them relative to a directory whose name starts with a dot."
  (mapcar (lambda (entry) (merge-pathnames entry directory))
          (append (uiop:directory-files directory) (uiop:subdirectories directory))))

(defun rename-path (from to)
  "Rename FROM to TO, a file or a directory, as rename(2) does: RENAME-FILE
   would give TO the type of FROM when TO has none."
  (let ((from (string-right-trim "/" (namestring from)))
        (to (string-right-trim "/" (namestring to))))
    (unless (zerop (cffi:foreign-funcall "rename" :string from :string to :int))
      (editor-error "Could not rename ~A to ~A." from to))))

(defun settle-unpacked (scratch archive-base parent)
  "Move what was unpacked into SCRATCH to where it belongs in PARENT: one
   file as it is, one directory named ARCHIVE-BASE, several in a directory
   named ARCHIVE-BASE."
  (let ((roots (remove-duplicates (unpacked-roots scratch) :test #'equal)))
    (cond ((null roots)
           (uiop:delete-directory-tree scratch :validate t)
           (editor-error "The archive held nothing."))
          ((and (null (rest roots)) (not (directoryp (first roots))))
           (rename-path (first roots)
                        (concatenate 'string (namestring parent)
                                     (unused-name parent (file-namestring (first roots)))))
           (uiop:delete-directory-tree scratch :validate t))
          ((null (rest roots))
           (rename-path (first roots)
                        (concatenate 'string (namestring parent) (unused-name parent archive-base)))
           (uiop:delete-directory-tree scratch :validate t))
          (t
           (rename-path scratch
                        (concatenate 'string (namestring parent) (unused-name parent archive-base)))))))

(defun unpack-archive (archive suffix kind)
  (let* ((parent (uiop:pathname-directory-pathname archive))
         (name (file-namestring archive))
         (base (subseq name 0 (- (length name) (length suffix))))
         (scratch (merge-pathnames (format nil ".~A.unpacking-~36R/" base (random (expt 36 6)))
                                   parent)))
    (ensure-directories-exist scratch)
    (run-in-background (unpack-command archive kind scratch) parent
                       (format nil "Unpacked ~A" name)
                       (lambda (ok)
                         (if ok
                             (settle-unpacked scratch base parent)
                             (uiop:delete-directory-tree scratch :validate t
                                                                 :if-does-not-exist :ignore))))))

(defun compress-target (target)
  "Do to TARGET what Dired Compress does."
  (let* ((parent (if (directoryp target)
                     (uiop:pathname-parent-directory-pathname target)
                     (uiop:pathname-directory-pathname target)))
         (name (target-name target)))
    (multiple-value-bind (suffix kind) (and (not (directoryp target)) (archive-suffix name))
      (let ((compressed (and (not (directoryp target)) (compressed-entry name))))
        (cond (suffix
               (unpack-archive target suffix kind))
              (compressed
               (let ((command (fourth compressed)))
                 (need-program (first command))
                 (run-in-background (append command (list name)) parent
                                    (format nil "Uncompressed ~A" name)
                                    (lambda (ok) (declare (ignore ok))))))
              ((directoryp target)
               (let ((archive (unused-name parent name
                                           (format nil ".tar.~A" (second (compression-entry))))))
                 (need-program "tar")
                 (run-in-background (list "tar" "-caf" archive name) parent
                                    (format nil "Made ~A" archive)
                                    (lambda (ok)
                                      (unless ok
                                        (delete-file (merge-pathnames archive parent)))))))
              (t
               (let ((command (third (compression-entry))))
                 (need-program (first command))
                 (run-in-background (append command (list name)) parent
                                    (format nil "Compressed ~A" name)
                                    (lambda (ok) (declare (ignore ok)))))))))))

(defcommand "Dired Compress" (p)
  "Unpack, uncompress or compress each of the marked files and directories,
   or the one under point.  An archive (.tar.gz, .tgz, .tar.bz2, .tar.xz,
   .tar.zst, .zip, .7z and the like) is unpacked beside it: what it holds
   under one name is moved out, a directory named as the archive is less
   its type; several things go into a directory so named.  A compressed
   file (.gz, .bz2, .xz, .zst, .lz4, .Z, .br) is uncompressed.  A directory
   is put in a tar beside it, compressed as \"Dired Compression\" says, and
   any other file is compressed so."
  "Unpack, uncompress or compress the files."
  (declare (ignore p))
  (dolist (target (dired-targets))
    (compress-target target)))


;;;; All of them in one archive.

(defun pack-command (archive kind names)
  (ecase kind
    (:tar (need-program "tar") (list* "tar" "-caf" archive names))
    (:zip (need-program "zip") (list* "zip" "-qr" archive names))
    (:7z (need-program "7z") (list* "7z" "a" "-bd" archive names))))

(defcommand "Dired Compress To" (p)
  "Put the marked files and directories, or the one under point, in one
   archive, whose name's type says what kind: .tar.gz, .tgz, .tar.bz2,
   .tar.xz, .tar.zst, .tar, .zip or .7z."
  "Put the files in one archive."
  (declare (ignore p))
  (let* ((targets (dired-targets))
         (directory (dired-directory))
         (default (concatenate 'string
                               (if (rest targets)
                                   (car (last (pathname-directory directory)))
                                   (target-name (first targets)))
                               ".tar." (second (compression-entry))))
         (archive (prompt-for-string
                   :prompt (format nil "Compress ~:[~A~;~*~D files~] to: "
                                   (rest targets) (target-name (first targets)) (length targets))
                   :default default :default-string default :trim t
                   :help "The archive's name; its type says what kind it is.")))
    (multiple-value-bind (suffix kind) (archive-suffix archive)
      (declare (ignore suffix))
      (unless kind
        (editor-error "~A does not end in a kind of archive Heml knows." archive))
      (let ((path (merge-pathnames archive directory)))
        (when (and (probe-file path)
                   (not (prompt-for-y-or-n :prompt (format nil "~A is there.  Replace it? " archive)
                                           :default nil)))
          (editor-error "Not replaced."))
        (when (probe-file path) (delete-file path))
        (run-in-background (pack-command (namestring path) kind
                                         (mapcar (lambda (target)
                                                   (enough-namestring
                                                    (string-right-trim "/" (namestring target))
                                                    directory))
                                                 targets))
                           directory
                           (format nil "Made ~A" archive)
                           (lambda (ok)
                             (unless ok
                               (when (probe-file path) (delete-file path)))))))))
