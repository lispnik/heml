;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Grep and Compile run a command and list what it prints in a result
;;; buffer (results.lisp): a line FILE:LINE:TEXT, or FILE:LINE:COLUMN:TEXT as
;;; compilers write, names a place.  Grep colours the file, the line number
;;; and what matched; C-x C-q makes the matched text editable (Wgrep), and
;;; C-c C-c writes the lines changed back to their files' buffers.

(in-package :heml)

(defmode "Grep" :major-p t
  :documentation
  "The lines a grep found, one a line after the command.  Return visits a
   line's file there, n and p show the next and previous, g runs the command
   again, and C-x C-q edits the lines in place.")

(defmode "Compilation" :major-p t
  :documentation
  "What a compilation printed.  Return visits the place an error names, n
   and p show the next and previous, and g compiles again.")

(defmode "Wgrep" :major-p t
  :documentation
  "A Grep buffer whose matched lines can be edited.  C-c C-c writes the
   lines changed to their files' buffers, and C-c C-k puts them back.")

;;; A line naming a place: the file, the line number, perhaps a column, and
;;; the text.  The file may not start with a space (which a compiler's
;;; message about its own progress might), and may be a Windows-free
;;; relative or absolute name.
;;;
;;; Node's test runner says where a failing test is with test at x.js:3:1,
;;; which is one of the other places, below, not a file "test at x.js".
;;;
(defparameter *location-scanner*
  (cl-ppcre:create-scanner "^(?!test at )([^:\\s][^:]*):(\\d+):(?:(\\d+):)?"))

(defun grep-line-parts (string)
  "For a line naming a place: the end of the file's name, the line number's
   start and end, and the start of the text; and the line and column."
  (multiple-value-bind (start end starts ends) (cl-ppcre:scan *location-scanner* string)
    (when start
      (values (aref ends 0) (aref starts 1) (aref ends 1) end
              (parse-integer string :start (aref starts 1) :end (aref ends 1))
              (and (aref starts 2)
                   (parse-integer string :start (aref starts 2) :end (aref ends 2)))))))

;;; What else a compilation's lines may name a place with: a Python
;;; traceback's File "x.py", line 12, a shell's x.sh: line 3:, Free
;;; Pascal's and Delphi's x.pas(3,5) Error:, rustc's indented --> x.rs:3:5,
;;; a Rust test's panicked at x.rs:3:5, a Go test's indented
;;; x_test.go:12:, tsc's x.ts(3,5): error, Node's test at x.js:3:1, and
;;; a Node stack frame's at f (/x.js:3:5).  The third group, when there is
;;; one, is the column.
;;;
(defparameter *other-location-scanners*
  (list (cl-ppcre:create-scanner "^\\s*File \"([^\"]+)\", line (\\d+)")
        (cl-ppcre:create-scanner "^([^(\\s][^(]*)\\((\\d+)(?:,(\\d+))?\\):? (?:Fatal|Error|Warning|Hint|Note|error|warning)")
        (cl-ppcre:create-scanner "^([^:\\s][^:]*): line (\\d+):")
        (cl-ppcre:create-scanner "^\\s*--> ([^:\\s][^:]*):(\\d+):(\\d+)")
        (cl-ppcre:create-scanner "panicked at ([^:\\s][^:]*):(\\d+):(\\d+)")
        (cl-ppcre:create-scanner "^test at ([^:\\s][^:]*):(\\d+):(\\d+)")
        (cl-ppcre:create-scanner "^\\s+([^\\s:]+\\.go):(\\d+):")
        (cl-ppcre:create-scanner "^\\s+at (?:.*\\()?(?:file://)?(/[^():]+):(\\d+):(\\d+)\\)?\\s*$")))

(defun other-location-parts (string)
  "For a traceback's or a shell's line, and the others above: the file's
   start and end, the line number, and the column or NIL."
  (dolist (scanner *other-location-scanners*)
    (multiple-value-bind (start end starts ends) (cl-ppcre:scan scanner string)
      (declare (ignore end))
      (when start
        (return (values (aref starts 0) (aref ends 0)
                        (parse-integer string :start (aref starts 1) :end (aref ends 1))
                        (and (> (length starts) 2) (aref starts 2)
                             (parse-integer string :start (aref starts 2) :end (aref ends 2)))))))))

(defun grep-line-location (line)
  (let ((string (line-string line))
        (directory (variable-value 'grep-directory :buffer (line-buffer line))))
    (multiple-value-bind (file-end number-start number-end text-start number column)
        (grep-line-parts string)
      (declare (ignore number-start number-end text-start))
      (if file-end
          (list (merge-pathnames (subseq string 0 file-end) directory) number column)
          (when (string= (buffer-major-mode (line-buffer line)) "Compilation")
            (multiple-value-bind (file-start file-end number column)
                (other-location-parts string)
              (when file-start
                (list (merge-pathnames (subseq string file-start file-end) directory)
                      number column))))))))


;;;; What a grep searched for.

(defun shell-words (command)
  "COMMAND's words as the shell would split them, quotes removed."
  (let ((words '()) (word nil) (quote nil) (i 0) (n (length command)))
    (flet ((add (char) (push char word))
           (end () (when word (push (coerce (nreverse word) 'string) words) (setf word nil))))
      (loop while (< i n)
            do (let ((char (char command i)))
                 (cond (quote
                        (if (char= char quote)
                            (progn (setf quote nil) (unless word (setf word (list))))
                            (add char)))
                       ((member char '(#\' #\")) (setf quote char) (unless word (setf word (list))))
                       ((char= char #\\) (incf i) (when (< i n) (add (char command i))))
                       ((member char '(#\Space #\Tab)) (end))
                       (t (add char))))
               (incf i))
      (end))
    (nreverse words)))

(defun bre-to-ere (pattern)
  "A basic regular expression as an extended one: \\( \\) \\| \\{ \\} \\+ \\?
   are the operators, and ( ) | { } + ? are characters."
  (with-output-to-string (s)
    (loop with i = 0 while (< i (length pattern))
          do (let ((char (char pattern i)))
               (cond ((and (char= char #\\) (< (1+ i) (length pattern))
                           (find (char pattern (1+ i)) "(){}|+?"))
                      (write-char (char pattern (1+ i)) s)
                      (incf i))
                     ((find char "(){}|+?")
                      (write-char #\\ s) (write-char char s))
                     (t (write-char char s))))
             (incf i))))

(defun grep-pattern-scanner (command)
  "A scanner for what COMMAND, a grep, searches for, or NIL."
  (let* ((words (shell-words command))
         (program (file-namestring (or (first words) "")))
         (options (remove-if-not (lambda (w) (and (> (length w) 1) (char= (char w 0) #\-)))
                                 (rest words)))
         (letters (apply #'concatenate 'string
                         (mapcar (lambda (o) (if (char= (char o 1) #\-) "" (subseq o 1)))
                                 options)))
         (pattern (or (second (member "-e" words :test #'string=))
                      (let ((long (find-if (lambda (w) (eql 0 (search "--regexp=" w))) words)))
                        (and long (subseq long 9)))
                      (find-if (lambda (w) (not (char= (char w 0) #\-))) (rest words)))))
    (when (and pattern (plusp (length pattern)))
      (let ((extended (or (find #\E letters) (find #\P letters)
                          (member program '("egrep" "rg" "ag" "ack") :test #'string=)))
            (fixed (or (find #\F letters) (string= program "fgrep"))))
        (ignore-errors
         (cl-ppcre:create-scanner
          (cond (fixed (cl-ppcre:quote-meta-chars pattern))
                (extended pattern)
                (t (bre-to-ere pattern)))
          :case-insensitive-mode (or (find #\i letters)
                                     (and (string= program "rg")
                                          (member "-S" words :test #'string=)
                                          (string= pattern (string-downcase pattern))))))))))


;;;; Colours.

(defparameter *grep-file-font* 5)
(defparameter *grep-line-number-font* 2)
(defparameter *grep-match-font* '(:fg 1 :bold t))
(defparameter *compilation-error-font* '(:fg 1 :bold t))
(defparameter *compilation-warning-font* 3)

(defun grep-line-fonts (buffer string)
  "Where STRING's colours start, as ((POSITION . FONT) ...)."
  (multiple-value-bind (file-end number-start number-end text-start)
      (grep-line-parts string)
    (cond
      ((zerop (length string)) '())
      ((not file-end)
       (multiple-value-bind (file-start file-end)
           (and (string= (buffer-major-mode buffer) "Compilation")
                (other-location-parts string))
         (cond (file-start
                (list (cons file-start *grep-file-font*) (cons file-end 0)))
               ;; The header and the summary.
               ((member (char string 0) '(#\Space #\Tab)) '())
               (t (list (cons 0 '(:bold t)))))))
      (t
       (append
        (list (cons 0 *grep-file-font*) (cons file-end 0)
              (cons number-start *grep-line-number-font*) (cons number-end 0))
        (if (string= (buffer-major-mode buffer) "Compilation")
            (let ((kind (cl-ppcre:scan "(?i)\\b(error|fatal)\\b" string :start text-start))
                  (warning (cl-ppcre:scan "(?i)\\bwarning\\b" string :start text-start)))
              (cond (kind (list (cons text-start *compilation-error-font*)))
                    (warning (list (cons text-start *compilation-warning-font*)))))
            (let ((scanner (and (heml-bound-p 'grep-pattern :buffer buffer)
                                (variable-value 'grep-pattern :buffer buffer))))
              (when scanner
                (let ((fonts '()))
                  (cl-ppcre:do-matches (start end scanner string nil :start text-start)
                    (when (< start end)
                      (push (cons start *grep-match-font*) fonts)
                      (push (cons end 0) fonts)))
                  (nreverse fonts))))))))))

(defun grep-highlight-line (line)
  (let ((old (getf (line-plist line) 'grep-marks)))
    (unless (and old (eq (car old) (line-signature line)))
      (dolist (mark (cdr old))
        (hi::delete-font-mark mark))
      (setf (getf (line-plist line) 'grep-marks)
            (cons (line-signature line)
                  (loop for (position . font)
                          in (grep-line-fonts (line-buffer line) (line-string line))
                        collect (hi::font-mark line position font)))))))

(define-mode-highlighter "Grep" 'grep-highlight-line)
(define-mode-highlighter "Compilation" 'grep-highlight-line)
(define-mode-highlighter "Wgrep" 'grep-highlight-line)


;;;; Running the command.

(defun utf-8-complete-end (bytes)
  "How many of BYTES make whole UTF-8 characters, leaving out a character a
   read has split."
  (let ((n (length bytes)))
    (loop for i from (1- n) downto (max 0 (- n 4))
          for byte = (aref bytes i)
          do (cond ((< byte #x80) (return n))
                   ((>= byte #xC0)
                    (return (if (>= (- n i) (cond ((>= byte #xF0) 4) ((>= byte #xE0) 3) (t 2)))
                                n
                                i))))
          finally (return n))))

(defun insert-at-end (buffer string)
  "Add STRING to the end of BUFFER, moving any mark there that was at the
   end, as a window's point that is following the output."
  (with-writable-buffer (buffer)
    (with-mark ((end (buffer-end-mark buffer) :left-inserting))
      (insert-string end string))))

(defun run-into-result-buffer (buffer command directory what)
  "Run COMMAND with the shell in DIRECTORY, adding what it prints to the end
   of BUFFER and then a line saying how many results it found."
  (let ((pending (make-array 0 :element-type '(unsigned-byte 8)))
        (finished nil))
    (insert-at-end buffer (format nil "~A in ~A~%~A~%~%" what (namestring directory) command))
    (make-process-connection
     (list "/bin/sh" "-c" (format nil "exec 2>&1; ~A" command))
     :directory directory
     :filter (lambda (connection bytes)
               (declare (ignore connection))
               (let* ((bytes (concatenate '(vector (unsigned-byte 8)) pending bytes))
                      (end (utf-8-complete-end bytes)))
                 (setf pending (subseq bytes end))
                 (when (member buffer *buffer-list*)
                   (insert-at-end buffer
                                  (remove #\Return
                                          (babel:octets-to-string bytes :end end
                                                                  :encoding :utf-8
                                                                  :errorp nil)))))
               nil)
     :sentinel (lambda (connection event)
                 (declare (ignore connection))
                 ;; No message: the sentinel runs inside an event handler,
                 ;; where redisplay, which dispatches events, is not safe.
                 (when (and (eq event :disconnected) (not finished)
                            (member buffer *buffer-list*))
                   (setf finished t)
                   (let ((count 0))
                     (do ((line (mark-line (buffer-start-mark buffer)) (line-next line)))
                         ((null line))
                       (when (grep-line-location line) (incf count)))
                     (insert-at-end buffer
                                    (format nil "~&~%~A finished: ~[no results~:;~:*~D result~:P~].~%"
                                            what count))))))))

(defun start-result-command (name mode command directory what)
  (let ((buffer (make-result-buffer name mode 'grep-line-location)))
    (dolist (variable '(grep-directory grep-command grep-pattern))
      (unless (heml-bound-p variable :buffer buffer)
        (defhvar (string-capitalize (substitute #\Space #\- (string variable)))
          "Where, what and for what a Grep ran." :buffer buffer)))
    (setf (variable-value 'grep-directory :buffer buffer) directory
          (variable-value 'grep-command :buffer buffer) command
          (variable-value 'grep-pattern :buffer buffer)
          (and (string= mode "Grep") (grep-pattern-scanner command)))
    (run-into-result-buffer buffer command directory what)
    (let ((window (result-window buffer)))
      (if window
          (select-window window)
          (change-to-buffer buffer)))
    (buffer-start (current-point))
    buffer))

(defvar *last-grep-command* "grep -nH -e ")
(defvar *last-compile-command* "make -k ")

(defcommand "Grep"
    (p &optional (command (prompt-for-string :prompt "Run grep (like this): "
                                             :default *last-grep-command*))
                 (directory (default-directory)))
  "Run a grep, listing the lines it finds in the buffer *grep*: Return on
   one visits it, and C-x ` visits the next from anywhere."
  "Run a grep and list what it finds."
  (declare (ignore p))
  (setf *last-grep-command* command)
  (start-result-command "*grep*" "Grep" command directory "Grep"))

(defcommand "Recursive Grep" (p)
  "Search for a regular expression in the files under a directory, leaving
   out version control's: with ripgrep (rg) where it is installed, which
   also leaves out what .gitignore names, and grep otherwise."
  "Search the files under a directory."
  (declare (ignore p))
  (let* ((pattern (prompt-for-string :prompt "Search for: "
                                     :default (let ((word (word-at-point)))
                                                (and (plusp (length word)) word))))
         (directory (prompt-for-file :prompt "In directory: "
                                     :default (default-directory)
                                     :must-exist t)))
    (grep-command nil (recursive-grep-command-line pattern)
                  (directory-namestring (merge-pathnames directory (default-directory))))))

(defun recursive-grep-command-line (pattern)
  "A command searching the files under its directory for PATTERN."
  (if (find-program "rg")
      ;; ripgrep leaves out what .gitignore does, and binaries.
      (format nil "rg -n --no-heading --color never -e ~A ." (shell-quote pattern))
      (format nil "grep -rnH -I --exclude-dir=.git --exclude-dir=.hg -e ~A ."
              (shell-quote pattern))))

(defun find-program (name)
  "Where the shell would find the program NAME, or NIL."
  (loop for directory in (uiop:split-string (or (uiop:getenv "PATH") "") :separator ":")
        for file = (and (plusp (length directory))
                        (probe-file (merge-pathnames name (uiop:ensure-directory-pathname directory))))
        when file return file))

(defun shell-quote (string)
  (format nil "'~A'" (cl-ppcre:regex-replace-all "'" string "'\\\\''")))

(defun word-at-point ()
  (with-mark ((start (current-point)) (end (current-point)))
    (loop while (and (previous-character start) (alphanumericp (previous-character start))
                     (mark-before start)))
    (loop while (and (next-character end) (or (alphanumericp (next-character end))
                                              (find (next-character end) "-_*")))
          do (mark-after end))
    (region-to-string (region start end))))

(defcommand "Compile"
    (p &optional (command (prompt-for-string :prompt "Compile command: "
                                             :default *last-compile-command*))
                 (directory (default-directory)))
  "Run a compilation, listing what it prints in the buffer *compilation*:
   Return on an error visits its place, and C-x ` visits the next from
   anywhere."
  "Run a compilation and list its errors."
  (declare (ignore p))
  (setf *last-compile-command* command)
  (start-result-command "*compilation*" "Compilation" command directory "Compilation"))

(defcommand "Grep Again" (p)
  "Run this buffer's command again."
  "Run this buffer's command again."
  (declare (ignore p))
  (unless (heml-bound-p 'grep-command :buffer (current-buffer))
    (editor-error "Not a Grep buffer."))
  (let ((buffer (current-buffer)))
    (start-result-command (buffer-name buffer) (buffer-major-mode buffer)
                          (variable-value 'grep-command :buffer buffer)
                          (variable-value 'grep-directory :buffer buffer)
                          (if (string= (buffer-major-mode buffer) "Grep") "Grep" "Compilation"))))

;;; Hemlock's name for visiting a match.
(defcommand "Grep Goto" (p)
  "Visit the place on this line."
  "Visit the place on this line."
  (result-goto-command p))


;;;; Wgrep: editing the lines found.

;;; Each line's text as it was is kept in its plist when editing starts, so
;;; that a line whose text has changed is known, and can be put back.

(defun grep-line-text (line)
  (multiple-value-bind (file-end number-start number-end text-start)
      (grep-line-parts (line-string line))
    (declare (ignore number-start number-end))
    (when file-end (subseq (line-string line) text-start))))

(defun map-grep-lines (function buffer)
  (do ((line (mark-line (buffer-start-mark buffer)) (line-next line)))
      ((null line))
    (when (grep-line-location line)
      (funcall function line))))

(defcommand "Grep Edit" (p)
  "Make the lines found editable: change them, then C-c C-c writes them to
   their files' buffers."
  "Make the lines found editable."
  (declare (ignore p))
  (let ((buffer (current-buffer)))
    (unless (string= (buffer-major-mode buffer) "Grep")
      (editor-error "Not a Grep buffer."))
    (map-grep-lines (lambda (line)
                      (setf (getf (line-plist line) 'grep-original) (grep-line-text line)))
                    buffer)
    (setf (buffer-major-mode buffer) "Wgrep")
    (setf (buffer-writable buffer) t)
    (message "Edit the lines, then C-c C-c to write them, C-c C-k to give up.")))

(defun leave-wgrep (buffer)
  (map-grep-lines (lambda (line) (remf (line-plist line) 'grep-original)) buffer)
  (setf (buffer-major-mode buffer) "Grep")
  (setf (buffer-writable buffer) nil))

(defcommand "Wgrep Finish" (p)
  "Write each line whose text has changed to its file's buffer, replacing
   that line there, and go back to Grep.  The buffers are left to save."
  "Write the changed lines to their files' buffers."
  (declare (ignore p))
  (let ((buffer (current-buffer)) (changed 0) (buffers '()) (conflicts 0))
    (map-grep-lines
     (lambda (line)
       (let ((original (getf (line-plist line) 'grep-original))
             (text (grep-line-text line)))
         (when (and original (string/= original text))
           (destructuring-bind (pathname number &optional column) (grep-line-location line)
             (declare (ignore column))
             (let ((target (find-file-buffer pathname)))
               (with-mark ((mark (buffer-start-mark target)))
                 (cond ((and (line-offset mark (1- number))
                             (string= (line-string (mark-line mark)) original))
                        (line-start mark)
                        (with-mark ((end mark))
                          (line-end end)
                          (delete-region (region mark end))
                          (insert-string mark text))
                        (setf (getf (line-plist line) 'grep-original) text)
                        (incf changed)
                        (pushnew target buffers))
                       (t (incf conflicts)))))))))
     buffer)
    (leave-wgrep buffer)
    (message "Changed ~D line~:P in ~D buffer~:P~[~:;; ~:*~D line~:P had changed in its file~]~@[; save them with C-x s~]."
             changed (length buffers) conflicts (plusp changed))))

(defcommand "Wgrep Abort" (p)
  "Put back each line's text as grep found it, and go back to Grep."
  "Put back the lines as they were."
  (declare (ignore p))
  (let ((buffer (current-buffer)))
    (with-writable-buffer (buffer)
      (map-grep-lines
       (lambda (line)
         (let ((original (getf (line-plist line) 'grep-original))
               (text (grep-line-text line)))
           (when (and original (string/= original text))
             (with-mark ((mark (mark line 0)))
               (character-offset mark (- (length (line-string line)) (length text)))
               (with-mark ((end mark))
                 (line-end end)
                 (delete-region (region mark end))
                 (insert-string mark original))))))
       buffer))
    (leave-wgrep buffer)
    (message "Put back.")))
