;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Slave Apropos (as opposed to "heml com>mand name apropos" aka Apropos)

(in-package :heml)



(defvar *apropos-entries* nil)
(defvar *apropos-entries-end* nil)
;;;

(defstruct (apropos-entry
             (:constructor internal-make-apropos-entry
                           (slavesym kinds)))
  slavesym
  ;; What the symbol names -- a function, a variable, a class -- as a list
  ;; ((KIND DOCSTRING) ...), DOCSTRING NIL where there is none.
  kinds)

(defun parse-apropos-entry (slot-list)
  (destructuring-bind (slavesym &rest plist) slot-list
    (internal-make-apropos-entry
     slavesym
     (or (loop for (kind docstring) on plist by #'cddr
               collect (list kind (if (eq docstring :not-documented) nil docstring)))
         (list (list :unbound nil))))))

;;; This is the apropos buffer if it exists.
;;;
(defvar *apropos-buffer* nil)

;;; This is the cleanup method for deleting *apropos-buffer*.
;;;
(defun delete-apropos-buffers (buffer)
  (when (eq buffer *apropos-buffer*)
    (setf *apropos-buffer* nil)
    (setf *apropos-entries* nil)))


;;;; Commands.

(defmode "Apropos" :major-p t
  :documentation "Apropos mode presents a list of slave symbols: what each
   names, with its documentation's first line.  Space describes the symbol on
   the line, . visits its definition, and n and p move between symbols.")

(defcommand "Apropos Quit" (p)
  "Kill the apropos buffer."
  ""
  (declare (ignore p))
  (when *apropos-buffer* (delete-buffer-if-possible *apropos-buffer*)))

;;; Each of an entry's lines keeps the entry in its plist.
;;;
(defun apropos-entry-from-mark (mark)
  (or (getf (line-plist (mark-line mark)) 'apropos-entry)
      (editor-error "No symbol on this line.")))

(defun slave-symbol-form (slavesym)
  "SLAVESYM as a form the slave reads as that symbol."
  (flet ((escape (string)
           (with-output-to-string (s)
             (loop for char across string
                   do (when (find char "|\\") (write-char #\\ s))
                      (write-char char s)))))
    (format nil "'|~A|::|~A|"
            (escape (slave-symbol-package-name slavesym))
            (escape (slave-symbol-name slavesym)))))

(defcommand "Apropos Find Definition" (p)
  "Visit the definition of the symbol on this line."
  "Visit the definition of the symbol on this line."
  (declare (ignore p))
  (find-definitions (apropos-entry-slavesym (apropos-entry-from-mark (current-point)))))

(defcommand "Apropos Describe" (p)
  "Describe the symbol on this line, as the slave's DESCRIBE does."
  "Describe the symbol on this line."
  (declare (ignore p))
  (let ((entry (apropos-entry-from-mark (current-point)))
        (info (or (value current-eval-server)
                  (editor-error "No slave to describe the symbol in."))))
    (with-pop-up-display (s)
      (write-string (eval-form-in-server-1
                     info
                     (format nil "(heml::describe-symbol-aux ~A)"
                             (slave-symbol-form (apropos-entry-slavesym entry))))
                    s))))

(defcommand "Apropos Next Symbol" (p)
  "Move to the next symbol."
  "Move to the next symbol."
  (apropos-move (or p 1)))

(defcommand "Apropos Previous Symbol" (p)
  "Move to the previous symbol."
  "Move to the previous symbol."
  (apropos-move (- (or p 1))))

(defun apropos-move (count)
  (let ((line (mark-line (current-point))))
    (dotimes (i (abs count))
      (loop (setf line (if (minusp count) (line-previous line) (line-next line)))
            (unless line (editor-error "No more symbols."))
            (when (and (getf (line-plist line) 'apropos-entry)
                       (plusp (line-length line))
                       (char/= (line-character line 0) #\Space))
              (return))))
    (move-to-position (current-point) 0 line)))

(defun refresh-apropos (buf entries &optional title)
  (setf *apropos-entries-end* (length entries))
  (setf *apropos-entries* (coerce entries 'vector))
  (with-writable-buffer (buf)
    (delete-region (buffer-region buf))
    (let ((point (buffer-point buf)))
      (when title
        (insert-string point (format nil "~A~%~%" title)))
      (dolist (entry entries)
        (flet ((out (control &rest args)
                 (let ((line (mark-line point)))
                   (insert-string point (apply #'format nil control args))
                   (setf (getf (line-plist line) 'apropos-entry) entry))))
          (out "~A~%" (apropos-entry-slavesym entry))
          (loop for (kind docstring) in (apropos-entry-kinds entry)
                do (out "  ~:(~A~)~@[: ~A~]~%" (substitute #\Space #\- (string kind))
                        (and docstring (first-line-of-string docstring))))
          (insert-string point (string #\Newline)))))))

(defun apropos-line-fonts (string)
  (cond ((zerop (length string)) '())
        ((char/= (char string 0) #\Space) (list (cons 0 '(:bold t))))
        (t (let ((colon (position #\: string)))
             (list* (cons 2 (if (search "Unbound" string :end2 (min 12 (length string))) 7 6))
                    (when colon (list (cons (1+ colon) 0))))))))

(defun apropos-highlight-line (line)
  (let ((old (getf (line-plist line) 'apropos-marks)))
    (unless (and old (eq (car old) (line-signature line)))
      (dolist (mark (cdr old))
        (hi::delete-font-mark mark))
      (setf (getf (line-plist line) 'apropos-marks)
            (cons (line-signature line)
                  (loop for (position . font) in (apropos-line-fonts (line-string line))
                        collect (hi::font-mark line position font)))))))

(define-mode-highlighter "Apropos" 'apropos-highlight-line)

(defun make-apropos-buffer (entries &optional title)
  (let ((buf (or *apropos-buffer*
                 (make-buffer "*Slave Apropos*" :modes '("Apropos")
                                                :delete-hook (list 'delete-apropos-buffers)))))
    (setf *apropos-buffer* buf)
    (refresh-apropos buf entries title)
    (let ((fields (buffer-modeline-fields *apropos-buffer*)))
      (unless (member :apropos-cmds fields :key #'modeline-field-name)
        (setf (cdr (last fields))
              (list (or (modeline-field :apropos-cmds)
                        (make-modeline-field
                         :name :apropos-cmds :width 18
                         :function
                         #'(lambda (buffer window)
                             (declare (ignore buffer window))
                             "  Type ? for help.")))))
        (setf (buffer-modeline-fields *apropos-buffer*) fields)))
    (change-to-buffer buf)
    (buffer-start (current-point))
    (when title (apropos-move 1))))

(defun first-line-of-string (str)
  (with-input-from-string (s str) (read-line s nil "")))

(defcommand "Apropos Help" (p)
  "Show this help."
  "Show this help."
  (declare (ignore p))
  (describe-mode-command nil "Apropos"))

(defcommand "Slave Apropos Ignoring Point"
            (p &optional (str
                          (heml-interface::prompt-for-string
                           :prompt "Apropos string: ")))
  "" ""
  (declare (ignore p))
  (slave-apropos str))

(defcommand "Slave Apropos" (p)
  "" ""
  (declare (ignore p))
  (let ((default (heml::symbol-string-at-point)))
    ;; Fixme: MARK-SYMBOL isn't very good, meaning that often we
    ;; will get random forms rather than a symbol.  Let's at least
    ;; catch the case where the result is more than a line long,
    ;; and give up.
    (when (find #\newline default)
      (setf default nil))
    (slave-apropos
     (heml-interface::prompt-for-string
      :prompt "Apropos string: "
      :default default))))

(defun slave-apropos (str)
  (heml::eval-in-slave `(%apropos ',str)))

(defun %apropos (str)
  (let ((data
         (mapcar (lambda (sym)
                   (cons (make-slave-symbol sym)
                         (conium:describe-symbol-for-emacs sym)))
                 (apropos-list str))))
    (heml::eval-in-master `(%apropos-results ',data ',str))))

(defun %apropos-results (data str)
  (let ((entries (mapcar #'parse-apropos-entry data)))
    (cond
     ((null data)
      (message "No apropos results for: ~A" str))
     (t
      (make-apropos-buffer
       (sort entries #'string<
             :key (lambda (entry)
                    (slave-symbol-name (apropos-entry-slavesym entry))))
       (format nil "Symbols matching ~S: ~D" str (length entries)))))))
