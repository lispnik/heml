;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;; This file contains slave-list-related code.
;;;

(in-package :heml)



;;;; Representation of existing slaves.

(defvar *slave-list-items* nil)
(defvar *slave-list-items-end* nil)
;;;

(defstruct slave-list-item
  (marked nil)
  name
  info)


;;; This is the slave-list buffer if it exists.
;;;
(defvar *slave-list-buffer* nil)

;;; This is the cleanup method for deleting *slave-list-buffer*.
;;;
(defun delete-slave-list-buffers (buffer)
  (when (eq buffer *slave-list-buffer*)
    (setf *slave-list-buffer* nil)
    (setf *slave-list-items* nil)))


;;;; Commands.

(defmode "Slave-List" :major-p t
  :documentation
  "The slave Lisps, one a line: the current one marked, then its name, what
   it is doing, and its Lisp.  Return makes a slave current, space goes to
   its buffer, b to its background buffer, and g lists them again.")

;;; Each slave's line keeps its item in its plist, below a header.
;;;
(defun slave-item-at-point ()
  (or (getf (line-plist (mark-line (current-point))) 'slave-item)
      (editor-error "No slave on this line.")))

(defcommand "Mark Slave" (p)
  "" ""
  (declare (ignore p))
  (let* ((point (current-point))
         (item-at-point (slave-item-at-point)))
    (with-writable-buffer (*slave-list-buffer*)
      (setf (slave-list-item-marked item-at-point) t)
      (with-mark ((point point))
        (character-offset (line-start point) 1)
        (setf (next-character point) #\*))
      (line-offset point 1))))

(defcommand "Unmark Slave" (p)
  "" ""
  (declare (ignore p))
  (with-writable-buffer (*slave-list-buffer*)
    (setf (slave-list-item-marked (slave-item-at-point)) nil)
    (with-mark ((point (current-point)))
      (character-offset (line-start point) 1)
      (setf (next-character point) #\space))
    (line-offset (current-point) 1)))

(defcommand "Quit Slave List" (p)
  "" ""
  (declare (ignore p))
  (when *slave-list-buffer* (delete-buffer-if-possible *slave-list-buffer*)))

(defcommand "Goto Slave" (p)
  "" ""
  (let ((info (slave-list-item-info (slave-item-at-point))))
    (change-to-buffer
     (or (server-info-slave-buffer info)
         (editor-error "Slave has no buffer")))
    (unless (or p (not (prompt-for-y-or-n :prompt "Set as current slave? "
                                          :default t
                                          :must-exist t
                                          :default-string "Y")))
      (setf (variable-value 'current-eval-server :global) info))))

(defcommand "Activate Slave" (p)
  "" ""
  (declare (ignore p))
  (let ((info (slave-list-item-info (slave-item-at-point))))
    (setf (variable-value 'current-eval-server :global) info)
    (refresh-slave-list *slave-list-buffer*)
    (message "~A is the current slave." (server-info-name info))))

(defcommand "Goto Slave Background" (p)
  "Go to the background buffer of the slave on this line, where what it does
   for the editor -- compiling, evaluating -- is shown."
  "Go to the slave's background buffer."
  (declare (ignore p))
  (change-to-buffer
   (or (server-info-background-buffer (slave-list-item-info (slave-item-at-point)))
       (editor-error "Slave has no background buffer"))))

(defun list-slave-items ()
  (hi::map-string-table 'list
                        (lambda (info)
                          (make-slave-list-item
                           :name (server-info-name info)
                           :marked nil
                           :info info))
                        *server-names*))

(defun refresh-slave-list (buf)
  (with-writable-buffer (buf)
    (delete-region (buffer-region buf))
    (let ((items (coerce (sort (list-slave-items) #'string< :key #'slave-list-item-name)
                         'vector))
          (point (buffer-point buf)))
      (setf *slave-list-items-end* (length items))
      (setf *slave-list-items* items)
      (insert-string point (format nil "  ~A~30T~A~42T~A~%" "Slave" "State" "Lisp"))
      (if (zerop (length items))
          (insert-string point (format nil "  (no slaves: M-x Start Slave Thread starts one)~%"))
          (iter:iter (iter:for c in-vector items)
                     (let ((line (mark-line point)))
                       (insert-string point (with-output-to-string (s)
                                              (slave-list-write-line c s)))
                       (setf (getf (line-plist line) 'slave-item) c))))
      (buffer-start (buffer-point buf))
      (when (plusp (length items))
        (line-offset (buffer-point buf) 1)))))

(defun slave-state (info)
  (cond ((null (server-info-wire info)) "disconnected")
        ((server-info-notes info)
         (format nil "busy (~D)" (length (server-info-notes info))))
        (t "idle")))

(defun slave-list-line-fonts (string)
  (cond ((zerop (length string)) '())
        ((string= (string-trim " " string) "") '())
        ((search "Slave" string :end2 (min 8 (length string)))
         (list (cons 0 '(:bold t))))
        ((search "(no slaves" string) (list (cons 0 7)))
        (t (let ((fonts (list (cons 2 (if (char= (char string 0) #\>) '(:bold t :fg 2) '(:bold t)))
                              (cons (min (length string)
                                         (or (position #\Space string :start 2) (length string)))
                                    0))))
             (loop for (word font) in '(("busy" 3) ("idle" 2) ("disconnected" 1))
                   for at = (search word string)
                   when at do (setf fonts (append fonts (list (cons at font)
                                                              (cons (+ at (length word)) 0)))))
             fonts))))

(defun slave-list-highlight-line (line)
  (let ((old (getf (line-plist line) 'slave-list-marks)))
    (unless (and old (eq (car old) (line-signature line)))
      (dolist (mark (cdr old))
        (hi::delete-font-mark mark))
      (setf (getf (line-plist line) 'slave-list-marks)
            (cons (line-signature line)
                  (loop for (position . font) in (slave-list-line-fonts (line-string line))
                        collect (hi::font-mark line position font)))))))

(define-mode-highlighter "Slave-List" 'slave-list-highlight-line)

(defcommand "List Slaves" (p)
  "" ""
  (declare (ignore p))
  (let ((buf (or *slave-list-buffer*
                 (make-buffer "Slave-List" :modes '("Slave-List")
                              :delete-hook (list #'delete-slave-list-buffers)))))
    (unless *slave-list-buffer*
      (setf *slave-list-buffer* buf)
      (refresh-slave-list buf)
      (let ((fields (buffer-modeline-fields *slave-list-buffer*)))
        (setf (cdr (last fields))
              (list (or (modeline-field :slave-list-cmds)
                        (make-modeline-field
                         :name :slave-list-cmds :width 18
                         :function
                         #'(lambda (buffer window)
                             (declare (ignore buffer window))
                             "  Type ? for help.")))))
        (setf (buffer-modeline-fields *slave-list-buffer*) fields))
      (buffer-start (buffer-point buf)))
    (change-to-buffer buf)))

(defcommand "Refresh Slave List" (p)
  "" ""
  (declare (ignore p))
  (when *slave-list-buffer*
    (refresh-slave-list *slave-list-buffer*)))

(defun slave-list-write-line (item s)
  (let ((info (slave-list-item-info item)))
    (format s "~:[ ~;>~]~:[ ~;*~]~A~30T~A~42T~A ~A~%"
            (eq info (value current-eval-server))
            (slave-list-item-marked item)
            (slave-list-item-name item)
            (slave-state info)
            (server-info-implementation-type info)
            (server-info-implementation-version info))))

(defcommand "Slave-List Help" (p)
  "Show this help."
  "Show this help."
  (declare (ignore p))
  (describe-mode-command nil "Slave-List"))

(bind-key "Mark Slave" #k"m" :mode "Slave-List")
(bind-key "Unmark Slave" #k"u" :mode "Slave-List")
(bind-key "Quit Slave List" #k"q" :mode "Slave-List")
(bind-key "Goto Slave" #k"space" :mode "Slave-List")
(bind-key "Activate Slave" #k"return" :mode "Slave-List")
(bind-key "Refresh Slave List" #k"g" :mode "Slave-List")
(bind-key "Goto Slave Background" #k"b" :mode "Slave-List")
(bind-key "Next Line" #k"n" :mode "Slave-List")
(bind-key "Previous Line" #k"p" :mode "Slave-List")
(bind-key "Slave-List Help" #k"?" :mode "Slave-List")

