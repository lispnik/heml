;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; The Settings window: the settings worth a control, which change the
;;; editor at once and are written to the init file, between two lines that
;;; say so, so that they last.  Everything else is still the init file's,
;;; which a button opens.

(in-package :heml.cocoa)

(objc:define-objc-class settings-target ()
  ()
  (:objc-class-name "HemlSettingsTarget"))

(defparameter *setting-switches*
  '(("Close parentheses and quotes as they are typed" "Lisp Structural Editing")
    ("Keep parentheses balanced on Backspace and Delete" "Lisp Keep Parens Balanced")
    ("Indent the new line on Return in Lisp" "Lisp Indent on Return")
    ("Show what a call takes as it is typed" "Signature Help")
    ("Complete words as they are typed" "Complete as You Type")
    ("Start language servers" "Language Servers")
    ("Mark lines changed since the last commit" "Git Fringe")
    ("Reopen each project's files and windows" "Project Sessions")
    ("Indent with tabs" "Indent with Tabs"))
  "(TITLE VARIABLE): a check box for each Heml variable, true or false.")

(defvar *settings-window* nil)

(defvar *settings-controls* '()
  "(KEY . CONTROL) for each control of the settings window.")

(defun variable-mode (name)
  "Where the Heml variable NAME is set: (:MODE MODE) when Lisp mode has its
   own, else (:GLOBAL)."
  (let ((symbol (hi::string-to-variable name)))
    (if (hi::heml-bound-p symbol :mode "Lisp")
        (list :mode "Lisp")
        (list :global))))

(defun setting-value (name)
  (ignore-errors
   (apply #'hi::variable-value (hi::string-to-variable name) (variable-mode name))))

(defun set-setting (name value)
  "Set the Heml variable NAME to VALUE, on the editor's thread."
  (post-to-editor
   (list :call (lambda ()
                 (let ((symbol (hi::string-to-variable name)))
                   (if (eq (first (variable-mode name)) :mode)
                       (setf (hi::variable-value symbol :mode "Lisp") value)
                       (setf (hi::variable-value symbol :global) value)))))))


;;;; What is written to the init file.

(defparameter +settings-start+
  ";;; --- Settings, written by Heml's Settings window: change them there ---")

(defparameter +settings-end+ ";;; --- End of settings ---")

(defun settings-forms ()
  "The forms that put the settings as they are now, as text."
  (with-output-to-string (out)
    (dolist (switch *setting-switches*)
      (destructuring-bind (title name) switch
        (declare (ignore title))
        (format out "(setf (heml-interface:variable-value (heml-interface:string-to-variable ~S)~{ ~S~})~%      ~S)~%"
                name (variable-mode name) (and (setting-value name) t))))
    ;; The Mac's own, which a terminal Heml has no package for.
    (format out "(let ((package (find-package \"HEML.COCOA\")))~%  (when package~%")
    (format out "    (setf (symbol-value (find-symbol \"*CURSOR-STYLE*\" package)) ~S~%" *cursor-style*)
    (format out "          (symbol-value (find-symbol \"*CURSOR-BLINK*\" package)) ~S)))~%"
            *cursor-blink*)))

(defun write-settings ()
  "Put the settings in the init file, in place of what the window wrote
   before, or at its end."
  (let* ((file (hi::init-file))
         (text (if (probe-file file) (uiop:read-file-string file) ""))
         (start (search +settings-start+ text))
         (end (and start (search +settings-end+ text :start2 start)))
         (block (format nil "~A~%~A~A~%" +settings-start+ (settings-forms) +settings-end+)))
    (ensure-directories-exist file)
    (with-open-file (out file :direction :output :if-exists :supersede
                              :external-format :utf-8)
      (cond ((and start end)
             (write-string (subseq text 0 start) out)
             (write-string block out)
             (write-string (string-left-trim '(#\Newline)
                                             (subseq text (+ end (length +settings-end+))))
                           out))
            (t
             (write-string text out)
             (unless (or (zerop (length text))
                         (char= (char text (1- (length text))) #\Newline))
               (terpri out))
             (unless (zerop (length text)) (terpri out))
             (write-string block out))))))


;;;; The window.

(objc:define-objc-method ("hemlSwitchChanged:" :void)
    ((self settings-target) (sender objc:objc-object-pointer))
  (handler-case
      (let ((switch (nth (objc:invoke sender "tag") *setting-switches*)))
        (when switch
          (let ((value (= 1 (objc:invoke sender "state"))))
            (set-setting (second switch) value)
            ;; Written once the editor has it.
            (post-to-editor (list :call (lambda () (on-main-thread (write-settings))))))))
    (error (condition) (log-error "settings switch" condition))))

(objc:define-objc-method ("hemlCaretChanged:" :void)
    ((self settings-target) (sender objc:objc-object-pointer))
  (declare (ignore sender))
  (handler-case
      (progn
        (setf *cursor-style* (if (zerop (objc:invoke (cdr (assoc :caret *settings-controls*))
                                                     "indexOfSelectedItem"))
                                 :bar :block)
              *cursor-blink* (= 1 (objc:invoke (cdr (assoc :blink *settings-controls*)) "state"))
              *caret-shown* t)
        (request-redraw)
        (write-settings))
    (error (condition) (log-error "settings caret" condition))))

(objc:define-objc-method ("hemlChooseFont:" :void)
    ((self settings-target) (sender objc:objc-object-pointer))
  (declare (ignore sender))
  (show-font-panel))

(objc:define-objc-method ("hemlEditInitFile:" :void)
    ((self settings-target) (sender objc:objc-object-pointer))
  (declare (ignore sender))
  (open-init-file))

(defun font-description ()
  (format nil "~A, ~D pt" (or *font-name* "System monospaced") *font-size*))

(defun settings-row (label control)
  "A row: LABEL, right-aligned, beside CONTROL."
  (let ((row (objc:invoke "NSStackView" "stackViewWithViews:"
                          (objc:invoke "NSArray" "arrayWithObjects:count:"
                                       (let ((array (cffi:foreign-alloc :pointer :count 2)))
                                         (setf (cffi:mem-aref array :pointer 0)
                                               (objc:invoke "NSTextField" "labelWithString:" label)
                                               (cffi:mem-aref array :pointer 1) control)
                                         array)
                                       2))))
    (objc:invoke row "setOrientation:" 0)
    (objc:invoke row "setSpacing:" 8d0)
    row))

(defun make-settings-window ()
  (let* ((target (objc:objc-object-pointer (make-instance 'settings-target)))
         (window (objc:invoke (objc:invoke "NSWindow" "alloc")
                              "initWithContentRect:styleMask:backing:defer:"
                              (vector 0d0 0d0 520d0 420d0) (logior 1 2) ; titled, closable
                              +backing-store-buffered+ nil))
         (stack (objc:invoke (objc:invoke "NSStackView" "alloc") "initWithFrame:"
                             (vector 0d0 0d0 520d0 420d0)))
         (controls '()))
    (objc:invoke window "setReleasedWhenClosed:" nil)
    (objc:invoke window "setTitle:" "Settings")
    (objc:invoke stack "setOrientation:" 1)                ; one above another
    (objc:invoke stack "setAlignment:" 1)                  ; leading edges
    (objc:invoke stack "setSpacing:" 8d0)
    (objc:invoke stack "setEdgeInsets:" (vector 20d0 24d0 20d0 24d0))
    ;; The font.
    (let ((label (objc:invoke "NSTextField" "labelWithString:" (font-description)))
          (button (objc:invoke "NSButton" "buttonWithTitle:target:action:" "Choose…" target
                               (objc:coerce-to-selector "hemlChooseFont:"))))
      (push (cons :font label) controls)
      (objc:invoke stack "addArrangedSubview:" (settings-row "Font:" label))
      (objc:invoke stack "addArrangedSubview:" button))
    ;; The caret.
    (let ((popup (objc:invoke (objc:invoke "NSPopUpButton" "alloc") "initWithFrame:pullsDown:"
                              (vector 0d0 0d0 120d0 24d0) nil))
          (blink (objc:invoke "NSButton" "checkboxWithTitle:target:action:" "Blink" target
                              (objc:coerce-to-selector "hemlCaretChanged:"))))
      (objc:invoke popup "addItemWithTitle:" "Bar")
      (objc:invoke popup "addItemWithTitle:" "Block")
      (objc:invoke popup "selectItemAtIndex:" (if (eq *cursor-style* :bar) 0 1))
      (objc:invoke popup "setTarget:" target)
      (objc:invoke popup "setAction:" (objc:coerce-to-selector "hemlCaretChanged:"))
      (objc:invoke blink "setState:" (if *cursor-blink* 1 0))
      (push (cons :caret popup) controls)
      (push (cons :blink blink) controls)
      (objc:invoke stack "addArrangedSubview:" (settings-row "Caret:" popup))
      (objc:invoke stack "addArrangedSubview:" blink))
    ;; A check box for each switch.
    (loop for (title name) in *setting-switches*
          for index from 0
          do (let ((box (objc:invoke "NSButton" "checkboxWithTitle:target:action:" title target
                                     (objc:coerce-to-selector "hemlSwitchChanged:"))))
               (objc:invoke box "setTag:" index)
               (objc:invoke box "setState:" (if (setting-value name) 1 0))
               (push (cons name box) controls)
               (objc:invoke stack "addArrangedSubview:" box)))
    ;; Everything else.
    (objc:invoke stack "addArrangedSubview:"
                 (objc:invoke "NSButton" "buttonWithTitle:target:action:" "Edit init.lisp…" target
                              (objc:coerce-to-selector "hemlEditInitFile:")))
    (objc:invoke window "setContentView:" stack)
    (objc:invoke window "center")
    (setf *settings-controls* controls
          *settings-window* window)))

(defun refresh-settings-window ()
  "The controls as the settings are now."
  (loop for (key . control) in *settings-controls*
        do (cond ((eq key :font)
                  (objc:invoke control "setStringValue:" (font-description)))
                 ((eq key :caret)
                  (objc:invoke control "selectItemAtIndex:" (if (eq *cursor-style* :bar) 0 1)))
                 ((eq key :blink)
                  (objc:invoke control "setState:" (if *cursor-blink* 1 0)))
                 ((stringp key)
                  (objc:invoke control "setState:" (if (setting-value key) 1 0))))))

(defun open-settings ()
  "The Settings window, made the first time."
  (unless *settings-window*
    (make-settings-window))
  (refresh-settings-window)
  (objc:invoke *settings-window* "makeKeyAndOrderFront:" (cffi:null-pointer)))
