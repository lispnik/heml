;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; The find bar, as a Mac text editor has one: a search field under the
;;; title bar, ⌘F to it, Return and ⌘G for the next match, Shift-Return and
;;; ⇧⌘G for the one before, ⌘E to find what is selected, and every match in
;;; sight highlighted.  Heml's own searches, C-s and C-r, are as they were.
;;;
;;; The editor finds: the bar posts the commands below, which move point to
;;; a match and count them, and each frame carries the string, the matches'
;;; count and which of them point is at (FIND-DESCRIPTOR).  The main thread
;;; highlights the matches it sees in the rows of the window with the caret,
;;; so they show in every mode, coloured or not.

(in-package :heml.cocoa)

;;;; The editor's side.

(hi:defhvar "Find Bar Case"
  "Whether the find bar's search minds case: :IGNORE, never; :MATCH,
   always; :SMART, only when what is looked for has a capital in it."
  :value :smart)

(defun find-case-sensitive-p (string)
  "Whether looking for STRING minds case, as \"Find Bar Case\" says."
  (case (hi:variable-value 'heml::find-bar-case)
    (:match t)
    (:ignore nil)
    (t (some #'upper-case-p string))))

(defvar *find-string* nil
  "What the find bar looks for while it is open, or NIL.")

(defvar *find-last-string* ""
  "What was looked for last, for ⌘G with the bar closed.")

(defvar *find-origin* nil
  "Point when the bar was opened: typing finds from there.")

(defvar *find-cache* nil
  "(BUFFER SIGNATURE STRING . MATCHES), the last buffer's matches.")

(defun find-matches (buffer string)
  "Where STRING is in BUFFER, minding case as \"Find Bar Case\" says, in
   order: a vector of (INDEX LINE CHARPOS), INDEX the line's place in the
   buffer."
  (let* ((signature (hi:buffer-signature buffer))
         (test (if (find-case-sensitive-p string) #'char= #'char-equal))
         (key (list string test)))
    (if (and *find-cache*
             (eq (first *find-cache*) buffer)
             (eql (second *find-cache*) signature)
             (equal (third *find-cache*) key))
        (cdddr *find-cache*)
        (let ((matches (make-array 16 :adjustable t :fill-pointer 0)))
          (when (plusp (length string))
            (loop for line = (hi:mark-line (hi:buffer-start-mark buffer)) then (hi:line-next line)
                  for index from 0
                  while line
                  do (loop with text = (hi:line-string line)
                           for start = (search string text :test test)
                             then (search string text :test test :start2 (1+ start))
                           while start
                           do (vector-push-extend (list index line start) matches))))
          (setf *find-cache* (list* buffer signature key matches))
          matches))))

(defun mark-place (mark)
  "MARK as (INDEX CHARPOS), INDEX its line's place in its buffer."
  (let ((target (hi:mark-line mark)))
    (list (loop for line = (hi:mark-line (hi:buffer-start-mark (hi:line-buffer target)))
                  then (hi:line-next line)
                for index from 0
                until (or (null line) (eq line target))
                finally (return index))
          (hi:mark-charpos mark))))

(defun place< (a b)
  (or (< (first a) (first b))
      (and (= (first a) (first b)) (< (second a) (second b)))))

(defun find-from (place matches direction &key inclusive)
  "The match after PLACE (before it, when DIRECTION is :BACKWARD), wrapping
   around the buffer; at PLACE too when INCLUSIVE."
  (when (plusp (length matches))
    (flet ((at (match) (list (first match) (third match))))
      (if (eq direction :backward)
          (or (find-if (lambda (match) (place< (at match) place)) matches :from-end t)
              (aref matches (1- (length matches))))
          (or (find-if (lambda (match)
                         (or (place< place (at match))
                             (and inclusive (equal place (at match)))))
                       matches)
              (aref matches 0))))))

(defun go-to-match (match)
  (hi:move-to-position (hi:current-point) (third match) (second match)))

(defun find-and-go (string from direction &key inclusive)
  "Look for STRING from the mark FROM, and go to what is found."
  (setf *find-last-string* string)
  (let* ((matches (find-matches (hi:current-buffer) string))
         (match (find-from (mark-place from) matches direction :inclusive inclusive)))
    (cond (match (go-to-match match))
          ((plusp (length string)) (hi:beep)))))

(defun find-descriptor ()
  "The find bar's state, for the main thread: (STRING INDEX COUNT), INDEX
   the place among the matches of the one point is at, or NIL; NIL when the
   bar is closed."
  (when *find-string*
    (let* ((matches (find-matches (hi:current-buffer) *find-string*))
           (point (hi:current-point))
           (line (hi:mark-line point))
           (charpos (hi:mark-charpos point)))
      (list *find-string*
            (position-if (lambda (match) (and (eq (second match) line) (= (third match) charpos)))
                         matches)
            (length matches)
            (find-case-sensitive-p *find-string*)))))

(hi:defcommand "Find Bar Start" (p &optional (string ""))
  "The find bar opened: what is typed in it is looked for from here."
  "The find bar opened."
  (declare (ignore p))
  (when *find-origin* (hi:delete-mark *find-origin*))
  (setf *find-origin* (hi:copy-mark (hi:current-point) :temporary)
        *find-string* string))

(hi:defcommand "Find Bar Search" (p &optional (string ""))
  "Look for STRING from where the find bar was opened, as it is typed."
  "Look for the find bar's string."
  (declare (ignore p))
  (unless (and *find-origin* (eq (hi:line-buffer (hi:mark-line *find-origin*))
                                 (hi:current-buffer)))
    (find-bar-start-command nil string))
  (setf *find-string* string)
  (if (plusp (length string))
      (find-and-go string *find-origin* :forward :inclusive t)
      (hi:move-mark (hi:current-point) *find-origin*)))

(hi:defcommand "Find Bar Next" (p)
  "Go to the next match of what the find bar looks for."
  "Go to the next match."
  (declare (ignore p))
  (find-and-go (or *find-string* *find-last-string*) (hi:current-point) :forward))

(hi:defcommand "Find Bar Previous" (p)
  "Go to the match before point of what the find bar looks for."
  "Go to the previous match."
  (declare (ignore p))
  (find-and-go (or *find-string* *find-last-string*) (hi:current-point) :backward))

(hi:defcommand "Find Bar Done" (p)
  "The find bar closed: point stays at the match."
  "The find bar closed."
  (declare (ignore p))
  (when *find-origin*
    (hi:delete-mark *find-origin*)
    (setf *find-origin* nil))
  (setf *find-string* nil))

(hi:defcommand "Find Bar Use Selection" (p)
  "Have the find bar look for the selection, or the word at point."
  "Find the selection."
  (declare (ignore p))
  (let ((string (if (heml::region-active-p)
                    (hi:region-to-string (heml::current-region))
                    (let* ((point (hi:current-point))
                           (text (hi:line-string (hi:mark-line point)))
                           (at (hi:mark-charpos point))
                           (start (or (position-if-not #'alphanumericp text :end at :from-end t) -1))
                           (end (or (position-if-not #'alphanumericp text :start at) (length text))))
                      (subseq text (1+ start) end)))))
    (when (plusp (length string))
      (setf *find-last-string* string)
      (when *find-string*
        (setf *find-string* string))
      (on-main-thread (set-find-field string)))))


;;;; The bar.

(objc:define-objc-class find-target ()
  ()
  (:objc-class-name "HemlFindTarget"))

(defvar *find-bar* nil
  "(CONTROLLER FIELD LABEL TARGET), made the first time the bar is shown.")

(defvar *find-bar-shown* nil)

(defun find-field () (second *find-bar*))

(defun field-string ()
  (objc:ns-string-to-string (objc:invoke (find-field) "stringValue")))

(objc:define-objc-method ("hemlFindChanged:" :void)
    ((self find-target) (sender objc:objc-object-pointer))
  (declare (ignore sender))
  (handler-case (post-to-editor (list :command "Find Bar Search" (field-string)))
    (error (condition) (log-error "find changed" condition))))

(objc:define-objc-method ("hemlFindNext:" :void)
    ((self find-target) (sender objc:objc-object-pointer))
  (declare (ignore sender))
  (find-bar-next))

(objc:define-objc-method ("hemlFindPrevious:" :void)
    ((self find-target) (sender objc:objc-object-pointer))
  (declare (ignore sender))
  (find-bar-previous))

(objc:define-objc-method ("hemlFindDone:" :void)
    ((self find-target) (sender objc:objc-object-pointer))
  (declare (ignore sender))
  (hide-find-bar))

(defconstant +shift-key-mask+ (ash 1 17))

;;; Return and Escape in the field: the next match, or with Shift the one
;;; before, and closing the bar.
(objc:define-objc-method ("control:textView:doCommandBySelector:" objc:objc-bool)
    ((self find-target) (control objc:objc-object-pointer) (text-view objc:objc-object-pointer)
     (selector :pointer))
  (declare (ignore control text-view))
  (handler-case
      (let ((name (cffi:foreign-funcall "sel_getName" :pointer selector :string)))
        (cond ((string= name "insertNewline:")
               (if (logtest +shift-key-mask+
                            (objc:invoke (objc:invoke (objc:invoke "NSApplication" "sharedApplication")
                                                      "currentEvent")
                                         "modifierFlags"))
                   (find-bar-previous)
                   (find-bar-next))
               t)
              ((string= name "cancelOperation:")
               (hide-find-bar)
               t)
              (t nil)))
    (error (condition) (log-error "find key" condition) nil)))

(defun symbol-button (symbol description target action)
  (let ((button (objc:invoke "NSButton" "buttonWithImage:target:action:"
                             (objc:invoke "NSImage" "imageWithSystemSymbolName:accessibilityDescription:"
                                          symbol description)
                             target (objc:coerce-to-selector action))))
    (objc:invoke button "setBordered:" nil)
    (objc:invoke button "setToolTip:" description)
    button))

(defun make-find-bar (display)
  (let* ((controller (objc:alloc-init-object "NSTitlebarAccessoryViewController"))
         (width (aref (objc:invoke (display-window display) "frame") 2))
         (container (objc:invoke (objc:invoke "NSView" "alloc") "initWithFrame:"
                                 (vector 0d0 0d0 (df width) 32d0)))
         (stack (objc:invoke (objc:invoke "NSStackView" "alloc") "initWithFrame:"
                             (vector 0d0 0d0 (df width) 32d0)))
         (target (objc:objc-object-pointer (make-instance 'find-target)))
         (field (objc:invoke (objc:invoke "NSSearchField" "alloc") "initWithFrame:"
                             (vector 0d0 0d0 280d0 22d0)))
         (label (objc:invoke "NSTextField" "labelWithString:" ""))
         (done (objc:invoke "NSButton" "buttonWithTitle:target:action:" "Done" target
                            (objc:coerce-to-selector "hemlFindDone:"))))
    (objc:invoke field "setPlaceholderString:" "Find")
    (objc:invoke field "setSendsSearchStringImmediately:" t)
    (objc:invoke field "setTarget:" target)
    (objc:invoke field "setAction:" (objc:coerce-to-selector "hemlFindChanged:"))
    (objc:invoke field "setDelegate:" target)
    (objc:invoke label "setTextColor:" (objc:invoke "NSColor" "secondaryLabelColor"))
    (objc:invoke done "setControlSize:" 1)                ; small
    (objc:invoke stack "setOrientation:" 0)
    (objc:invoke stack "setSpacing:" 6d0)
    (objc:invoke stack "setEdgeInsets:" (vector 4d0 10d0 4d0 10d0))
    (objc:invoke stack "setAlignment:" 10)                ; centred on Y
    (objc:invoke stack "addArrangedSubview:" field)
    (objc:invoke stack "addArrangedSubview:"
                 (symbol-button "chevron.left" "Previous match" target "hemlFindPrevious:"))
    (objc:invoke stack "addArrangedSubview:"
                 (symbol-button "chevron.right" "Next match" target "hemlFindNext:"))
    (objc:invoke stack "addArrangedSubview:" label)
    ;; Done at the far right.
    (objc:invoke stack "addView:inGravity:" done 3)
    (objc:invoke stack "setClippingResistancePriority:forOrientation:" 1f0 0)
    (objc:invoke stack "setTranslatesAutoresizingMaskIntoConstraints:" t)
    (objc:invoke stack "setAutoresizingMask:" 18)
    (objc:invoke container "setAutoresizingMask:" 2)
    (objc:invoke container "addSubview:" stack)
    (objc:invoke controller "setView:" container)
    (objc:invoke controller "setLayoutAttribute:" 4)      ; under the title bar
    (objc:invoke controller "setHidden:" t)
    (objc:invoke (display-window display) "addTitlebarAccessoryViewController:" controller)
    (setf *find-bar* (list controller field label target))))

(defun ensure-find-bar ()
  (or *find-bar*
      (let ((display *display*))
        (and display (make-find-bar display)))))

(defun show-find-bar ()
  "⌘F: the bar shown, and what it holds chosen, to type over."
  (when (ensure-find-bar)
    (objc:invoke (first *find-bar*) "setHidden:" nil)
    (setf *find-bar-shown* t)
    (objc:invoke (display-window *display*) "makeFirstResponder:" (find-field))
    (objc:invoke (find-field) "selectText:" (cffi:null-pointer))
    (post-to-editor (list :command "Find Bar Start" (field-string)))))

(defun hide-find-bar ()
  (when *find-bar*
    (objc:invoke (first *find-bar*) "setHidden:" t)
    (setf *find-bar-shown* nil)
    (objc:invoke (display-window *display*) "makeFirstResponder:" (display-view *display*))
    (post-to-editor (list :command "Find Bar Done"))))

(defun find-bar-next ()
  "⌘G: the next match, or the bar when there is nothing to look for."
  (if (and (not *find-bar-shown*) (zerop (length (if *find-bar* (field-string) ""))))
      (show-find-bar)
      (post-to-editor (list :command "Find Bar Next"))))

(defun find-bar-previous ()
  (if (and (not *find-bar-shown*) (zerop (length (if *find-bar* (field-string) ""))))
      (show-find-bar)
      (post-to-editor (list :command "Find Bar Previous"))))

(defun set-find-field (string)
  "⌘E: STRING in the field, for ⌘G, without showing the bar."
  (when (ensure-find-bar)
    (objc:invoke (find-field) "setStringValue:" string)))

(defun show-find-status (find)
  "The count beside the field, from the editor's (STRING INDEX COUNT)."
  (when *find-bar*
    (objc:invoke (third *find-bar*) "setStringValue:"
                 (if (null find)
                     ""
                     (destructuring-bind (string index count &optional case) find
                       (declare (ignore case))
                       (cond ((zerop (length string)) "")
                             ((zerop count) "Not found")
                             (index (format nil "~D of ~D" (1+ index) count))
                             (t (format nil "~D match~:P" count))))))))

;;; Every match in sight, in the window with the caret: found in the rows
;;; shown, so that they are where the text is drawn, whatever the mode.  The
;;; one point is at is the stronger.  Drawn before the text, under it.
;;;
(defun draw-find-matches (display screen)
  (let* ((find (screen-shown-find screen))
         (string (first find))
         (test (if (fourth find) #'char= #'char-equal))
         (x (screen-shown-cursor-x screen))
         (y (screen-shown-cursor-y screen))
         (rows (screen-shown-rows screen)))
    (when (and string (plusp (length string)) x y)
      (let ((entry (find-if (lambda (entry)
                              (destructuring-bind (column line width height &rest more) entry
                                (declare (ignore more))
                                (and (<= line y (+ line height -1)) (<= column x (+ column width)))))
                            (screen-shown-scrolls screen))))
        (when entry
          (destructuring-bind (column line width height position size
                               &optional at-start at-end above below (fringe 0) &rest more)
              entry
            (declare (ignore position size at-start at-end above below more))
            (let ((first (+ column fringe))
                  (last (+ column width)))
              (loop for row from line below (min (length rows) (+ line height))
                    for text = (row-text (svref rows row))
                    do (loop with end = (min last (length text))
                             for start = (and (< first end)
                                              (search string text :test test
                                                                  :start2 first :end2 end))
                               then (search string text :test test
                                                        :start2 (1+ start) :end2 end)
                             while start
                             do (fill-rect (objc:invoke (ns-color display "systemYellowColor")
                                                        "colorWithAlphaComponent:"
                                                        (if (and (= row y) (= start x)) 0.75d0 0.3d0))
                                           (cell-x display start) (cell-y display row)
                                           (* (length string) (display-char-width display))
                                           (display-char-height display)))))))))))

(heml-interface:add-menu-item "Edit" '("Find…" (:call show-find-bar) :key "f")
                              :before "Find Backward…")
(heml-interface:add-menu-item "Edit" '("Find Next" (:call find-bar-next) :key "g")
                              :after "Find…")
(heml-interface:add-menu-item "Edit" '("Find Previous" (:call find-bar-previous)
                                       :key "g" :modifiers (:shift))
                              :after "Find Next")
(heml-interface:add-menu-item "Edit" '("Use Selection for Find" (:command "Find Bar Use Selection")
                                       :key "e")
                              :after "Find Previous")
(heml-interface:remove-menu-item "Edit" "Find Backward…")
