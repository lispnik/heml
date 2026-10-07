;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; The window's chrome, as a Mac editor has it: a sidebar of the project's
;;; files beside the editor, and a strip of tabs, one for each file open,
;;; under the title bar.  Both are AppKit's own views, on the main thread;
;;; what they show comes with each frame (the title's ROOT, SCREEN-TABS), and
;;; what is done in them is posted to the editor as a menu's command is.

(in-package :heml.cocoa)


;;;; The editor's grid follows its view.

;;; With a sidebar beside it and tabs above it, the view changes size when
;;; the window does not: the grid is fitted to the view whenever its frame
;;; changes.
;;;
(objc:define-objc-method ("hemlFrameChanged:" :void)
    ((self heml-view) (notification objc:objc-object-pointer))
  (declare (ignore notification))
  (handler-case
      (when *display*
        (multiple-value-bind (columns lines) (grid-size *display*)
          (post-to-editor (list :resize columns lines))))
    (error (condition) (log-error "hemlFrameChanged:" condition))))

(defun follow-view-frame (display)
  (let ((view (display-view display)))
    (objc:invoke view "setPostsFrameChangedNotifications:" t)
    (objc:invoke (objc:invoke "NSNotificationCenter" "defaultCenter")
                 "addObserver:selector:name:object:"
                 view (objc:coerce-to-selector "hemlFrameChanged:")
                 "NSViewFrameDidChangeNotification" view)))


;;;; The sidebar.

(objc:define-objc-class sidebar-source ()
  ()
  (:objc-class-name "HemlSidebarSource")
  (:objc-protocols "NSOutlineViewDataSource" "NSOutlineViewDelegate"))

(defvar *sidebar-shown* nil
  "Whether the sidebar of the project's files is shown.")

(defparameter *sidebar-width* 220
  "The sidebar's width when it is first shown, in points.")

(defvar *sidebar* nil
  "(SPLIT EFFECT OUTLINE SOURCE) once the sidebar has been made.")

(defvar *sidebar-root* nil
  "The directory the sidebar shows, a namestring.")

(defvar *sidebar-items* (make-hash-table :test 'equal)
  "Each path the sidebar has shown to its NSString, kept, so that the
   outline is given the same object for it each time it asks.")

(defvar *sidebar-paths* (make-hash-table)
  "Each of those NSStrings' addresses to its path.")

(defvar *sidebar-children* (make-hash-table :test 'equal)
  "Each directory listed to its entries, as paths.")

(defvar *sidebar-git* (make-hash-table :test 'equal)
  "Each file Git says something of to :MODIFIED or :NEW.")

(defparameter *sidebar-ignored* '(".git" ".DS_Store" ".hg" ".svn")
  "Names the sidebar does not list.")

(defun sidebar-item (path)
  (or (gethash path *sidebar-items*)
      (let ((string (objc:retain (objc:string-to-ns-string path))))
        (setf (gethash (cffi:pointer-address string) *sidebar-paths*) path
              (gethash path *sidebar-items*) string))))

(defun item-path (item)
  (if (cffi:null-pointer-p item)
      *sidebar-root*
      (gethash (cffi:pointer-address item) *sidebar-paths*)))

(defun directory-path-p (path)
  (let ((length (length path)))
    (and (plusp length) (char= (char path (1- length)) #\/))))

(defun sidebar-entries (directory)
  "DIRECTORY's entries as paths: its directories, then its files, each by
   name, but for those *SIDEBAR-IGNORED* names."
  (or (gethash directory *sidebar-children*)
      (setf (gethash directory *sidebar-children*)
            (flet ((keep (paths)
                     (sort (remove-if (lambda (path)
                                        (member (car (last (pathname-directory path)))
                                                *sidebar-ignored* :test #'equal))
                                      (mapcar #'namestring paths))
                           #'string-lessp)))
              (ignore-errors
               (append (keep (uiop:subdirectories directory))
                       (sort (remove-if (lambda (path)
                                          (member (file-namestring path) *sidebar-ignored*
                                                  :test #'equal))
                                        (mapcar #'namestring (uiop:directory-files directory)))
                             #'string-lessp)))))))

(defun read-git-statuses (root)
  "What Git says of ROOT's files, into *SIDEBAR-GIT*."
  (clrhash *sidebar-git*)
  (let ((output (ignore-errors
                 (uiop:run-program (list "git" "-C" root "status" "--porcelain=v1" "-z"
                                         "--untracked-files=all")
                                   :output :string :ignore-error-status t))))
    (when output
      (dolist (entry (uiop:split-string output :separator (string (code-char 0))))
        (when (> (length entry) 3)
          (let ((path (concatenate 'string root (subseq entry 3))))
            (setf (gethash path *sidebar-git*)
                  (if (string= (subseq entry 0 2) "??") :new :modified))))))))

(objc:define-objc-method ("outlineView:numberOfChildrenOfItem:" :long)
    ((self sidebar-source) (outline objc:objc-object-pointer) (item objc:objc-object-pointer))
  (declare (ignore outline))
  (handler-case
      (let ((path (item-path item)))
        (if (and path (directory-path-p path)) (length (sidebar-entries path)) 0))
    (error (condition) (log-error "sidebar children" condition) 0)))

(objc:define-objc-method ("outlineView:child:ofItem:" objc:objc-object-pointer)
    ((self sidebar-source) (outline objc:objc-object-pointer) (index :long)
     (item objc:objc-object-pointer))
  (declare (ignore outline))
  (handler-case
      (sidebar-item (nth index (sidebar-entries (item-path item))))
    (error (condition) (log-error "sidebar child" condition) (cffi:null-pointer))))

(objc:define-objc-method ("outlineView:isItemExpandable:" objc:objc-bool)
    ((self sidebar-source) (outline objc:objc-object-pointer) (item objc:objc-object-pointer))
  (declare (ignore outline))
  (let ((path (item-path item)))
    (and path (directory-path-p path))))

(defun entry-name (path)
  (if (directory-path-p path)
      (car (last (pathname-directory path)))
      (file-namestring path)))

(objc:define-objc-method ("outlineView:viewForTableColumn:item:" objc:objc-object-pointer)
    ((self sidebar-source) (outline objc:objc-object-pointer) (column objc:objc-object-pointer)
     (item objc:objc-object-pointer))
  (declare (ignore outline column))
  (handler-case
      (let* ((path (item-path item))
             (cell (objc:invoke (objc:invoke "NSTableCellView" "alloc") "initWithFrame:"
                                (vector 0d0 0d0 200d0 20d0)))
             (image (objc:invoke (objc:invoke "NSImageView" "alloc") "initWithFrame:"
                                 (vector 2d0 2d0 16d0 16d0)))
             (text (objc:invoke "NSTextField" "labelWithString:" (entry-name path)))
             (status (gethash (string-right-trim "/" path) *sidebar-git*)))
        (objc:invoke image "setImage:"
                     (objc:invoke (objc:invoke "NSWorkspace" "sharedWorkspace") "iconForFile:" path))
        (objc:invoke text "setFrame:" (vector 22d0 1d0 170d0 18d0))
        (objc:invoke text "setAutoresizingMask:" 2)
        (objc:invoke text "setLineBreakMode:" 4)
        (when status
          (objc:invoke text "setTextColor:"
                       (objc:invoke "NSColor" (if (eq status :new) "systemGreenColor" "systemOrangeColor"))))
        (objc:invoke cell "addSubview:" image)
        (objc:invoke cell "addSubview:" text)
        (objc:invoke cell "setImageView:" image)
        (objc:invoke cell "setTextField:" text)
        (objc:invoke image "release")
        (objc:invoke cell "autorelease"))
    (error (condition) (log-error "sidebar view" condition) (cffi:null-pointer))))

;;; A click on a file visits it; on a directory, opens or closes it.
;;;
(objc:define-objc-method ("hemlSidebarClicked:" :void)
    ((self sidebar-source) (sender objc:objc-object-pointer))
  (handler-case
      (let ((row (objc:invoke sender "clickedRow")))
        (when (>= row 0)
          (let* ((item (objc:invoke sender "itemAtRow:" row))
                 (path (item-path item)))
            (cond ((null path))
                  ((directory-path-p path)
                   (if (objc:invoke-bool sender "isItemExpanded:" item)
                       (objc:invoke sender "collapseItem:" item)
                       (objc:invoke sender "expandItem:" item)))
                  (t (post-to-editor (list :open path)))))))
    (error (condition) (log-error "sidebar click" condition))))

(defconstant +material-sidebar+ 7)

(defun make-sidebar (display)
  (let* ((window (display-window display))
         (view (display-view display))
         (bounds (objc:invoke (objc:invoke window "contentView") "bounds"))
         (split (objc:invoke (objc:invoke "NSSplitView" "alloc") "initWithFrame:" bounds))
         (effect (objc:invoke (objc:invoke "NSVisualEffectView" "alloc") "initWithFrame:"
                              (vector 0d0 0d0 (df *sidebar-width*) (aref bounds 3))))
         (scroll (objc:invoke (objc:invoke "NSScrollView" "alloc") "initWithFrame:"
                              (vector 0d0 0d0 (df *sidebar-width*) (aref bounds 3))))
         (outline (objc:invoke (objc:invoke "NSOutlineView" "alloc") "initWithFrame:"
                               (vector 0d0 0d0 (df *sidebar-width*) (aref bounds 3))))
         (column (objc:invoke (objc:invoke "NSTableColumn" "alloc") "initWithIdentifier:" "name"))
         (source (make-instance 'sidebar-source)))
    (objc:invoke effect "setMaterial:" +material-sidebar+)
    (objc:invoke effect "setBlendingMode:" 0)
    (objc:invoke column "setWidth:" (df (- *sidebar-width* 20)))
    (objc:invoke outline "addTableColumn:" column)
    (objc:invoke outline "setOutlineTableColumn:" column)
    (objc:invoke outline "setHeaderView:" (cffi:null-pointer))
    (if (objc:invoke-bool outline "respondsToSelector:" (objc:coerce-to-selector "setStyle:"))
        (objc:invoke outline "setStyle:" 3)            ; a source list
        (objc:invoke outline "setSelectionHighlightStyle:" 1))
    (objc:invoke outline "setBackgroundColor:" (objc:invoke "NSColor" "clearColor"))
    (objc:invoke outline "setDataSource:" (objc:objc-object-pointer source))
    (objc:invoke outline "setDelegate:" (objc:objc-object-pointer source))
    (objc:invoke outline "setTarget:" (objc:objc-object-pointer source))
    (objc:invoke outline "setAction:" (objc:coerce-to-selector "hemlSidebarClicked:"))
    (objc:invoke scroll "setDocumentView:" outline)
    (objc:invoke scroll "setHasVerticalScroller:" t)
    (objc:invoke scroll "setAutohidesScrollers:" t)
    (objc:invoke scroll "setDrawsBackground:" nil)
    (objc:invoke scroll "setAutoresizingMask:" 18)
    (objc:invoke effect "addSubview:" scroll)
    (objc:invoke split "setVertical:" t)
    (objc:invoke split "setDividerStyle:" 2)             ; thin
    (objc:invoke split "setAutoresizingMask:" 18)
    (objc:invoke view "retain")
    (objc:invoke view "removeFromSuperview")
    (objc:invoke split "addSubview:" effect)
    (objc:invoke split "addSubview:" view)
    (objc:invoke view "release")
    ;; The sidebar keeps its width as the window is resized.
    (objc:invoke split "setHoldingPriority:forSubviewAtIndex:" 260f0 0)
    (objc:invoke split "setHoldingPriority:forSubviewAtIndex:" 250f0 1)
    (objc:invoke window "setContentView:" split)
    (objc:invoke window "makeFirstResponder:" view)
    (setf *sidebar* (list split effect outline source))))

(defun reload-sidebar ()
  (when (and *sidebar* *sidebar-root*)
    (clrhash *sidebar-children*)
    (read-git-statuses *sidebar-root*)
    (objc:invoke (third *sidebar*) "reloadData")))

(defun show-sidebar (shown)
  "Show the sidebar, or hide it.  On the main thread."
  (let ((display *display*))
    (when display
      (setf *sidebar-shown* shown)
      (save-preference "HemlSidebarShown" (if shown 1 0))
      (when (and shown (null *sidebar*))
        (make-sidebar display))
      (when *sidebar*
        (destructuring-bind (split effect &rest rest) *sidebar*
          (declare (ignore rest))
          (objc:invoke effect "setHidden:" (not shown))
          (objc:invoke split "adjustSubviews")
          (when shown
            (objc:invoke split "setPosition:ofDividerAtIndex:" (df *sidebar-width*) 0)
            (reload-sidebar))))
      (objc:invoke (display-window display) "makeFirstResponder:" (display-view display)))))

(defun toggle-sidebar ()
  (show-sidebar (not *sidebar-shown*)))

(defun note-sidebar-root (root)
  "The current buffer's project is ROOT: the sidebar shows it."
  (when (and root (not (equal root *sidebar-root*)))
    (setf *sidebar-root* root)
    (when *sidebar-shown* (reload-sidebar))))


;;;; Tabs, one for each file open.

(objc:define-objc-class tab-target ()
  ()
  (:objc-class-name "HemlTabTarget"))

(defvar *tabs-shown* t
  "Whether a strip of tabs, one for each file open, is under the title bar,
   when there are two or more.")

(defvar *tab-bar* nil
  "(CONTROLLER STACK TARGET) once the strip has been made.")

(defvar *tab-names* '()
  "The buffers the tabs show, in their order, as last shown.")

(defun make-tab-bar (display)
  (let* ((controller (objc:alloc-init-object "NSTitlebarAccessoryViewController"))
         (width (aref (objc:invoke (display-window display) "frame") 2))
         ;; A plain view the window's width, holding the tabs: the stack
         ;; itself, as the accessory's view, made the window as wide as all
         ;; its buttons, and wider with each tab.
         (container (objc:invoke (objc:invoke "NSView" "alloc") "initWithFrame:"
                                 (vector 0d0 0d0 (df width) 30d0)))
         ;; Tabs that do not fit are scrolled to, sideways, by a swipe or
         ;; Shift and the wheel.
         (scroll (objc:invoke (objc:invoke "NSScrollView" "alloc") "initWithFrame:"
                              (vector 0d0 0d0 (df width) 30d0)))
         (stack (objc:invoke (objc:invoke "NSStackView" "alloc") "initWithFrame:"
                             (vector 0d0 0d0 (df width) 30d0)))
         (target (make-instance 'tab-target)))
    (objc:invoke stack "setOrientation:" 0)              ; side by side
    (objc:invoke stack "setSpacing:" 2d0)
    (objc:invoke stack "setEdgeInsets:" (vector 2d0 8d0 2d0 8d0))
    (objc:invoke stack "setAlignment:" 10)                ; centred on Y
    ;; Too many tabs are scrolled, not room made for them.
    (objc:invoke stack "setClippingResistancePriority:forOrientation:" 1f0 0)
    (objc:invoke stack "setHuggingPriority:forOrientation:" 1f0 0)
    (objc:invoke stack "setTranslatesAutoresizingMaskIntoConstraints:" t)
    (objc:invoke scroll "setDrawsBackground:" nil)
    (objc:invoke scroll "setHasHorizontalScroller:" t)
    (objc:invoke scroll "setHasVerticalScroller:" nil)
    (objc:invoke scroll "setScrollerStyle:" 1)           ; overlay
    (objc:invoke scroll "setAutohidesScrollers:" t)
    (objc:invoke scroll "setVerticalScrollElasticity:" 1) ; none
    (objc:invoke scroll "setAutoresizingMask:" 18)
    (objc:invoke scroll "setDocumentView:" stack)
    (objc:invoke container "setAutoresizingMask:" 2)     ; the window's width
    (objc:invoke container "addSubview:" scroll)
    (objc:invoke controller "setView:" container)
    (objc:invoke controller "setLayoutAttribute:" 4)      ; under the title bar
    (objc:invoke (display-window display) "addTitlebarAccessoryViewController:" controller)
    (setf *tab-bar* (list controller stack target))))

(objc:define-objc-method ("hemlTabClicked:" :void)
    ((self tab-target) (sender objc:objc-object-pointer))
  (handler-case
      (let ((name (nth (objc:invoke sender "tag") *tab-names*)))
        (when name
          (post-to-editor (list :command "Cocoa Select Buffer" name))))
    (error (condition) (log-error "tab click" condition))))

(defun show-tabs (tabs)
  "TABS, from the editor: (CURRENT (NAME MODIFIED TITLE) ...), the buffers
   of files.  Their tabs keep the order they were first shown in."
  (let ((display *display*))
    (when display
      (destructuring-bind (current &rest entries) tabs
        (let* ((names (mapcar #'first entries))
               (order (append (remove-if-not (lambda (name) (member name names :test #'string=))
                                             *tab-names*)
                              (remove-if (lambda (name) (member name *tab-names* :test #'string=))
                                         names))))
          (setf *tab-names* order)
          (unless *tab-bar* (make-tab-bar display))
          (destructuring-bind (controller stack target) *tab-bar*
            (objc:invoke controller "setHidden:" (not (and *tabs-shown* (> (length order) 1))))
            (dolist (button (coerce-ns-array (objc:invoke stack "arrangedSubviews")))
              (objc:invoke stack "removeView:" button))
            (loop for name in order
                  for index from 0
                  for entry = (assoc name entries :test #'string=)
                  for modified = (second entry)
                  for title = (or (third entry) name)
                  do (let ((button (objc:invoke "NSButton" "buttonWithTitle:target:action:"
                                                (if modified (format nil "~A •" title) title)
                                                (objc:objc-object-pointer target)
                                                (objc:coerce-to-selector "hemlTabClicked:"))))
                       (objc:invoke button "setBezelStyle:" 13) ; recessed
                       (objc:invoke button "setButtonType:" 1)  ; on or off
                       (objc:invoke button "setShowsBorderOnlyWhileMouseInside:" t)
                       (objc:invoke button "setState:" (if (string= name current) 1 0))
                       (objc:invoke button "setTag:" index)
                       (objc:invoke stack "addArrangedSubview:" button)))
            (fit-tabs stack (position current order :test #'string=))))))))

(defun fit-tabs (stack current)
  "The tabs' stack as wide as its tabs, or the bar when they are fewer, and
   the current tab, at CURRENT among them, scrolled into sight."
  (let* ((clip (objc:invoke stack "superview"))
         (shown (aref (objc:invoke clip "bounds") 2))
         (buttons (coerce-ns-array (objc:invoke stack "arrangedSubviews")))
         ;; The buttons' own widths, the spacing and the insets: the stack's
         ;; fitting size is nothing, its clipping resistance being so low.
         (needed (+ 16 (* 2 (max 0 (1- (length buttons))))
                    (loop for button in buttons
                          sum (aref (objc:invoke button "fittingSize") 0)))))
    (objc:invoke stack "setFrame:" (vector 0d0 0d0 (df (max shown needed)) 30d0))
    (progn
      (when (and current (< current (length buttons)))
        (let ((button (nth current buttons)))
          (objc:invoke stack "layoutSubtreeIfNeeded")
          (objc:invoke button "scrollRectToVisible:" (objc:invoke button "bounds")))))))

(defun coerce-ns-array (array)
  (loop for i below (objc:invoke array "count")
        collect (objc:invoke array "objectAtIndex:" i)))

(defun toggle-tabs ()
  (setf *tabs-shown* (not *tabs-shown*))
  (save-preference "HemlTabsShown" (if *tabs-shown* 1 0))
  (when *tab-bar*
    (objc:invoke (first *tab-bar*) "setHidden:"
                 (not (and *tabs-shown* (> (length *tab-names*) 1))))))


;;;; Preferences kept between launches.

(defun save-preference (key value)
  (objc:invoke (objc:invoke "NSUserDefaults" "standardUserDefaults")
               "setInteger:forKey:" value key))

(defun saved-preference (key)
  "KEY's integer in the user's defaults, or NIL when there is none."
  (let ((defaults (objc:invoke "NSUserDefaults" "standardUserDefaults")))
    (unless (cffi:null-pointer-p (objc:invoke defaults "objectForKey:" key))
      (objc:invoke defaults "integerForKey:" key))))

(defvar *remember-chrome* t
  "Whether the sidebar and the tabs are as they were when last closed.  The
   smoke test leaves them alone.")

(defun restore-chrome (display)
  "At the window's making: its view followed, and the sidebar and tabs as
   they were."
  (follow-view-frame display)
  (when *remember-chrome*
    (let ((tabs (saved-preference "HemlTabsShown")))
      (when tabs (setf *tabs-shown* (= tabs 1))))
    (when (eql (saved-preference "HemlSidebarShown") 1)
      (show-sidebar t))))

(dolist (entry '(("Show Sidebar" (:call toggle-sidebar) :key "s" :modifiers (:control))
                 ("Show Tab Bar" (:call toggle-tabs))))
  (heml-interface:add-menu-item "View" entry :before "Split Window"))
