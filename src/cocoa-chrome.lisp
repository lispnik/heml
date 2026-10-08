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
;;;
;;; A workspace of projects, as Emacs's treemacs has: each project's root is
;;; a node at the top, its files under it.  A project is added when one of
;;; its files is visited (*SIDEBAR-FOLLOW-PROJECTS*), from the sidebar's
;;; menu (Add Project...), or with "Sidebar Add Project"; Remove Project
;;; takes one away.  The file being edited is shown and chosen in the tree
;;; (*SIDEBAR-FOLLOW-FILE*), what Git says of each file colours it, and a
;;; directory with changes in it is coloured too.  The projects are kept
;;; between launches, and what is open in the tree is kept as it is listed
;;; again.

(defvar *remember-chrome* t
  "Whether the sidebar and the tabs are as they were when last closed.  The
   smoke test leaves them alone.")

(objc:define-objc-class sidebar-source ()
  ()
  (:objc-class-name "HemlSidebarSource")
  (:objc-protocols "NSOutlineViewDataSource" "NSOutlineViewDelegate"))

(defvar *sidebar-shown* nil
  "Whether the sidebar of the projects' files is shown.")

(defparameter *sidebar-width* 240
  "The sidebar's width when it is first shown, in points.")

(defvar *sidebar* nil
  "(SPLIT EFFECT OUTLINE SOURCE) once the sidebar has been made.")

(defvar *sidebar-roots* '()
  "The projects the sidebar shows, as their directories' namestrings, in
   order.  On the main thread.")

(defvar *sidebar-root-names* (make-hash-table :test 'equal)
  "A project's name, as its settings give it, by its root, when known.")

(defvar *sidebar-follow-projects* t
  "Whether visiting a file of a project not in the sidebar adds it:
   \"Sidebar Follow Projects\", as the editor last said.")

(defvar *sidebar-follow-file* t
  "Whether the file being edited is shown and chosen in the sidebar:
   \"Sidebar Follow File\", as the editor last said.")

(defvar *sidebar-file* nil
  "The file being edited, as the last frame said.")

(defvar *sidebar-items* (make-hash-table :test 'equal)
  "Each path the sidebar has shown to its NSString, kept, so that the
   outline is given the same object for it each time it asks.")

(defvar *sidebar-paths* (make-hash-table)
  "Each of those NSStrings' addresses to its path.")

(defvar *sidebar-children* (make-hash-table :test 'equal)
  "Each directory listed to its entries, as paths.")

(defvar *sidebar-git* (make-hash-table :test 'equal)
  "Each file Git says something of to :MODIFIED or :NEW, and each directory
   with such a file in it to :INSIDE.")

(defvar *sidebar-ignored* '(".git" ".DS_Store" ".hg" ".svn")
  "Names the sidebar does not list: \"Sidebar Ignored\", as the editor last
   said.")

(defvar *sidebar-root-ignores* (make-hash-table :test 'equal)
  "Each project's .heml-project :ignore patterns, by its root, as the
   editor read them.")

(defun sidebar-root-of (path)
  "The project of the sidebar's that PATH is in: the innermost, for a
   project within another's directory."
  (let ((best nil))
    (dolist (root *sidebar-roots* best)
      (when (and (uiop:string-prefix-p root path)
                 (or (null best) (> (length root) (length best))))
        (setf best root)))))

(defun sidebar-ignored-p (path)
  "Whether the sidebar leaves PATH out: its name is in \"Sidebar Ignored\",
   or its project's :ignore patterns leave it out."
  (let ((name (if (directory-path-p path)
                  (car (last (pathname-directory path)))
                  (file-namestring path)))
        (root (sidebar-root-of path)))
    (or (member name *sidebar-ignored* :test #'equal)
        (and root
             (let ((patterns (gethash root *sidebar-root-ignores*)))
               (and patterns
                    (heml::ignored-file-p (subseq path (length root)) patterns)))))))

(defun fetch-sidebar-ignores (root)
  "Have the editor read ROOT's :ignore patterns, and list the sidebar again
   with them."
  (post-to-editor
   (list :call (lambda ()
                 (let ((patterns (ignore-errors (getf (heml::project-settings root) :ignore))))
                   (on-main-thread
                     (unless (equal patterns (gethash root *sidebar-root-ignores*))
                       (setf (gethash root *sidebar-root-ignores*) patterns)
                       (when *sidebar-shown* (reload-sidebar)))))))))

(defun sidebar-item (path)
  (or (gethash path *sidebar-items*)
      (let ((string (objc:retain (objc:string-to-ns-string path))))
        (setf (gethash (cffi:pointer-address string) *sidebar-paths*) path
              (gethash path *sidebar-items*) string))))

(defun item-path (item)
  "ITEM's path, or NIL for the outline's top, the workspace."
  (if (cffi:null-pointer-p item)
      nil
      (gethash (cffi:pointer-address item) *sidebar-paths*)))

(defun directory-path-p (path)
  (let ((length (length path)))
    (and (plusp length) (char= (char path (1- length)) #\/))))

(defun sidebar-root-p (path)
  (member path *sidebar-roots* :test #'equal))

(defvar *sidebar-sort* :name
  "How each folder's entries are ordered: \"Sidebar Sort\", as the editor
   last said.")

(defparameter *sidebar-sorts*
  '((10 :name "Name") (11 :kind "Kind") (12 :date "Date Modified") (13 :size "Size"))
  "(TAG SORT TITLE) for each order the sidebar's Sort By submenu offers.")

(defvar *sidebar-folders-first* t
  "\"Sidebar Folders First\", as the editor last said.")

(defun entry-stat (path)
  "PATH's modification time and size, or zeros."
  (handler-case (let ((stat (sb-posix:stat (string-right-trim "/" path))))
                  (values (sb-posix:stat-mtime stat) (sb-posix:stat-size stat)))
    (error () (values 0 0))))

(defun sort-sidebar-entries (paths)
  "PATHS in the order \"Sidebar Sort\" says, the name breaking ties; a
   folder has no size worth ordering by, and goes by its name for :SIZE."
  (let ((keyed (mapcar (lambda (path)
                         (multiple-value-bind (date size) (entry-stat path)
                           (list path (string-downcase (entry-name path)) date
                                 (if (directory-path-p path) -1 size)
                                 (if (directory-path-p path)
                                     ""
                                     (string-downcase (or (pathname-type path) ""))))))
                       paths)))
    (flet ((by-name (a b) (string< (second a) (second b))))
      (mapcar #'first
              (stable-sort (sort keyed #'by-name)
                           (ecase *sidebar-sort*
                             (:name (constantly nil))
                             (:kind (lambda (a b) (string< (fifth a) (fifth b))))
                             (:date (lambda (a b) (> (third a) (third b))))
                             (:size (lambda (a b) (> (fourth a) (fourth b))))))))))

(defun sidebar-entries (directory)
  "DIRECTORY's entries as paths, in the order \"Sidebar Sort\" says, its
   folders first while \"Sidebar Folders First\", but for those it leaves
   out (SIDEBAR-IGNORED-P)."
  (or (gethash directory *sidebar-children*)
      (setf (gethash directory *sidebar-children*)
            (ignore-errors
             (let ((folders (remove-if #'sidebar-ignored-p
                                       (mapcar #'namestring (uiop:subdirectories directory))))
                   (files (remove-if #'sidebar-ignored-p
                                     (mapcar #'namestring (uiop:directory-files directory)))))
               (if *sidebar-folders-first*
                   (append (sort-sidebar-entries folders) (sort-sidebar-entries files))
                   (sort-sidebar-entries (append folders files))))))))

(defun sidebar-children (path)
  "What the outline shows under PATH: the projects at the top."
  (if path (sidebar-entries path) *sidebar-roots*))

(defun read-git-statuses ()
  "What Git says of each project's files, into *SIDEBAR-GIT*, with the
   directories between a changed file and its project marked :INSIDE."
  (clrhash *sidebar-git*)
  (dolist (root *sidebar-roots*)
    (let ((output (ignore-errors
                   (uiop:run-program (list "git" "-C" root "status" "--porcelain=v1" "-z"
                                           "--untracked-files=all")
                                     :output :string :ignore-error-status t))))
      (when output
        (dolist (entry (uiop:split-string output :separator (string (code-char 0))))
          (when (> (length entry) 3)
            (let ((path (concatenate 'string root (subseq entry 3))))
              (setf (gethash path *sidebar-git*)
                    (if (string= (subseq entry 0 2) "??") :new :modified))
              ;; Each directory from the file up to the project.
              (loop for slash = (position #\/ path :end (1- (length path)) :from-end t)
                      then (position #\/ path :end slash :from-end t)
                    while (and slash (> slash (length root)))
                    do (let ((directory (subseq path 0 (1+ slash))))
                         (unless (gethash directory *sidebar-git*)
                           (setf (gethash directory *sidebar-git*) :inside)))))))))))

(objc:define-objc-method ("outlineView:numberOfChildrenOfItem:" :long)
    ((self sidebar-source) (outline objc:objc-object-pointer) (item objc:objc-object-pointer))
  (declare (ignore outline))
  (handler-case
      (let ((path (item-path item)))
        (if (or (null path) (directory-path-p path)) (length (sidebar-children path)) 0))
    (error (condition) (log-error "sidebar children" condition) 0)))

(objc:define-objc-method ("outlineView:child:ofItem:" objc:objc-object-pointer)
    ((self sidebar-source) (outline objc:objc-object-pointer) (index :long)
     (item objc:objc-object-pointer))
  (declare (ignore outline))
  (handler-case
      (sidebar-item (nth index (sidebar-children (item-path item))))
    (error (condition) (log-error "sidebar child" condition) (cffi:null-pointer))))

(objc:define-objc-method ("outlineView:isItemExpandable:" objc:objc-bool)
    ((self sidebar-source) (outline objc:objc-object-pointer) (item objc:objc-object-pointer))
  (declare (ignore outline))
  (let ((path (item-path item)))
    (and path (directory-path-p path))))

(defun entry-name (path)
  (cond ((sidebar-root-p path)
         (or (gethash path *sidebar-root-names*) (car (last (pathname-directory path)))))
        ((directory-path-p path) (car (last (pathname-directory path))))
        (t (file-namestring path))))

(objc:define-objc-method ("outlineView:viewForTableColumn:item:" objc:objc-object-pointer)
    ((self sidebar-source) (outline objc:objc-object-pointer) (column objc:objc-object-pointer)
     (item objc:objc-object-pointer))
  (declare (ignore outline column))
  (handler-case
      (let* ((path (item-path item))
             (root (sidebar-root-p path))
             (cell (objc:invoke (objc:invoke "NSTableCellView" "alloc") "initWithFrame:"
                                (vector 0d0 0d0 200d0 20d0)))
             (image (objc:invoke (objc:invoke "NSImageView" "alloc") "initWithFrame:"
                                 (vector 2d0 2d0 16d0 16d0)))
             (text (objc:invoke "NSTextField" "labelWithString:" (entry-name path)))
             (status (gethash (if (directory-path-p path) path (string-right-trim "/" path))
                              *sidebar-git*)))
        (objc:invoke image "setImage:"
                     (if root
                         (objc:invoke "NSImage" "imageWithSystemSymbolName:accessibilityDescription:"
                                      "shippingbox" "Project")
                         (objc:invoke (objc:invoke "NSWorkspace" "sharedWorkspace") "iconForFile:" path)))
        (objc:invoke text "setFrame:" (vector 22d0 1d0 170d0 18d0))
        (objc:invoke text "setAutoresizingMask:" 2)
        (objc:invoke text "setLineBreakMode:" 4)
        (when root
          (objc:invoke text "setFont:" (objc:invoke "NSFont" "boldSystemFontOfSize:" 0d0))
          (objc:invoke cell "setToolTip:" path))
        (when status
          (objc:invoke text "setTextColor:"
                       (objc:invoke "NSColor" (case status
                                                (:new "systemGreenColor")
                                                (:modified "systemOrangeColor")
                                                (t "systemBrownColor")))))
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
                  ;; Visited, and the keys the editor's again.
                  (t (post-to-editor (list :open path))
                     (focus-editor))))))
    (error (condition) (log-error "sidebar click" condition))))


;;;; The workspace's projects.

(defun sidebar-outline () (third *sidebar*))

(defun clicked-path ()
  "The path of the row the sidebar's menu was opened on, or NIL."
  (let ((row (objc:invoke (sidebar-outline) "clickedRow")))
    (and (>= row 0) (item-path (objc:invoke (sidebar-outline) "itemAtRow:" row)))))

(defun clicked-root ()
  "The project of the row the sidebar's menu was opened on."
  (let ((path (clicked-path)))
    (and path (sidebar-root-of path))))

(defun save-sidebar-roots ()
  (when *remember-chrome*
    (objc:invoke (objc:invoke "NSUserDefaults" "standardUserDefaults") "setObject:forKey:"
                 (coerce-to-ns-array (mapcar #'objc:string-to-ns-string *sidebar-roots*))
                 "HemlSidebarRoots")))

(defun saved-sidebar-roots ()
  (let ((array (objc:invoke (objc:invoke "NSUserDefaults" "standardUserDefaults")
                            "stringArrayForKey:" "HemlSidebarRoots")))
    (unless (cffi:null-pointer-p array)
      (remove-if-not #'probe-file
                     (mapcar #'objc:ns-string-to-string (coerce-ns-array array))))))

(defun coerce-to-ns-array (objects)
  (let ((array (objc:invoke "NSMutableArray" "array")))
    (dolist (object objects array)
      (objc:invoke array "addObject:" object))))

(defun add-sidebar-root (root &optional name)
  "Put the project at ROOT, a directory, in the sidebar, at its end.  On the
   main thread."
  ;; As the file system names it: a path through .. or a link is the same
  ;; project, not another.
  (let ((root (namestring (let ((directory (uiop:ensure-directory-pathname root)))
                            (or (ignore-errors (probe-file directory)) directory)))))
    (when name (setf (gethash root *sidebar-root-names*) name))
    (unless (sidebar-root-p root)
      (setf *sidebar-roots* (append *sidebar-roots* (list root)))
      (save-sidebar-roots)
      (fetch-sidebar-ignores root)
      (when *sidebar-shown*
        (reload-sidebar)
        (objc:invoke (sidebar-outline) "expandItem:" (sidebar-item root))))))

(defun remove-sidebar-root (root)
  "Take the project at ROOT out of the sidebar.  On the main thread."
  (when (sidebar-root-p root)
    (setf *sidebar-roots* (remove root *sidebar-roots* :test #'equal))
    (save-sidebar-roots)
    (when *sidebar-shown* (reload-sidebar))))

(defun place-sidebar-root (root index)
  "Put the project at ROOT at INDEX among the sidebar's projects, keep the
   order, list them again with it chosen.  On the main thread."
  (when (sidebar-root-p root)
    (let* ((others (remove root *sidebar-roots* :test #'equal))
           (index (max 0 (min index (length others)))))
      (setf *sidebar-roots* (append (subseq others 0 index) (list root) (nthcdr index others)))
      (save-sidebar-roots)
      (when *sidebar-shown*
        (reload-sidebar)
        (let* ((outline (sidebar-outline))
               (row (objc:invoke outline "rowForItem:" (sidebar-item root))))
          (when (>= row 0)
            (objc:invoke outline "selectRowIndexes:byExtendingSelection:"
                         (objc:invoke "NSIndexSet" "indexSetWithIndex:" row) nil)
            (objc:invoke outline "scrollRowToVisible:" row)))))))

(defun move-sidebar-root (root offset)
  "Move the project at ROOT OFFSET places: up for -1, down for 1."
  (let ((index (position root *sidebar-roots* :test #'equal)))
    (when index
      (place-sidebar-root root (+ index offset)))))

(defun set-sidebar-sort (sort &optional (folders-first *sidebar-folders-first*))
  "Order the sidebar by SORT, setting the editor's variables, whose next
   frame lists it again so; and say so."
  (post-to-editor
   (list :call (lambda ()
                 (setf (hi:variable-value 'heml::sidebar-sort :global) sort
                       (hi:variable-value 'heml::sidebar-folders-first :global) folders-first)
                 (hi:message "Sidebar sorted by ~(~A~)~:[~;, folders first~]."
                              (if (eq sort :date) "date modified" sort) folders-first)))))

(defun expanded-paths ()
  "The paths open in the tree, those above first."
  (let ((outline (sidebar-outline)))
    (loop for row below (objc:invoke outline "numberOfRows")
          for item = (objc:invoke outline "itemAtRow:" row)
          when (objc:invoke-bool outline "isItemExpanded:" item)
            collect (item-path item))))

(defun reload-sidebar ()
  "List the projects again, keeping what is open in the tree and what is
   chosen; the file being edited is chosen when nothing was."
  (when *sidebar*
    (let* ((open (expanded-paths))
           (outline (sidebar-outline))
           (row (objc:invoke outline "selectedRow"))
           (chosen (and (>= row 0) (item-path (objc:invoke outline "itemAtRow:" row)))))
      (clrhash *sidebar-children*)
      (read-git-statuses)
      (objc:invoke outline "reloadData")
      (dolist (path open)
        (when (and path (probe-file path))
          (objc:invoke outline "expandItem:" (sidebar-item path))))
      (let ((row (if chosen (objc:invoke outline "rowForItem:" (sidebar-item chosen)) -1)))
        (if (>= row 0)
            (objc:invoke outline "selectRowIndexes:byExtendingSelection:"
                         (objc:invoke "NSIndexSet" "indexSetWithIndex:" row) nil)
            (follow-sidebar-file))))))

(defun follow-sidebar-file ()
  "Show and choose the file being edited, opening the directories above it."
  (let ((file *sidebar-file*)
        (outline (and *sidebar* (sidebar-outline))))
    (when (and file outline *sidebar-follow-file* *sidebar-shown*)
      (let ((root (sidebar-root-of file)))
        (when root
          ;; Each directory from the project down to the file's.
          (objc:invoke outline "expandItem:" (sidebar-item root))
          (loop for slash = (position #\/ file :start (length root))
                  then (position #\/ file :start (1+ slash))
                while slash
                do (objc:invoke outline "expandItem:" (sidebar-item (subseq file 0 (1+ slash)))))
          (let ((row (objc:invoke outline "rowForItem:" (sidebar-item file))))
            (when (>= row 0)
              (objc:invoke outline "selectRowIndexes:byExtendingSelection:"
                           (objc:invoke "NSIndexSet" "indexSetWithIndex:" row) nil)
              (objc:invoke outline "scrollRowToVisible:" row))))))))

(defun note-sidebar-title (title)
  "The frame's title, (NAME FILE MODIFIED PROJECT ROOT): its project joins
   the sidebar, its file is followed, and Git is asked again when the file
   is saved."
  (destructuring-bind (name file modified project root) title
    (declare (ignore name))
    (let ((saved (and *sidebar-file* (equal file *sidebar-file*) (not modified))))
      (when (and root project)
        (setf (gethash (namestring (uiop:ensure-directory-pathname root)) *sidebar-root-names*)
              project))
      (when (and root *sidebar-follow-projects*)
        (add-sidebar-root root))
      (let ((changed (not (equal file *sidebar-file*))))
        (setf *sidebar-file* file)
        (cond ((and *sidebar-shown* saved)
               (reload-sidebar))
              (changed (follow-sidebar-file)))))))


;;;; The sidebar's keys, as treemacs has them: M-0 goes between it and the
;;;; editor (Escape and C-g go back too); Return visits the file chosen and
;;;; goes back, o visits it in the other window, Tab opens or closes a
;;;; directory, a adds a project, d removes the one chosen, g lists them
;;;; again, R shows the file in Finder.  Arrows and typing a name are
;;;; AppKit's own.

(objc:define-objc-class sidebar-outline ()
  ()
  (:objc-class-name "HemlSidebarOutline")
  (:objc-superclass-name "NSOutlineView"))

(defun focus-editor ()
  (let ((display *display*))
    (when display
      (objc:invoke (display-window display) "makeFirstResponder:" (display-view display)))))

(defun focus-sidebar ()
  "The keyboard to the sidebar, shown first if it is not, with the file
   being edited chosen when nothing is."
  (unless *sidebar-shown* (show-sidebar t))
  (when *sidebar*
    (let ((outline (sidebar-outline)))
      (when (minusp (objc:invoke outline "selectedRow"))
        (follow-sidebar-file))
      (objc:invoke (display-window *display*) "makeFirstResponder:" outline))))

(defun selected-path ()
  (let* ((outline (sidebar-outline))
         (row (objc:invoke outline "selectedRow")))
    (and (>= row 0) (item-path (objc:invoke outline "itemAtRow:" row)))))

(defun toggle-sidebar-directory (path)
  (let ((outline (sidebar-outline))
        (item (sidebar-item path)))
    (if (objc:invoke-bool outline "isItemExpanded:" item)
        (objc:invoke outline "collapseItem:" item)
        (objc:invoke outline "expandItem:" item))))

(defun reveal-in-finder (path)
  (objc:invoke (objc:invoke "NSWorkspace" "sharedWorkspace")
               "selectFile:inFileViewerRootedAtPath:" (string-right-trim "/" path) ""))

(defun sidebar-key (event)
  "Act on the key EVENT in the sidebar, or say it is not one of its own."
  (let* ((characters (objc:ns-string-to-string (objc:invoke event "charactersIgnoringModifiers")))
         (flags (objc:invoke event "modifierFlags"))
         (code (objc:invoke event "keyCode"))
         (control (logtest flags +control-mask+))
         (meta (meta-p flags))
         (plain (not (or control meta (logtest flags +command-key-mask+))))
         (path (selected-path)))
    (cond ((and meta (string= characters "0")) (focus-editor) t)
          ((or (= code 53) (and control (string-equal characters "g"))) (focus-editor) t)
          ((and plain (member code '(36 76)))  ; Return, Enter
           (cond ((null path))
                 ((directory-path-p path) (toggle-sidebar-directory path))
                 (t (post-to-editor (list :open path)) (focus-editor)))
           t)
          ((and plain (= code 48))             ; Tab
           (when (and path (directory-path-p path)) (toggle-sidebar-directory path))
           t)
          ((and plain (string= characters "o"))
           (when (and path (not (directory-path-p path)))
             (post-to-editor (list :command "Sidebar Visit Other Window" path))
             (focus-editor))
           t)
          ((and plain (string= characters "a"))
           (let ((directory (choose-directory)))
             (when directory (add-sidebar-root directory)))
           t)
          ((and plain (string= characters "d"))
           (let ((root (and path (sidebar-root-of path))))
             (when root (remove-sidebar-root root)))
           t)
          ((and plain (string= characters "g"))
           (mapc #'fetch-sidebar-ignores *sidebar-roots*)
           (reload-sidebar)
           t)
          ((and plain (string= characters "R"))
           (when path (reveal-in-finder path))
           t)
          ((and plain (string= characters "s"))
           (set-sidebar-sort (let ((orders '(:name :kind :date :size)))
                               (or (second (member *sidebar-sort* orders)) (first orders))))
           t)
          ((and meta (member code '(125 126)))   ; Option-Down, Option-Up
           (let ((root (and path (sidebar-root-of path))))
             (when root (move-sidebar-root root (if (= code 126) -1 1))))
           t)
          (t nil))))

(objc:define-objc-method ("keyDown:" :void)
    ((self sidebar-outline) (event objc:objc-object-pointer))
  (unless (handler-case (sidebar-key event)
            (error (condition) (log-error "sidebar key" condition) t))
    (objc:invoke (objc:current-super) "keyDown:" event)))

(hi:defcommand "Sidebar Focus" (p)
  "Go to the sidebar, showing it if it is hidden; M-0 there comes back."
  "Go to the sidebar."
  (declare (ignore p))
  (on-main-thread (focus-sidebar)))

(hi:defcommand "Sidebar Visit Other Window" (p &optional path)
  "Visit PATH, the sidebar's file, in the other window."
  "Visit a file in the other window."
  (declare (ignore p))
  (when path
    (heml::show-in-other-window (heml::find-file-buffer path))))


;;;; Projects dragged into another order.  Only a project's row drags, and
;;;; only between projects at the top does it drop; the root dragged is
;;;; kept here rather than read back from the pasteboard.

(defparameter +sidebar-drag-type+ "org.lispnik.heml.sidebar-project")

(defvar *sidebar-dragged-root* nil)

(objc:define-objc-method ("outlineView:pasteboardWriterForItem:" objc:objc-object-pointer)
    ((self sidebar-source) (outline objc:objc-object-pointer) (item objc:objc-object-pointer))
  (declare (ignore outline))
  (let ((path (item-path item)))
    (cond ((and path (sidebar-root-p path))
           (setf *sidebar-dragged-root* path)
           (let ((writer (objc:alloc-init-object "NSPasteboardItem")))
             (objc:invoke writer "setString:forType:" path +sidebar-drag-type+)
             (objc:invoke writer "autorelease")))
          (t (cffi:null-pointer)))))

(objc:define-objc-method ("outlineView:validateDrop:proposedItem:proposedChildIndex:" (:unsigned :long))
    ((self sidebar-source) (outline objc:objc-object-pointer) (info objc:objc-object-pointer)
     (item objc:objc-object-pointer) (index :long))
  (declare (ignore info))
  (cond ((null *sidebar-dragged-root*) 0)
        ;; Between projects: as proposed.
        ((and (cffi:null-pointer-p item) (>= index 0)) 16)
        ;; Onto a project's tree: before that project instead.
        (t (let* ((path (item-path item))
                  (root (and path (sidebar-root-of path)))
                  (place (and root (position root *sidebar-roots* :test #'equal))))
             (cond (place
                    (unless (cffi:null-pointer-p outline)
                      (objc:invoke outline "setDropItem:dropChildIndex:" (cffi:null-pointer) place))
                    16)
                   (t 0))))))

(objc:define-objc-method ("outlineView:acceptDrop:item:childIndex:" objc:objc-bool)
    ((self sidebar-source) (outline objc:objc-object-pointer) (info objc:objc-object-pointer)
     (item objc:objc-object-pointer) (index :long))
  (declare (ignore outline info))
  (let ((root *sidebar-dragged-root*))
    (setf *sidebar-dragged-root* nil)
    (cond ((and root (cffi:null-pointer-p item) (>= index 0))
           ;; INDEX counts the root dragged where it was.
           (let ((from (position root *sidebar-roots* :test #'equal)))
             (place-sidebar-root root (if (and from (< from index)) (1- index) index)))
           t)
          (t nil))))


;;;; The sidebar's menu.

(defun choose-directory ()
  "A directory chosen in an open panel, or NIL."
  (let ((panel (objc:invoke "NSOpenPanel" "openPanel")))
    (objc:invoke panel "setCanChooseDirectories:" t)
    (objc:invoke panel "setCanChooseFiles:" nil)
    (objc:invoke panel "setAllowsMultipleSelection:" nil)
    (objc:invoke panel "setPrompt:" "Add Project")
    (when (= 1 (objc:invoke panel "runModal"))
      (objc:ns-string-to-string (objc:invoke (objc:invoke panel "URL") "path")))))

(objc:define-objc-method ("hemlSidebarAddProject:" :void)
    ((self sidebar-source) (sender objc:objc-object-pointer))
  (declare (ignore sender))
  (handler-case (let ((directory (choose-directory)))
                  (when directory (add-sidebar-root directory)))
    (error (condition) (log-error "sidebar add" condition))))

(objc:define-objc-method ("hemlSidebarRemoveProject:" :void)
    ((self sidebar-source) (sender objc:objc-object-pointer))
  (declare (ignore sender))
  (handler-case (let ((root (clicked-root)))
                  (when root (remove-sidebar-root root)))
    (error (condition) (log-error "sidebar remove" condition))))

(objc:define-objc-method ("hemlSidebarReveal:" :void)
    ((self sidebar-source) (sender objc:objc-object-pointer))
  (declare (ignore sender))
  (handler-case (let ((path (clicked-path)))
                  (when path (reveal-in-finder path)))
    (error (condition) (log-error "sidebar reveal" condition))))

(objc:define-objc-method ("hemlSidebarCollapseAll:" :void)
    ((self sidebar-source) (sender objc:objc-object-pointer))
  (declare (ignore sender))
  (objc:invoke (sidebar-outline) "collapseItem:collapseChildren:" (cffi:null-pointer) t))

(objc:define-objc-method ("hemlSidebarRefresh:" :void)
    ((self sidebar-source) (sender objc:objc-object-pointer))
  (declare (ignore sender))
  ;; A project's settings may have changed too.
  (mapc #'fetch-sidebar-ignores *sidebar-roots*)
  (reload-sidebar))

(objc:define-objc-method ("validateMenuItem:" objc:objc-bool)
    ((self sidebar-source) (item objc:objc-object-pointer))
  ;; Remove Project on a project's rows, Reveal on any row.
  (let ((tag (objc:invoke item "tag")))
    ;; The orders' check marks, as they are.
    (let ((sort (assoc tag *sidebar-sorts*)))
      (cond (sort (objc:invoke item "setState:" (if (eq (second sort) *sidebar-sort*) 1 0)))
            ((= tag 14) (objc:invoke item "setState:" (if *sidebar-folders-first* 1 0)))))
    (case tag
      ((2 6 7) (and (clicked-root) t))
      (3 (and (clicked-path) t))
      (t t))))

(defun sidebar-menu-items (menu source entries)
  (loop for (title selector tag) in entries
        do (if (eq title :separator)
               (objc:invoke menu "addItem:" (objc:invoke "NSMenuItem" "separatorItem"))
               (let ((item (objc:alloc-init-object "NSMenuItem")))
                 (objc:invoke item "setTitle:" title)
                 (objc:invoke item "setAction:" (objc:coerce-to-selector selector))
                 (objc:invoke item "setTarget:" source)
                 (objc:invoke item "setTag:" tag)
                 (objc:invoke menu "addItem:" item)))))

(defun make-sidebar-menu (source)
  (let ((menu (make-menu "Sidebar"))
        (sort (make-menu "Sort By"))
        (sort-item (objc:alloc-init-object "NSMenuItem")))
    (sidebar-menu-items menu source '(("Add Project…" "hemlSidebarAddProject:" 1)
                                      ("Remove Project" "hemlSidebarRemoveProject:" 2)
                                      ("Move Project Up" "hemlSidebarMoveUp:" 6)
                                      ("Move Project Down" "hemlSidebarMoveDown:" 7)
                                      (:separator)
                                      ("Reveal in Finder" "hemlSidebarReveal:" 3)
                                      ("Collapse All" "hemlSidebarCollapseAll:" 4)
                                      ("Refresh" "hemlSidebarRefresh:" 5)
                                      (:separator)))
    (sidebar-menu-items sort source
                        (append (loop for (tag nil title) in *sidebar-sorts*
                                      collect (list title "hemlSidebarSort:" tag))
                                '((:separator)
                                  ("Folders First" "hemlSidebarFoldersFirst:" 14))))
    (objc:invoke sort-item "setTitle:" "Sort By")
    (objc:invoke sort-item "setSubmenu:" sort)
    (objc:invoke menu "addItem:" sort-item)
    menu))

(objc:define-objc-method ("hemlSidebarMoveUp:" :void)
    ((self sidebar-source) (sender objc:objc-object-pointer))
  (declare (ignore sender))
  (let ((root (clicked-root))) (when root (move-sidebar-root root -1))))

(objc:define-objc-method ("hemlSidebarMoveDown:" :void)
    ((self sidebar-source) (sender objc:objc-object-pointer))
  (declare (ignore sender))
  (let ((root (clicked-root))) (when root (move-sidebar-root root 1))))

(objc:define-objc-method ("hemlSidebarSort:" :void)
    ((self sidebar-source) (sender objc:objc-object-pointer))
  (let ((entry (assoc (objc:invoke sender "tag") *sidebar-sorts*)))
    (when entry (set-sidebar-sort (second entry)))))

(objc:define-objc-method ("hemlSidebarFoldersFirst:" :void)
    ((self sidebar-source) (sender objc:objc-object-pointer))
  (declare (ignore sender))
  (set-sidebar-sort *sidebar-sort* (not *sidebar-folders-first*)))

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
         (outline (objc:invoke (objc:invoke "HemlSidebarOutline" "alloc") "initWithFrame:"
                               (vector 0d0 0d0 (df *sidebar-width*) (aref bounds 3))))
         (column (objc:invoke (objc:invoke "NSTableColumn" "alloc") "initWithIdentifier:" "name"))
         (source (objc:objc-object-pointer (make-instance 'sidebar-source))))
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
    (objc:invoke outline "setDataSource:" source)
    (objc:invoke outline "setDelegate:" source)
    (objc:invoke outline "setTarget:" source)
    (objc:invoke outline "setAction:" (objc:coerce-to-selector "hemlSidebarClicked:"))
    (objc:invoke outline "setMenu:" (make-sidebar-menu source))
    (objc:invoke outline "registerForDraggedTypes:"
                 (coerce-to-ns-array (list (objc:string-to-ns-string +sidebar-drag-type+))))
    (objc:invoke outline "setDraggingSourceOperationMask:forLocal:" 16 t)
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

(defun show-sidebar (shown)
  "Show the sidebar, or hide it.  On the main thread."
  (let ((display *display*))
    (when display
      (setf *sidebar-shown* shown)
      (save-preference "HemlSidebarShown" (if shown 1 0))
      (when (and shown (null *sidebar*))
        (when *remember-chrome*
          (dolist (root (saved-sidebar-roots))
            (unless (sidebar-root-p root)
              (setf *sidebar-roots* (append *sidebar-roots* (list root)))
              (fetch-sidebar-ignores root))))
        (make-sidebar display))
      (when *sidebar*
        (destructuring-bind (split effect &rest rest) *sidebar*
          (declare (ignore rest))
          (objc:invoke effect "setHidden:" (not shown))
          (objc:invoke split "adjustSubviews")
          (when shown
            (objc:invoke split "setPosition:ofDividerAtIndex:" (df *sidebar-width*) 0)
            (reload-sidebar)
            ;; The projects open at first.
            (dolist (root *sidebar-roots*)
              (objc:invoke (sidebar-outline) "expandItem:" (sidebar-item root)))
            (follow-sidebar-file))))
      (objc:invoke (display-window display) "makeFirstResponder:" (display-view display)))))

(defun apply-editor-settings (settings)
  "Take the editor's variables, as a frame carries them, into the main
   thread's own copies.  On the main thread."
  (destructuring-bind (&key cursor-style cursor-blink pixel-scrolling mouse-wheel-lines
                         sidebar-ignored sidebar-follow-projects sidebar-follow-file
                         (sidebar-sort :name) (sidebar-folders-first t))
      settings
    (let ((ignored-changed (not (and (equal sidebar-ignored *sidebar-ignored*)
                                     (eq sidebar-sort *sidebar-sort*)
                                     (eq (and sidebar-folders-first t) *sidebar-folders-first*)))))
      (setf *cursor-style* (if (eq cursor-style :block) :block :bar)
            *cursor-blink* (and cursor-blink t)
            *pixel-scrolling* (and pixel-scrolling t)
            *lines-per-wheel-step* (if (and (integerp mouse-wheel-lines) (plusp mouse-wheel-lines))
                                       mouse-wheel-lines
                                       3)
            *sidebar-ignored* sidebar-ignored
            *sidebar-follow-projects* (and sidebar-follow-projects t)
            *sidebar-follow-file* (and sidebar-follow-file t)
            *sidebar-sort* (if (member sidebar-sort '(:name :kind :date :size)) sidebar-sort :name)
            *sidebar-folders-first* (and sidebar-folders-first t))
      (when (and ignored-changed *sidebar-shown*)
        (reload-sidebar))
      (request-redraw))))

(defun toggle-sidebar ()
  (show-sidebar (not *sidebar-shown*)))

(hi:defcommand "Sidebar Add Project" (p)
  "Put a project in the sidebar: this buffer's, or with an argument, a
   directory asked for."
  "Put a project in the sidebar."
  (let ((root (if p
                  (namestring (hi:prompt-for-file :prompt "Add project: "
                                                   :default (heml::buffer-default-directory
                                                             (hi:current-buffer))
                                                   :must-exist t))
                  (or (heml::buffer-project-root (hi:current-buffer))
                      (hi:editor-error "This buffer is in no project.")))))
    (on-main-thread (add-sidebar-root root) (show-sidebar t))))

(hi:defcommand "Sidebar Remove Project" (p)
  "Take a project out of the sidebar, asked for among those it shows."
  "Take a project out of the sidebar."
  (declare (ignore p))
  (let ((roots (copy-list *sidebar-roots*)))
    (unless roots (hi:editor-error "The sidebar shows no project."))
    (let ((root (nth-value 1 (hi:prompt-for-keyword
                              (list (hi:make-string-table
                                     :initial-contents (mapcar (lambda (root) (cons root root)) roots)))
                              :prompt "Remove project: " :help "A project the sidebar shows."))))
      (on-main-thread (remove-sidebar-root root)))))


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
