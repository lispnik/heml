;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;;; Menus, as data, for whichever frontend shows them.
;;;;
;;;; A menu is a title, options and entries, kept in order in *MENUS*.  An
;;;; entry is :SEPARATOR, or (TITLE ACTION &key KEY MODIFIERS HIDDEN), where
;;;; ACTION is the name of a Heml command, or a list:
;;;;
;;;;   (:command name arg ...)   the command, given ARGs
;;;;   (:call function)          FUNCTION, called by the frontend: Cocoa
;;;;                             calls it on its main thread
;;;;   (:selector "name:")       an AppKit action, sent up the responder chain
;;;;   (:font-size delta)        Cocoa's font, bigger, smaller or the default
;;;;
;;;; or :SERVICES, for the Mac's Services submenu.  KEY and MODIFIERS make a
;;;; Command-key equivalent on the Mac.  A menu's options are :MODE, a major
;;;; mode's name, for a menu shown only while the current buffer is in that
;;;; mode, and :ROLE, :APPLICATION, :WINDOWS or :HELP, for the menus a
;;;; frontend treats specially.
;;;;
;;;; Every change calls the functions in *MENU-CHANGE-FUNCTIONS*: the Cocoa
;;;; editor rebuilds its menu bar.  The terminal editor has no menu bar, but
;;;; "Menu Bar" (M-`) picks a menu and then an item in the echo area.

(in-package :heml)

(defstruct (menu (:constructor make-menu (title mode role entries)))
  title
  mode                                  ; a major mode's name, or NIL
  role                                  ; :APPLICATION, :WINDOWS, :HELP or NIL
  entries)

(defvar *menus* '()
  "The menu bar's menus, in order.")

(defvar *context-menus* '()
  "Right-click menus, as (MODE . ENTRIES): MODE a major mode's name, or NIL
for the one other buffers get.")

(defvar *menu-change-functions* '()
  "Functions of no arguments, called after the menus change.")

(defun menus-changed ()
  (dolist (function *menu-change-functions*)
    (funcall function)))

(defun find-menu (title)
  (find title *menus* :key #'menu-title :test #'string=))

(defun menu-entry-title (entry)
  (and (consp entry) (first entry)))

(defun insertion-index (list key before after)
  "Where to put something in LIST: before or after the element whose KEY is
BEFORE or AFTER, or NIL for the default."
  (cond (before (or (position before list :key key :test #'equal)
                    (error "Nothing called ~S to go before." before)))
        (after (1+ (or (position after list :key key :test #'equal)
                       (error "Nothing called ~S to go after." after))))))

(defun insert-at (item list index)
  (append (subseq list 0 index) (list item) (nthcdr index list)))

(defun set-menu (title entries &key mode role before after)
  "Make the menu TITLE have ENTRIES.  A new menu goes BEFORE or AFTER the one
so called; otherwise an application menu first, and anything else before the
Window and Help menus.  Redefining a menu keeps its place."
  (let ((old (find-menu title))
        (menu (make-menu title mode role (copy-list entries))))
    (setf *menus*
          (cond ((and old (not before) (not after))
                 (substitute menu old *menus*))
                (t
                 (let ((rest (remove old *menus*)))
                   (insert-at menu rest
                              (or (insertion-index rest #'menu-title before after)
                                  (if (eq role :application)
                                      0
                                      (or (position-if (lambda (m) (member (menu-role m) '(:windows :help)))
                                                       rest)
                                          (length rest)))))))))
    (menus-changed)
    menu))

(defmacro define-menu (title (&key mode role before after) &body entries)
  "Define the menu TITLE, with ENTRIES as *MENUS* describes them, in the menu
bar, or redefine it.  MODE makes it a menu shown only while the current
buffer is in that major mode.  For example:

  (define-menu \"Git\" (:mode \"Lisp\")
    (\"Status\" \"Git Status\")
    :separator
    (\"Commit…\" \"Git Commit\"))"
  `(set-menu ,title ',entries :mode ,mode :role ,role :before ,before :after ,after))

(defun add-menu-item (menu-title entry &key before after)
  "Put ENTRY in the menu MENU-TITLE, BEFORE or AFTER the item so titled, or
at the end.  An item with ENTRY's title already there is replaced."
  (let ((menu (or (find-menu menu-title) (error "There is no menu ~S." menu-title))))
    (let* ((entries (if (menu-entry-title entry)
                        (remove (menu-entry-title entry) (menu-entries menu)
                                :key #'menu-entry-title :test #'equal)
                        (menu-entries menu))))
      (setf (menu-entries menu)
            (insert-at entry entries
                       (or (insertion-index entries #'menu-entry-title before after)
                           (length entries)))))
    (menus-changed)
    entry))

(defun remove-menu-item (menu-title item-title)
  "Take the item ITEM-TITLE out of the menu MENU-TITLE."
  (let ((menu (or (find-menu menu-title) (error "There is no menu ~S." menu-title))))
    (setf (menu-entries menu)
          (remove item-title (menu-entries menu) :key #'menu-entry-title :test #'equal))
    (menus-changed)))

(defun remove-menu (title)
  "Take the menu TITLE out of the menu bar."
  (setf *menus* (remove (find-menu title) *menus*))
  (menus-changed))

(defun set-context-menu (mode entries)
  (setf *context-menus*
        (acons mode (copy-list entries) (remove mode *context-menus* :key #'car :test #'equal)))
  (menus-changed))

(defmacro define-context-menu ((&key mode) &body entries)
  "Define the right click's menu in buffers in the major mode MODE, or, with
no MODE, in every other buffer."
  `(set-context-menu ,mode ',entries))

(defun copy-menus ()
  "The menus as plain lists, (TITLE MODE ROLE ENTRIES), for a frontend to
build from on another thread."
  (mapcar (lambda (menu)
            (list (menu-title menu) (menu-mode menu) (menu-role menu)
                  (copy-tree (menu-entries menu))))
          *menus*))

(defun menu-entry-command (entry)
  "The command ENTRY runs, as (NAME . ARGUMENTS), or NIL when it does
something only a frontend can."
  (when (consp entry)
    (destructuring-bind (title action &key hidden &allow-other-keys) entry
      (declare (ignore title))
      (cond (hidden nil)
            ((stringp action) (list action))
            ((and (consp action) (eq (first action) :command)) (rest action))))))



;;;; The menus, from the keyboard.

(defcommand "Menu Bar" (p)
  "Choose a menu, then one of its items, in the echo area, and run it: the
menu bar, for the terminal, or from the keyboard.  A mode's menu is offered
while the current buffer is in the mode."
  "Choose from the menus."
  (declare (ignore p))
  (let* ((mode (buffer-major-mode (current-buffer)))
         (menus (remove-if-not (lambda (menu)
                                 (and (or (null (menu-mode menu))
                                          (equal (menu-mode menu) mode))
                                      (some #'menu-entry-command (menu-entries menu))))
                               *menus*))
         (menu-table (make-string-table
                      :initial-contents (mapcar (lambda (menu) (cons (menu-title menu) menu))
                                                menus))))
    (when (null menus) (editor-error "No menus."))
    (let* ((menu (nth-value 1 (prompt-for-keyword (list menu-table) :prompt "Menu: "
                                                  :help "The menu to choose from.")))
           (items (remove-if-not #'menu-entry-command (menu-entries menu)))
           (item-table (make-string-table
                        :initial-contents (mapcar (lambda (entry) (cons (first entry) entry)) items)))
           (entry (nth-value 1 (prompt-for-keyword (list item-table)
                                                   :prompt (format nil "~A: " (menu-title menu))
                                                   :help "The item to run.")))
           (command (menu-entry-command entry))
           (object (or (getstring (first command) *command-names*)
                       (editor-error "There is no command ~S." (first command)))))
      (apply (command-function object) nil (rest command)))))



;;;; Heml's menus.  A frontend adds what only it can do: the Cocoa editor
;;;; its application and Window menus, Open... and the fonts.

(define-menu "File" ()
  ("New Buffer…" "Select Buffer" :key "n")
  ("Directory…" "Dired" :key "d" :modifiers (:shift))
  ("Open Recent…" "Find Recent File")
  :separator
  ("Close Buffer…" "Kill Buffer" :key "w")
  ("Save" "Save File" :key "s")
  ("Save All" "Save All Files" :key "s" :modifiers (:option))
  ("Revert to Saved" "Revert File"))

(define-menu "Edit" ()
  ("Undo" "Undo" :key "z")
  :separator
  ("Cut" "Kill Region" :key "x")
  ("Copy" "Save Region" :key "c")
  ("Paste" "Un-Kill" :key "v")
  ("Select All" "Mark Whole Buffer" :key "a")
  :separator
  ("Find…" "Incremental Search" :key "f")
  ("Find Backward…" "Reverse Incremental Search" :key "f" :modifiers (:shift))
  ("Replace…" "Query Replace" :key "f" :modifiers (:option)))

(define-menu "View" ()
  ("Split Window" "Split Window")
  ("Split Window Side by Side" "Split Window Horizontally")
  ("Balance Windows" "Balance Windows")
  ("Next Window" "Next Window")
  ("Delete Window" "Delete Window")
  ("Delete Other Windows" "Delete Other Windows"))

(define-menu "Buffer" ()
  ("Switch to Buffer…" "Select Buffer" :key "b")
  ("List Buffers" "Bufed")
  ("Kill Buffer…" "Kill Buffer")
  :separator
  ("Lisp Mode" "Lisp Mode")
  ("Fundamental Mode" "Fundamental Mode"))

(define-menu "Lisp" ()
  ("Evaluate Defun" "Evaluate Defun")
  ("Evaluate Region" "Evaluate Region")
  ("Evaluate Expression…" "Evaluate Expression")
  ("Compile File" "Compile File")
  ("Load File…" "Load File")
  :separator
  ("Edit Definition…" "Edit Definition")
  ("Describe Symbol" "Describe Symbol")
  :separator
  ("Start Slave Thread" "Start Slave Thread")
  ("Start Slave Process" "Start Slave Process")
  ("Select Slave" "Select Slave")
  :separator
  ("Shell" "Shell"))

(define-menu "Tools" ()
  ("Grep…" "Grep")
  ("Search Files…" "Recursive Grep")
  ("Compile…" "Compile")
  :separator
  ("Next Result" "Next Result")
  ("Previous Result" "Previous Result")
  :separator
  ("Shell Command…" "Shell Command"))

(define-menu "Project" ()
  ("Find File…" "Project Find File")
  ("Search…" "Project Grep")
  ("Replace…" "Project Replace")
  ("Compile…" "Project Compile")
  ("Shell" "Project Shell")
  ("Shell Command…" "Project Shell Command")
  ("Directory" "Project Dired")
  :separator
  ("Switch Project…" "Switch Project")
  ("Buffers…" "Project Switch Buffer")
  ("List Buffers" "List Project Buffers")
  ("Kill Buffers…" "Kill Project Buffers")
  :separator
  ("Save Session" "Save Project Session")
  ("Reopen Session" "Restore Project Session")
  ("Settings" "Edit Project Settings")
  ("Forget Project" "Forget Project"))

(define-menu "Results" (:mode "Grep")
  ("Visit" "Result Goto")
  ("Show in Other Window" "Result Display")
  ("Next" "Next Result Line")
  ("Previous" "Previous Result Line")
  :separator
  ("Edit Lines" "Grep Edit")
  ("Run Again" "Grep Again")
  ("Quit" "Result Quit"))

(define-menu "Compilation" (:mode "Compilation")
  ("Visit" "Result Goto")
  ("Show in Other Window" "Result Display")
  ("Next" "Next Result Line")
  ("Previous" "Previous Result Line")
  :separator
  ("Compile Again" "Grep Again")
  ("Quit" "Result Quit"))

(define-menu "Edit Lines" (:mode "Wgrep")
  ("Write Changes" "Wgrep Finish")
  ("Cancel" "Wgrep Abort"))

(define-menu "Dired" (:mode "Dired")
  ("Open" "Dired Edit File")
  ("Open in Other Window" "Dired Edit File Other Window")
  ("View" "Dired View File")
  ("Open with Default Application" "Dired Open Externally")
  :separator
  ("Mark" "Dired Mark")
  ("Unmark" "Dired Unmark")
  ("Unmark All" "Dired Unmark All")
  ("Toggle Marks" "Dired Toggle Marks")
  ("Mark Matching…" "Dired Mark with Pattern")
  :separator
  ("Copy…" "Dired Copy")
  ("Rename…" "Dired Rename")
  ("Delete…" "Dired Delete")
  ("Make Symbolic Link…" "Dired Symlink")
  ("Change Mode…" "Dired Change Mode")
  ("Compress or Uncompress" "Dired Compress")
  ("Shell Command…" "Dired Shell Command")
  ("New Directory…" "Dired Create Directory")
  :separator
  ("Flag for Deletion" "Dired Delete File and Down Line")
  ("Delete Flagged…" "Dired Expunge Files")
  :separator
  ("Sort" "Dired Sort")
  ("Show or Hide Hidden Files" "Dired Toggle Hidden Files")
  ("Insert or Fold Subdirectory" "Dired Insert Subdirectory")
  ("Edit Names" "Dired Edit Names")
  ("Refresh" "Dired Update Buffer")
  ("Up to Parent" "Dired Up Directory")
  :separator
  ("Quit Dired" "Dired Quit"))

(define-menu "Edit Names" (:mode "Wdired")
  ("Rename as Edited" "Wdired Finish")
  ("Cancel" "Wdired Abort"))

(define-menu "Buffers" (:mode "Bufed")
  ("Visit" "Bufed Goto")
  ("Visit in Other Window" "Bufed Goto Other Window")
  ("Show in Other Window" "Bufed Display")
  :separator
  ("Mark" "Bufed Mark")
  ("Unmark" "Bufed Unmark")
  ("Unmark All" "Bufed Unmark All")
  ("Toggle Marks" "Bufed Toggle Marks")
  ("Mark Matching…" "Bufed Mark Matching")
  :separator
  ("Save" "Bufed Save File")
  ("Kill…" "Bufed Kill")
  ("Not Modified" "Bufed Not Modified")
  ("Toggle Read Only" "Bufed Toggle Read Only")
  ("Revert" "Bufed Revert")
  :separator
  ("Sort" "Bufed Sort")
  ("Show Only…" "Bufed Filter")
  ("Group by Directory" "Bufed Toggle Groups")
  ("Refresh" "Bufed Update")
  :separator
  ("Quit" "Bufed Quit"))

(define-menu "Help" (:role :help)
  ("Heml Help" "Help" :key "?")
  ("Describe Key…" "Describe Key")
  ("Describe Command…" "Describe Command")
  ("Apropos…" "Apropos"))

(define-context-menu ()
  ("Cut" "Kill Region")
  ("Copy" "Save Region")
  ("Paste" "Un-Kill")
  :separator
  ("Edit Definition" "Edit Definition")
  ("Describe Symbol" "Describe Symbol")
  ("Evaluate Region" "Evaluate Region"))

(define-context-menu (:mode "Dired")
  ("Open" "Dired Edit File")
  ("Open in Other Window" "Dired Edit File Other Window")
  ("Open with Default Application" "Dired Open Externally")
  :separator
  ("Mark" "Dired Mark")
  ("Unmark" "Dired Unmark")
  :separator
  ("Copy…" "Dired Copy")
  ("Rename…" "Dired Rename")
  ("Delete…" "Dired Delete"))

(define-context-menu (:mode "Bufed")
  ("Visit" "Bufed Goto")
  ("Visit in Other Window" "Bufed Goto Other Window")
  :separator
  ("Mark" "Bufed Mark")
  ("Unmark" "Bufed Unmark")
  :separator
  ("Save" "Bufed Save File")
  ("Kill…" "Bufed Kill"))
