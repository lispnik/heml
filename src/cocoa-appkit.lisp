;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;;; The AppKit half of the Cocoa backend: the screen the editor thread
;;;; fills, the window and view that paint it, and keyboard input.
;;;; Everything here that touches an Objective-C object runs on the main
;;;; thread; the editor thread reaches it only through CALL-ON-MAIN-THREAD
;;;; and REQUEST-REDRAW.

(in-package :hemlock.cocoa)

(defvar *font-name* nil
  "A font's PostScript or family name, or NIL for the system monospaced font.")

(defvar *font-size* 13
  "The font size in points.")

(defvar *option-is-meta* t
  "Whether Option is Meta.  NIL leaves Option to AppKit, so that it types the
characters the keyboard layout puts on it.")

(defvar *initial-columns* 100)
(defvar *initial-lines* 40)

(defparameter *margin* 4
  "Points of blank border around the character grid.")

(defparameter +appkit-path+
  "/System/Library/Frameworks/AppKit.framework/Versions/C/AppKit")

(defun log-error (where condition)
  (ignore-errors
   (format *error-output* "~&;; hemlock.cocoa: ~A: ~A~%" where condition)
   (force-output *error-output*)))


;;;; The screen

;;; What -drawRect: paints: a grid of rows, each a string and the runs of
;;; it that are not in the default font.  The editor thread writes it and
;;; the main thread reads it, both under the lock.

(defstruct (row (:constructor make-row ()))
  (text "" :type simple-string)
  ;; ((start end . font) ...), ascending, non-overlapping.  FONT is what
  ;; Hemlock's font-changes carry: an ANSI colour index or a property list.
  (runs '() :type list))

(defstruct (screen (:constructor %make-screen (columns lines)))
  (lock (bt:make-lock "hemlock.cocoa screen"))
  (columns 80 :type fixnum)
  (lines 24 :type fixnum)
  (rows #() :type simple-vector)
  (cursor-x nil)
  (cursor-y nil))

(defun make-screen (columns lines)
  (let ((screen (%make-screen columns lines)))
    (setf (screen-rows screen) (make-rows lines))
    screen))

(defun make-rows (lines)
  (let ((rows (make-array lines)))
    (dotimes (i lines rows)
      (setf (svref rows i) (make-row)))))

(defmacro with-screen-lock ((screen) &body body)
  `(bt:with-lock-held ((screen-lock ,screen))
     ,@body))

(defvar *screen* nil
  "The one screen, made with the window.")


;;;; The main-thread queue

;;; Closures for the main thread, drained by an Objective-C method on the
;;; view that -performSelectorOnMainThread: delivers.  The same scheme as
;;; lem-cocoa's main-thread.lisp, for the same reasons.

(defvar *main-thread-queue* '())
(defvar *main-thread-queue-lock* (bt:make-lock "hemlock.cocoa main-thread queue"))
(defvar *main-thread-target* nil
  "The view's pointer: the object whose -xoamaxDrain runs the queue.")

(defparameter +run-loop-modes+
  #("NSDefaultRunLoopMode" "NSEventTrackingRunLoopMode" "NSModalPanelRunLoopMode")
  "Named one by one: lem-cocoa measured that a perform queued in the
common-modes pseudo mode from a Lisp thread never ran.")

(defun drain-main-thread-queue ()
  (let ((closures (bt:with-lock-held (*main-thread-queue-lock*)
                    (prog1 (reverse *main-thread-queue*)
                      (setf *main-thread-queue* '())))))
    (dolist (closure closures)
      (handler-case (funcall closure)
        (error (condition) (log-error "main thread" condition))))))

(defun call-on-main-thread (function)
  "Run FUNCTION on the main thread, without waiting for it.  On the main
thread it is simply called."
  (cond
    ((objc.runloop:main-thread-p)
     (funcall function))
    (*main-thread-target*
     (bt:with-lock-held (*main-thread-queue-lock*)
       (push function *main-thread-queue*))
     (objc:invoke *main-thread-target*
                  "performSelectorOnMainThread:withObject:waitUntilDone:modes:"
                  (objc:coerce-to-selector "xoamaxDrain")
                  nil nil +run-loop-modes+))))

(defmacro on-main-thread (&body body)
  `(call-on-main-thread (lambda () ,@body)))


;;;; The inbox

;;; What the main thread has for the editor: key descriptors, :RESIZE and
;;; :QUIT.  A byte on the wakeup pipe tells the editor thread's iolib loop
;;; that there is something; the contents travel in the list.

(defvar *inbox* '())
(defvar *inbox-lock* (bt:make-lock "hemlock.cocoa inbox"))
(defvar *wakeup-read-fd* nil)
(defvar *wakeup-write-fd* nil)

(defun ensure-wakeup-pipe ()
  (unless *wakeup-read-fd*
    (multiple-value-bind (read write) (isys:pipe)
      ;; The main thread must never block on a full pipe; the byte only
      ;; says "look", so one already waiting is as good as another.
      (setf (isys:fd-nonblock-p write) t
            (isys:fd-nonblock-p read) t
            *wakeup-read-fd* read
            *wakeup-write-fd* write))))

(defvar *wakeup-byte*
  (make-array 1 :element-type '(unsigned-byte 8) :initial-element 1))

(defun post-to-editor (item)
  (bt:with-lock-held (*inbox-lock*)
    (push item *inbox*))
  (when *wakeup-write-fd*
    (sb-unix:unix-write *wakeup-write-fd* *wakeup-byte* 0 1)))

(defun take-inbox ()
  "Everything posted since the last call, oldest first."
  (bt:with-lock-held (*inbox-lock*)
    (prog1 (reverse *inbox*)
      (setf *inbox* '()))))

(defun inbox-empty-p ()
  (bt:with-lock-held (*inbox-lock*)
    (null *inbox*)))


;;;; The display

;;; AppKit's side: the window, the view, the fonts and their cell, and the
;;; cached colours and text attributes.  Main thread only.

(defclass display ()
  ((window :initform nil :accessor display-window)
   (view :initform nil :accessor display-view
         :documentation "The XoamaxView, as a pointer.")
   (view-object :initform nil :accessor display-view-object
                :documentation "The same view as a Lisp object, held so it is not collected.")
   (delegate :initform nil :accessor display-delegate)
   (app-delegate :initform nil :accessor display-app-delegate)
   (font :initform nil :accessor display-font)
   (bold-font :initform nil :accessor display-bold-font)
   (char-width :initform 8 :accessor display-char-width)
   (char-height :initform 16 :accessor display-char-height)
   (char-advance :initform 8d0 :accessor display-char-advance)
   (colors :initform (make-hash-table :test 'equal) :reader display-colors)
   (attributes :initform (make-hash-table :test 'equal) :reader display-attributes)))

(defvar *display* nil)

(defvar *editor-running-p* nil
  "True while the editor thread is in the command loop: what decides
whether Quit asks Hemlock or just ends the application.")

(defun null-pointer-p (pointer)
  (or (null pointer) (cffi:null-pointer-p pointer)))

(defun df (x) (float x 1d0))


;;;; Fonts

(defun make-font (name size bold)
  (let ((size (df size)))
    (or (and name
             (let ((font (objc:invoke "NSFont" "fontWithName:size:" name size)))
               (cond ((null-pointer-p font) nil)
                     (bold (objc:invoke (objc:invoke "NSFontManager" "sharedFontManager")
                                        "convertFont:toHaveTrait:" font 2)) ; NSBoldFontMask
                     (t font))))
        (objc:invoke "NSFont" "monospacedSystemFontOfSize:weight:"
                     size (if bold 0.4d0 0d0)))))

(defun measure-font (font)
  "The cell FONT wants, as (VALUES WIDTH HEIGHT ADVANCE).  The width is the
advance of M rounded, and the difference is kerned away when drawing, so
that every character sits exactly in its cell."
  (let* ((attributes (objc:invoke "NSDictionary" "dictionaryWithObject:forKey:" font "NSFont"))
         (size (objc:invoke (objc:string-to-ns-string "M") "sizeWithAttributes:" attributes))
         (advance (aref size 0))
         (ascender (objc:invoke font "ascender"))
         (descender (objc:invoke font "descender")))
    (values (max 1 (round advance))
            (max 1 (ceiling (- ascender descender)))
            advance)))

(defun install-fonts (display)
  (setf (display-font display) (objc:retain (make-font *font-name* *font-size* nil))
        (display-bold-font display) (objc:retain (make-font *font-name* *font-size* t)))
  (multiple-value-bind (width height advance) (measure-font (display-font display))
    (setf (display-char-width display) width
          (display-char-height display) height
          (display-char-advance display) advance))
  display)


;;;; Colours

;;; Hemlock's fonts are ANSI colour indexes, as the TTY backend reads them.
;;; They map onto AppKit's dynamic system colours, so light and dark
;;; appearance both come out right without any code here.

(defparameter +palette+
  #("blackColor" "systemRedColor" "systemGreenColor" "systemOrangeColor"
    "systemBlueColor" "systemPurpleColor" "systemTealColor" "whiteColor"
    "systemGrayColor" "textColor"))

(defun ns-color (display name)
  (or (gethash name (display-colors display))
      (setf (gethash name (display-colors display))
            (objc:retain (objc:invoke "NSColor" name)))))

(defun color-name-for (index)
  (if (and (integerp index) (< -1 index (length +palette+)))
      (svref +palette+ index)
      "textColor"))

(defun palette-color (display index)
  (ns-color display (color-name-for index)))

(defun foreground-color (display) (ns-color display "textColor"))
(defun background-color (display) (ns-color display "textBackgroundColor"))

(defun font-style (font)
  "FONT's foreground index, background index and boldness, each NIL for
the default."
  (cond ((integerp font) (values (if (zerop font) nil font) nil nil))
        ((consp font) (values (getf font :fg) (getf font :bg) (getf font :bold)))
        (t (values nil nil nil))))

(defun text-attributes (display color-name bold)
  "A retained NSDictionary: the font, COLOR-NAME's colour, and the kern
that makes every character advance exactly one cell."
  (let ((key (list color-name bold)))
    (or (gethash key (display-attributes display))
        (setf (gethash key (display-attributes display))
              (let ((dictionary (objc:alloc-init-object "NSMutableDictionary")))
                (objc:invoke dictionary "setObject:forKey:"
                             (if bold (display-bold-font display) (display-font display))
                             "NSFont")
                (objc:invoke dictionary "setObject:forKey:"
                             (ns-color display color-name) "NSColor")
                (objc:invoke dictionary "setObject:forKey:"
                             (objc:invoke "NSNumber" "numberWithDouble:"
                                          (df (- (display-char-width display)
                                                 (display-char-advance display))))
                             "NSKern")
                dictionary)))))


;;;; Drawing

(defun fill-rect (color x y width height)
  (when (and (plusp width) (plusp height))
    (objc:invoke color "set")
    (objc:invoke "NSBezierPath" "fillRect:" (vector (df x) (df y) (df width) (df height)))))

(defun frame-rect (color x y width height)
  (objc:invoke color "set")
  (objc:invoke "NSBezierPath" "strokeRect:"
               (vector (df (+ x 0.5)) (df (+ y 0.5)) (df (- width 1)) (df (- height 1)))))

(defun cell-x (display column)
  (+ *margin* (* column (display-char-width display))))

(defun cell-y (display line)
  (+ *margin* (* line (display-char-height display))))

(defun draw-text (display string start end column line color-name bold)
  (when (and (< start end)
             (find #\Space string :start start :end end :test-not #'char=))
    (objc:invoke (objc:string-to-ns-string (subseq string start end))
                 "drawAtPoint:withAttributes:"
                 (vector (df (cell-x display column)) (df (cell-y display line)))
                 (text-attributes display color-name bold))))

(defun draw-segment (display text start end line font)
  (multiple-value-bind (fg bg bold) (font-style font)
    (when bg
      (fill-rect (palette-color display bg)
                 (cell-x display start) (cell-y display line)
                 (* (- end start) (display-char-width display))
                 (display-char-height display)))
    (draw-text display text start (min end (length text)) start line
               (color-name-for fg) bold)))

(defun draw-row (display row line)
  (let ((text (row-text row))
        (position 0))
    (dolist (run (row-runs row))
      (destructuring-bind (start end . font) run
        (when (< position start)
          (draw-segment display text position start line nil))
        (draw-segment display text start end line font)
        (setf position end)))
    (when (< position (length text))
      (draw-segment display text position (length text) line nil))))

(defun draw-cursor (display screen key-window-p)
  (let ((x (screen-cursor-x screen))
        (y (screen-cursor-y screen)))
    (when (and x y (< -1 y (screen-lines screen)))
      (let* ((left (cell-x display x))
             (top (cell-y display y))
             (width (display-char-width display))
             (height (display-char-height display))
             (color (foreground-color display)))
        (cond (key-window-p
               (fill-rect color left top width height)
               (let ((text (row-text (svref (screen-rows screen) y))))
                 (when (< x (length text))
                   (draw-text display text x (1+ x) x y "textBackgroundColor" nil))))
              (t
               (frame-rect color left top width height)))))))

(defun draw-screen (display screen)
  (let ((bounds (objc:invoke (display-view display) "bounds")))
    (fill-rect (background-color display) 0 0 (aref bounds 2) (aref bounds 3)))
  (with-screen-lock (screen)
    (let ((rows (screen-rows screen)))
      (dotimes (line (length rows))
        (draw-row display (svref rows line) line)))
    (draw-cursor display screen
                 (objc:invoke-bool (display-window display) "isKeyWindow"))))

(defun request-redraw ()
  "Ask the view to repaint, from either thread."
  (on-main-thread
    (let ((display *display*))
      (when display
        (objc:invoke (display-view display) "setNeedsDisplay:" t)))))

(defun beep ()
  (on-main-thread (cffi:foreign-funcall "NSBeep" :void)))

(defun set-title (title)
  (on-main-thread
    (let ((display *display*))
      (when display
        (objc:invoke (display-window display) "setTitle:" title)))))


;;;; The grid size

(defun grid-size (display)
  "How many columns and lines fit the view as it is now."
  (let* ((bounds (objc:invoke (display-view display) "bounds"))
         (columns (floor (- (aref bounds 2) (* 2 *margin*)) (display-char-width display)))
         (lines (floor (- (aref bounds 3) (* 2 *margin*)) (display-char-height display))))
    (values (max 20 (min columns hi::hunk-width-limit))
            (max 6 lines))))


;;;; Keyboard

;;; -keyDown: turns the event into descriptors, which are plain data: a
;;; descriptor is (:NAMED keysym-name modifiers) or (:CHAR character
;;; modifiers), with modifiers a list of Hemlock modifier names.  The editor
;;; thread makes the key-events, since that writes to Hemlock's tables.

(defconstant +control-mask+ (ash 1 18))
(defconstant +option-mask+ (ash 1 19))
(defconstant +command-mask+ (ash 1 20))

(defparameter *function-keys*
  (append '((#xF700 . "Uparrow") (#xF701 . "Downarrow")
            (#xF702 . "Leftarrow") (#xF703 . "Rightarrow")
            (#xF727 . "Insert") (#xF728 . "Delete")
            (#xF729 . "Home") (#xF72B . "End")
            (#xF72C . "Pageup") (#xF72D . "Pagedown"))
          (loop for n from 1 to 35
                collect (cons (+ #xF704 (1- n)) (format nil "F~D" n))))
  "NSEvent's function-key characters to Hemlock's keysym names.")

(defparameter *control-keys*
  '((127 . "Backspace") (8 . "Backspace")
    (13 . "Return") (3 . "Return") (10 . "Return")
    (9 . "Tab") (25 . "Tab")
    (27 . "Escape"))
  "Control characters that name keys.  The Delete key sends 127, which
Hemlock calls Rubout and binds to deleting forward, so it is named
Backspace here, which is what the key is for.")

(defun key-name-for (character)
  (let ((code (char-code character)))
    (or (cdr (assoc code *function-keys*))
        (cdr (assoc code *control-keys*)))))

(defun event-descriptors (event)
  (let* ((flags (objc:invoke event "modifierFlags"))
         (modifiers (append (when (logtest flags +control-mask+) '("Control"))
                            (when (and *option-is-meta* (logtest flags +option-mask+))
                              '("Meta"))))
         (unmodified (objc:ns-string-to-string
                      (objc:invoke event "charactersIgnoringModifiers") t))
         (text (objc:ns-string-to-string (objc:invoke event "characters") t)))
    (cond
      ;; Command belongs to the menus, which have had their chance already.
      ((logtest flags +command-mask+) '())
      ((zerop (length unmodified)) '())
      (t
       (let* ((character (char unmodified 0))
              (name (key-name-for character)))
         (cond
           (name (list (list :named name modifiers)))
           (modifiers (list (list :char character modifiers)))
           (t
            (loop for c across (if (plusp (length text)) text unmodified)
                  collect (let ((name (key-name-for c)))
                            (if name
                                (list :named name '())
                                (list :char c '())))))))))))


;;;; The view and the delegates

(objc:define-objc-class xoamax-view ()
  ()
  (:objc-class-name "XoamaxView")
  (:objc-superclass-name "NSView"))

(objc:define-objc-class window-delegate ()
  ()
  (:objc-class-name "XoamaxWindowDelegate"))

(objc:define-objc-class app-delegate ()
  ()
  (:objc-class-name "XoamaxAppDelegate"))

(objc:define-objc-method ("isFlipped" objc:objc-bool) ((self xoamax-view))
  t)

(objc:define-objc-method ("acceptsFirstResponder" objc:objc-bool) ((self xoamax-view))
  t)

(objc:define-objc-method ("isOpaque" objc:objc-bool) ((self xoamax-view))
  t)

(objc:define-objc-method ("xoamaxDrain" :void) ((self xoamax-view))
  (drain-main-thread-queue))

(objc:define-objc-method ("drawRect:" :void)
    ((self xoamax-view) (dirty cocoa:ns-rect))
  (declare (ignore dirty))
  (handler-case
      (when (and *display* *screen*)
        (draw-screen *display* *screen*))
    (error (condition) (log-error "drawRect:" condition))))

(objc:define-objc-method ("keyDown:" :void)
    ((self xoamax-view) (event objc:objc-object-pointer))
  (handler-case
      (let ((descriptors (event-descriptors event)))
        (when descriptors
          (objc:invoke "NSCursor" "setHiddenUntilMouseMoves:" t)
          (dolist (descriptor descriptors)
            (post-to-editor descriptor))))
    (error (condition) (log-error "keyDown:" condition))))

(objc:define-objc-method ("windowDidResize:" :void)
    ((self window-delegate) (notification objc:objc-object-pointer))
  (declare (ignore notification))
  (handler-case
      (when *display*
        (multiple-value-bind (columns lines) (grid-size *display*)
          (post-to-editor (list :resize columns lines))))
    (error (condition) (log-error "windowDidResize:" condition))))

(objc:define-objc-method ("windowShouldClose:" objc:objc-bool)
    ((self window-delegate) (sender objc:objc-object-pointer))
  (declare (ignore sender))
  ;; Hemlock decides: it may want to save files first.
  (post-to-editor :quit)
  nil)

(objc:define-objc-method ("windowDidBecomeKey:" :void)
    ((self window-delegate) (notification objc:objc-object-pointer))
  (declare (ignore notification))
  (request-redraw))

(objc:define-objc-method ("windowDidResignKey:" :void)
    ((self window-delegate) (notification objc:objc-object-pointer))
  (declare (ignore notification))
  (request-redraw))

(defconstant +terminate-cancel+ 0)
(defconstant +terminate-now+ 1)

(objc:define-objc-method ("applicationShouldTerminate:" (:unsigned :long))
    ((self app-delegate) (sender objc:objc-object-pointer))
  (declare (ignore sender))
  ;; Quit asks Hemlock to exit as C-x C-c would, and the application ends
  ;; when the editor does.
  (cond (*editor-running-p*
         (post-to-editor :quit)
         +terminate-cancel+)
        (t +terminate-now+)))


;;;; Making the window

(defconstant +window-style-mask+ 15
  "Titled, closable, miniaturizable, resizable.")
(defconstant +backing-store-buffered+ 2)

(defun menu-item (title action key)
  (objc:invoke (objc:invoke "NSMenuItem" "alloc")
               "initWithTitle:action:keyEquivalent:"
               title (objc:coerce-to-selector action) key))

(defun install-main-menu (app)
  "An application menu with Hide and Quit, when there is no menu yet: a
process started from a REPL, or a bundle without a nib, has none."
  (when (null-pointer-p (objc:invoke app "mainMenu"))
    (let ((menubar (objc:invoke (objc:invoke "NSMenu" "alloc") "initWithTitle:" ""))
          (app-item (objc:alloc-init-object "NSMenuItem"))
          (app-menu (objc:invoke (objc:invoke "NSMenu" "alloc") "initWithTitle:" "Xoamax")))
      (objc:invoke app-menu "addItem:" (menu-item "Hide Xoamax" "hide:" "h"))
      (objc:invoke app-menu "addItem:" (objc:invoke "NSMenuItem" "separatorItem"))
      (objc:invoke app-menu "addItem:" (menu-item "Quit Xoamax" "terminate:" "q"))
      (objc:invoke app-item "setSubmenu:" app-menu)
      (objc:invoke menubar "addItem:" app-item)
      (objc:invoke app "setMainMenu:" menubar))))

(defun make-window (display)
  (let* ((width (+ (* 2 *margin*) (* *initial-columns* (display-char-width display))))
         (height (+ (* 2 *margin*) (* *initial-lines* (display-char-height display))))
         (rect (vector 0d0 0d0 (df width) (df height)))
         (window (objc:invoke (objc:invoke "NSWindow" "alloc")
                              "initWithContentRect:styleMask:backing:defer:"
                              rect +window-style-mask+ +backing-store-buffered+ nil))
         (view-object (make-instance 'xoamax-view))
         (view (objc:objc-object-pointer view-object))
         (delegate (make-instance 'window-delegate)))
    ;; Lisp owns the window: closing it must not free it under us.
    (objc:invoke window "setReleasedWhenClosed:" nil)
    (objc:invoke window "setTitle:" "Xoamax")
    (objc:invoke window "setContentMinSize:"
                 (vector (df (+ (* 2 *margin*) (* 20 (display-char-width display))))
                         (df (+ (* 2 *margin*) (* 6 (display-char-height display))))))
    (objc:invoke window "setContentResizeIncrements:"
                 (vector (df (display-char-width display))
                         (df (display-char-height display))))
    (objc:invoke view "setFrame:" rect)
    (objc:invoke window "setContentView:" view)
    (objc:invoke window "setDelegate:" (objc:objc-object-pointer delegate))
    (objc:invoke window "makeFirstResponder:" view)
    (objc:invoke window "center")
    (setf (display-window display) window
          (display-view display) view
          (display-view-object display) view-object
          (display-delegate display) delegate
          *main-thread-target* view)
    display))

(defun ensure-display ()
  "The display, made the first time: AppKit brought up, the menu, the
window and the screen.  Main thread only."
  (or *display*
      (progn
        (objc:ensure-objc-initialized :modules (list +appkit-path+))
        (let ((app (objc.runloop:shared-application))
              (display (make-instance 'display)))
          (install-main-menu app)
          (let ((app-delegate (make-instance 'app-delegate)))
            (setf (display-app-delegate display) app-delegate)
            (objc:invoke app "setDelegate:" (objc:objc-object-pointer app-delegate)))
          (install-fonts display)
          (make-window display)
          (multiple-value-bind (columns lines) (grid-size display)
            (setf *screen* (make-screen columns lines)))
          (ensure-wakeup-pipe)
          (setf *display* display)))))

(defun show-window ()
  (let ((display *display*))
    (objc:invoke (display-window display) "makeKeyAndOrderFront:" nil)
    (objc:invoke (objc.runloop:shared-application) "activateIgnoringOtherApps:" t)))

(defun hide-window ()
  (let ((display *display*))
    (when display
      (objc:invoke (display-window display) "orderOut:" nil))))
