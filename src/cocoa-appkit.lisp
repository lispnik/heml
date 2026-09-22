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
  "Whether the left Option key is Meta.  NIL leaves it to AppKit, so that it
types the characters the keyboard layout puts on it, dead keys included.")

(defvar *right-option-is-meta* nil
  "Whether the right Option key is Meta too.  By default it is left to
AppKit, so that one Option key is Meta and the other types characters.")

(defvar *activate* t
  "Whether showing the window makes Xoamax the active application.  The
smoke test turns it off, so that a run does not take the keyboard from
whoever is working while it runs.")

(defvar *pasteboard-name* nil
  "The pasteboard the kill ring is joined to: NIL for the general one, that
every application shares, or the name of a private one, as the smoke test
uses so as not to overwrite the user's clipboard.")

(defvar *remember-font* t
  "Whether the font chosen is kept in the user defaults for next time.")

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

(defun call-on-main-thread-and-wait (function)
  "Run FUNCTION on the main thread and return its values, waiting for it.
Only from a thread the main thread never waits for -- the editor's."
  (if (objc.runloop:main-thread-p)
      (funcall function)
      (let ((values '())
            (condition nil))
        (bt:with-lock-held (*main-thread-queue-lock*)
          (push (lambda ()
                  (handler-case (setf values (multiple-value-list (funcall function)))
                    (error (c) (setf condition c))))
                *main-thread-queue*))
        (objc:invoke *main-thread-target*
                     "performSelectorOnMainThread:withObject:waitUntilDone:modes:"
                     (objc:coerce-to-selector "xoamaxDrain")
                     nil t +run-loop-modes+)
        (when condition (error condition))
        (values-list values))))


;;;; The clipboard

;;; Hemlock's kill ring and the general pasteboard, joined as Emacs joins
;;; them (killcoms.lisp).  The pasteboard's change count says whether
;;; anyone has written to it since this process last did, or last read it:
;;; only then is its text news to the kill ring.  Main thread only.

(defparameter +plain-text-type+ "public.utf8-plain-text")

(defvar *pasteboard-change-count* nil
  "The general pasteboard's change count when this process last wrote or
read it, or NIL before either.")

(defun general-pasteboard ()
  (if *pasteboard-name*
      (objc:invoke "NSPasteboard" "pasteboardWithName:" *pasteboard-name*)
      (objc:invoke "NSPasteboard" "generalPasteboard")))

(defun write-pasteboard (text)
  (let ((pasteboard (general-pasteboard)))
    (objc:invoke pasteboard "clearContents")
    (objc:invoke pasteboard "setString:forType:" text +plain-text-type+)
    (setf *pasteboard-change-count* (objc:invoke pasteboard "changeCount"))))

(defun read-pasteboard-if-changed ()
  "The pasteboard's text, if something other than this process has put it
there since it last looked; otherwise NIL."
  (let* ((pasteboard (general-pasteboard))
         (count (objc:invoke pasteboard "changeCount")))
    (unless (eql count *pasteboard-change-count*)
      (setf *pasteboard-change-count* count)
      (let ((string (objc:invoke pasteboard "stringForType:" +plain-text-type+)))
        (unless (null-pointer-p string)
          (objc:ns-string-to-string string t))))))


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
   (marked-text :initform nil :accessor display-marked-text
                :documentation "Text an input method is composing, shown at the cursor
until it is committed or dropped.")
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
  "Make the fonts from *FONT-NAME* and *FONT-SIZE* and remeasure the cell.
The text attribute cache goes with the old fonts: its dictionaries name them."
  (let ((old (list (display-font display) (display-bold-font display))))
    (setf (display-font display) (objc:retain (make-font *font-name* *font-size* nil))
          (display-bold-font display) (objc:retain (make-font *font-name* *font-size* t)))
    (multiple-value-bind (width height advance) (measure-font (display-font display))
      (setf (display-char-width display) width
            (display-char-height display) height
            (display-char-advance display) advance))
    (loop for dictionary being the hash-values of (display-attributes display)
          do (objc:release dictionary))
    (clrhash (display-attributes display))
    (dolist (font old)
      (when font (objc:release font))))
  display)

;;; The font chosen last is kept in the user defaults: the application's,
;;; or the SBCL process's when the editor runs from a REPL.

(defparameter +font-name-key+ "XoamaxFontName")
(defparameter +font-size-key+ "XoamaxFontSize")
(defparameter +default-font-size+ 13)

(defun user-defaults ()
  (objc:invoke "NSUserDefaults" "standardUserDefaults"))

(defun restore-font-choice ()
  (when *remember-font*
    (restore-saved-font)))

(defun restore-saved-font ()
  (let* ((defaults (user-defaults))
         (name (objc:invoke defaults "stringForKey:" +font-name-key+))
         (size (objc:invoke defaults "doubleForKey:" +font-size-key+)))
    (unless (null-pointer-p name)
      (setf *font-name* (objc:ns-string-to-string name)))
    (when (plusp size)
      (setf *font-size* size))))

(defun save-font-choice ()
  (when *remember-font*
    (save-font)))

(defun save-font ()
  (let ((defaults (user-defaults)))
    (if *font-name*
        (objc:invoke defaults "setObject:forKey:" *font-name* +font-name-key+)
        (objc:invoke defaults "removeObjectForKey:" +font-name-key+))
    (objc:invoke defaults "setDouble:forKey:" (df *font-size*) +font-size-key+)))

(defun fit-window-to-cell (display)
  "Resizing snaps to whole cells, and the window cannot be made smaller than
a usable grid."
  (let ((window (display-window display)))
    (objc:invoke window "setContentMinSize:"
                 (vector (df (+ (* 2 *margin*) (* 20 (display-char-width display))))
                         (df (+ (* 2 *margin*) (* 6 (display-char-height display))))))
    (objc:invoke window "setContentResizeIncrements:"
                 (vector (df (display-char-width display))
                         (df (display-char-height display))))))

(defun change-font (&key (name *font-name*) (size *font-size*))
  "Use font NAME, or the system monospaced font for NIL, at SIZE points.
The window keeps its size and the grid is fitted to it again.  Main thread."
  (setf *font-name* name
        *font-size* (max 6 (min 96 size)))
  (let ((display *display*))
    (install-fonts display)
    (fit-window-to-cell display)
    (save-font-choice)
    (multiple-value-bind (columns lines) (grid-size display)
      (post-to-editor (list :resize columns lines)))
    (request-redraw)))

(defun change-font-size (delta)
  (change-font :size (if delta (+ *font-size* delta) +default-font-size+)))


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
  (cond ((and (integerp index) (< -1 index (length +palette+)))
         (svref +palette+ index))
        ;; What the Cocoa backend makes the active region's font, so that
        ;; a region looks like a selection anywhere else on the Mac.
        ((eq index :selection) "selectedTextBackgroundColor")
        (t "textColor")))

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

;;; Every character of a row is at the column of its index.  A run of
;;; ASCII is drawn at once, kerned to the cell; anything else alone at its
;;; column, since a fallback font's advance has nothing to do with the cell,
;;; and a wide character's filler not at all -- the character before it has
;;; the room.
;;;
(defun draw-text (display string start end line color-name bold)
  (let ((attributes (text-attributes display color-name bold)))
    (flet ((draw (from to)
             (when (find #\Space string :start from :end to :test-not #'char=)
               (objc:invoke (objc:string-to-ns-string (subseq string from to))
                            "drawAtPoint:withAttributes:"
                            (vector (df (cell-x display from)) (df (cell-y display line)))
                            attributes))))
      (loop with i = start
            while (< i end)
            do (let ((character (char string i)))
                 (cond ((char= character hi::wide-character-filler)
                        (incf i))
                       ((< (char-code character) 128)
                        (let ((j (or (position-if (lambda (c) (>= (char-code c) 128))
                                                  string :start i :end end)
                                     end)))
                          (draw i j)
                          (setf i j)))
                       (t
                        (draw i (1+ i))
                        (incf i))))))))

(defun wide-at-p (string index)
  "Whether the character at INDEX of STRING covers the next cell too."
  (and (< (1+ index) (length string))
       (char= (char string (1+ index)) hi::wide-character-filler)))

(defun draw-segment (display text start end line font)
  (multiple-value-bind (fg bg bold) (font-style font)
    (when bg
      (fill-rect (palette-color display bg)
                 (cell-x display start) (cell-y display line)
                 (* (- end start) (display-char-width display))
                 (display-char-height display)))
    (draw-text display text start (min end (length text)) line
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
      (let* ((text (row-text (svref (screen-rows screen) y)))
             (left (cell-x display x))
             (top (cell-y display y))
             (width (* (if (wide-at-p text x) 2 1) (display-char-width display)))
             (height (display-char-height display))
             (color (foreground-color display)))
        (cond (key-window-p
               (fill-rect color left top width height)
               (when (< x (length text))
                 (draw-text display text x (1+ x) y "textBackgroundColor" nil)))
              (t
               (frame-rect color left top width height)))))))

(defun draw-marked-text (display screen)
  "An input method's uncommitted text at the cursor, in reverse video and
underlined, laid out in cells as a row's text is."
  (let ((marked (display-marked-text display))
        (x (screen-cursor-x screen))
        (y (screen-cursor-y screen)))
    (when (and marked x y)
      (let* ((cells (with-output-to-string (out)
                      (loop for c across marked
                            do (write-char c out)
                               (when (hi::wide-character-p c)
                                 (write-char hi::wide-character-filler out)))))
             ;; Laid out as if the row began at the cursor.
             (text (concatenate 'string (make-string x :initial-element #\Space) cells))
             (left (cell-x display x))
             (top (cell-y display y))
             (width (* (length cells) (display-char-width display)))
             (height (display-char-height display)))
        (fill-rect (foreground-color display) left top width height)
        (draw-text display text x (length text) y "textBackgroundColor" nil)
        (fill-rect (background-color display) left (+ top height -1) width 1)))))

(defun draw-screen (display screen)
  (let ((bounds (objc:invoke (display-view display) "bounds")))
    (fill-rect (background-color display) 0 0 (aref bounds 2) (aref bounds 3)))
  (with-screen-lock (screen)
    (let ((rows (screen-rows screen)))
      (dotimes (line (length rows))
        (draw-row display (svref rows line) line)))
    (draw-cursor display screen
                 (objc:invoke-bool (display-window display) "isKeyWindow"))
    (draw-marked-text display screen)))

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

(defconstant +left-option-mask+ #x20
  "NX_DEVICELALTKEYMASK: the device-dependent bit for the left Option key.")
(defconstant +right-option-mask+ #x40
  "NX_DEVICERALTKEYMASK: the same for the right one.")

(defun meta-p (flags)
  "Whether FLAGS hold an Option key that is Meta.  An event that does not
say which Option key -- a synthetic one -- counts as the left."
  (and (logtest flags +option-mask+)
       (let ((left (logtest flags +left-option-mask+))
             (right (logtest flags +right-option-mask+)))
         (or (and *option-is-meta* (or left (not right)))
             (and *right-option-is-meta* right)))))

(defun character-descriptor (character &optional modifiers)
  (let ((name (key-name-for character)))
    (if name
        (list :named name modifiers)
        (list :char character modifiers))))

(defun text-descriptors (string)
  "Typed text, one descriptor a character."
  (map 'list #'character-descriptor string))

(defun event-descriptors (event)
  "The descriptors for a key-down EVENT that the view handles itself, and
true, or NIL and NIL for an event that is text for the input context:
dead keys, input methods, and Option as AppKit uses it."
  (let* ((flags (objc:invoke event "modifierFlags"))
         (modifiers (append (when (logtest flags +control-mask+) '("Control"))
                            (when (meta-p flags) '("Meta"))))
         (unmodified (objc:ns-string-to-string
                      (objc:invoke event "charactersIgnoringModifiers") t)))
    (cond
      ;; Command belongs to the menus, which have had their chance already.
      ((logtest flags +command-mask+) (values '() t))
      ((zerop (length unmodified)) (values '() nil))
      (t
       (let* ((character (char unmodified 0))
              (name (key-name-for character)))
         (cond
           (name (values (list (list :named name modifiers)) t))
           ;; With a modifier the key is the unshifted character the event
           ;; names, C-x as "x", with Shift already applied: M-< as "<".
           (modifiers (values (list (list :char character modifiers)) t))
           (t (values '() nil))))))))


;;;; Mouse

;;; A mouse descriptor is (:MOUSE keysym-name modifiers column line), in
;;; cells from the top left of the grid.  The editor thread works out which
;;; window and which of its lines that is.

(defconstant +shift-mask+ (ash 1 17))

(defun event-modifiers (event)
  (let ((flags (objc:invoke event "modifierFlags")))
    (append (when (logtest flags +shift-mask+) '("Shift"))
            (when (logtest flags +control-mask+) '("Control"))
            (when (logtest flags +option-mask+) '("Meta"))
            (when (logtest flags +command-mask+) '("Super")))))

(defun event-cell (event)
  "The cell under EVENT's pointer, as (VALUES COLUMN LINE)."
  (let* ((display *display*)
         (point (objc:invoke (display-view display) "convertPoint:fromView:"
                             (objc:invoke event "locationInWindow") nil)))
    (values (max 0 (floor (- (aref point 0) *margin*) (display-char-width display)))
            (max 0 (floor (- (aref point 1) *margin*) (display-char-height display))))))

(defvar *drag-cell* nil
  "The cell the last Leftdown or Leftdrag was posted for, so that a drag
posts only when the pointer reaches another cell.")

(defun post-mouse (name event)
  (multiple-value-bind (column line) (event-cell event)
    (post-to-editor (list :mouse name (event-modifiers event) column line))
    (cons column line)))

(defvar *scroll-remainder* 0d0
  "The part of a line scrolled but not yet posted: a trackpad reports its
movement in points, a fraction of a line at a time.")

(defparameter *lines-per-wheel-step* 3
  "Lines a notch of a mouse wheel scrolls.  A trackpad scrolls by distance.")

(defun post-scroll (event)
  (let* ((delta (objc:invoke event "scrollingDeltaY"))
         (lines (+ *scroll-remainder*
                   (if (objc:invoke-bool event "hasPreciseScrollingDeltas")
                       (/ delta (display-char-height *display*))
                       (* delta *lines-per-wheel-step*))))
         (whole (truncate lines)))
    (setf *scroll-remainder* (- lines whole))
    ;; A positive delta moves the content down, showing earlier lines.
    (loop repeat (min 100 (abs whole))
          do (post-mouse (if (plusp whole) "Scrollup" "Scrolldown") event))))


;;;; The view and the delegates

(objc:define-objc-class xoamax-view ()
  ()
  (:objc-class-name "XoamaxView")
  (:objc-superclass-name "NSView")
  ;; So that AppKit's input context talks to the view: dead keys, input
  ;; methods, and their marked text.  The methods are below.
  (:objc-protocols "NSTextInputClient"))

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

;;; A click on the window while another application is active both brings
;;; it forward and lands, as in Emacs, instead of only activating it.
(objc:define-objc-method ("acceptsFirstMouse:" objc:objc-bool)
    ((self xoamax-view) (event objc:objc-object-pointer))
  (declare (ignore event))
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

;;; Named keys and keys with Control or Meta are Hemlock's directly.
;;; Plain typing goes through AppKit's input context, which composes dead
;;; keys and runs input methods, and comes back as -insertText: or
;;; -setMarkedText:.  While an input method holds marked text, every key is
;;; its to interpret.
;;;
(objc:define-objc-method ("keyDown:" :void)
    ((self xoamax-view pointer) (event objc:objc-object-pointer))
  (handler-case
      (multiple-value-bind (descriptors direct) (event-descriptors event)
        (objc:invoke "NSCursor" "setHiddenUntilMouseMoves:" t)
        (if (and direct (null (display-marked-text *display*)))
            (dolist (descriptor descriptors)
              (post-to-editor descriptor))
            (unless (and direct (null descriptors))
              (objc:invoke pointer "interpretKeyEvents:"
                           (objc:invoke "NSArray" "arrayWithObject:" event)))))
    (error (condition) (log-error "keyDown:" condition))))

;;;; NSTextInputClient

;;; What the input context calls.  The document is Hemlock's, not
;;; AppKit's, so there is no text to hand back and no selection to report:
;;; only the marked text, which is shown at the cursor until it is
;;; committed, and where the cursor is, for the candidate window.

(defun text-of (string)
  "STRING's characters: the input context may pass an NSAttributedString."
  (if (objc:invoke-bool string "isKindOfClass:"
                        (objc:coerce-to-objc-class "NSAttributedString"))
      (objc:ns-string-to-string (objc:invoke string "string") t)
      (objc:ns-string-to-string string t)))

(defun set-marked-text (text)
  (setf (display-marked-text *display*) (and text (plusp (length text)) text))
  (request-redraw))

(objc:define-objc-method ("insertText:replacementRange:" :void)
    ((self xoamax-view) (string objc:objc-object-pointer) (range cocoa:ns-range))
  (declare (ignore range))
  (handler-case
      (progn
        (set-marked-text nil)
        (dolist (descriptor (text-descriptors (text-of string)))
          (post-to-editor descriptor)))
    (error (condition) (log-error "insertText:" condition))))

(objc:define-objc-method ("setMarkedText:selectedRange:replacementRange:" :void)
    ((self xoamax-view) (string objc:objc-object-pointer)
     (selected cocoa:ns-range) (replacement cocoa:ns-range))
  (declare (ignore selected replacement))
  (handler-case (set-marked-text (text-of string))
    (error (condition) (log-error "setMarkedText:" condition))))

(objc:define-objc-method ("unmarkText" :void) ((self xoamax-view))
  (set-marked-text nil))

(objc:define-objc-method ("hasMarkedText" objc:objc-bool) ((self xoamax-view))
  (not (null (display-marked-text *display*))))

(objc:define-objc-method ("markedRange" cocoa:ns-range) ((self xoamax-view))
  (let ((marked (display-marked-text *display*)))
    (if marked
        (cons 0 (length marked))
        (cons cocoa:ns-not-found 0))))

(objc:define-objc-method ("selectedRange" cocoa:ns-range) ((self xoamax-view))
  (let ((marked (display-marked-text *display*)))
    (cons (if marked (length marked) 0) 0)))

(objc:define-objc-method ("attributedSubstringForProposedRange:actualRange:"
                          objc:objc-object-pointer)
    ((self xoamax-view) (range cocoa:ns-range) (actual (:pointer :void)))
  (declare (ignore range actual))
  (cffi:null-pointer))

(objc:define-objc-method ("validAttributesForMarkedText" objc:objc-object-pointer)
    ((self xoamax-view))
  (objc:invoke "NSArray" "array"))

(objc:define-objc-method ("firstRectForCharacterRange:actualRange:" cocoa:ns-rect)
    ((self xoamax-view pointer) (range cocoa:ns-range) (actual (:pointer :void)))
  (declare (ignore range actual))
  ;; Where the candidate window goes: at the cursor, in screen coordinates.
  (let* ((display *display*)
         (screen *screen*)
         (x (or (screen-cursor-x screen) 0))
         (y (or (screen-cursor-y screen) 0))
         (in-window (objc:invoke pointer "convertRect:toView:"
                                 (vector (df (cell-x display x)) (df (cell-y display y))
                                         (df (display-char-width display))
                                         (df (display-char-height display)))
                                 nil)))
    (objc:invoke (display-window display) "convertRectToScreen:" in-window)))

(objc:define-objc-method ("characterIndexForPoint:" (:unsigned :long))
    ((self xoamax-view) (point cocoa:ns-point))
  (declare (ignore point))
  0)

(defparameter *command-selector-keys*
  '(("insertNewline:" . "Return") ("insertLineBreak:" . "Return")
    ("insertTab:" . "Tab") ("insertBacktab:" . "Tab")
    ("deleteBackward:" . "Backspace") ("deleteForward:" . "Delete")
    ("cancelOperation:" . "Escape")
    ("moveLeft:" . "Leftarrow") ("moveRight:" . "Rightarrow")
    ("moveUp:" . "Uparrow") ("moveDown:" . "Downarrow")
    ("scrollPageUp:" . "Pageup") ("scrollPageDown:" . "Pagedown"))
  "The editing selectors the input context sends for keys it did not
consume, and the keys they are.")

(objc:define-objc-method ("doCommandBySelector:" :void)
    ((self xoamax-view) (selector objc:sel))
  (let ((name (cdr (assoc (objc:selector-name selector) *command-selector-keys*
                          :test #'string=))))
    (when name
      (post-to-editor (list :named name '())))))

;;;; Menu actions

;;; Every item of the menus, and of the context menu, is the application
;;; delegate's -xoamaxMenuItem:, and its tag says which action it is:
;;;
;;;   (:command name arg ...)  a Hemlock command, run by the command loop
;;;   (:call function)         a function called on the main thread
;;;   (:font-size delta)       bigger, smaller, or with NIL the default size
;;;
;;; Items for AppKit's own selectors -- hide:, terminate:, toggleFullScreen:
;;; -- keep them, with no target, so that the responder chain finds them.

(defvar *menu-actions* (make-array 0 :adjustable t :fill-pointer t))

(defun menu-action-tag (action)
  (or (position action *menu-actions* :test #'equal)
      (vector-push-extend action *menu-actions*)))

(defun perform-menu-action (action)
  (ecase (first action)
    (:command (post-to-editor (cons :command (rest action))))
    (:call (funcall (second action)))
    (:font-size (change-font-size (second action)))))

(objc:define-objc-method ("xoamaxMenuItem:" :void)
    ((self app-delegate) (sender objc:objc-object-pointer))
  (handler-case
      (when *display*
        (perform-menu-action (aref *menu-actions* (objc:invoke sender "tag"))))
    (error (condition) (log-error "xoamaxMenuItem:" condition))))

(defun show-font-panel ()
  (let ((manager (objc:invoke "NSFontManager" "sharedFontManager")))
    (objc:invoke manager "setSelectedFont:isMultiple:" (display-font *display*) nil)
    (objc:invoke manager "orderFrontFontPanel:" nil)))

(objc:define-objc-method ("changeFont:" :void)
    ((self xoamax-view) (sender objc:objc-object-pointer))
  (handler-case
      (let ((font (objc:invoke sender "convertFont:" (display-font *display*))))
        (change-font :name (objc:ns-string-to-string (objc:invoke font "fontName"))
                     :size (objc:invoke font "pointSize")))
    (error (condition) (log-error "changeFont:" condition))))

(defconstant +modal-response-ok+ 1)

(defun choose-files-to-open ()
  "The Open panel; the files chosen are visited as files from Finder are."
  (let ((panel (objc:invoke "NSOpenPanel" "openPanel")))
    (objc:invoke panel "setAllowsMultipleSelection:" t)
    (objc:invoke panel "setCanChooseDirectories:" t)
    (when (= (objc:invoke panel "runModal") +modal-response-ok+)
      (let ((urls (objc:invoke panel "URLs")))
        (dotimes (i (objc:invoke urls "count"))
          (post-to-editor
           (list :open (objc:ns-string-to-string
                        (objc:invoke (objc:invoke urls "objectAtIndex:" i) "path")))))))))

(defun choose-file-to-save-as ()
  "The Save panel; the current buffer is written to the file chosen."
  (let ((panel (objc:invoke "NSSavePanel" "savePanel")))
    (when (= (objc:invoke panel "runModal") +modal-response-ok+)
      (post-to-editor
       (list :command "Write File"
             (objc:ns-string-to-string (objc:invoke (objc:invoke panel "URL") "path")))))))

(defun open-settings ()
  "The init file, which is where Hemlock's settings are: the first that
exists of those Hemlock loads, or the first of them to create."
  (let* ((home (user-homedir-pathname))
         (names (mapcar (lambda (name) (merge-pathnames name home))
                        '(".hemlock.lisp" ".hemlock/hemlock.lisp" ".hemlock-init.lisp"))))
    (post-to-editor
     (list :open (namestring (or (find-if #'probe-file names) (first names)))))))

(defun show-about ()
  (let ((options (objc:alloc-init-object "NSMutableDictionary")))
    (objc:invoke options "setObject:forKey:" "Xoamax" "ApplicationName")
    (objc:invoke options "setObject:forKey:"
                 (princ-to-string hi::*hemlock-version*) "ApplicationVersion")
    (objc:invoke options "setObject:forKey:"
                 (format nil "Hemlock on ~A ~A" (lisp-implementation-type)
                         (lisp-implementation-version))
                 "Version")
    (objc:invoke (objc.runloop:shared-application)
                 "orderFrontStandardAboutPanelWithOptions:" options)
    (objc:release options)))

(defmacro define-mouse-method (selector &body body)
  `(objc:define-objc-method (,selector :void)
       ((self xoamax-view) (event objc:objc-object-pointer))
     (handler-case (when *display* ,@body)
       (error (condition) (log-error ,selector condition)))))

(define-mouse-method "mouseDown:"
  ;; A second and a third click in a row are keys of their own: they
  ;; select a word and a line.
  (setf *drag-cell* (post-mouse (case (objc:invoke event "clickCount")
                                  (1 "Leftdown")
                                  (2 "Doubleleftdown")
                                  (t "Tripleleftdown"))
                                event)))

(define-mouse-method "mouseDragged:"
  (unless (equal (multiple-value-list (event-cell event))
                 (list (car *drag-cell*) (cdr *drag-cell*)))
    (setf *drag-cell* (post-mouse "Leftdrag" event))))

(define-mouse-method "mouseUp:"
  (setf *drag-cell* nil)
  (post-mouse "Leftup" event))

;;; A right click, or a Control-click, is AppKit's: -rightMouseDown: asks
;;; for this menu and shows it.  The click goes to the editor too, so that
;;; point moves to it unless it is in the selection.
;;;
(objc:define-objc-method ("menuForEvent:" objc:objc-object-pointer)
    ((self xoamax-view) (event objc:objc-object-pointer))
  (handler-case
      (progn
        (when *display* (post-mouse "Rightdown" event))
        (context-menu))
    (error (condition)
      (log-error "menuForEvent:" condition)
      (cffi:null-pointer))))

;;; The middle button, and any others, which Hemlock has no names for.
(define-mouse-method "otherMouseDown:"
  (when (= 2 (objc:invoke event "buttonNumber"))
    (post-mouse "Middledown" event)))
(define-mouse-method "otherMouseUp:"
  (when (= 2 (objc:invoke event "buttonNumber"))
    (post-mouse "Middleup" event)))

(define-mouse-method "scrollWheel:" (post-scroll event))

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

;;; Files from Finder -- Open With, a drop on the Dock icon, `open -a`
;;; -- arrive here, at launch as well as later, and are the editor's to
;;; visit, as files named on its command line are.
;;;
(objc:define-objc-method ("application:openURLs:" :void)
    ((self app-delegate) (app objc:objc-object-pointer) (urls objc:objc-object-pointer))
  (declare (ignore app))
  (handler-case
      (dotimes (i (objc:invoke urls "count"))
        (let ((url (objc:invoke urls "objectAtIndex:" i)))
          (when (objc:invoke-bool url "isFileURL")
            (post-to-editor
             (list :open (objc:ns-string-to-string (objc:invoke url "path")))))))
    (error (condition) (log-error "application:openURLs:" condition))))

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

;;;; The menus

(defparameter *menu-bar*
  '(("Xoamax"
     ("About Xoamax" (:call show-about))
     :separator
     ("Settings…" (:call open-settings) :key ",")
     :separator
     ("Services" :services)
     :separator
     ("Hide Xoamax" (:selector "hide:") :key "h")
     ("Hide Others" (:selector "hideOtherApplications:") :key "h" :modifiers (:option))
     ("Show All" (:selector "unhideAllApplications:"))
     :separator
     ("Quit Xoamax" (:selector "terminate:") :key "q"))
    ("File"
     ("New Buffer…" (:command "Select Buffer") :key "n")
     ("Open…" (:call choose-files-to-open) :key "o")
     :separator
     ("Close Buffer…" (:command "Kill Buffer") :key "w")
     ("Save" (:command "Save File") :key "s")
     ("Save As…" (:call choose-file-to-save-as) :key "s" :modifiers (:shift))
     ("Save All" (:command "Save All Files") :key "s" :modifiers (:option))
     ("Revert to Saved" (:command "Revert File")))
    ("Edit"
     ("Undo" (:command "Undo") :key "z")
     :separator
     ("Cut" (:command "Kill Region") :key "x")
     ("Copy" (:command "Save Region") :key "c")
     ("Paste" (:command "Un-Kill") :key "v")
     ("Select All" (:command "Mark Whole Buffer") :key "a")
     :separator
     ("Find…" (:command "Incremental Search") :key "f")
     ("Find Backward…" (:command "Reverse Incremental Search") :key "f" :modifiers (:shift))
     ("Replace…" (:command "Query Replace") :key "f" :modifiers (:option))
     :separator
     ("Emoji & Symbols" (:selector "orderFrontCharacterPalette:")
      :key " " :modifiers (:control)))
    ("View"
     ("Show Fonts" (:call show-font-panel) :key "t")
     ("Bigger" (:font-size 1) :key "+")
     ;; Cmd-= is Cmd-+ without the Shift nobody presses.
     ("Bigger" (:font-size 1) :key "=" :hidden t)
     ("Smaller" (:font-size -1) :key "-")
     ("Default Size" (:font-size nil) :key "0")
     :separator
     ("Split Window" (:command "Split Window"))
     ("Next Window" (:command "Next Window"))
     ("Delete Window" (:command "Delete Window"))
     ("Delete Next Window" (:command "Delete Next Window"))
     :separator
     ("Enter Full Screen" (:selector "toggleFullScreen:") :key "f" :modifiers (:control)))
    ("Buffer"
     ("Switch to Buffer…" (:command "Select Buffer") :key "b")
     ("List Buffers" (:command "Bufed"))
     ("Kill Buffer…" (:command "Kill Buffer"))
     :separator
     ("Lisp Mode" (:command "Lisp Mode"))
     ("Fundamental Mode" (:command "Fundamental Mode")))
    ("Lisp"
     ("Evaluate Defun" (:command "Evaluate Defun"))
     ("Evaluate Region" (:command "Evaluate Region"))
     ("Evaluate Expression…" (:command "Evaluate Expression"))
     ("Compile File" (:command "Compile File"))
     ("Load File…" (:command "Load File"))
     :separator
     ("Edit Definition…" (:command "Edit Definition"))
     ("Describe Symbol" (:command "Describe Symbol"))
     :separator
     ("Start Slave Thread" (:command "Start Slave Thread"))
     ("Start Slave Process" (:command "Start Slave Process"))
     ("Select Slave" (:command "Select Slave"))
     :separator
     ("Shell" (:command "Shell")))
    ("Window" :windows
     ("Minimize" (:selector "performMiniaturize:") :key "m")
     ("Zoom" (:selector "performZoom:"))
     :separator
     ("Bring All to Front" (:selector "arrangeInFront:")))
    ("Help" :help
     ("Xoamax Help" (:command "Help") :key "?")
     ("Describe Key…" (:command "Describe Key"))
     ("Describe Command…" (:command "Describe Command"))
     ("Apropos…" (:command "Apropos"))))
  "The menu bar.  A menu is (title [role] entry ...), with ROLE :WINDOWS or
:HELP for the menus AppKit keeps up itself; an entry is :SEPARATOR, or
(title action &key key modifiers hidden) -- KEY the Command-key equivalent,
MODIFIERS any of :SHIFT, :OPTION and :CONTROL besides -- or (title
:SERVICES).")

(defparameter *context-menu*
  '(("Cut" (:command "Kill Region"))
    ("Copy" (:command "Save Region"))
    ("Paste" (:command "Un-Kill"))
    :separator
    ("Edit Definition" (:command "Edit Definition"))
    ("Describe Symbol" (:command "Describe Symbol"))
    ("Evaluate Region" (:command "Evaluate Region")))
  "The right click's menu: entries as in *MENU-BAR*.")

(defconstant +shift-key-mask+ (ash 1 17))
(defconstant +control-key-mask+ (ash 1 18))
(defconstant +option-key-mask+ (ash 1 19))
(defconstant +command-key-mask+ (ash 1 20))

(defun make-menu (title)
  (objc:invoke (objc:invoke "NSMenu" "alloc") "initWithTitle:" title))

(defun menu-entry (entry target)
  "The NSMenuItem for ENTRY of a menu, its actions TARGET's."
  (if (eq entry :separator)
      (objc:invoke "NSMenuItem" "separatorItem")
      (destructuring-bind (title action &key key modifiers hidden) entry
        (let ((item (objc:alloc-init-object "NSMenuItem")))
          (objc:invoke item "setTitle:" title)
          (cond ((eq action :services)
                 (let ((services (make-menu title)))
                   (objc:invoke item "setSubmenu:" services)
                   (objc:invoke (objc.runloop:shared-application)
                                "setServicesMenu:" services)))
                ((eq (first action) :selector)
                 (objc:invoke item "setAction:" (objc:coerce-to-selector (second action))))
                (t
                 (objc:invoke item "setAction:" (objc:coerce-to-selector "xoamaxMenuItem:"))
                 (objc:invoke item "setTarget:" target)
                 (objc:invoke item "setTag:" (menu-action-tag action))))
          (when key
            (objc:invoke item "setKeyEquivalent:" key)
            (objc:invoke item "setKeyEquivalentModifierMask:"
                         (logior +command-key-mask+
                                 (if (member :shift modifiers) +shift-key-mask+ 0)
                                 (if (member :option modifiers) +option-key-mask+ 0)
                                 (if (member :control modifiers) +control-key-mask+ 0))))
          (when hidden
            (objc:invoke item "setHidden:" t)
            (objc:invoke item "setAllowsKeyEquivalentWhenHidden:" t))
          item))))

(defun build-menu (title entries target)
  (let ((menu (make-menu title)))
    (dolist (entry entries menu)
      (objc:invoke menu "addItem:" (menu-entry entry target)))))

(defun install-main-menu (app target)
  "The menu bar of *MENU-BAR*, when there is no menu yet: a process started
from a REPL, or a bundle without a nib, has none."
  (when (null-pointer-p (objc:invoke app "mainMenu"))
    (let ((menubar (make-menu "")))
      (dolist (spec *menu-bar*)
        (destructuring-bind (title . entries) spec
          (let* ((role (when (keywordp (first entries)) (pop entries)))
                 (menu (build-menu title entries target))
                 (item (objc:alloc-init-object "NSMenuItem")))
            (objc:invoke item "setTitle:" title)
            (objc:invoke item "setSubmenu:" menu)
            (objc:invoke menubar "addItem:" item)
            (case role
              (:windows (objc:invoke app "setWindowsMenu:" menu))
              (:help (objc:invoke app "setHelpMenu:" menu))))))
      (objc:invoke app "setMainMenu:" menubar))))

(defvar *context-menu-object* nil)

(defun context-menu ()
  (or *context-menu-object*
      (setf *context-menu-object*
            (objc:retain
             (build-menu "" *context-menu*
                         (objc:objc-object-pointer (display-app-delegate *display*)))))))

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
    (fit-window-to-cell display)
    display))

(defun use-icon-if-unbundled (app)
  "Give a process started from a REPL the application's icon in the Dock.
A bundle has its own, from its Info.plist."
  (let ((path (ignore-errors
               (asdf:system-relative-pathname :hemlock.cocoa "resources/xoamax.png"))))
    (when (and path (probe-file path)
               (null-pointer-p (objc:invoke (objc:invoke "NSBundle" "mainBundle")
                                            "bundleIdentifier")))
      (let ((image (objc:invoke (objc:invoke "NSImage" "alloc") "initWithContentsOfFile:"
                                (namestring path))))
        (unless (null-pointer-p image)
          (objc:invoke app "setApplicationIconImage:" image))))))

(defun ensure-display ()
  "The display, made the first time: AppKit brought up, the menu, the
window and the screen.  Main thread only."
  (or *display*
      (progn
        (objc:ensure-objc-initialized :modules (list +appkit-path+))
        (let ((app (objc.runloop:shared-application))
              (display (make-instance 'display)))
          (let ((app-delegate (make-instance 'app-delegate)))
            (setf (display-app-delegate display) app-delegate)
            (objc:invoke app "setDelegate:" (objc:objc-object-pointer app-delegate))
            (install-main-menu app (objc:objc-object-pointer app-delegate)))
          (use-icon-if-unbundled app)
          (restore-font-choice)
          (install-fonts display)
          (make-window display)
          (multiple-value-bind (columns lines) (grid-size display)
            (setf *screen* (make-screen columns lines)))
          (ensure-wakeup-pipe)
          (setf *display* display)))))

(defun show-window ()
  (let ((display *display*))
    (objc:invoke (display-window display) "makeKeyAndOrderFront:" nil)
    (when *activate*
      (objc:invoke (objc.runloop:shared-application) "activateIgnoringOtherApps:" t))))

(defun hide-window ()
  (let ((display *display*))
    (when display
      (objc:invoke (display-window display) "orderOut:" nil))))
