;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;;; The AppKit half of the Cocoa backend: the screen the editor thread
;;;; fills, the window and view that paint it, and keyboard input.
;;;; Everything here that touches an Objective-C object runs on the main
;;;; thread; the editor thread reaches it only through CALL-ON-MAIN-THREAD
;;;; and REQUEST-REDRAW.

(in-package :heml.cocoa)

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

(defvar *hosted* nil
  "True while Heml is a guest in another program's NSApplication: see
START-HOSTED in src/cocoa-main.lisp.")

(defvar *activate* t
  "Whether showing the window makes Heml the active application.  The
smoke test turns it off, so that a run does not take the keyboard from
whoever is working while it runs.")

(defvar *pasteboard-name* nil
  "The pasteboard the kill ring is joined to: NIL for the general one, that
every application shares, or the name of a private one, as the smoke test
uses so as not to overwrite the user's clipboard.")

(defvar *remember-window-frame* t
  "Whether the window is put where it was when last closed (its frame is
   saved by AppKit as \"NSWindow Frame HemlWindow\").  The smoke test
   leaves it alone.")

(defvar *settings-window* nil
  "The Settings window, once it has been opened (cocoa-settings.lisp).")

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
   (format *error-output* "~&;; heml.cocoa: ~A: ~A~%" where condition)
   (force-output *error-output*)))


;;;; The screen

;;; What -drawRect: paints: a grid of rows, each a string and the runs of
;;; it that are not in the default font.  The editor thread writes ROWS and
;;; the cursor while it redisplays, and PRESENT-SCREEN copies them, once a
;;; pass is done, to the SHOWN- slots, which are all the main thread reads.
;;; So the view never paints a frame half drawn.  Both threads take the
;;; lock.

(defstruct (row (:constructor make-row ()))
  (text "" :type simple-string)
  ;; ((start end . font) ...), ascending, non-overlapping.  FONT is what
  ;; Heml's font-changes carry: an ANSI colour index or a property list.
  (runs '() :type list))

(defstruct (screen (:constructor %make-screen (columns lines)))
  (lock (bt:make-lock "heml.cocoa screen"))
  (columns 80 :type fixnum)
  (lines 24 :type fixnum)
  (rows #() :type simple-vector)
  (cursor-x nil)
  (cursor-y nil)
  (shown-rows #() :type simple-vector)
  (shown-cursor-x nil)
  (shown-cursor-y nil)
  ;; The edges a drag resizes windows by, as (DIRECTION COLUMN LINE COLUMNS
  ;; LINES) rectangles of cells, where the pointer is a resize cursor.
  (borders '())
  (shown-borders '())
  ;; The current buffer's major mode, for the menus that belong to one.
  (mode nil)
  (shown-mode nil)
  ;; What the window's title bar says: (NAME FILE MODIFIED PROJECT ROOT).
  (title nil)
  (shown-title nil)
  ;; The popup, shown as a panel of its own: a property list, or NIL.
  (popup nil)
  (shown-popup nil)
  ;; Each window's place in its buffer, for its scroll bar and for scrolling
  ;; by points: ((COLUMN LINE WIDTH HEIGHT POSITION SIZE AT-START AT-END
  ;; ABOVE BELOW FRINGE) ...), POSITION and SIZE fractions (WINDOW-SCROLLS).
  (scrolls nil)
  (shown-scrolls nil)
  ;; The find bar's (STRING INDEX COUNT) while it is open (cocoa-find.lisp).
  (find nil)
  (shown-find nil)
  ;; The current buffer's values of the editor variables the main thread
  ;; acts on, as a property list (EDITOR-SETTINGS).
  (settings nil)
  (shown-settings nil)
  ;; Frames presented, so that the main thread knows when one has come.
  (frames 0 :type fixnum)
  ;; The files open, for their tabs: (CURRENT (NAME MODIFIED TITLE) ...).
  (tabs nil)
  (shown-tabs nil)
  ;; The echo area as a palette while it prompts: a property list, or NIL.
  (palette nil)
  (shown-palette nil))

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
(defvar *main-thread-queue-lock* (bt:make-lock "heml.cocoa main-thread queue"))
(defvar *main-thread-target* nil
  "The view's pointer: the object whose -hemlDrain runs the queue.")

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
                  (objc:coerce-to-selector "hemlDrain")
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
                     (objc:coerce-to-selector "hemlDrain")
                     nil t +run-loop-modes+)
        (when condition (error condition))
        (values-list values))))


;;;; The clipboard

;;; Heml's kill ring and the general pasteboard, joined as Emacs joins
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
(defvar *inbox-lock* (bt:make-lock "heml.cocoa inbox"))
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
         :documentation "The HemlView, as a pointer.")
   (view-object :initform nil :accessor display-view-object
                :documentation "The same view as a Lisp object, held so it is not collected.")
   (delegate :initform nil :accessor display-delegate)
   (app-delegate :initform nil :accessor display-app-delegate)
   (font :initform nil :accessor display-font)
   (bold-font :initform nil :accessor display-bold-font)
   (italic-font :initform nil :accessor display-italic-font)
   (bold-italic-font :initform nil :accessor display-bold-italic-font)
   (slanted :initform '() :accessor display-slanted
            :documentation "Which of :ITALIC and :BOLD-ITALIC the font has no face
for, and so are the upright face slanted.")
   (char-ascent :initform 12 :accessor display-char-ascent
                :documentation "From the top of a cell to its baseline, where an
underline is drawn.")
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
whether Quit asks Heml or just ends the application.")

(defun null-pointer-p (pointer)
  (or (null pointer) (cffi:null-pointer-p pointer)))

(defun df (x) (float x 1d0))


;;;; Fonts

(defconstant +bold-font-mask+ 2)
(defconstant +italic-font-mask+ 1)
(defconstant +italic-trait+ 1)          ; NSFontDescriptorTraitItalic

(defun make-font (name size bold &optional italic)
  "The font NAME, or the system's monospaced font, at SIZE.  For ITALIC, a
second value is true when the font has no italic face, and what is returned
is the upright one, to be slanted."
  (let* ((size (df size))
         (font (or (and name
                        (let ((font (objc:invoke "NSFont" "fontWithName:size:" name size)))
                          (cond ((null-pointer-p font) nil)
                                (bold (objc:invoke (objc:invoke "NSFontManager" "sharedFontManager")
                                                   "convertFont:toHaveTrait:" font +bold-font-mask+))
                                (t font))))
                   (objc:invoke "NSFont" "monospacedSystemFontOfSize:weight:"
                                size (if bold 0.4d0 0d0)))))
    (if (not italic)
        font
        (let ((italic (objc:invoke (objc:invoke "NSFontManager" "sharedFontManager")
                                   "convertFont:toHaveTrait:" font +italic-font-mask+)))
          (if (logtest +italic-trait+
                       (objc:invoke (objc:invoke italic "fontDescriptor") "symbolicTraits"))
              (values italic nil)
              (values font t))))))

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
            advance
            (round ascender))))

(defun install-fonts (display)
  "Make the fonts from *FONT-NAME* and *FONT-SIZE* and remeasure the cell.
The text attribute cache goes with the old fonts: its dictionaries name them."
  (let ((old (list (display-font display) (display-bold-font display)
                   (display-italic-font display) (display-bold-italic-font display))))
    (setf (display-font display) (objc:retain (make-font *font-name* *font-size* nil))
          (display-bold-font display) (objc:retain (make-font *font-name* *font-size* t))
          (display-slanted display) '())
    (multiple-value-bind (font slanted) (make-font *font-name* *font-size* nil t)
      (setf (display-italic-font display) (objc:retain font))
      (when slanted (push :italic (display-slanted display))))
    (multiple-value-bind (font slanted) (make-font *font-name* *font-size* t t)
      (setf (display-bold-italic-font display) (objc:retain font))
      (when slanted (push :bold-italic (display-slanted display))))
    (multiple-value-bind (width height advance ascent) (measure-font (display-font display))
      (setf (display-char-width display) width
            (display-char-height display) height
            (display-char-advance display) advance
            (display-char-ascent display) ascent))
    (loop for dictionary being the hash-values of (display-attributes display)
          do (objc:release dictionary))
    (clrhash (display-attributes display))
    (dolist (font old)
      (when font (objc:release font))))
  display)

;;; The font chosen last is kept in the user defaults: the application's,
;;; or the SBCL process's when the editor runs from a REPL.

(defparameter +font-name-key+ "HemlFontName")
(defparameter +font-size-key+ "HemlFontSize")
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
    (when *settings-window* (refresh-settings-window))
    (multiple-value-bind (columns lines) (grid-size display)
      (post-to-editor (list :resize columns lines)))
    ;; The edges are where they were in cells, but not in points.
    (objc:invoke (display-window display) "invalidateCursorRectsForView:"
                 (display-view display))
    (request-redraw)))

(defun change-font-size (delta)
  (change-font :size (if delta (+ *font-size* delta) +default-font-size+)))


;;;; Colours

;;; Heml's fonts are ANSI colour indexes, as the TTY backend reads them.
;;; They map onto AppKit's dynamic system colours, so light and dark
;;; appearance both come out right without any code here.

(defparameter +palette+
  #("blackColor" "systemRedColor" "systemGreenColor" "systemOrangeColor"
    "systemBlueColor" "systemPurpleColor" "systemTealColor" "whiteColor"
    "systemGrayColor" "textColor"))

(defun ns-color (display name)
  "The colour NAME names: an NSColor's own name, or (RED GREEN BLUE)."
  (or (gethash name (display-colors display))
      (setf (gethash name (display-colors display))
            (objc:retain
             (if (consp name)
                 (destructuring-bind (red green blue) name
                   (objc:invoke "NSColor" "colorWithSRGBRed:green:blue:alpha:"
                                (/ red 255d0) (/ green 255d0) (/ blue 255d0) 1d0))
                 (objc:invoke "NSColor" name))))))

(defun xterm-rgb (index)
  "The colour of INDEX among xterm's 256: its bright colours, its six by six
   by six cube, and its greys."
  (cond ((< index 16)
         (svref #((0 0 0) (205 0 0) (0 205 0) (205 205 0) (0 0 238) (205 0 205)
                  (0 205 205) (229 229 229) (127 127 127) (255 0 0) (0 255 0)
                  (255 255 0) (92 92 255) (255 0 255) (0 255 255) (255 255 255))
                index))
        ((< index 232)
         (let ((levels #(0 95 135 175 215 255))
               (n (- index 16)))
           (list (svref levels (floor n 36)) (svref levels (mod (floor n 6) 6))
                 (svref levels (mod n 6)))))
        (t (let ((grey (+ 8 (* 10 (- index 232))))) (list grey grey grey)))))

(defun color-name-for (index)
  "What NS-COLOR is given for a font's colour: one of Heml's palette, an
   index among xterm's 256 past it, or (RED GREEN BLUE), as a terminal's
   text has."
  (cond ((and (integerp index) (< -1 index (length +palette+)))
         (svref +palette+ index))
        ((and (integerp index) (< -1 index 256))
         (xterm-rgb index))
        ((and (consp index) (= (length index) 3) (every #'integerp index))
         index)
        ;; What the Cocoa backend makes the active region's font, so that
        ;; a region looks like a selection anywhere else on the Mac.
        ((eq index :selection) "selectedTextBackgroundColor")
        ;; The accent colour the user chose in System Settings, and the
        ;; text macOS draws on it: the modelines' and a popup's choice.
        ((eq index :accent) "controlAccentColor")
        ((eq index :accent-text) "alternateSelectedControlTextColor")
        (t "textColor")))

(defun palette-color (display index)
  (ns-color display (color-name-for index)))

(defun foreground-color (display) (ns-color display "textColor"))
(defun background-color (display) (ns-color display "textBackgroundColor"))

(defun font-style (font)
  "FONT's foreground index, background index, boldness, italic and
underline, each NIL for the default."
  (cond ((integerp font) (values (if (zerop font) nil font) nil nil nil nil))
        ((consp font) (values (getf font :fg) (getf font :bg) (getf font :bold)
                              (getf font :italic) (getf font :underline)))
        (t (values nil nil nil nil nil))))

(defun text-attributes (display color-name bold &optional italic)
  "A retained NSDictionary: the font, COLOR-NAME's colour, and the kern
that makes every character advance exactly one cell.  An italic that the
font has no face for is the upright face slanted."
  (let ((key (list color-name bold italic)))
    (or (gethash key (display-attributes display))
        (setf (gethash key (display-attributes display))
              (let ((dictionary (objc:alloc-init-object "NSMutableDictionary")))
                (objc:invoke dictionary "setObject:forKey:"
                             (cond ((and bold italic) (display-bold-italic-font display))
                                   (italic (display-italic-font display))
                                   (bold (display-bold-font display))
                                   (t (display-font display)))
                             "NSFont")
                (when (and italic
                           (member (if bold :bold-italic :italic) (display-slanted display)))
                  (objc:invoke dictionary "setObject:forKey:"
                               (objc:invoke "NSNumber" "numberWithDouble:" 0.2d0)
                               "NSObliqueness"))
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
(defun draw-text (display string start end line color-name bold &optional italic)
  (let ((attributes (text-attributes display color-name bold italic)))
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
                       ;; The bar between windows side by side: a line the
                       ;; full height of the cell, which a glyph is not.
                       ((char= character #\│)
                        (fill-rect (ns-color display "separatorColor")
                                   (+ (cell-x display i)
                                      (floor (display-char-width display) 2))
                                   (cell-y display line)
                                   1 (display-char-height display))
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

(defun shape-path (shape x y w h)
  "An NSBezierPath of SHAPE in the cell at X and Y, W by H, or NIL."
  (flet ((polygon (&rest points)
           (let ((path (objc:invoke "NSBezierPath" "bezierPath")))
             (loop for (px py) on points by #'cddr
                   for first = t then nil
                   do (objc:invoke path (if first "moveToPoint:" "lineToPoint:")
                                   (vector (df (+ x (* px w))) (df (+ y (* py h))))))
             (objc:invoke path "closePath")
             path))
         (rect (rx ry rw rh)
           (objc:invoke "NSBezierPath" "bezierPathWithRect:"
                        (vector (df (+ x (* rx w))) (df (+ y (* ry h)))
                                (df (* rw w)) (df (* rh h))))))
    (case shape
      ;; A fold's open and closed triangles, the cell's width.
      (:fold-open (polygon 0.1 0.32 0.9 0.32 0.5 0.74))
      (:fold-closed (polygon 0.22 0.14 0.22 0.86 0.9 0.5))
      ;; A breakpoint: a tag pointing at the line, as Xcode draws one.
      (:breakpoint (polygon 0.0 0.18 0.62 0.18 1.0 0.5 0.62 0.82 0.0 0.82))
      ;; Where the program stopped: an arrow.
      (:arrow (polygon 0.0 0.34 0.48 0.34 0.48 0.14 1.0 0.5 0.48 0.86 0.48 0.66 0.0 0.66))
      ;; Git: lines added or changed, and lines taken out below or above.
      (:bar (rect 0.3 0.0 0.3 1.0))
      (:edge-below (rect 0.0 0.88 1.0 0.12))
      (:edge-above (rect 0.0 0.0 1.0 0.12)))))

(defun draw-shape (display shape color-name column line)
  (let ((path (shape-path shape (cell-x display column) (cell-y display line)
                          (display-char-width display) (display-char-height display))))
    (when path
      (objc:invoke (ns-color display color-name) "set")
      (objc:invoke path "fill"))))

;;; A modeline is a status bar: the system's own font, small, on the
;;; window's background with a hairline above it, the current window's
;;; tinted with the accent colour and the others plain, and the echo area's
;;; quieter still.
;;;
(defun modeline-attributes (display kind)
  (let ((key (list :modeline kind)))
    (or (gethash key (display-attributes display))
        (setf (gethash key (display-attributes display))
              (let ((dictionary (objc:alloc-init-object "NSMutableDictionary"))
                    (style (objc:alloc-init-object "NSMutableParagraphStyle")))
                (objc:invoke style "setLineBreakMode:" 4) ; truncating its tail
                (objc:invoke dictionary "setObject:forKey:"
                             (objc:invoke "NSFont" "systemFontOfSize:weight:"
                                          (df (max 9 (- *font-size* 2)))
                                          (if (eq kind :active) 0.23d0 0d0))
                             "NSFont")
                (objc:invoke dictionary "setObject:forKey:"
                             (ns-color display (if (eq kind :active)
                                                   "labelColor"
                                                   "secondaryLabelColor"))
                             "NSColor")
                (objc:invoke dictionary "setObject:forKey:" style "NSParagraphStyle")
                dictionary)))))

(defun modeline-background (display kind)
  (let ((key (list :modeline-background kind)))
    (or (gethash key (display-colors display))
        (setf (gethash key (display-colors display))
              (objc:retain
               (if (eq kind :active)
                   (objc:invoke (objc:invoke "NSColor" "controlAccentColor")
                                "colorWithAlphaComponent:" 0.22d0)
                   (objc:invoke "NSColor" "windowBackgroundColor")))))))

(defun status-text (text start end)
  "TEXT from START to END as a status bar shows it: without the spaces at
   its end, and no more than three between fields."
  (let ((string (string-right-trim " " (subseq text start (min end (length text))))))
    (with-output-to-string (out)
      (let ((spaces 0))
        (loop for c across string
              do (if (char= c #\Space)
                     (when (< (incf spaces) 4) (write-char c out))
                     (progn (setf spaces 0) (write-char c out))))))))

(defun draw-modeline (display text start end line kind)
  (let* ((x (cell-x display start))
         (y (cell-y display line))
         (width (* (- end start) (display-char-width display)))
         (height (display-char-height display))
         (attributes (modeline-attributes display kind))
         (font (objc:invoke attributes "objectForKey:" "NSFont"))
         (font-height (- (objc:invoke font "ascender") (objc:invoke font "descender")))
         (padding 6))
    (fill-rect (ns-color display "windowBackgroundColor") x y width height)
    (when (eq kind :active)
      (fill-rect (modeline-background display kind) x y width height))
    (fill-rect (ns-color display "separatorColor") x y width 1)
    (objc:invoke (objc:string-to-ns-string (status-text text start end))
                 "drawInRect:withAttributes:"
                 (vector (df (+ x padding)) (df (+ y (/ (- height font-height) 2)))
                         (df (max 0 (- width (* 2 padding)))) (df font-height))
                 attributes)))

(defun draw-segment (display text start end line font)
  (when (and (consp font) (getf font :modeline))
    (return-from draw-segment
      (draw-modeline display text start end line (getf font :modeline))))
  (when (and (consp font) (getf font :shape))
    (return-from draw-segment
      (loop for column from start below end
            do (draw-shape display (getf font :shape)
                           (color-name-for (or (getf font :fg) 8)) column line))))
  (multiple-value-bind (fg bg bold italic underline) (font-style font)
    (when bg
      (fill-rect (palette-color display bg)
                 (cell-x display start) (cell-y display line)
                 (* (- end start) (display-char-width display))
                 (display-char-height display)))
    (draw-text display text start (min end (length text)) line
               (color-name-for fg) bold italic)
    ;; Drawn rather than asked of AppKit, whose underline breaks at
    ;; descenders and need not end where the cells do.
    (when underline
      (fill-rect (palette-color display fg)
                 (cell-x display start)
                 (+ (cell-y display line) (display-char-ascent display) 1)
                 (* (- end start) (display-char-width display))
                 1))))

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

(defvar *cursor-style* :bar
  "How the caret is drawn: \"Cursor Style\", as the editor last said.")

(defvar *cursor-blink* t
  "Whether the caret blinks: \"Cursor Blink\", as the editor last said.")

(defparameter *blink-interval* 0.53d0
  "Seconds the caret is shown, and then hidden, as it blinks.")

(defvar *caret-shown* t)

(defvar *caret-moved-at* 0
  "When the caret last moved, or a key was typed: it is shown steadily for
   a while after.")

(defun note-caret-moved ()
  (setf *caret-moved-at* (get-internal-real-time)
        *caret-shown* t))

(defvar *caret-rect* nil
  "Where DRAW-CURSOR last drew the bar, as #(LEFT TOP WIDTH HEIGHT) in the
   view's points, or NIL: all a blink has to redraw.")

(defun blink-caret ()
  "From the view's timer, on the main thread: the caret on or off, unless it
   moved a moment ago; and a scroll bar away once it has been shown long
   enough.

   Only in the key window, and only the caret's own rectangle.  Redrawing the
   whole window twice a second, key or not, kept the main thread drawing on a
   machine that draws slowly -- a CI runner with no GPU -- and starved what
   else wanted it: a host's work stalled for minutes behind it.  A Mac text
   view does not blink its caret in a window that is not key either; it is
   grey and steady there (DRAW-CURSOR)."
  (let ((display *display*))
    (when display
      (when (scrollers-to-hide-p)
        (objc:invoke (display-view display) "setNeedsDisplay:" t))
      (let ((still (/ (- (get-internal-real-time) *caret-moved-at*)
                      internal-time-units-per-second)))
        (cond ((not (heml-window-key-p))
               ;; Shown when the window is key again, before the first blink.
               (setf *caret-shown* t))
              ((and *cursor-blink* (eq *cursor-style* :bar) (> still *blink-interval*))
               (setf *caret-shown* (not *caret-shown*))
               (let ((rect *caret-rect*))
                 (if rect
                     (objc:invoke (display-view display) "setNeedsDisplayInRect:" rect)
                     (objc:invoke (display-view display) "setNeedsDisplay:" t)))))))))

;;; Scroll bars as the Mac's own overlay ones are: a thin rounded thumb at
;;; a window's right edge, shown while it scrolls and for a moment after.
;;;
(defparameter *scroller-shown-for* 1.2
  "Seconds a window's scroll bar stays after it last scrolled.")

(defvar *scrolled-at* (make-hash-table :test 'equal)
  "Each window, by its column and line on the screen, to when it last
   scrolled and its place then.")

(defun note-scrolls (new old)
  "The windows whose place in their buffers moved between the frames OLD and
   NEW scrolled now."
  (let ((now (get-internal-real-time)))
    (dolist (entry new)
      (destructuring-bind (column line width height position size &rest more) entry
        (declare (ignore width height size more))
        (let ((before (find-if (lambda (e) (and (eql (first e) column) (eql (second e) line))) old)))
          (when (and before (/= (fifth before) position))
            (setf (gethash (cons column line) *scrolled-at*) now)))))))

(defun scroller-age (column line)
  (let ((at (gethash (cons column line) *scrolled-at*)))
    (and at (/ (- (get-internal-real-time) at) internal-time-units-per-second))))

(defun scrollers-to-hide-p ()
  (loop for at being the hash-values of *scrolled-at*
        thereis (< (/ (- (get-internal-real-time) at) internal-time-units-per-second)
                   (+ *scroller-shown-for* 1))))

(defun draw-scrollers (display scrolls)
  (loop for (column line width height position size) in scrolls
        for age = (scroller-age column line)
        when (and age (< age *scroller-shown-for*) (< size 1))
          do (let* ((track-top (+ (cell-y display line) 2))
                    (track (- (* height (display-char-height display)) 4))
                    (thumb (max 24 (* size track)))
                    (top (+ track-top (* position (- track thumb))))
                    (left (- (cell-x display (+ column width)) 8))
                    (path (objc:invoke "NSBezierPath" "bezierPathWithRoundedRect:xRadius:yRadius:"
                                       (vector (df left) (df top) 6d0 (df thumb))
                                       3d0 3d0)))
               (objc:invoke (objc:invoke (objc:invoke "NSColor" "labelColor")
                                         "colorWithAlphaComponent:" 0.35d0)
                            "set")
               (objc:invoke path "fill"))))

(defun draw-cursor (display screen key-window-p)
  (let ((x (screen-shown-cursor-x screen))
        (y (screen-shown-cursor-y screen))
        (rows (screen-shown-rows screen)))
    (when (and x y (< -1 y (length rows)) (not (palette-hides-row-p y)))
      (let* ((text (row-text (svref rows y)))
             (left (cell-x display x))
             (top (cell-y display y))
             (width (* (if (wide-at-p text x) 2 1) (display-char-width display)))
             (height (display-char-height display))
             (color (foreground-color display)))
        (cond ((eq *cursor-style* :bar)
               ;; Before the character, the height of the line; steady and
               ;; grey in a window that is not the key one.  Remembered,
               ;; with a point to spare, for BLINK-CARET.
               (setf *caret-rect* (vector (df (- left 1)) (df (- top 1)) 4d0 (df (+ height 2))))
               (cond ((not key-window-p)
                      (fill-rect (ns-color display "tertiaryLabelColor") left top 2 height))
                     (*caret-shown*
                      (fill-rect (ns-color display "controlAccentColor") left top 2 height))))
              (key-window-p
               (fill-rect color left top width height)
               (when (< x (length text))
                 (draw-text display text x (1+ x) y "textBackgroundColor" nil)))
              (t
               (frame-rect color left top width height)))))))

(defun draw-marked-text (display screen)
  "An input method's uncommitted text at the cursor, in reverse video and
underlined, laid out in cells as a row's text is."
  (let ((marked (display-marked-text display))
        (x (screen-shown-cursor-x screen))
        (y (screen-shown-cursor-y screen)))
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
    (let ((rows (screen-shown-rows screen))
          (key-window-p (objc:invoke-bool (display-window display) "isKeyWindow")))
      (draw-find-matches display screen)
      (dotimes (line (length rows))
        (unless (palette-hides-row-p line)
          (draw-row display (svref rows line) line)))
      (multiple-value-bind (entry shift) (scroll-shift screen)
        (cond (entry
               (draw-shifted display screen entry shift key-window-p))
              (t
               (draw-cursor display screen key-window-p))))
      (draw-scrollers display (screen-shown-scrolls screen))
      (draw-marked-text display screen))))

(defun present-screen (screen)
  "Make what the editor has drawn what the view shows.  A row's text and
runs are replaced, never changed, so a copy of each row will do.  When the
edges have moved, AppKit is told to ask the view for its cursor rectangles
again."
  (let ((borders-moved nil))
    (with-screen-lock (screen)
      (incf (screen-frames screen))
      (setf (screen-shown-rows screen) (map 'simple-vector #'copy-row (screen-rows screen))
            (screen-shown-cursor-x screen) (screen-cursor-x screen)
            (screen-shown-cursor-y screen) (screen-cursor-y screen))
      (unless (equal (screen-borders screen) (screen-shown-borders screen))
        (setf (screen-shown-borders screen) (screen-borders screen)
              borders-moved t))
      (unless (or (null (screen-mode screen))
                  (equal (screen-mode screen) (screen-shown-mode screen)))
        (let ((mode (setf (screen-shown-mode screen) (screen-mode screen))))
          (on-main-thread (show-mode-menus mode))))
      (unless (or (null (screen-settings screen))
                  (equal (screen-settings screen) (screen-shown-settings screen)))
        (let ((settings (setf (screen-shown-settings screen) (screen-settings screen))))
          (on-main-thread (apply-editor-settings settings))))
      (unless (or (null (screen-title screen))
                  (equal (screen-title screen) (screen-shown-title screen)))
        (let ((title (setf (screen-shown-title screen) (screen-title screen))))
          (on-main-thread (show-title title)
                          (note-sidebar-title title))))
      (unless (or (null (screen-tabs screen))
                  (equal (screen-tabs screen) (screen-shown-tabs screen)))
        (let ((tabs (setf (screen-shown-tabs screen) (screen-tabs screen))))
          (on-main-thread (show-tabs tabs))))
      (note-scrolls (screen-scrolls screen) (screen-shown-scrolls screen))
      (setf (screen-shown-scrolls screen) (screen-scrolls screen))
      (unless (equal (screen-find screen) (screen-shown-find screen))
        (let ((find (setf (screen-shown-find screen) (screen-find screen))))
          (on-main-thread (show-find-status find))))
      (unless (equal (screen-palette screen) (screen-shown-palette screen))
        (let ((palette (setf (screen-shown-palette screen) (screen-palette screen))))
          (on-main-thread (show-palette palette))))
      (unless (equal (screen-popup screen) (screen-shown-popup screen))
        (let ((popup (setf (screen-shown-popup screen) (screen-popup screen))))
          (on-main-thread (show-popup-panel popup))))
      ;; Typing, or anything else drawn, shows the caret steadily.
      (note-caret-moved))
    (when borders-moved
      (on-main-thread
        (let ((display *display*))
          (when display
            (objc:invoke (display-window display) "invalidateCursorRectsForView:"
                         (display-view display))))))))

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

;;; The title bar as a Mac document window's: the buffer's name, its file's
;;; icon (Command-click on the title shows the folder it is in), a dot in
;;; the close button while it has unsaved changes, and the project under it.
;;;
(defun show-title (title)
  (destructuring-bind (name file modified project &optional root) title
    (declare (ignore root))
    (let* ((display *display*)
           (window (and display (display-window display))))
      (when window
        (objc:invoke window "setTitle:" name)
        (when (objc:invoke-bool window "respondsToSelector:"
                                (objc:coerce-to-selector "setSubtitle:"))
          (objc:invoke window "setSubtitle:" (or project "")))
        (objc:invoke window "setRepresentedURL:"
                     (if file
                         (objc:invoke "NSURL" "fileURLWithPath:" file)
                         (cffi:null-pointer)))
        (objc:invoke window "setDocumentEdited:" (and modified t))))))


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
;;; modifiers), with modifiers a list of Heml modifier names.  The editor
;;; thread makes the key-events, since that writes to Heml's tables.

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
  "NSEvent's function-key characters to Heml's keysym names.")

(defparameter *control-keys*
  '((127 . "Backspace") (8 . "Backspace")
    (13 . "Return") (3 . "Return") (10 . "Return")
    (9 . "Tab") (25 . "Tab")
    (27 . "Escape"))
  "Control characters that name keys.  The Delete key sends 127, which
Heml calls Rubout and binds to deleting forward, so it is named
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

(defvar *lines-per-wheel-step* 3
  "Lines a notch of a mouse wheel scrolls: \"Mouse Wheel Lines\", as the
   editor last said.")

;;; Scrolling by points, as a Mac text view scrolls with a trackpad.  The
;;; editor scrolls by lines; between them the main thread draws the window's
;;; text moved by the part of a line scrolled so far, with the line coming in
;;; (each window's scroll entry carries its text) in the strip that opens.
;;; At either end of the buffer the text is pulled on a rubber band, and
;;; springs back when the fingers leave; a part of a line left when they do
;;; settles to the nearest line.  macOS gives the momentum itself, as more
;;; scroll events after the fingers leave.
;;;
(defvar *pixel-scrolling* t
  "Whether a trackpad scrolls by points: \"Pixel Scrolling\", as the
   editor last said.")

(defparameter *overscroll-limit* 1/4
  "How far past an end the text can be pulled, as a fraction of its
   window's height.")

(defvar *pixel-scroll* nil
  "The window being scrolled by points, as a list: its (COLUMN LINE), the
   offset in points (positive with the text moved down, toward the lines
   before), the lines posted and the frame they were posted after, and the
   cell and modifiers to post more at.  On the main thread.")

(defvar *scroll-touching* nil
  "Whether fingers are on the trackpad, scrolling.")

(defvar *scroll-event-at* 0)

(defvar *settle-timer* nil)

(defmacro scroll-state (field)
  `(getf (cdr *pixel-scroll*) ,field))

(defun scrolls-entry (key)
  (find-if (lambda (entry) (and (eql (first entry) (first key))
                                (eql (second entry) (second key))))
           (with-screen-lock (*screen*) (screen-shown-scrolls *screen*))))

(defun scrolls-entry-at (column line)
  (find-if (lambda (entry)
             (destructuring-bind (c l width height &rest more) entry
               (declare (ignore more))
               (and (<= c column (+ c width -1)) (<= l line (+ l height -1)))))
           (with-screen-lock (*screen*) (screen-shown-scrolls *screen*))))

(defun pending-lines ()
  "Lines posted that the frame shown does not have yet."
  (if (and *pixel-scroll*
           (eql (scroll-state :posted-after) (with-screen-lock (*screen*) (screen-frames *screen*))))
      (scroll-state :posted)
      0))

(defun post-scroll-line (up)
  "Have the editor scroll the window a line: toward the lines before when UP."
  (destructuring-bind (column line modifiers) (scroll-state :cell)
    (let ((frames (with-screen-lock (*screen*) (screen-frames *screen*))))
      (unless (eql frames (scroll-state :posted-after))
        (setf (scroll-state :posted-after) frames
              (scroll-state :posted) 0))
      (incf (scroll-state :posted) (if up 1 -1))
      (post-to-editor (list :mouse (if up "Scrollup" "Scrolldown") modifiers column line)))))

(defun scroll-by-points (entry delta)
  "Move the text of ENTRY's window DELTA points, posting a line to the
   editor each time a line's height is passed, or, past an end, pulling it
   on the rubber band."
  (destructuring-bind (column line width height position size
                       &optional at-start at-end &rest more)
      entry
    (declare (ignore column line width position size more))
    (let* ((cell (display-char-height *display*))
           (limit (* *overscroll-limit* height cell))
           (offset (scroll-state :offset)))
      (when (or (and at-start (plusp delta) (>= offset 0))
                (and at-end (minusp delta) (<= offset 0)))
        (setf delta (* delta (max 0 (- 1 (/ (abs offset) limit))))))
      (incf offset delta)
      (loop while (and (not at-start) (>= offset cell))
            do (post-scroll-line t)
               (decf offset cell))
      (loop while (and (not at-end) (<= offset (- cell)))
            do (post-scroll-line nil)
               (incf offset cell))
      (setf (scroll-state :offset) offset))))

(defun pixel-scroll (event)
  (multiple-value-bind (column line) (event-cell event)
    ;; Began, stationary, changed or may begin: the fingers are down.
    (scroll-points-at column line (event-modifiers event)
                      (objc:invoke event "scrollingDeltaY")
                      (logtest (objc:invoke event "phase") #x27))))

(defun scroll-points-at (column line modifiers delta touching)
  "Scroll the window at the cell COLUMN, LINE by DELTA points, positive
   toward the lines before; TOUCHING when the fingers are still down."
  (setf *scroll-touching* touching
        *scroll-event-at* (get-internal-real-time))
  (let ((entry (scrolls-entry-at column line)))
    (when entry
      (let ((key (list (first entry) (second entry))))
        (unless (equal key (car *pixel-scroll*))
          (setf *pixel-scroll* (list key :offset 0d0 :posted 0 :posted-after -1)))
        (setf (scroll-state :cell) (list column line modifiers))
        (scroll-by-points entry (df delta))
        (start-settling)
        (objc:invoke (display-view *display*) "setNeedsDisplay:" t)))))

(defun start-settling ()
  (unless *settle-timer*
    (setf *settle-timer*
          (objc:invoke "NSTimer" "scheduledTimerWithTimeInterval:target:selector:userInfo:repeats:"
                       (df 1/60) (display-view *display*) (objc:coerce-to-selector "hemlSettle:")
                       (cffi:null-pointer) t))))

(defun stop-settling ()
  (when *settle-timer*
    (objc:invoke *settle-timer* "invalidate")
    (setf *settle-timer* nil)))

(defun settle-scroll ()
  "From the timer, while a window's text is moved by part of a line: once the
   fingers are off and no momentum is coming, move it toward where it rests,
   its place before the rubber band, or the nearest line."
  (let ((entry (and *pixel-scroll* (scrolls-entry (car *pixel-scroll*)))))
    (cond ((null entry)
           (setf *pixel-scroll* nil)
           (stop-settling))
          ((or *scroll-touching*
               (< (- (get-internal-real-time) *scroll-event-at*)
                  (* 0.05 internal-time-units-per-second))))
          (t
           (destructuring-bind (column line width height position size
                                &optional at-start at-end &rest more)
               entry
             (declare (ignore column line width height position size more))
             (let* ((cell (display-char-height *display*))
                    (offset (scroll-state :offset))
                    (target (cond ((or (and at-start (plusp offset)) (and at-end (minusp offset))) 0)
                                  ((> (abs offset) (/ cell 2)) (* (signum offset) cell))
                                  (t 0)))
                    (step (* 0.2d0 (- target offset))))
               (scroll-by-points entry (if (< (abs (- target offset)) 0.5d0) (- target offset) step))
               (when (and (zerop (scroll-state :offset)) (zerop (pending-lines)))
                 (stop-settling))
               (objc:invoke (display-view *display*) "setNeedsDisplay:" t)))))))

(defun scroll-shift (screen)
  "The scroll entry of the window drawn moved, and by how many points, or
   NIL.  With the screen locked."
  (let ((state *pixel-scroll*))
    (when state
      (let* ((key (car state))
             (entry (find-if (lambda (entry) (and (eql (first entry) (first key))
                                                  (eql (second entry) (second key))))
                             (screen-shown-scrolls screen)))
             (shift (and entry
                         (+ (getf (cdr state) :offset)
                            (* (if (eql (getf (cdr state) :posted-after) (screen-frames screen))
                                   (getf (cdr state) :posted)
                                   0)
                               (display-char-height *display*))))))
        (when (and entry (/= shift 0))
          (values entry shift))))))

(defun draw-shifted (display screen entry shift key-window-p)
  "ENTRY's window's text drawn again SHIFT points down, within the window,
   with the line coming in drawn plainly in the strip that opens, and the
   caret with it when it is there."
  (destructuring-bind (column line width height position size
                       &optional at-start at-end above below (fringe 0) &rest more)
      entry
    (declare (ignore position size more))
    (let* ((rows (screen-shown-rows screen))
           (left (cell-x display column))
           (top (cell-y display line))
           (rect (vector (df left) (df top)
                         (df (* width (display-char-width display)))
                         (df (* height (display-char-height display)))))
           (transform (objc:invoke "NSAffineTransform" "transform"))
           (x (screen-shown-cursor-x screen))
           (y (screen-shown-cursor-y screen))
           (caret-inside (and x y (<= line y (+ line height -1)) (<= column x (+ column width)))))
      (fill-rect (background-color display) (aref rect 0) (aref rect 1) (aref rect 2) (aref rect 3))
      (objc:invoke "NSGraphicsContext" "saveGraphicsState")
      (unwind-protect
           (progn
             (objc:invoke "NSBezierPath" "clipRect:" rect)
             (objc:invoke transform "translateXBy:yBy:" 0d0 (df shift))
             (objc:invoke transform "concat")
             (loop for row from line below (min (length rows) (+ line height))
                   do (draw-row display (svref rows row) row))
             (flet ((incoming (text row)
                      (when (and text (plusp (length text)))
                        (let* ((start (+ column fringe))
                               (string (concatenate 'string (make-string start :initial-element #\Space)
                                                    (subseq text 0 (min (length text) (- width fringe))))))
                          (draw-text display string start (length string) row
                                     (color-name-for 9) nil)))))
               (cond ((and (plusp shift) (not at-start)) (incoming above (1- line)))
                     ((and (minusp shift) (not at-end)) (incoming below (+ line height)))))
             (when caret-inside
               (draw-cursor display screen key-window-p)))
        (objc:invoke "NSGraphicsContext" "restoreGraphicsState"))
      (unless caret-inside
        (draw-cursor display screen key-window-p)))))

(defun post-scroll (event)
  (if (and *pixel-scrolling* (objc:invoke-bool event "hasPreciseScrollingDeltas"))
      (pixel-scroll event)
      (post-wheel-scroll event)))

(defun post-wheel-scroll (event)
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

(objc:define-objc-class heml-view ()
  ()
  (:objc-class-name "HemlView")
  (:objc-superclass-name "NSView")
  ;; So that AppKit's input context talks to the view: dead keys, input
  ;; methods, and their marked text.  The methods are below.
  (:objc-protocols "NSTextInputClient"))

(objc:define-objc-class window-delegate ()
  ()
  (:objc-class-name "HemlWindowDelegate"))

(objc:define-objc-class app-delegate ()
  ()
  (:objc-class-name "HemlAppDelegate"))

(objc:define-objc-method ("isFlipped" objc:objc-bool) ((self heml-view))
  t)

;;; Over an edge that resizes windows, the pointer says which way it drags.
;;;
(objc:define-objc-method ("resetCursorRects" :void) ((self heml-view pointer))
  (handler-case
      (let ((display *display*)
            (screen *screen*))
        (when (and display screen)
          (dolist (border (with-screen-lock (screen) (screen-shown-borders screen)))
            (destructuring-bind (direction column line columns lines) border
              (objc:invoke pointer "addCursorRect:cursor:"
                           (vector (df (cell-x display column)) (df (cell-y display line))
                                   (df (* columns (display-char-width display)))
                                   (df (* lines (display-char-height display))))
                           (objc:invoke "NSCursor" (if (eq direction :columns)
                                                       "resizeLeftRightCursor"
                                                       "resizeUpDownCursor")))))))
    (error (condition) (log-error "resetCursorRects" condition))))

(objc:define-objc-method ("acceptsFirstResponder" objc:objc-bool) ((self heml-view))
  t)

(objc:define-objc-method ("isOpaque" objc:objc-bool) ((self heml-view))
  t)

;;; A click on the window while another application is active both brings
;;; it forward and lands, as in Emacs, instead of only activating it.
(objc:define-objc-method ("acceptsFirstMouse:" objc:objc-bool)
    ((self heml-view) (event objc:objc-object-pointer))
  (declare (ignore event))
  t)

;;; The accent colour changed in System Settings: the colours are the
;;; system's own, resolved as they are drawn, so drawing again shows it.
(objc:define-objc-method ("hemlSystemColorsChanged:" :void)
    ((self heml-view) (notification objc:objc-object-pointer))
  (declare (ignore notification))
  (objc:invoke (objc:objc-object-pointer self) "setNeedsDisplay:" t))

(objc:define-objc-method ("hemlDrain" :void) ((self heml-view))
  (drain-main-thread-queue))

(objc:define-objc-method ("hemlSettle:" :void)
    ((self heml-view) (timer objc:objc-object-pointer))
  (declare (ignore timer))
  (handler-case (settle-scroll)
    (error (condition)
      (stop-settling)
      (log-error "hemlSettle:" condition))))

(objc:define-objc-method ("hemlBlink:" :void)
    ((self heml-view) (timer objc:objc-object-pointer))
  (declare (ignore timer))
  (handler-case (blink-caret)
    (error (condition) (log-error "hemlBlink:" condition))))

(objc:define-objc-method ("drawRect:" :void)
    ((self heml-view) (dirty cocoa:ns-rect))
  (declare (ignore dirty))
  (handler-case
      (when (and *display* *screen*)
        (draw-screen *display* *screen*))
    (error (condition) (log-error "drawRect:" condition))))

;;; Named keys and keys with Control or Meta are Heml's directly.
;;; Plain typing goes through AppKit's input context, which composes dead
;;; keys and runs input methods, and comes back as -insertText: or
;;; -setMarkedText:.  While an input method holds marked text, every key is
;;; its to interpret.
;;;
(objc:define-objc-method ("keyDown:" :void)
    ((self heml-view pointer) (event objc:objc-object-pointer))
  (handler-case
      (multiple-value-bind (descriptors direct) (event-descriptors event)
        (objc:invoke "NSCursor" "setHiddenUntilMouseMoves:" t)
        (note-caret-moved)
        (if (and direct (null (display-marked-text *display*)))
            (dolist (descriptor descriptors)
              (post-to-editor descriptor))
            (unless (and direct (null descriptors))
              (objc:invoke pointer "interpretKeyEvents:"
                           (objc:invoke "NSArray" "arrayWithObject:" event)))))
    (error (condition) (log-error "keyDown:" condition))))

;;;; NSTextInputClient

;;; What the input context calls.  The document is Heml's, not
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
    ((self heml-view) (string objc:objc-object-pointer) (range cocoa:ns-range))
  (declare (ignore range))
  (handler-case
      (progn
        (set-marked-text nil)
        (dolist (descriptor (text-descriptors (text-of string)))
          (post-to-editor descriptor)))
    (error (condition) (log-error "insertText:" condition))))

(objc:define-objc-method ("setMarkedText:selectedRange:replacementRange:" :void)
    ((self heml-view) (string objc:objc-object-pointer)
     (selected cocoa:ns-range) (replacement cocoa:ns-range))
  (declare (ignore selected replacement))
  (handler-case (set-marked-text (text-of string))
    (error (condition) (log-error "setMarkedText:" condition))))

(objc:define-objc-method ("unmarkText" :void) ((self heml-view))
  (set-marked-text nil))

(objc:define-objc-method ("hasMarkedText" objc:objc-bool) ((self heml-view))
  (not (null (display-marked-text *display*))))

(objc:define-objc-method ("markedRange" cocoa:ns-range) ((self heml-view))
  (let ((marked (display-marked-text *display*)))
    (if marked
        (cons 0 (length marked))
        (cons cocoa:ns-not-found 0))))

(objc:define-objc-method ("selectedRange" cocoa:ns-range) ((self heml-view))
  (let ((marked (display-marked-text *display*)))
    (cons (if marked (length marked) 0) 0)))

(objc:define-objc-method ("attributedSubstringForProposedRange:actualRange:"
                          objc:objc-object-pointer)
    ((self heml-view) (range cocoa:ns-range) (actual (:pointer :void)))
  (declare (ignore range actual))
  (cffi:null-pointer))

(objc:define-objc-method ("validAttributesForMarkedText" objc:objc-object-pointer)
    ((self heml-view))
  (objc:invoke "NSArray" "array"))

(objc:define-objc-method ("firstRectForCharacterRange:actualRange:" cocoa:ns-rect)
    ((self heml-view pointer) (range cocoa:ns-range) (actual (:pointer :void)))
  (declare (ignore range actual))
  ;; Where the candidate window goes: at the cursor, in screen coordinates.
  (let* ((display *display*)
         (screen *screen*)
         (x (or (screen-shown-cursor-x screen) 0))
         (y (or (screen-shown-cursor-y screen) 0))
         (in-window (objc:invoke pointer "convertRect:toView:"
                                 (vector (df (cell-x display x)) (df (cell-y display y))
                                         (df (display-char-width display))
                                         (df (display-char-height display)))
                                 nil)))
    (objc:invoke (display-window display) "convertRectToScreen:" in-window)))

(objc:define-objc-method ("characterIndexForPoint:" (:unsigned :long))
    ((self heml-view) (point cocoa:ns-point))
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
    ((self heml-view) (selector objc:sel))
  (let ((name (cdr (assoc (objc:selector-name selector) *command-selector-keys*
                          :test #'string=))))
    (when name
      (post-to-editor (list :named name '())))))

;;;; Menu actions

;;; Every item of the menus, and of the context menu, is the application
;;; delegate's -hemlMenuItem:, and its tag says which action it is:
;;;
;;;   (:command name arg ...)  a Heml command, run by the command loop
;;;   (:call function)         a function called on the main thread
;;;   (:font-size delta)       bigger, smaller, or with NIL the default size
;;;
;;; Items for AppKit's own selectors -- hide:, terminate:, toggleFullScreen:
;;; -- keep them, with no target, so that the responder chain finds them.

(defvar *menu-actions* (make-array 0 :adjustable t :fill-pointer t))

(defvar *mode-menus* '()
  "Each mode's menu-bar item, as (MODE . ITEM).  Main thread only.")

(defvar *context-menu-objects* '()
  "Each context menu made, as (MODE . MENU), MODE NIL for the general one.")

(defun menu-action-tag (action)
  (or (position action *menu-actions* :test #'equal)
      (vector-push-extend action *menu-actions*)))

(defun perform-menu-action (action)
  (ecase (first action)
    (:command (post-to-editor (cons :command (rest action))))
    (:call (funcall (second action)))
    (:font-size (change-font-size (second action)))))

(objc:define-objc-method ("hemlMenuItem:" :void)
    ((self app-delegate) (sender objc:objc-object-pointer))
  (handler-case
      (when *display*
        (perform-menu-action (aref *menu-actions* (objc:invoke sender "tag"))))
    (error (condition) (log-error "hemlMenuItem:" condition))))

(defun show-font-panel ()
  (let ((manager (objc:invoke "NSFontManager" "sharedFontManager")))
    (objc:invoke manager "setSelectedFont:isMultiple:" (display-font *display*) nil)
    ;; The editor's view takes the choice whatever window is in front: the
    ;; Settings window's button opens the panel too.
    (objc:invoke manager "setTarget:" (display-view *display*))
    (objc:invoke manager "orderFrontFontPanel:" nil)))

(objc:define-objc-method ("changeFont:" :void)
    ((self heml-view) (sender objc:objc-object-pointer))
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

(defun open-init-file ()
  "The init file, which is where Heml's settings are: the first that exists
of those Heml loads, or else ~/.config/heml/init.lisp, made in a directory
that exists, ready to save."
  (let ((file (hi::init-file)))
    (ensure-directories-exist file)
    (post-to-editor (list :open (namestring file)))))

(defun show-about ()
  (let ((options (objc:alloc-init-object "NSMutableDictionary")))
    (objc:invoke options "setObject:forKey:" "Heml" "ApplicationName")
    (objc:invoke options "setObject:forKey:"
                 (princ-to-string hi::*heml-version*) "ApplicationVersion")
    (objc:invoke options "setObject:forKey:"
                 (format nil "Heml on ~A ~A" (lisp-implementation-type)
                         (lisp-implementation-version))
                 "Version")
    (objc:invoke (objc.runloop:shared-application)
                 "orderFrontStandardAboutPanelWithOptions:" options)
    (objc:release options)))

(defmacro define-mouse-method (selector &body body)
  `(objc:define-objc-method (,selector :void)
       ((self heml-view) (event objc:objc-object-pointer))
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
    ((self heml-view) (event objc:objc-object-pointer))
  (handler-case
      (progn
        (when *display* (post-mouse "Rightdown" event))
        (context-menu))
    (error (condition)
      (log-error "menuForEvent:" condition)
      (cffi:null-pointer))))

;;; The middle button, and any others, which Heml has no names for.
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
  (if *hosted*
      ;; A guest's window is put away, not quit: the buffers live on, and the
      ;; host shows it again when it is next asked for.
      (hide-window)
      ;; Heml decides: it may want to save files first.
      (post-to-editor :quit))
  nil)

(objc:define-objc-method ("windowDidBecomeKey:" :void)
    ((self window-delegate) (notification objc:objc-object-pointer))
  (declare (ignore notification))
  (when *hosted* (use-hosted-menubar t))
  (request-redraw))

(objc:define-objc-method ("windowDidResignKey:" :void)
    ((self window-delegate) (notification objc:objc-object-pointer))
  (declare (ignore notification))
  (when *hosted* (use-hosted-menubar nil))
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
  ;; Quit asks Heml to exit as C-x C-c would, and the application ends
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

(defconstant +shift-key-mask+ (ash 1 17))
(defconstant +control-key-mask+ (ash 1 18))
(defconstant +option-key-mask+ (ash 1 19))
(defconstant +command-key-mask+ (ash 1 20))

(defun make-menu (title)
  (objc:invoke (objc:invoke "NSMenu" "alloc") "initWithTitle:" title))

(defun menu-font ()
  (objc:invoke "NSFont" "menuFontOfSize:" 0d0))

(defun menu-text-width (string)
  (aref (objc:invoke (objc:string-to-ns-string string) "sizeWithAttributes:"
                     (let ((attributes (objc:alloc-init-object "NSMutableDictionary")))
                       (objc:invoke attributes "setObject:forKey:" (menu-font) "NSFont")
                       attributes))
        0))

(defun binding-tab-stop (entries)
  "Where a menu's key bindings end, right-aligned: past its widest item."
  (loop for entry in entries
        when (consp entry)
          maximize (+ (menu-text-width (first entry)) 32
                      (let ((binding (getf (cddr entry) :binding)))
                        (if binding (menu-text-width binding) 0)))))

(defun titled-with-binding (title binding tab-stop)
  "TITLE with BINDING after it at TAB-STOP, right-aligned and grey, as an
   attributed string: a key sequence such as C-c a a is no key equivalent
   AppKit can show."
  (let ((style (objc:alloc-init-object "NSMutableParagraphStyle"))
        (plain (objc:alloc-init-object "NSMutableDictionary"))
        (grey (objc:alloc-init-object "NSMutableDictionary"))
        (text (objc:invoke (objc:invoke "NSMutableAttributedString" "alloc") "init")))
    (objc:invoke style "setTabStops:"
                 (objc:invoke "NSArray" "arrayWithObject:"
                              (objc:invoke (objc:invoke "NSTextTab" "alloc")
                                           "initWithTextAlignment:location:options:"
                                           2 (df tab-stop) ; right-aligned
                                           (objc:alloc-init-object "NSDictionary"))))
    (dolist (attributes (list plain grey))
      (objc:invoke attributes "setObject:forKey:" (menu-font) "NSFont")
      (objc:invoke attributes "setObject:forKey:" style "NSParagraphStyle"))
    (objc:invoke grey "setObject:forKey:" (objc:invoke "NSColor" "secondaryLabelColor") "NSColor")
    (flet ((add (string attributes)
             (objc:invoke text "appendAttributedString:"
                          (objc:invoke (objc:invoke "NSAttributedString" "alloc")
                                       "initWithString:attributes:" string attributes))))
      (add (format nil "~A~C" title #\Tab) plain)
      (add binding grey))
    text))

(defun menu-entry (entry target &optional tab-stop)
  "The NSMenuItem for ENTRY of a menu, its actions TARGET's; a key binding
   the entry has is shown at TAB-STOP."
  (if (eq entry :separator)
      (objc:invoke "NSMenuItem" "separatorItem")
      (destructuring-bind (title action &key key modifiers hidden binding) entry
        (when (stringp action)
          (setf action (list :command action)))
        (let ((item (objc:alloc-init-object "NSMenuItem")))
          (objc:invoke item "setTitle:" title)
          (when (and binding tab-stop (not key))
            (objc:invoke item "setAttributedTitle:" (titled-with-binding title binding tab-stop)))
          (cond ((eq action :services)
                 (let ((services (make-menu title)))
                   (objc:invoke item "setSubmenu:" services)
                   ;; The application's Services menu is its own host's.
                   (unless *hosted*
                     (objc:invoke (objc.runloop:shared-application)
                                  "setServicesMenu:" services))))
                ((eq (first action) :selector)
                 (objc:invoke item "setAction:" (objc:coerce-to-selector (second action))))
                (t
                 (objc:invoke item "setAction:" (objc:coerce-to-selector "hemlMenuItem:"))
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
  (let ((menu (make-menu title))
        (tab-stop (and (some (lambda (entry) (and (consp entry) (getf (cddr entry) :binding)))
                             entries)
                       (binding-tab-stop entries))))
    (dolist (entry entries menu)
      (objc:invoke menu "addItem:" (menu-entry entry target tab-stop)))))

(defvar *context-menu-specs* '()
  "The right-click menus the menu bar was last built with, as (MODE . ENTRIES).
Main thread only.")

(defvar *hosted-menubar* nil
  "A guest's menu bar, retained, while the host's is the application's.")

(defvar *host-menubar* nil
  "The host's menu bar, retained, while a guest's has taken its place.")

(defun heml-window-key-p ()
  (and *display*
       (objc:invoke-bool (display-window *display*) "isKeyWindow")))

(defun use-hosted-menubar (heml-key-p)
  "Make the menu bar Heml's while its window is key, and the host's otherwise.
Main thread."
  (let ((app (objc.runloop:shared-application)))
    (cond ((and heml-key-p *hosted-menubar*)
           (let ((current (objc:invoke app "mainMenu")))
             (unless (or (null-pointer-p current)
                         (cffi:pointer-eq current *hosted-menubar*))
               (when *host-menubar* (objc:release *host-menubar*))
               (setf *host-menubar* (objc:retain current))))
           (objc:invoke app "setMainMenu:" *hosted-menubar*))
          ((and (not heml-key-p) *host-menubar*)
           (objc:invoke app "setMainMenu:" *host-menubar*)
           (objc:release *host-menubar*)
           (setf *host-menubar* nil)))))

(defun rebuild-main-menu (menus context-menus)
  "Make the menu bar from MENUS, as HEML::COPY-MENUS gives them, and keep
CONTEXT-MENUS for the right click.  Main thread."
  (let* ((app (objc.runloop:shared-application))
         (target (objc:objc-object-pointer (display-app-delegate *display*)))
         (menubar (make-menu "")))
    (setf *mode-menus* '())
    (dolist (spec menus)
      (destructuring-bind (title mode role entries) spec
        (let ((menu (build-menu title entries target))
              (item (objc:alloc-init-object "NSMenuItem")))
          (objc:invoke item "setTitle:" title)
          (objc:invoke item "setSubmenu:" menu)
          (objc:invoke menubar "addItem:" item)
          (unless *hosted*
            (case role
              (:windows (objc:invoke app "setWindowsMenu:" menu))
              (:help (objc:invoke app "setHelpMenu:" menu))))
          (when mode
            ;; A mode's menu: hidden until its mode is current.
            (push (cons mode (objc:retain item)) *mode-menus*)
            (objc:invoke item "setHidden:" t)))))
    (cond ((not *hosted*)
           (objc:invoke app "setMainMenu:" menubar))
          (t
           ;; A guest's menu bar is the menu bar only while its window is key:
           ;; kept here, and swapped in and out by the window's delegate.
           (when *hosted-menubar* (objc:release *hosted-menubar*))
           (setf *hosted-menubar* (objc:retain menubar))
           (when (heml-window-key-p)
             (objc:invoke app "setMainMenu:" menubar))))
    (show-mode-menus (and *screen* (screen-shown-mode *screen*)))
    (setf *context-menu-specs* context-menus
          *context-menu-objects* '())))

(defun menus-changed ()
  "The menus changed, on the editor thread or while loading: build the menu
bar again, from a copy, on the main thread."
  (when *display*
    (let ((menus (heml::copy-menus))
          (context-menus (copy-tree heml::*context-menus*)))
      (on-main-thread (rebuild-main-menu menus context-menus)))))

(pushnew 'menus-changed heml::*menu-change-functions*)

(defun show-mode-menus (mode)
  "Show the menus of MODE, the current buffer's major mode, and hide the
other modes'.  Main thread."
  (loop for (menu-mode . item) in *mode-menus*
        do (objc:invoke item "setHidden:" (not (equal menu-mode mode)))))

(defun context-menu ()
  "The right click's menu for the current buffer's mode."
  (let* ((screen *screen*)
         (mode (and screen (screen-shown-mode screen)))
         (key (and (assoc mode *context-menu-specs* :test #'equal) mode)))
    (or (cdr (assoc key *context-menu-objects* :test #'equal))
        (let ((menu (objc:retain
                     (build-menu "" (cdr (assoc key *context-menu-specs* :test #'equal))
                                 (objc:objc-object-pointer (display-app-delegate *display*))))))
          (push (cons key menu) *context-menu-objects*)
          menu))))

;;; What only the Mac editor can do: its application and Window menus, the
;;; Open and Save panels, the font, full screen and the character palette.
;;; The rest of the menus are Heml's own (menus.lisp).

(heml-interface:define-menu "Heml" (:role :application)
  ("About Heml" (:call show-about))
  :separator
  ("Settings…" (:call open-settings) :key ",")
  :separator
  ("Services" :services)
  :separator
  ("Hide Heml" (:selector "hide:") :key "h")
  ("Hide Others" (:selector "hideOtherApplications:") :key "h" :modifiers (:option))
  ("Show All" (:selector "unhideAllApplications:"))
  :separator
  ("Quit Heml" (:selector "terminate:") :key "q"))

(heml-interface:define-menu "Window" (:role :windows)
  ("Minimize" (:selector "performMiniaturize:") :key "m")
  ("Zoom" (:selector "performZoom:"))
  :separator
  ("Bring All to Front" (:selector "arrangeInFront:")))

(heml-interface:add-menu-item "File" '("Open…" (:call choose-files-to-open) :key "o")
                              :after "New Buffer…")
(heml-interface:add-menu-item "File" '("Save As…" (:call choose-file-to-save-as)
                                       :key "s" :modifiers (:shift))
                              :after "Save")
(heml-interface:add-menu-item "Edit" :separator)
(heml-interface:add-menu-item "Edit" '("Emoji & Symbols" (:selector "orderFrontCharacterPalette:")
                                       :key " " :modifiers (:control)))
(dolist (entry '(("Show Fonts" (:call show-font-panel) :key "t")
                 ("Bigger" (:font-size 1) :key "+")
                 ;; Cmd-= is Cmd-+ without the Shift nobody presses.
                 ("Bigger (=)" (:font-size 1) :key "=" :hidden t)
                 ("Smaller" (:font-size -1) :key "-")
                 ("Default Size" (:font-size nil) :key "0")))
  (heml-interface:add-menu-item "View" entry :before "Split Window"))
(heml-interface:add-menu-item "View" :separator :before "Split Window")
(heml-interface:add-menu-item "View" :separator)
(heml-interface:add-menu-item "View" '("Enter Full Screen" (:selector "toggleFullScreen:")
                                       :key "f" :modifiers (:control)))

(defun make-window (display)
  (let* ((width (+ (* 2 *margin*) (* *initial-columns* (display-char-width display))))
         (height (+ (* 2 *margin*) (* *initial-lines* (display-char-height display))))
         (rect (vector 0d0 0d0 (df width) (df height)))
         (window (objc:invoke (objc:invoke "NSWindow" "alloc")
                              "initWithContentRect:styleMask:backing:defer:"
                              rect +window-style-mask+ +backing-store-buffered+ nil))
         (view-object (make-instance 'heml-view))
         (view (objc:objc-object-pointer view-object))
         (delegate (make-instance 'window-delegate)))
    ;; Lisp owns the window: closing it must not free it under us.
    (objc:invoke window "setReleasedWhenClosed:" nil)
    (objc:invoke window "setTitle:" "Heml")
    (objc:invoke view "setFrame:" rect)
    (objc:invoke window "setContentView:" view)
    (objc:invoke (objc:invoke "NSNotificationCenter" "defaultCenter")
                 "addObserver:selector:name:object:"
                 view (objc:coerce-to-selector "hemlSystemColorsChanged:")
                 "NSSystemColorsDidChangeNotification" nil)
    (objc:invoke window "setDelegate:" (objc:objc-object-pointer delegate))
    (objc:invoke window "makeFirstResponder:" view)
    ;; The caret's blink.
    (objc:invoke "NSTimer" "scheduledTimerWithTimeInterval:target:selector:userInfo:repeats:"
                 *blink-interval* view (objc:coerce-to-selector "hemlBlink:")
                 (cffi:null-pointer) t)
    (objc:invoke window "center")
    ;; Where it was when last closed, and reopened after a logout.
    (when *remember-window-frame*
      (objc:invoke window "setFrameAutosaveName:" "HemlWindow")
      (objc:invoke window "setRestorable:" t))
    (setf (display-window display) window
          (display-view display) view
          (display-view-object display) view-object
          (display-delegate display) delegate
          *main-thread-target* view)
    (fit-window-to-cell display)
    (restore-chrome display)
    display))

(defun use-icon-if-unbundled (app)
  "Give a process started from a REPL the application's icon in the Dock.
A bundle has its own, from its Info.plist."
  (let ((path (ignore-errors
               (asdf:system-relative-pathname :heml.cocoa "resources/heml.png"))))
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
            ;; Made either way: it is the menu items' target.  It is the
            ;; APPLICATION's delegate only when the application is Heml's.
            (setf (display-app-delegate display) app-delegate)
            (unless *hosted*
              (objc:invoke app "setDelegate:" (objc:objc-object-pointer app-delegate))))
          (unless *hosted*
            (use-icon-if-unbundled app))
          (restore-font-choice)
          (install-fonts display)
          (make-window display)
          (multiple-value-bind (columns lines) (grid-size display)
            (setf *screen* (make-screen columns lines)))
          (ensure-wakeup-pipe)
          (setf *display* display)
          (rebuild-main-menu (heml::copy-menus) (copy-tree heml::*context-menus*))
          display))))

(defun show-window ()
  (let ((display *display*))
    (objc:invoke (display-window display) "makeKeyAndOrderFront:" nil)
    (when *activate*
      (objc:invoke (objc.runloop:shared-application) "activateIgnoringOtherApps:" t))))

(defun hide-window ()
  (let ((display *display*))
    (when display
      (objc:invoke (display-window display) "orderOut:" nil))))
