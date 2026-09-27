;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;;; The editor-thread half of the Cocoa backend: Hemlock's device, its
;;;; hunks, editor input, and the redisplay methods that copy dis-lines into
;;;; the screen.  Window management is the TTY backend's (tty-screen.lisp):
;;;; the display is a grid of character cells, just as a terminal's is.
;;;;
;;;; Hunk geometry, in lines from the top of the grid: POSITION is the
;;;; hunk's bottom line, where its modeline is; TEXT-POSITION is its last
;;;; text line; the text starts at TEXT-POSITION - TEXT-HEIGHT + 1.

(in-package :hemlock.cocoa)

(pushnew :cocoa hi::*available-backends*)

(defclass cocoa-device (hi::device)
  ((dirty :initform nil :accessor device-dirty
          :documentation "Whether the screen has changed since the last redraw request.")))

(defclass cocoa-hunk (hi::device-hunk)
  ((text-position :initarg :text-position :accessor hunk-text-position)
   (text-height :initarg :text-height :accessor hunk-text-height)))

(defun make-hunk (device position height text-position text-height)
  (make-instance 'cocoa-hunk :device device
                             :position position :height height
                             :text-position text-position :text-height text-height))

(defun hunk-top-line (hunk)
  (1+ (- (hunk-text-position hunk) (hunk-text-height hunk))))

(defun current-device ()
  (hi::device-hunk-device (hi::window-hunk (hi::current-window))))


;;;; Editor input

(defclass cocoa-editor-input (hi::editor-input) ())

(defmethod hi::get-key-event
    ((stream cocoa-editor-input) &optional ignore-abort-attempts-p)
  (hi::%editor-input-method stream ignore-abort-attempts-p))

(defmethod hi::unget-key-event (key-event (stream cocoa-editor-input))
  (hi::un-event key-event stream))

(defmethod hi::clear-editor-input ((stream cocoa-editor-input))
  (take-inbox)
  (hemlock-ext:without-interrupts
    (let* ((head (hi::editor-input-head stream))
           (next (hi::input-event-next head)))
      (when next
        (setf (hi::input-event-next head) nil)
        (shiftf (hi::input-event-next (hi::editor-input-tail stream))
                hi::*free-input-events* next)
        (setf (hi::editor-input-tail stream) head)))))

(defmethod hi::listen-editor-input ((stream cocoa-editor-input))
  (hi::dispatch-events-no-hang)
  (not (null (hi::input-event-next (hi::editor-input-head stream)))))

(defmethod hi::backend-init-raw-io ((backend (eql :cocoa)) display)
  (declare (ignore display))
  (setf hi::*editor-input* (make-instance 'cocoa-editor-input))
  (setf hi::*real-editor-input* hi::*editor-input*))

(defun modifier-bits (names)
  (reduce #'logior names
          :key #'hemlock-ext::key-event-modifier-mask
          :initial-value 0))

(defun descriptor-key-event (descriptor)
  (destructuring-bind (kind thing modifiers) descriptor
    (let ((bits (modifier-bits modifiers)))
      (ecase kind
        (:named (hemlock-ext::make-key-event thing bits))
        ;; Any character: one Hemlock has no keysym for gets one, bound to
        ;; Self Insert, the first time it is typed.
        (:char (let ((key-event (hemlock-ext:character-key-event thing)))
                 (and key-event
                      (if (zerop bits)
                          key-event
                          (hemlock-ext:make-key-event key-event bits)))))))))

(defvar *menu-commands* '()
  "Menu commands waiting for the command loop, oldest first, each (name
arg ...).  The editor thread's only.")

(hi::defcommand "Menu Command" (p)
  "Run the command a menu item chose.  Bound to the key the Cocoa backend
queues for one, so that it runs as a typed command does, prefix argument
included."
  "Run the next command a menu chose."
  (let ((entry (pop *menu-commands*)))
    (when entry
      (destructuring-bind (name &rest arguments) entry
        (let ((command (hi::getstring name hi::*command-names*)))
          (unless command
            (hi::editor-error "No command ~S." name))
          (apply (hi::command-function command) p arguments))))))

(defun queue-key-event (key-event)
  (hi::q-event hi::*real-editor-input* key-event))

(defun process-inbox ()
  "Turn what the main thread has posted into Hemlock input.  Runs as the
wakeup connection's filter, inside DISPATCH-EVENTS on the editor thread."
  (dolist (item (take-inbox))
    (handler-case
        (cond
          ((eq item :quit)
           ;; What C-x C-c does, whatever it is bound to.
           (let ((control (modifier-bits '("Control"))))
             (queue-key-event (hemlock-ext::make-key-event "x" control))
             (queue-key-event (hemlock-ext::make-key-event "c" control))))
          ((eq (car item) :resize)
           (resize-screen (current-device) (second item) (third item)))
          ((eq (car item) :command)
           ;; A menu's command: kept here, and run by the command loop
           ;; when it reads the key that says so.
           (setf *menu-commands* (append *menu-commands* (list (rest item))))
           (queue-key-event (hemlock-ext:make-key-event "Menucommand" 0)))
          ((eq (car item) :open)
           ;; As a file named on the command line is visited.
           (hi::process-command-line-argument (second item)))
          ((eq (car item) :mouse)
           (destructuring-bind (name modifiers column line) (rest item)
             (queue-mouse-event name modifiers column line)))
          (t
           (let ((key-event (descriptor-key-event item)))
             (if key-event
                 (queue-key-event key-event)
                 (beep)))))
      (error (condition) (log-error "input" condition)))))

;;; Mouse input is a key-event queued with the position it happened at, the
;;; way Hemlock's pointer commands expect (LAST-KEY-EVENT-CURSORPOS): X and
;;; Y within the window's text, Y NIL on its modeline, and the hunk.
;;;
(defun locate-cell (column line)
  (dolist (hunk (cons (hi::window-hunk hi::*echo-area-window*)
                      (hunks-top-to-bottom (current-device)))
                (values nil nil nil))
    (let ((top (hunk-top-line hunk))
          (height (hunk-text-height hunk))
          (window (hi::device-hunk-window hunk)))
      (cond ((and (<= top line) (< line (+ top height)))
             (return (values column (- line top) hunk)))
            ((and (= line (+ top height)) (hi::window-modeline-buffer window))
             (return (values column nil hunk)))))))

(defun queue-mouse-event (name modifiers column line)
  (multiple-value-bind (x y hunk) (locate-cell column line)
    (hi::q-event hi::*real-editor-input*
                 (hemlock-ext:make-key-event name (modifier-bits modifiers))
                 x y hunk)))

;;; What makes the editor behave as a Mac application.  A click moves point
;;; and a drag marks a region, rather than CMU Hemlock's left button, which
;;; scrolled the line clicked to the top of the window; the active region
;;; looks like a selection; a double click selects a word and a triple
;;; click a line; a right click moves point unless it is in the selection,
;;; and opens a menu.  The menus' commands arrive as the Menucommand key,
;;; and the kill ring is joined to the general pasteboard.
;;;
(defun install-mac-bindings ()
  (flet ((key (name &rest modifiers)
           (hemlock-ext:make-key-event name (modifier-bits modifiers))))
    (hi::bind-key "Mouse Set Point" (key "Leftdown"))
    (hi::bind-key "Mouse Drag Region" (key "Leftup"))
    (hi::bind-key "Mouse Extend Region" (key "Leftdown" "Shift"))
    (hi::bind-key "Mouse Drag Region" (key "Leftup" "Shift"))
    (hi::bind-key "Mouse Select Word" (key "Doubleleftdown"))
    (hi::bind-key "Mouse Select Line" (key "Tripleleftdown"))
    (hi::bind-key "Mouse Point Unless In Region" (key "Rightdown"))
    (hi::bind-key "Do Nothing" (key "Rightup"))
    (hi::bind-key "Menu Command" (key "Menucommand")))
  (setf hemlock::*active-region-highlight-font* '(:bg :selection)
        hemlock::*interprogram-cut-function*
        (lambda (text) (on-main-thread (write-pasteboard text)))
        hemlock::*interprogram-paste-function*
        (lambda () (call-on-main-thread-and-wait #'read-pasteboard-if-changed))))

(defvar *wakeup-connection* nil)

(defun ensure-wakeup-connection ()
  (unless *wakeup-connection*
    (setf *wakeup-connection*
          (hi::make-pipelike-connection
           *wakeup-read-fd* *wakeup-read-fd*
           :name "Cocoa input"
           :buffer nil
           :filter (lambda (connection bytes)
                     (declare (ignore connection bytes))
                     (process-inbox)
                     nil)))))


;;;; Screen manager initialization

;;; The TTY layout: the main window above, its modeline, and the echo area
;;; with its own modeline at the bottom.
;;;
(defmethod hi::%init-screen-manager ((backend-type (eql :cocoa)) (display t))
  (declare (ignore display))
  (let* ((device (make-instance 'cocoa-device :name "Cocoa"))
         (width (screen-columns *screen*))
         (height (screen-lines *screen*))
         (echo-height (hi::value hemlock::echo-area-height))
         (main-lines (- height echo-height 1))
         (main-text-lines (1- main-lines))
         (last-text-line (1- main-text-lines)))
    (setf hi::*window-list* ())
    (setf (hi::device-bottom-window-base device) last-text-line)
    (let* ((echo-hunk (make-hunk device (1- height) echo-height (- height 2) echo-height))
           (echo (hi::internal-make-window :hunk echo-hunk)))
      (setf hi::*echo-area-window* echo)
      (setf (hi::device-hunk-window echo-hunk) echo)
      (hi::setup-window-image hi::*parse-starting-mark* echo echo-height width)
      (hi::setup-modeline-image hi::*echo-area-buffer* echo)
      (setf (hi::device-hunk-previous echo-hunk) echo-hunk
            (hi::device-hunk-next echo-hunk) echo-hunk))
    (let* ((main-hunk (make-hunk device main-text-lines main-lines
                                 last-text-line main-text-lines))
           (main (hi::internal-make-window :hunk main-hunk)))
      (setf (hi::device-hunk-window main-hunk) main)
      (setf hi::*current-window* main)
      (hi::setup-window-image (hi::buffer-point hi::*current-buffer*)
                              main main-text-lines width)
      (hi::setup-modeline-image hi::*current-buffer* main)
      (setf (hi::device-hunk-previous main-hunk) main-hunk
            (hi::device-hunk-next main-hunk) main-hunk)
      (setf (hi::device-hunks device) main-hunk))
    (ensure-wakeup-connection)
    (install-mac-bindings)
    device))


;;;; Redisplay

(defun font-runs (dis-line length)
  "DIS-LINE's font changes as ((start end . font) ...), leaving out the
default font and clipped to LENGTH."
  (let ((runs '()))
    (do ((change (hi::dis-line-font-changes dis-line) (hi::font-change-next change)))
        ((null change))
      (let* ((next (hi::font-change-next change))
             (start (min length (hi::font-change-x change)))
             (end (min length (if next (hi::font-change-x next) length)))
             (font (hi::font-change-font change)))
        (when (and font (not (eql font 0)) (< start end))
          (push (list* start end font) runs))))
    (nreverse runs)))

(defun store-row (screen line text runs)
  (when (< -1 line (screen-lines screen))
    (let ((row (svref (screen-rows screen) line)))
      (setf (row-text row) text
            (row-runs row) runs))))

(defun store-dis-line (screen line dis-line)
  (let ((length (hi::dis-line-length dis-line)))
    (store-row screen line
               (subseq (hi::dis-line-chars dis-line) 0 length)
               (font-runs dis-line length))))

(defun store-modeline (screen line dis-line)
  "A modeline fills its line: its text is padded to the width and its last
run, which carries the modeline's colours, is carried to the edge."
  (let* ((columns (screen-columns screen))
         (length (min columns (hi::dis-line-length dis-line)))
         (text (make-string columns :initial-element #\Space))
         (runs (font-runs dis-line length)))
    (replace text (hi::dis-line-chars dis-line) :end2 length)
    (when runs
      (setf (second (car (last runs))) columns))
    (store-row screen line text runs)))

(defun render-window (device window)
  "Copy all of WINDOW's image into the screen."
  (let* ((screen *screen*)
         (hunk (hi::window-hunk window))
         (top (hunk-top-line hunk))
         (height (hunk-text-height hunk))
         (first (hi::window-first-line window)))
    (with-screen-lock (screen)
      (dotimes (i height)
        (store-row screen (+ top i) "" '()))
      (do ((dl (cdr first) (cdr dl)))
          ((eq dl hi::the-sentinel))
        (let* ((dis-line (car dl))
               (position (hi::dis-line-position dis-line)))
          (when (< -1 position height)
            (store-dis-line screen (+ top position) dis-line))))
      (when (hi::window-modeline-buffer window)
        (store-modeline screen (+ top height)
                        (hi::window-modeline-dis-line window))))
    (setf (device-dirty device) t)))

;;; The whole window is copied every time, and the view draws all of it.
;;;
(defmethod hi::device-redisplay ((device cocoa-device) window)
  (render-window device window))

(defmethod hi::device-clear ((device cocoa-device))
  (let ((screen *screen*))
    (with-screen-lock (screen)
      (loop for row across (screen-rows screen)
            do (setf (row-text row) "" (row-runs row) '()))))
  (setf (device-dirty device) t))

(defmethod hi::device-put-cursor ((device cocoa-device) hunk x y)
  (let ((screen *screen*)
        (line (+ (hunk-top-line hunk) y)))
    (with-screen-lock (screen)
      (unless (and (eql x (screen-cursor-x screen))
                   (eql line (screen-cursor-y screen)))
        (setf (screen-cursor-x screen) x
              (screen-cursor-y screen) line
              (device-dirty device) t)))))

(defmethod hi::device-force-output ((device cocoa-device))
  (when (device-dirty device)
    (setf (device-dirty device) nil)
    (request-redraw)))

(defmethod hi::device-finish-output ((device cocoa-device) window)
  (declare (ignore window))
  (hi::device-force-output device))

(defmethod hi::device-init ((device cocoa-device))
  (setf hi::*screen-image-trashed* t))

(defmethod hi::device-exit ((device cocoa-device)))

(defmethod hi::device-beep ((device cocoa-device) stream)
  (declare (ignore stream))
  (beep))

;;; Show the cursor at X, Y for TIME seconds or until there is input: how
;;; the matching open paren is flashed.
;;;
(defmethod hi::device-show-mark ((device cocoa-device) window x y time)
  (cond ((hi::listen-editor-input hi::*editor-input*) nil)
        (x (hi::internal-redisplay)
           (hi::device-put-cursor device (hi::window-hunk window) x y)
           (hi::device-force-output device)
           (let ((deadline (+ (get-internal-real-time)
                              (round (* (or time 0) internal-time-units-per-second)))))
             (loop while (and (inbox-empty-p)
                              (< (get-internal-real-time) deadline))
                   do (sleep 0.01)))
           t)
        (t nil)))


;;;; Windows

(defmethod hi::device-next-window ((device cocoa-device) window)
  (hi::device-hunk-window (hi::device-hunk-next (hi::window-hunk window))))

(defmethod hi::device-previous-window ((device cocoa-device) window)
  (hi::device-hunk-window (hi::device-hunk-previous (hi::window-hunk window))))

(defmethod hi::device-make-window ((device cocoa-device) start modelinep proportion)
  (let* ((old-window (hi::current-window))
         (victim (hi::window-hunk old-window))
         (text-height (hunk-text-height victim))
         (availability (if modelinep (1- text-height) text-height)))
    (when (> availability 1)
      (let* ((new-lines (truncate (* availability proportion)))
             (old-lines (- availability new-lines))
             (pos (hi::device-hunk-position victim))
             (new-height (if modelinep (1+ new-lines) new-lines))
             (new-text-pos (if modelinep (1- pos) pos))
             (new-hunk (make-hunk device pos new-height new-text-pos new-lines))
             (new-window (hi::internal-make-window :hunk new-hunk)))
        (setf (hi::device-hunk-window new-hunk) new-window)
        (let* ((old-text-pos-diff (- pos (hunk-text-position victim)))
               (old-win-new-pos (- pos new-height)))
          (setf (hi::device-hunk-height victim)
                (- (hi::device-hunk-height victim) new-height))
          (setf (hunk-text-height victim) old-lines)
          (setf (hi::device-hunk-position victim) old-win-new-pos)
          (setf (hunk-text-position victim)
                (- old-win-new-pos old-text-pos-diff)))
        (hi::setup-window-image start new-window new-lines
                                (hi::window-width old-window))
        (when modelinep
          (hi::setup-modeline-image (hi::line-buffer (hi::mark-line start)) new-window))
        (hi::change-window-image-height old-window old-lines)
        (shiftf (hi::device-hunk-previous new-hunk)
                (hi::device-hunk-previous (hi::device-hunk-next victim))
                new-hunk)
        (shiftf (hi::device-hunk-next new-hunk)
                (hi::device-hunk-next victim)
                new-hunk)
        (setf hi::*screen-image-trashed* t)
        new-window))))

(defmethod hi::device-delete-window ((device cocoa-device) window)
  (let* ((hunk (hi::window-hunk window))
         (prev (hi::device-hunk-previous hunk))
         (next (hi::device-hunk-next hunk)))
    (setf (hi::device-hunk-next prev) next)
    (setf (hi::device-hunk-previous next) prev)
    (let ((buffer (hi::window-buffer window)))
      (setf (hi::buffer-windows buffer) (delete window (hi::buffer-windows buffer))))
    (let ((new-lines (hi::device-hunk-height hunk)))
      (cond ((eq hunk (hi::device-hunks device))
             (incf (hi::device-hunk-height next) new-lines)
             (incf (hunk-text-height next) new-lines)
             (let ((w (hi::device-hunk-window next)))
               (hi::change-window-image-height w (+ new-lines (hi::window-height w)))))
            (t
             (incf (hi::device-hunk-height prev) new-lines)
             (incf (hi::device-hunk-position prev) new-lines)
             (incf (hunk-text-height prev) new-lines)
             (incf (hunk-text-position prev) new-lines)
             (let ((w (hi::device-hunk-window prev)))
               (hi::change-window-image-height w (+ new-lines (hi::window-height w)))))))
    (when (eq hunk (hi::device-hunks device))
      (setf (hi::device-hunks device) next)))
  (setf hi::*screen-image-trashed* t))

(defun hunks-top-to-bottom (device)
  "The window hunks in screen order; the echo area's is not among them."
  (let ((first (hi::device-hunks device)))
    (loop for hunk = first then (hi::device-hunk-next hunk)
          collect hunk
          until (eq (hi::device-hunk-next hunk) first))))

(defun grow-hunk (device hunk delta)
  "Make HUNK DELTA lines taller, moving every hunk below it down."
  (incf (hi::device-hunk-height hunk) delta)
  (incf (hunk-text-height hunk) delta)
  (let ((w (hi::device-hunk-window hunk)))
    (hi::change-window-image-height w (+ delta (hi::window-height w))))
  (dolist (below (cons hunk (rest (member hunk (hunks-top-to-bottom device)))))
    (incf (hi::device-hunk-position below) delta)
    (incf (hunk-text-position below) delta))
  (let ((echo (hi::window-hunk hi::*echo-area-window*)))
    (incf (hi::device-hunk-position echo) delta)
    (incf (hunk-text-position echo) delta)))

(defmethod hi::device-enlarge-window ((device cocoa-device) window offset)
  (let* ((hunk (hi::window-hunk window))
         (hunks (hunks-top-to-bottom device))
         (victim (or (second (member hunk hunks))
                     (second (member hunk (reverse hunks))))))
    (unless victim
      (hi::editor-error "Cannot enlarge only window"))
    (when (< (- (hunk-text-height victim) offset) 1)
      (hi::editor-error "Not enough room"))
    ;; Take the lines from the neighbour, then give them to the window;
    ;; each step keeps the hunks below in place.
    (grow-hunk device victim (- offset))
    (grow-hunk device hunk offset)
    (setf hi::*screen-image-trashed* t)))

(defmethod hi::enlarge-device ((device cocoa-device) offset)
  "OFFSET more lines (or fewer): a gain goes to the first window, a loss
comes a line at a time from whichever window is tallest."
  (if (plusp offset)
      (grow-hunk device (hi::device-hunks device) offset)
      (loop repeat (- offset)
            for tallest = (reduce (lambda (a b)
                                    (if (>= (hunk-text-height a) (hunk-text-height b)) a b))
                                  (hunks-top-to-bottom device))
            while (> (hunk-text-height tallest) 1)
            do (grow-hunk device tallest -1)))
  (setf hi::*screen-image-trashed* t))


;;;; Resizing

(defun change-window-image-width (window width)
  "Give WINDOW dis-lines WIDTH characters wide, and a modeline to match.
The image is rebuilt by the next redisplay."
  (unless (eq (cdr (hi::window-first-line window)) hi::the-sentinel)
    (shiftf (cdr (hi::window-last-line window))
            (hi::window-spare-lines window)
            (cdr (hi::window-first-line window))
            hi::the-sentinel))
  (setf (hi::window-spare-lines window)
        (loop repeat (max (length (hi::window-spare-lines window))
                          (* 2 (hi::window-height window)))
              collect (hi::make-window-dis-line (make-string width))))
  (setf (hi::window-width window) width)
  (let ((dis-line (hi::window-modeline-dis-line window)))
    (when (and dis-line (hi::window-modeline-buffer window))
      (setf (hi::dis-line-chars dis-line) (make-string width :initial-element #\Space))
      (hi::update-modeline-fields (hi::window-buffer window) window))))

(defun resize-screen (device columns lines)
  (let* ((screen *screen*)
         (delta (- lines (screen-lines screen)))
         (columns-changed (/= columns (screen-columns screen))))
    (when (or columns-changed (/= delta 0))
      (with-screen-lock (screen)
        (setf (screen-columns screen) columns
              (screen-lines screen) lines
              (screen-rows screen) (make-rows lines)
              (screen-cursor-x screen) nil
              (screen-cursor-y screen) nil))
      (when columns-changed
        (dolist (window hi::*window-list*)
          (change-window-image-width window columns)))
      (unless (zerop delta)
        (hi::enlarge-device device delta))
      (setf hi::*screen-image-trashed* t))))


;;;; Font commands

;;; The View menu's actions, for M-x and key bindings.  They run on the main
;;; thread, which owns the fonts; the grid follows as a :RESIZE.

(hi::defcommand "Increase Font Size" (p)
  "Make the font a point larger, or P points."
  "Make the font larger."
  (let ((delta (or p 1)))
    (on-main-thread (change-font-size delta))))

(hi::defcommand "Decrease Font Size" (p)
  "Make the font a point smaller, or P points."
  "Make the font smaller."
  (let ((delta (- (or p 1))))
    (on-main-thread (change-font-size delta))))

(hi::defcommand "Default Font Size" (p)
  "Go back to the default font size, or with an argument, make it P points."
  "Set the font size."
  (on-main-thread (if p (change-font :size p) (change-font-size nil))))

(hi::defcommand "Select Font" (p)
  "Open the font panel; the font chosen there is used, and kept for next time."
  "Open the font panel."
  (declare (ignore p))
  (on-main-thread (show-font-panel)))

(hi::defcommand "Use System Font" (p)
  "Go back to the system monospaced font, keeping the size."
  "Use the system monospaced font."
  (declare (ignore p))
  (on-main-thread (change-font :name nil)))
