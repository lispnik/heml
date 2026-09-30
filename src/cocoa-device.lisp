;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;;; The editor-thread half of the Cocoa backend: Heml's device, its
;;;; hunks, editor input, and the redisplay methods that copy dis-lines into
;;;; the screen.  Window management is the TTY backend's (tty-screen.lisp):
;;;; the display is a grid of character cells, just as a terminal's is.
;;;;
;;;; Hunk geometry, in lines from the top of the grid: POSITION is the
;;;; hunk's bottom line, where its modeline is; TEXT-POSITION is its last
;;;; text line; the text starts at TEXT-POSITION - TEXT-HEIGHT + 1.  COLUMN
;;;; and WIDTH say which columns are its.  layout.lisp sets them all.

(in-package :heml.cocoa)

(pushnew :cocoa hi::*available-backends*)

(defclass cocoa-device (hi::device)
  ((dirty :initform nil :accessor device-dirty
          :documentation "Whether the screen has changed since the last redraw request.")))

(defclass cocoa-hunk (hi::device-hunk) ())

(defmethod hi::device-make-hunk ((device cocoa-device))
  (make-instance 'cocoa-hunk :device device))

(defun hunk-top-line (hunk)
  (1+ (- (hi::device-hunk-text-position hunk) (hi::device-hunk-text-height hunk))))

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
  (heml-ext:without-interrupts
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
          :key #'heml-ext::key-event-modifier-mask
          :initial-value 0))

(defun descriptor-key-event (descriptor)
  (destructuring-bind (kind thing modifiers) descriptor
    (let ((bits (modifier-bits modifiers)))
      (ecase kind
        (:named (heml-ext::make-key-event thing bits))
        ;; Any character: one Heml has no keysym for gets one, bound to
        ;; Self Insert, the first time it is typed.
        (:char (let ((key-event (heml-ext:character-key-event thing)))
                 (and key-event
                      (if (zerop bits)
                          key-event
                          (heml-ext:make-key-event key-event bits)))))))))

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
  "Turn what the main thread has posted into Heml input.  Runs as the
wakeup connection's filter, inside DISPATCH-EVENTS on the editor thread."
  (dolist (item (take-inbox))
    (handler-case
        (cond
          ((eq item :quit)
           ;; What C-x C-c does, whatever it is bound to.
           (let ((control (modifier-bits '("Control"))))
             (queue-key-event (heml-ext::make-key-event "x" control))
             (queue-key-event (heml-ext::make-key-event "c" control))))
          ((eq (car item) :resize)
           (resize-screen (current-device) (second item) (third item)))
          ((eq (car item) :command)
           ;; A menu's command: kept here, and run by the command loop
           ;; when it reads the key that says so.
           (setf *menu-commands* (append *menu-commands* (list (rest item))))
           (queue-key-event (heml-ext:make-key-event "Menucommand" 0)))
          ((eq (car item) :open)
           ;; As a file named on the command line is visited.
           (hi::process-command-line-argument (second item)))
          ((eq (car item) :mouse)
           (destructuring-bind (name modifiers column line) (rest item)
             (unless (drag-border name column line)
               (queue-mouse-event name modifiers column line))))
          (t
           (let ((key-event (descriptor-key-event item)))
             (if key-event
                 (queue-key-event key-event)
                 (beep)))))
      (error (condition) (log-error "input" condition)))))

;;; Mouse input is a key-event queued with the position it happened at, the
;;; way Heml's pointer commands expect (LAST-KEY-EVENT-CURSORPOS): X and
;;; Y within the window's text, Y NIL on its modeline, and the hunk.
;;;
(defun locate-cell (column line)
  (dolist (hunk (cons (hi::window-hunk hi::*echo-area-window*)
                      (hi::device-window-hunks (current-device)))
                (values nil nil nil))
    (let ((top (hunk-top-line hunk))
          (height (hi::device-hunk-text-height hunk))
          (left (hi::device-hunk-column hunk))
          (window (hi::device-hunk-window hunk)))
      (when (and (<= left column) (< column (+ left (hi::device-hunk-width hunk))))
        (cond ((and (<= top line) (< line (+ top height)))
               (return (values (- column left) (- line top) hunk)))
              ((and (= line (+ top height)) (hi::window-modeline-buffer window))
               (return (values (- column left) nil hunk))))))))

;;; Windows are resized by dragging what divides them: the bar between
;;; windows side by side, or a modeline, which is its window's bottom edge.
;;; A press there starts a drag, and until the release, the pointer moves
;;; that edge instead of reaching the editor as keys.  A press on a
;;; modeline still selects its window.

(defvar *border-drag* nil
  "While an edge is dragged: (DIRECTION HUNK WHERE), the hunk whose right
side or bottom it is, and the column or line it is at.")

;;; Every edge a drag can move, for the view's cursor rectangles.
;;;
(defun layout-borders ()
  (let ((device (current-device))
        (screen-columns (screen-columns *screen*))
        (borders '()))
    (dolist (hunk (hi::device-window-hunks device) (nreverse borders))
      (let ((left (hi::device-hunk-column hunk))
            (width (hi::device-hunk-width hunk))
            (top (hunk-top-line hunk))
            (bottom (hi::device-hunk-position hunk)))
        (when (< (+ left width) screen-columns)
          (push (list :columns (+ left width) top 1 (1+ (- bottom top))) borders))
        (when (and (hi::device-hunk-modelinep hunk)
                   (hi::layout-edge-split device hunk :rows))
          (push (list :rows left bottom width 1) borders))))))

(defun border-at (column line)
  "The edge at the cell, as (DIRECTION HUNK WHERE), or NIL."
  (let ((screen-columns (screen-columns *screen*)))
    (dolist (hunk (hi::device-window-hunks (current-device)))
      (let ((left (hi::device-hunk-column hunk))
            (width (hi::device-hunk-width hunk))
            (top (hunk-top-line hunk))
            (bottom (hi::device-hunk-position hunk)))
        (cond ((and (= column (+ left width))
                    (< column screen-columns)
                    (<= top line bottom))
               (return (list :columns hunk column)))
              ((and (= line bottom)
                    (hi::device-hunk-modelinep hunk)
                    (<= left column) (< column (+ left width)))
               (return (list :rows hunk line))))))))

(defun drag-border (name column line)
  "Start, continue or end dragging an edge.  True when the event was
that, and should not also reach the editor as a key."
  (cond ((and (string= name "Leftdown") (not *border-drag*))
         (let ((border (border-at column line)))
           (when border
             (setf *border-drag* border)
             ;; A modeline's press goes on, to select its window.
             (eq (first border) :columns))))
        ((null *border-drag*) nil)
        ((string= name "Leftdrag")
         (destructuring-bind (direction hunk where) *border-drag*
           (let ((moved (hi::layout-move-edge
                         (current-device) hunk direction
                         (- (if (eq direction :columns) column line) where))))
             (incf (third *border-drag*) moved)))
         t)
        ((string= name "Leftup")
         (setf *border-drag* nil)
         t)
        (t nil)))

(defun queue-mouse-event (name modifiers column line)
  (multiple-value-bind (x y hunk) (locate-cell column line)
    (hi::q-event hi::*real-editor-input*
                 (heml-ext:make-key-event name (modifier-bits modifiers))
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
           (heml-ext:make-key-event name (modifier-bits modifiers))))
    (hi::bind-key "Mouse Set Point" (key "Leftdown"))
    (hi::bind-key "Mouse Drag Region" (key "Leftup"))
    (hi::bind-key "Mouse Extend Region" (key "Leftdown" "Shift"))
    (hi::bind-key "Mouse Drag Region" (key "Leftup" "Shift"))
    (hi::bind-key "Mouse Select Word" (key "Doubleleftdown"))
    ;; In Dired, a double click opens what it is on.
    (hi::bind-key "Dired Mouse Edit File" (key "Doubleleftdown") :mode "Dired")
    (hi::bind-key "Bufed Mouse Goto" (key "Doubleleftdown") :mode "Bufed")
    (hi::bind-key "Mouse Select Line" (key "Tripleleftdown"))
    (hi::bind-key "Mouse Point Unless In Region" (key "Rightdown"))
    (hi::bind-key "Do Nothing" (key "Rightup"))
    (hi::bind-key "Menu Command" (key "Menucommand")))
  (setf heml::*active-region-highlight-font* '(:bg :selection)
        heml::*interprogram-cut-function*
        (lambda (text) (on-main-thread (write-pasteboard text)))
        heml::*interprogram-paste-function*
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
         (echo-height (hi::value heml::echo-area-height))
         (main-lines (- height echo-height 1))
         (main-text-lines (1- main-lines))
         (last-text-line (1- main-text-lines)))
    (setf hi::*window-list* ())
    (setf (hi::device-bottom-window-base device) last-text-line)
    (let* ((echo-hunk (make-instance 'cocoa-hunk :device device
                                                 :position (1- height) :height echo-height
                                                 :text-position (- height 2)
                                                 :text-height echo-height :width width))
           (echo (hi::internal-make-window :hunk echo-hunk)))
      (setf hi::*echo-area-window* echo)
      (setf (hi::device-hunk-window echo-hunk) echo)
      (hi::setup-window-image hi::*parse-starting-mark* echo echo-height width)
      (hi::setup-modeline-image hi::*echo-area-buffer* echo)
      (setf (hi::device-hunk-previous echo-hunk) echo-hunk
            (hi::device-hunk-next echo-hunk) echo-hunk))
    (let ((main-hunk (hi::device-make-hunk device)))
      (hi::init-layout device main-hunk 0 0 main-lines width)
      (let ((main (hi::internal-make-window :hunk main-hunk)))
        (setf (hi::device-hunk-window main-hunk) main)
        (setf hi::*current-window* main)
        (hi::setup-window-image (hi::buffer-point hi::*current-buffer*)
                                main (hi::device-hunk-text-height main-hunk) width)
        (hi::setup-modeline-image hi::*current-buffer* main)))
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

;;; A row is shared by the windows side by side on it.  A window's part of
;;; a row, from COLUMN for WIDTH cells, is replaced with TEXT, blank past
;;; its end, and RUNS, whose columns count from COLUMN; what the other
;;; windows put in the row is kept.
;;;
(defun store-segment (screen line column width text runs)
  (when (< -1 line (screen-lines screen))
    (let* ((row (svref (screen-rows screen) line))
           (end (+ column width))
           (new (make-string (max end (screen-columns screen)) :initial-element #\Space))
           (length (min width (length text))))
      (replace new (row-text row))
      (fill new #\Space :start column :end end)
      (replace new text :start1 column :end2 length)
      (setf (row-text row) new
            (row-runs row)
            (sort (append
                   ;; Other windows' runs, cut back to outside this part.
                   (loop for (start stop . font) in (row-runs row)
                         when (< start column)
                           collect (list* start (min stop column) font)
                         when (> stop end)
                           collect (list* (max start end) stop font))
                   (loop for (start stop . font) in runs
                         when (< start width)
                           collect (list* (+ column start) (+ column (min stop width)) font)))
                  #'< :key #'first)))))

(defun store-dis-line (screen line column width dis-line)
  (let ((length (min width (hi::dis-line-length dis-line))))
    (store-segment screen line column width
                   (subseq (hi::dis-line-chars dis-line) 0 length)
                   (font-runs dis-line length))))

(defun store-modeline (screen line column width dis-line)
  "A modeline fills its window's width: its last run, which carries the
modeline's colours, is carried to the edge."
  (let* ((length (min width (hi::dis-line-length dis-line)))
         (runs (font-runs dis-line length)))
    (when runs
      (setf (second (car (last runs))) width))
    (store-segment screen line column width
                   (subseq (hi::dis-line-chars dis-line) 0 length)
                   runs)))

(defun render-window (device window)
  "Copy all of WINDOW's image into the screen, and the bar beside it when
another window is on its right."
  (let* ((screen *screen*)
         (hunk (hi::window-hunk window))
         (top (hunk-top-line hunk))
         (left (hi::device-hunk-column hunk))
         (width (hi::device-hunk-width hunk))
         (height (hi::device-hunk-text-height hunk))
         (first (hi::window-first-line window)))
    (with-screen-lock (screen)
      (dotimes (i height)
        (store-segment screen (+ top i) left width "" '()))
      (do ((dl (cdr first) (cdr dl)))
          ((eq dl hi::the-sentinel))
        (let* ((dis-line (car dl))
               (position (hi::dis-line-position dis-line)))
          (when (< -1 position height)
            (store-dis-line screen (+ top position) left width dis-line))))
      (when (hi::window-modeline-buffer window)
        (store-modeline screen (+ top height) left width
                        (hi::window-modeline-dis-line window)))
      (when (< (+ left width) (screen-columns screen))
        (dotimes (i (hi::device-hunk-height hunk))
          (store-segment screen (+ top i) (+ left width) 1 "│" '()))))
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
        (x (+ (hi::device-hunk-column hunk) x))
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
    (setf (screen-borders *screen*) (layout-borders))
    ;; The mode the menus follow.  A prompt's echo area leaves them as they
    ;; were, rather than hiding a mode's menu while it asks.
    (let ((buffer (hi::current-buffer)))
      (unless (eq buffer hi::*echo-area-buffer*)
        (setf (screen-mode *screen*) (hi::buffer-major-mode buffer))))
    (present-screen *screen*)
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


;;;; Resizing

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
      (hi::resize-device-layout device lines columns))))


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
