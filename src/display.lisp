;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;; Written by Bill Chiles.
;;;
;;; This is the device independent redisplay entry points for Heml.
;;;

(in-package :heml-internals)

(declaim (special *in-the-editor*)) ; defined in main.lisp --amb

;;;; Main redisplay entry points.

(defvar *things-to-do-once* ()
  "This is a list of lists of functions and args to be applied to.  The
  functions are called with args supplied at the top of the command loop.")

(defvar *screen-image-trashed* ()
  "This variable is set to true if the screen has been trashed by some screen
   manager operation, and so should be cleared before it is drawn again.")

;;; The internal real time at which the screen was last drawn.
;;;
(defvar *last-redisplay-time* 0)

(defvar *redisplay-interval* 1/60
  "The shortest time, in seconds, between the redisplays that events such as
   output cause.  A flood of output is drawn at most this often, and a
   keystroke waits at most this long to be drawn.")

(defun time-since-redisplay ()
  "The seconds since the screen was last drawn."
  (/ (- (get-internal-real-time) *last-redisplay-time*)
     internal-time-units-per-second))

;;; WITH-DEVICE-REDISPLAY brackets one pass of redisplay on Device, so that
;;; the device can present it all at once, and notes when it was drawn.
;;;
(defmacro with-device-redisplay ((device) &body body)
  (let ((d (gensym "DEVICE")))
    `(let ((,d ,device))
       (device-begin-redisplay ,d)
       (unwind-protect (progn ,@body)
         (device-end-redisplay ,d)
         (setf *last-redisplay-time* (get-internal-real-time))))))

;;; True if we are in redisplay, and thus don't want to enter it recursively.
;;;
(defvar *in-redisplay* nil)

(declaim (special *window-list*))

;;; REDISPLAY-LOOP -- Internal.
;;;
;;; This executes internal redisplay routines on all windows interleaved with
;;; checking for input, and if any input shows up we punt returning
;;; :editor-input.  Special-fun is for windows that the redisplay interface
;;; wants to recenter to keep the window's buffer's point visible.  General-fun
;;; is for other windows.
;;;
;;; Every window is drawn in full, so one pass is enough: the internal
;;; routines return NIL, and we return T, meaning redisplay should run again,
;;; only when the cursor cannot be placed.
;;;
;;; After checking each window, we put the cursor in the appropriate place and
;;; force output.  When we try to position the cursor, it may no longer lie
;;; within the window due to buffer modifications during redisplay.  If it is
;;; out of the window, return t to indicate we need to finish redisplaying.
;;;
;;; Then we check for the after-redisplay method.  Routines such as REDISPLAY
;;; and REDISPLAY-ALL want to invoke the after method to make sure we handle
;;; any events generated from redisplaying.  There wouldn't be a problem with
;;; handling these events if we were going in and out of Heml's event
;;; handling, but some user may loop over one of these interface functions for
;;; a long time without going through Heml's input loop; when that happens,
;;; each call to redisplay may not result in a complete redisplay of the
;;; device.  Routines such as INTERNAL-REDISPLAY don't want to worry about this
;;; since Heml calls them while going in and out of the input/event-handling
;;; loop.
;;;
;;; Around all of this, we establish the 'redisplay-catcher tag.  Some device
;;; redisplay methods throw to this to abort redisplay in addition to this
;;; code.
;;;
(defun redisplay-loop (general-fun special-fun &optional (afterp t))
  (let ((n-res nil)
        (*in-redisplay* t))
    (catch 'redisplay-catcher
      (when (listen-editor-input *real-editor-input*)
        (throw 'redisplay-catcher :editor-input))
      (with-device-redisplay ((device-hunk-device (window-hunk *current-window*)))
        (let ((win *current-window*))
          (when (funcall special-fun win)
            (setf n-res t)))
        (dolist (win *window-list*)
          (unless (eq win *current-window*)
            (when (listen-editor-input *real-editor-input*)
              (throw 'redisplay-catcher :editor-input))
            (when (funcall (if (window-display-recentering win)
                               special-fun
                               general-fun)
                           win)
              (setf n-res t))))
        (let* ((hunk (window-hunk *current-window*))
               (device (device-hunk-device hunk))
               (point (window-point *current-window*)))
          (move-mark point (buffer-point (window-buffer *current-window*)))
          (multiple-value-bind (x y)
                               (mark-to-cursorpos point *current-window*)
            (if x
                (device-put-cursor device hunk x y)
                (setf n-res t)))
          (device-force-output device)
          (when afterp
            (device-after-redisplay device)
            ;; The after method may have queued input that the input
            ;; loop won't see until the next input arrives, so check
            ;; here to return the correct value as per the redisplay
            ;; contract.
            (when (listen-editor-input *real-editor-input*)
              (setf n-res :editor-input)))
          n-res)))))

;;; REDISPLAY -- Public.
;;;
;;; This function draws every window.  There is no incremental redisplay:
;;; each window's image is rebuilt from its buffer, and the device draws all
;;; of it.
;;;
(defun redisplay ()
  "The main entry into redisplay; draws every window."
  (when *things-to-do-once*
    (dolist (thing *things-to-do-once*) (apply (car thing) (cdr thing)))
    (setf *things-to-do-once* nil))
  (cond (*in-redisplay* t)
        (*screen-image-trashed* (redisplay-trashed-screen))
        (t
         (redisplay-loop #'redisplay-window #'redisplay-window-recentering))))


;;; REDISPLAY-ALL -- Public.
;;;
;;; Update the screen making no assumptions about its correctness.  This is
;;; useful if the screen gets trashed, or redisplay gets lost.  Since windows
;;; may be on different devices, we have to go through the list clearing all
;;; possible devices.  Returns what REDISPLAY-LOOP does.
;;;
(defun redisplay-all ()
  "An entry into redisplay; causes all windows to be fully refreshed."
  (let ((cleared-devices nil))
    (dolist (w *window-list*)
      (let* ((hunk (window-hunk w))
             (device (device-hunk-device hunk)))
        (unless (member device cleared-devices :test #'eq)
          (device-clear device)
          ;;
          ;; It's cleared whether we did clear it or there was no method.
          (push device cleared-devices)))))
  (redisplay-loop #'redisplay-window #'redisplay-window-recentering))



;;; The screen was cleared and every window drawn unless input cut it
;;; short, and only then must it be cleared again next time.  A true result
;;; that is not :EDITOR-INPUT means only that the cursor could not be
;;; placed.
;;;
(defun redisplay-trashed-screen ()
  (let ((result (redisplay-all)))
    (unless (eq result :editor-input)
      (setf *screen-image-trashed* nil))
    result))


;;;; Internal redisplay entry points.

(defun internal-redisplay ()
  "The main internal entry into redisplay.  This is just like REDISPLAY, but it
   doesn't call the device's after-redisplay method."
  (when *things-to-do-once*
    (dolist (thing *things-to-do-once*) (apply (car thing) (cdr thing)))
    (setf *things-to-do-once* nil))
  (cond (*in-redisplay*
         t)
        (*screen-image-trashed* (redisplay-trashed-screen))
        (t
         (redisplay-loop #'redisplay-window #'redisplay-window-recentering))))

;;; REDISPLAY-WINDOWS-FROM-MARK -- Internal Interface.
;;;
;;; heml-output-stream methods call this to update the screen.  It only
;;; redisplays windows which are displaying the buffer concerned and doesn't
;;; deal with making the cursor track the point.  This must call the device
;;; after-redisplay method since stream output may occur without ever
;;; returning to the Heml input/event-handling loop.
;;;
;;; When Throttlep, as it is for ordinary output, nothing is drawn if the
;;; screen was drawn less than *redisplay-interval* ago: the next output, or
;;; the input loop once it gets control back, draws it.  FINISH-OUTPUT and
;;; FORCE-OUTPUT draw at once.
;;;
(defun redisplay-windows-from-mark (mark &optional throttlep)
  (when *things-to-do-once*
    (dolist (thing *things-to-do-once*) (apply (car thing) (cdr thing)))
    (setf *things-to-do-once* nil))
  (cond ((or *in-redisplay* (not *in-the-editor*)) t)
        ((and throttlep (< (time-since-redisplay) *redisplay-interval*)) t)
        ((listen-editor-input *editor-input*) :editor-input)
        (*screen-image-trashed* (redisplay-trashed-screen))
        (t
         (let ((*in-redisplay* t))
           (catch 'redisplay-catcher
             (let ((buffer (line-buffer (mark-line mark))))
               (when buffer
                 (with-device-redisplay ((device-hunk-device
                                          (window-hunk *current-window*)))
                   (flet ((frob (win)
                            (let* ((device (device-hunk-device
                                            (window-hunk win))))
                              (device-force-output device)
                              (device-after-redisplay device))))
                     (let ((windows (buffer-windows buffer)))
                       (when (member *current-window* windows :test #'eq)
                         (redisplay-window-recentering *current-window*)
                         (frob *current-window*))
                       (dolist (window windows)
                         (unless (eq window *current-window*)
                           (redisplay-window window)
                           (frob window)))))))))))))

;;; REDISPLAY-WINDOW -- Internal.
;;;
;;; Rebuild the window's image and draw all of it.  Returns NIL: there is
;;; nothing left over for another pass.
;;;
(defun redisplay-window (window)
  "Rebuild the window's image and draw it.  NOTE: the device's redisplay
   method may throw to 'hi::redisplay-catcher to abort redisplay."
  (update-window-image window)
  (device-redisplay (device-hunk-device (window-hunk window)) window)
  nil)

(defun random-typeout-redisplay (window)
  (catch 'redisplay-catcher
    (let ((device (device-hunk-device (window-hunk window))))
      (with-device-redisplay (device)
        (update-window-image window)
        (device-redisplay device window)
        (device-force-output device)))))


;;;; Support for redisplay entry points.

;;; REDISPLAY-WINDOW-RECENTERING -- Internal.
;;;
;;; This recenters the window if its buffer's point moved off it, runs the
;;; redisplay hook -- whose functions may add font marks, such as the
;;; highlighting of an open paren -- and draws the window.  NOTE: the
;;; device's redisplay method may throw to 'hi::redisplay-catcher to abort
;;; redisplay.
;;;
(defun redisplay-window-recentering (window)
  (setup-for-recentering-redisplay window)
  (invoke-hook heml::redisplay-hook window)
  (setup-for-recentering-redisplay window)
  (device-redisplay (device-hunk-device (window-hunk window)) window)
  nil)

(defun setup-for-recentering-redisplay (window)
  (let* ((display-start (window-display-start window))
         (old-start (window-old-start window)))
    ;;
    ;; If the start is in the middle of a line and it wasn't before,
    ;; then move the start there.
    (when (and (same-line-p display-start old-start)
               (not (start-line-p display-start))
               (start-line-p old-start))
      (line-start display-start))
    (update-window-image window)
    (maybe-recenter-window window)))
