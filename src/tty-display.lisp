;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;;    Written by Bill Chiles.
;;;

(in-package :heml-internals)

#+(or)
(export '(redisplay redisplay-all define-tty-font))



;;;; Macros.

(defmacro tty-hunk-modeline-pos (hunk)
  `(device-hunk-text-height ,hunk))

;;; The screen line of HUNK's first line of text.
;;;
(defun hunk-top-line (hunk)
  (1+ (- (device-hunk-text-position hunk) (device-hunk-text-height hunk))))

;;; Whether HUNK reaches the right edge of the screen, rather than having a
;;; window beside it.
;;;
(defun rightmost-hunk-p (device hunk)
  (>= (+ (device-hunk-column hunk) (device-hunk-width hunk))
      (tty-device-columns device)))



;;; Font support.

(defun compute-font-usages (dis-line)
  (do ((results nil)
       (change (dis-line-font-changes dis-line) (font-change-next change))
       (prev nil change))
      ((null change)
       (when prev
         (let ((font (font-change-font prev)))
           (when (and font (not (eql font 0)))
             (push (list* (font-change-font prev)
                          (font-change-x prev)
                          (dis-line-length dis-line))
                   results))))
       (nreverse results))
    (when prev
      (let ((font (font-change-font prev)))
        (when (and font (not (eql font 0)))
          (push (list* (font-change-font prev)
                       (font-change-x prev)
                       (font-change-x change))
                results))))))


;;;; Terminal size.

(defun maybe-resize-tty-device (device)
  (multiple-value-bind (lines cols)
      (hi::get-terminal-attributes)
    (let ((cols (if heml.terminfo:auto-right-margin (1- cols) cols)))
      (unless (and (eql lines (tty-device-lines device))
                   (eql cols (tty-device-columns device)))
        (setf (tty-device-lines device) lines
              (tty-device-columns device) cols)
        (resize-device-layout device lines cols)))))


;;;; Redisplay.

;;; There is no incremental redisplay.  Every row of the window is written
;;; each time, and cleared to the end of the line after its text, so the
;;; terminal shows exactly the window's image whatever it showed before.
;;; Nothing is cleared first, which is what keeps it from flickering.

;;; The rest of a row of HUNK, from X, made blank.  Clearing to the end of
;;; the line would clear a window beside it too, so that is only for a hunk
;;; at the right edge; others are written over with spaces.
;;;
(defun tty-blank-to-edge (device hunk x y)
  (let ((width (device-hunk-width hunk)))
    (when (< x width)
      (if (rightmost-hunk-p device hunk)
          (funcall (tty-device-clear-to-eol device) hunk x y)
          (let ((blanks (- width x)))
            (update-cursor hunk x y)
            (device-write-string (make-string blanks :initial-element #\Space))
            (incf (tty-device-cursor-x device) blanks))))))

(defun tty-write-dis-line (device hunk dis-line y)
  (let ((length (min (dis-line-length dis-line) (device-hunk-width hunk))))
    (funcall (tty-device-display-string device)
             hunk 0 y (dis-line-chars dis-line) (compute-font-usages dis-line)
             0 length)
    (tty-blank-to-edge device hunk length y)))

;;; The column after a hunk that has a window beside it belongs to neither,
;;; and has a bar down the hunk's lines, its modeline's included.
;;;
(defun tty-write-separator (device hunk)
  (unless (rightmost-hunk-p device hunk)
    (let ((x (device-hunk-width hunk)))
      (dotimes (y (device-hunk-height hunk))
        (update-cursor hunk x y)
        (device-write-string "|")
        (incf (tty-device-cursor-x device))))))

;;; Each pass of redisplay is bracketed so that the terminal shows it at
;;; once: synchronized output (DEC private mode 2026) holds the old frame
;;; until the end, on the terminals that have it, and others ignore the
;;; request.  The cursor is hidden meanwhile, so that it does not show on
;;; every row as they are written.  Where hiding it is DECTCEM, as it almost
;;; always is, it is shown again with DECTCEM alone: terminfo's cnorm often
;;; does more, such as xterm's turning off a blinking cursor, which would
;;; happen on every redisplay.
;;;
(defvar *tty-synchronized-output* t
  "When true, each redisplay asks the terminal to show it all at once.")

(defparameter +begin-synchronized-update+
  (format nil "~C[?2026h" (code-char 27)))

(defparameter +end-synchronized-update+
  (format nil "~C[?2026l" (code-char 27)))

(defparameter +hide-cursor+ (format nil "~C[?25l" (code-char 27)))
(defparameter +show-cursor+ (format nil "~C[?25h" (code-char 27)))

(defun show-cursor-string ()
  (let ((civis heml.terminfo:cursor-invisible))
    (cond ((null civis) nil)
          ((equal civis +hide-cursor+) +show-cursor+)
          (t (heml.terminfo:tputs heml.terminfo:cursor-normal)))))

(defmethod device-begin-redisplay ((device tty-device))
  (when *tty-synchronized-output*
    (tty-write-cmd +begin-synchronized-update+))
  (when heml.terminfo:cursor-invisible
    (tty-write-cmd (heml.terminfo:tputs heml.terminfo:cursor-invisible))))

(defmethod device-end-redisplay ((device tty-device))
  (let ((show (show-cursor-string)))
    (when show (tty-write-cmd show)))
  (when *tty-synchronized-output*
    (tty-write-cmd +end-synchronized-update+))
  (device-force-output device))

(defmethod device-redisplay ((device tty-device) window)
  (maybe-resize-tty-device device)
  (let ((hunk (window-hunk window))
        (dl (cdr (window-first-line window))))
    (dotimes (y (window-height window))
      (cond ((and (not (eq dl the-sentinel))
                  (= (dis-line-position (car dl)) y))
             (tty-write-dis-line device hunk (car dl) y)
             (setf dl (cdr dl)))
            (t
             (tty-blank-to-edge device hunk 0 y))))
    (when (window-modeline-buffer window)
      (tty-write-dis-line device hunk (window-modeline-dis-line window)
                          (tty-hunk-modeline-pos hunk)))
    (tty-write-separator device hunk)))



;;;; Device methods

;;; Initializing and exiting the device (DEVICE-INIT and DEVICE-EXIT functions).
;;; These can be found in Tty-Display-Rt.Lisp.


;;; Clearing the device (DEVICE-CLEAR functions).

(defmethod device-clear ((device tty-device))
  (tty-write-cmd (tty-device-clear-string device))
  (cursor-motion device 0 0)
  (setf (tty-device-cursor-x device) 0)
  (setf (tty-device-cursor-y device) 0))


;;; Moving the cursor around (DEVICE-PUT-CURSOR)

;;; TTY-PUT-CURSOR makes sure the coordinates are mapped from the hunk's
;;; axis to the screen's and determines the minimal cost cursor motion
;;; sequence.  Currently, it does no cost analysis of relative motion
;;; compared to absolute motion but simply makes sure the cursor isn't
;;; already where we want it.
;;;
(defmethod device-put-cursor ((device tty-device) hunk x y)
  (declare (fixnum x y))
  (let ((x (+ (device-hunk-column hunk) x))
        (y (+ (hunk-top-line hunk) y))
        (device (device-hunk-device hunk)))
    (declare (fixnum x y))
    (unless (and (= (the fixnum (tty-device-cursor-x device)) x)
                 (= (the fixnum (tty-device-cursor-y device)) y))
      (cursor-motion device x y)
      (setf (tty-device-cursor-x device) x)
      (setf (tty-device-cursor-y device) y))))

;;; UPDATE-CURSOR is used in device redisplay methods to make sure the
;;; cursor is where it should be.
;;;
(defun update-cursor (hunk x y)
  (device-put-cursor (device-hunk-device hunk) hunk x y))

;;; CURSOR-MOTION takes two coordinates on the screen's axis,
;;; moving the cursor to that location.  X is the column index,
;;; and y is the line index, but Unix and Termcap believe that
;;; the default order of indexes is first the line and then the
;;; column or (y,x).  Because of this, when reversep is non-nil,
;;; we send first x and then y.
;;;
(defun cursor-motion (device x y)
  (tty-write-cmd
   (heml.terminfo:tputs
    (heml.terminfo:tparm heml.terminfo:cursor-address y x))))

;;; CM-OUTPUT-COORDINATE outputs the coordinate with respect to the pad.  If
;;; there is a pad, then the coordinate needs to be sent as digit-char's (for
;;; each digit in the coordinate), and if there is no pad, the coordinate needs
;;; to be converted into a character.  Using CODE-CHAR here is not really
;;; portable.  With a pad, the coordinate buffer is filled from the end as we
;;; truncate the coordinate by 10, generating ones digits.
;;;
(defconstant cm-coordinate-buffer-len 5)
(defvar *cm-coordinate-buffer* (make-string cm-coordinate-buffer-len))
;;;
(defun cm-output-coordinate (coordinate pad)
  (cond (pad
         (let ((i (1- cm-coordinate-buffer-len)))
           (loop
             (when (= i -1) (error "Terminal has too many lines!"))
             (multiple-value-bind (tens ones)
                                  (truncate coordinate 10)
               (setf (schar *cm-coordinate-buffer* i) (digit-char ones))
               (when (zerop tens)
                 (dotimes (n (- pad (- cm-coordinate-buffer-len i)))
                   (decf i)
                   (setf (schar *cm-coordinate-buffer* i) #\0))
                 (device-write-string *cm-coordinate-buffer* i
                                      cm-coordinate-buffer-len)
                 (return))
               (decf i)
               (setf coordinate tens)))))
        (t (tty-write-char (code-char coordinate)))))


;;; Writing strings (TTY-DEVICE-DISPLAY-STRING functions)

;;; Font attribute support: color, bold.

(defun setaf (color)
  (when heml.terminfo:set-a-foreground
    (tty-write-cmd
     (heml.terminfo:tputs
      (heml.terminfo:tparm heml.terminfo:set-a-foreground color)))))

(defun setab (color)
  (when heml.terminfo:set-a-background
    (tty-write-cmd
     (heml.terminfo:tputs
      (heml.terminfo:tparm heml.terminfo:set-a-background color)))))

(defun enter-bold-mode ()
  (when heml.terminfo:enter-bold-mode
    (tty-write-cmd
     (heml.terminfo:tputs heml.terminfo:enter-bold-mode))))

(defun enter-italics-mode ()
  (when heml.terminfo:enter-italics-mode
    (tty-write-cmd
     (heml.terminfo:tputs heml.terminfo:enter-italics-mode))))

(defun enter-underline-mode ()
  (when heml.terminfo:enter-underline-mode
    (tty-write-cmd
     (heml.terminfo:tputs heml.terminfo:enter-underline-mode))))

(defun exit-attribute-mode ()
  (tty-write-cmd
   (heml.terminfo:tputs heml.terminfo:exit-attribute-mode)))

(defvar *terminal-has-colors* :unknown)



;;; DISPLAY-STRING is used to put a string at (x,y) on the device.
;;;
(defun display-string (hunk x y string font-info
                            &optional (start 0) (end (strlen string)))
  (declare (fixnum x y start end))
  (update-cursor hunk x y)
  ;; Ignore font info for chars before the start of the string.
  (loop
    (if (or (null font-info)
            (< start (cddar font-info)))
        (return)
        (pop font-info)))
  (let ((posn start))
    (dolist (next-font font-info)
      (let ((font (car next-font))
            (start (cadr next-font))
            (stop (cddr next-font)))
        (when (<= end start)
          (return))
        (when (< posn start)
          (device-write-string string posn start)
          (setf posn start))
        (let ((new-posn (min stop end)))
          (when (eq *terminal-has-colors* :unknown)
            (setf *terminal-has-colors*
                  (and heml.terminfo:set-a-foreground
                       heml.terminfo:set-a-background
                       heml.terminfo:exit-attribute-mode
                       t)))
          (cond (*terminal-has-colors*
                 (unwind-protect
                     (progn
                       (let ((foreground (cond ((integerp font)
                                                font)
                                               ((listp font)
                                                (getf font :fg)))))
                         (when (and foreground (<= 0 foreground 9))
                           (setaf foreground)))
                       (let ((background (and (listp font) (getf font :bg))))
                         (when (and background (<= 0 background 9))
                           (setab background)))
                       (let ((boldp (and (listp font) (getf font :bold))))
                         (when boldp
                           (enter-bold-mode)))
                       (let ((italicp (and (listp font) (getf font :italic))))
                         (when italicp
                           (enter-italics-mode)))
                       (let ((underlinep (and (listp font) (getf font :underline))))
                         (when underlinep
                           (enter-underline-mode)))
                       ;; A link is an OSC 8 hyperlink, which terminals that
                       ;; know it make clickable, and the rest ignore.
                       (let ((link (and (listp font) (getf font :link))))
                         (when link
                           (tty-write-cmd (format nil "~C]8;;~A~C\\" #\Esc link #\Esc)))
                         (device-write-string string posn new-posn)
                         (when link
                           (tty-write-cmd (format nil "~C]8;;~C\\" #\Esc #\Esc)))))
                   (exit-attribute-mode)))
                (t
                 (device-write-string string posn new-posn)))
          (setf posn new-posn))))
    (when (< posn end)
      (device-write-string string posn end)))
  (setf (tty-device-cursor-x (device-hunk-device hunk))
        (+ (device-hunk-column hunk) x (- end start))))

;;; DISPLAY-STRING-CHECKING-UNDERLINES is used for terminals that special
;;; case underlines doing an overstrike when they don't otherwise overstrike.
;;; Note: we do not know in this code whether the terminal can backspace (or
;;; what the sequence is), whether the terminal has insert-mode, or whether
;;; the terminal has delete-mode.
;;;
(defun display-string-checking-underlines (hunk x y string font-info
                                                &optional (start 0)
                                                          (end (strlen string)))
  (declare (ignore font-info))
  (declare (fixnum x y start end) (simple-string string))
  (update-cursor hunk x y)
  (let ((upos (position #\_ string :test #'char= :start start :end end))
        (device (device-hunk-device hunk)))
    (if upos
        (let ((previous start)
              (after-pos 0))
          (declare (fixnum previous after-pos))
          (loop (device-write-string string previous upos)
                (setf after-pos (do ((i (1+ upos) (1+ i)))
                                    ((or (= i end)
                                         (char/= (schar string i) #\_)) i)
                                  (declare (fixnum i))))
                (let ((ulen (the fixnum (- after-pos upos)))
                      (cursor-x (+ (device-hunk-column hunk) x
                                   (- after-pos start))))
                  (declare (fixnum ulen))
                  (dotimes (i ulen) (tty-write-char #\space))
                  (setf (tty-device-cursor-x device) cursor-x)
                  (update-cursor hunk upos y)
                  (dotimes (i ulen) (tty-write-char #\_))
                  (setf (tty-device-cursor-x device) cursor-x))
                (setf previous after-pos)
                (setf upos (position #\_ string :test #'char=
                                     :start previous :end end))
                (unless upos
                  (device-write-string string previous end)
                  (return))))
        (device-write-string string start end))
    (setf (tty-device-cursor-x device)
          (+ (device-hunk-column hunk) x (- end start)))))


;;; DEVICE-WRITE-STRING is used to shove a string at the terminal regardless
;;; of cursor position.
;;;
;;; A wide character's filler is not written: the terminal has already
;;; moved two columns for the character itself.
;;;
(defun device-write-string (string &optional (start 0) (end (strlen string)))
  (declare (fixnum start end))
  (loop while (< start end)
        do (let ((stop (or (position wide-character-filler string
                                     :start start :end end)
                           end)))
             (unless (= start stop)
               (tty-write-string string start (the fixnum (- stop start))))
             (setf start (1+ stop)))))


;;; Clearing to the end of a line (TTY-DEVICE-CLEAR-TO-EOL functions).

(defun clear-to-eol (hunk x y)
  (update-cursor hunk x y)
  (tty-write-cmd
   (tty-device-clear-to-eol-string (device-hunk-device hunk))))

(defun space-to-eol (hunk x y)
  (declare (fixnum x))
  (update-cursor hunk x y)
  (let* ((device (device-hunk-device hunk))
         (x (+ (device-hunk-column hunk) x))
         (num (- (tty-device-columns device) x)))
    (declare (fixnum x num))
    (dotimes (i num) (tty-write-char #\space))
    (setf (tty-device-cursor-x device) (+ x num))))


;;; Standout mode (TTY-DEVICE-STANDOUT-INIT and TTY-DEVICE-STANDOUT-END)

(defun standout-init (hunk)
  (tty-write-cmd
   (tty-device-standout-init-string (device-hunk-device hunk))))

(defun standout-end (hunk)
  (tty-write-cmd
   (tty-device-standout-end-string (device-hunk-device hunk))))
