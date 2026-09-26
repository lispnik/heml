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

(in-package :hemlock-internals)

#+(or)
(export '(redisplay redisplay-all define-tty-font))



;;;; Macros.

(defmacro tty-hunk-modeline-pos (hunk)
  `(tty-hunk-text-height ,hunk))


(defvar *currently-selected-hunk* nil)
(defvar *hunk-top-line*)

(declaim (fixnum *hunk-top-line*))

(defmacro select-hunk (hunk)
  `(unless (eq ,hunk *currently-selected-hunk*)
     (setf *currently-selected-hunk* ,hunk)
     (setf *hunk-top-line*
           (the fixnum
                (1+ (the fixnum
                         (- (the fixnum
                                 (tty-hunk-text-position ,hunk))
                            (the fixnum
                                 (tty-hunk-text-height ,hunk)))))))))



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
    (let ((delta (- lines (tty-device-lines device)))
          #+nil (cols (if hemlock.terminfo:auto-right-margin
                    (1- cols)
                    cols)))
      (unless (and (zerop delta)
                   #+nil (eql (tty-device-columns device) cols))
        (setf (tty-device-lines device) lines)
        #+nil (setf (tty-device-columns device) cols)
        (enlarge-device device delta)))))


;;;; Redisplay.

;;; There is no incremental redisplay.  Every row of the window is written
;;; each time, and cleared to the end of the line after its text, so the
;;; terminal shows exactly the window's image whatever it showed before.
;;; Nothing is cleared first, which is what keeps it from flickering.

(defun tty-write-dis-line (device hunk dis-line y)
  (let ((length (dis-line-length dis-line)))
    (funcall (tty-device-display-string device)
             hunk 0 y (dis-line-chars dis-line) (compute-font-usages dis-line)
             0 length)
    (when (< length (tty-device-columns device))
      (funcall (tty-device-clear-to-eol device) hunk length y))))

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
             (funcall (tty-device-clear-to-eol device) hunk 0 y))))
    (when (window-modeline-buffer window)
      (tty-write-dis-line device hunk (window-modeline-dis-line window)
                          (tty-hunk-modeline-pos hunk)))))



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
  (select-hunk hunk)
  (let ((y (the fixnum (+ *hunk-top-line* y)))
        (device (device-hunk-device hunk)))
    (declare (fixnum y))
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
   (hemlock.terminfo:tputs
    (hemlock.terminfo:tparm hemlock.terminfo:cursor-address y x))))

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
  (when hemlock.terminfo:set-a-foreground
    (tty-write-cmd
     (hemlock.terminfo:tputs
      (hemlock.terminfo:tparm hemlock.terminfo:set-a-foreground color)))))

(defun setab (color)
  (when hemlock.terminfo:set-a-background
    (tty-write-cmd
     (hemlock.terminfo:tputs
      (hemlock.terminfo:tparm hemlock.terminfo:set-a-background color)))))

(defun enter-bold-mode ()
  (when hemlock.terminfo:enter-bold-mode
    (tty-write-cmd
     (hemlock.terminfo:tputs hemlock.terminfo:enter-bold-mode))))

(defun enter-italics-mode ()
  (when hemlock.terminfo:enter-italics-mode
    (tty-write-cmd
     (hemlock.terminfo:tputs hemlock.terminfo:enter-italics-mode))))

(defun enter-underline-mode ()
  (when hemlock.terminfo:enter-underline-mode
    (tty-write-cmd
     (hemlock.terminfo:tputs hemlock.terminfo:enter-underline-mode))))

(defun exit-attribute-mode ()
  (tty-write-cmd
   (hemlock.terminfo:tputs hemlock.terminfo:exit-attribute-mode)))

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
                  (and hemlock.terminfo:set-a-foreground
                       hemlock.terminfo:set-a-background
                       hemlock.terminfo:exit-attribute-mode
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
                       (device-write-string string posn new-posn))
                   (exit-attribute-mode)))
                (t
                 (device-write-string string posn new-posn)))
          (setf posn new-posn))))
    (when (< posn end)
      (device-write-string string posn end)))
  (setf (tty-device-cursor-x (device-hunk-device hunk))
        (the fixnum (+ x (the fixnum (- end start))))))

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
                      (cursor-x (the fixnum (+ x (the fixnum
                                                      (- after-pos start))))))
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
          (the fixnum (+ x (the fixnum (- end start)))))))


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
         (num (- (the fixnum (tty-device-columns device))
                 x)))
    (declare (fixnum num))
    (dotimes (i num) (tty-write-char #\space))
    (setf (tty-device-cursor-x device) (+ x num))))


;;; Standout mode (TTY-DEVICE-STANDOUT-INIT and TTY-DEVICE-STANDOUT-END)

(defun standout-init (hunk)
  (tty-write-cmd
   (tty-device-standout-init-string (device-hunk-device hunk))))

(defun standout-end (hunk)
  (tty-write-cmd
   (tty-device-standout-end-string (device-hunk-device hunk))))
