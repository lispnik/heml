;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;; Written by Bill Chiles, except for the code that implements random typeout,
;;; which was done by Blaine Burks and Bill Chiles.  The code for splitting
;;; windows was rewritten by Blaine Burks to allow more than a 50/50 split.
;;;
;;; Terminal device screen management functions.
;;;

(in-package :heml-internals)



;;;; Terminal screen initialization

(declaim (special *parse-starting-mark*))

(defvar *do-not-finalize*)
(defvar *tty-connection*)

(defun init-tty-screen-manager (device)
  (setf *line-wrap-char* #\!)
  (setf *window-list* ())
  (setf *tty-connection*
        (let* ((stream (open "/dev/tty" :direction :io :if-exists :overwrite))
               (fd (stream-fd stream)))
          (setf *do-not-finalize* stream)
          (make-pipelike-connection
           fd
           fd
           :name "tty"
           :buffer nil
           :filter (lambda (connection bytes)
                     (tty-key-event
                      (default-filter connection bytes))
                     nil))))
  (let* ((width (tty-device-columns device))
         (height (tty-device-lines device))
         (echo-height (value heml::echo-area-height))
         (main-lines (- height echo-height 1)) ;-1 for echo modeline.
         (main-text-lines (1- main-lines)) ;also main-modeline-pos.
         (last-text-line (1- main-text-lines)))
    (setf (device-bottom-window-base device) last-text-line)
    ;;
    ;; Make echo area.
    (let* ((echo-hunk (make-tty-hunk :position (1- height) :height echo-height
                                     :text-position (- height 2)
                                     :text-height echo-height :device device
                                     :width width))
           (echo (internal-make-window :hunk echo-hunk)))
      (setf *echo-area-window* echo)
      (setf (device-hunk-window echo-hunk) echo)
      (setup-window-image *parse-starting-mark* echo echo-height width)
      (setup-modeline-image *echo-area-buffer* echo)
      (setf (device-hunk-previous echo-hunk) echo-hunk
            (device-hunk-next echo-hunk) echo-hunk))
    ;;
    ;; Make the main window, the whole of the layout.
    (let ((main-hunk (make-tty-hunk :device device)))
      (init-layout device main-hunk 0 0 main-lines width)
      (let ((main (internal-make-window :hunk main-hunk)))
        (setf (device-hunk-window main-hunk) main)
        (setf *current-window* main)
        (setup-window-image (buffer-point *current-buffer*)
                            main (device-hunk-text-height main-hunk) width)
        (setup-modeline-image *current-buffer* main)))
    (defhvar "Paren Pause Period"
      "This is how long commands that deal with \"brackets\" shows the cursor at
      the matching \"bracket\" for this number of seconds."
      :value 0.5
      :mode "Lisp")))



;;;; Building devices from termcaps.

;;; MAKE-TTY-DEVICE returns a device built from a termcap.  Some function
;;; slots are set to the appropriate function even though the capability
;;; might not exist; in this case, we simply set the control string value
;;; to the empty string.  Some function slots are set differently depending
;;; on available capability.
;;;
(defun make-tty-device (name)
  (heml.terminfo:set-terminal)
  (register-tty-translations)
  (let ((device (%make-tty-device :name name)))
    (when (termcap :overstrikes)
      (error "Terminal sufficiently irritating -- not currently supported."))
    ;;
    ;; Get size and speed.
    (multiple-value-bind  (lines cols speed)
                          (get-terminal-attributes)
      (setf (tty-device-lines device) (or lines (termcap :lines)))
      (let ((cols (or cols (termcap :columns))))
        (setf (tty-device-columns device)
              (if heml.terminfo:auto-right-margin (1- cols) cols)))
      (setf (tty-device-speed device) speed))
    ;;
    ;; Some function slots.
    (setf (tty-device-display-string device)
          (if (termcap :underlines)
              #'display-string-checking-underlines
              #'display-string))
    (setf (tty-device-standout-init device) #'standout-init)
    (setf (tty-device-standout-end device) #'standout-end)
    (setf (tty-device-clear-to-eol device)
          (if (termcap :clear-to-eol)
              #'clear-to-eol
              #'space-to-eol))
    ;;
    ;; Some string slots.
    (setf (tty-device-standout-init-string device)
          (or (heml.terminfo:tputs (termcap :init-standout-mode)) ""))
    (setf (tty-device-standout-end-string device)
          (or (heml.terminfo:tputs (termcap :end-standout-mode)) ""))
    (setf (tty-device-clear-to-eol-string device)
          (heml.terminfo:tputs (termcap :clear-to-eol)))
    (let ((clear-string (termcap :clear-display)))
      (unless clear-string
        (error "Terminal not sufficiently powerful enough to run Heml."))
      (setf (tty-device-clear-string device) (heml.terminfo:tputs clear-string)))
    (let* ((init-string (termcap :init-string))
           (init-file (termcap :init-file))
           (init-file-string (if init-file (get-init-file-string init-file)))
           (init-cm-string (termcap :init-cursor-motion)))
      (setf (tty-device-init-string device)
            (heml.terminfo:tputs (concatenate 'simple-string
                                (or init-string "")
                                (or init-file-string "")
                                (or init-cm-string "")
                                ;; Transmit-mode: this makes arrow-keys give sequences matching
                                ;; the terminfo db.
                                heml.terminfo:keypad-xmit))))
    (setf (tty-device-cm-end-string device)
          (heml.terminfo:tputs
           (concatenate 'simple-string
                        (or (termcap :end-cursor-motion) "")
                        ;; Exit transmit-mode.
                        heml.terminfo:keypad-local)))
    device))


;;;; from rompsite.lisp

(defmethod device-show-mark ((device tty-device) window x y time)
  (declare (ignore time))
  (cond ((listen-editor-input *editor-input*))
        (x (internal-redisplay)
           (let* ((hunk (window-hunk window))
                  (device (device-hunk-device hunk)))
             (device-put-cursor device hunk x y)
             (device-force-output device)
             ;; the original code had a delay for TIME here
             )
           t)
        (t nil)))


