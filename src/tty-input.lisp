;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :heml-internals)

;;;; Editor input from a tty.

(defclass tty-editor-input (editor-input)
  ((fd :initarg :fd
       :accessor tty-editor-input-fd)))

(defun make-tty-editor-input (&rest args)
  (apply #'make-instance 'tty-editor-input args))

(defmethod get-key-event
    ((stream tty-editor-input) &optional ignore-abort-attempts-p)
  (%editor-input-method stream ignore-abort-attempts-p))

(defmethod unget-key-event (key-event (stream tty-editor-input))
  (un-event key-event stream))

(defmethod clear-editor-input ((stream tty-editor-input))
  (heml-ext:without-interrupts
   (let* ((head (editor-input-head stream))
          (next (input-event-next head)))
     (when next
       (setf (input-event-next head) nil)
       (shiftf (input-event-next (editor-input-tail stream))
               *free-input-events* next)
       (setf (editor-input-tail stream) head)))))

;;; Note that we never return NIL as long as there are events to be served with
;;; SERVE-EVENT.  Thus non-keyboard input (i.e. process output)
;;; effectively causes LISTEN to block until either all the non-keyboard input
;;; has happened, or there is some real keyboard input.
;;;
(defmethod listen-editor-input ((stream tty-editor-input))
  (process-editor-tty-input)
  ;; Whether a key is waiting.  It said NIL always, so a key that arrived
  ;; while the screen was drawn (heml:repl's redisplay dispatches events) sat
  ;; in the queue while the input loop waited on the terminal for another.
  (not (null (input-event-next (editor-input-head stream)))))

(defvar *tty-translations* (make-hash-table :test #'equal))

(defun register-tty-translations ()
  (assert heml.terminfo:*terminfo*)
  (flet ((reg (string keysym)
           (let ((string (etypecase string
                           (character (string string))
                           (list (coerce string 'simple-string))
                           (string string))))
              (setf (gethash string *tty-translations*) keysym))))
    ;; KLUDGE: There seems to be no way get F1-F4 reliably transmit
    ;; things in the terminfo db, since some terminals transmit them
    ;; as if they were vt100 PF1-PF4, so, register these aliases here.
    ;; If they double as something else, that will override these.
    (reg '(#\Esc #\O #\P) #k"F1")
    (reg '(#\Esc #\O #\Q) #k"F2")
    (reg '(#\Esc #\O #\R) #k"F3")
    (reg '(#\Esc #\O #\S) #k"F4")
    ;; Terminfo definitions for F1-F12
    (reg heml.terminfo:key-f1 #k"F1")
    (reg heml.terminfo:key-f2 #k"F2")
    (reg heml.terminfo:key-f3 #k"F3")
    (reg heml.terminfo:key-f4 #k"F4")
    (reg heml.terminfo:key-f5 #k"F5")
    (reg heml.terminfo:key-f6 #k"F6")
    (reg heml.terminfo:key-f7 #k"F7")
    (reg heml.terminfo:key-f8 #k"F8")
    (reg heml.terminfo:key-f9 #k"F9")
    (reg heml.terminfo:key-f10 #k"F10")
    (reg heml.terminfo:key-f11 #k"F11")
    (reg heml.terminfo:key-f12 #k"F12")
    ;; Terminfo definitions for movement keys
    (reg heml.terminfo:key-up #k"Uparrow")
    (reg heml.terminfo:key-down #k"Downarrow")
    (reg heml.terminfo:key-right #k"Rightarrow")
    (reg heml.terminfo:key-left #k"Leftarrow")
    (reg heml.terminfo:key-home #k"Home")
    (reg heml.terminfo:key-end #k"End")
    (reg heml.terminfo:key-ic #k"Insert")
    (reg heml.terminfo:key-dc #k"Delete")
    (reg heml.terminfo:key-ppage #k"Pageup")
    (reg heml.terminfo:key-npage #k"Pagedown")
    (reg heml.terminfo:key-backspace #k"Backspace")

    (reg heml.terminfo:key-sr #k"Shift-Uparrow")
    (reg heml.terminfo:key-sf #k"Shift-Downarrow")
    (reg heml.terminfo:key-sright #k"Shift-Rightarrow")
    (reg heml.terminfo:key-sleft #k"Shift-Leftarrow")
    (reg heml.terminfo:key-shome #k"Shift-Home")
    (reg heml.terminfo:key-send #k"Shift-End")
    (reg heml.terminfo:key-sic #k"Shift-Insert")
    (reg heml.terminfo:key-sdc #k"Shift-Delete")
    (reg heml.terminfo:key-sprevious #k"Shift-Pageup")
    (reg heml.terminfo:key-snext #k"Shift-Pagedown")
    
    ;; Xterm definitions, not in terminfo.

    (reg "[1;5A" #k"Control-Uparrow")
    (reg "[1;5B" #k"Control-Downarrow")
    (reg "[1;5C" #k"Control-Rightarrow")
    (reg "[1;5D" #k"Control-Leftarrow")
    (reg "[1;5H" #k"Control-Home")
    (reg "[1;5F" #k"Control-End")
    (reg "[2;3~" #k"Control-Insert")
    (reg "[3;5~" #k"Control-Delete")
    (reg "[5;3~" #k"Control-Pageup")
    (reg "[6;3~" #k"Control-Pagedown")

    (reg "[1;3A" #k"Meta-Uparrow")
    (reg "[1;3B" #k"Meta-Downarrow")
    (reg "[1;3C" #k"Meta-Rightarrow")
    (reg "[1;3D" #k"Meta-Leftarrow")
    (reg "[1;3H" #k"Meta-Home")
    (reg "[1;3F" #k"Meta-End")
    (reg "[2;3~" #k"Meta-Insert")
    (reg "[3;3~" #k"Meta-Delete")
    (reg "[5;3~" #k"Meta-Pageup")
    (reg "[6;3~" #k"Meta-Pagedown")
    
    (reg "[1;6A" #k"Shift-Control-Uparrow")
    (reg "[1;6B" #k"Shift-Control-Downarrow")
    (reg "[1;6C" #k"Shift-Control-Rightarrow")
    (reg "[1;6D" #k"Shift-Control-Leftarrow")
    (reg "[1;6H" #k"Shift-Control-Home")
    (reg "[1;6F" #k"Shift-Control-End")
    (reg "[2;6~" #k"Shift-Control-Insert")
    (reg "[3;6~" #k"Shift-Control-Delete")
    (reg "[5;6~" #k"Shift-Control-PageUp")
    (reg "[6;6~" #k"Shift-Control-PageDown")
    (reg "[3;6~" #k"Shift-Control-Delete")

    (reg "[1;4A" #k"Shift-Meta-Uparrow")
    (reg "[1;4B" #k"Shift-Meta-Downarrow")
    (reg "[1;4C" #k"Shift-Meta-Rightarrow")
    (reg "[1;4D" #k"Shift-Meta-Leftarrow")
    (reg "[1;4H" #k"Shift-Meta-Home")
    (reg "[1;4F" #k"Shift-Meta-End")
    (reg "[2;4~" #k"Shift-Meta-Insert")
    (reg "[3;4~" #k"Shift-Meta-Delete")
    (reg "[5;4~" #k"Shift-Meta-PageUp")
    (reg "[6;4~" #k"Shift-Meta-PageDown")

    (reg "[1;7A" #k"Meta-Control-Uparrow")
    (reg "[1;7B" #k"Meta-Control-Downarrow")
    (reg "[1;7C" #k"Meta-Control-Rightarrow")
    (reg "[1;7D" #k"Meta-Control-Leftarrow")
    (reg "[1;7H" #k"Meta-Control-Home")
    (reg "[1;7F" #k"Meta-Control-End")
    (reg "[2;7~" #k"Meta-Control-Insert")
    (reg "[3;7~" #k"Meta-Control-Delete")
    (reg "[5;7~" #k"Meta-Control-PageUp")
    (reg "[6;7~" #k"Meta-Control-PageDown")

    (reg "[1;8A" #k"Shift-Meta-Control-Uparrow")
    (reg "[1;8B" #k"Shift-Meta-Control-Downarrow")
    (reg "[1;8C" #k"Shift-Meta-Control-Rightarrow")
    (reg "[1;8D" #k"Shift-Meta-Control-Leftarrow")
    (reg "[1;8H" #k"Shift-Meta-Control-Home")
    (reg "[1;8F" #k"Shift-Meta-Control-End")
    (reg "[2;8~" #k"Shift-Meta-Control-Insert")
    (reg "[3;8~" #k"Shift-Meta-Control-Delete")
    (reg "[5;8~" #k"Shift-Meta-Control-PageUp")
    (reg "[6;8~" #k"Shift-Meta-Control-PageDown")

    ;; Misc.
    ;;
    ;; Not #\return, because then C-j turns into return aka C-m.
    ;; Is this translation needed at all?
    (reg #\newline #k"Linefeed")
    ;;
    (reg #\tab #k"Tab")
    (reg #\escape #k"Escape")
    ;; Kludge: This shouldn't be needed, but otherwise C-c M-i doesn't work.
    (reg '(#\Esc #\i) #k"meta-i")))

;;; The terminal's erase character is Backspace, whatever terminfo says:
;;; kbs is ^H in most entries, but a Mac's terminals, and tmux, send the
;;; ^? that stty calls erase, which Heml would otherwise take for Delete.
;;;
(defun translate-tty-event (data)
  (let ((string (coerce data 'string)))
    (or (and *tty-erase-char*
             (= 1 (length string))
             (= (char-code (char string 0)) *tty-erase-char*)
             #k"Backspace")
        (gethash string *tty-translations*)
        (when (= 1 (length string))
          (heml-ext:character-key-event (char string 0))))))

(defun tty-key-event (data)
  (loop with start = 0
        with length = (length data)
        while (< start length)
        do (loop for end from length downto (1+ start)
                 do (let ((event (translate-tty-event (subseq data start end))))
                      (when event
                        (q-event *real-editor-input* event)
                        (setf start end)
                        (return))))))

