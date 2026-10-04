;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; A client for debug adapters (the Debug Adapter Protocol): lldb-dap for
;;; C and Rust, debugpy for Python, Delve for Go, and whatever
;;; DEFINE-DEBUG-ADAPTER names for a mode.
;;;
;;; An adapter is a program that runs the program being debugged and
;;; answers for it: Heml talks to it as it talks to a language server
;;; (lsp.lisp), with JSON messages after a Content-Length header, over the
;;; adapter's standard input and output or, for one that listens instead
;;; (Delve), a socket it says it is listening on.  One program is debugged
;;; at a time.
;;;
;;; Breakpoints are kept at marks, a line each, and drawn as a red dot
;;; before the line (*LINE-INLINE-FUNCTIONS*); where the program stopped is
;;; drawn as an arrow.  The buffer "Debugger" shows the stopped thread's
;;; frames and the selected frame's variables: Return on a frame selects it,
;;; and on a variable with parts shows them.  What the program prints is in
;;; the buffer "Debug Output".
;;;
;;; What arrives from the adapter arrives while events are handled, where
;;; nothing may change the windows: what is to be shown is shown by the
;;; command loop, through QUEUE-COMMAND.

(in-package :heml)


;;;; Adapters.

(defvar *debug-adapters* '()
  "((NAME MODES COMMANDS LAUNCH LISTENS) ...): for each adapter, the major
   modes it debugs; the commands that run it, the first whose program is
   installed being used; a function of a buffer that returns the launch
   request's arguments, or NIL when the user declines; and whether it
   listens on a socket, which it names on its output, rather than talking
   on its standard input and output.")

(defun define-debug-adapter (name &key modes commands launch listens)
  "The adapter NAME debugs the files of MODES: COMMANDS are the command
   lines that run it, LAUNCH a function of the buffer being debugged that
   returns what to launch -- the protocol's launch arguments -- and LISTENS
   true for an adapter that says on its output where it is listening."
  (setf *debug-adapters*
        (cons (list name modes commands launch listens)
              (remove name *debug-adapters* :key #'first :test #'string=)))
  ;; Their windows have a fringe, for the breakpoints and the arrow.
  (dolist (mode modes)
    (setf (mode-fringe-width mode) (max 2 (mode-fringe-width mode))))
  name)

(defun buffer-debug-adapter (buffer)
  "The adapter for BUFFER's mode, and the command that runs it; NIL when
   there is none installed."
  (dolist (entry *debug-adapters*)
    (when (member (buffer-major-mode buffer) (second entry) :test #'string=)
      (let ((command (find-if (lambda (command)
                                (or (uiop:absolute-pathname-p (first command))
                                    (find-program (first command))))
                              (third entry))))
        (when command
          (return (values entry command)))))))


;;;; A session.

(defstruct (dap-session (:constructor %make-dap-session))
  adapter                               ; the entry in *DEBUG-ADAPTERS*
  connection
  process                               ; for an adapter that listens: its process
  (seq 0)
  (pending (make-hash-table))           ; request seq to the function its answer goes to
  (input (make-array 4096 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
  capabilities
  (state :starting)                     ; :STARTING, :RUNNING, :STOPPED or :ENDED
  launch                                ; the launch arguments
  launched                              ; whether the launch request has been sent
  configure                             ; whether the adapter is waiting to be configured
  thread                                ; the stopped thread's id
  reason                                ; why it stopped
  frames                                ; the stopped thread's frames, the protocol's
  frame                                 ; the selected one
  variables                             ; ((VARIABLE DEPTH) ...): what the Debugger buffer shows
  (expanded (make-hash-table :test 'equal))) ; variable reference to its parts, once shown

(defvar *dap* nil
  "The debugging session, or NIL.")

(defvar *debug-output* '()
  "What the program has printed that is yet to be put in its buffer, the
   latest first.")

(defun dap-log (direction text)
  (lsp-log (format nil "dap~A" direction) text))

(defun dap-send (session object)
  (let ((connection (dap-session-connection session)))
    (when connection
      (let* ((text (com.inuoe.jzon:stringify object))
             (body (progn (dap-log "->" text)
                          (babel:string-to-octets text :encoding :utf-8)))
             (header (babel:string-to-octets
                      (format nil "Content-Length: ~D~C~C~C~C" (length body)
                              #\Return #\Linefeed #\Return #\Linefeed)
                      :encoding :utf-8)))
        (handler-case
            (connection-write (concatenate '(simple-array (unsigned-byte 8) (*)) header body)
                              connection)
          (error (condition)
            (dap-log "!!" (format nil "not sent: ~A" condition))))))))

(defun dap-request (session command arguments &optional function)
  "Ask the adapter COMMAND, with ARGUMENTS; FUNCTION, if given, is called
   with the answer's body and whether it succeeded."
  (let ((seq (incf (dap-session-seq session))))
    (when function
      (setf (gethash seq (dap-session-pending session)) function))
    (dap-send session (json "seq" seq "type" "request" "command" command
                            "arguments" (or arguments (json))))
    seq))

(defun dap-request-wait (session command arguments &key (timeout 5))
  "Ask the adapter and wait for its answer: the body, or NIL; and, as a
   second value, whether it succeeded, or the adapter's message when not."
  (let ((done nil) (body nil) (success nil))
    (dap-request session command arguments
                 (lambda (answer ok)
                   (setf body answer success ok done t)))
    (lsp-wait (lambda () (or done (eq (dap-session-state session) :ended))) timeout)
    (values body success)))

(defun dap-receive (session bytes)
  "Bytes from the adapter: each message, once its header and body are here,
   is dispatched."
  (let ((input (dap-session-input session)))
    (loop for byte across bytes do (vector-push-extend byte input))
    (loop
      (let ((header-end (search #(13 10 13 10) input)))
        (unless header-end (return))
        (let* ((header (babel:octets-to-string input :end header-end :encoding :utf-8 :errorp nil))
               (at (search "content-length:" header :test #'char-equal))
               (length (and at (parse-integer header :start (+ at 15) :junk-allowed t)))
               (start (+ header-end 4)))
          (unless length
            (setf (fill-pointer input) 0)
            (return))
          (when (< (length input) (+ start length))
            (return))
          (let ((body (babel:octets-to-string input :start start :end (+ start length)
                                                    :encoding :utf-8 :errorp nil)))
            (replace input input :start2 (+ start length))
            (decf (fill-pointer input) (+ start length))
            (dap-log "<-" body)
            (handler-case (dap-dispatch session (com.inuoe.jzon:parse body))
              (error (condition) (dap-log "!!" (princ-to-string condition))))))))))

(defun dap-dispatch (session message)
  (let ((type (jref message "type")))
    (cond ((equal type "response")
           (let ((function (gethash (jref message "request_seq") (dap-session-pending session))))
             (when function
               (remhash (jref message "request_seq") (dap-session-pending session))
               (funcall function (jref message "body")
                        (or (jref message "success") (or (jref message "message") nil))))))
          ((equal type "event")
           (dap-event session (jref message "event") (jref message "body")))
          ((equal type "request")
           ;; What only an editor with a terminal of its own could do, and
           ;; sessions within this one: Heml does neither.
           (dap-send session (json "seq" (incf (dap-session-seq session)) "type" "response"
                                   "request_seq" (jref message "seq")
                                   "command" (jref message "command")
                                   "success" nil
                                   "message" "Heml does not do this."))))))


;;;; What the adapter says.

(defun dap-event (session event body)
  (cond ((equal event "initialized")
         ;; Now the breakpoints, and then the program may go -- once it has
         ;; been launched: an adapter may say this as soon as it is
         ;; initialized, before it has been told what to run.
         (if (dap-session-launched session)
             (dap-configure session)
             (setf (dap-session-configure session) t)))
        ((equal event "stopped")
         (setf (dap-session-state session) :stopped
               (dap-session-reason session) (or (jref body "description") (jref body "reason"))
               (dap-session-thread session) (or (jref body "threadId") (dap-session-thread session)))
         (if (dap-session-thread session)
             (dap-fetch-frames session)
             (dap-request session "threads" (json)
                          (lambda (answer ok)
                            (declare (ignore ok))
                            (setf (dap-session-thread session)
                                  (jref (first (jlist (jref answer "threads"))) "id"))
                            (dap-fetch-frames session)))))
        ((equal event "continued")
         (setf (dap-session-state session) :running
               (dap-session-frames session) nil
               (dap-session-frame session) nil)
         (incf hi:*decoration-tick*))
        ((equal event "output")
         (unless (equal (jref body "category") "telemetry")
           (push (jref body "output") *debug-output*)))
        ((member event '("terminated" "exited") :test #'equal)
         (when (equal event "exited")
           (push (format nil "~&The program exited~@[ with ~D~].~%" (jref body "exitCode"))
                 *debug-output*))
         (unless (eq (dap-session-state session) :ended)
           (dap-end session)))))

(defun dap-configure (session)
  "Set the breakpoints, and say the program may go."
  (setf (dap-session-configure session) nil)
  (dap-send-all-breakpoints session)
  (when (jref (dap-session-capabilities session) "exceptionBreakpointFilters")
    (dap-request session "setExceptionBreakpoints" (json "filters" (vector))))
  (dap-request session "configurationDone" (json)))

(defun dap-fetch-frames (session)
  "Ask for the stopped thread's frames, and then show where it stopped."
  (dap-request session "stackTrace"
               (json "threadId" (dap-session-thread session) "startFrame" 0 "levels" 50)
               (lambda (answer ok)
                 (declare (ignore ok))
                 (setf (dap-session-frames session) (jlist (jref answer "stackFrames"))
                       (dap-session-frame session) (first (dap-session-frames session)))
                 (dap-fetch-variables session
                                      (lambda ()
                                        (queue-command 'dap-show-stop))))))

(defun dap-fetch-variables (session then)
  "Ask for the selected frame's variables -- those of its first scope, its
   locals -- and call THEN."
  (let ((frame (dap-session-frame session)))
    (setf (dap-session-variables session) '())
    (clrhash (dap-session-expanded session))
    (if (null frame)
        (funcall then)
        (dap-request session "scopes" (json "frameId" (jref frame "id"))
                     (lambda (answer ok)
                       (declare (ignore ok))
                       (let ((scope (first (jlist (jref answer "scopes")))))
                         (if (null scope)
                             (funcall then)
                             (dap-request session "variables"
                                          (json "variablesReference"
                                                (jref scope "variablesReference"))
                                          (lambda (answer ok)
                                            (declare (ignore ok))
                                            (setf (dap-session-variables session)
                                                  (mapcar (lambda (variable) (list variable 0))
                                                          (jlist (jref answer "variables"))))
                                            (funcall then))))))))))

(defun dap-show-output ()
  "Put what the program has printed in the buffer \"Debug Output\"."
  (when *debug-output*
    (let ((buffer (or (getstring "Debug Output" *buffer-names*)
                      (make-buffer "Debug Output" :modes '("Fundamental")))))
      (with-writable-buffer (buffer)
        (dolist (text (nreverse (shiftf *debug-output* '())))
          (insert-string (buffer-end-mark buffer) (remove #\Return text))))
      (setf (buffer-modified buffer) nil)
      ;; Shown, at its end, in a window that shows it.
      (dolist (window (buffer-windows buffer))
        (move-mark (window-point window) (buffer-end-mark buffer))))))

(defun dap-idle (&optional elapsed)
  (declare (ignore elapsed))
  (ignore-errors (dap-show-output)))

(defun start-dap-idle ()
  (remove-scheduled-event 'dap-idle)
  (schedule-event 0.5 'dap-idle))

(add-hook entry-hook 'start-dap-idle)


;;;; Starting and ending.

(defun dap-start (buffer)
  "Debug what BUFFER's adapter launches for BUFFER."
  (multiple-value-bind (adapter command) (buffer-debug-adapter buffer)
    (unless adapter
      (editor-error "No debugger for ~A here." (buffer-major-mode buffer)))
    (let ((launch (funcall (fourth adapter) buffer)))
      (unless launch (editor-error "Nothing to debug."))
      (when *dap* (dap-end *dap*))
      (setf *debug-output* '())
      (let* ((session (%make-dap-session :adapter adapter :launch launch))
             (directory (or (jref launch "cwd") (directory-namestring (buffer-pathname buffer))))
             (shell-command (format nil "exec ~{~A~^ ~}~:[ 2>/dev/null~;~]"
                                    (mapcar #'shell-quote command) (fifth adapter))))
        (setf *dap* session)
        (flet ((filter (connection bytes)
                 (declare (ignore connection))
                 (handler-case (dap-receive session bytes)
                   (error (condition) (dap-log "!!" (princ-to-string condition))))
                 nil)
               (sentinel (connection event)
                 (declare (ignore connection))
                 (when (and (member event '(:disconnected :error))
                            (not (eq (dap-session-state session) :ended)))
                   (dap-end session))))
          (cond ((fifth adapter)
                 ;; It says where it listens, and is then dialled.
                 (let ((said (make-array 0 :element-type 'character :adjustable t
                                           :fill-pointer 0)))
                   (setf (dap-session-process session)
                         (make-process-connection
                          (list "/bin/sh" "-c" (format nil "~A 2>&1" shell-command))
                          :directory directory
                          :filter (lambda (connection bytes)
                                    (declare (ignore connection))
                                    (unless (dap-session-connection session)
                                      (loop for char across (babel:octets-to-string
                                                             bytes :encoding :utf-8 :errorp nil)
                                            do (vector-push-extend char said))
                                      (multiple-value-bind (start end starts ends)
                                          (cl-ppcre:scan "listening at:? *([0-9.]+):([0-9]+)" said)
                                        (declare (ignore end))
                                        (when start
                                          (setf (dap-session-connection session)
                                                (make-tcp-connection
                                                 "debug adapter"
                                                 (subseq said (aref starts 0) (aref ends 0))
                                                 (parse-integer said :start (aref starts 1)
                                                                     :end (aref ends 1))
                                                 :filter #'filter :sentinel #'sentinel)))))
                                    nil)
                          :sentinel #'sentinel))
                   (unless (lsp-wait (lambda () (dap-session-connection session)) 10)
                     (dap-end session)
                     (editor-error "The debugger did not say where it listens."))))
                (t
                 (setf (dap-session-connection session)
                       (make-process-connection
                        (list "/bin/sh" "-c" shell-command)
                        :directory directory :filter #'filter :sentinel #'sentinel)))))
        (multiple-value-bind (capabilities ok)
            (dap-request-wait session "initialize"
                              (json "clientID" "heml" "clientName" "Heml"
                                    "adapterID" (first adapter)
                                    "pathFormat" "path" "linesStartAt1" t "columnsStartAt1" t
                                    "supportsVariableType" t
                                    "supportsRunInTerminalRequest" nil)
                              :timeout 15)
          (unless (eq ok t)
            (dap-end session)
            (editor-error "The debugger would not start~@[: ~A~]." (and (stringp ok) ok)))
          (setf (dap-session-capabilities session) capabilities
                (dap-session-state session) :running)
          ;; Answered, by some adapters, only once the program has been
          ;; configured; what it says when it fails is said.
          (dap-request session "launch" launch
                       (lambda (answer ok)
                         (declare (ignore answer))
                         (unless (eq ok t)
                           (push (format nil "~&The program could not be launched~@[: ~A~].~%"
                                         (and (stringp ok) ok))
                                 *debug-output*)
                           (dap-end session))))
          (setf (dap-session-launched session) t)
          (when (dap-session-configure session)
            (dap-configure session))
          (message "Debugging ~A." (or (jref launch "program") (jref launch "name") "")))))))

(defun dap-end (session)
  "SESSION is over: the adapter is let go of, and where it stopped is no
   longer shown."
  (unless (eq (dap-session-state session) :ended)
    (setf (dap-session-state session) :ended)
    (ignore-errors (dap-request session "disconnect" (json "terminateDebuggee" t)))
    (dolist (connection (list (dap-session-connection session) (dap-session-process session)))
      (when connection
        (ignore-errors (delete-connection connection))))
    (setf (dap-session-connection session) nil
          (dap-session-process session) nil
          (dap-session-frames session) nil
          (dap-session-frame session) nil)
    (when (eq *dap* session) (setf *dap* nil))
    (incf hi:*decoration-tick*)
    (push (format nil "~&Debugging is over.~%") *debug-output*)))

(defun current-dap (&optional (state :stopped))
  "The session, in STATE: :STOPPED for stepping, T for any."
  (cond ((null *dap*) (editor-error "Nothing is being debugged."))
        ((and (eq state :stopped) (not (eq (dap-session-state *dap*) :stopped)))
         (editor-error "The program is running."))
        (t *dap*)))


;;;; Breakpoints.

(defvar *breakpoints* '()
  "The breakpoints: a mark at the start of each line that has one.")

(defun mark-line-number (mark)
  "MARK's line's number, from 1."
  (count-lines (region (buffer-start-mark (line-buffer (mark-line mark))) mark)))

(defun file-breakpoints (file)
  "The lines, from 1, of FILE's breakpoints."
  (sort (loop for mark in *breakpoints*
              for buffer = (line-buffer (mark-line mark))
              when (and buffer (buffer-pathname buffer)
                        (equal (namestring (buffer-pathname buffer)) file))
                collect (mark-line-number mark))
        #'<))

(defun dap-send-breakpoints (session file)
  (dap-request session "setBreakpoints"
               (json "source" (json "path" file)
                     "breakpoints" (map 'vector (lambda (line) (json "line" line))
                                        (file-breakpoints file))
                     "lines" (coerce (file-breakpoints file) 'vector))))

(defun dap-send-all-breakpoints (session)
  (dolist (file (remove-duplicates
                 (loop for mark in *breakpoints*
                       for buffer = (line-buffer (mark-line mark))
                       when (and buffer (buffer-pathname buffer))
                         collect (namestring (buffer-pathname buffer)))
                 :test #'equal))
    (dap-send-breakpoints session file)))

(defparameter *breakpoint-font* '(:fg 1 :bold t))
(defparameter *stopped-font* '(:fg 2 :bold t))

(defun dap-stopped-place ()
  "The file and line, from 1, of the selected frame, or NIL."
  (let ((frame (and *dap* (eq (dap-session-state *dap*) :stopped) (dap-session-frame *dap*))))
    (when frame
      (values (jref frame "source" "path") (jref frame "line")))))

(defun dap-line-fringe (line)
  "In the fringe (winimage.lisp), a dot beside a line with a breakpoint, and
   an arrow beside the line the program stopped at."
  (let ((buffer (line-buffer line)))
    (when (and buffer (or *breakpoints* *dap*))
      (append
       (when (find line *breakpoints* :key #'mark-line)
         (list (list 0 "●" *breakpoint-font*)))
       (multiple-value-bind (file number) (dap-stopped-place)
         (when (and file (buffer-pathname buffer)
                    (equal (namestring (buffer-pathname buffer)) file)
                    (eql number (mark-line-number (mark line 0))))
           (list (list 1 "▶" *stopped-font*))))))))

(pushnew 'dap-line-fringe hi:*line-fringe-functions*)


;;;; Showing where it stopped.

(defmode "Debugger" :major-p t
  :documentation "Where the program being debugged stopped: its frames, and
   the selected frame's variables.  Return on a frame selects it; on a
   variable with parts, shows them.")

(defparameter *debugger-header-font* '(:bold t))

(defun dap-show-stop ()
  "In the command loop: show the place the selected frame is at, and the
   Debugger buffer."
  (let ((session *dap*))
    (when (and session (eq (dap-session-state session) :stopped))
      (dap-fill-debugger-buffer session)
      (multiple-value-bind (file line) (dap-stopped-place)
        (when (and file (probe-file file))
          (let ((buffer (find-file-buffer file)))
            (unless (eq (current-buffer) buffer)
              (change-to-buffer buffer))
            (buffer-start (current-point))
            (line-offset (current-point) (1- line) 0)
            ;; At the line's code, so that the arrow before it is seen.
            (let ((text (line-string (mark-line (current-point)))))
              (line-offset (current-point) 0
                           (or (position-if-not (lambda (char) (member char '(#\Space #\Tab)))
                                                text)
                               0))))))
      (incf hi:*decoration-tick*)
      (message "Stopped~@[: ~A~]." (dap-session-reason session)))))

(defun variable-text (variable depth)
  (format nil "~vT~A = ~A~@[  (~A)~]~:[~;  ...~]" (+ 2 (* 2 depth))
          (jref variable "name")
          (substitute #\Space #\Newline (or (jref variable "value") ""))
          (let ((type (jref variable "type"))) (and (stringp type) (plusp (length type)) type))
          (let ((reference (jref variable "variablesReference")))
            (and (integerp reference) (plusp reference)))))

(defun dap-fill-debugger-buffer (session)
  (let ((buffer (or (getstring "Debugger" *buffer-names*)
                    (make-buffer "Debugger" :modes '("Debugger")))))
    (with-writable-buffer (buffer)
      (delete-region (buffer-region buffer))
      (let ((point (buffer-point buffer)))
        (flet ((out (text &optional entry)
                 (let ((line (mark-line point)))
                   (insert-string point text)
                   (insert-character point #\Newline)
                   (setf (getf (line-plist line) 'debugger-entry) entry))))
          (out (format nil "Stopped~@[: ~A~]" (dap-session-reason session)))
          (out "")
          (out "Frames")
          (loop for frame in (dap-session-frames session)
                for number from 0
                do (out (format nil "  ~:[ ~;>~]#~D ~A~@[  ~A~]~@[:~D~]"
                                (eq frame (dap-session-frame session)) number
                                (jref frame "name")
                                (let ((path (jref frame "source" "path")))
                                  (and path (file-namestring path)))
                                (jref frame "line"))
                        (list :frame frame)))
          (out "")
          (out "Variables")
          (dolist (entry (dap-session-variables session))
            (destructuring-bind (variable depth) entry
              (out (variable-text variable depth) (list :variable variable depth)))))))
    (setf (buffer-modified buffer) nil)
    buffer))

(defun debugger-highlight-line (line)
  (let ((text (line-string line)))
    (hi:delete-line-font-marks line)
    (when (member text '("Frames" "Variables") :test #'string=)
      (hi:font-mark line 0 *debugger-header-font*))
    (when (uiop:string-prefix-p "Stopped" text)
      (hi:font-mark line 0 *stopped-font*))))

(define-mode-highlighter "Debugger" 'debugger-highlight-line)

(defcommand "Debugger Select" (p)
  "On a frame: make it the frame shown, its place and its variables.  On a
   variable with parts: show them, under it."
  "Select the frame, or show the variable's parts."
  (declare (ignore p))
  (let* ((session (current-dap))
         (entry (getf (line-plist (mark-line (current-point))) 'debugger-entry)))
    (case (first entry)
      (:frame
       (setf (dap-session-frame session) (second entry))
       (dap-fetch-variables session (lambda () (queue-command 'dap-show-stop))))
      (:variable
       (destructuring-bind (variable depth) (rest entry)
         (let ((reference (jref variable "variablesReference")))
           (unless (and (integerp reference) (plusp reference))
             (editor-error "~A has no parts." (jref variable "name")))
           (unless (gethash reference (dap-session-expanded session))
             (let ((parts (jlist (jref (dap-request-wait session "variables"
                                                         (json "variablesReference" reference))
                                       "variables"))))
               (setf (gethash reference (dap-session-expanded session)) t)
               (let ((at (position variable (dap-session-variables session) :key #'first)))
                 (setf (dap-session-variables session)
                       (append (subseq (dap-session-variables session) 0 (1+ at))
                               (mapcar (lambda (part) (list part (1+ depth))) parts)
                               (subseq (dap-session-variables session) (1+ at)))))))
           (let ((line (count-lines (region (buffer-start-mark (current-buffer)) (current-point)))))
             (dap-fill-debugger-buffer session)
             (buffer-start (current-point))
             (line-offset (current-point) (1- line) 0)))))
      (t (editor-error "Nothing here to select.")))))


;;;; Commands.

(defun dap-step (command)
  (let ((session (current-dap)))
    (setf (dap-session-state session) :running)
    (incf hi:*decoration-tick*)
    (dap-request session command (json "threadId" (dap-session-thread session)))))

(defcommand "Debug" (p)
  "Debug what this buffer is: run it, or the program its project builds, in
   its language's debugger, stopping at its breakpoints.  With a session
   stopped, go on."
  "Debug what this buffer is, or go on."
  (declare (ignore p))
  (if (and *dap* (eq (dap-session-state *dap*) :stopped))
      (dap-step "continue")
      (dap-start (current-buffer))))

(defcommand "Debug Toggle Breakpoint" (p)
  "Put a breakpoint on this line, or take it away."
  "Put a breakpoint on this line, or take it away."
  (declare (ignore p))
  (let* ((line (mark-line (current-point)))
         (old (find line *breakpoints* :key #'mark-line))
         (pathname (buffer-pathname (current-buffer))))
    (unless pathname (editor-error "A breakpoint is in a file, and this buffer has none."))
    (cond (old
           (setf *breakpoints* (remove old *breakpoints*))
           (delete-mark old))
          (t
           (push (mark line 0 :right-inserting) *breakpoints*)))
    (when (and *dap* (not (eq (dap-session-state *dap*) :ended)))
      (dap-send-breakpoints *dap* (namestring pathname)))
    (incf hi:*decoration-tick*)
    (message "~:[Breakpoint taken away~;Breakpoint~] at line ~D."
             (not old) (mark-line-number (current-point)))))

(defcommand "Debug Continue" (p)
  "Let the stopped program go on."
  "Let the stopped program go on."
  (declare (ignore p))
  (dap-step "continue"))

(defcommand "Debug Next" (p)
  "Go on to the next line, over calls."
  "Go on to the next line, over calls."
  (declare (ignore p))
  (dap-step "next"))

(defcommand "Debug Step In" (p)
  "Go on into the call on this line."
  "Go on into the call on this line."
  (declare (ignore p))
  (dap-step "stepIn"))

(defcommand "Debug Step Out" (p)
  "Go on until this function returns."
  "Go on until this function returns."
  (declare (ignore p))
  (dap-step "stepOut"))

(defcommand "Debug Pause" (p)
  "Stop the running program where it is."
  "Stop the running program where it is."
  (declare (ignore p))
  (let ((session (current-dap t)))
    (dap-request session "pause" (json "threadId" (or (dap-session-thread session) 1)))))

(defcommand "Debug Stop" (p)
  "Stop debugging: the program is ended."
  "Stop debugging."
  (declare (ignore p))
  (dap-end (current-dap t))
  (message "Debugging is over."))

(defcommand "Debug Evaluate" (p)
  "Evaluate an expression in the selected frame of the stopped program,
   and say what it is."
  "Evaluate an expression in the stopped program."
  (declare (ignore p))
  (let* ((session (current-dap))
         (expression (prompt-for-string :prompt "Evaluate: "
                                        :default (let ((word (word-at-point)))
                                                   (and (plusp (length word)) word))))
         (frame (dap-session-frame session)))
    (multiple-value-bind (body ok)
        (dap-request-wait session "evaluate"
                          (json "expression" expression "context" "watch"
                                "frameId" (if frame (jref frame "id") 'null)))
      (if (eq ok t)
          (message "~A = ~A" expression (substitute #\Space #\Newline (or (jref body "result") "")))
          (editor-error "~A" (if (stringp ok) ok "It could not be evaluated."))))))

(defcommand "Debug Show" (p)
  "Show the Debugger buffer: the stopped program's frames and variables."
  "Show the Debugger buffer."
  (declare (ignore p))
  (let ((session (current-dap)))
    (let ((buffer (dap-fill-debugger-buffer session)))
      (select-window (other-window))
      (change-to-buffer buffer))))


;;;; The adapters Heml knows.

(defun project-or-file-directory (buffer)
  (or (buffer-project-root buffer) (directory-namestring (buffer-pathname buffer))))

(defun file-launch (buffer)
  "What to launch for a program that is BUFFER's file itself, a script."
  (json "name" (file-namestring (buffer-pathname buffer))
        "program" (namestring (buffer-pathname buffer))
        "cwd" (project-or-file-directory buffer)))

(defun prompt-for-program (buffer default)
  "The program to debug, asked for, DEFAULT offered."
  (let ((program (prompt-for-file :prompt "Program to debug: "
                                  :default (or default (project-or-file-directory buffer))
                                  :must-exist t)))
    (and program (namestring program))))

(defun rust-program (buffer)
  "The binary cargo builds for BUFFER's crate, at its usual place."
  (let* ((root (project-or-file-directory buffer))
         (manifest (merge-pathnames "Cargo.toml" root)))
    (when (probe-file manifest)
      (let ((name (with-open-file (in manifest)
                    (loop for line = (read-line in nil)
                          while line
                          do (multiple-value-bind (match groups)
                                 (cl-ppcre:scan-to-strings "^name *= *\"([^\"]+)\"" line)
                               (when match (return (aref groups 0))))))))
        (when name
          (namestring (merge-pathnames (format nil "target/debug/~A" name) root)))))))

(defun lldb-launch (buffer)
  (let* ((file (buffer-pathname buffer))
         (default (if (string-equal (buffer-major-mode buffer) "Rust")
                      (rust-program buffer)
                      (let ((guess (make-pathname :type nil :defaults file)))
                        (and (probe-file guess) (namestring guess)))))
         (program (prompt-for-program buffer default)))
    (json "name" (file-namestring program) "program" program
          "args" (vector) "cwd" (project-or-file-directory buffer)
          "stopOnEntry" nil)))

(defun lldb-dap-command ()
  "lldb-dap: from PATH, or Xcode's."
  (or (and (find-program "lldb-dap") '("lldb-dap"))
      (let ((path (ignore-errors
                   (uiop:run-program '("xcrun" "-f" "lldb-dap")
                                     :output '(:string :stripped t) :error-output nil))))
        (and path (plusp (length path)) (list path)))))

(define-debug-adapter "lldb"
  :modes '("C" "Rust")
  :commands (remove nil (list '("lldb-dap") (ignore-errors (lldb-dap-command))))
  :launch 'lldb-launch)

(define-debug-adapter "debugpy"
  :modes '("Python")
  :commands '(("debugpy-adapter") ("python3" "-m" "debugpy.adapter"))
  :launch (lambda (buffer)
            (json "name" (file-namestring (buffer-pathname buffer))
                  "type" "python" "request" "launch"
                  "program" (namestring (buffer-pathname buffer))
                  "cwd" (project-or-file-directory buffer)
                  "console" "internalConsole" "justMyCode" t)))

(define-debug-adapter "delve"
  :modes '("Go")
  :commands '(("dlv" "dap" "-l" "127.0.0.1:0"))
  :listens t
  :launch (lambda (buffer)
            ;; What the program prints comes as events, not on dlv's own
            ;; output, which Heml does not read once it is connected.
            (json "name" "go" "request" "launch" "mode" "debug" "outputMode" "remote"
                  "program" (directory-namestring (buffer-pathname buffer))
                  "cwd" (project-or-file-directory buffer))))


;;;; Keys: the function keys most editors use for this, and C-c d for a
;;;; terminal without them.

(bind-key "Debug" #k"F5")
(bind-key "Debug Stop" #k"shift-F5")
(bind-key "Debug Toggle Breakpoint" #k"F9")
(bind-key "Debug Next" #k"F10")
(bind-key "Debug Step In" #k"F11")
(bind-key "Debug Step Out" #k"shift-F11")
(bind-key "Debug" #k"control-c d d")
(bind-key "Debug Toggle Breakpoint" #k"control-c d b")
(bind-key "Debug Continue" #k"control-c d c")
(bind-key "Debug Next" #k"control-c d n")
(bind-key "Debug Step In" #k"control-c d s")
(bind-key "Debug Step Out" #k"control-c d f")
(bind-key "Debug Pause" #k"control-c d p")
(bind-key "Debug Stop" #k"control-c d k")
(bind-key "Debug Evaluate" #k"control-c d e")
(bind-key "Debug Show" #k"control-c d w")
(bind-key "Debugger Select" #k"return" :mode "Debugger")
(bind-key "Debugger Select" #k"control-m" :mode "Debugger")

(define-menu "Debug" ()
  ("Debug" "Debug")
  ("Toggle Breakpoint" "Debug Toggle Breakpoint")
  :separator
  ("Continue" "Debug Continue")
  ("Step Over" "Debug Next")
  ("Step In" "Debug Step In")
  ("Step Out" "Debug Step Out")
  ("Pause" "Debug Pause")
  ("Evaluate…" "Debug Evaluate")
  ("Show Frames and Variables" "Debug Show")
  :separator
  ("Stop" "Debug Stop"))
