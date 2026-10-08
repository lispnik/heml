;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :hi)
#+sbcl (declaim (optimize (speed 2)))

(defmethod invoke-with-new-event-loop ((backend (eql :iolib)) fun)
  ;; the epoll muxer gives me segfaults and memory corruption.
  ;; Don't know why, but select works, so let's use it:
  (iolib:with-event-base
      (*event-base* :mux 'iolib.multiplex:select-multiplexer)
    (funcall fun)))

(defmethod make-event-loop ((backend (eql :iolib)))
  (make-instance 'iolib:event-base
                 :mux 'iolib.multiplex:select-multiplexer))

(defmethod invoke-with-existing-event-loop ((backend (eql :iolib)) loop fun)
  (let ((*event-base* loop))
    (funcall fun)))

(defmethod dispatch-events-with-backend ((backend (eql :iolib)))
  (handler-case
      (iolib:event-dispatch *event-base* :one-shot t :min-step 0)
    ((or isys:etimedout isys:ewouldblock) ())))

(defmethod dispatch-events-no-hang-with-backend ((backend (eql :iolib)))
  (handler-case
      (iolib:event-dispatch *event-base*
                            :one-shot t
                            :timeout 0
                            :min-step 0)
    ((or isys:etimedout isys:ewouldblock) ())))

(defmethod dispatch-events-for-with-backend ((backend (eql :iolib)) seconds)
  (handler-case
      (iolib:event-dispatch *event-base*
                            :one-shot t
                            :timeout seconds
                            :min-step 0)
    ((or isys:etimedout isys:ewouldblock) ())))

(defmethod invoke-later ((backend (eql :iolib)) fun)
  (iolib.multiplex:add-timer *event-base* fun 0 :one-shot t))


;;;;
;;;; IOLIB-CONNECTION
;;;;

(defclass iolib-connection (io-connection)
  ((read-fd :initarg :read-fd
            :initarg :fd
            :initform nil
            :accessor connection-read-fd)
   (write-fd :initarg :write-fd
             :initarg :fd
             :initform nil
             :accessor connection-write-fd)
   (write-buffers :initform nil
                  :accessor connection-write-buffers)))

(defmethod initialize-instance :after
    ((instance iolib-connection) &key)
  )

(defmethod (setf connection-read-fd)
    :after
    ((newval t) (connection iolib-connection))
  (when (connection-read-fd connection)
    (set-iolib-handlers connection)))

(defmethod (setf connection-write-fd)
    :after
    ((newval t) (connection iolib-connection))
  (when (connection-write-fd connection)
    (set-iolib-handlers connection)))

(defun set-iolib-handlers (connection)
  (let ((fd (connection-read-fd connection)))
    (iolib:set-io-handler
     *event-base*
     fd
     :read
     (lambda (.fd event error)
       (declare (ignore event .fd))
       (when (or (eq error :error)
                 (eq (process-incoming-data connection) :eof))
         (iolib:remove-fd-handlers *event-base* fd :read t))))))

(defmethod %read ((connection iolib-connection))
  (let* ((fd (connection-read-fd connection))
         (buffer (connection-input-buffer connection))
         (n
          ;; fixme: with-pointer-to-vector-data isn't portable
          (cffi-sys:with-pointer-to-vector-data (ptr buffer)
            (isys:read fd ptr (length buffer)))))
    (cond
      ((zerop n)
       :eof)
      (t
       (subseq buffer 0 n)))))

(defmethod delete-connection :before ((connection iolib-connection))
  (with-slots (read-fd write-fd) connection
    ;; The event loop forgets the descriptors before they are closed.  Left
    ;; watching a closed one, its next select(2) fails with EBADF, as it did
    ;; when a shell's buffer was killed.
    (dolist (fd (remove-duplicates (remove nil (list read-fd write-fd))))
      (iolib:remove-fd-handlers *event-base* fd))
    (when read-fd
      (isys:close read-fd))
    (when write-fd
      (unless (eql write-fd read-fd)
        (isys:close write-fd)))))

(defmethod connection-listen ((connection iolib-connection))
  (iolib.multiplex:fd-readablep (connection-read-fd connection)))

;;; What is written is queued, and a handler writes it when the descriptor
;;; can take it.  The handler is there exactly while the queue is not empty:
;;; it removes itself on emptying it.  Marked one-shot instead, it would be
;;; removed only when the loop had handled every descriptor ready in that
;;; pass, and a filter run before then that wrote here (a language server's
;;; question, answered as it is read) would find the queue empty and the
;;; handler still set, and its write, and every one after it, would be lost.
;;;
(defmethod connection-write (data (connection iolib-connection))
  (let ((bytes (filter-connection-output connection data))
        (fd (connection-write-fd connection))
        (need-handler (null (connection-write-buffers connection))))
    (check-type bytes (simple-array (unsigned-byte 8) (*)))
    (setf (connection-write-buffers connection)
          (nconc (connection-write-buffers connection)
                 (list bytes)))
    (when need-handler
      (iolib:set-io-handler
       *event-base*
       fd
       :write
       (lambda (.fd event error)
         (declare (ignore event))
         ;; What cannot be written -- the other end has gone -- is dropped,
         ;; with the rest of the queue: the handler must not stay with
         ;; nothing to write, and whoever reads the connection will find
         ;; it closed.
         (handler-case
             (progn
               (when (eq error :error) (error "error with ~A" .fd))
               ;; fixme: with-pointer-to-vector-data isn't portable
               (let ((bytes (pop (connection-write-buffers connection))))
                 (when bytes
                   (cffi-sys:with-pointer-to-vector-data (ptr bytes)
                     (let ((n-bytes-written
                             (isys:write fd ptr (length bytes))))
                       (unless (eql n-bytes-written (length bytes))
                         (push (subseq bytes n-bytes-written)
                               (connection-write-buffers connection))))))))
           (error ()
             (setf (connection-write-buffers connection) nil)))
         (unless (connection-write-buffers connection)
           (ignore-errors (iolib:remove-fd-handlers *event-base* fd :write t))))))))


;;;;
;;;; PROCESS-CONNECTION/IOLIB
;;;;

(defclass process-connection/iolib
    (process-connection-mixin iolib-connection)
  ((pid :initform nil
        :accessor connection-pid)))

(defmethod initialize-instance
    :after
    ((instance process-connection/iolib) &key)
  (with-slots (read-fd write-fd pid command slave-pty-name directory) instance
    (connection-note-event instance :initialized)
    (when (stringp command)
      (setf command (cl-ppcre:split " " command)))
    (assert (every #'stringp command))
    (assert command)
    (setf (values pid read-fd write-fd)
          (%fork-and-exec (car command) command directory slave-pty-name
                          (connection-environment instance)
                          (connection-terminal instance)))
    (set-iolib-handlers instance)
    (note-connected instance)))

;;;; Signals.  A process Heml starts is the leader of a session and of a
;;;; process group of its own (setsid in %EXEC-IN-CHILD), and one on a
;;;; terminal has it as its controlling terminal: the terminal's line
;;;; discipline makes ^C, ^Z and ^\ typed to it SIGINT, SIGTSTP and SIGQUIT
;;;; for the job in its foreground, and its size changing SIGWINCH.  Heml
;;;; signals the same job itself (CONNECTION-SIGNAL), hangs up a terminal's
;;;; processes when its connection goes (SIGHUP to the group, as closing a
;;;; terminal does), and reaps each process that ends, keeping its exit
;;;; code: nothing else waits for them, and they were left as zombies.

(defun signal-number (signal)
  (if (integerp signal)
      signal
      (ecase signal
        (:sighup osicat-posix:sighup) (:sigint osicat-posix:sigint)
        (:sigquit osicat-posix:sigquit) (:sigkill osicat-posix:sigkill)
        (:sigterm osicat-posix:sigterm) (:sigstop osicat-posix:sigstop)
        (:sigtstp osicat-posix:sigtstp) (:sigcont osicat-posix:sigcont)
        (:sigwinch osicat-posix:sigwinch))))

(defun signal-group (group signal)
  "Send SIGNAL to the process group GROUP; whether it could be."
  (zerop (cffi:foreign-funcall "killpg" :int group :int (signal-number signal) :int)))

(defun signal-process (pid signal)
  (zerop (cffi:foreign-funcall "kill" :int pid :int (signal-number signal) :int)))

(defmethod connection-signal ((connection process-connection/iolib) signal)
  (let ((pid (connection-pid connection)))
    (when (and pid (not (connection-exit-code connection)))
      (or (signal-group pid signal) (signal-process pid signal)))))

(defun reap-process (connection)
  "Wait for CONNECTION's process if it has ended, keeping its exit code (128
   and the signal's number for one a signal ended, as a shell says) and the
   signal that ended it, or 0; whether it had."
  (or (connection-exit-code connection)
      (let ((pid (connection-pid connection)))
        (cffi:with-foreign-object (status :int)
          (let ((result (cffi:foreign-funcall "waitpid" :int pid :pointer status
                                                        :int osicat-posix::wnohang :int)))
            (cond ((= result pid)
                   (let* ((status (cffi:mem-ref status :int))
                          (signal (logand status #x7f)))
                     (if (zerop signal)
                         (setf (connection-exit-code connection) (ldb (byte 8 8) status)
                               (connection-exit-status connection) 0)
                         (setf (connection-exit-code connection) (+ 128 signal)
                               (connection-exit-status connection) signal)))
                   t)
                  ;; Not a child of ours any more: someone else waited.
                  ((minusp result)
                   (setf (connection-exit-code connection) -1
                         (connection-exit-status connection) 0)
                   t)
                  (t nil)))))))

(defvar *unreaped* '()
  "(CONNECTION . DEADLINE) for each process ended or told to end, but not yet
   reaped: one still there at its DEADLINE is killed.")

(defun reap-processes (elapsed)
  (declare (ignore elapsed))
  (let ((now (get-internal-real-time)))
    (setf *unreaped*
          (remove-if (lambda (entry)
                       (destructuring-bind (connection . deadline) entry
                         (or (reap-process connection)
                             (when (and deadline (> now deadline))
                               (connection-signal connection :sigkill)
                               (setf (cdr entry) nil)
                               nil))))
                     *unreaped*)))
  (unless *unreaped*
    (remove-scheduled-event 'reap-processes)))

(defun reap-later (connection &optional kill-after)
  "Reap CONNECTION's process when it has ended, killing it after KILL-AFTER
   seconds if it has not."
  (unless (or (reap-process connection) (assoc connection *unreaped*))
    (unless *unreaped*
      (schedule-event 0.5 'reap-processes))
    (push (cons connection
                (and kill-after
                     (+ (get-internal-real-time)
                        (* kill-after internal-time-units-per-second))))
          *unreaped*)))

(defmethod note-process-ended ((connection process-connection/iolib))
  (reap-later connection))

(defmethod delete-connection :before ((connection process-connection/iolib))
  (unless (reap-process connection)
    ;; A terminal's processes are hung up, as closing a terminal does; a
    ;; program on pipes, and what it started, are asked to end.
    (connection-signal connection
                       (if (connection-slave-pty-name connection) :sighup :sigterm))
    (reap-later connection 3)))

(defun invoke-without-interrupts (fun)
  (funcall fun))

(defmacro maybe-without-interrupts (&body body)
  `(invoke-without-interrupts (lambda () ,@body)))

(defun %exec
       (stdin-read stdin-write stdout-read stdout-write file args directory
                   slave-pty-name &optional environment terminal)
  ;; No signal from outside is taken from here to exec, and nothing the
  ;; Lisp had queued is handled: ECL's child took a SIGCHLD queued before the fork as an
  ;; error, and sat in the debugger.
  (block-child-signals)
  (heml-ext:without-interrupts
   ;; This is a forked copy of a threaded Lisp: nothing may unwind or reach
   ;; the debugger here, or a second editor goes on running where the
   ;; program was to be, writing into the first's buffer.  Whatever fails --
   ;; a directory that will not do, a terminal setting -- ends the child.
   (handler-case
       (%exec-in-child stdin-read stdin-write stdout-read stdout-write file args directory
                       slave-pty-name environment terminal)
     (serious-condition () nil))
   (cffi:foreign-funcall "_exit" :int 127 :void)))

(defconstant +sig-setmask+ #+darwin 3 #-darwin 2)

(defun child-signals ()
  "The signals that come from outside -- not a trap or a fault, which the
   Lisp itself uses (SBCL's allocation traps), and which may not be blocked."
  (list osicat-posix:sighup osicat-posix:sigint osicat-posix:sigquit
        osicat-posix:sigpipe osicat-posix:sigalrm osicat-posix:sigterm
        osicat-posix:sigchld osicat-posix:sigtstp osicat-posix:sigttin
        osicat-posix:sigttou osicat-posix:sigwinch osicat-posix:sigusr1
        osicat-posix:sigusr2 osicat-posix:sigio osicat-posix:sigurg
        osicat-posix:sigprof osicat-posix:sigvtalrm osicat-posix:sigxcpu))

(defun block-child-signals ()
  (cffi:with-foreign-object (set :uint8 128)
    (cffi:foreign-funcall "sigemptyset" :pointer set :int)
    (dolist (signal (child-signals))
      (cffi:foreign-funcall "sigaddset" :pointer set :int signal :int))
    (cffi:foreign-funcall "sigprocmask" :int +sig-setmask+ :pointer set
                                        :pointer (cffi:null-pointer) :int)))

(defun reset-child-signals ()
  "Leave the program to be run its signals as a shell would: none blocked,
   each to its default action.  A thread of the editor's Lisp may block
   some, or have them ignored, and exec keeps both: a shell started so
   never saw the SIGINT its terminal sent it for ^C."
  (dolist (signal (child-signals))
    (cffi:foreign-funcall "signal" :int signal :pointer (cffi:null-pointer) :pointer))
  ;; sigset_t is 4 bytes on macOS and 128 on Linux: room for either.
  (cffi:with-foreign-object (set :uint8 128)
    (cffi:foreign-funcall "sigemptyset" :pointer set :int)
    (cffi:foreign-funcall "sigprocmask" :int +sig-setmask+ :pointer set
                                        :pointer (cffi:null-pointer) :int)))

(defun close-other-descriptors ()
  "In a forked child, every descriptor but standard input, output and error
   closed: it had all the editor's, and a pseudo-terminal's master among
   them kept the program from ever seeing its terminal hang up, so that a
   shell outlived the editor that started it, holding a terminal of the
   system's few.  macOS has no closefrom."
  (loop for fd from 3 below (min (cffi:foreign-funcall "getdtablesize" :int) 10240)
        do (cffi:foreign-funcall "close" :int fd :int)))

(defun %exec-in-child
       (stdin-read stdin-write stdout-read stdout-write file args directory
                   slave-pty-name &optional environment terminal)
  (progn
   ;; A process without a terminal of its own is in a session of its own,
   ;; with none: nothing it starts -- a program a debugger runs, say -- can
   ;; take the editor's terminal from it, or ask anything on it.
   (unless slave-pty-name
     (isys:setsid))
   (isys:close stdin-write)
   (isys:close stdout-read)
   (isys:dup2 stdin-read 0)
   (isys:dup2 stdout-write 1)
   (isys:dup2 stdout-write 2)
   (isys:close stdin-read)
   (isys:close stdout-write)
   (when slave-pty-name
     (isys:setsid)
     (handler-case
         (isys:open "/dev/tty" isys:o-rdwr)
       (isys:enoent ())
       (isys:enxio ())
       (:no-error (fd)
         (isys:ioctl fd osicat-posix:tiocnotty 0)
         (isys:close fd)))
     (isys:close 0)
     (isys:open slave-pty-name isys:o-rdwr)
     ;; Its controlling terminal: opening it after setsid makes it one on
     ;; Linux but not on macOS, where without this the line discipline had
     ;; no foreground job to send ^C's SIGINT to, and a shell no job control.
     (ignore-errors (isys:ioctl 0 osicat-posix:tiocsctty))
     (isys:dup2 0 1)
     (isys:dup2 0 2)
     (unless terminal
      (cffi:with-foreign-object (tios '(:struct osicat-posix::termios))
       (osicat-posix::tcgetattr 0 tios)
       (cffi:with-foreign-slots ((osicat-posix::iflag
                                  osicat-posix::oflag
                                  osicat-posix::lflag
                                  osicat-posix::cc)
                                 tios (:struct osicat-posix::termios))
         (setf osicat-posix::lflag
               (logandc2 osicat-posix::lflag
                         (logior osicat-posix::tty-echo
                                 osicat-posix::tty-echonl)))
         (setf osicat-posix::iflag
               (logior (logandc2 osicat-posix::iflag
                                 osicat-posix::tty-brkint)
                       osicat-posix::tty-icanon
                       osicat-posix::tty-icrnl))
         (setf osicat-posix::oflag
               (logandc2 osicat-posix::oflag
                         osicat-posix::tty-onlcr ))
         (setf (cffi:mem-ref osicat-posix::cc
                             :uint8
                             osicat-posix::cflag-verase)
               #o177)
         (osicat-posix::tcsetattr 0 osicat-posix::tcsaflush tios)))))
   (when directory
     (isys:chdir (if (pathnamep directory) (namestring directory) directory)))
   (loop for (name . value) in environment
         do (cffi:foreign-funcall "setenv" :string name :string value :int 1 :int))
   (reset-child-signals)
   (close-other-descriptors)
   (let ((n (length args)))
     (cffi:with-foreign-object (argv :pointer (1+ n))
       (iter:iter (iter:for i from 0)
                  (iter:for arg in args)
                  (setf (cffi:mem-aref argv :pointer i)
                        (cffi:foreign-string-alloc arg)))
       (setf (cffi:mem-aref argv :pointer n) (cffi:null-pointer))
       (isys:execvp file argv)))))

(defun %fork-and-exec (file args &optional directory slave-pty-name environment terminal)
  (multiple-value-bind (stdin-read stdin-write)
      (isys:pipe)
    (multiple-value-bind (stdout-read stdout-write)
        (isys:pipe)
      (let ((pid (isys:fork)))
        (case pid
          (0 (%exec stdin-read
                    stdin-write
                    stdout-read
                    stdout-write
                    file
                    args
                    directory
                    slave-pty-name
                    environment
                    terminal))
          (t
           (isys:close stdin-read)
           (isys:close stdout-write)
           (values pid stdout-read stdin-write)))))))


;;;;
;;;; TCP-CONNECTION/IOLIB
;;;;

(defclass tcp-connection/iolib (tcp-connection-mixin iolib-connection)
  ((socket :accessor connection-socket)))

(defmethod initialize-instance :after ((instance tcp-connection/iolib) &key)
  (with-slots (read-fd write-fd socket host port) instance
    (connection-note-event instance :initialized)
    (unless (or read-fd write-fd)
      (setf socket
            (iolib.sockets:make-socket :address-family :ipv4
                                       :connect :active
                                       :type :stream
                                       :remote-host host
                                       :remote-port port))
      (setf read-fd (iolib.sockets:socket-os-fd socket))
      (setf write-fd (iolib.sockets:socket-os-fd socket)))
    (set-iolib-handlers instance)
    (note-connected instance)))

;;;
;;; PIPELIKE-CONNECTION/IOLIB
;;;

(defclass pipelike-connection/iolib
    (pipelike-connection-mixin iolib-connection)
  ())

(defmethod initialize-instance
    :after
    ((instance pipelike-connection/iolib) &key)
  (connection-note-event instance :initialized)
  (set-iolib-handlers instance))


;;;
;;; PROCESS-WITH-PTY-CONNECTION/IOLIB
;;;

(defclass process-with-pty-connection/iolib
    (process-with-pty-connection-mixin pipelike-connection/iolib)
  ())

;;; Once nothing has the terminal's other side open, reading this side
;;; fails with EIO on Linux, where macOS reads nothing: both are its end.
(defmethod %read ((connection process-with-pty-connection/iolib))
  (handler-case (call-next-method)
    (isys:eio () :eof)))

(defmethod connection-signal ((connection process-with-pty-connection/iolib) signal)
  ;; The terminal's foreground job: a shell's command, or the shell.
  (let ((group (cffi:foreign-funcall "tcgetpgrp" :int (connection-read-fd connection) :int)))
    (if (plusp group)
        (signal-group group signal)
        (connection-signal (connection-process-connection connection) signal))))

(defmethod note-process-ended ((connection process-with-pty-connection/iolib))
  (reap-later (connection-process-connection connection)))



;;;;
;;;; LISTENING-CONNECTION/IOLIB
;;;;

(defclass listening-connection/iolib (listening-connection)
  ((socket :accessor connection-socket)
   (fd :initform nil
       :accessor connection-fd)))

(defmethod initialize-instance :after
    ((instance listening-connection/iolib) &key)
  (with-slots (fd socket host port) instance
    (unless fd
      (connection-note-event instance :initialized)
      (let ((addr (if host
                      (iolib.sockets:ensure-hostname host)
                      iolib.sockets:+ipv4-unspecified+)))
        (setf socket
              (flet ((doit (port)
                       (iolib.sockets:make-socket :address-family :ipv4
                                                  :connect :passive
                                                  :type :stream
                                                  :local-host addr
                                                  :local-port port)))
                (if port
                    ;; Port 0 is one the system chooses, read back.
                    (let ((socket (doit port)))
                      (when (zerop port)
                        (setf port (iolib.sockets:local-port socket)))
                      socket)
                    (iter:iter (iter:for p from 1024 below 65536)
                               (handler-case
                                   (doit p)
                                 (:no-error (socket)
                                   (setf port p)
                                   (return socket))
                                 (error (c) (warn "~A" c))))))))
      (setf fd (iolib.sockets:socket-os-fd socket)))
    (set-iolib-server-handlers instance)))

(defun set-iolib-server-handlers (instance)
  (iolib:set-io-handler
   *event-base*
   (connection-fd instance)
   :read
   (lambda (.fd event error)
     (declare (ignore event))
     (when (eq error :error) (error "error with ~A" .fd))
     (process-incoming-connection instance))))

(defmethod delete-connection :before ((connection listening-connection/iolib))
  ;; Forgotten by the event loop first, as any connection's descriptors
  ;; are: left watching a closed one, its next select(2) fails with EBADF.
  (when (connection-fd connection)
    (iolib:remove-fd-handlers *event-base* (connection-fd connection)))
  (close (connection-socket connection)))

(defmethod (setf connection-fd)
    :after
    ((newval t) (connection listening-connection/iolib))
  (set-iolib-server-handlers connection))

(defun %tcp-connection-from-fd (name fd host port initargs)
  (apply #'make-instance
         'tcp-connection/iolib
         :name name
         :fd fd
         :host host
         :port port
         initargs))


;;;;
;;;; TCP-LISTENER/IOLIB
;;;;

(defclass tcp-listener/iolib (tcp-listener-mixin listening-connection/iolib)
  ())

(defmethod initialize-instance :after ((instance tcp-listener/iolib) &key)
  ;; ...
  )

(defmethod convert-pending-connection ((connection tcp-listener/iolib))
  (iolib.sockets::with-sockaddr-storage-and-socklen (ss size)
    (let* ((socket (connection-socket connection))
           (fd (iolib.sockets::%accept (iolib.streams:fd-of socket) ss size)))
      (multiple-value-bind (host port)
          (iolib.sockets::sockaddr-storage->sockaddr ss)
        (%tcp-connection-from-fd
         (format nil "Accepted for: ~A" (connection-name connection))
         fd
         host
         port
         (connection-initargs connection))))))

#+(or)
(trace connection-write
       %read
       isys:read
       isys:write)
