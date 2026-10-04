;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;;    This file contains definitions of various types of streams used
;;; in Heml.  They are implementation dependant, but should be
;;; portable to all implementations based on Spice Lisp with little
;;; difficulty.
;;;
;;; Written by Skef Wholey and Rob MacLachlan.
;;;

(in-package :heml-internals)

;;; Note: although this stream is intended for output only it also supports
;;; input to help if the debugger is called.
(defclass heml-output-stream (hi::trivial-gray-stream-mixin
                                 hi::fundamental-character-output-stream
                                 hi::fundamental-character-input-stream)
  ((mark
    :initform nil
    :accessor heml-output-stream-mark
    :documentation "The mark we insert at.")
   (input-string
    :initform nil)
   (input-pos
    :initform 0)
   (out
    :accessor old-lisp-stream-out)
   (sout
    :accessor old-lisp-stream-sout)))

(defun heml-output-stream-p (x)
  (typep x 'heml-output-stream))

(defmethod hi::stream-write-char ((stream heml-output-stream) char)
  (funcall (old-lisp-stream-out stream) stream char))

(defmethod hi::stream-write-sequence
    ((stream heml-output-stream) seq start end &key)
  (check-type seq string)
  (heml-output-buffered-sout stream seq start end))


(defmethod hi::stream-line-column ((stream heml-output-stream))
  (mark-charpos (heml-output-stream-mark stream)))

(defmethod hi::stream-line-length ((stream heml-output-stream))
  (mark-charpos (heml-output-stream-mark stream))
  (let* ((buffer
          (line-buffer (mark-line (heml-output-stream-mark stream)))))
    (when buffer
      (do ((w (buffer-windows buffer) (cdr w))
           (min most-positive-fixnum (min (window-text-width (car w)) min)))
          ((null w)
           (if (/= min most-positive-fixnum) min))))))

(defmethod print-object ((object heml-output-stream) stream)
  (write-string "#<Heml output stream>" stream))

(defun make-heml-output-stream (mark &optional (buffered :line))
  "Returns an output stream whose output will be inserted at the Mark.
  Buffered, which indicates to what extent the stream may be buffered
  is one of the following:
   :None  -- The screen is brought up to date after each stream operation.
   :Line  -- The screen is brought up to date when a newline is written.
   :Full  -- The screen is not updated except explicitly via Force-Output."
  (modify-heml-output-stream (make-instance 'heml-output-stream)
                                mark
                                buffered))


;;; Note: this is called when re-using a stream and is expected to
;;; re-initialize the stream.
(defun modify-heml-output-stream (stream mark buffered)
  (unless (and (markp mark)
               (member (mark-kind mark) '(:right-inserting :left-inserting)))
    (error "~S is not a permanent mark." mark))
  (setf (heml-output-stream-mark stream) mark)
  ;;
  ;; Free the current stream buffers, resetting the buffer pointers.
  ;;
  (case buffered
    (:none
     (setf (old-lisp-stream-out stream) #'heml-output-unbuffered-out
           (old-lisp-stream-sout stream) #'heml-output-unbuffered-sout))
    (:line
     (setf (old-lisp-stream-out stream) #'heml-output-line-buffered-out
           (old-lisp-stream-sout stream) #'heml-output-line-buffered-sout))
    (:full
     (setf (old-lisp-stream-out stream) #'heml-output-buffered-out
           (old-lisp-stream-sout stream) #'heml-output-buffered-sout))
    (t
     (error "~S is a losing value for Buffered." buffered)))
  stream)

(defmacro with-left-inserting-mark ((var form) &body forms)
  (let ((change (gensym)))
    `(let* ((,var ,form)
            (,change (eq (mark-kind ,var) :right-inserting)))
       (unwind-protect
           (progn
             (when ,change
               (setf (mark-kind ,var) :left-inserting))
             ,@forms)
         (when ,change
           (setf (mark-kind ,var) :right-inserting))))))

(defun heml-output-unbuffered-out (stream character)
  (with-left-inserting-mark (mark (heml-output-stream-mark stream))
    (insert-character mark character)
    (redisplay-windows-from-mark mark t)))

(defun heml-output-unbuffered-sout (stream string start end)
  (with-left-inserting-mark (mark (heml-output-stream-mark stream))
    (insert-string mark string start end)
    (redisplay-windows-from-mark mark t)))

(defun heml-output-buffered-out (stream character)
  (with-left-inserting-mark (mark (heml-output-stream-mark stream))
    (insert-character mark character)))

(defun heml-output-buffered-sout (stream string start end)
  (with-left-inserting-mark (mark (heml-output-stream-mark stream))
    (insert-string mark string start end)))

(defun heml-output-line-buffered-out (stream character)
  (with-left-inserting-mark (mark (heml-output-stream-mark stream))
    (insert-character mark character)
    (when (char= character #\newline)
      (redisplay-windows-from-mark mark t))))

(defun heml-output-line-buffered-sout (stream string start end)
  (declare (simple-string string))
  (with-left-inserting-mark (mark (heml-output-stream-mark stream))
    (insert-string mark string start end)
    (when (find #\newline string :start start :end end)
      (redisplay-windows-from-mark mark t))))

(defmethod stream-finish-output ((stream heml-output-stream))
  (redisplay-windows-from-mark (heml-output-stream-mark stream)))

(defmethod stream-force-output ((stream heml-output-stream))
  (redisplay-windows-from-mark (heml-output-stream-mark stream)))

(defmethod close ((stream heml-output-stream) &key abort)
  (declare (ignore abort))
  (setf (heml-output-stream-mark stream) nil))

(defmethod stream-line-column ((stream heml-output-stream))
  (mark-charpos (heml-output-stream-mark stream)))


;;; Input methods: although called HEML-OUTPUT-STREAM, the following
;;; methods allow the stream to used for input, too.  Don't do this
;;; at home, because it enters the command loop recursively in a potentially
;;; bad way, but it can be very useful for debugging purposes;

(defvar hi::*reading-lispbuf-input* nil)

(defun ensure-output-stream-input (stream)
  (with-slots (input-string input-pos mark) stream
    (do ()
        ((and input-string (< input-pos (length input-string))))
      (setf input-string
            (catch 'hi::lispbuf-input
              (let ((hi::*reading-lispbuf-input* t)
                    (buffer (line-buffer (mark-line mark))))
                (move-mark
                 (variable-value 'heml::buffer-input-mark :buffer buffer)
                 (buffer-point buffer))
                (%command-loop))))
      (check-type input-string string)
      (setf input-pos 0))))

(defmethod stream-read-char ((stream heml-output-stream))
  (ensure-output-stream-input stream)
  (with-slots (input-string input-pos) stream
    (prog1
        (elt input-string input-pos)
      (incf input-pos))))

(defmethod stream-read-char-no-hang ((stream heml-output-stream))
  (with-slots (input-string input-pos) stream
    (when (and input-string (< input-pos (length input-string)))
      (prog1
          (elt input-string input-pos)
        (incf input-pos)))))

(defmethod stream-listen ((stream heml-output-stream))
  (with-slots (input-string input-pos) stream
    (and input-string (< input-pos (length input-string)))))

(defmethod stream-unread-char ((stream heml-output-stream) char)
  (with-slots (input-pos) stream
    (unless (plusp input-pos)
      (error "nothing to unread"))
    (decf input-pos))
  nil)

(defmethod stream-clear-input ((stream heml-output-stream))
  (with-slots (input-string input-pos) stream
    (unless (and input-string (< input-pos (length input-string)))
      (setf input-string nil)))
  nil)

;;; end of input methods, back in sane code


(defclass heml-region-stream (fundamental-character-input-stream)
  ;;
  ;; The region we read from.
  ((region :initarg :region
           :accessor heml-region-stream-region)
   ;;
   ;; The mark pointing to the next character to read.
   (mark :initarg :mark
         :accessor heml-region-stream-mark)) )

(defmethod print-object ((object heml-region-stream) stream)
  (declare (ignorable object))
  (write-string "#<Heml region stream>" stream))

(defun make-heml-region-stream (region)
  "Returns an input stream that will return successive characters from the
  given Region when asked for input."
  (make-instance 'heml-region-stream
                 :region region
                 :mark (copy-mark (region-start region) :right-inserting)))

;;; Note: this is called when re-using a stream and is expected to
;;; re-initialize the stream.
(defun modify-heml-region-stream (stream region)
  (setf (heml-region-stream-region stream) region)
  (let* ((mark (heml-region-stream-mark stream))
         (start (region-start region))
         (start-line (mark-line start)))
    ;; Make sure it's dead.
    (delete-mark mark)
    (setf (mark-line mark) start-line  (mark-charpos mark) (mark-charpos start))
    (push mark (line-marks start-line)))
  ;;
  ;; Reset the buffer pointers.
  ;;
  stream)

(defmethod stream-read-char ((stream heml-region-stream))
  (let ((mark (heml-region-stream-mark stream)))
    (cond ((mark< mark
                  (region-end (heml-region-stream-region stream)))
           (prog1 (next-character mark) (mark-after mark)))
          (t :eof))))

(defmethod stream-listen ((stream heml-region-stream))
  (mark< (heml-region-stream-mark stream)
         (region-end (heml-region-stream-region stream))))

(defmethod stream-unread-char ((stream heml-region-stream) char)
  (let ((mark (heml-region-stream-mark stream)))
    (unless (mark> mark
                   (region-start (heml-region-stream-region stream)))
      (error "Nothing to unread."))
    (unless (char= char (previous-character mark))
      (error "Unreading something not read: ~S" char))
    (mark-before mark))
  nil)

(defmethod stream-clear-input ((stream heml-region-stream))
  (move-mark
   (heml-region-stream-mark stream)
   (region-end (heml-region-stream-region stream)))
  nil)

(defmethod close ((stream heml-region-stream) &key abort)
  (declare (ignorable abort))
  (delete-mark (heml-region-stream-mark stream))
  (setf (heml-region-stream-region stream) nil))


#||
(defmethod excl::stream-file-position ((stream heml-output-stream) &optional pos)
  (assert (null pos))
  (mark-charpos (heml-output-stream-mark stream)))

(defun region-misc (stream operation &optional arg1 arg2)
  (declare (ignore arg2))
  (case operation

    (:file-position
     (let ((start (region-start (heml-region-stream-region stream)))
           (mark (heml-region-stream-mark stream)))
       (cond (arg1
              (move-mark mark start)
              (character-offset mark arg1))
             (t
              (count-characters (region start mark)))))) ))
||#


;;;; Stuff to support keyboard macros.

(defclass kbdmac-stream (editor-input)
  ((buffer :initarg :buffer
           :initform nil
           :accessor kbdmac-stream-buffer
           :documentation "The simple-vector that holds the characters.")
   (index  :initarg :index
           :initform nil
           :accessor kbdmac-stream-index
           :documentation "Index of the next character.")))

(defun make-kbdmac-stream ()
  (make-instance 'kbdmac-stream))

(defmethod get-key-event ((stream kbdmac-stream) &optional ignore-abort-attempts-p)
  (declare (ignore ignore-abort-attempts-p))
  (let ((index (kbdmac-stream-index stream)))
    (setf (kbdmac-stream-index stream) (1+ index))
    (setq *last-key-event-typed*
          (svref (kbdmac-stream-buffer stream) index))))

(defmethod unget-key-event (ignore (stream kbdmac-stream))
  (declare (ignore ignore))
  (if (plusp (kbdmac-stream-index stream))
      (decf (kbdmac-stream-index stream))
      (error "Nothing to unread.")))

(defmethod listen-editor-input ((stream kbdmac-stream))
  (declare (ignore stream))
  t)

;; clear-editor-input ?? --GB

;;; MODIFY-KBDMAC-STREAM  --  Internal
;;;
;;;    Bash the kbdmac-stream Stream so that it will return the Input.
;;;
(defun modify-kbdmac-stream (stream input)
  (setf (kbdmac-stream-index stream) 0)
  (setf (kbdmac-stream-buffer stream) input)
  stream)
