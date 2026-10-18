;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Prelimiary Undo
;;;   Created: 2004-12-26
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;;   License: MIT style (see below)
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2004 by Gilbert Baumann

;;;  Permission is hereby granted, free of charge, to any person obtaining
;;;  a copy of this software and associated documentation files (the
;;;  "Software"), to deal in the Software without restriction, including
;;;  without limitation the rights to use, copy, modify, merge, publish,
;;;  distribute, sublicense, and/or sell copies of the Software, and to
;;;  permit persons to whom the Software is furnished to do so, subject to
;;;  the following conditions:
;;;
;;;  The above copyright notice and this permission notice shall be
;;;  included in all copies or substantial portions of the Software.
;;;
;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;;; TODO

;; - Some form of consolidation
;; - Ensure that what we indentified as a protocol of functions
;;   modifying a buffer is not violated. Both by runtime and compile
;;   time measures.
;; - Look and Feel:
;;   Find out what different variants of undo are implemented both
;;   with gnu emacs and xemacs, try to simulate those.
;; - Q: Are there any commands that modify more than one buffer?
;;   (Besides the kill ring and stuff)
;; - FILTER-REGION (function region) is missing
;; - (SETF NEXT-CHARACTER) (character mark) is missing
;; - Don't record undo information of functions that don't modify the
;;   buffer at all.

;; There is this MODIFYING-BUFFER which should enable access to
;; LINE-NEXT and LINE-PREVIOUS.

;; Instead of putting :command entires onto an undo list, do
;; undo-chunks directly.

;; Q: Do we need a separate Redo command or should we go the route
;;    that XEmacs follows and make any other command than undo change
;;    the current undo sequence and let subsequent undos really redo
;;    stuff? This is useful but awkward.

;; Also: I really want the self-insert-commands be grouped by words
;; and not just by 20 characters as the [documented] behaviour of
;; XEmacs. This also is the observed behavior.

(in-package :heml-internals)

;; Unfortunately we need numeric buffer positions. (Hmm, maybe after
;; all RMS has a point?) Anyhow to graft this onto heml we define
;; two functions MARK-POSITION and POSTION-MARK to convert and back
;; and fro. Further these new kind of buffer positions are passed
;; around as (buffer line-number character-position) triples.

(defun mark-position (mark)
  (let ((line-no 0)
        (line (mark-line mark)))
    (do ()
        ((null (line-previous line)))
      (incf line-no)
      (setf line (line-previous line)))
    (list (line-buffer (mark-line mark))
          line-no (mark-charpos mark))))

(defun position-mark (buffer line-no char-pos)
  (let ((line (mark-line (buffer-start-mark buffer))))
    (assert line)
    (dotimes (i line-no)
      (let ((next (line-next line)))
        (unless next
          (error "Corrupted undo position, line ~D column ~D"
                 line-no char-pos))
        (setf line next)))
    (mark line char-pos)))

;;;; Insertion

(defun update-tag-line-number (mark)
  (let* ((line (mark-line mark))
         (buffer (line-buffer line)))
    (assert buffer)
    (setf (buffer-tag-line-number buffer)
          (min (buffer-tag-line-number buffer)
               (line-number line)))))

;;; Insert can call itself and other functions.  We only want to recorder
;;; the outermost call.
(defvar *insert-noted-p* nil)

(defmethod insert-character :around (mark character)
  (let ((buffer (line-buffer (mark-line mark))))
    (cond ((and buffer (buffer-undo-p buffer))
           (unless *insert-noted-p*
             (push `(insert-string ,(mark-position mark)
                                   ,(string character))
                   (buffer-undo-list buffer))
             (update-tag-line-number mark))
           (let ((*insert-noted-p* t))
             (call-next-method)))
          (t
           (when buffer
             (update-tag-line-number mark))
           (call-next-method)))))

(defmethod insert-string :around (mark string &optional (start 0) (end (length string)))
  (let ((buffer (line-buffer (mark-line mark))))
    (cond ((and buffer (buffer-undo-p buffer))
           (unless *insert-noted-p*
             (push `(insert-string ,(mark-position mark)
                                   ,(subseq string start end))
                   (buffer-undo-list buffer))
             (update-tag-line-number mark))
           (let ((*insert-noted-p* t))
             (call-next-method)))
          (t
           (when buffer
             (update-tag-line-number mark))
           (call-next-method)))))

(defmethod insert-region :around (mark region)
  (let ((buffer (line-buffer (mark-line mark))))
    (cond ((and buffer (buffer-undo-p buffer))
           (unless *insert-noted-p*
             (push `(insert-string ,(mark-position mark)
                                   ,(region-to-string region))
                   (buffer-undo-list buffer))
             (update-tag-line-number mark))
           (let ((*insert-noted-p* t))
             (call-next-method)))
          (t
           (when buffer
             (update-tag-line-number mark))
           (call-next-method)))))

(defmethod ninsert-region :around (mark region)
  ;; the "n" refers to the region argument.
  (let ((buffer (line-buffer (mark-line mark))))
    (cond ((and buffer (buffer-undo-p buffer))
           (unless *insert-noted-p*
             (push `(insert-region ,(mark-position mark)
                                   ,(region-to-string region))
                   (buffer-undo-list buffer))
             (update-tag-line-number mark))
           (let ((*insert-noted-p* t))
             (call-next-method)))
          (t
           (when buffer
             (update-tag-line-number mark))
           (call-next-method)))))

;;;; Deletion

;; We make delete-characters and delete-region both call off to
;; delete-and-save-region which is the most general method and has the
;; benefit to return the deleted stuff.

(defmethod delete-characters :around (mark &optional (n 1))
  (let ((buffer (line-buffer (mark-line mark))))
    (cond ((and buffer (buffer-undo-p buffer))
           ;; For now delete-characters just calls delete-region in
           ;; any case.  code borrowed from htext4.lisp
           (let* ((line (mark-line mark))
                  (charpos (mark-charpos mark))
                  (length (line-length* line)))
             (setf (mark-line *internal-temp-mark*) line
                   (mark-charpos *internal-temp-mark*) charpos)
             (let ((other-mark (character-offset *internal-temp-mark* n)))
               (cond
                 (other-mark
                  (if (< n 0)
                      (setf (region-start *internal-temp-region*) other-mark
                            (region-end *internal-temp-region*) mark)
                      (setf (region-start *internal-temp-region*) mark
                            (region-end *internal-temp-region*) other-mark))
                  (delete-and-save-region *internal-temp-region*)
                  t)
                 (t nil)))))
          (t
           (when buffer
             (update-tag-line-number mark))
           (call-next-method)))))
           

(defmethod delete-region :around (region)
  (let* ((mark (region-start region))
         (buffer (line-buffer (mark-line mark))))
    (cond ((and buffer (buffer-undo-p buffer))
           (delete-and-save-region region))
          (t
           (when buffer
             (update-tag-line-number mark))
           (call-next-method)))))

(defmethod delete-and-save-region :around (region)
  (let* ((mark (region-start region))
         (buffer (line-buffer (mark-line mark))))
    (cond ((and buffer (buffer-undo-p buffer))
           (let ((pos (mark-position mark))
                 (matter (call-next-method)))
             (push `(delete-region ,pos ,(region-to-string matter))
                   (buffer-undo-list buffer))
             (update-tag-line-number mark)
             matter))
          (t
           (when buffer
             (update-tag-line-number mark))
           (call-next-method)))))

;;;;

;;; Undoing and redoing, as a Mac application does.  An undo or a redo is a
;;; command whose changes are on the undo list too, but undoing never undoes
;;; them: each such command's group (known by its (:COMMAND ...) cell) is
;;; recorded in *UNDO-EQUIVALENTS* with the place in the list whose state it
;;; left the buffer in, and undoing goes on from there.  So C-_ again and
;;; again goes back command by command, and "Redo" does again, latest first,
;;; what was undone, until something else changes the buffer.

;;; A group's (:COMMAND ...) cell to (after . before): where undoing goes on
;;; from after it, and for an undo, where it went on from before it.
(defvar *undo-equivalents* (make-hash-table :test 'eq :weakness :key))

;;; A buffer to (undos . top): the undos "Redo" can do again, latest first,
;;; each (top-of-its-group . its-command-cell), and the top of the undo list
;;; when the last undo or redo ended.
(defvar *redo-states* (make-hash-table :test 'eq :weakness :key))

(defun undo-change-p (entry buffer)
  (and (member (car entry) '(insert-string insert-region delete-region))
       (consp (cadr entry))
       (eq (car (cadr entry)) buffer)))

(defun undo-group-end (cell)
  "The (:COMMAND ...) cell that ends the group CELL is in, or NIL."
  (loop for c on cell
        when (and (consp (car c)) (eq (caar c) :command))
          return c))

(defun undoable-position (cell buffer)
  "Where undoing goes on from CELL: past the groups that undid or redid, to
  the state each left the buffer in, and past those that changed nothing."
  (loop
    (let* ((end (undo-group-end cell))
           (equivalent (and end (gethash end *undo-equivalents*))))
      (cond (equivalent
             (setf cell (car equivalent)))
            ((or (null end)
                 (loop for c on cell until (eq c end)
                       thereis (undo-change-p (car c) buffer)))
             (return cell))
            (t
             (setf cell (cdr end)))))))

(defun revert-undo-entries (cell end buffer &key restore-point)
  "Undoes the changes to BUFFER recorded from CELL to END, latest first,
  leaving point at the last, or with RESTORE-POINT where it was before them.
  Returns true when it changed the buffer."
  (let ((changed nil)
        (point (current-point)))
    (loop for c on cell until (eq c end)
          for entry = (car c)
          when (and (consp (cadr entry)) (eq (car (cadr entry)) buffer))
            do (case (car entry)
                 ((insert-string insert-region)
                  (let* ((matter (caddr entry))
                         (n (length matter))
                         (mark (apply #'position-mark (cadr entry))))
                    ;; Firstly double check that the characters being
                    ;; deleted are as expected.
                    (with-mark ((check mark))
                      (dotimes (i n)
                        (unless (eql (next-character check) (char matter i))
                          (error "Undo lost sync at line ~D column ~D"
                                 (second (cadr entry)) (third (cadr entry))))
                        (mark-after check)))
                    (delete-characters mark n)
                    (move-mark point mark)
                    (setf changed t)))
                 (delete-region
                  (let ((mark (apply #'position-mark (cadr entry))))
                    (move-mark point mark)
                    (insert-string mark (caddr entry))
                    (setf changed t)))
                 (point-position
                  (when restore-point
                    (move-mark point (apply #'position-mark (cadr entry)))))))
    changed))

(defun undo-own-group (buffer)
  "This command's (:COMMAND ...) cell, which the invoke hook pushed with the
  point before it."
  (unless (buffer-undo-p buffer)
    (editor-error "Undo is off in this buffer."))
  (let ((own (cdr (buffer-undo-list buffer))))
    (assert (and (consp (car own)) (eq (caar own) :command)))
    own))

(defun redo-state (buffer)
  (or (gethash buffer *redo-states*)
      (setf (gethash buffer *redo-states*) (cons nil nil))))

(defun forget-overtaken-redos (buffer own)
  "Forgets what \"Redo\" could do in BUFFER when anything else has changed it
  since the last undo or redo."
  (let ((state (redo-state buffer)))
    (unless (loop for c on (cdr own)
                  when (eq c (cdr state)) return t
                  never (undo-change-p (car c) buffer))
      (setf (car state) nil))
    state))

(defun undone-command-name (end)
  (let ((command (and end (cadr (car end)))))
    (if (commandp command) (command-name command) "the change")))

(defcommand "New Undo" (p)
  "Undoes the last command that changed the buffer, and again the one before
  that.  \"Redo\" does again what was undone."
  "Undoes the last command that changed the current buffer."
  (declare (ignore p))
  (let* ((buffer (current-buffer))
         (own (undo-own-group buffer))
         (state (forget-overtaken-redos buffer own))
         (start (undoable-position (cdr own) buffer))
         (end (undo-group-end start)))
    (unless (revert-undo-entries start end buffer :restore-point t)
      (editor-error "No further undo information."))
    (setf (gethash own *undo-equivalents*) (cons (cdr end) start))
    (push (cons (buffer-undo-list buffer) own) (car state))
    (setf (cdr state) (buffer-undo-list buffer))
    (message "Undid ~A." (undone-command-name end))))

(defcommand "Redo" (p)
  "Does again what the last undo undid, and again the undo before that, until
  something else changes the buffer."
  "Does again what the last undo undid."
  (declare (ignore p))
  (let* ((buffer (current-buffer))
         (own (undo-own-group buffer))
         (state (forget-overtaken-redos buffer own))
         (undo (pop (car state))))
    (unless undo
      (editor-error "Nothing to redo."))
    (destructuring-bind (top . undo-own) undo
      (revert-undo-entries top undo-own buffer)
      ;; Undoing now goes on from where that undo did.
      (setf (gethash own *undo-equivalents*)
            (list (cdr (gethash undo-own *undo-equivalents*))))
      (setf (cdr state) (buffer-undo-list buffer))
      (message "Redid ~A."
               (undone-command-name
                (undo-group-end (cdr (gethash undo-own *undo-equivalents*))))))))

(defun new-undo-invoke-hook (command p)
  (declare (ignore p))
  (let ((buffer (current-buffer)))
    (when (and buffer (buffer-undo-p buffer))
      (push (list :command command) (buffer-undo-list buffer))
      (push (list 'point-position (mark-position (current-point)))
            (buffer-undo-list buffer))))
  nil)

(defparameter *invoke-hook* #'(lambda (command p)
                                (new-undo-invoke-hook command p)
                                (funcall (command-function command) p))
  "This function is called by the command interpreter when it wants to invoke a
  command.  The arguments are the command to invoke and the prefix argument.
  The default value just calls the Command-Function with the prefix argument.")


(defcommand "Undo Mode" (p)
  "Enable the recording of Undo information in the current buffer."
  "Enable the recording of Undo information in the current buffer."
  (let* ((buffer (current-buffer))
         (p (if p (plusp p) (not (buffer-undo-p buffer)))))
    (setf (buffer-undo-p buffer) p)
    (setf (buffer-undo-list buffer) nil)
    (if p
        (message "Undo enabled")
        (message "Undo disabled"))))

