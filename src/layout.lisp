;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;;; Window layout: how a device's windows tile its screen.
;;;;
;;;; The windows, other than the echo area's, share a rectangle of lines and
;;;; columns, divided as a tree.  A leaf is a window's hunk; a split divides
;;;; its space among its children, one above another (:ROWS) or side by side
;;;; (:COLUMNS), each child having a size in that direction.  Side by side
;;;; windows are divided by a column that neither owns, where the device
;;;; draws a separator.
;;;;
;;;; Splitting, deleting and enlarging change the tree, and APPLY-LAYOUT then
;;;; places every hunk from it: its lines, its columns, the size of its
;;;; window's image, and its place in the ring of hunks that NEXT-WINDOW
;;;; walks, which is the tree's order.  A device only has to draw a hunk
;;;; where it says it is.

(in-package :hemlock-internals)

(defstruct (layout-split (:constructor make-layout-split (direction children sizes)))
  direction                             ; :ROWS or :COLUMNS
  children                              ; hunks and splits
  sizes)                                ; each child's lines or columns

(defstruct (layout (:constructor make-layout (root top left rows columns)))
  root                                  ; a hunk or a split
  top left rows columns)                ; the rectangle the windows share

(defconstant +min-window-columns+ 4)

;;; The fewest lines (DIRECTION :ROWS) or columns NODE can have.
;;;
(defun layout-min (node direction)
  (etypecase node
    (device-hunk
     (ecase direction
       (:rows (if (device-hunk-modelinep node) 2 1))
       (:columns +min-window-columns+)))
    (layout-split
     (let ((mins (mapcar (lambda (child) (layout-min child direction))
                         (layout-split-children node))))
       (if (eq direction (layout-split-direction node))
           (+ (reduce #'+ mins)
              (if (eq direction :columns) (1- (length mins)) 0))
           (reduce #'max mins))))))

;;; SIZES made to add up to TOTAL, each keeping its share and none below its
;;; entry in MINS.  When TOTAL is too small for that, the last ones give way.
;;;
(defun fit-sizes (sizes total mins)
  (let ((old (reduce #'+ sizes)))
    (if (= old total)
        sizes
        (let ((new (mapcar (lambda (size min)
                             (max min (floor (* size total) (max old 1))))
                           sizes mins)))
          ;; Rounding down leaves some over: the last child takes it.
          (let ((over (- total (reduce #'+ new))))
            (when (plusp over)
              (incf (car (last new)) over)))
          ;; The mins can make too many: take them from the largest.
          (loop for excess = (- (reduce #'+ new) total)
                while (plusp excess)
                do (let* ((spare (mapcar #'- new mins))
                          (i (position (reduce #'max spare) spare)))
                     (if (plusp (nth i spare))
                         (decf (nth i new))
                         (return))))
          new))))

;;; Place NODE and everything in it in the rectangle given.
;;;
(defun place-layout (node top left rows columns)
  (etypecase node
    (device-hunk (place-hunk node top left rows columns))
    (layout-split
     (let* ((direction (layout-split-direction node))
            (children (layout-split-children node))
            (columnsp (eq direction :columns))
            (total (if columnsp
                       (- columns (1- (length children)))
                       rows))
            (sizes (fit-sizes (layout-split-sizes node) total
                              (mapcar (lambda (child) (layout-min child direction))
                                      children)))
            (offset 0))
       (setf (layout-split-sizes node) sizes)
       (loop for child in children
             for size in sizes
             do (if columnsp
                    (place-layout child top (+ left offset) rows size)
                    (place-layout child (+ top offset) left size columns))
                (incf offset (if columnsp (1+ size) size)))))))

;;; A hunk's POSITION is its bottom line, which is its modeline when it has
;;; one, and its text ends at TEXT-POSITION.  The window's image follows.
;;;
(defun place-hunk (hunk top left rows columns)
  (let ((text-height (if (device-hunk-modelinep hunk) (1- rows) rows))
        (window (device-hunk-window hunk)))
    (setf (device-hunk-height hunk) rows
          (device-hunk-position hunk) (+ top rows -1)
          (device-hunk-text-height hunk) text-height
          (device-hunk-text-position hunk) (+ top text-height -1)
          (device-hunk-column hunk) left
          (device-hunk-width hunk) columns)
    (when window
      (unless (eql (window-height window) text-height)
        (change-window-image-height window text-height))
      (unless (eql (window-width window) columns)
        (change-window-image-width window columns)))))

(defun layout-hunks (node)
  "The hunks in NODE, in order: top to bottom, and left to right."
  (etypecase node
    (device-hunk (list node))
    (layout-split (mapcan #'layout-hunks (copy-list (layout-split-children node))))))

(defun device-window-hunks (device)
  "DEVICE's window hunks in layout order; the echo area's is not among them."
  (layout-hunks (layout-root (device-layout device))))

(defun apply-layout (device)
  (let ((layout (device-layout device)))
    (place-layout (layout-root layout)
                  (layout-top layout) (layout-left layout)
                  (layout-rows layout) (layout-columns layout))
    ;; The ring of hunks, in the tree's order.
    (let ((hunks (device-window-hunks device)))
      (loop for (hunk next) on (append hunks (list (first hunks)))
            while next
            do (setf (device-hunk-next hunk) next
                     (device-hunk-previous next) hunk))
      (setf (device-hunks device) (first hunks)))
    (setf *screen-image-trashed* t)))

(defun init-layout (device hunk top left rows columns)
  "Make HUNK the only window of DEVICE, filling the rectangle given."
  (setf (device-layout device) (make-layout hunk top left rows columns))
  (apply-layout device))

(defun layout-parent (node root)
  (labels ((walk (split)
             (when (layout-split-p split)
               (if (member node (layout-split-children split))
                   split
                   (some #'walk (layout-split-children split))))))
    (walk root)))

(defun replace-layout-node (device old new)
  "Put NEW where OLD is in DEVICE's layout."
  (let* ((layout (device-layout device))
         (parent (layout-parent old (layout-root layout))))
    (if parent
        (setf (layout-split-children parent)
              (substitute new old (layout-split-children parent)))
        (setf (layout-root layout) new))))

;;; Divide HUNK's space between it and NEW-HUNK, which goes below it or to
;;; its right and gets PROPORTION of the room.  NIL when there is not room
;;; for both.
;;;
(defun layout-split-hunk (device hunk direction proportion new-hunk)
  (let* ((columnsp (eq direction :columns))
         (room (if columnsp
                   (1- (device-hunk-width hunk))
                   (device-hunk-height hunk)))
         (new-size (truncate (* room proportion)))
         (old-size (- room new-size)))
    (when (and (>= new-size (layout-min new-hunk direction))
               (>= old-size (layout-min hunk direction)))
      (let ((parent (layout-parent hunk (layout-root (device-layout device)))))
        (if (and parent (eq (layout-split-direction parent) direction))
            ;; A split the same way: the new window is its next child.
            (let ((i (position hunk (layout-split-children parent))))
              (setf (layout-split-children parent)
                    (append (subseq (layout-split-children parent) 0 (1+ i))
                            (list new-hunk)
                            (nthcdr (1+ i) (layout-split-children parent)))
                    (layout-split-sizes parent)
                    (append (subseq (layout-split-sizes parent) 0 i)
                            (list old-size new-size)
                            (nthcdr (1+ i) (layout-split-sizes parent)))))
            (replace-layout-node
             device hunk
             (make-layout-split direction (list hunk new-hunk)
                                (list old-size new-size)))))
      (apply-layout device)
      t)))

;;; HUNK's space goes to the window before it in its split, or after it when
;;; it is the first.  A split left with one child is replaced by it.
;;;
(defun layout-delete-hunk (device hunk)
  (let* ((layout (device-layout device))
         (parent (layout-parent hunk (layout-root layout))))
    (unless parent
      (editor-error "Cannot delete the only window."))
    (let* ((children (layout-split-children parent))
           (sizes (layout-split-sizes parent))
           (i (position hunk children))
           (heir (if (plusp i) (1- i) 1)))
      (incf (nth heir sizes)
            (+ (nth i sizes)
               (if (eq (layout-split-direction parent) :columns) 1 0)))
      (setf (layout-split-children parent) (remove hunk children)
            (layout-split-sizes parent)
            (append (subseq sizes 0 i) (nthcdr (1+ i) sizes))))
    (when (= 1 (length (layout-split-children parent)))
      (let* ((only (first (layout-split-children parent)))
             (grandparent (layout-parent parent (layout-root layout))))
        (if (and grandparent
                 (layout-split-p only)
                 (eq (layout-split-direction only)
                     (layout-split-direction grandparent)))
            ;; The same way as the split above it: its children join that.
            (let ((j (position parent (layout-split-children grandparent))))
              (setf (layout-split-children grandparent)
                    (append (subseq (layout-split-children grandparent) 0 j)
                            (layout-split-children only)
                            (nthcdr (1+ j) (layout-split-children grandparent)))
                    (layout-split-sizes grandparent)
                    (append (subseq (layout-split-sizes grandparent) 0 j)
                            (layout-split-sizes only)
                            (nthcdr (1+ j) (layout-split-sizes grandparent)))))
            (replace-layout-node device parent only))))
    (apply-layout device)))

;;; OFFSET more lines or columns for HUNK, from the window next to it in the
;;; nearest split that way.
;;;
(defun layout-enlarge-hunk (device hunk offset direction)
  (let* ((root (layout-root (device-layout device)))
         (node hunk)
         (parent (loop for p = (layout-parent node root)
                       while p
                       when (eq (layout-split-direction p) direction)
                         return p
                       do (setf node p))))
    (unless parent
      (editor-error (if (eq direction :columns)
                        "No window beside this one."
                        "No window above or below this one.")))
    (let* ((children (layout-split-children parent))
           (sizes (layout-split-sizes parent))
           (i (position node children))
           (j (if (< (1+ i) (length children)) (1+ i) (1- i))))
      (unless (and (>= (+ (nth i sizes) offset) (layout-min node direction))
                   (>= (- (nth j sizes) offset) (layout-min (nth j children) direction)))
        (editor-error "Not enough room."))
      (incf (nth i sizes) offset)
      (decf (nth j sizes) offset))
    (apply-layout device)))

;;; The screen is now LINES by COLUMNS.  The windows share what the echo
;;; area, at the bottom with its own modeline, leaves.
;;;
(defun resize-device-layout (device lines columns)
  (let ((layout (device-layout device))
        (echo (window-hunk *echo-area-window*)))
    (setf (layout-rows layout) (- lines (device-hunk-height echo) 1)
          (layout-columns layout) columns)
    (setf (device-hunk-position echo) (1- lines)
          (device-hunk-text-position echo) (- lines 2)
          (device-hunk-width echo) columns)
    (unless (eql (window-width *echo-area-window*) columns)
      (change-window-image-width *echo-area-window* columns))
    (apply-layout device)))


;;;; The window methods every device shares.

(defmethod device-make-window ((device device) start modelinep proportion direction)
  (let ((old-hunk (window-hunk (current-window)))
        (new-hunk (device-make-hunk device)))
    (setf (device-hunk-modelinep new-hunk) modelinep)
    (when (layout-split-hunk device old-hunk direction proportion new-hunk)
      (let ((window (internal-make-window :hunk new-hunk)))
        (setf (device-hunk-window new-hunk) window)
        (setup-window-image start window (device-hunk-text-height new-hunk)
                            (device-hunk-width new-hunk))
        (when modelinep
          (setup-modeline-image (line-buffer (mark-line start)) window))
        window))))

(defmethod device-delete-window ((device device) window)
  (let ((buffer (window-buffer window)))
    (setf (buffer-windows buffer) (delete window (buffer-windows buffer))))
  (layout-delete-hunk device (window-hunk window)))

(defmethod device-enlarge-window ((device device) window offset)
  (layout-enlarge-hunk device (window-hunk window) offset :rows))

(defmethod device-next-window ((device device) window)
  (device-hunk-window (device-hunk-next (window-hunk window))))

(defmethod device-previous-window ((device device) window)
  (device-hunk-window (device-hunk-previous (window-hunk window))))
