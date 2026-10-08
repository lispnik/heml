;;; -*- Log: heml.log; Package: heml -*-
;;;
;;; A terminal emulator in a buffer.  "Term" runs a shell, or another
;;; program, on a pseudo-terminal of its own (its controlling terminal, so
;;; that ^C, ^Z and a resized window reach it as signals), and libvterm
;;; emulates the terminal it writes to: what it writes goes to
;;; VTERM-INPUT-WRITE, and the screen libvterm keeps is read back, cell by
;;; cell, into the buffer's last lines, with the lines that scroll off its
;;; top kept above them.  So less, top, vim and a progress bar work, as in a
;;; terminal window.
;;;
;;; In Term mode every key goes to the program, as a terminal would send it
;;; (libvterm's VTERM-KEYBOARD-*), but for C-x and M-x, which are Heml's,
;;; and C-c, which starts Heml's terminal commands: C-c C-c sends ^C, C-c
;;; C-x sends ^X, C-c C-j goes to Term Copy mode, where the buffer is read
;;; and copied from as any other, and q or C-c C-k comes back.  C-y pastes,
;;; bracketed, as a terminal does.

(in-package :heml)


;;;; The terminals.

(defstruct (term (:constructor %make-term))
  id buffer connection vt screen state rows columns
  (scrollback 0)                        ; the buffer's lines above the screen
  screen-mark                           ; at the start of the screen's first line
  (pushed '())                          ; lines off the top, newest first
  (last-refresh 0)
  (refresh-pending nil)
  (title nil)
  (exit-code nil)
  cell                                  ; a VTermScreenCell to read into
  rgb                                   ; a VTermColor to convert in
  (dirty nil)                           ; a bit for each row libvterm damaged, or T
  (positions nil)                       ; each row's column positions, last read
  (last-cursor nil))                    ; (ROW . COLUMN) at the last refresh

(defvar *term-rows-skipped* 0
  "Rows a refresh did not read, being as they were: for measuring.")

(defvar *terms* (make-hash-table)
  "Each terminal by its number, which libvterm's callbacks are given.")

(defvar *term-count* 0)

(defun buffer-term (buffer)
  (and (heml-bound-p 'term :buffer buffer)
       (variable-value 'term :buffer buffer)))

(defun current-term ()
  (or (buffer-term (current-buffer))
      (editor-error "Not a terminal.")))

(defhvar "Term Scrollback"
  "How many lines that have scrolled off a terminal's screen its buffer
   keeps above it."
  :value 10000)

(defhvar "Term Program"
  "The program Term runs: NIL for the user's own shell, $SHELL."
  :value nil)


;;;; libvterm's callbacks.  Each is given the terminal's number as its user
;;;; data, and runs inside VTERM-INPUT-WRITE or VTERM-KEYBOARD-*, on the
;;;; editor's thread.

(defun user-term (user)
  (gethash (cffi:pointer-address user) *terms*))

;;; What a key becomes, for the program.
(cffi:defcallback term-output :void ((bytes :pointer) (length :size) (user :pointer))
  (let ((term (user-term user)))
    (when (and term (term-connection term) (not (term-exit-code term)))
      (let ((octets (make-array length :element-type '(unsigned-byte 8))))
        (dotimes (i length)
          (setf (aref octets i) (cffi:mem-aref bytes :uint8 i)))
        (connection-write octets (term-connection term))))))

;;; What a write changed: libvterm's damage, a rectangle of rows and columns
;;; given by value.  A VTermRect is four ints, start_row, end_row,
;;; start_col and end_col, which on arm64 and x86-64 come in two 64-bit
;;; registers: the rows in the first, the columns in the second.  Only the
;;; rows matter -- each is read whole -- and they are marked to be read at
;;; the next refresh.  Lines scrolled are damage too: with no moverect
;;; callback, libvterm damages where they went.
(cffi:defcallback term-damage :int ((rows :uint64) (columns :uint64) (user :pointer))
  (let ((term (user-term user)))
    (when term
      (multiple-value-bind (start end) (vterm:unpack-rect rows columns)
        (mark-term-rows term start end))))
  1)

(defun mark-term-rows (term start end)
  "Mark rows START (inclusive) to END (exclusive) to be read again."
  (let ((dirty (term-dirty term)))
    (unless (eq dirty t)
      (unless (and dirty (= (length dirty) (term-rows term)))
        (setf dirty (make-array (term-rows term) :element-type 'bit :initial-element 0)
              (term-dirty term) dirty))
      (loop for row from (max 0 start) below (min end (term-rows term))
            do (setf (sbit dirty row) 1)))))

(defun mark-term-all (term)
  "Have the next refresh read every row: after a resize, at the start."
  (setf (term-dirty term) t))

(defun row-dirty-p (term row)
  (let ((dirty (term-dirty term)))
    (or (eq dirty t)
        (and dirty (< row (length dirty)) (= 1 (sbit dirty row))))))

;;; A line scrolling off the top of the screen, kept as scrollback.
(cffi:defcallback term-pushline :int ((columns :int) (cells :pointer) (user :pointer))
  (let ((term (user-term user)))
    (when term
      (push (multiple-value-list
             (cells-text term columns cells vterm:+cell-size+ :positionsp nil))
            (term-pushed term))))
  1)

;;; A property: the window's title is kept, and shown in the modeline.
(cffi:defcallback term-settermprop :int ((prop :int) (value :pointer) (user :pointer))
  (let ((term (user-term user)))
    (when (and term (= prop vterm:+prop-title+))
      ;; A string property's VTermValue is its VTermStringFragment.
      (let* ((pointer (vterm:vsf-str value))
             (packed (vterm:vsf-packed value))
             (fragment (if (cffi:null-pointer-p pointer)
                           ""
                           (cffi:foreign-string-to-lisp pointer :count (vterm:vsf-len packed)
                                                                :encoding :utf-8))))
        (setf (term-title term)
              (if (vterm:vsf-initial-p packed)
                  fragment
                  (concatenate 'string (or (term-title term) "") fragment))))))
  1)

(defvar *term-callbacks* nil
  "The VTermScreenCallbacks every terminal's screen is given: libvterm keeps
   the pointer, so it is made once.")

(defun term-callbacks ()
  (or *term-callbacks*
      (let ((callbacks (cffi:foreign-alloc '(:struct vterm:vterm-screen-callbacks))))
        (dotimes (i 9)
          (setf (cffi:mem-aref callbacks :pointer i) (cffi:null-pointer)))
        (setf (vterm:vscb-damage callbacks) (cffi:callback term-damage)
              (vterm:vscb-settermprop callbacks) (cffi:callback term-settermprop)
              (vterm:vscb-sb-pushline callbacks) (cffi:callback term-pushline))
        (setf *term-callbacks* callbacks))))


;;;; Reading the screen.

(defconstant +cell-chars+ 0)
(defparameter *cell-width-offset*
  (cffi:foreign-slot-offset '(:struct vterm:vterm-screen-cell) 'vterm:width))
(defparameter *cell-attrs-offset*
  (cffi:foreign-slot-offset '(:struct vterm:vterm-screen-cell) 'vterm:attrs))
(defparameter *cell-fg-offset*
  (cffi:foreign-slot-offset '(:struct vterm:vterm-screen-cell) 'vterm:fg))
(defparameter *cell-bg-offset*
  (cffi:foreign-slot-offset '(:struct vterm:vterm-screen-cell) 'vterm:bg))

(defun term-color (term color foreground)
  "A VTermColor at COLOR as a font's colour: NIL for the terminal's default,
   an index among xterm's 256 (9, which Heml's palette has for its text's
   colour, as its red), or (RED GREEN BLUE)."
  (cond ((if foreground (vterm:color-default-fg-p color) (vterm:color-default-bg-p color))
         nil)
        ((vterm:color-indexed-p color)
         (let ((index (vterm:vterm-color-index color)))
           ;; Heml's palette is xterm's first nine but for 9, which is its
           ;; text's own colour; past it, xterm's index, which each backend
           ;; knows.
           (if (/= index 9)
               index
               (let ((rgb (term-color-buffer term)))
                 (dotimes (i 4)
                   (setf (cffi:mem-aref rgb :uint8 i) (cffi:mem-aref color :uint8 i)))
                 (vterm:vterm-screen-convert-color-to-rgb (term-screen term) rgb)
                 (list (vterm:vterm-color-red rgb) (vterm:vterm-color-green rgb)
                       (vterm:vterm-color-blue rgb))))))
        (t (list (vterm:vterm-color-red color) (vterm:vterm-color-green color)
                 (vterm:vterm-color-blue color)))))

(defun term-color-buffer (term)
  (or (term-rgb term)
      (setf (term-rgb term) (cffi:foreign-alloc :uint8 :count 4))))

(defun cell-font (term cell)
  "The font a cell is drawn in: 0, or a property list (font numbers are
   colours: see window.lisp)."
  (let* ((attrs (cffi:mem-ref cell :uint32 *cell-attrs-offset*))
         (fg (term-color term (cffi:inc-pointer cell *cell-fg-offset*) t))
         (bg (term-color term (cffi:inc-pointer cell *cell-bg-offset*) nil)))
    (when (vterm:attrs-reverse-p attrs)
      (rotatef fg bg)
      (setf fg (or fg 0) bg (or bg 7)))
    (let ((font (append (when fg (list :fg fg)) (when bg (list :bg bg))
                        (when (vterm:attrs-bold-p attrs) (list :bold t))
                        (when (/= (vterm:attrs-underline attrs) vterm:+underline-off+)
                          (list :underline t))
                        (when (vterm:attrs-italic-p attrs) (list :italic t)))))
      (or font 0))))

(defun blank-cell-p (cells offset)
  "Whether the cell OFFSET bytes into CELLS is a blank in the default font:
   no character, and nothing CELL-FONT would draw it with -- no bold,
   underline, italic or reverse, and the default colours."
  (let ((attrs (cffi:mem-ref cells :uint32 (+ offset *cell-attrs-offset*))))
    (and (zerop (cffi:mem-ref cells :uint32 offset))
         (not (or (vterm:attrs-bold-p attrs) (vterm:attrs-italic-p attrs)
                  (vterm:attrs-reverse-p attrs)
                  (/= (vterm:attrs-underline attrs) vterm:+underline-off+)))
         ;; The colours' type bytes, read in place: no pointer is made.
         (logtest (cffi:mem-ref cells :uint8 (+ offset *cell-fg-offset*))
                  vterm:+color-default-fg+)
         (logtest (cffi:mem-ref cells :uint8 (+ offset *cell-bg-offset*))
                  vterm:+color-default-bg+))))

(defun cells-text (term columns cells stride &key fetch (positionsp t))
  "The text of a row of COLUMNS cells, column N's (* N STRIDE) bytes into
   CELLS -- after (funcall FETCH N), if FETCH is given, which may put it there:
   its characters, its fonts as ((POSITION . FONT) ...), and a vector of where
   each column's character is in the text (unless POSITIONSP is false, for a
   line scrolled off, which has no cursor).  Blanks in the default font at
   its end are left out.  The cells are read at offsets from CELLS, rather
   than each through a pointer of its own: a pointer is an object, and a
   flood of output made millions of them."
  (flet ((cell-offset (column)
           (when fetch (funcall fetch column))
           (* column stride)))
    (let ((text (make-string-output-stream))
          (fonts '()) (font 0) (length 0) (last-ink 0)
          ;; A cell styled as the one before it -- most are -- has its font:
          ;; its attributes and colours, as they are, are compared first.
          (styled nil) (style-attrs 0) (style-fg 0) (style-bg 0)
          (positions (and positionsp (make-array (1+ columns) :initial-element 0)))
          ;; The blanks at the end -- most of a line of output -- are left
          ;; out of the text anyway: only looked at once, from the end, and
          ;; not read again.  They are a column each in POSITIONS.
          (end (loop for column downfrom (1- columns) to 0
                     unless (blank-cell-p cells (cell-offset column))
                       return (1+ column)
                     finally (return 0))))
      (dotimes (column end)
        (let* ((offset (cell-offset column))
               (first (cffi:mem-ref cells :uint32 offset)))
          (when positions (setf (aref positions column) length))
          (unless (= first #xFFFFFFFF)  ; the right half of a wide character
            (let* ((attrs (cffi:mem-ref cells :uint32 (+ offset *cell-attrs-offset*)))
                   (fg (cffi:mem-ref cells :uint32 (+ offset *cell-fg-offset*)))
                   (bg (cffi:mem-ref cells :uint32 (+ offset *cell-bg-offset*)))
                   (this (if (and styled (= attrs style-attrs) (= fg style-fg) (= bg style-bg))
                             font
                             (progn (setf styled t style-attrs attrs style-fg fg style-bg bg)
                                    (cell-font term (cffi:inc-pointer cells offset))))))
              (unless (equal this font)
                (push (cons length this) fonts)
                (setf font this))
              (cond ((zerop first) (write-char #\Space text))
                    (t (write-char (code-char first) text)
                       ;; Combining characters after it.
                       (loop for i from 1 below 6
                             for code = (cffi:mem-ref cells :uint32 (+ offset (* 4 i)))
                             until (zerop code)
                             do (write-char (code-char code) text) (incf length))))
              (incf length)
              (unless (and (zerop first) (eql this 0))
                (setf last-ink length))))))
      (when positions
        (loop for column from end to columns
              do (setf (aref positions column) (+ length (- column end)))))
      (let ((string (get-output-stream-string text)))
        (values (subseq string 0 last-ink)
                (nreverse (remove-if (lambda (entry) (>= (car entry) last-ink)) fonts))
                positions)))))

(defun screen-row-text (term row)
  ;; Each cell read into the one buffer in turn: a stride of 0.
  (let ((cell (term-cell term)) (screen (term-screen term)))
    (cells-text term (term-columns term) cell 0
                :fetch (lambda (column)
                         (vterm:vterm-screen-get-cell screen row column cell)))))


;;;; Drawing it in the buffer: the scrollback, then a line for each row of
;;;; the screen.  A line's fonts are kept in its plist, and the mode's
;;;; highlighter lays them down.

(defun term-highlight-line (line)
  (let ((old (getf (line-plist line) 'term-marks)))
    (unless (and old (eq (car old) (line-signature line)))
      (dolist (mark (cdr old))
        (delete-font-mark mark))
      (setf (getf (line-plist line) 'term-marks)
            (cons (line-signature line)
                  (loop for (position . font) in (getf (line-plist line) 'term-fonts)
                        when (<= position (line-length line))
                          collect (font-mark line position font)))))))

(defun set-line-text (line string fonts)
  (unless (and (string= string (line-string line))
               (equal fonts (getf (line-plist line) 'term-fonts)))
    (with-mark ((start (mark line 0)) (end (mark line (line-length line))))
      (delete-region (region start end))
      (insert-string start string))
    (setf (getf (line-plist line) 'term-fonts) fonts)
    ;; Its marks are made again by the highlighter.
    (let ((old (getf (line-plist line) 'term-marks)))
      (when old (setf (car old) nil)))))

(defun screen-first-line (term)
  ;; A mark, kept there: the scrollback was walked to find it, and with ten
  ;; thousand lines that was most of the time.  It stays put as text is
  ;; inserted at it -- the rows' text, the newlines that make the rows when
  ;; the buffer is empty -- and is moved past the scrollback's new lines.
  (mark-line (term-screen-mark term)))

(defun term-refresh (term)
  "Bring the buffer up to date with the terminal: the lines pushed off the
   screen added to the scrollback, and each row of the screen as it is."
  (setf (term-last-refresh term) (get-internal-real-time)
        (term-refresh-pending term) nil)
  (let ((buffer (term-buffer term)))
    (when (member buffer *buffer-list*)
      (with-writable-buffer (buffer)
        ;; The scrollback, oldest first, before the screen's first line.
        (let ((pushed (reverse (term-pushed term))))
          (setf (term-pushed term) '())
          (when pushed
            ;; All of them in one region.  A line inserted on its own is
            ;; numbered halfway between its neighbours, and lines inserted
            ;; one after another at the same place use the gap up in a few
            ;; lines: then the whole buffer, scrollback and all, is
            ;; renumbered.  With a flood of output that was most of the
            ;; time; inserted as one region, they are renumbered at most once.
            (with-mark ((mark (term-screen-mark term) :left-inserting))
              (let ((first (mark-line mark)))
                (ninsert-region mark (string-to-region
                                      (with-output-to-string (s)
                                        (dolist (entry pushed)
                                          (write-string (first entry) s)
                                          (write-char #\Newline s)))))
                (loop for entry in pushed
                      for line = first then (line-next line)
                      do (setf (getf (line-plist line) 'term-fonts) (second entry))))
              (move-mark (term-screen-mark term) mark))
            (incf (term-scrollback term) (length pushed))
            ;; The scrollback kept within its limit.
            (let ((excess (- (term-scrollback term) (or (value term-scrollback) most-positive-fixnum))))
              (when (plusp excess)
                (with-mark ((start (buffer-start-mark buffer))
                            (end (buffer-start-mark buffer)))
                  (line-offset end excess 0)
                  (delete-region (region start end)))
                (decf (term-scrollback term) excess)))))
        ;; As many lines after it as the screen has rows.
        (let ((wanted (term-rows term))
              (have (count-lines (region (term-screen-mark term) (buffer-end-mark buffer)))))
          (unless (= have wanted)
            (mark-term-all term))
          (cond ((< have wanted)
                 (with-mark ((end (buffer-end-mark buffer) :left-inserting))
                   (dotimes (i (- wanted have)) (insert-character end #\Newline))))
                ((> have wanted)
                 (with-mark ((start (buffer-end-mark buffer))
                             (end (buffer-end-mark buffer)))
                   (line-offset start (- wanted have))
                   (line-end start)
                   (delete-region (region start end))))))
        ;; The screen: the rows libvterm damaged since the last refresh,
        ;; and the cursor's rows, old and new, which a cursor moved without
        ;; writing leaves undamaged (its row is long enough for it).
        (let ((line (screen-first-line term))
              (cursor-positions nil))
          (multiple-value-bind (cursor-row cursor-column) (term-cursor term)
            (let ((old (term-last-cursor term)))
              (unless (and old (= (car old) cursor-row) (= (cdr old) cursor-column))
                (when old (mark-term-rows term (car old) (1+ (car old))))
                (mark-term-rows term cursor-row (1+ cursor-row))
                (setf (term-last-cursor term) (cons cursor-row cursor-column))))
            (unless (and (term-positions term)
                         (= (length (term-positions term)) (term-rows term)))
              (setf (term-positions term) (make-array (term-rows term) :initial-element nil))
              (mark-term-all term))
            (dotimes (row (term-rows term))
              (if (row-dirty-p term row)
                  (multiple-value-bind (string fonts positions) (screen-row-text term row)
                    (setf (svref (term-positions term) row) positions)
                    (when (= row cursor-row)
                      ;; The line long enough for the cursor to be on it.
                      (let ((at (aref positions (min cursor-column (term-columns term)))))
                        (when (< (length string) at)
                          (setf string (concatenate 'string string
                                                    (make-string (- at (length string))
                                                                 :initial-element #\Space))))))
                    (set-line-text line string fonts))
                  (incf *term-rows-skipped*))
              (when (= row cursor-row)
                (setf cursor-positions (svref (term-positions term) row)))
              (setf line (or (line-next line) line)))
            (setf (term-dirty term) nil)
            ;; Point at the cursor, but in Term Copy mode, where it is read.
            (when (string= (buffer-major-mode buffer) "Term")
              (let ((cursor-line (screen-first-line term)))
                (dotimes (i cursor-row) (setf cursor-line (or (line-next cursor-line) cursor-line)))
                (let ((charpos (min (line-length cursor-line)
                                    (if cursor-positions
                                        (aref cursor-positions
                                              (min cursor-column (term-columns term)))
                                        cursor-column))))
                  (move-to-position (buffer-point buffer) charpos cursor-line)
                  ;; Each window shows the screen from its first row, as a
                  ;; terminal's window does: rewriting the rows moves the
                  ;; marks on them, its display start too.
                  (dolist (window (buffer-windows buffer))
                    (move-mark (window-point window) (buffer-point buffer))
                    (move-mark (window-display-start window) (term-screen-mark term))))))))
        (setf (buffer-modified buffer) nil))
      (setf (buffer-writable buffer) nil))))

(defun term-cursor (term)
  (cffi:with-foreign-object (position '(:struct vterm:vterm-pos))
    (vterm:vterm-state-get-cursorpos (term-state term) position)
    (values (vterm:vterm-pos-row position) (vterm:vterm-pos-col position))))

(defparameter *term-refresh-interval* 1/60
  "The least time between two readings of a terminal's screen: output in a
   flood is drawn at most this often, and once more when it stops.")

(defun term-note-output (term)
  (let ((since (/ (- (get-internal-real-time) (term-last-refresh term))
                  internal-time-units-per-second)))
    (cond ((or (>= since *term-refresh-interval*)
               ;; Just after a key, output is its echo: read at once.
               (hi::echo-expected-p))
           (term-refresh term))
          ((not (term-refresh-pending term))
           (setf (term-refresh-pending term) t)
           (schedule-event (- *term-refresh-interval* since)
                           (lambda (elapsed)
                             (declare (ignore elapsed))
                             (when (term-refresh-pending term)
                               (term-refresh term)))
                           nil)))))


;;;; Its size: the window's.  libvterm and the terminal are told, and the
;;;; kernel sends SIGWINCH to the program in its foreground.

(defun term-window-size (term)
  "The rows and columns of the first window showing TERM's buffer, or NIL."
  (let ((window (first (buffer-windows (term-buffer term)))))
    (when window
      ;; A column fewer, so that a full row is not taken for a long line.
      (values (max 2 (window-height window))
              (max 10 (1- (window-text-width window)))))))

(defun term-fit-window (term)
  (multiple-value-bind (rows columns) (term-window-size term)
    (when (and rows (or (/= rows (term-rows term)) (/= columns (term-columns term))))
      (setf (term-rows term) rows
            (term-columns term) columns)
      (vterm:vterm-set-size (term-vt term) rows columns)
      (vterm:vterm-screen-flush-damage (term-screen term))
      (mark-term-all term)
      (when (and (term-connection term) (not (term-exit-code term)))
        (set-pty-size (hi::connection-read-fd (term-connection term)) rows columns))
      (term-refresh term))))

(defun term-idle (elapsed)
  (declare (ignore elapsed))
  (maphash (lambda (id term)
             (declare (ignore id))
             (when (buffer-windows (term-buffer term))
               (term-fit-window term)))
           *terms*)
  (when (zerop (hash-table-count *terms*))
    (remove-scheduled-event 'term-idle)))


;;;; Making one.

(defmode "Term" :major-p t
  :documentation "A terminal: every key goes to the program but C-x, M-x
   and C-c, which starts Heml's own: C-c C-c sends ^C, C-c C-x ^X, C-c C-j
   goes to Term Copy mode, and C-y pastes.")

(defmode "Term Copy" :major-p t
  :documentation "A terminal's text, to be read, searched and copied from;
   q or C-c C-k goes back to Term mode, where keys go to the program.")

(define-mode-highlighter "Term" 'term-highlight-line)
(define-mode-highlighter "Term Copy" 'term-highlight-line)

(defun term-shell ()
  (or (value term-program) (uiop:getenv "SHELL") "/bin/sh"))

(defun new-term-buffer-name ()
  (loop for i from 1
        for name = (if (= i 1) "*terminal*" (format nil "*terminal<~D>*" i))
        unless (getstring name *buffer-names*) return name))

(defun make-term (command directory &key environment name)
  "Run COMMAND in DIRECTORY in a new terminal, in a buffer NAME or the next
   *terminal*, with ENVIRONMENT, an alist, beside the terminal's own."
  (handler-case (vterm:ensure-libvterm)
    (error ()
      (editor-error "libvterm is not installed (brew install libvterm).")))
  (let* ((buffer (make-buffer (or name (new-term-buffer-name)) :modes '("Term")
                                                    :delete-hook (list 'term-buffer-deleted)))
         (id (incf *term-count*))
         (term (%make-term :id id :buffer buffer :rows 24 :columns 80
                           :cell (cffi:foreign-alloc '(:struct vterm:vterm-screen-cell)))))
    (defhvar "Term" "This buffer's terminal." :buffer buffer :value term)
    (setf (term-screen-mark term) (copy-mark (buffer-start-mark buffer) :right-inserting))
    ;; What the program draws is not the user's to undo, and recording it
    ;; would take most of the time and keep all of it.
    (setf (buffer-undo-p buffer) nil)
    ;; Nor is it Lisp: the modeline's package, read from the (in-package
    ;; ...) above point, had every line the program wrote parsed as Lisp at
    ;; each redisplay -- an eighth of the time output streamed in.  (By
    ;; name: the default fields' objects need not be MODELINE-FIELD's.)
    (setf (buffer-modeline-fields buffer)
          (remove :package (buffer-modeline-fields buffer) :key #'modeline-field-name))
    (setf (gethash id *terms*) term)
    (change-to-buffer buffer)
    (multiple-value-bind (rows columns) (term-window-size term)
      (when rows (setf (term-rows term) rows (term-columns term) columns)))
    (let* ((vt (vterm:vterm-new (term-rows term) (term-columns term)))
           (screen (vterm:vterm-obtain-screen vt)))
      (setf (term-vt term) vt
            (term-screen term) screen
            (term-state term) (vterm:vterm-obtain-state vt))
      (vterm:vterm-set-utf8 vt 1)
      (vterm:vterm-output-set-callback vt (cffi:callback term-output) (cffi:make-pointer id))
      (vterm:vterm-screen-set-callbacks screen (term-callbacks) (cffi:make-pointer id))
      (vterm:vterm-screen-enable-altscreen screen 1)
      ;; A narrower terminal wraps its lines, and a wider one joins them
      ;; again, rather than cutting them off at the new width.
      (vterm:vterm-screen-enable-reflow screen 1)
      ;; Damage merged a row at a time, and flushed after each write.
      (vterm:vterm-screen-set-damage-merge screen vterm:+damage-row+)
      (vterm:vterm-screen-reset screen 1))
    (setf (term-connection term)
          (make-process-with-pty-connection
           command
           :name (format nil "terminal ~D" id)
           :directory directory
           :terminal t
           :rows (term-rows term) :columns (term-columns term)
           :environment `(("TERM" . "xterm-256color") ("COLORTERM" . "truecolor")
                          ("INSIDE_HEML" . "1") ,@environment)
           :filter (lambda (connection bytes)
                     (declare (ignore connection))
                     (term-input term bytes)
                     nil)
           :sentinel (lambda (connection event)
                       (when (eq event :disconnected)
                         (term-ended term connection)))))
    (term-refresh term)
    (remove-scheduled-event 'term-idle)
    (schedule-event 0.25 'term-idle)
    term))

(defun term-input (term bytes)
  "What the program wrote, given to libvterm."
  (when (term-vt term)
    (let ((length (length bytes)))
      (cffi:with-foreign-object (buffer :uint8 length)
        (dotimes (i length)
          (setf (cffi:mem-aref buffer :uint8 i) (aref bytes i)))
        (vterm:vterm-input-write (term-vt term) buffer length)
        (vterm:vterm-screen-flush-damage (term-screen term))))
    (term-note-output term)))

(defun term-ended (term connection)
  (let* ((process (hi::connection-process-connection connection))
         (code (connection-exit-code process))
         (signal (let ((signal (connection-exit-status process)))
                   (and (integerp signal) (plusp signal) signal))))
    (setf (term-exit-code term) (or code t))
    ;; Said on the terminal itself, so that it is on its screen, scrolling
    ;; it as the program's own last line would.
    (when (term-vt term)
      (let ((octets (babel:string-to-octets
                     (format nil "~C~C[The program ended~@[ with code ~D~]~@[, by signal ~D~].]"
                             #\Return #\Newline (and (not signal) code) signal)
                     :encoding :utf-8)))
        (cffi:with-foreign-object (buffer :uint8 (length octets))
          (dotimes (i (length octets))
            (setf (cffi:mem-aref buffer :uint8 i) (aref octets i)))
          (vterm:vterm-input-write (term-vt term) buffer (length octets))
          (vterm:vterm-screen-flush-damage (term-screen term))))
      (term-refresh term))))

(defun term-buffer-deleted (buffer)
  (let ((term (buffer-term buffer)))
    (when term
      (remhash (term-id term) *terms*)
      (let ((connection (term-connection term)))
        (setf (term-connection term) nil)
        ;; Its processes are hung up, as closing a terminal window does,
        ;; and its descriptor closed: after the program has ended too, or
        ;; each terminal left a pseudo-terminal open.
        (when connection
          (ignore-errors (delete-connection connection))))
      (when (term-vt term)
        (vterm:vterm-free (term-vt term))
        (setf (term-vt term) nil))
      (cffi:foreign-free (term-cell term))
      (when (term-rgb term) (cffi:foreign-free (term-rgb term))))))

(defcommand "Term" (p)
  "Run a shell -- \"Term Program\", or $SHELL -- in a terminal emulated in a
   buffer of its own, from this buffer's directory: every key goes to it
   but C-x, M-x and C-c.  With an argument, ask what to run."
  "Run a shell in a terminal."
  (let ((command (if p
                     (prompt-for-string :prompt "Run in a terminal: " :default (term-shell))
                     (term-shell))))
    (make-term (cl-ppcre:split "\\s+" (string-trim " " command))
               (default-directory))))


;;;; Keys.

(defparameter *term-keys*
  `(("Return" . ,vterm:+key-enter+) ("Linefeed" . ,vterm:+key-enter+)
    ("Tab" . ,vterm:+key-tab+) ("Backspace" . ,vterm:+key-backspace+)
    ("Escape" . ,vterm:+key-escape+) ("Uparrow" . ,vterm:+key-up+)
    ("Downarrow" . ,vterm:+key-down+) ("Leftarrow" . ,vterm:+key-left+)
    ("Rightarrow" . ,vterm:+key-right+) ("Insert" . ,vterm:+key-ins+)
    ("Delete" . ,vterm:+key-del+) ("Home" . ,vterm:+key-home+)
    ("End" . ,vterm:+key-end+) ("Pageup" . ,vterm:+key-pageup+)
    ("Pagedown" . ,vterm:+key-pagedown+))
  "Heml's names for keys and libvterm's.")

(defun term-send-key-event (term key-event)
  (let* ((keysym (heml-ext:key-event-keysym key-event))
         (modifiers (heml-ext:key-event-bits-modifiers (heml-ext:key-event-bits key-event)))
         (modifier (logior (if (member "Control" modifiers :test #'string-equal) vterm:+mod-ctrl+ 0)
                           (if (member "Meta" modifiers :test #'string-equal) vterm:+mod-alt+ 0)
                           (if (member "Shift" modifiers :test #'string-equal) vterm:+mod-shift+ 0)))
         (name (heml-ext:keysym-preferred-name keysym))
         (key (cdr (assoc name *term-keys* :test #'string=)))
         (vt (term-vt term)))
    (cond ((term-exit-code term) (editor-error "The program has ended."))
          (key (vterm:vterm-keyboard-key vt key modifier))
          ((and name (> (length name) 1) (char-equal (char name 0) #\F)
                (every #'digit-char-p (subseq name 1)))
           (vterm:vterm-keyboard-key vt (+ vterm:+key-function-0+ (parse-integer name :start 1))
                                     modifier))
          ((< keysym #xFF00)
           (vterm:vterm-keyboard-unichar vt keysym modifier))
          ((>= keysym #x01000000)
           (vterm:vterm-keyboard-unichar vt (- keysym #x01000000) modifier))
          (t (editor-error "~A is not a key a terminal has." name)))))

(defcommand "Term Send Key" (p)
  "Send the key just typed to the terminal's program, as a terminal would."
  "Send this key to the program."
  (declare (ignore p))
  (term-send-key-event (current-term) *last-key-event-typed*))

(defcommand "Term Paste" (p)
  "Send the last killed text -- or the clipboard's -- to the program, as a
   terminal pastes, bracketed so that it knows it was not typed."
  "Paste into the terminal."
  (declare (ignore p))
  (let ((term (current-term)))
    (interprogram-paste)
    (when (zerop (ring-length *kill-ring*))
      (editor-error "Nothing to paste."))
    (term-paste-string term (region-to-string (ring-ref *kill-ring* 0)))))

(defun term-paste-string (term text)
  "Send TEXT to TERM's program as a terminal pastes, bracketed."
  (let ((vt (term-vt term)))
    (vterm:vterm-keyboard-start-paste vt)
    (loop for char across text
          do (vterm:vterm-keyboard-unichar vt (char-code (if (char= char #\Newline) #\Return char)) 0))
    (vterm:vterm-keyboard-end-paste vt)))

(defcommand "Term Copy Mode" (p)
  "Read the terminal's text as a buffer: move, search and copy; q or C-c C-k
   goes back."
  "Read the terminal's text as a buffer."
  (declare (ignore p))
  (current-term)
  (setf (buffer-major-mode (current-buffer)) "Term Copy")
  (message "Term Copy mode: q or C-c C-k goes back."))

(defcommand "Term Char Mode" (p)
  "Go back to sending every key to the terminal's program."
  "Send keys to the terminal again."
  (declare (ignore p))
  (let ((term (current-term)))
    (setf (buffer-major-mode (current-buffer)) "Term")
    (term-refresh term)))

(defcommand "Term Interrupt" (p)
  "Send SIGINT to the job in the terminal's foreground, as ^C typed to it
   does."
  "Interrupt the terminal's job."
  (declare (ignore p))
  (let ((term (current-term)))
    (when (term-connection term)
      (connection-signal (term-connection term) :sigint))))

(defun bind-term-keys ()
  (flet ((term (key-event) (bind-key "Term Send Key" key-event :mode "Term")))
    (let ((control (heml-ext:key-event-modifier-mask "Control"))
          (meta (heml-ext:key-event-modifier-mask "Meta"))
          (shift (heml-ext:key-event-modifier-mask "Shift")))
      ;; Every character, alone and with Meta, but M-x.
      (loop for code from 32 below 127
            for key-event = (heml-ext:char-key-event (code-char code))
            when key-event
              do (term key-event)
                 (unless (char= (code-char code) #\x)
                   (term (heml-ext:make-key-event key-event meta))))
      ;; Control and a letter, but C-c and C-x, which are Heml's, and C-g,
      ;; which the editor takes as its abort before any key is looked up:
      ;; C-c C-g sends it.
      (loop for char across "abdefhijklmnopqrstuvwyz@[\\]^_ "
            for key-event = (heml-ext:char-key-event char)
            when key-event
              do (term (heml-ext:make-key-event key-event control))
                 (term (heml-ext:make-key-event key-event (logior control meta))))
      ;; The named keys, alone and with each modifier.
      (dolist (name (append (mapcar #'car *term-keys*)
                            (loop for n from 1 to 12 collect (format nil "F~D" n))))
        (let ((key-event (heml-ext:make-key-event name)))
          (dolist (bits (list 0 control meta shift (logior control shift)))
            (term (heml-ext:make-key-event key-event bits)))))))
  ;; A character past ASCII, as its key is made.
  (let ((previous heml-ext::*new-character-key-event-hook*))
    (setf heml-ext::*new-character-key-event-hook*
          (lambda (key-event)
            (when previous (funcall previous key-event))
            (bind-key "Term Send Key" key-event :mode "Term"))))
  (bind-key "Term Send Key" #k"control-c control-c" :mode "Term")
  (bind-key "Term Send Key" #k"control-c control-x" :mode "Term")
  (bind-key "Term Send Key" #k"control-c control-z" :mode "Term")
  (bind-key "Term Send Key" #k"control-c control-d" :mode "Term")
  (bind-key "Term Send Key" #k"control-c control-g" :mode "Term")
  (bind-key "Term Copy Mode" #k"control-c control-j" :mode "Term")
  ;; A terminal's ^J is Linefeed.
  (bind-key "Term Copy Mode" #k"control-c linefeed" :mode "Term")
  ;; Escape goes to the program, as vim wants: in a terminal Heml, where
  ;; Meta is Escape and a key, M-x is then the program's too, and Heml's is
  ;; C-c M-x or C-c x.
  (bind-key "Extended Command" #k"control-c meta-x" :mode "Term")
  (bind-key "Extended Command" #k"control-c x" :mode "Term")
  (bind-key "Term Paste" #k"control-y" :mode "Term")
  (bind-key "Term Paste" #k"super-v" :mode "Term")
  (bind-key "Term Char Mode" #k"control-c control-k" :mode "Term Copy")
  (bind-key "Term Char Mode" #k"q" :mode "Term Copy"))

(bind-term-keys)

(add-menu-item "Tools" '("Terminal" "Term") :before "Shell Command…")
