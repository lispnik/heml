;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(in-package :heml-internals)

;;;
;;; **********************************************************************
;;;
;;;    Written by Rob MacLachlan
;;;
;;; This file contains functions related to building line images.
;;;


;;;    The code in here is factored out in this way because it is more
;;; or less implementation dependant.  The reason this code is
;;; implementation dependant is not because it is not written in
;;; Common Lisp per se, but because it uses this thing called
;;; %SP-Find-Character-With-Attribute to find any characters that
;;; are to be displayed on the line which do not print as themselves.
;;; This permits us to have an arbitrary string or even string-valued
;;; function to as the representation for such a "Funny" character
;;; with minimal penalty for the normal case.  This function can be written
;;; in lisp, and is included commented-out below, but if this function
;;; is not real fast then redisplay performance will suffer.
;;;

(defconstant winning-char #b01 "Bit for a char that prints normally")
(defconstant losing-char #b10 "Bit for char with funny representation.")

;;; Wide characters -- CJK, fullwidth forms, most emoji -- take two columns
;;; on a terminal and in any monospaced font.  Redisplay gives each one a
;;; print representation of itself followed by WIDE-CHARACTER-FILLER, the
;;; way it gives ^X to a control character, so that everything that counts
;;; columns counts two.  A device draws the character across both cells and
;;; draws nothing for the filler.
;;;
(defconstant wide-character-filler (code-char #xFFFF)
  "Stands in the column a wide character covers after its own.  U+FFFF is
   a noncharacter, which no text should contain.")

(defun wide-character-p (character)
  #+sbcl (member (sb-unicode:east-asian-width character) '(:w :f))
  #+ecl (let ((code (char-code character)))
           (or (<= #x1100 code #x115F) (<= #x2E80 code #xA4CF)
               (<= #xAC00 code #xD7A3) (<= #xF900 code #xFAFF)
               (<= #xFE30 code #xFE4F) (<= #xFF00 code #xFF60)
               (<= #xFFE0 code #xFFE6) (<= #x1F300 code #x1F64F)
               (<= #x1F900 code #x1F9FF) (<= #x20000 code #x3FFFD))))

(defvar *losing-character-mask*
  (make-character-set
   :page0 (make-array 256 :element-type '(mod 256)
                      :initial-element winning-char)
   :table (make-hash-table)
   :default winning-char
   :default-function (lambda (code)
                       (if (wide-character-p (code-char code))
                           losing-char
                           winning-char)))
  "This is a character set used by redisplay to find funny chars.")
(defvar *print-representation-char-set* nil
  "Redisplay's handle on the :print-representation attribute.")

;;;  Do a find-character-with-attribute on the *losing-character-mask*.
(defmacro %fcwa (str start end mask)
  `(%sp-find-character-with-attribute
    ,str ,start ,end *losing-character-mask* ,mask))

;;; Get the print-representation of a character.
(defmacro get-rep (ch)
  `(char-set-ref *print-representation-char-set* (char-code ,ch)))



(declaim (special *character-attributes*))

;;; %init-line-image  --  Internal
;;;
;;;    Set up the print-representations for funny chars.  We make the
;;; attribute character-set by hand.
;;;
(defun %init-line-image ()
  (defattribute "Print Representation"
    "The value of this attribute determines how a character is displayed
    on the screen.  If the value is a string this string is literally
    displayed.  If it is a function, then that function is called with
    the current X position to get the string to display.")
  (setq *print-representation-char-set*
        (make-character-set
         :page0 (make-array 256 :initial-element nil)
         :table (make-hash-table)
         :default nil
         :default-function (lambda (code)
                             (let ((character (code-char code)))
                               (when (wide-character-p character)
                                 (coerce (list character wide-character-filler)
                                         'simple-string))))))
  (setf (attribute-descriptor-char-set
         (gethash :print-representation *character-attributes*))
        *print-representation-char-set*)
  (add-hook heml::character-attribute-hook
            #'redis-set-char-attribute-hook-fun)
  (do ((i (1- (char-code #\space)) (1- i)) str)
      ((minusp i))
    (setq str (make-string 2))
    (setf (elt (the simple-string str) 0) #\^)
    (setf (elt (the simple-string str) 1)
          (code-char (+ i (char-code #\@))))
    (setf (character-attribute :print-representation (code-char i)) str))
  (setf (character-attribute :print-representation (code-char #o177)) "^?")
  (setf (character-attribute :print-representation #\tab)
        #'redis-tab-display-fun))

;;; redis-set-char-attribute-hook-fun
;;;
;;;    Keep track of which characters have funny representations.
;;;
(defun redis-set-char-attribute-hook-fun (attribute char new-value)
  (when (eq attribute :print-representation)
    (cond
     ((simple-string-p new-value)
      (if (and (= (length (the simple-string new-value)) 1)
               (char= char (elt (the simple-string new-value) 0)))
          (setf (char-set-ref *losing-character-mask* (char-code char)) winning-char)
          (setf (char-set-ref *losing-character-mask* (char-code char))
                losing-char)))
     ((functionp new-value)
      (setf (char-set-ref *losing-character-mask* (char-code char)) losing-char))
     (t (error "Bad print representation: ~S" new-value)))))

;;; redis-tab-display-fun
;;;
;;;    This function is initially the :print-representation for tab.
;;;
(defun redis-tab-display-fun (xpos)
  (svref '#("        "
            "       "
            "      "
            "     "
            "    "
            "   "
            "  "
            " ")
         (logand xpos 7)))


;;;; The actual line image computing functions.
;;;;

(eval-when (:compile-toplevel :execute)
;;; display-some-chars  --  internal
;;;
;;;    Put some characters into a window.  Characters from src-start
;;; to src-end in src are are put in the window's dis-line's.  Lines
;;; are wrapped as necessary.  dst is the dis-line-chars of the dis-line
;;; currently being written.  Dis-lines is the window's vector of dis-lines.
;;; dis-line is the dis-line currently being written.  Line is the index
;;; into dis-lines of the current dis-line.  dst-start is the index to
;;; start writing chars at.  Height and width are the height and width of the
;;; window.  src-start, dst, dst-start, line and dis-line are updated.
;;; Done-P indicates whether there are more characters after this sequence.
;;;
(defmacro display-some-chars (src src-start src-end dst dst-start width done-p)
  `(let ((dst-end (+ ,dst-start (- ,src-end ,src-start))))
     (declare (fixnum dst-end))
     (cond
      ((>= dst-end ,width)
       (cond
        ((and ,done-p (= dst-end ,width))
         (%sp-byte-blt ,src ,src-start ,dst ,dst-start dst-end)
         (setq ,dst-start dst-end  ,src-start ,src-end))
        (t
         (let ((1-width (1- ,width)))
           (%sp-byte-blt ,src ,src-start ,dst ,dst-start 1-width)
           (setf (elt (the simple-string ,dst) 1-width) *line-wrap-char*)
           (setq ,src-start (+ ,src-start (- 1-width ,dst-start)))
           (setq ,dst-start nil)))))
      (t (%sp-byte-blt ,src ,src-start ,dst ,dst-start dst-end)
         (setq ,dst-start dst-end  ,src-start ,src-end)))))

;;; These macros are given as args to display-losing-chars to get the
;;; print representation of whatever is in the data vector.
(defmacro string-get-rep (string index)
  `(get-rep (schar ,string ,index)))

;;; display-losing-chars  --  Internal
;;;
;;;    This macro is called by the compute-line-image functions to
;;; display a group of losing characters.
;;;
(defmacro display-losing-chars (line-chars index end dest xpos width
                                           string underhang access-fun
                                           &optional (done-p `(= ,index ,end)))
  `(do ((last (or (%fcwa ,line-chars ,index ,end winning-char) ,end))
        (len 0)
        (zero 0)
        str)
       (())
     (declare (fixnum last len zero))
     (setq str (,access-fun ,line-chars ,index))
     (unless (simple-string-p str) (setq str (funcall str ,xpos)))
     (setq len (strlen str)  zero 0)
     (incf ,index)
     (display-some-chars str zero len ,dest ,xpos ,width ,done-p)
     (cond ((not ,xpos)
            ;; We wrapped in the middle of a losing char.
            (setq ,underhang zero  ,string str)
            (return nil))
           ((= ,index last)
            ;; No more losing chars in this bunch.
            (return nil)))))

(defmacro update-and-punt (dis-line length string underhang end)
  `(progn (setf (dis-line-length ,dis-line) ,length)
          (return (values ,string ,underhang
                          (setf (dis-line-end ,dis-line) ,end)))))

); eval-when

;;; compute-normal-line-image  --  Internal
;;;
;;;    Compute the screen representation of Line starting at Start
;;; putting it in Dis-Line beginning at Xpos.  Width is the width of the
;;; window we are displaying in.  If the line will wrap then we display
;;; as many chars as we can then put in *line-wrap-char*.  The values
;;; returned are described in Compute-Line-Image, which tail-recursively
;;; returns them.  The length slot in Dis-Line is updated.
;;;
;;; We use the *losing-character-mask* to break the line to be displayed
;;; up into chunks of characters with normal print representation and
;;; those with funny representations.
;;;
(defun compute-normal-line-image (line start dis-line xpos width)
  (declare (fixnum start width) (type (or fixnum null) xpos))
  (do* ((index start)
        (line-chars (line-chars line))
        (end (strlen line-chars))
        (dest (dis-line-chars dis-line))
        (losing 0)
        underhang string)
       (())
    (declare (fixnum index end)
             (type (or fixnum null) losing)
             (simple-string line-chars dest))
    (cond
     (underhang
      (update-and-punt dis-line width string underhang index))
     ((null xpos)
      (update-and-punt dis-line width nil 0 index))
     ((= index end)
      (update-and-punt dis-line xpos nil nil index)))
    (setq losing (%fcwa line-chars index end losing-char))
    (when (null losing)
      (display-some-chars line-chars index end dest xpos width t)
      (if (or xpos (= index end))
          (update-and-punt dis-line xpos nil nil index)
          (update-and-punt dis-line width nil 0 index)))
    (display-some-chars line-chars index losing dest xpos width nil)
    (cond
     ;; Did we wrap?
     ((null xpos)
      (update-and-punt dis-line width nil 0 index))
     ;; Are we about to cause the line to wrap? If so, wrap before
     ;; it's too late.
     ((= xpos width)
      (setf (char dest (1- width)) *line-wrap-char*)
      (update-and-punt dis-line width nil 0 index))
     (t
      (display-losing-chars line-chars index end dest xpos width string
                            underhang string-get-rep)))))


;;; compute-cached-line-image  --  Internal
;;;
;;;    Like compute-normal-line-image, only works on the cached line.
;;;
(defun compute-cached-line-image (index dis-line xpos width)
  (declare (fixnum index width) (type (or fixnum null) xpos))
  (prog ((gap (- right-open-pos left-open-pos))
         (dest (dis-line-chars dis-line))
         (done-p (= right-open-pos line-cache-length))
         (losing 0)
         string underhang)
    (declare (fixnum gap) (simple-string dest)
             (type (or fixnum null) losing))
   LEFT-LOOP
    (cond
     (underhang
      (update-and-punt dis-line width string underhang index))
     ((null xpos)
      (update-and-punt dis-line width nil 0 index))
     ((>= index left-open-pos)
      (go RIGHT-START)))
    (setq losing (%fcwa open-chars index left-open-pos losing-char))
    (cond
     (losing
      (display-some-chars open-chars index losing dest xpos width nil)
      ;; If we we didn't wrap then display some losers...
      (if xpos
          (display-losing-chars open-chars index left-open-pos dest xpos
                                width string underhang string-get-rep
                                (and done-p (= index left-open-pos)))
          (update-and-punt dis-line width nil 0 index)))
     (t
      (display-some-chars open-chars index left-open-pos dest xpos width done-p)))
    (go LEFT-LOOP)

   RIGHT-START
    (setq index (+ index gap))
   RIGHT-LOOP
    (cond
     (underhang
      (update-and-punt dis-line width string underhang (- index gap)))
     ((null xpos)
      (update-and-punt dis-line width nil 0 (- index gap)))
     ((= index line-cache-length)
      (update-and-punt dis-line xpos nil nil (- index gap))))
    (setq losing (%fcwa open-chars index line-cache-length losing-char))
    (cond
     (losing
      (display-some-chars open-chars index losing dest xpos width nil)
      (cond
       ;; Did we wrap?
       ((null xpos)
        (update-and-punt dis-line width nil 0 (- index gap)))
       (t
        (display-losing-chars open-chars index line-cache-length dest xpos
                              width string underhang string-get-rep))))
     (t
      (display-some-chars open-chars index line-cache-length dest xpos width t)))
    (go RIGHT-LOOP)))

(defun make-some-font-changes ()
  (do ((res nil (make-font-change res))
       (i 42 (1- i)))
      ((zerop i) res)))

(defvar *free-font-changes* (make-some-font-changes)
  "Font-Change structures that nobody's using at the moment.")

(defmacro alloc-font-change (x font mark)
  `(progn
    (unless *free-font-changes*
      (setq *free-font-changes* (make-some-font-changes)))
    (let ((new-fc *free-font-changes*))
      (setq *free-font-changes* (font-change-next new-fc))
      (setf (font-change-x new-fc) ,x
            (font-change-font new-fc) ,font
            (font-change-next new-fc) nil
            (font-change-mark new-fc) ,mark)
      new-fc)))

;;;
;;; compute-line-image  --  Internal
;;;
;;;    This function builds a full line image from some characters in
;;; a line and from some characters which may be left over from the previous
;;; line.
;;;
;;; Parameters:
;;;    String - This is the string which contains the characters left over
;;; from the previous line.  This is NIL if there are none.
;;;    Underhang - Characters from here to the end of String are put at the
;;; beginning of the line image.
;;;    Line - This is the line to display characters from.
;;;    Offset - This is the index of the first character to display in Line.
;;;    Dis-Line - This is the dis-line to put the line-image in.  The only
;;; slots affected are the chars and the length.
;;;    Width - This is the width of the field to display in.
;;;
;;; Three values are returned:
;;;    1) The new overhang string, if none this is NIL.
;;;    2) The new underhang, if this is NIL then the entire line was
;;; displayed.  If the entire line was not displayed, but there was no
;;; underhang, then this is 0.
;;;    3) The index in line after the last character displayed.
;;;
(defun compute-line-image (string underhang line offset dis-line width)
  ;;
  ;; Bring the line's syntax highlighting up to date, as its buffer's major
  ;; mode does it: it is font marks, read below.
  (highlight-line line)
  ;;
  ;; Release any old font-changes.
  (let ((changes (dis-line-font-changes dis-line)))
    (when changes
      (do ((prev changes current)
           (current (font-change-next changes)
                    (font-change-next current)))
          ((null current)
           (setf (dis-line-font-changes dis-line) nil)
           (shiftf (font-change-next prev) *free-font-changes* changes))
        (setf (font-change-mark current) nil))))
  ;;
  ;; If the line has any Font-Marks, add Font-Changes for them.
  (let ((marks (line-marks line)))
    (when (dolist (m marks nil)
            (when (fast-font-mark-p m) (return t)))
      (let ((prev nil))
        ;;
        ;; Find the last Font-Mark with charpos less than Offset.  If there is
        ;; such a Font-Mark, then there is a font-change to this font at X = 0.
        (let ((max -1)
              (max-mark nil))
          (dolist (m marks)
            (when (fast-font-mark-p m)
              (let ((charpos (mark-charpos m)))
                (when (and (< charpos offset) (> charpos max))
                  (setq max charpos  max-mark m)))))
          (when max-mark
            (setq prev (alloc-font-change 0 (font-mark-font max-mark) max-mark))
            (setf (dis-line-font-changes dis-line) prev)))
        ;;
        ;; Repeatedly scan through marks, adding a font-change for the
        ;; smallest Font-Mark with a charpos greater than Bound, until
        ;; we find no such mark.
        (do ((bound (1- offset) min)
             (min most-positive-fixnum most-positive-fixnum)
             (min-mark nil nil))
            (())
          (dolist (m marks)
            (when (fast-font-mark-p m)
              (let ((charpos (mark-charpos m)))
                (when (and (> charpos bound) (< charpos min))
                  (setq min charpos  min-mark m)))))
          (unless min-mark (return nil))
          (let ((len (if (eq line open-line)
                         (cached-real-line-length line 10000 offset min)
                         (real-line-length line 10000 offset min))))
            (when (< len width)
              (let ((new (alloc-font-change
                          (+ len
                             (if string
                                 (- (length (the simple-string string)) underhang)
                                 0))
                          (font-mark-font min-mark)
                          min-mark)))
                (if prev
                    (setf (font-change-next prev) new)
                    (setf (dis-line-font-changes dis-line) new))
                (setq prev new))))))))
  ;;
  ;; Recompute the line image.
  (cond
   (string
    (let ((len (strlen string))
          (chars (dis-line-chars dis-line))
          (xpos 0))
      (declare (type (or fixnum null) xpos) (simple-string chars))
      (display-some-chars string underhang len chars xpos width nil)
      (cond
       ((null xpos)
        (values string underhang offset))
       ((eq line open-line)
        (compute-cached-line-image offset dis-line xpos width))
       (t
        (compute-normal-line-image line offset dis-line xpos width)))))
   ((eq line open-line)
    (compute-cached-line-image offset dis-line 0 width))
   (t
    (compute-normal-line-image line offset dis-line 0 width))))


;;;; Popups: rows of text laid over a window's image (winimage.lisp).

(defun release-font-changes (dis-line)
  (let ((changes (dis-line-font-changes dis-line)))
    (when changes
      (do ((last changes (font-change-next last)))
          ((null (font-change-next last))
           (setf (font-change-next last) *free-font-changes*
                 *free-font-changes* changes))
        (setf (font-change-mark last) nil))
      (setf (dis-line-font-changes dis-line) nil))))

(defun popup-dis-line (window y)
  "The dis-line WINDOW shows on its line Y: one past the last line of the
   buffer is added to the image, showing nothing of the buffer."
  (do ((dl (cdr (window-first-line window)) (cdr dl)))
      ((eq dl the-sentinel)
       (let* ((cell (window-spare-lines window))
              (dis-line (car cell))
              (last (window-last-line window)))
         (setf (window-spare-lines window) (cdr cell)
               (cdr cell) the-sentinel)
         (if (eq last the-sentinel)
             (setf (cdr (window-first-line window)) cell)
             (setf (cdr last) cell))
         (setf (window-last-line window) cell)
         (release-font-changes dis-line)
         (setf (dis-line-line dis-line) nil
               (dis-line-position dis-line) y
               (dis-line-length dis-line) 0
               (dis-line-end dis-line) 0)
         dis-line))
    (when (= (dis-line-position (car dl)) y)
      (return (car dl)))))

(defun overlay-dis-line (dis-line x0 x1 text font)
  "Show TEXT in FONT over DIS-LINE's columns X0 to X1."
  (let ((chars (dis-line-chars dis-line))
        (length (dis-line-length dis-line)))
    (when (<= x1 (length chars))
      (when (< length x0)
        (fill chars #\Space :start length :end x0))
      (replace chars text :start1 x0 :end1 x1)
      (setf (dis-line-length dis-line) (max length x1))
      ;; The font changes within the columns go; the font that was in effect
      ;; at their end comes back there.
      (let ((before '()) (after '()) (restore 0) (at-end nil))
        (do ((change (dis-line-font-changes dis-line) (font-change-next change)))
            ((null change))
          (let ((x (font-change-x change)))
            (cond ((< x x0) (push change before) (setf restore (font-change-font change)))
                  ((< x x1) (setf restore (font-change-font change)))
                  (t (when (= x x1) (setf at-end t))
                     (push change after)))))
        (let* ((changes (append (nreverse before)
                                (list (alloc-font-change x0 font nil))
                                (unless at-end
                                  (list (alloc-font-change x1 restore nil)))
                                (nreverse after))))
          (loop for (change next) on changes
                do (setf (font-change-next change) next))
          (setf (dis-line-font-changes dis-line) (first changes)))))))

(defun overlay-popup (window popup)
  (let ((width (window-width window))
        (height (window-height window))
        (x0 (max 0 (popup-x popup)))
        (y (popup-y popup)))
    (dolist (row (popup-rows popup))
      (when (< -1 y height)
        (let ((x1 (min width (+ x0 (length (car row))))))
          (when (< x0 x1)
            (overlay-dis-line (popup-dis-line window y) x0 x1 (car row) (cdr row)))))
      (incf y))
    ;; Parts of rows drawn again, in a font of their own.
    (loop for (row start end font) in (popup-highlights popup)
          for line = (+ (popup-y popup) row)
          for text = (car (nth row (popup-rows popup)))
          when (and text (< -1 line height))
            do (let ((from (+ x0 (max 0 start)))
                     (to (min width (+ x0 (min end (length text))))))
                 (when (< from to)
                   (overlay-dis-line (popup-dis-line window line) from to
                                     (subseq text (- from x0) (- to x0)) font))))))
