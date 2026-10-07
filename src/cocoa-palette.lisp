;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; The echo area as a command palette: while it prompts -- M-x, a file, a
;;; buffer, a variable -- what is typed is shown in a field floating over
;;; the editor, centred near its top, as Xcode's Open Quickly and Spotlight
;;; show theirs, with what it matches below it, the best first.  The arrows
;;; (and C-n, C-p) choose among them, and Return takes the one chosen when
;;; what is typed is not itself an answer.
;;;
;;; The prompt is still the echo area's, and its keys the editor's: each
;;; frame carries a description (PALETTE-DESCRIPTOR) and the main thread
;;; shows it, as the popups' panel does.

(in-package :heml.cocoa)


;;;; On the editor's thread: what the palette shows, and its keys.

(defparameter *palette-rows* 10
  "The most matches the palette lists.")

(defvar *palette-index* 0
  "Which match is chosen.")

(defvar *palette-input* nil
  "What was typed when the matches were last made, so that typing chooses
   the first again.")

(defvar *palette-chosen* nil
  "Whether a match was chosen with the arrows since the input changed.")

(defvar *palette-matches* '()
  "The matches the palette shows, as (TEXT . ANSWER): what is listed, and
   what Return puts in the prompt for it.")

(defun palette-prompting-p ()
  (and (eq hi::*current-window* hi::*echo-area-window*)
       (hi::regionp hi::*parse-input-region*)))

(defun string-table-names (tables)
  (let ((names '()))
    (dolist (table tables names)
      (hi::do-strings (name value table)
        (declare (ignore value))
        (push name names)))))

(defun file-matches (input)
  "The files of INPUT's directory that the rest of INPUT matches."
  (let* ((expanded (ignore-errors (heml-ext:expand-file-name input)))
         (slash (and expanded (position #\/ expanded :from-end t)))
         (directory (if slash (subseq expanded 0 (1+ slash)) (namestring (heml-ext:default-directory))))
         (partial (if slash (subseq expanded (1+ slash)) (or expanded ""))))
    (let ((entries (ignore-errors
                    (append (mapcar (lambda (path)
                                      (concatenate 'string (car (last (pathname-directory path))) "/"))
                                    (uiop:subdirectories directory))
                            (mapcar #'file-namestring (uiop:directory-files directory))))))
      (mapcar (lambda (name) (cons name (concatenate 'string directory name)))
              (rank-matches partial entries)))))

(defun rank-matches (input names)
  "NAMES that INPUT matches, the best first; all of them, by name, for no
   INPUT."
  (if (zerop (length input))
      (sort (copy-list names) #'string-lessp)
      (mapcar #'cdr
              (sort (loop for name in names
                          for score = (heml::fuzzy-file-score input name)
                          when score collect (cons score name))
                    #'> :key #'car))))

(defun palette-matches (input)
  (cond ((eq hi::*parse-type* :file) (file-matches input))
        (hi::*parse-string-tables*
         (mapcar (lambda (name) (cons name name))
                 (rank-matches input (string-table-names hi::*parse-string-tables*))))))

(defun update-palette (input)
  "The matches for INPUT, worked out again when it has changed, and nothing
   chosen among them."
  (unless (equal input *palette-input*)
    (setf *palette-input* input
          *palette-index* 0
          *palette-chosen* nil
          *palette-matches* (ignore-errors (palette-matches input)))))

(defun forget-palette ()
  "Nothing of the last prompt's palette, so that the next starts afresh."
  (setf *palette-input* nil
        *palette-index* 0
        *palette-chosen* nil
        *palette-matches* '()))

(defun palette-descriptor ()
  "The palette, as the main thread shows it, or NIL when the echo area is
   not prompting."
  (unless (palette-prompting-p)
    (forget-palette))
  (when (palette-prompting-p)
    (let* ((input (hi::region-to-string hi::*parse-input-region*))
           (point (hi::buffer-point hi::*echo-area-buffer*))
           (start (hi::region-start hi::*parse-input-region*))
           (echo-hunk (hi::window-hunk hi::*echo-area-window*)))
      (update-palette input)
      (list :prompt (string-right-trim " " (or hi::*parse-prompt* ""))
            :input input
            :caret (if (eq (hi::mark-line point) (hi::mark-line start))
                       (max 0 (- (hi::mark-charpos point) (hi::mark-charpos start)))
                       (length input))
            :matches (loop for (text) in *palette-matches*
                           repeat *palette-rows*
                           collect text)
            :index (and *palette-matches* *palette-index*)
            :echo-top (hunk-top-line echo-hunk)
            :echo-height (hi::device-hunk-text-height echo-hunk)))))

(hi::defcommand "Palette Next" (p)
  "Choose the next of the palette's matches."
  "Choose the next match."
  (declare (ignore p))
  (update-palette (hi::region-to-string hi::*parse-input-region*))
  (when *palette-matches*
    (setf *palette-chosen* t)
    (setf *palette-index* (mod (1+ *palette-index*)
                               (min *palette-rows* (length *palette-matches*))))))

(hi::defcommand "Palette Previous" (p)
  "Choose the previous of the palette's matches."
  "Choose the previous match."
  (declare (ignore p))
  (update-palette (hi::region-to-string hi::*parse-input-region*))
  (when *palette-matches*
    (setf *palette-chosen* t)
    (setf *palette-index* (mod (1- *palette-index*)
                               (min *palette-rows* (length *palette-matches*))))))

(defun answer-p (input)
  "Whether INPUT is an answer to the prompt as it stands."
  (cond ((eq hi::*parse-type* :file)
         (let ((expanded (ignore-errors (heml-ext:expand-file-name input))))
           (and expanded (plusp (length input)) (probe-file expanded))))
        (hi::*parse-string-tables*
         (some (lambda (table) (hi::getstring input table)) hi::*parse-string-tables*))
        (t t)))

(hi::defcommand "Palette Confirm" (p)
  "Take what is typed, or else the palette's match chosen: one chosen with
   the arrows, or the first where only an existing answer will do (M-x),
   never in place of a new file's or buffer's name.  A directory chosen is
   gone into rather than taken."
  "Take the input, or the match chosen."
  (let* ((input (hi::region-to-string hi::*parse-input-region*))
         (match (progn (update-palette input)
                       (nth *palette-index* *palette-matches*))))
    (cond ((or (null match) (zerop (length input)) (answer-p input)
               (not (or *palette-chosen*
                        (and hi::*parse-value-must-exist*
                             (not (eq hi::*parse-type* :file))))))
           (forget-palette)
           (heml::confirm-parse-command p))
          ((and (eq hi::*parse-type* :file)
                (let ((answer (cdr match)))
                  (char= (char answer (1- (length answer))) #\/)))
           (hi::delete-region hi::*parse-input-region*)
           (hi::insert-string (hi::region-end hi::*parse-input-region*) (cdr match)))
          (t
           (hi::delete-region hi::*parse-input-region*)
           (hi::insert-string (hi::region-end hi::*parse-input-region*) (cdr match))
           (forget-palette)
           (heml::confirm-parse-command p)))))

(defun install-palette-bindings (key)
  "The palette's keys, in the echo area: Cocoa's alone, since only it shows
   the palette."
  (hi::bind-key "Palette Next" (funcall key "Downarrow") :mode "Echo Area")
  (hi::bind-key "Palette Previous" (funcall key "Uparrow") :mode "Echo Area")
  (hi::bind-key "Palette Next" (funcall key "n" "Control") :mode "Echo Area")
  (hi::bind-key "Palette Previous" (funcall key "p" "Control") :mode "Echo Area")
  (hi::bind-key "Palette Confirm" (funcall key "Return") :mode "Echo Area"))


;;;; On the main thread: the panel.

(objc:define-objc-class palette-view ()
  ()
  (:objc-class-name "HemlPaletteView")
  (:objc-superclass-name "NSView"))

(objc:define-objc-method ("isFlipped" objc:objc-bool) ((self palette-view))
  t)

(defvar *palette-panel* nil)
(defvar *palette-panel-view* nil)
(defvar *palette-shown* nil
  "The description of the palette the panel shows, or NIL.")

(defparameter *palette-width* 640)

(defun palette-field-height (display)
  (+ (display-char-height display) 18))

(defun palette-height (display palette)
  (let ((matches (length (getf palette :matches))))
    (+ (palette-field-height display)
       (if (plusp matches) (+ 8 (* matches (+ (display-char-height display) 4))) 0))))

(defun draw-palette (display palette width)
  (let* ((char-height (display-char-height display))
         (field (palette-field-height display))
         (prompt (getf palette :prompt))
         (prompt-attributes (modeline-attributes display :inactive))
         (prompt-font (objc:invoke prompt-attributes "objectForKey:" "NSFont"))
         (prompt-width (if (plusp (length prompt))
                           (+ 8 (aref (objc:invoke (objc:string-to-ns-string prompt)
                                                   "sizeWithAttributes:" prompt-attributes)
                                      0))
                           0))
         (input-left (+ 14 prompt-width)))
    (declare (ignore prompt-font))
    ;; The prompt, small, then what is typed, with its caret.
    (when (plusp (length prompt))
      (objc:invoke (objc:string-to-ns-string prompt) "drawAtPoint:withAttributes:"
                   (vector 14d0 (df (/ (- field char-height) 2))) prompt-attributes))
    (draw-popup-string display (getf palette :input) input-left (/ (- field char-height) 2)
                       "labelColor")
    (fill-rect (ns-color display "controlAccentColor")
               (+ input-left (* (getf palette :caret) (display-char-width display)))
               (/ (- field char-height) 2) 2 char-height)
    ;; The matches, the chosen one marked.
    (when (getf palette :matches)
      (fill-rect (ns-color display "separatorColor") 0 field width 1)
      (loop for text in (getf palette :matches)
            for i from 0
            for y = (+ field 4 (* i (+ char-height 4)))
            do (when (eql i (getf palette :index))
                 (let ((path (objc:invoke "NSBezierPath" "bezierPathWithRoundedRect:xRadius:yRadius:"
                                          (vector 6d0 (df y) (df (- width 12)) (df (+ char-height 4)))
                                          5d0 5d0)))
                   (objc:invoke (ns-color display "controlAccentColor") "set")
                   (objc:invoke path "fill")))
               (draw-popup-string display text 14 (+ y 2)
                                  (if (eql i (getf palette :index))
                                      "alternateSelectedControlTextColor"
                                      "labelColor"))))))

(objc:define-objc-method ("drawRect:" :void)
    ((self palette-view pointer) (dirty cocoa:ns-rect))
  (declare (ignore dirty))
  (handler-case
      (let ((display *display*)
            (palette *palette-shown*))
        (when (and display palette)
          (draw-palette display palette (aref (objc:invoke pointer "bounds") 2))))
    (error (condition) (log-error "palette drawRect:" condition))))

(defun ensure-palette-panel ()
  (or *palette-panel*
      (let* ((rect (vector 0d0 0d0 100d0 100d0))
             (panel (objc:invoke (objc:invoke "NSPanel" "alloc")
                                 "initWithContentRect:styleMask:backing:defer:"
                                 rect (logior +borderless+ +nonactivating-panel+)
                                 +backing-store-buffered+ nil))
             (effect (objc:invoke (objc:invoke "NSVisualEffectView" "alloc") "initWithFrame:" rect))
             (view (objc:objc-object-pointer (make-instance 'palette-view))))
        (objc:invoke panel "setReleasedWhenClosed:" nil)
        (objc:invoke panel "setOpaque:" nil)
        (objc:invoke panel "setBackgroundColor:" (objc:invoke "NSColor" "clearColor"))
        (objc:invoke panel "setHasShadow:" t)
        (objc:invoke panel "setIgnoresMouseEvents:" t)
        (objc:invoke effect "setMaterial:" +material-menu+)
        (objc:invoke effect "setState:" 1)
        (objc:invoke effect "setBlendingMode:" 0)
        (objc:invoke effect "setWantsLayer:" t)
        (let ((layer (objc:invoke effect "layer")))
          (objc:invoke layer "setCornerRadius:" 10d0)
          (objc:invoke layer "setMasksToBounds:" t))
        (objc:invoke view "setFrame:" rect)
        (objc:invoke view "setAutoresizingMask:" 18)
        (objc:invoke effect "addSubview:" view)
        (objc:invoke panel "setContentView:" effect)
        (setf *palette-panel-view* view
              *palette-panel* panel))))

(defun show-palette (palette)
  "Show PALETTE, or put it away for NIL.  On the main thread."
  (let* ((display *display*)
         (window (and display (display-window display))))
    (when window
      (setf *palette-shown* palette)
      (cond ((null palette)
             (when *palette-panel*
               (objc:invoke window "removeChildWindow:" *palette-panel*)
               (objc:invoke *palette-panel* "orderOut:" (cffi:null-pointer))))
            (t
             (let* ((panel (ensure-palette-panel))
                    (frame (objc:invoke window "frame"))
                    (width (min *palette-width* (- (aref frame 2) 60)))
                    (height (palette-height display palette))
                    ;; Centred, a fifth of the way down the window.
                    (x (+ (aref frame 0) (/ (- (aref frame 2) width) 2)))
                    (y (- (+ (aref frame 1) (aref frame 3)) (* 0.2 (aref frame 3)) height)))
               (objc:invoke panel "setFrame:display:"
                            (vector (df x) (df y) (df width) (df height)) t)
               (objc:invoke *palette-panel-view* "setNeedsDisplay:" t)
               (unless (objc:invoke-bool panel "isVisible")
                 (objc:invoke window "addChildWindow:ordered:" panel 1)))))
      ;; The echo area's rows are hidden while the palette shows them.
      (request-redraw))))

(defun palette-hides-row-p (line)
  "Whether LINE is one of the echo area's rows, which the palette shows."
  (let ((palette *palette-shown*))
    (and palette
         (<= (getf palette :echo-top) line
             (+ (getf palette :echo-top) (getf palette :echo-height) -1)))))
