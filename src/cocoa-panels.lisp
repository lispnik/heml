;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Popups as panels: completions, a call's signature, a server's hover text
;;; and a menu of choices are shown in a window of their own -- borderless,
;;; translucent and rounded, as the Mac's own menus and Xcode's completion
;;; list are -- rather than as rows of text over the editor's.
;;;
;;; The editor thread still owns the popup (*POPUP*, popup.lisp) and reads
;;; its keys; the device only tells the core not to lay it over the text
;;; (*DEVICE-DRAWS-POPUPS*), and each frame carries a description of it
;;; (POPUP-DESCRIPTOR, cocoa-device.lisp), which the main thread shows.

(in-package :heml.cocoa)

(objc:define-objc-class popup-view ()
  ()
  (:objc-class-name "HemlPopupView")
  (:objc-superclass-name "NSView"))

(objc:define-objc-method ("isFlipped" objc:objc-bool) ((self popup-view))
  t)

(defvar *popup-panel* nil
  "The panel the popup is shown in, made the first time one is shown.")

(defvar *popup-panel-view* nil)

(defvar *popup-shown* nil
  "The description of the popup the panel shows, or NIL.")

(defparameter *popup-padding* 4
  "Points between a panel's edge and its rows.")

(defun kind-badge (note)
  "The letter and colour of the icon for a completion of the kind NOTE."
  (let ((kind (string-downcase (string-trim " " note))))
    (flet ((is (&rest words)
             (some (lambda (word) (search word kind)) words)))
      (cond ((is "macro") (values "#" "systemOrangeColor"))
            ((is "function" "method" "constructor") (values "ƒ" "systemPurpleColor"))
            ((is "class" "struct" "type" "interface" "enum") (values "C" "systemBlueColor"))
            ((is "variable" "field" "property" "parameter") (values "V" "systemGreenColor"))
            ((is "constant" "keyword") (values "K" "systemPinkColor"))
            ((is "module" "package" "namespace") (values "M" "systemTealColor"))
            ((is "file" "directory" "folder") (values "F" "systemBrownColor"))
            ((plusp (length kind)) (values (string-upcase (subseq kind 0 1)) "systemGrayColor"))
            (t (values nil nil))))))

(defun popup-row-height (display)
  (display-char-height display))

(defun popup-text-left (display popup)
  "Where a row's text starts in the panel: after the icons, when it has any."
  (+ *popup-padding* 2
     (if (some #'identity (getf popup :notes))
         (+ (popup-row-height display) 2)
         0)))

(defun popup-size (display popup)
  "The panel's width and height, in points."
  (let* ((rows (getf popup :rows))
         (columns (reduce #'max rows :key (lambda (row) (length (string-right-trim " " (first row))))
                                     :initial-value 1)))
    (values (+ (popup-text-left display popup)
               (* columns (display-char-width display))
               *popup-padding* 4)
            (+ (* 2 *popup-padding*) (* (length rows) (popup-row-height display))))))

(defun draw-popup (display popup width)
  (let ((height (popup-row-height display))
        (left (popup-text-left display popup))
        (rows (getf popup :rows))
        (notes (getf popup :notes)))
    (loop for (text selected) in rows
          for note = (pop notes)
          for i from 0
          for y = (+ *popup-padding* (* i height))
          do (when selected
               (let ((path (objc:invoke "NSBezierPath" "bezierPathWithRoundedRect:xRadius:yRadius:"
                                        (vector (df *popup-padding*) (df y)
                                                (df (- width (* 2 *popup-padding*))) (df height))
                                        4d0 4d0)))
                 (objc:invoke (ns-color display "controlAccentColor") "set")
                 (objc:invoke path "fill")))
             (when note
               (multiple-value-bind (letter color) (kind-badge note)
                 (when letter
                   (let* ((size (- height 4))
                          (x (+ *popup-padding* 3))
                          (path (objc:invoke "NSBezierPath" "bezierPathWithRoundedRect:xRadius:yRadius:"
                                             (vector (df x) (df (+ y 2)) (df size) (df size))
                                             3d0 3d0)))
                     (objc:invoke (ns-color display color) "set")
                     (objc:invoke path "fill")
                     (draw-popup-string display letter
                                        (+ x (/ (- size (display-char-width display)) 2)) y
                                        "whiteColor")))))
             (draw-popup-string display (string-right-trim " " (string-left-trim " " text))
                                left y
                                (if selected "alternateSelectedControlTextColor" "labelColor")))
    ;; Parts of rows marked: the argument being typed in a signature.
    (loop for (row start end) in (getf popup :highlights)
          for entry = (nth row rows)
          when entry
            do (let* ((text (first entry))
                      ;; The row is drawn without its leading spaces.
                      (offset (or (position #\Space text :test-not #'char=) 0))
                      (from (max 0 (- start offset)))
                      (to (- (min (length text) end) offset)))
                 (when (< from to)
                   (fill-rect (ns-color display "controlAccentColor")
                              (+ left (* from (display-char-width display)))
                              (+ *popup-padding* (* row height) height -2)
                              (* (- to from) (display-char-width display)) 2))))))

(defun draw-popup-string (display string x y color-name)
  (objc:invoke (objc:string-to-ns-string string) "drawAtPoint:withAttributes:"
               (vector (df x) (df y))
               (text-attributes display color-name nil)))

(objc:define-objc-method ("drawRect:" :void)
    ((self popup-view pointer) (dirty cocoa:ns-rect))
  (declare (ignore dirty))
  (handler-case
      (let ((display *display*)
            (popup *popup-shown*))
        (when (and display popup)
          (draw-popup display popup (aref (objc:invoke pointer "bounds") 2))))
    (error (condition) (log-error "popup drawRect:" condition))))

(defconstant +borderless+ 0)
(defconstant +nonactivating-panel+ (ash 1 7))
(defconstant +material-menu+ 5)

(defun ensure-popup-panel ()
  (or *popup-panel*
      (let* ((rect (vector 0d0 0d0 100d0 100d0))
             (panel (objc:invoke (objc:invoke "NSPanel" "alloc")
                                 "initWithContentRect:styleMask:backing:defer:"
                                 rect (logior +borderless+ +nonactivating-panel+)
                                 +backing-store-buffered+ nil))
             (effect (objc:invoke (objc:invoke "NSVisualEffectView" "alloc")
                                  "initWithFrame:" rect))
             (view (objc:objc-object-pointer (make-instance 'popup-view))))
        (objc:invoke panel "setReleasedWhenClosed:" nil)
        (objc:invoke panel "setOpaque:" nil)
        (objc:invoke panel "setBackgroundColor:" (objc:invoke "NSColor" "clearColor"))
        (objc:invoke panel "setHasShadow:" t)
        (objc:invoke panel "setIgnoresMouseEvents:" t)
        (objc:invoke effect "setMaterial:" +material-menu+)
        (objc:invoke effect "setState:" 1)          ; active, whatever has the focus
        (objc:invoke effect "setBlendingMode:" 0)   ; what is behind the window
        (objc:invoke effect "setWantsLayer:" t)
        (let ((layer (objc:invoke effect "layer")))
          (objc:invoke layer "setCornerRadius:" 7d0)
          (objc:invoke layer "setMasksToBounds:" t))
        (objc:invoke view "setFrame:" rect)
        (objc:invoke view "setAutoresizingMask:" 18) ; width and height
        (objc:invoke effect "addSubview:" view)
        (objc:invoke panel "setContentView:" effect)
        (setf *popup-panel-view* view
              *popup-panel* panel))))

(defun show-popup-panel (popup)
  "Show POPUP, a description from the editor, in the panel, or put the
   panel away for NIL.  On the main thread."
  (let* ((display *display*)
         (window (and display (display-window display))))
    (when window
      (setf *popup-shown* popup)
      (cond ((null popup)
             (when *popup-panel*
               (objc:invoke window "removeChildWindow:" *popup-panel*)
               (objc:invoke *popup-panel* "orderOut:" (cffi:null-pointer))))
            (t
             (let ((panel (ensure-popup-panel)))
               (multiple-value-bind (width height) (popup-size display popup)
                 ;; Its first row over the cell the editor put it at.
                 (let* ((cell (vector (df (- (cell-x display (getf popup :column)) *popup-padding*))
                                      (df (- (cell-y display (getf popup :line)) *popup-padding*))
                                      (df width) (df height)))
                        (in-window (objc:invoke (display-view display) "convertRect:toView:"
                                                cell (cffi:null-pointer)))
                        (on-screen (objc:invoke window "convertRectToScreen:" in-window)))
                   (objc:invoke panel "setFrame:display:" on-screen t)
                   (objc:invoke *popup-panel-view* "setNeedsDisplay:" t)
                   (unless (objc:invoke-bool panel "isVisible")
                     (objc:invoke window "addChildWindow:ordered:" panel 1))))))))))
