;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;; This file defines all the definitions of keysyms (see key-event.lisp).
;;; These keysyms match those for X11.
;;;
;;; Written by Bill Chiles
;;; Modified by Blaine Burks.
;;;

(in-package :heml-internals)


;;; The IBM RT keyboard has X11 keysyms defined for the following modifier
;;; keys, but we leave them mapped to nil indicating that they are non-events
;;; to be ignored:
;;;    ctrl             65507
;;;    meta (left)      65513
;;;    meta (right)     65514
;;;    shift (left)     65505
;;;    shift (right)    65506
;;;    lock             65509
;;;


;;; Function keys for the RT.
;;;
(heml-ext:define-keysym 65470 "F1")
(heml-ext:define-keysym 65471 "F2")
(heml-ext:define-keysym 65472 "F3")
(heml-ext:define-keysym 65473 "F4")
(heml-ext:define-keysym 65474 "F5")
(heml-ext:define-keysym 65475 "F6")
(heml-ext:define-keysym 65476 "F7")
(heml-ext:define-keysym 65477 "F8")
(heml-ext:define-keysym 65478 "F9")
(heml-ext:define-keysym 65479 "F10")
(heml-ext:define-keysym 65480 "F11" "L1")
(heml-ext:define-keysym 65481 "F12" "L2")

;;; Function keys for the Sun (and other keyboards) -- L1-L10 and R1-R15.
;;;
(heml-ext:define-keysym 65482 "F13" "L3")
(heml-ext:define-keysym 65483 "F14" "L4")
(heml-ext:define-keysym 65484 "F15" "L5")
(heml-ext:define-keysym 65485 "F16" "L6")
(heml-ext:define-keysym 65486 "F17" "L7")
(heml-ext:define-keysym 65487 "F18" "L8")
(heml-ext:define-keysym 65488 "F19" "L9")
(heml-ext:define-keysym 65489 "F20" "L10")
(heml-ext:define-keysym 65490 "F21" "R1")
(heml-ext:define-keysym 65491 "F22" "R2")
(heml-ext:define-keysym 65492 "F23" "R3")
(heml-ext:define-keysym 65493 "F24" "R4")
(heml-ext:define-keysym 65494 "F25" "R5")
(heml-ext:define-keysym 65495 "F26" "R6")
(heml-ext:define-keysym 65496 "F27" "R7")
(heml-ext:define-keysym 65497 "F28" "R8")
(heml-ext:define-keysym 65498 "F29" "R9")
(heml-ext:define-keysym 65499 "F30" "R10")
(heml-ext:define-keysym 65500 "F31" "R11")
(heml-ext:define-keysym 65501 "F32" "R12")
(heml-ext:define-keysym 65502 "F33" "R13")
(heml-ext:define-keysym 65503 "F34" "R14")
(heml-ext:define-keysym 65504 "F35" "R15")

;;; Upper right key bank.
;;;
(heml-ext:define-keysym 65377 "Printscreen")
;; Couldn't type scroll lock.
(heml-ext:define-keysym 65299 "Pause")

;;; Middle right key bank.
;;;
(heml-ext:define-keysym 65379 "Insert")
(heml-ext:define-keysym 65535 "Delete" "Rubout" (string (code-char 127)))
(heml-ext:define-keysym 65360 "Home")
(heml-ext:define-keysym 65365 "Pageup")
(heml-ext:define-keysym 65367 "End")
(heml-ext:define-keysym 65366 "Pagedown")

;;; Arrows.
;;;
(heml-ext:define-keysym 65361 "Leftarrow")
(heml-ext:define-keysym 65362 "Uparrow")
(heml-ext:define-keysym 65364 "Downarrow")
(heml-ext:define-keysym 65363 "Rightarrow")

;;; Number pad.
;;;
(heml-ext:define-keysym 65407 "Numlock")
(heml-ext:define-keysym 65421 "Numpad\-Return" "Numpad\-Enter")      ;num-pad-enter
(heml-ext:define-keysym 65455 "Numpad/")                             ;num-pad-/
(heml-ext:define-keysym 65450 "Numpad*")                             ;num-pad-*
(heml-ext:define-keysym 65453 "Numpad-")                             ;num-pad--
(heml-ext:define-keysym 65451 "Numpad+")                             ;num-pad-+
(heml-ext:define-keysym 65456 "Numpad0")                             ;num-pad-0
(heml-ext:define-keysym 65457 "Numpad1")                             ;num-pad-1
(heml-ext:define-keysym 65458 "Numpad2")                             ;num-pad-2
(heml-ext:define-keysym 65459 "Numpad3")                             ;num-pad-3
(heml-ext:define-keysym 65460 "Numpad4")                             ;num-pad-4
(heml-ext:define-keysym 65461 "Numpad5")                             ;num-pad-5
(heml-ext:define-keysym 65462 "Numpad6")                             ;num-pad-6
(heml-ext:define-keysym 65463 "Numpad7")                             ;num-pad-7
(heml-ext:define-keysym 65464 "Numpad8")                             ;num-pad-8
(heml-ext:define-keysym 65465 "Numpad9")                             ;num-pad-9
(heml-ext:define-keysym 65454 "Numpad.")                             ;num-pad-.

;;; "Named" keys.
;;;
(heml-ext:define-keysym 65289 "Tab")
(heml-ext:define-keysym 65307 "Escape" "Altmode" "Alt")              ;escape
(heml-ext:define-keysym 65288 "Backspace")                           ;backspace
(heml-ext:define-keysym 65293 "Return" "Enter")                      ;enter
#+nil
;; 65512 = #xffe8 is Meta_R for me.  As per the comment on IBM RT at the
;; to of this file, it needs to be unmapped.
(heml-ext:define-keysym 65512 "Linefeed" "Action" "Newline")         ;action
(heml-ext:define-keysym 10 "Linefeed" "Action" "Newline")            ;action
(heml-ext:define-keysym 32 "Space" " ")

;;; Letters.
;;;
(heml-ext:define-keysym 97 "a") (heml-ext:define-keysym 65 "A")
(heml-ext:define-keysym 98 "b") (heml-ext:define-keysym 66 "B")
(heml-ext:define-keysym 99 "c") (heml-ext:define-keysym 67 "C")
(heml-ext:define-keysym 100 "d") (heml-ext:define-keysym 68 "D")
(heml-ext:define-keysym 101 "e") (heml-ext:define-keysym 69 "E")
(heml-ext:define-keysym 102 "f") (heml-ext:define-keysym 70 "F")
(heml-ext:define-keysym 103 "g") (heml-ext:define-keysym 71 "G")
(heml-ext:define-keysym 104 "h") (heml-ext:define-keysym 72 "H")
(heml-ext:define-keysym 105 "i") (heml-ext:define-keysym 73 "I")
(heml-ext:define-keysym 106 "j") (heml-ext:define-keysym 74 "J")
(heml-ext:define-keysym 107 "k") (heml-ext:define-keysym 75 "K")
(heml-ext:define-keysym 108 "l") (heml-ext:define-keysym 76 "L")
(heml-ext:define-keysym 109 "m") (heml-ext:define-keysym 77 "M")
(heml-ext:define-keysym 110 "n") (heml-ext:define-keysym 78 "N")
(heml-ext:define-keysym 111 "o") (heml-ext:define-keysym 79 "O")
(heml-ext:define-keysym 112 "p") (heml-ext:define-keysym 80 "P")
(heml-ext:define-keysym 113 "q") (heml-ext:define-keysym 81 "Q")
(heml-ext:define-keysym 114 "r") (heml-ext:define-keysym 82 "R")
(heml-ext:define-keysym 115 "s") (heml-ext:define-keysym 83 "S")
(heml-ext:define-keysym 116 "t") (heml-ext:define-keysym 84 "T")
(heml-ext:define-keysym 117 "u") (heml-ext:define-keysym 85 "U")
(heml-ext:define-keysym 118 "v") (heml-ext:define-keysym 86 "V")
(heml-ext:define-keysym 119 "w") (heml-ext:define-keysym 87 "W")
(heml-ext:define-keysym 120 "x") (heml-ext:define-keysym 88 "X")
(heml-ext:define-keysym 121 "y") (heml-ext:define-keysym 89 "Y")
(heml-ext:define-keysym 122 "z") (heml-ext:define-keysym 90 "Z")

;;; Standard number keys.
;;;
(heml-ext:define-keysym 49 "1") (heml-ext:define-keysym 33 "!")
(heml-ext:define-keysym 50 "2") (heml-ext:define-keysym 64 "@")
(heml-ext:define-keysym 51 "3") (heml-ext:define-keysym 35 "#")
(heml-ext:define-keysym 52 "4") (heml-ext:define-keysym 36 "$")
(heml-ext:define-keysym 53 "5") (heml-ext:define-keysym 37 "%")
(heml-ext:define-keysym 54 "6") (heml-ext:define-keysym 94 "^")
(heml-ext:define-keysym 55 "7") (heml-ext:define-keysym 38 "&")
(heml-ext:define-keysym 56 "8") (heml-ext:define-keysym 42 "*")
(heml-ext:define-keysym 57 "9") (heml-ext:define-keysym 40 "(")
(heml-ext:define-keysym 48 "0") (heml-ext:define-keysym 41 ")")

;;; "Standard" symbol keys.
;;;
(heml-ext:define-keysym 96 "`") (heml-ext:define-keysym 126 "~")
(heml-ext:define-keysym 45 "-") (heml-ext:define-keysym 95 "_")
(heml-ext:define-keysym 61 "=") (heml-ext:define-keysym 43 "+")
(heml-ext:define-keysym 91 "[") (heml-ext:define-keysym 123 "{")
(heml-ext:define-keysym 93 "]") (heml-ext:define-keysym 125 "}")
(heml-ext:define-keysym 92 "\\") (heml-ext:define-keysym 124 "|")
(heml-ext:define-keysym 59 ";") (heml-ext:define-keysym 58 ":")
(heml-ext:define-keysym 39 "'") (heml-ext:define-keysym 34 "\"")
(heml-ext:define-keysym 44 ",") (heml-ext:define-keysym 60 "<")
(heml-ext:define-keysym 46 ".") (heml-ext:define-keysym 62 ">")
(heml-ext:define-keysym 47 "/") (heml-ext:define-keysym 63 "?")

;;; Standard Mouse keysyms.
;;;
(heml-ext::define-mouse-keysym 1 25601 "Leftdown" "Super" :button-press)
(heml-ext::define-mouse-keysym 1 25602 "Leftup" "Super" :button-release)

(heml-ext::define-mouse-keysym 2 25603 "Middledown" "Super" :button-press)
(heml-ext::define-mouse-keysym 2 25604 "Middleup" "Super" :button-release)

(heml-ext::define-mouse-keysym 3 25605 "Rightdown" "Super" :button-press)
(heml-ext::define-mouse-keysym 3 25606 "Rightup" "Super" :button-release)

;;; Pointer motion with the left button down, and the scroll wheel, one
;;; line to an event.  No X button maps to these; a backend that sees them
;;; queues them itself (the Cocoa backend does).
;;;
(heml-ext:define-keysym 25607 "Leftdrag")
(heml-ext:define-keysym 25608 "Scrollup")
(heml-ext:define-keysym 25609 "Scrolldown")

;;; A double and a triple click of the left button, as a backend that knows
;;; the click count reports them in place of a second or third Leftdown.
;;;
(heml-ext:define-keysym 25611 "Doubleleftdown")
(heml-ext:define-keysym 25612 "Tripleleftdown")

;;; What a backend queues to have a command from its menus run by the
;;; command loop; it keeps the command to run itself.
;;;
(heml-ext:define-keysym 25610 "Menucommand")

;;; Sun keyboard.
;;;
(heml-ext:define-keysym 65387 "break")                       ;alternate (Sun).
;(heml-ext:define-keysym 65290 "linefeed")



;;;; SETFs of KEY-EVANT-CHAR and CHAR-KEY-EVENT.

;;; Converting ASCII control characters to Common Lisp control characters:
;;; ASCII control character codes are separated from the codes of the
;;; "non-controlified" characters by the code of atsign.  The ASCII control
;;; character codes range from ^@ (0) through ^_ (one less than the code of
;;; space).  We iterate over this range adding the ASCII code of atsign to
;;; get the "non-controlified" character code.  With each of these, we turn
;;; the code into a Common Lisp character and set its :control bit.  Certain
;;; ASCII control characters have to be translated to special Common Lisp
;;; characters outside of the loop.
;;;    With the advent of Heml running under X, and all the key bindings
;;; changing, we also downcase each Common Lisp character (where normally
;;; control characters come in upcased) in an effort to obtain normal command
;;; bindings.  Commands bound to uppercase modified characters will not be
;;; accessible to terminal interaction.
;;;
(let ((@-code (char-code #\@)))
  (dotimes (i (char-code #\space))
    (setf (heml-ext:char-key-event (code-char i))
          (heml-ext::make-key-event (string (char-downcase (code-char (+ i @-code))))
                               (heml-ext:key-event-modifier-mask "control")))))
(setf (heml-ext:char-key-event (code-char 9)) (heml-ext::make-key-event #k"Tab"))
(setf (heml-ext:char-key-event (code-char 10)) (heml-ext::make-key-event #k"Linefeed"))
(setf (heml-ext:char-key-event (code-char 13)) (heml-ext::make-key-event #k"Return"))
(setf (heml-ext:char-key-event (code-char 27)) (heml-ext::make-key-event #k"Alt"))
;;;
;;; Other ASCII codes are exactly the same as the Common Lisp codes.
;;;
(do ((i (char-code #\space) (1+ i)))
    ((= i 128))
  (setf (heml-ext:char-key-event (code-char i))
        (heml-ext::make-key-event (string (code-char i)))))

;;; This makes KEY-EVENT-CHAR the inverse of CHAR-KEY-EVENT from the start.
;;; It need not be this way, but it is.
;;;
(dotimes (i 128)
  (let ((character (code-char i)))
    (setf (heml-ext::key-event-char (heml-ext:char-key-event character)) character)))

;;; Since we treated these characters specially above when setting
;;; HEML-EXT:CHAR-KEY-EVENT above, we must set these HEML-EXT:KEY-EVENT-CHAR's specially
;;; to make quoting characters into Heml buffers more obvious for users.
;;;
(setf (heml-ext:key-event-char #k"C-h") #\backspace)
(setf (heml-ext:key-event-char #k"C-i") #\tab)
(setf (heml-ext:key-event-char #k"C-j") #\linefeed)
(setf (heml-ext:key-event-char #k"C-m") #\return)
