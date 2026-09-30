;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;; Some bindings:
;;;

(in-package :heml)



;;;; Default key translations:

;;; This page defines prefix characters that set specified modifier bits on
;;; the next character typed.
;;;
(setf (key-translation #k"escape") '(:bits :meta))
(setf (key-translation #k"control-[") '(:bits :meta))
(setf (key-translation #k"control-z") '(:bits :control :meta))
(setf (key-translation #k"control-Z") '(:bits :control :meta))
(setf (key-translation #k"control-^") '(:bits :control))
(setf (key-translation #k"control-c") '(:bits :hyper))
(setf (key-translation #k"control-C") '(:bits :hyper))



;;;; Most every binding.

;;; Self insert letters:
;;;
(heml-ext:do-alpha-key-events (key-event :both)
                                 (bind-key "Self Insert" key-event))

;;; ... and every other character, as it gets a key-event of its own the
;;; first time it is typed (HEML-EXT:CHARACTER-KEY-EVENT).
;;;
(setf heml-ext::*new-character-key-event-hook*
      (lambda (key-event) (bind-key "Self Insert" key-event)))

(bind-key "Beginning of Line" #k"control-a")
(bind-key "Beginning of Line" #k"home")
(bind-key "Delete Next Character" #k"control-d")
(bind-key "End of Line" #k"control-e")
(bind-key "End of Line" #k"end")
(bind-key "Forward Character" #k"control-f")
(bind-key "Forward Character" #k"rightarrow")
(bind-key "Backward Character" #k"control-b")
(bind-key "Backward Character" #k"leftarrow")
(bind-key "Kill Line" #k"control-k")
(bind-key "Refresh Screen" #k"control-l")
(bind-key "Next Line" #k"control-n")
(bind-key "Next Line" #k"downarrow")
(bind-key "Previous Line" #k"control-p")
(bind-key "Previous Line" #k"uparrow")
(bind-key "Query Replace" #k"meta-%")
(bind-key "Reverse Incremental Search" #k"control-r")
(bind-key "Incremental Search" #k"control-s")
(bind-key "Forward Search" #k"meta-s")
(bind-key "Reverse Search" #k"meta-r")
(bind-key "Transpose Characters" #k"control-t")
(bind-key "Universal Argument" #k"control-u")
(bind-key "Scroll Window Down" #k"control-v")
(bind-key "Scroll Window Up" #k"meta-v")
(bind-key "Scroll Next Window Down" #k"control-meta-v")
(bind-key "Scroll Next Window Up" #k"control-meta-V")

(bind-key "Help" #k"control-h")
(bind-key "Help" #k"control-_")
(bind-key "Describe Key" #k"meta-?")


(bind-key "Here to Top of Window" #k"leftdown")
(bind-key "Do Nothing" #k"leftup")
(bind-key "Top Line to Here" #k"rightdown")
(bind-key "Do Nothing" #k"rightup")
(bind-key "Point to Here" #k"middledown")
(bind-key "Point to Here" #k"super-leftdown")
(bind-key "Generic Pointer Up" #k"middleup")
(bind-key "Generic Pointer Up" #k"super-leftup")
(bind-key "Do Nothing" #k"super-rightup")
(bind-key "Insert Kill Buffer" #k"super-rightdown")
(bind-key "Mouse Drag Region" #k"leftdrag")
(bind-key "Mouse Drag Region" #k"shift-leftdrag")
(bind-key "Mouse Scroll Up" #k"scrollup")
(bind-key "Mouse Scroll Down" #k"scrolldown")


(bind-key "Insert File" #k"control-x control-r")
(bind-key "Save File" #k"control-x control-s")
(bind-key "Visit File" #k"control-x control-v")
(bind-key "Write File" #k"control-x control-w")
(bind-key "Find File" #k"control-x control-f")
(bind-key "Backup File" #k"control-x meta-b")
(bind-key "Save All Files" #k"control-x s")
(bind-key "Save All Files and Exit" #k"control-x control-c")

(bind-key "Bufed" #k"control-x control-b")
(bind-key "Buffer Not Modified" #k"meta-~")
(bind-key "Check Buffer Modified" #k"control-x ~")
(bind-key "Select Buffer" #k"control-x b")
(bind-key "Select Previous Buffer" #k"control-meta-l")
(bind-key "Circulate Buffers" #k"control-meta-L")
(bind-key "Create Buffer" #k"control-x meta-b")
(bind-key "Kill Buffer" #k"control-x k")
(bind-key "Select Random Typeout Buffer" #k"hyper-t")

(bind-key "Next Window" #k"control-x n")
(bind-key "Next Window" #k"control-x o")
(bind-key "Previous Window" #k"control-x p")
(bind-key "Split Window" #k"control-x 2")
(bind-key "Split Window Horizontally" #k"control-x 3")
(bind-key "Enlarge Window Horizontally" #k"control-x }")
(bind-key "Shrink Window Horizontally" #k"control-x {")
(bind-key "Balance Windows" #k"control-x +")
(bind-key "Enlarge Window" #k"control-x ^")
(bind-key "New Window" #k"control-x 5 2")
(bind-key "Delete Window" #k"control-x 0")
(bind-key "Delete Other Windows" #k"control-x 1")
#+nil (bind-key "Line to Top of Window" #k"meta-!")
(bind-key "Line to Center of Window" #k"meta-#")
(bind-key "Top of Window" #k"meta-,")
#+nil (bind-key "Bottom of Window" #k"meta-.")

#+nil (bind-key "Exit Heml" #k"control-x control-c")
(bind-key "Exit Recursive Edit" #k"control-meta-z")
(bind-key "Abort Recursive Edit" #k"control-]")

(bind-key "Delete Previous Character" #k"backspace")
(bind-key "Delete Next Character" #k"delete")
(bind-key "Kill Next Word" #k"meta-d")
(bind-key "Kill Next Word" #k"meta-delete")
(bind-key "Kill Previous Word" #k"meta-backspace")
(bind-key "Exchange Point and Mark" #k"control-x control-x")
(bind-key "Mark Whole Buffer" #k"control-x h")
(bind-key "Set/Pop Mark" #k"control-@")
(bind-key "Set/Pop Mark" #k"control-space")
(bind-key "Pop and Goto Mark" #k"meta-space")
(bind-key "Pop and Goto Mark" #k"meta-@")
(bind-key "Pop Mark" #k"control-meta-space") ;#k"control-meta-@" = "Mark Form".
(bind-key "Kill Region" #k"control-w")
(bind-key "Save Region" #k"meta-w")
(bind-key "Un-Kill" #k"control-y")
(bind-key "Rotate Kill Ring" #k"meta-y")

(bind-key "Forward Word" #k"meta-f")
(bind-key "Backward Word" #k"meta-b")

(bind-key "Forward Paragraph" #k"meta-]")
(bind-key "Forward Sentence" #k"meta-e")
(bind-key "Backward Paragraph" #k"meta-[")
(bind-key "Backward Sentence" #k"meta-a")

(bind-key "Mark Paragraph" #k"meta-h")

(bind-key "Forward Kill Sentence" #k"meta-k")
(bind-key "Forward Kill Sentence" #k"control-x delete")
(bind-key "Backward Kill Sentence" #k"control-x backspace")

(bind-key "Beginning of Buffer" #k"meta-\<")
(bind-key "End of Buffer" #k"meta-\>")
(bind-key "Mark to Beginning of Buffer" #k"control-\<")
(bind-key "Mark to End of Buffer" #k"control-\>")

(bind-key "Extended Command" #k"meta-x")
(bind-key "Eval Expresion" #k"meta-:")

(bind-key "Uppercase Word" #k"meta-u")
(bind-key "Lowercase Word" #k"meta-l")
(bind-key "Capitalize Word" #k"meta-c")

(bind-key "Previous Page" #k"control-x [")
(bind-key "Next Page" #k"control-x ]")
(bind-key "Mark Page" #k"control-x control-p")
(bind-key "Count Lines Page" #k"control-x l")



;;;; Argument Digit and Negative Argument.

(bind-key "Negative Argument" #k"meta-\-")
(bind-key "Argument Digit" #k"meta-0")
(bind-key "Argument Digit" #k"meta-1")
(bind-key "Argument Digit" #k"meta-2")
(bind-key "Argument Digit" #k"meta-3")
(bind-key "Argument Digit" #k"meta-4")
(bind-key "Argument Digit" #k"meta-5")
(bind-key "Argument Digit" #k"meta-6")
(bind-key "Argument Digit" #k"meta-7")
(bind-key "Argument Digit" #k"meta-8")
(bind-key "Argument Digit" #k"meta-9")
(bind-key "Negative Argument" #k"control-\-")
(bind-key "Argument Digit" #k"control-0")
(bind-key "Argument Digit" #k"control-1")
(bind-key "Argument Digit" #k"control-2")
(bind-key "Argument Digit" #k"control-3")
(bind-key "Argument Digit" #k"control-4")
(bind-key "Argument Digit" #k"control-5")
(bind-key "Argument Digit" #k"control-6")
(bind-key "Argument Digit" #k"control-7")
(bind-key "Argument Digit" #k"control-8")
(bind-key "Argument Digit" #k"control-9")
(bind-key "Negative Argument" #k"control-meta-\-")
(bind-key "Argument Digit" #k"control-meta-0")
(bind-key "Argument Digit" #k"control-meta-1")
(bind-key "Argument Digit" #k"control-meta-2")
(bind-key "Argument Digit" #k"control-meta-3")
(bind-key "Argument Digit" #k"control-meta-4")
(bind-key "Argument Digit" #k"control-meta-5")
(bind-key "Argument Digit" #k"control-meta-6")
(bind-key "Argument Digit" #k"control-meta-7")
(bind-key "Argument Digit" #k"control-meta-8")
(bind-key "Argument Digit" #k"control-meta-9")


;;;; Self Insert and Quoted Insert.

(bind-key "Quoted Insert" #k"control-q")

(bind-key "Self Insert" #k"space")
(bind-key "Self Insert" #k"!")
(bind-key "Self Insert" #k"@")
(bind-key "Self Insert" #k"#")
(bind-key "Self Insert" #k"$")
(bind-key "Self Insert" #k"%")
(bind-key "Self Insert" #k"^")
(bind-key "Self Insert" #k"&")
(bind-key "Self Insert" #k"*")
(bind-key "Self Insert" #k"(")
(bind-key "Self Insert" #k")")
(bind-key "Self Insert" #k"_")
(bind-key "Self Insert" #k"+")
(bind-key "Self Insert" #k"~")
(bind-key "Self Insert" #k"1")
(bind-key "Self Insert" #k"2")
(bind-key "Self Insert" #k"3")
(bind-key "Self Insert" #k"4")
(bind-key "Self Insert" #k"5")
(bind-key "Self Insert" #k"6")
(bind-key "Self Insert" #k"7")
(bind-key "Self Insert" #k"8")
(bind-key "Self Insert" #k"9")
(bind-key "Self Insert" #k"0")
(bind-key "Self Insert" #k"[")
(bind-key "Self Insert" #k"]")
(bind-key "Self Insert" #k"\\")
(bind-key "Self Insert" #k"|")
(bind-key "Self Insert" #k":")
(bind-key "Self Insert" #k";")
(bind-key "Self Insert" #k"\"")
(bind-key "Self Insert" #k"'")
(bind-key "Self Insert" #k"\-")
(bind-key "Self Insert" #k"=")
(bind-key "Self Insert" #k"`")
(bind-key "Self Insert" #k"\<")
(bind-key "Self Insert" #k"\>")
(bind-key "Self Insert" #k",")
(bind-key "Self Insert" #k".")
(bind-key "Self Insert" #k"?")
(bind-key "Self Insert" #k"/")
(bind-key "Self Insert" #k"{")
(bind-key "Self Insert" #k"}")



;;;; Echo Area.

;;; Basic echo-area commands.
;;;
(bind-key "Help on Parse" #k"control-h" :mode "Echo Area")
(bind-key "Help on Parse" #k"control-_" :mode "Echo Area")
(bind-key "Help on Parse" #k"control-Delete" :mode "Echo Area")

;; disabled, because it breaks the use of escape as meta in the tty
;;(bind-key "Complete Keyword" #k"escape" :mode "Echo Area")

(bind-key "Complete Field" #k"control-i" :mode "Echo Area")
(bind-key "Complete Field" #k"tab" :mode "Echo Area")
(bind-key "Complete Field" #k"space" :mode "Echo Area")
(bind-key "Confirm Parse" #k"return" :mode "Echo Area")

;;; Rebind some standard commands to behave better.
;;;
(bind-key "Kill Parse" #k"control-u" :mode "Echo Area")
(bind-key "Echo Area Delete Previous Character" #k"backspace" :mode "Echo Area")
(bind-key "Echo Area Kill Previous Word" #k"meta-h" :mode "Echo Area")
(bind-key "Echo Area Kill Previous Word" #k"meta-backspace" :mode "Echo Area")
(bind-key "Echo Area Kill Previous Word" #k"control-w" :mode "Echo Area")
(bind-key "Beginning of Parse" #k"control-a" :mode "Echo Area")
(bind-key "Beginning of Parse" #k"home" :mode "Echo Area")
(bind-key "Beginning of Parse" #k"meta-\<" :mode "Echo Area")
(bind-key "Echo Area Backward Character" #k"control-b" :mode "Echo Area")
(bind-key "Echo Area Backward Word" #k"meta-b" :mode "Echo Area")
(bind-key "Next Parse" #k"meta-n" :mode "Echo Area")
(bind-key "Previous Parse" #k"meta-p" :mode "Echo Area")

;;; Remove some dangerous standard bindings.
;;;
(bind-key "Illegal" #k"control-x" :mode "Echo Area")
(bind-key "Illegal" #k"control-meta-c" :mode "Echo Area")
(bind-key "Illegal" #k"control-meta-s" :mode "Echo Area")
(bind-key "Illegal" #k"control-meta-l" :mode "Echo Area")
(bind-key "Illegal" #k"meta-x" :mode "Echo Area")
(bind-key "Illegal" #k"control-s" :mode "Echo Area")
(bind-key "Illegal" #k"control-r" :mode "Echo Area")
(bind-key "Illegal" #k"hyper-t" :mode "Echo Area")
(bind-key "Illegal" #k"middledown" :mode "Echo Area")
(bind-key "Do Nothing" #k"middleup" :mode "Echo Area")
(bind-key "Illegal" #k"super-leftdown" :mode "Echo Area")
(bind-key "Do Nothing" #k"super-leftup" :mode "Echo Area")
(bind-key "Illegal" #k"super-rightdown" :mode "Echo Area")
(bind-key "Do Nothing" #k"super-rightup" :mode "Echo Area")



;;;; Eval and Editor Modes.
(bind-key "Confirm Eval Input" #k"return" :mode "Eval")
(bind-key "Previous Interactive Input" #k"meta-p" :mode "Eval")
(bind-key "Search Previous Interactive Input" #k"meta-P" :mode "Eval")
(bind-key "Next Interactive Input" #k"meta-n" :mode "Eval")
(bind-key "Kill Interactive Input" #k"meta-i" :mode "Eval")
(bind-key "Abort Eval Input" #k"control-meta-i" :mode "Eval")
(bind-key "Interactive Beginning of Line" #k"control-a" :mode "Eval")
(bind-key "Interactive Beginning of Line" #k"home" :mode "Eval")
(bind-key "Reenter Interactive Input" #k"control-return" :mode "Eval")
(bind-key "Clear Eval Buffer" #k"control-c meta-o" :mode "Eval")

(bind-key "Editor Evaluate Expression" #k"control-meta-escape")
(bind-key "Editor Evaluate Expression" #k"meta-escape"  :mode "Editor")
(bind-key "Editor Evaluate Defun" #k"control-x control-e" :mode "Editor")
(bind-key "Editor Compile Defun" #k"control-c control-c" :mode "Editor")
(bind-key "Editor Macroexpand Expression" #k"control-m" :mode "Editor")
(bind-key "Editor Describe Function Call" #k"control-meta-A" :mode "Editor")
(bind-key "Editor Describe Symbol" #k"control-meta-S" :mode "Editor")
(bind-key "Editor Fuzzy Complete Symbol" #k"control-c meta-i" :mode "Editor")

;;;; Typescript.
(bind-key "Confirm Typescript Input" #k"return" :mode "Typescript")
(bind-key "Interactive Beginning of Line" #k"control-a" :mode "Typescript")
(bind-key "Interactive Beginning of Line" #k"home" :mode "Typescript")
(bind-key "Kill Interactive Input" #k"meta-i" :mode "Typescript")
(bind-key "Previous Interactive Input" #k"meta-p" :mode "Typescript")
(bind-key "Search Previous Interactive Input" #k"meta-P" :mode "Typescript")
(bind-key "Next Interactive Input" #k"meta-n" :mode "Typescript")
(bind-key "Reenter Interactive Input" #k"control-return" :mode "Typescript")
(bind-key "Typescript Slave Break" #k"hyper-b" :mode "Typescript")
(bind-key "Typescript Slave to Top Level" #k"hyper-g" :mode "Typescript")
(bind-key "Typescript Slave Status" #k"hyper-s" :mode "Typescript")
(bind-key "Select Slave" #k"control-meta-\c")
(bind-key "Select Background" #k"control-meta-C")
(bind-key "Clear Typescript Buffer" #k"control-c meta-o" :mode "Typescript")


;;;; Lisp (some).

(bind-key "Indent Form" #k"control-meta-q")
(bind-key "Fill Lisp Comment Paragraph" #k"meta-q" :mode "Lisp")
(bind-key "Defindent" #k"control-meta-#")
(bind-key "Beginning of Defun" #k"control-meta-[")
(bind-key "End of Defun" #k"control-meta-]")
(bind-key "Beginning of Defun" #k"control-meta-a")
(bind-key "End of Defun" #k"control-meta-e")
(bind-key "Forward Form" #k"control-meta-f")
(bind-key "Backward Form" #k"control-meta-b")
(bind-key "Forward List" #k"control-meta-n")
(bind-key "Backward List" #k"control-meta-p")
(bind-key "Transpose Forms" #k"control-meta-t")
(bind-key "Forward Kill Form" #k"control-meta-k")
(bind-key "Forward Kill Form" #k"control-meta-delete")
(bind-key "Backward Kill Form" #k"control-meta-backspace")
(bind-key "Mark Form" #k"control-meta-@")
(bind-key "Mark Defun" #k"control-meta-h")
(bind-key "Insert ()" #k"meta-(")
(bind-key "Move over )" #k"meta-)")
(bind-key "Backward Up List" #k"control-meta-(")
(bind-key "Backward Up List" #k"control-meta-u")
(bind-key "Forward Up List" #k"control-meta-)")
(bind-key "Down List" #k"control-meta-d")
(bind-key "Extract List" #k"control-meta-x")
(bind-key "Lisp Insert )" #k")" :mode "Lisp")
(bind-key "Delete Previous Character Expanding Tabs" #k"backspace" :mode "Lisp")

(bind-key "Evaluate Expression" #k"meta-escape")
(bind-key "Evaluate Defun" #k"control-x control-e")
(bind-key "Compile Defun" #k"control-c control-c")
(bind-key "Compile Buffer File" #k"control-c control-k")
(bind-key "Macroexpand Expression" #k"control-M")

(bind-key "Describe Function Call" #k"control-meta-A")
(bind-key "Describe Symbol" #k"control-meta-S")
(bind-key "Goto Definition" #k"control-meta-F")

(bind-key "Fuzzy Complete Symbol" #k"control-c meta-i" :mode "Lisp")



;;;; Debug mode

(bind-key "Debug Quit" #k"q" :mode "Debug")


;;;; More Miscellaneous bindings.

(bind-key "Open Line" #k"Control-o")
(bind-key "New Line" #k"return")
(bind-key "New Line" #k"control-m")

(bind-key "Transpose Words" #k"meta-t")
(bind-key "Transpose Lines" #k"control-x control-t")
(bind-key "Transpose Regions" #k"control-x t")

(bind-key "Uppercase Region" #k"control-x control-u")
(bind-key "Lowercase Region" #k"control-x control-l")

(bind-key "Delete Indentation" #k"meta-^")
(bind-key "Delete Indentation" #k"control-meta-^")
(bind-key "Delete Horizontal Space" #k"meta-\\")
(bind-key "Delete Blank Lines" #k"control-x control-o" :global)
(bind-key "Just One Space" #k"meta-\|")
(bind-key "Back to Indentation" #k"meta-m")
(bind-key "Back to Indentation" #k"control-meta-m")
(bind-key "Indent Rigidly" #k"control-x tab")
(bind-key "Indent Rigidly" #k"control-x control-i")

(bind-key "Indent New Line" #k"linefeed")
(bind-key "Indent New Line" #k"control-j")
(bind-key "Indent Or Complete" #k"tab")
(bind-key "Indent Or Complete" #k"control-i")
(bind-key "Indent Region" #k"control-meta-\\")
(bind-key "Quote Tab" #k"meta-tab")

(bind-key "Directory" #k"control-x control-\d")
(bind-key "Verbose Directory" #k"control-x control-D")

(bind-key "Activate Region" #k"control-x control-@")
(bind-key "Activate Region" #k"control-x control-space")

;; (bind-key "Save Position" #k"control-x s")
;; (bind-key "Jump to Saved Position" #k"control-x j")
(bind-key "Put Register" #k"control-x x")
(bind-key "Get Register" #k"control-x g")

(bind-key "Delete Previous Character Expanding Tabs" #k"backspace"
          :mode "Pascal")
(bind-key "Insert Close Bracket" #k")" :mode "Pascal")
(bind-key "Insert Close Bracket" #k"]" :mode "Pascal")
(bind-key "Insert Close Bracket" #k"}" :mode "Pascal")


;;;; Auto Fill Mode.

(bind-key "Fill Paragraph" #k"meta-q")
(bind-key "Fill Region" #k"meta-g")
(bind-key "Set Fill Prefix" #k"control-x .")
(bind-key "Set Fill Column" #k"control-x f")
(bind-key "Auto Fill Return" #k"return" :mode "Fill")
(bind-key "Auto Fill Space" #k"space" :mode "Fill")
(bind-key "Auto Fill Linefeed" #k"linefeed" :mode "Fill")



;;;; Keyboard macro bindings.

(bind-key "Define Keyboard Macro" #k"control-x (")
(bind-key "Define Keyboard Macro Key" #k"control-x meta-(")
(bind-key "End Keyboard Macro" #k"control-x )")
(bind-key "End Keyboard Macro" #k"control-x hyper-)")
(bind-key "Last Keyboard Macro" #k"control-x e")
(bind-key "Keyboard Macro Query" #k"control-x q")


;;;; Spell bindings.

(bind-key "Check Word Spelling" #k"meta-$")
(bind-key "Add Word to Spelling Dictionary" #k"control-x $")

(dolist (info (command-bindings (getstring "Self Insert" *command-names*)))
  (let* ((key (car info))
         (key-event (svref key 0))
         (character (key-event-char key-event)))
    (unless (or (alpha-char-p character) (eq key-event #k"'"))
      (bind-key "Auto Check Word Spelling" key :mode "Spell"))))
(bind-key "Auto Check Word Spelling" #k"return" :mode "Spell")
(bind-key "Auto Check Word Spelling" #k"tab" :mode "Spell")
(bind-key "Auto Check Word Spelling" #k"linefeed" :mode "Spell")
(bind-key "Correct Last Misspelled Word" #k"meta-:" :mode "Spell")
(bind-key "Undo Last Spelling Correction" #k"control-x a" :mode "Spell")


;;;; Overwrite Mode.

(bind-key "Overwrite Mode" #k"Insert")
(bind-key "Overwrite Delete Previous Character" #k"backspace"
          :mode "Overwrite")


;;; Do up the printing characters ...
(do ((i 33 (1+ i)))
    ((= i 126))
  (let ((key-event (heml-ext:char-key-event (code-char i))))
    (bind-key "Self Overwrite" key-event :mode "Overwrite")))

(bind-key "Self Overwrite" #k"space" :mode "Overwrite")



;;;; Comment bindings.

(bind-key "Indent for Comment" #k"meta-;")
(bind-key "Set Comment Column" #k"control-x ;")
(bind-key "Kill Comment" #k"control-meta-;")
(bind-key "Down Comment Line" #k"meta-n")
(bind-key "Up Comment Line" #k"meta-p")
(bind-key "Indent New Comment Line" #k"meta-j")
(bind-key "Indent New Comment Line" #k"meta-linefeed")


;;;; Word Abbrev Mode.

(bind-key "Add Mode Word Abbrev" #k"control-x control-a")
;; C-x + is Balance Windows, as in Emacs.
(bind-key "Inverse Add Mode Word Abbrev" #k"control-x control-h")
(bind-key "Inverse Add Global Word Abbrev" #k"control-x \-")
;; Removed in lieu of "Pop and Goto Mark".
;;(bind-key "Abbrev Expand Only" #k"meta-space")
(bind-key "Word Abbrev Prefix Mark" #k"meta-\"")
;; (bind-key "Unexpand Last Word" #k"control-x u")

(dolist (key (list #k"!" #k"~" #k"@" #k"#" #k";" #k"$" #k"%" #k"^" #k"&" #k"*"
                   #k"\-" #k"_" #k"=" #k"+" #k"[" #k"]" #k"(" #k")" #k"/" #k"|"
                   #k":" #k"'" #k"\"" #k"{" #k"}" #k"," #k"\<" #k"." #k"\>"
                   #k"`" #k"\\" #k"?" #k"return" #k"newline" #k"tab" #k"space"))
  (bind-key "Abbrev Expand Only" key :mode "Abbrev"))




;;;; Process (Shell).

(bind-key "Shell" #k"control-meta-s")
(bind-key "Confirm Process Input" #k"return" :mode "Process")
(bind-key "Shell Complete Filename" #k"M-escape" :mode "Process")
(bind-key "Interrupt Buffer Subprocess" #k"hyper-c" :mode "Process")
(bind-key "Stop Buffer Subprocess" #k"hyper-z" :mode "Process")
(bind-key "Quit Buffer Subprocess" #k"hyper-\\")
(bind-key "Send EOF to Process" #k"hyper-d")

(bind-key "Previous Interactive Input" #k"meta-p" :mode "Process")
(bind-key "Search Previous Interactive Input" #k"meta-P" :mode "Process")
(bind-key "Interactive Beginning of Line" #k"control-a" :mode "Process")
(bind-key "Interactive Beginning of Line" #k"home" :mode "Process")
(bind-key "Kill Interactive Input" #k"meta-i" :mode "Process")
(bind-key "Next Interactive Input" #k"meta-n" :mode "Process")
(bind-key "Reenter Interactive Input" #k"control-return" :mode "Process")

;;;; Bufed.

(bind-key "Bufed" #k"control-x control-meta-b")
(bind-key "Bufed Delete" #k"d" :mode "Bufed")
(bind-key "Bufed Delete" #k"control-d" :mode "Bufed")
(bind-key "Bufed Delete" #k"Delete" :mode "Bufed")
(bind-key "Bufed Unmark" #k"u" :mode "Bufed")
(bind-key "Bufed Unmark All" #k"U" :mode "Bufed")
(bind-key "Bufed Mark" #k"m" :mode "Bufed")
(bind-key "Bufed Toggle Marks" #k"t" :mode "Bufed")
(bind-key "Bufed Mark Matching" #k"%" :mode "Bufed")
(bind-key "Bufed Expunge" #k"x" :mode "Bufed")
(bind-key "Bufed Expunge" #k"!" :mode "Bufed")
(bind-key "Bufed Quit" #k"q" :mode "Bufed")
(bind-key "Bufed Goto" #k"space" :mode "Bufed")
(bind-key "Bufed Goto" #k"e" :mode "Bufed")
(bind-key "Bufed Goto" #k"return" :mode "Bufed")
(bind-key "Bufed Goto" #k"control-m" :mode "Bufed")
(bind-key "Bufed Goto Other Window" #k"o" :mode "Bufed")
(bind-key "Bufed Display" #k"v" :mode "Bufed")
(bind-key "Bufed Mouse Goto" #k"super-leftdown" :mode "Bufed")
(bind-key "Bufed Save File" #k"S" :mode "Bufed")
(bind-key "Bufed Kill" #k"D" :mode "Bufed")
(bind-key "Bufed Not Modified" #k"~" :mode "Bufed")
(bind-key "Bufed Toggle Read Only" #k"T" :mode "Bufed")
(bind-key "Bufed Revert" #k"V" :mode "Bufed")
(bind-key "Bufed Sort" #k"s" :mode "Bufed")
(bind-key "Bufed Filter" #k"/" :mode "Bufed")
(bind-key "Bufed Toggle Groups" #k"G" :mode "Bufed")
(bind-key "Bufed Update" #k"g" :mode "Bufed")
(bind-key "Next Line" #k"n" :mode "Bufed")
(bind-key "Previous Line" #k"p" :mode "Bufed")


(bind-key "Bufed Help" #k"?" :mode "Bufed")



;;;; Coned.

(bind-key "Coned" #k"control-x control-meta-b")
(bind-key "Coned Delete" #k"d" :mode "Coned")
(bind-key "Coned Delete" #k"control-d" :mode "Coned")
(bind-key "Coned Delete" #k"Delete" :mode "Coned")
(bind-key "Coned Undelete" #k"u" :mode "Coned")
(bind-key "Coned Expunge" #k"!" :mode "Coned")
(bind-key "Coned Expunge" #k"x" :mode "Coned")
(bind-key "Coned Quit" #k"q" :mode "Coned")
(bind-key "Coned Goto" #k"space" :mode "Coned")
(bind-key "Coned Refresh" #k"g" :mode "Coned")
(bind-key "Next Line" #k"n" :mode "Coned")
(bind-key "Previous Line" #k"p" :mode "Coned")
(bind-key "Coned Help" #k"?" :mode "Coned")



;;;; Xref.

(bind-key "Find Definitions" #k"meta-." :mode "Lisp")
(bind-key "Who Specializes"  #k"control-c control-w control-a" :mode "Lisp")
(bind-key "Who Binds"        #k"control-c control-w control-b" :mode "Lisp")
(bind-key "Who Calls"        #k"control-c control-w control-c" :mode "Lisp")
(bind-key "Who Macroexpands" #k"control-c control-w control-m" :mode "Lisp")
(bind-key "Who References"   #k"control-c control-w control-r" :mode "Lisp")
(bind-key "Who Sets"         #k"control-c control-w control-s" :mode "Lisp")

(bind-key "Xref Quit" #k"q" :mode "Xref")
(bind-key "Xref Goto" #k"space" :mode "Xref")
(bind-key "Xref Help" #k"?" :mode "Xref")



;;;; (Slave) Apropos.

(bind-key "Slave Apropos" #k"control-c ?")

(bind-key "Find Definitions" #k"meta-." :mode "Apropos")

(bind-key "Apropos Quit" #k"q" :mode "Apropos")
(bind-key "Apropos Find Definition" #k"." :mode "Apropos")
(bind-key "Apropos Describe" #k"space" :mode "Apropos")
(bind-key "Apropos Help" #k"?" :mode "Apropos")



;;;; Fuzzylist

(bind-key "Fuzzylist Pick" #k"space" :mode "Fuzzylist")
(bind-key "Fuzzylist Quit" #k"q" :mode "Fuzzylist")
(bind-key "Fuzzylist Find Definition" #k"." :mode "Fuzzylist")
(bind-key "Fuzzylist Help" #k"?" :mode "Fuzzylist")

(bind-key "Completelist Pick" #k"space" :mode "Completelist")
(bind-key "Completelist Quit" #k"q" :mode "Completelist")
(bind-key "Completelist Find Definition" #k"." :mode "Completelist")
(bind-key "Completelist Help" #k"?" :mode "Completelist")



;;;; Dired.

(bind-key "Menu Bar" #k"meta-`")
(bind-key "Dired" #k"control-x d")
(bind-key "Dired" #k"control-x control-meta-d")

(bind-key "Dired Delete File and Down Line" #k"d" :mode "Dired")
(bind-key "Dired Delete" #k"D" :mode "Dired")
(bind-key "Dired Delete File" #k"control-d" :mode "Dired")
(bind-key "Dired Delete File" #k"Delete" :mode "Dired")
(bind-key "Dired Delete File" #k"k" :mode "Dired")

(bind-key "Dired Unmark" #k"u" :mode "Dired")
(bind-key "Dired Unmark All" #k"U" :mode "Dired")
(bind-key "Dired Undelete File" #k"control-u" :mode "Dired")

(bind-key "Dired Expunge Files" #k"x" :mode "Dired")
(bind-key "Dired Shell Command" #k"!" :mode "Dired")
(bind-key "Dired Mark" #k"m" :mode "Dired")
(bind-key "Dired Toggle Marks" #k"t" :mode "Dired")
(bind-key "Dired Mark with Pattern" #k"%" :mode "Dired")
(bind-key "Dired Create Directory" #k"+" :mode "Dired")
(bind-key "Dired Symlink" #k"S" :mode "Dired")
(bind-key "Dired Change Mode" #k"M" :mode "Dired")
(bind-key "Dired Compress" #k"Z" :mode "Dired")
(bind-key "Dired Update Buffer" #k"hyper-u" :mode "Dired")
(bind-key "Dired Update Buffer" #k"g" :mode "Dired")
(bind-key "Dired Toggle Hidden Files" #k"h" :mode "Dired")
(bind-key "Dired Sort" #k"s" :mode "Dired")
(bind-key "Dired Edit File Other Window" #k"o" :mode "Dired")
(bind-key "Dired View File" #k"v" :mode "Dired")
(bind-key "Dired Open Externally" #k"W" :mode "Dired")
(bind-key "Dired Insert Subdirectory" #k"i" :mode "Dired")
(bind-key "Dired Edit Names" #k"control-x control-q" :mode "Dired")
(bind-key "Wdired Finish" #k"control-c control-c" :mode "Wdired")
(bind-key "Wdired Abort" #k"control-c control-k" :mode "Wdired")
;; (bind-key "Dired View File" #k"space" :mode "Dired")
(bind-key "Dired Edit File" #k"space" :mode "Dired")
(bind-key "Dired Edit File" #k"e" :mode "Dired")
(bind-key "Dired Edit File" #k"return" :mode "Dired")
(bind-key "Dired Edit File" #k"control-m" :mode "Dired")
(bind-key "Dired Up Directory" #k"^" :mode "Dired")
(bind-key "Dired Quit" #k"q" :mode "Dired")
(bind-key "Dired Help" #k"?" :mode "Dired")

(bind-key "Dired Copy" #k"c" :mode "Dired")
(bind-key "Dired Copy" #k"C" :mode "Dired")
(bind-key "Dired Rename" #k"r" :mode "Dired")
(bind-key "Dired Rename" #k"R" :mode "Dired")

(bind-key "Next Line" #k"n" :mode "Dired")
(bind-key "Previous Line" #k"p" :mode "Dired")


;;;; View Mode.
(bind-key "View Scroll Down" #k"space" :mode "View")
(bind-key "Scroll Window Up" #k"b" :mode "View")
(bind-key "Scroll Window Up" #k"backspace" :mode "View")
(bind-key "View Return" #k"^" :mode "View")
(bind-key "View Quit" #k"q" :mode "View")
(bind-key "View Edit File" #k"e" :mode "View")
(bind-key "View Help" #k"?" :mode "View")
(bind-key "Beginning of Buffer" #k"\<" :mode "View")
(bind-key "End of Buffer" #k"\>" :mode "View")

;;;; Completion mode.

(dolist (c (command-bindings (getstring "Self Insert" *command-names*)))
  (bind-key "Completion Self Insert" (car c) :mode "Completion"))

(bind-key "Completion Self Insert" #k"space" :mode "Completion")
;; (bind-key "Completion Self Insert" #k"tab" :mode "Completion")
(bind-key "Completion Self Insert" #k"return" :mode "Completion")
(bind-key "Completion Self Insert" #k"linefeed" :mode "Completion")

(bind-key "Completion Complete Word" #k"tab" :mode "Completion")
(bind-key "Completion Rotate Completions" #k"meta-end" :mode "Completion")



;;;; Caps-Lock mode.

(heml-ext:do-alpha-key-events (key-event :lower)
                                 (bind-key "Self Insert Caps Lock" key-event :mode "CAPS-LOCK"))


;;;; pheml changes

(bind-key "Scroll Window Down" #k"pagedown")
(bind-key "Scroll Window Up"   #k"pageup")
(bind-key "Dabbrev Expand"     #k"meta-/")
(bind-key "Just One Space"     #k"meta-space")
(bind-key "Mark Form"          #k"control-meta-space")
(bind-key "New Undo"           #k"control-_")
(bind-key "New Undo"           #k"control-Delete")
(bind-key "New Undo"           #k"control-x u")

(bind-key "Quickselect"           #k"control-c space")

;;;; Logical characters.

(setf (logical-key-event-p #k"control-s" :forward-search) t)
(setf (logical-key-event-p #k"control-r" :backward-search) t)
(setf (logical-key-event-p #k"control-r" :recursive-edit) t)
(setf (logical-key-event-p #k"delete" :cancel) t)
(setf (logical-key-event-p #k"backspace" :cancel) t)
(setf (logical-key-event-p #k"control-g" :abort) t)
(setf (logical-key-event-p #k"escape" :exit) t)
(setf (logical-key-event-p #k"y" :yes) t)
(setf (logical-key-event-p #k"space" :yes) t)
(setf (logical-key-event-p #k"n" :no) t)
(setf (logical-key-event-p #k"backspace" :no) t)
(setf (logical-key-event-p #k"delete" :no) t)
(setf (logical-key-event-p #k"!" :do-all) t)
(setf (logical-key-event-p #k"." :do-once) t)
(setf (logical-key-event-p #k"control-h" :help) t)
(setf (logical-key-event-p #k"h" :help) t)
(setf (logical-key-event-p #k"?" :help) t)
(setf (logical-key-event-p #k"control-_" :help) t)
(setf (logical-key-event-p #k"return" :confirm) t)
(setf (logical-key-event-p #k"control-q" :quote) t)
(setf (logical-key-event-p #k"k" :keep) t)
