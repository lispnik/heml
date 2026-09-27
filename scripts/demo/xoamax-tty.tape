# Xoamax's terminal editor, recorded by `make demo' into build/demo/.
Output build/demo/xoamax-tty.mp4
Output build/demo/xoamax-tty.gif

Set Shell "bash"
Set FontSize 16
Set Width 1400
Set Height 820
Set Padding 16
Set TypingSpeed 40ms

Hide
Type "clear" Enter
Type "sbcl --noinform --load scripts/demo/start.lisp" Enter
Wait+Screen@120s /Hemlock CL-USER/
Show
Sleep 1s

# Lisp, coloured and indented as it is typed.
Type ";;; Fibonacci, in Xoamax" Enter
Type "(defun fib (n)" Ctrl+J
Type `"The Nth Fibonacci number."` Ctrl+J
Type "(if (< n 2)" Ctrl+J
Type "n" Ctrl+J
Type "(+ (fib (- n 1)) (fib (- n 2)))))" Enter Enter
Type ";; Wide characters take two columns: λ 日本語 ｆｕｌｌｗｉｄｔｈ" Enter
Sleep 1s

# Evaluated in the editor's own Lisp.
Escape Type "x"
Sleep 0.3s
Type "Editor Evaluate Buffer" Enter
Sleep 1s
Escape Type "x"
Sleep 0.3s
Type "Editor Evaluate Expression" Enter
Sleep 0.4s
Type "(fib 20)" Enter
Sleep 2s

# A quote at the top turns what follows into a string, and back.
Escape Type "<"
Sleep 0.5s
Type `"`
Sleep 2s
Backspace
Sleep 1.2s

# Side by side, with a shell on the right.
Ctrl+X Type "3"
Sleep 1s
Escape Type "x"
Sleep 0.3s
Type "Shell" Enter
Sleep 1.5s
Type "ls src | head -10" Enter
Sleep 1.5s
Type "seq 1 2000" Enter
Sleep 2.5s

# The left window split again, and a real file paged through below.
Ctrl+X Type "o"
Sleep 0.5s
Ctrl+X Type "2"
Sleep 0.8s
Ctrl+X Ctrl+F
Sleep 0.5s
Type "display.lisp" Enter
Sleep 1.2s
Ctrl+V
Sleep 1s
Ctrl+V
Sleep 1s
Escape Type "v"
Sleep 1s

# The left column twelve wider, then that window gone.
Ctrl+U Type "12"
Ctrl+X Type "}"
Sleep 1.5s
Ctrl+X Type "0"
Sleep 1.5s

# Leave.
Ctrl+X Ctrl+C
Sleep 1s
Type "y"
Sleep 1s

# hemlock:repl, whose lines Hemlock edits where they stand.
Hide
Type "clear" Enter
Show
Type "sbcl --noinform --load scripts/demo/repl.lisp" Enter
Hide
Wait+Screen@120s /CL-USER>/
Show
Sleep 1s
Type "(loop for i below 10 collect (* i i))" Enter
Sleep 1.2s
Ctrl+P
Sleep 1s
Enter
Sleep 1s
Type "(string-up"
Sleep 0.5s
Tab
Sleep 0.8s
Type ` "xoamax")` Enter
Sleep 1.2s
Type "(defun" Enter
Sleep 1.5s
Ctrl+A Ctrl+K
Ctrl+D
Sleep 1s

# The same editor on ECL.
Hide
Type "clear" Enter
Show
Type "ecl --load scripts/demo/start.lisp" Enter
Hide
Wait+Screen@240s /on ECL/
Show
Sleep 1s
Type ";; The same editor, on ECL." Enter
Sleep 1s
Ctrl+X Type "3"
Sleep 2s
Ctrl+X Ctrl+C
Sleep 1s
Type "n"
Sleep 1s
Hide
