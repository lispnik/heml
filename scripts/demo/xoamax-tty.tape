# Xoamax's terminal editor, recorded by `make demo' into build/demo/.
Output build/demo/xoamax-tty.mp4
Output build/demo/xoamax-tty.gif

Set Shell "bash"
Set FontSize 18
Set Width 1280
Set Height 760
Set Padding 16
Set TypingSpeed 45ms

Hide
Type "clear" Enter
Type "sbcl --noinform --load scripts/demo/start.lisp" Enter
Wait+Screen@120s /Hemlock CL-USER/
Show
Sleep 1.5s

# Lisp, coloured and indented as it is typed.
Type ";;; Fibonacci, in Xoamax" Enter
Type "(defun fib (n)" Ctrl+J
Type `"The Nth Fibonacci number."` Ctrl+J
Type "(if (< n 2)" Ctrl+J
Type "n" Ctrl+J
Type "(+ (fib (- n 1)) (fib (- n 2)))))" Enter Enter
Type ";; Wide characters take two columns: λ 日本語 ｆｕｌｌｗｉｄｔｈ" Enter
Sleep 1.5s

# A quote at the top turns what follows into a string, and back.
Escape Type "<"
Sleep 0.5s
Type `"`
Sleep 2.5s
Backspace
Sleep 1.5s

# Two windows, a shell in the other.
Ctrl+X Type "2"
Sleep 0.8s
Ctrl+X Type "o"
Escape Type "x"
Sleep 0.4s
Type "Shell" Enter
Sleep 2s
Type "ls src | head -12" Enter
Sleep 2s
Type "seq 1 3000" Enter
Sleep 3s

# One window, and a real file paged through: every page is drawn whole.
Ctrl+X Type "1"
Ctrl+X Ctrl+F
Sleep 0.5s
Type "display.lisp" Enter
Sleep 1.5s
Ctrl+V
Sleep 1s
Ctrl+V
Sleep 1s
Ctrl+V
Sleep 1s
Escape Type "v"
Sleep 1s
Escape Type "v"
Sleep 1.5s

# Leave.
Ctrl+X Ctrl+C
Sleep 1s
Type "y"
Sleep 1s
Hide
