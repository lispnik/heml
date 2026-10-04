;;;; scripts/demo/debug.lisp -- `make demo-debug': a C program debugged
;;;; with lldb-dap over the Debug Adapter Protocol -- breakpoints and the
;;;; place it stopped in the fringe, frames and variables, stepping, an
;;;; expression evaluated, and what it printed.

(load (merge-pathnames "driver.lisp" *load-truename*))
(in-package :heml-demo)

(defvar *project* (fresh-directory "debug"))

(write-file (merge-pathnames ".heml-project" *project*) "()")
(write-file (merge-pathnames "primes.c" *project*)
            "#include <stdio.h>" ""
            "static int is_prime(int n) {"
            "    if (n < 2)" "        return 0;"
            "    for (int d = 2; d * d <= n; d++)"
            "        if (n % d == 0)" "            return 0;"
            "    return 1;" "}" ""
            "int main(void) {"
            "    int count = 0;"
            "    for (int n = 1; n <= 20; n++) {"
            "        if (is_prime(n))" "            count++;" "    }"
            "    printf(\"%d primes up to 20\\n\", count);"
            "    return 0;" "}")
(sh *project* "cc -g -O0 -o primes primes.c")

(setf (heml::variable-value 'heml::language-servers :global) nil)

(defun goto-line (line)
  (keys '(#\< "Meta"))
  (dotimes (i (1- line)) (post-key #\n "Control") (sleep 0.03))
  (settle))

(defun steps ()
  (caption "Debugging, over the Debug Adapter Protocol: lldb-dap for C and Rust, debugpy, Delve")
  (open-file (merge-pathnames "primes.c" *project*))
  (pause 1.5)
  (caption "F9 puts a breakpoint on a line: a dot in the fringe")
  (goto-line 15)
  (keys "F9")
  (wait-for "●" 5)
  (pause 2)

  (caption "F5 runs the program to it: the arrow in the fringe is where it stopped")
  (keys "F5")
  (wait-for "Program to debug" 15)
  (pause 1)
  (keys "Return")
  (wait-for "▶" 60)
  (pause 2)
  (caption "C-c d w shows the frames and the selected frame's variables")
  (keys '(#\c "Control") #\d #\w)
  (wait-for "count = 0" 10)
  (pause 3)
  ;; Back to the source, so that the Debugger stays in view.
  (keys '(#\x "Control") #\o)

  (caption "F11 steps into is_prime; F10 steps over a line")
  (keys "F11")
  (pause 2)
  (keys "F10")
  (pause 1.5)
  (keys "F10")
  (pause 2)

  (caption "C-c d e evaluates an expression in the selected frame")
  (keys '(#\c "Control") #\d #\e)
  (pause 0.6)
  (type-text "n * 10")
  (keys "Return")
  (wait-for "n * 10 =" 10)
  (pause 2.5)

  (caption "F5 goes on to the breakpoint again, with the variables as they are now")
  (keys "F5")
  (wait-for "count = 0" 10)
  (pause 1)
  (keys "F5")
  (pause 2.5)

  (caption "F9 again takes the breakpoint away, and F5 lets the program finish")
  (goto-line 15)
  (keys "F9")
  (pause 1)
  (keys "F5")
  (pause 2)
  (caption "What it printed is in the buffer Debug Output")
  (keys '(#\x "Control") #\o '(#\x "Control") #\b)
  (pause 0.4)
  ;; A space at the prompt completes: C-q puts one in.
  (keys '(#\a "Control") '(#\k "Control"))
  (type-text "Debug")
  (keys '(#\q "Control") #\Space)
  (type-text "Output")
  (keys "Return")
  (wait-for "primes up to 20" 10)
  (pause 4))

(run-demo "debug" #'steps)
