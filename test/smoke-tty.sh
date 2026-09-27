#!/bin/sh
# test/smoke-tty.sh -- `make smoke-tty`: the TTY backend in a real terminal.
#
# The editor runs in a detached tmux session, the way it runs in Terminal:
# its terminfo is tmux-256color, its erase character ^?, its input bytes.
# Keys go in with `tmux send-keys`, and each check reads the screen back
# with `tmux capture-pane`.  Exits 0 when every check passes, 1 otherwise.

set -u

# LISP=ecl runs the same checks under ECL.
LISP=${LISP:-sbcl}
case $(basename "$LISP") in
    ecl*) quiet= ;;
    *) quiet=--noinform ;;
esac
session=xoamax-smoke-tty-$$
failures=0
checks=0

cd "$(dirname "$0")/.." || exit 1

screen() { tmux capture-pane -p -t "$session"; }
send() { tmux send-keys -t "$session" "$@"; }
type_text() { tmux send-keys -t "$session" -l "$1"; }

# expect PATTERN DESCRIPTION [SECONDS]: wait for PATTERN (a fixed string) on
# the screen.
expect() {
    checks=$((checks + 1))
    tries=$(( ${3:-10} * 5 ))
    while [ "$tries" -gt 0 ]; do
        if screen | grep -qF -- "$1"; then
            echo "  ok    $2"
            return 0
        fi
        sleep 0.2
        tries=$((tries - 1))
    done
    echo "  FAIL  $2"
    echo "        (no \"$1\" on the screen:)"
    screen | sed 's/^/        | /'
    failures=$((failures + 1))
    return 1
}

cleanup() { tmux kill-session -t "$session" 2>/dev/null; }
trap cleanup EXIT

tmux new-session -d -s "$session" -x 100 -y 30 \
     "$LISP $quiet \
        --eval '(asdf:load-system :hemlock.tty)' \
        --eval '(uiop:symbol-call :hemlock :hemlock nil :backend-type :tty :load-user-init nil)' \
        --eval '(progn (format t \"~%EDITOR-RETURNED~%\") (finish-output) (sleep 30))'"

expect "Hemlock CL-USER:" "the editor starts, with a modeline" 180

type_text 'café λ 日本語 end'
expect 'café λ 日本語 end' "typing, wide characters included"

send BSpace BSpace BSpace
type_text 'fin'
expect 'café λ 日本語 fin' "Backspace (the terminal's erase character) deletes"

send C-a C-k C-y C-y
expect 'café λ 日本語 fincafé λ 日本語 fin' "kill and yank"

send C-x 2
checks=$((checks + 1))
sleep 1
if [ "$(screen | grep -c 'Hemlock CL-USER:')" -ge 2 ]; then
    echo "  ok    C-x 2 splits the window"
else
    echo "  FAIL  C-x 2 splits the window"
    failures=$((failures + 1))
fi
send C-x 1

# Side by side: two modelines on one line, with the bar between them.
send C-x 3
sleep 1
checks=$((checks + 1))
if screen | grep -q 'Hemlock CL-USER:.*|Hemlock CL-USER:'; then
    echo "  ok    C-x 3 splits the window side by side"
else
    echo "  FAIL  C-x 3 splits the window side by side"
    screen | sed 's/^/        | /'
    failures=$((failures + 1))
fi
send C-x 0
sleep 1
checks=$((checks + 1))
if [ "$(screen | grep -c 'Hemlock CL-USER:')" -eq 1 ] && ! screen | grep -q '|Hemlock'; then
    echo "  ok    C-x 0 leaves one window across the screen"
else
    echo "  FAIL  C-x 0 leaves one window across the screen"
    screen | sed 's/^/        | /'
    failures=$((failures + 1))
fi

send M-x
expect 'Extended Command:' "Meta (ESC) prefixes: M-x prompts"
type_text 'Shell'
send Enter
sleep 2
type_text 'echo tty-$((6*7))'
send Enter
expect 'tty-42' "a shell runs in a buffer" 15

# C-v is the terminal's literal-next character unless the editor turns it
# off, and then it is swallowed rather than scrolling.
type_text 'seq -f row-%03g 1 200'
send Enter
expect 'row-200' "a shell's output is shown" 15
send 'M-<'
sleep 1
send C-v
expect 'row-030' "C-v scrolls a page"

send C-x C-c
sleep 1
send n
expect 'EDITOR-RETURNED' "C-x C-c leaves the editor" 15

# The :mini backend: HEMLOCK:REPL, a REPL whose lines are edited by Hemlock
# where they stand rather than on a screen of their own.
tmux kill-session -t "$session" 2>/dev/null
session=xoamax-smoke-repl-$$
tmux new-session -d -s "$session" -x 100 -y 30 \
     "$LISP $quiet \
        --eval '(asdf:load-system :hemlock.tty)' \
        --eval '(uiop:symbol-call :hemlock :repl)'; \
      echo REPL-EXITED; sleep 30"

expect 'CL-USER>' "hemlock:repl prompts" 180

type_text '(format nil "repl-~A" (* 6 7))'
send Enter
expect 'repl-42' "hemlock:repl evaluates what is typed"

send C-p
sleep 1
checks=$((checks + 1))
if [ "$(screen | grep -c 'format nil')" -ge 2 ]; then
    echo "  ok    C-p recalls the last line"
else
    echo "  FAIL  C-p recalls the last line"
    failures=$((failures + 1))
fi

send C-a C-k
type_text '(defun'
send Enter
expect 'Not a complete form' "an unfinished form is not read"
checks=$((checks + 1))
if screen | grep -q '^CL-USER> (defun'; then
    echo "  ok    the prompt comes back at the left margin"
else
    echo "  FAIL  the prompt comes back at the left margin"
    screen | sed 's/^/        | /'
    failures=$((failures + 1))
fi

send C-a C-k
send C-d
expect 'REPL-EXITED' "C-d on an empty line leaves the REPL" 15

echo "$checks checks, $failures failed"
[ "$failures" -eq 0 ]
