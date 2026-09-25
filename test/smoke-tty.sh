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

send M-x
expect 'Extended Command:' "Meta (ESC) prefixes: M-x prompts"
type_text 'Shell'
send Enter
sleep 2
type_text 'echo tty-$((6*7))'
send Enter
expect 'tty-42' "a shell runs in a buffer" 15

send C-x C-c
sleep 1
send n
expect 'EDITOR-RETURNED' "C-x C-c leaves the editor" 15

echo "$checks checks, $failures failed"
[ "$failures" -eq 0 ]
