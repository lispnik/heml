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
session=heml-smoke-tty-$$
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

# expect_re REGEX DESCRIPTION [SECONDS]: the same, for a basic regular
# expression.
expect_re() {
    checks=$((checks + 1))
    tries=$(( ${3:-10} * 5 ))
    while [ "$tries" -gt 0 ]; do
        if screen | grep -q -- "$1"; then
            echo "  ok    $2"
            return 0
        fi
        sleep 0.2
        tries=$((tries - 1))
    done
    echo "  FAIL  $2"
    echo "        (nothing matching \"$1\" on the screen:)"
    screen | sed 's/^/        | /'
    failures=$((failures + 1))
    return 1
}

cleanup() { tmux kill-session -t "$session" 2>/dev/null; }
trap cleanup EXIT

tmux new-session -d -s "$session" -x 100 -y 30 \
     "$LISP $quiet \
        --eval '(asdf:load-system :heml.tty)' \
        --eval '(uiop:symbol-call :heml :heml nil :backend-type :tty :load-user-init nil)' \
        --eval '(progn (format t \"~%EDITOR-RETURNED~%\") (finish-output) (sleep 30))'"

expect "Heml CL-USER:" "the editor starts, with a modeline" 180

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
if [ "$(screen | grep -c 'Heml CL-USER:')" -ge 2 ]; then
    echo "  ok    C-x 2 splits the window"
else
    echo "  FAIL  C-x 2 splits the window"
    failures=$((failures + 1))
fi
# With windows above and beside it, C-x 1 leaves the current one alone.
send C-x 3
sleep 1
send C-x 1
sleep 1
checks=$((checks + 1))
if [ "$(screen | grep -c 'Heml CL-USER:')" -eq 1 ] && ! screen | grep -q '|Heml'; then
    echo "  ok    C-x 1 deletes the other windows"
else
    echo "  FAIL  C-x 1 deletes the other windows"
    screen | sed 's/^/        | /'
    failures=$((failures + 1))
fi

# Side by side: two modelines on one line, with the bar between them.
send C-x 3
sleep 1
checks=$((checks + 1))
if screen | grep -q 'Heml CL-USER:.*|Heml CL-USER:'; then
    echo "  ok    C-x 3 splits the window side by side"
else
    echo "  FAIL  C-x 3 splits the window side by side"
    screen | sed 's/^/        | /'
    failures=$((failures + 1))
fi
# C-x } widens the current window, the right one, moving the bar; C-x + puts
# it back in the middle of the 100 columns, with 49 or 50 on each side.
bar_column() { screen | grep 'Heml CL-USER:.*|Heml' | head -1 | awk -F'|' '{ print length($1) }'; }
balanced=$(bar_column)
send C-u 1 0 C-x }
sleep 1
widened=$(bar_column)
send C-x +
sleep 1
checks=$((checks + 1))
if [ "$widened" -lt "$balanced" ] && [ $((2 * $(bar_column) - 99)) -ge -1 ] && [ $((2 * $(bar_column) - 99)) -le 1 ]; then
    echo "  ok    C-x } widens a window, and C-x + balances them"
else
    echo "  FAIL  C-x } widens a window, and C-x + balances them ($balanced, $widened, $(bar_column))"
    screen | sed 's/^/        | /'
    failures=$((failures + 1))
fi
send C-x 0
sleep 1
checks=$((checks + 1))
if [ "$(screen | grep -c 'Heml CL-USER:')" -eq 1 ] && ! screen | grep -q '|Heml'; then
    echo "  ok    C-x 0 leaves one window across the screen"
else
    echo "  FAIL  C-x 0 leaves one window across the screen"
    screen | sed 's/^/        | /'
    failures=$((failures + 1))
fi

# Visiting a file: its name is in the modeline.  (On ECL a namestring is
# not a simple string, and the modeline once refused it.)  A file that does
# not exist yet is a new one.
# The prompt starts with the current directory: clear it for a full path.
send C-x C-f
sleep 0.5
send C-a C-k
type_text "$PWD/README.org"
send Enter
expect 'README.org' "C-x C-f visits a file"
send C-x C-f
sleep 0.5
send C-a C-k
type_text "$PWD/build/smoke-tty-new-file.txt"
send Enter
expect '(New File)' "C-x C-f on a new name starts a new file"

send C-x d
expect 'Edit Directory:' "C-x d asks for a directory to edit"
send C-a C-k
type_text "$PWD/src/"
send Enter
expect 'abbrev.lisp' "and Dired lists it"
expect_re '/src/  ([0-9]* entries, ' "a header names the directory, its entries and their size"
expect_re ' [0-9.]*K [A-Z][a-z][a-z] ' "sizes are human-readable"
checks=$((checks + 1))
if [ "$(screen | grep -E '^  [dlpscb-][rwxs-]{9} ' | sed -E 's/^(.*)[A-Z][a-z]{2} [ 0-9][0-9] .*/\1/' | awk '{ print length }' | sort -u | wc -l)" -eq 1 ]; then
    echo "  ok    every line's date is in the same column"
else
    echo "  FAIL  every line's date is in the same column"; screen | sed 's/^/        | /'
    failures=$((failures + 1))
fi
checks=$((checks + 1))
if screen | grep -q -E '^  [dlpscb-][rwxs-]{9}[0-9]'; then
    echo "  FAIL  the link count has a column of its own"
    screen | sed 's/^/        | /'
    failures=$((failures + 1))
else
    echo "  ok    the link count has a column of its own"
fi
send h
expect 'Showing hidden files.' "h shows the hidden files"
send g
sleep 0.5

# Marks and operations, in a directory of the test's own.
send C-x k
sleep 0.5
send Enter
sleep 0.5
D=$PWD/build/smoke-tty-dired
rm -rf "$D"
mkdir -p "$D/sub"
printf 'one\n' > "$D/a.txt"
printf 'two\n2\n' > "$D/b.txt"
printf 'three\n' > "$D/c.log"
send C-x d
sleep 0.5
send C-a C-k
type_text "$D/"
send Enter
expect 'c.log' "Dired lists the test's directory"
send m m
expect_re '^\* .*b\.txt' "m marks files"
send C
expect 'Copy 2 files to directory:' "C copies the marked files"
send C-a C-k
type_text "$D/sub/"
send Enter
sleep 1
checks=$((checks + 1))
if [ -f "$D/sub/a.txt" ] && [ -f "$D/sub/b.txt" ]; then
    echo "  ok    into the directory given"
else
    echo "  FAIL  into the directory given"; ls -la "$D/sub" | sed 's/^/        | /'
    failures=$((failures + 1))
fi
send U
sleep 0.5
checks=$((checks + 1))
if screen | grep -q '^\* '; then
    echo "  FAIL  U unmarks everything"; failures=$((failures + 1))
else
    echo "  ok    U unmarks everything"
fi
send +
expect 'Create directory:' "+ asks for a new directory"
type_text 'newdir'
send Enter
expect 'newdir/' "and makes it"
send 'M-<' C-n C-n C-n
send M
expect 'Mode (octal) for c.log' "M asks for c.log's mode"
type_text '600'
send Enter
expect_re '^  -rw------- .*c\.log' "and changes it"
send Z
expect 'c.log.gz' "Z compresses a file"
send 'M-<' C-n
send '!'
expect '! on a.txt:' "! asks for a command for the file under point"
type_text 'wc -l'
send Enter
expect_re '1 a\.txt' "and runs it"
send R
expect 'Move a.txt to:' "R asks where a file goes"
send C-a C-k
type_text "$D/renamed.txt"
send Enter
expect 'renamed.txt' "and renames it"
send s
expect 'by date)' "s sorts by date"
send s
expect 'by size)' "and by size"
send s
sleep 1
checks=$((checks + 1))
if screen | grep -q 'by size)\|by date)'; then
    echo "  FAIL  and by name again"; failures=$((failures + 1))
else
    echo "  ok    and by name again"
fi
touch "$D/appeared.txt"
expect 'appeared.txt' "a file made outside Heml appears in the listing" 10
send 'M-<' C-n
send o
sleep 1
checks=$((checks + 1))
if [ "$(screen | grep -c 'Heml CL-USER:')" -ge 2 ] && screen | grep -q 'CL-USER:.*appeared.txt'; then
    echo "  ok    o visits the file in the other window"
else
    echo "  FAIL  o visits the file in the other window"; screen | sed 's/^/        | /'
    failures=$((failures + 1))
fi
send C-x o
sleep 0.3
send C-x 1
sleep 0.5
send v
expect 'View' "v views the file"
send q
expect '(Dired)' "and q goes back to Dired"
send 'M-<' C-n
send D
if [ -x /usr/bin/trash ]; then
    expect 'Move 1 file to the Trash?' "D asks before moving a file to the Trash"
else
    expect 'Really delete files?' "D asks before deleting"
fi
send y
sleep 1
checks=$((checks + 1))
if [ ! -e "$D/appeared.txt" ] && [ -e "$D/b.txt" ] && [ -e "$D/renamed.txt" ] && [ -e "$D/c.log.gz" ]; then
    echo "  ok    and deletes only the file under point"
else
    echo "  FAIL  and deletes only the file under point"; ls -la "$D" | sed 's/^/        | /'
    failures=$((failures + 1))
fi
send 'M->'
send i
expect 'sub/a.txt' "i lists a subdirectory inline"
send i
sleep 1
checks=$((checks + 1))
if screen | grep -q 'sub/a.txt'; then
    echo "  FAIL  and i again folds it away"; failures=$((failures + 1))
else
    echo "  ok    and i again folds it away"
fi
send C-x C-q
expect '(Wdired)' "C-x C-q makes the names editable"
send 'M-<'
send M-x
sleep 0.3
type_text 'Replace String'
send Enter
sleep 0.3
type_text 'renamed.txt'
send Enter
sleep 0.3
type_text 'final.txt'
send Enter
sleep 0.5
send C-c C-c
expect '1 file renamed.' "C-c C-c renames what was edited"
checks=$((checks + 1))
if [ -e "$D/final.txt" ] && [ ! -e "$D/renamed.txt" ]; then
    echo "  ok    and the file has its new name"
else
    echo "  FAIL  and the file has its new name"; ls -la "$D" | sed 's/^/        | /'
    failures=$((failures + 1))
fi
expect '(Dired)' "and Dired is back"
send C-x k
sleep 0.5
send Enter
sleep 0.5
send C-x k
sleep 0.5
send Enter
sleep 0.5

# Bufed.
send C-x C-b
expect 'Buffers  (' "C-x C-b lists the buffers, under a header"
expect_re 'README\.org  *[0-9.]*K  *Fundamental  *~/' "each with its size, mode and file"
send s
expect 'by name' "s sorts them by name"
send /
expect 'Show buffers' "/ asks which buffers to show"
type_text 'Text'
send Enter
sleep 1
checks=$((checks + 1))
if screen | grep -q 'showing "Text"' && ! screen | grep -q ' README\.org '; then
    echo "  ok    and shows only those"
else
    echo "  FAIL  and shows only those"; screen | sed 's/^/        | /'; failures=$((failures + 1))
fi
send /
sleep 0.3
send Enter
sleep 0.5
send G
expect '[no file]' "G groups the buffers by directory"
send G
sleep 0.5
send 'M-<' C-n
send m
expect_re '^\* ' "m marks a buffer"
send U
sleep 0.5
checks=$((checks + 1))
if screen | grep -q '^\* '; then
    echo "  FAIL  U unmarks it"; failures=$((failures + 1))
else
    echo "  ok    U unmarks it"
fi
# A buffer made while the list is shown appears in it.
send C-x 2
sleep 0.5
send C-x o
sleep 0.3
send C-x C-f
sleep 0.5
send C-a C-k
type_text "$PWD/build/smoke-tty-bufed.txt"
send Enter
expect_re 'smoke-tty-bufed\.txt  *0' "a buffer made while Bufed is shown appears in it" 10
send C-x o
sleep 0.3
send C-x 1
sleep 0.5
send /
sleep 0.3
type_text 'smoke-tty-bufed'
send Enter
sleep 0.5
send 'M-<' C-n
send D
expect 'Kill 1 buffer?' "D asks before killing a buffer"
send y
sleep 1
checks=$((checks + 1))
if screen | grep -q 'smoke-tty-bufed.txt'; then
    echo "  FAIL  and kills it"; screen | sed 's/^/        | /'; failures=$((failures + 1))
else
    echo "  ok    and kills it"
fi
send /
sleep 0.3
send Enter
sleep 0.5
send /
sleep 0.3
type_text 'README'
send Enter
sleep 0.5
send 'M-<' C-n
send Enter
expect_re 'Heml CL-USER:.*README\.org' "Return visits the buffer under point"
# Leave Bufed properly: q closes it.
send C-x C-b
sleep 0.5
send /
sleep 0.3
send Enter
sleep 0.3
send q
expect_re 'Heml CL-USER:.*README\.org' "q closes Bufed"

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

# Killing the shell's buffer closes its pty.  The event loop must forget
# the descriptor first, or its next select(2) fails with EBADF.
send C-x k
sleep 0.5
send Enter
sleep 1
type_text 'after-the-shell'
expect 'after-the-shell' "the editor goes on after a shell's buffer is killed"
checks=$((checks + 1))
if screen | grep -q 'Bad file descriptor'; then
    echo "  FAIL  and nothing complains of a bad file descriptor"
    screen | sed 's/^/        | /'
    failures=$((failures + 1))
else
    echo "  ok    and nothing complains of a bad file descriptor"
fi

send C-x C-c
sleep 1
send n
expect 'EDITOR-RETURNED' "C-x C-c leaves the editor" 15

# The :mini backend: HEML:REPL, a REPL whose lines are edited by Heml
# where they stand rather than on a screen of their own.
tmux kill-session -t "$session" 2>/dev/null
session=heml-smoke-repl-$$
tmux new-session -d -s "$session" -x 100 -y 30 \
     "$LISP $quiet \
        --eval '(asdf:load-system :heml.tty)' \
        --eval '(uiop:symbol-call :heml :repl)'; \
      echo REPL-EXITED; sleep 30"

expect 'CL-USER>' "heml:repl prompts" 180

# Keys typed before the REPL is editing a line can be lost, so each line
# waits for an empty prompt at the bottom of the screen, and a moment more.
ready() {
    tries=50
    while [ "$tries" -gt 0 ] && ! screen | grep -v '^$' | tail -1 | grep -q '^CL-USER> *$'; do
        sleep 0.2
        tries=$((tries - 1))
    done
    sleep 0.5
}

ready
type_text '(format nil "repl-~A" (* 6 7))'
send Enter
expect 'repl-42' "heml:repl evaluates what is typed"

ready
send C-p
checks=$((checks + 1))
tries=50
while [ "$tries" -gt 0 ] && [ "$(screen | grep -c 'format nil')" -lt 2 ]; do
    sleep 0.2
    tries=$((tries - 1))
done
if [ "$(screen | grep -c 'format nil')" -ge 2 ]; then
    echo "  ok    C-p recalls the last line"
else
    echo "  FAIL  C-p recalls the last line"
    screen | sed 's/^/        | /'
    failures=$((failures + 1))
fi

send C-a C-k
type_text '(defun'
send Enter
expect 'Not a complete form' "an unfinished form is not read"
expect_re '^CL-USER> (defun' "the prompt comes back at the left margin"

# C-d leaves only on an empty line, so wait for the kill to be drawn.
send C-a C-k
tries=50
while [ "$tries" -gt 0 ] && screen | grep -q '^CL-USER> (defun'; do
    sleep 0.2
    tries=$((tries - 1))
done
send C-d
expect 'REPL-EXITED' "C-d on an empty line leaves the REPL" 15

echo "$checks checks, $failures failed"
[ "$failures" -eq 0 ]
