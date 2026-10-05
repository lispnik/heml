#!/bin/sh
# test/smoke-git.sh -- `make smoke-git`: Git, in the TTY editor, in tmux,
# against a repository made for the run in build/smoke-git/repo: the marks
# in the fringe beside lines changed since the last commit, the status
# buffer (a hunk staged, a file staged and unstaged, a commit, a change
# discarded), the log, a commit shown and a line of its diff visited, the
# blame, and a file's diff.

set -u

LISP=${LISP:-sbcl}
case $(basename "$LISP") in
    ecl*) quiet= ;;
    *) quiet=--noinform ;;
esac
session=heml-smoke-git-$$

cd "$(dirname "$0")/.." || exit 1

. test/tmux-lib.sh

dir=$PWD/build/smoke-git
repo=$dir/repo
rm -rf "$dir"
mkdir -p "$repo"
(
    cd "$repo" || exit 1
    git init -q -b main
    git config user.email test@example.com
    git config user.name "Heml Test"
    printf 'one\ntwo\nthree\nfour\nfive\n' > notes.txt
    git add notes.txt
    git commit -q -m "First commit"
    # Line 2 changed and line 6 added; and a file Git does not know.
    printf 'one\nTWO\nthree\nfour\nfive\nsix\n' > notes.txt
    printf 'new\n' > new.txt
)

# expect_gone PATTERN DESCRIPTION: PATTERN (a basic regular expression)
# leaves the screen within a few seconds.
expect_gone() {
    checks=$((checks + 1))
    tries=25
    while [ "$tries" -gt 0 ]; do
        if ! screen | grep -q -- "$1"; then
            echo "  ok    $2"
            return 0
        fi
        sleep 0.2
        tries=$((tries - 1))
    done
    echo "  FAIL  $2"
    echo "        (\"$1\" is still on the screen:)"
    screen | sed 's/^/        | /'
    failures=$((failures + 1))
}

press() {
    n=$2
    while [ "$n" -gt 0 ]; do send "$1"; n=$((n - 1)); done
}

tmux new-session -d -s "$session" -x 120 -y 40 \
     "HEML_STATE_DIRECTORY='$dir/state' $LISP $quiet \
        --eval '(asdf:load-system :heml.tty)' \
        --eval '(eval (read-from-string \"(setf (hi:variable-value (quote heml::language-servers) :global) nil)\"))' \
        --eval '(uiop:symbol-call :heml :heml (list \"$repo/notes.txt\") :backend-type :tty :load-user-init nil)' \
        --eval '(progn (format t \"~%EDITOR-RETURNED~%\") (finish-output) (sleep 30))'"

expect "notes.txt" "the editor starts, visiting a file in a repository" 180
expect_re '^▎TWO' "a changed line is marked in the fringe"
expect_re '^▎six' "and an added one"
expect_re '^ one' "and an unchanged one is not"

send 'M->'
type_text 'seven'
send Enter
expect_re '^▎seven' "a line typed is marked within a second, before it is saved" 5
send C-x C-s
sleep 0.5

send C-x g
expect "Unstaged changes (1)" "C-x g shows the status: the changed file" 10
expect "Untracked files (1)" "and the untracked one"
expect "First commit" "and the commits"
# Head, then Untracked files, new.txt, Unstaged changes, notes.txt.
send 'M-<'
press n 4
send Tab
expect_re '^@@ -1,5 +1,7 @@' "Tab shows the file's hunk"
send n
send s
expect "Staged changes (1)" "s on the hunk stages it" 10
expect_gone 'Unstaged changes' "and nothing is left unstaged"

send 'M-<'
press n 2
send s
expect "Staged changes (2)" "s on an untracked file stages it" 10
expect_gone 'Untracked files' "and it is no longer untracked"
# Head, Staged changes, new.txt (before notes.txt).
send 'M-<'
press n 2
send u
expect "Untracked files (1)" "u unstages it again" 10

send c
expect "Write the commit's message" "c opens a buffer for the commit's message" 10
type_text 'Second commit'
send C-c C-c
expect_re '^  [0-9a-f]* Second commit' "C-c C-c commits, and the status shows it" 10
expect_gone 'Staged changes' "with nothing staged"

send C-x b
sleep 0.3
send C-a C-k
type_text 'notes.txt'
send Enter
expect_re '^ TWO' "after the commit, the file's marks are gone" 5

send C-x v l
expect_re '^[0-9a-f]* [0-9-]* Heml Test *Second commit' "C-x v l lists the file's commits" 10
send 'M-<' Enter
expect_re '^-two' "Return shows the commit's diff" 10
# The hunk: @@, one, -two, +TWO.
send C-x o
send 'M-<'
send n
# Return on the hunk's @@ line visits its first line.
send Enter
sleep 1
type_text '@@'
expect_re '^.@@one' "Return on a hunk's @@ line visits its first line" 5
send BSpace BSpace
send C-x o
press C-n 3
send Enter
sleep 1
type_text '@@'
expect_re '^.@@TWO' "Return on a line of the diff visits it" 5
send BSpace BSpace
send C-x 1
sleep 0.3

send C-x v g
expect_re '[0-9a-f]\{8\} [0-9-]\{10\} Heml Test *│ TWO' "C-x v g shows who last changed each line" 10
send q
sleep 0.3

send 'M->'
type_text 'eight'
send Enter
send C-x v =
expect_re '^+eight' "C-x v = saves the file and shows how it differs" 10
send C-x 1

send C-x g
expect "Unstaged changes (1)" "the status shows the change" 10
send 'M-<'
press n 4
send k
expect "Discard the changes to notes.txt?" "k asks before discarding a change" 10
type_text 'y'
send Enter
expect_gone 'Unstaged changes' "and discards it"

echo "$checks checks, $failures failed"
[ "$failures" -eq 0 ]
