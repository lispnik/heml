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

cd "$(dirname "$0")/.." || exit 1

. test/tmux-lib.sh

# Projects' sessions are kept here, not in ~/.heml, and start empty.  No
# language server this machine has is started, only the stand-in: the
# servers themselves are test/smoke-lsp.sh's.
state=$PWD/build/smoke-tty-state
rm -rf "$state"
tmux new-session -d -s "$session" -x 100 -y 30 \
     "HEML_STATE_DIRECTORY='$state' $LISP $quiet \
        --eval '(asdf:load-system :heml.tty)' \
        --eval '(setf (symbol-value (uiop:find-symbol* :*additional-language-servers* :heml)) nil)' \
        --eval '(uiop:symbol-call :heml :set-lsp-inlay-hints nil)' \
        --eval '(setf (symbol-value (uiop:find-symbol* :*debug-adapters* :heml)) nil)' \
        --eval '(uiop:symbol-call :heml :define-debug-adapter \"fake\" :modes (list \"Python\") :commands (list (list \"python3\" \"$PWD/test/fake-dap.py\")) :launch (uiop:find-symbol* :file-launch :heml))' \
        --eval '(let ((servers (uiop:find-symbol* :*language-servers* :heml))) (setf (symbol-value servers) (mapcar (lambda (entry) (list (first entry) nil (third entry) (fourth entry))) (symbol-value servers))))' \
        --eval '(uiop:symbol-call :heml :define-language-server \"Pascal\" (list (list \"python3\" \"$PWD/test/fake-lsp.py\" \"--refuse\") (list \"python3\" \"$PWD/test/fake-lsp.py\")) :language-id \"pascal\")' \
        --eval '(uiop:symbol-call :heml :define-language-server \"YAML\" (list (list \"python3\" \"$PWD/test/fake-lsp.py\" \"--pull\")))' \
        --eval '(setf (symbol-value (uiop:find-symbol* :*language-server-settings* :heml)) (list (list \"fake\" (cons \"greeting\" \"hello from settings\"))))' \
        --eval '(eval (read-from-string \"(setf (hi:variable-value (quote heml::term-program) :global) \\\"/bin/bash --norc --noprofile\\\")\"))' \
        --eval '(eval (read-from-string \"(setf (hi:variable-value (quote heml::claude-program) :global) \\\"$PWD/test/fake-claude.sh\\\")\"))' \
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

# The menus from the keyboard: M-` picks a menu, then an item.
send 'M-`'
expect 'Menu:' "M-\` asks for a menu"
type_text 'View'
send Enter
expect 'View:' "then for one of its items"
type_text 'Split Window Side by Side'
send Enter
sleep 1
checks=$((checks + 1))
if screen | grep -q 'Heml CL-USER:.*|Heml CL-USER:'; then
    echo "  ok    and runs it"
else
    echo "  FAIL  and runs it"; screen | sed 's/^/        | /'; failures=$((failures + 1))
fi
send C-x 1
sleep 0.5

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
send g
sleep 0.5

# Hidden files are listed, until h hides them.
H=$PWD/build/smoke-tty-hidden
rm -rf "$H"
mkdir -p "$H"
printf 'secret\n' > "$H/.secret"
printf 'plain\n' > "$H/plain.txt"
send C-x d
sleep 0.5
send C-a C-k
type_text "$H/"
send Enter
expect_re ' \.secret$' "Dired lists the files whose names start with a dot"
send h
expect 'Hiding hidden files.' "h hides them"
checks=$((checks + 1))
tries=25
while [ "$tries" -gt 0 ] && screen | grep -q ' \.secret'; do
    sleep 0.2
    tries=$((tries - 1))
done
if screen | grep -q ' plain\.txt' && ! screen | grep -q ' \.secret'; then
    echo "  ok    and they are gone from the listing"
else
    echo "  FAIL  and they are gone from the listing"; screen | sed 's/^/        | /'
    failures=$((failures + 1))
fi
send h
expect 'Showing hidden files.' "and h shows them again"
send C-x k
sleep 0.5
send Enter
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

# Compressing and unpacking: Z on each file, z for one archive of several.
echo "dired compress"
Z=$PWD/build/smoke-tty-compress
rm -rf "$Z"
mkdir -p "$Z/folder" "$Z/work/top" "$Z/work/many"
printf 'a note\n' > "$Z/note.txt"
printf 'another\n' > "$Z/other.txt"
printf 'inside\n' > "$Z/folder/inside.txt"
printf 'x\n' > "$Z/work/top/x.txt"
printf 'p\n' > "$Z/work/many/p.txt"
printf 'q\n' > "$Z/work/many/q.txt"
printf 'only\n' > "$Z/work/only.txt"
tar -czf "$Z/onedir.tar.gz" -C "$Z/work" top
tar -czf "$Z/many.tar.gz" -C "$Z/work/many" p.txt q.txt
tar -czf "$Z/single.tar.gz" -C "$Z/work" only.txt
rm -rf "$Z/work"
# expect_path TEST DESCRIPTION: wait for a test(1) of the file system.
expect_path() {
    checks=$((checks + 1))
    tries=50
    while [ "$tries" -gt 0 ]; do
        if eval "$1"; then
            echo "  ok    $2"
            return 0
        fi
        sleep 0.2
        tries=$((tries - 1))
    done
    echo "  FAIL  $2"; ls -laR "$Z" | sed 's/^/        | /'
    failures=$((failures + 1))
    return 1
}
# on PATTERN: only the files PATTERN matches marked (one * at most).
on() {
    send U
    sleep 0.3
    send %
    sleep 0.3
    type_text "$1"
    send Enter
    sleep 0.5
}
send C-x d
sleep 0.5
send C-a C-k
type_text "$Z/"
send Enter
expect 'single.tar.gz' "Dired lists the archives"
on 'note*'
send Z
expect_path '[ -f "$Z/note.txt.gz" ] && [ ! -e "$Z/note.txt" ]' "Z compresses a file, with gzip"
expect 'note.txt.gz' "and Dired lists what it made"
on 'note*'
send Z
expect_path '[ -f "$Z/note.txt" ] && [ ! -e "$Z/note.txt.gz" ]' "Z on a compressed file uncompresses it"
on 'fold*'
send Z
expect_path '[ -f "$Z/folder.tar.gz" ] && tar -tzf "$Z/folder.tar.gz" | grep -q "folder/inside.txt"' \
    "Z on a directory puts it in a tar beside it"
on 'onedir*'
send Z
expect_path '[ -f "$Z/onedir/x.txt" ] && [ ! -e "$Z/top" ]' \
    "an archive of one directory unpacks into one named as the archive"
on 'many*'
send Z
expect_path '[ -f "$Z/many/p.txt" ] && [ -f "$Z/many/q.txt" ]' \
    "an archive of several things unpacks into a directory named as it"
on 'single*'
send Z
expect_path '[ -f "$Z/only.txt" ] && [ ! -e "$Z/single" ]' \
    "an archive of one file unpacks just the file"
expect_path '! ls -a "$Z" | grep -q unpacking' "and nothing is left of the unpacking"
on 'oth*'
send %
sleep 0.3
type_text 'note*'
send Enter
sleep 0.5
send z
expect 'Compress 2 files to:' "z asks for one archive's name"
send C-a C-k
type_text 'both.zip'
send Enter
expect_path '[ -f "$Z/both.zip" ] && unzip -l "$Z/both.zip" | grep -q other.txt && unzip -l "$Z/both.zip" | grep -q note.txt' \
    "and puts the marked files in it, its type saying what kind"
send C-x k
sleep 0.5
send Enter
sleep 0.5
sleep 0.5

# Tree-sitter indentation, under SBCL and ECL alike: Return indents the new
# line, and a closing brace goes back out.
case $(basename "$LISP") in
    *)
        rm -f build/smoke-tty-indent.c
        send C-x C-f
        sleep 0.5
        send C-a C-k
        type_text "$PWD/build/smoke-tty-indent.c"
        send Enter
        sleep 1
        for line in 'int f(int x) {' 'if (x) {' 'x--;' '}' 'return x;' '}'; do
            type_text "$line"
            send Enter
            sleep 0.3
        done
        sleep 0.5
        checks=$((checks + 1))
        if screen | grep -q '^      if (x) {' && screen | grep -q '^          x--;' \
                && screen | grep -q '^      }' && screen | grep -q '^      return x;' \
                && screen | grep -q '^  }'; then
            echo "  ok    C is indented as it is typed"
        else
            echo "  FAIL  C is indented as it is typed"; screen | sed 's/^/        | /'
            failures=$((failures + 1))
        fi
        send C-x k
        sleep 0.3
        send Enter
        sleep 0.3
        send n
        sleep 0.5
        rm -f build/smoke-tty-indent.rs
        send C-x C-f
        sleep 0.5
        send C-a C-k
        type_text "$PWD/build/smoke-tty-indent.rs"
        send Enter
        sleep 1
        for line in 'fn main() {' 'let x = 1;' 'if x > 0 {' 'x - 1;' '}' '}'; do
            type_text "$line"
            send Enter
            sleep 0.3
        done
        sleep 0.5
        checks=$((checks + 1))
        if screen | grep -q '^      let x = 1;' && screen | grep -q '^      if x > 0 {' \
                && screen | grep -q '^          x - 1;' && screen | grep -q '^      }' \
                && screen | grep -q '^  }'; then
            echo "  ok    Rust is indented as it is typed"
        else
            echo "  FAIL  Rust is indented as it is typed"; screen | sed 's/^/        | /'
            failures=$((failures + 1))
        fi
        send C-x k
        sleep 0.3
        send Enter
        sleep 0.3
        send n
        sleep 0.5
        rm -f build/smoke-tty-indent.yaml
        send C-x C-f
        sleep 0.5
        send C-a C-k
        type_text "$PWD/build/smoke-tty-indent.yaml"
        send Enter
        sleep 1
        for line in 'servers:' 'main:' 'port: 80'; do
            type_text "$line"
            send Enter
            sleep 0.3
        done
        sleep 0.5
        checks=$((checks + 1))
        if screen | grep -q '^servers:' && screen | grep -q '^  main:' \
                && screen | grep -q '^    port: 80'; then
            echo "  ok    YAML is indented under a line that ends with a colon"
        else
            echo "  FAIL  YAML is indented under a line that ends with a colon"; screen | sed 's/^/        | /'
            failures=$((failures + 1))
        fi
        send C-x k
        sleep 0.3
        send Enter
        sleep 0.3
        send n
        sleep 0.5
        rm -f build/smoke-tty-indent.pas
        send C-x C-f
        sleep 0.5
        send C-a C-k
        type_text "$PWD/build/smoke-tty-indent.pas"
        send Enter
        sleep 1
        for line in 'program P;' 'begin' 'if x then' 'y := 1;' 'z := 2;' 'end.'; do
            type_text "$line"
            send Enter
            sleep 0.3
        done
        sleep 0.5
        checks=$((checks + 1))
        if screen | grep -q '^begin' && screen | grep -q '^  if x then' \
                && screen | grep -q '^    y := 1;' && screen | grep -q '^  z := 2;' \
                && screen | grep -q '^end\.'; then
            echo "  ok    Pascal is indented as it is typed"
        else
            echo "  FAIL  Pascal is indented as it is typed"; screen | sed 's/^/        | /'
            failures=$((failures + 1))
        fi
        send C-x k
        sleep 0.3
        send Enter
        sleep 0.3
        send n
        sleep 0.5
        ;;
esac

# Links: C-c C-o follows the one at point, here to a file, and the terminal
# is sent an OSC 8 hyperlink for it (tmux's pipe-pane sees what Heml writes).
printf 'linked-file-contents\n' > build/smoke-tty-linked.txt
printf 'See [the other file](smoke-tty-linked.txt) or https://example.com.\n' > build/smoke-tty-links.txt
rm -f build/smoke-tty-raw.log
tmux pipe-pane -t "$session" -o "cat >> $PWD/build/smoke-tty-raw.log"
send C-x C-f
sleep 0.5
send C-a C-k
type_text "$PWD/build/smoke-tty-links.txt"
send Enter
expect 'See [the other file]' "a file with links is visited"
tmux pipe-pane -t "$session"
checks=$((checks + 1))
if grep -q "]8;;https://example.com" build/smoke-tty-raw.log 2>/dev/null; then
    echo "  ok    the terminal is sent a hyperlink for a URL"
else
    echo "  FAIL  the terminal is sent a hyperlink for a URL"; failures=$((failures + 1))
fi
send 'M-<'
send C-f C-f C-f C-f C-f C-f
send C-c C-o
expect 'linked-file-contents' "C-c C-o follows a link to a file"
send C-x k
sleep 0.3
send Enter
sleep 0.3
send C-x k
sleep 0.3
send Enter
sleep 0.5

# Grep: its lines are coloured and counted, and Return visits one.
send M-x
type_text 'Grep'
send Enter
sleep 0.5
type_text "contents $PWD/build/smoke-tty-linked.txt"
send Enter
expect 'Grep finished: 1 result.' "Grep lists what it finds, and counts it"
send n
sleep 0.5
send Enter
expect_re '(Text.*smoke-tty-linked\.txt' "Return visits the line found"
send C-x 1
send C-x k
sleep 0.3
send Enter
sleep 0.3
send C-x b
sleep 0.3
send C-a C-k
type_text '*grep*'
send Enter
sleep 0.3
send C-x k
sleep 0.3
send Enter
sleep 0.5

# Projects: this repository is one, C-x p f finds a file in it, and C-x p g
# searches it.
send C-x p f
sleep 0.5
type_text 'README.org'
send Enter
expect_re '\[heml\].*README\.org' "C-x p f visits a project's file, and the modeline names the project"
send C-x p g
sleep 0.5
send C-a C-k
type_text 'save-sessions-on-exit'
send Enter
expect 'Grep finished' "C-x p g searches the project" 30
expect_re "Grep in .*/heml/$" "from its root"
send C-x 1
send C-x k
sleep 0.3
send Enter
sleep 0.5

# Completion at point: Tab in a word shows its completions in a popup
# under it, C-n chooses, and Return puts the choice in.
printf 'zebraone zebratwo\nzeb' > build/smoke-tty-comp.txt
send C-x C-f
sleep 0.5
send C-a C-k
type_text "$PWD/build/smoke-tty-comp.txt"
send Enter
expect 'zebraone zebratwo' "a file to complete in is visited"
send 'M->'
send C-b
send Tab
expect_re '^ zebratwo$' "Tab in a word shows its completions in a popup"
send C-n
send Enter
expect_re '^zebratwo' "C-n and Return put the second one in"
send C-x k
sleep 0.3
send Enter
sleep 0.3
send n
sleep 0.5

# A language server: test/fake-lsp.py serves Pascal for the run, and says
# the same of every file.  What it finds wrong is listed, and C-c C-d shows
# what it says, in a popup.  It is the second of two servers named for
# Pascal, the first refusing to start; and it asks for a setting.
printf 'program fake;\n  wrongthing here\nbegin end.\n' > build/smoke-tty-fake.pas
send C-x C-f
sleep 0.5
send C-a C-k
type_text "$PWD/build/smoke-tty-fake.pas"
send Enter
expect 'wrongthing here' "a file with a language server is visited"
sleep 2
send M-x
type_text 'LSP Diagnostics'
send Enter
expect 'smoke-tty-fake.pas:2: error: fake error' "what its language server finds wrong is listed" 20
send Enter
sleep 0.5
send C-c C-d
expect ' fake hover text' "C-c C-d shows what the server says, in a popup" 20
expect ' config: hello from settings' "and the setting it asked Heml for"
send Escape
sleep 0.3
send C-x 1
# What the server says of a line is shown after its end; what it says can
# be folded is; and a completion that is a snippet has places to fill in.
send M-x
type_text 'LSP Inlay Hints'
send Enter
expect 'argument: program fake: hinted;' "LSP Inlay Hints shows what the server infers, where it would be written"
send M-x
type_text 'LSP Code Lenses'
send Enter
expect 'hinted;  [Run the fake lens]' "and LSP Code Lenses what it offers to do there"
send M-x
type_text 'LSP Inlay Hints'
send Enter
send M-x
type_text 'LSP Code Lenses'
send Enter
send 'M-<'
send C-c C-f
expect 'program fake;  ... 1 line' "C-c C-f folds what the server says can be folded"
send C-c C-f
expect 'wrongthing here' "and opens the fold again"
send 'M->'
type_text 'fake_s'
send C-M-i
expect 'fake_snippet(first, second)' "a completion that is a snippet is put in with its places" 20
expect '{ imported }' "with the line the server says it needs"
type_text 'x'
send Tab
type_text 'y'
expect 'fake_snippet(x, y)' "typing at a place replaces what it held, and Tab goes to the next"
send C-x C-s
sleep 0.5
send C-x k
sleep 0.3
send Enter
sleep 0.5

# A server that says what is wrong only when it is asked: the stand-in
# again, as YAML's.
printf 'a: 1\nb: 2\n' > build/smoke-tty-pulled.yaml
send C-x C-f
sleep 0.5
send C-a C-k
type_text "$PWD/build/smoke-tty-pulled.yaml"
send Enter
expect '(1 error)' "a server that waits to be asked what is wrong is asked" 30
send C-x k
sleep 0.3
send Enter
sleep 0.5

# Debugging, with test/fake-dap.py as Python's debugger: a breakpoint, a
# stop there, a step, and the program's output when it is let go on.
printf 'def caller():\n    x = 1\n    point = 2\n    print(x)\n    return x\n' > build/smoke-tty-prog.py
send C-x C-f
sleep 0.5
send C-a C-k
type_text "$PWD/build/smoke-tty-prog.py"
send Enter
expect 'point = 2' "a Python file is visited"
send 'M-<'
send C-n C-n
send C-c d b
expect '●     point = 2' "C-c d b puts a breakpoint on a line, a dot before it"
send C-c d d
expect '●▶    point = 2' "C-c d d debugs the file, which stops at the breakpoint" 20
send C-c d n
expect '▶    print(x)' "C-c d n goes on to the next line" 10
send C-c d c
sleep 2
send C-x b
sleep 0.3
send C-a C-k
type_text 'Debug Output'
send Enter
expect 'hello from the fake program' "and C-c d c lets it go on to the end, its output in Debug Output" 10
send C-x k
sleep 0.3
send Enter
sleep 0.5

# Folding by indentation, with no server: the closing brace goes into the
# fold, and the fold reads as one line.
printf 'int f(void) {\n    return 1;\n}\nint g;\n' > build/smoke-tty-fold.c
send C-x C-f
sleep 0.5
send C-a C-k
type_text "$PWD/build/smoke-tty-fold.c"
send Enter
expect 'return 1;' "a C file is visited"
send C-c C-f
expect 'int f(void) {  ... 1 line }' "C-c C-f folds a block with the brace that closes it"
send C-n
type_text 'x'
expect 'xint g;' "and C-n goes past the fold, brace and all"
send C-x k
sleep 0.3
send Enter
sleep 0.5
send n
sleep 0.5

# Folding by section: a ;;;; title holding two dashed headers' sections,
# each with its marker in a fold column in the fringe; C-c @ folds the
# section point is in and opens it again; Fold All leaves the title.
printf ';;;; Tools\n;;; --- A ---\n(a1)\n(a2)\n;;; --- B ---\n(b1)\n' > build/smoke-tty-sections.lisp
send C-x C-f
sleep 0.5
send C-a C-k
type_text "$PWD/build/smoke-tty-sections.lisp"
send Enter
expect '(a2)' "a file with sections is visited"
expect_re '^▼;;; --- A ---' "its section headers are marked in the fringe"
send M-\< C-n C-n
send C-c @
expect ';;; --- A ---  ... 2 lines' "C-c @ folds the section point is in"
expect_re '^►;;; --- A ---' "and its marker says it is folded"
send C-c @
expect '(a2)' "C-c @ on its header opens it"
send M-x
sleep 0.5
type_text 'Fold All'
send Enter
expect ';;;; Tools  ... 5 lines' "Fold All folds the file to its outermost section"
send M-x
sleep 0.5
type_text 'Unfold All'
send Enter
expect '(b1)' "and Unfold All opens it all"
send C-x k
sleep 0.3
send Enter
sleep 0.5

# Regions marked in comments, as IntelliJ marks them: one whose
# defaultstate is collapsed is folded as the file is read; C-c C-f on
# another's first line folds it; and with the region active C-c C-f folds
# just its lines (Fold Selection).
printf '// region Setup\nint a;\nint b;\n// endregion\n// <editor-fold desc="Hidden" defaultstate="collapsed">\nint d;\n// </editor-fold>\nint e;\nint f;\nint g;\n' > build/smoke-tty-regions.c
send C-x C-f
sleep 0.5
send C-a C-k
type_text "$PWD/build/smoke-tty-regions.c"
send Enter
expect 'int a;' "a file with marked regions is visited"
expect '<editor-fold desc="Hidden" defaultstate="collapsed">  ... 2 lines' "a collapsed editor-fold is folded as the file is read"
send M-\<
send C-c C-f
expect '// region Setup  ... 3 lines' "C-c C-f folds a region from its first line"
send C-c C-f
expect 'int b;' "and opens it"
send M-\>
send C-p C-p C-p
send C-Space C-n C-n C-e
send C-c C-f
expect 'int e;  ... 2 lines' "C-c C-f with the region active folds its lines"
send C-x k
sleep 0.3
send Enter
sleep 0.5

# A file known by its name: a shell's own file is a shell script.
mkdir -p build/smoke-tty-dot
printf 'export EDITOR=heml # a comment\n' > build/smoke-tty-dot/.zshrc
send C-x C-f
sleep 0.5
send C-a C-k
type_text "$PWD/build/smoke-tty-dot/.zshrc"
send Enter
expect 'export EDITOR=heml' "a .zshrc is visited"
expect '(Shell Script' "and is in Shell Script mode, by its name"
send C-x k
sleep 0.3
send Enter
sleep 0.5

# A mode chosen by hand: a file whose name does not say what it is.
printf 'echo "$HOME" # a comment\n' > build/smoke-tty-noname
send C-x C-f
sleep 0.5
send C-a C-k
type_text "$PWD/build/smoke-tty-noname"
send Enter
expect 'echo "$HOME"' "a file with no type is visited"
send M-x
type_text 'Shell Script Mode'
send Enter
expect '(Shell Script' "M-x Shell Script Mode puts the buffer in that mode"
send C-x k
sleep 0.3
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

# The shell's command is on a terminal that is its controlling terminal,
# and C-c C-c sends SIGINT to it: were it not interrupted, the sleep would
# hold the next command for a hundred seconds.
send 'M->'
type_text 'sleep 100'
send Enter
sleep 1
send C-c C-c
sleep 0.5
type_text 'echo int-$((2*3))'
send Enter
expect 'int-6' "C-c C-c interrupts the shell's command" 10

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

# Return in Lisp mode indents the new line, as "Lisp Indent on Return"
# says, and not when it is NIL.
mkdir -p build/smoke-tty-lisp
rm -f build/smoke-tty-lisp/ret.lisp
send C-x C-f
sleep 0.5
send C-a C-k
type_text "$PWD/build/smoke-tty-lisp/ret.lisp"
send Enter
sleep 1
type_text '(defun twice (x)'
send Enter
type_text '(* x 2))'
expect_re '^  (\* x 2))' "Return in Lisp mode indents the new line"
send M-x
sleep 0.5
type_text 'Set Variable'
send Enter
sleep 0.5
type_text 'Lisp Indent on Return'
send Enter
sleep 0.5
type_text 'nil'
send Enter
sleep 0.5
send Enter
type_text '(list'
send Enter
type_text 'margin)'
expect_re '^margin)' "and with \"Lisp Indent on Return\" NIL, does not"
send Enter
# Backspace keeps a list's parentheses: after a closing one it moves inside,
# and an empty list goes whole.
type_text '(setq foo 42)'
send BSpace
type_text 'X'
expect '(setq foo 42X)' "Backspace after a closing paren moves inside the list"
send C-e Enter
type_text '(f ())'
send BSpace BSpace BSpace
type_text 'Y'
expect '(f Y)' "and an empty list is taken away whole"
send C-e Enter
# Structural editing, from sexp-edit, as the Lisp Listener has it: ( and "
# put in pairs, ) steps over, Delete before a list goes into it, M-( wraps,
# C-c ) slurps, and C-M-q indents by the shared rules.
type_text '(car "a'
expect '(car "a")' "( and \" put in their pairs"
send C-e Enter
type_text '(g (h))'
send C-a C-d
type_text 'Z'
expect '(Zg (h))' "Delete before a matched paren moves inside the list"
send C-e Enter
type_text '(m n)'
send C-b
send 'M-('
type_text 'w '
expect '(w (m n))' "M-( wraps the form in a list"
send C-e Enter
type_text '(s) t'
send C-a C-f C-f
send C-c ')'
expect '(s t)' "C-c ) slurps the next form into the list"
send C-e Enter
type_text '(defun f (x)'
send Enter
type_text '(flet ((g (y)'
send Enter
type_text '(* y 2)))'
send Enter
type_text '(loop for i in x'
send Enter
type_text 'collect (g i)'
# Up to the defun: every line starts at the margin, so C-M-a would stop at
# the first of them that starts with a parenthesis.
send C-M-u C-M-u C-M-u C-M-q
expect_re '^  (flet ((g (y)$' "C-M-q indents a form by sexp-edit's rules"
expect_re '^           (\* y 2)))$' "a function FLET defines like a DEFUN"
expect_re '^          collect (g i))))$' "and LOOP's clauses under the first"
send C-M-e Enter
# Every case of sexp-edit's corpus, through Heml's buffers: the same answers
# the library gives on a string, and the Lisp Listener on a text view.
send M-x
sleep 0.5
type_text 'Editor Evaluate Expression'
send Enter
sleep 0.5
type_text '(progn (load (asdf:system-relative-pathname "sexp-edit" "tests/cases.lisp")) (heml::sexp-corpus-failures (symbol-value (find-symbol "*EDIT-CASES*" "SEXP-EDIT-TESTS"))))'
send Enter
expect 'cases, 0 failed' "Heml edits sexp-edit's corpus as the library does" 90
send C-x C-s
sleep 0.5

# A terminal: bash on a pseudo-terminal of its own, its screen emulated by
# libvterm.  What it prints, coloured; its size, the window's; a program on
# the alternate screen; ^C, and ^Z with job control; a resized window; Term
# Copy mode; and its end.
send M-x
sleep 0.5
type_text 'Term'
send Enter
expect 'bash-' "M-x Term runs a shell in a terminal" 20
type_text "printf '\\033[31mred\\033[0m plain\\n'"
send Enter
expect 'red plain' "what the program prints is shown"
checks=$((checks + 1))
if tmux capture-pane -p -e -t "$session" | grep -q "$(printf '\033')\\[31mred"; then
    echo "  ok    in its colours"
else
    echo "  FAIL  in its colours"
    tmux capture-pane -p -e -t "$session" | grep 'red plain' | cat -v | sed 's/^/        | /'
    failures=$((failures + 1))
fi
type_text '[ $(tput cols) -gt 80 ] && echo wide-$(tput lines)'
send Enter
expect_re 'wide-2[0-9]' "the terminal is as big as the window"
type_text "printf 'first\\nsecond\\n' | less"
send Enter
expect '(END)' "less runs in it"
send q
sleep 0.5
type_text 'echo less-done'
send Enter
expect 'less-done' "and q leaves it"
type_text 'sleep 100'
send Enter
sleep 1
send C-c C-c
sleep 0.5
type_text 'echo int-$((3*3))'
send Enter
expect 'int-9' "C-c C-c interrupts what runs in it" 10
type_text 'sleep 50'
send Enter
sleep 1
send C-z
expect 'Stopped' "C-z stops it: the shell has job control" 10
type_text 'kill %1'
send Enter
send C-x 2
sleep 1.5
type_text 'echo rows-$(tput lines)'
send Enter
expect_re 'rows-1[0-9]' "a smaller window makes a smaller terminal" 10
send C-x 1
sleep 0.5
send C-c C-j
expect '(Term Copy)' "C-c C-j goes to Term Copy mode"
send q
expect '(Term)' "and q comes back"
type_text 'exit'
send Enter
expect 'The program ended with code 0' "the program's end, and its code, are shown" 10


# Claude Code: "Claude" runs "Claude Program" (here a stand-in that prints
# what it was given) at the project's root, with the project's own Claude
# directory, and Heml serves it as its IDE, which test/fake-claude-ide.py
# plays the client of.
echo "claude"
C=$PWD/build/smoke-tty-claude
rm -rf "$C"
mkdir -p "$C/proj/src" "$C/config"
printf '(:name "claudeproj" :variables (("Claude Config Directory" . "%s/config/")))\n' "$C" > "$C/proj/.heml-project"
printf 'first line\nsecond line\n' > "$C/proj/src/hello.txt"
send C-x C-f
sleep 0.5
send C-a C-k
type_text "$C/proj/src/hello.txt"
send Enter
expect 'first line' "a file of the project is visited"
send C-c a a
expect "CONFIG=$C/config" "C-c a a runs Claude with the project's Claude directory" 10
expect "PWD=$C/proj" "at the project's root"
expect_re 'PORT=[0-9][0-9]* IDE=true' "and told where Heml serves it as its IDE"
checks=$((checks + 1))
lockfile=$(ls "$C"/config/ide/*.lock 2>/dev/null | head -1)
if [ -n "$lockfile" ] && ls -l "$lockfile" | grep -q "^-rw------- "; then
    echo "  ok    the lock file is in that directory, for this user alone"
else
    echo "  FAIL  the lock file is in that directory, for this user alone"
    ls -laR "$C/config" | sed 's/^/        | /'; failures=$((failures + 1))
fi
python3 test/fake-claude-ide.py "$C/config" > "$C/client.out" 2>&1
client_has() {
    checks=$((checks + 1))
    if grep -qF -- "$1" "$C/client.out"; then
        echo "  ok    $2"
    else
        echo "  FAIL  $2"; sed 's/^/        | /' "$C/client.out"; failures=$((failures + 1))
    fi
}
client_has 'status HTTP/1.1 101' "a client with the token is let in"
client_has 'initialize Heml 2024-11-05' "and MCP is initialized"
client_has 'openDiff' "the tools are listed"
client_has "$C/proj" "getWorkspaceFolders names the project"
client_has 'hello.txt' "getOpenEditors names the file open"
client_has 'getCurrentSelection {' "getCurrentSelection answers"
client_has 'nonesuch ERROR' "and a tool there is not is an error"
python3 test/fake-claude-ide.py "$C/config" --bad-token > "$C/client.out" 2>&1
client_has 'refused HTTP/1.1 401' "a client without the token is refused"
python3 test/fake-claude-ide.py "$C/config" --diff "$C/proj/src/hello.txt" 'first line
changed line
' > "$C/client.out" 2>&1 &
client=$!
expect '+changed line' "openDiff shows the change proposed as a diff" 10
send C-x o
sleep 0.3
send C-c C-c
checks=$((checks + 1))
tries=50
while kill -0 $client 2>/dev/null && [ $tries -gt 0 ]; do sleep 0.2; tries=$((tries - 1)); done
if grep -qF 'openDiff FILE_SAVED | first line\nchanged line\n' "$C/client.out"; then
    echo "  ok    and C-c C-c accepts it, answering with the text to write"
else
    echo "  FAIL  and C-c C-c accepts it, answering with the text to write"
    sed 's/^/        | /' "$C/client.out"; screen | sed 's/^/        | /'; failures=$((failures + 1))
fi
kill $client 2>/dev/null
send C-x b
sleep 0.3
send C-a C-k
type_text '*claude proj*'
send Enter
sleep 0.5
send C-x k
sleep 0.3
send Enter
sleep 0.5
send C-x C-c
sleep 1
send n
expect 'EDITOR-RETURNED' "C-x C-c leaves the editor" 15
checks=$((checks + 1))
if ls "$C"/config/ide/*.lock >/dev/null 2>&1; then
    echo "  FAIL  and takes its lock file away"; failures=$((failures + 1))
else
    echo "  ok    and takes its lock file away"
fi

# The :mini backend: HEML:REPL, a REPL whose lines are edited by Heml
# where they stand rather than on a screen of their own.
tmux kill-session -t "$session" 2>/dev/null
session=heml-smoke-repl-$$
# Projects' sessions are kept here, not in ~/.heml, and start empty.  No
# language server this machine has is started, only the stand-in: the
# servers themselves are test/smoke-lsp.sh's.
state=$PWD/build/smoke-tty-state
rm -rf "$state"
tmux new-session -d -s "$session" -x 100 -y 30 \
     "HEML_STATE_DIRECTORY='$state' $LISP $quiet \
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

# Lisp mode's structural editing works here too: the pairs close
# themselves, and Backspace after a list goes inside it.
send C-a C-k
ready
type_text '(list (+ 1 2'
send Enter
expect_re '^(3)' "heml:repl closes the parentheses as they are typed"
ready
type_text '(+ 1 2)'
send BSpace
type_text ' 3'
send Enter
expect_re '^6$' "and Backspace after a list goes inside it"

# Meta-Return starts a new line without reading the form, indented past
# the prompt as the Lisp Listener's Option-Return indents.
ready
type_text '(defun twice (x)'
send M-Enter
type_text '(* x 2)'
expect_re '^           (\* x 2))' "M-Return at heml:repl starts an indented line, unread"
send Enter
ready
type_text '(twice 21)'
send Enter
expect_re '^42$' "and Return reads the form once it is complete"

ready
send C-a C-k
# A parenthesis on its own: C-q puts in just the character.
send C-q
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
