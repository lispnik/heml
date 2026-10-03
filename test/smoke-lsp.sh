#!/bin/sh
# test/smoke-lsp.sh -- `make smoke-lsp`: the language servers themselves.
#
# test/smoke-tty.sh and test/smoke.lisp check Heml's client against a
# stand-in, test/fake-lsp.py.  This runs the real ones: for each language a
# small project, from test/fixtures/lsp/, with a function, a call to it, a
# call to one in another file and one mistake, and the TTY editor, in tmux,
# is asked what its server says: the mistake, what the function is, where
# it and the other file's are defined, and what completes its name.
#
# A server that is not installed is skipped, and said to be; with
# SMOKE_LSP_STRICT=1 that is a failure, which is how CI runs it.
# SMOKE_LSP_ONLY="rust go" runs only those.  What Heml and the servers said
# to each other is in build/smoke-lsp/lsp.log.

set -u

# LISP=ecl runs the same checks under ECL.
LISP=${LISP:-sbcl}
case $(basename "$LISP") in
    ecl*) quiet= ;;
    *) quiet=--noinform ;;
esac
session=heml-smoke-lsp-$$
skipped=0

cd "$(dirname "$0")/.." || exit 1

. test/tmux-lib.sh

dir=$PWD/build/smoke-lsp
rm -rf "$dir"
mkdir -p "$dir"
log=$dir/lsp.log


# The projects are test/fixtures/lsp/, a directory for each language, copied
# here so that what the checks type and what the servers leave behind is in
# build/.  Each has a file .heml-project, which makes it a project, and two
# or three source files: one, main, has a function, forty lines of comment,
# and then a call to a function of another file, a call to its own, and a
# mistake, on three lines.  The comment puts the function out of sight of
# its call, so that what the server says of it is not already on the screen.

cp -R test/fixtures/lsp/. "$dir"


# The editor, wide enough for a modeline to hold a file's name, its project
# and what its server counts.

tmux new-session -d -s "$session" -x 200 -y 30 \
     "PATH='$PATH' HEML_STATE_DIRECTORY='$dir/state' HEML_LSP_LOG='$log' $LISP $quiet \
        --eval '(asdf:load-system :heml.tty)' \
        --eval '(uiop:symbol-call :heml :heml nil :backend-type :tty :load-user-init nil)' \
        --eval '(progn (format t \"~%EDITOR-RETURNED~%\") (finish-output) (sleep 30))'"

expect "Heml CL-USER:" "the editor starts" 180


# wanted LANGUAGE PROGRAM...: true when LANGUAGE is to be checked, which it
# is when one of the programs that serve it is installed.
wanted() {
    language=$1
    shift
    case " ${SMOKE_LSP_ONLY:-$language} " in
        *" $language "*) ;;
        *) return 1 ;;
    esac
    for program in "$@"; do
        if command -v "$program" > /dev/null 2>&1; then
            echo "$language ($program)"
            return 0
        fi
    done
    if [ -n "${SMOKE_LSP_STRICT:-}" ]; then
        checks=$((checks + 1))
        failures=$((failures + 1))
        echo "  FAIL  $language: $1 is not installed"
    else
        skipped=$((skipped + 1))
        echo "  skip  $language: $1 is not installed"
    fi
    return 1
}

# A popup is dismissed with C-g, which does nothing when there is none: an
# Escape would then be the next key's Meta.

# press KEY TIMES
press() {
    n=$2
    while [ "$n" -gt 0 ]; do
        send "$1"
        n=$((n - 1))
    done
}

visit() {
    send C-x C-f
    sleep 0.5
    send C-a C-k
    type_text "$1"
    send Enter
}

# mistake FILE TEXT: FILE is visited, its server says a line holding TEXT is
# wrong, and point is left there, in the only window.
mistake() {
    file=$1
    name=$(basename "$file")
    line=$(grep -n -F -- "$2" "$file" | tail -1 | cut -d: -f1)
    visit "$file"
    expect_re "([0-9]* error.*$name" "its server finds the mistake, which the modeline counts" 120
    send M-x
    type_text 'LSP Diagnostics'
    send Enter
    expect "$name:$line: error" "and LSP Diagnostics lists it, at its line" 30
    # Back to the file, and to the mistake's line: the list may have more
    # in it than the mistake.
    send C-x o
    sleep 0.3
    send C-x 1
    send 'M-<'
    press C-n $((line - 1))
    sleep 0.5
}

# function_checks FILE NAME HOVER DEFINITION BELOW: with point on the
# mistake's line and NAME called on the line before it, C-c C-d on the call
# shows HOVER, M-. goes to the line that starts with DEFINITION, and, on a
# new line after the call, the start of NAME is completed.  BELOW is how
# many lines of the file come after the mistake's.
function_checks() {
    file=$1 name=$2
    column=$(grep -F -- "$name" "$file" | tail -1 | awk -v name="$name" '{ print index($0, name) }')
    send C-p C-a
    press C-f "$column"
    sleep 0.3
    send C-c C-d
    expect "$3" "C-c C-d on a call says what is called" 60
    send C-g
    sleep 0.5
    send M-.
    expect_re "^$4" "M-. goes to its definition" 60
    send 'M->'
    press C-p $(( $5 + 2 ))
    send C-e Enter
    type_text "$(printf %s "$name" | cut -c1-4)"
    sleep 1
    send C-M-i
    checks=$((checks + 1))
    tries=150
    while [ "$tries" -gt 0 ] && [ "$(screen | grep -c -F -- "$name")" -lt 2 ]; do
        sleep 0.2
        tries=$((tries - 1))
    done
    if [ "$tries" -gt 0 ]; then
        echo "  ok    C-M-i completes its name"
    else
        echo "  FAIL  C-M-i completes its name"
        screen | sed 's/^/        | /'
        failures=$((failures + 1))
    fi
    send C-g
    sleep 0.3
}

# other_file FILE NAME OTHER DEFINITION: with point on the mistake's line,
# and NAME, which another file defines, called two lines before it, M-. on
# that call visits the file OTHER, at the line that starts with DEFINITION.
# Point is left on the mistake's line again.
other_file() {
    file=$1 name=$2
    column=$(grep -F -- "$name" "$file" | tail -1 | awk -v name="$name" '{ print index($0, name) }')
    send C-p C-p C-a
    press C-f "$column"
    sleep 0.3
    send M-.
    expect_re "([^)]*)  .*$3" "M-. on a call to another file's function visits that file" 60
    expect_re "^$4" "at its definition" 10
    visit "$file"
    sleep 0.5
    send C-n C-n
}

if wanted c clangd; then
    mistake "$dir/c/main.c" 'return y +'
    other_file "$dir/c/main.c" scale 'util\.[ch]' 'int scale(int x)'
    # What the server says can be folded: main, from its first line.
    send C-p C-p C-p
    send C-c C-f
    expect_re 'int main(void) {  \.\.\. [0-9]* lines }' "C-c C-f folds what its server says can be folded, and the brace that closes it"
    send C-c C-f
    expect 'return y +' "and opens the fold again"
    send C-n C-n C-n
    function_checks "$dir/c/main.c" add_one 'function add_one' 'static int add_one' 1
    # On the function's name, where it is defined: who calls it, and
    # another name for it.
    send 'M-<'
    send C-n C-n
    press C-f 11
    send C-c C-u
    expect 'function main' "C-c C-u lists what calls a function" 30
    send C-x 1
    visit "$dir/c/main.c"
    sleep 0.5
    send 'M-<'
    send C-n C-n
    press C-f 11
    send M-x
    type_text 'LSP Rename'
    send Enter
    expect 'Rename to:' "LSP Rename asks for the new name, when the server says the thing can be renamed" 20
    send C-a C-k
    type_text 'plus_one'
    send Enter
    expect 'static int plus_one(int x)' "and renames it" 30
fi

if wanted python pyright-langserver basedpyright-langserver pylsp jedi-language-server; then
    mistake "$dir/python/main.py" 'z = y +'
    other_file "$dir/python/main.py" scale 'util\.py' 'def scale'
    function_checks "$dir/python/main.py" add_one '(function)' 'def add_one' 0
fi

# Ruff, a second server for Python's buffers: what is wrong is what either
# server finds.
if wanted ruff ruff; then
    visit "$dir/python/lint.py"
    expect_re '(2 warnings).*lint.py' "a second server's findings are counted with the first's" 60
    send M-x
    type_text 'LSP Diagnostics'
    send Enter
    expect 'imported but unused' "and listed with them" 30
    send C-x 1
    sleep 0.3
fi

# What bash-language-server finds wrong, ShellCheck finds for it.
if wanted shell bash-language-server; then
    if command -v shellcheck > /dev/null 2>&1; then
        mistake "$dir/shell/main.sh" 'if then'
    else
        visit "$dir/shell/main.sh"
        expect 'add_one 1' "a shell script is visited"
        send 'M->'
        send C-p
    fi
    function_checks "$dir/shell/main.sh" add_one 'Function: ' 'add_one() {' 0
fi

if wanted rust rust-analyzer; then
    mistake "$dir/rust/src/main.rs" 'y +'
    # The type it infers for y, after y.
    expect 'let y: i32 = add_one(1);' "the types its server infers are shown where they would be written" 60
    other_file "$dir/rust/src/main.rs" scale 'util\.rs' 'pub fn scale'
    function_checks "$dir/rust/src/main.rs" add_one 'fn add_one(x: i32) -> i32' 'fn add_one' 1
    # What it offers to do with main, run, is done as a compilation.
    send M-x
    type_text 'LSP Code Lenses'
    send Enter
    send 'M-<'
    press C-n $(( $(grep -n 'fn main' "$dir/rust/src/main.rs" | cut -d: -f1) - 1 ))
    expect_re 'fn main() {  \[.*Run' "LSP Code Lenses shows what its server offers to do with a line" 60
    # Run is the only one offered for main, and so is done at once.
    send C-c C-l
    expect "cargo 'run' '--package' 'smoke'" "C-c C-l does it: Run runs cargo, as a compilation" 30
    expect_re 'error.*main.rs\|main.rs.*error\|expected' "which finds the mistake" 120
    send C-x 1
    send M-x
    type_text 'LSP Code Lenses'
    send Enter
fi

if wanted go gopls; then
    mistake "$dir/go/main.go" 'println(y +)'
    other_file "$dir/go/main.go" scale 'util\.go' 'func scale'
    function_checks "$dir/go/main.go" addOne 'func addOne(x int) int' 'func addOne' 2
fi

# typescript-language-server wants a TypeScript older than 7, whose compiler
# is a server itself: Heml tries the one and then the other.
if wanted typescript typescript-language-server tsc; then
    mistake "$dir/typescript/main.ts" 'const z'
    other_file "$dir/typescript/main.ts" scale 'util\.ts' 'export function scale'
    function_checks "$dir/typescript/main.ts" addOne 'function addOne(x: number): number' 'function addOne' 1
fi

if wanted javascript typescript-language-server tsc; then
    mistake "$dir/typescript/main.js" 'const w'
    function_checks "$dir/typescript/main.js" addTwo 'function addTwo(x: any): any' 'function addTwo' 0
    # No server was started between the two files' being opened.
    case " ${SMOKE_LSP_ONLY:-typescript} " in
        *" typescript "*)
            checks=$((checks + 1))
            started=$(grep -n '"method":"initialize"' "$log" | grep 'smoke-lsp/typescript"' | tail -1 | cut -d: -f1)
            opened=$(grep -n '"method":"textDocument/didOpen"' "$log" | grep 'typescript/main\.ts"' | head -1 | cut -d: -f1)
            if [ -n "$started" ] && [ -n "$opened" ] && [ "$started" -lt "$opened" ]; then
                echo "  ok    a project's JavaScript and TypeScript have one server"
            else
                echo "  FAIL  a project's JavaScript and TypeScript have one server"
                failures=$((failures + 1))
            fi ;;
    esac
fi

if wanted json vscode-json-language-server vscode-json-languageserver; then
    mistake "$dir/json/data.json" '"b": 2'
fi

if wanted yaml yaml-language-server; then
    mistake "$dir/yaml/data.yaml" 'c: 3'
    send 'M-<'
    sleep 0.3
    send C-c C-d
    expect 'as its schema describes it' "its server has the schema the project's settings name" 30
    send C-g
fi

if [ "$failures" -gt 0 ] && [ -f "$log" ]; then
    echo "the end of $log:"
    tail -20 "$log" | cut -c1-400 | sed 's/^/        | /'
fi

echo "$checks checks, $failures failed, $skipped skipped"
[ "$failures" -eq 0 ]
