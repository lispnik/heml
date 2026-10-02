#!/bin/sh
# test/smoke-lsp.sh -- `make smoke-lsp`: the language servers themselves.
#
# test/smoke-tty.sh and test/smoke.lisp check Heml's client against a
# stand-in, test/fake-lsp.py.  This runs the real ones: for each language a
# small project is written into build/smoke-lsp/, with a function, a call to
# it and one mistake, and the TTY editor, in tmux, is asked what its server
# says: the mistake, what the function is, where it is defined, and what
# completes its name.
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


# The projects.  A source file is its function, forty lines of comment, and
# then the call and the mistake on the line after it: so the definition is
# off the screen while the call is on it, and what the server says of the
# function is not something the screen already showed.

# project NAME: a directory that is a project's root.
project() {
    mkdir -p "$dir/$1/.git"
}

# filler COMMENT: forty lines of comment.
filler() {
    i=1
    while [ "$i" -le 40 ]; do
        echo "$1 filler $i"
        i=$((i + 1))
    done
}

project c
{
    printf 'static int add_one(int x) { return x + 1; }\n'
    filler '//'
    printf 'int main(void) {\n    int y = add_one(1);\n    return y +;\n}\n'
} > "$dir/c/main.c"

project python
{
    printf 'def add_one(x):\n    return x + 1\n'
    filler '#'
    printf 'y = add_one(1)\nz = y +\n'
} > "$dir/python/main.py"

project shell
{
    printf '#!/bin/sh\nadd_one() {\n    echo $(($1 + 1))\n}\n'
    filler '#'
    printf 'add_one 1\nif then\n'
} > "$dir/shell/main.sh"

project rust
mkdir -p "$dir/rust/src"
printf '[package]\nname = "smoke"\nversion = "0.1.0"\nedition = "2021"\n' > "$dir/rust/Cargo.toml"
{
    printf 'fn add_one(x: i32) -> i32 {\n    x + 1\n}\n'
    filler '//'
    printf 'fn main() {\n    let y = add_one(1);\n    println!("{}", y +);\n}\n'
} > "$dir/rust/src/main.rs"

project go
printf 'module smoke\n\ngo 1.21\n' > "$dir/go/go.mod"
{
    printf 'package main\n\nfunc addOne(x int) int {\n\treturn x + 1\n}\n'
    filler '//'
    printf 'func main() {\n\ty := addOne(1)\n\tprintln(y +)\n}\n'
} > "$dir/go/main.go"

project typescript
printf '{ "compilerOptions": { "strict": true } }\n' > "$dir/typescript/tsconfig.json"
{
    printf 'function addOne(x: number): number {\n    return x + 1;\n}\n'
    filler '//'
    printf 'const y = addOne(1);\nconst z: string = y;\n'
} > "$dir/typescript/main.ts"

project javascript
{
    printf 'function addOne(x) {\n    return x + 1;\n}\n'
    filler '//'
    printf 'const y = addOne(1);\nconst z = y +;\n'
} > "$dir/javascript/main.js"

project json
printf '{\n  "a": 1\n  "b": 2\n}\n' > "$dir/json/data.json"

project yaml
printf 'a: 1\nb: [1, 2\nc: 3\n' > "$dir/yaml/data.yaml"


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
    send Enter
    sleep 0.5
    send C-x 1
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

if wanted c clangd; then
    mistake "$dir/c/main.c" 'return y +'
    function_checks "$dir/c/main.c" add_one 'function add_one' 'static int add_one' 1
fi

if wanted python pyright-langserver basedpyright-langserver pylsp jedi-language-server; then
    mistake "$dir/python/main.py" 'z = y +'
    function_checks "$dir/python/main.py" add_one '(function)' 'def add_one' 0
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
    function_checks "$dir/rust/src/main.rs" add_one 'fn add_one(x: i32) -> i32' 'fn add_one' 1
fi

if wanted go gopls; then
    mistake "$dir/go/main.go" 'println(y +)'
    function_checks "$dir/go/main.go" addOne 'func addOne(x int) int' 'func addOne' 1
fi

# typescript-language-server wants a TypeScript older than 7, whose compiler
# is a server itself: Heml tries the one and then the other.
if wanted typescript typescript-language-server tsc; then
    mistake "$dir/typescript/main.ts" 'const z'
    function_checks "$dir/typescript/main.ts" addOne 'function addOne(x: number): number' 'function addOne' 0
fi

if wanted javascript typescript-language-server tsc; then
    mistake "$dir/javascript/main.js" 'const z'
    function_checks "$dir/javascript/main.js" addOne 'function addOne(x: any): any' 'function addOne' 0
fi

if wanted json vscode-json-language-server vscode-json-languageserver; then
    mistake "$dir/json/data.json" '"b": 2'
fi

if wanted yaml yaml-language-server; then
    mistake "$dir/yaml/data.yaml" 'c: 3'
fi

if [ "$failures" -gt 0 ] && [ -f "$log" ]; then
    echo "the end of $log:"
    tail -20 "$log" | cut -c1-400 | sed 's/^/        | /'
fi

echo "$checks checks, $failures failed, $skipped skipped"
[ "$failures" -eq 0 ]
