#!/bin/sh
# test/smoke-dap.sh -- `make smoke-dap`: the debuggers themselves.
#
# test/smoke-tty.sh and test/smoke.lisp check Heml's debugger client against
# a stand-in, test/fake-dap.py.  This runs the real ones -- lldb-dap on a C
# program, debugpy on a Python one, Delve on a Go one -- in the TTY editor,
# in tmux: a breakpoint, a stop there, a step, an expression evaluated, and
# what the program prints when it is let go on.
#
# A debugger that is not installed is skipped, and said to be; with
# SMOKE_DAP_STRICT=1 that is a failure, which is how CI runs it.  debugpy is
# looked for in build/debugpy-venv/ (python3 -m venv build/debugpy-venv;
# build/debugpy-venv/bin/pip install debugpy) as well as on PATH.

set -u

LISP=${LISP:-sbcl}
case $(basename "$LISP") in
    ecl*) quiet= ;;
    *) quiet=--noinform ;;
esac
session=heml-smoke-dap-$$
skipped=0

cd "$(dirname "$0")/.." || exit 1

. test/tmux-lib.sh

dir=$PWD/build/smoke-dap
rm -rf "$dir"
mkdir -p "$dir/c" "$dir/python" "$dir/go"
log=$dir/dap.log
PATH=$PWD/build/debugpy-venv/bin:$PATH
export PATH

printf '#include <stdio.h>\n\nstatic int twice(int x) {\n    return x * 2;\n}\n\nint main(void) {\n    int y = twice(1);\n    printf("y is %%d\\n", y);\n    return 0;\n}\n' > "$dir/c/hello.c"
printf 'def twice(x):\n    return x * 2\n\n\ny = twice(1)\nprint("y is", y)\n' > "$dir/python/hello.py"
printf 'module hello\n\ngo 1.21\n' > "$dir/go/go.mod"
printf 'package main\n\nimport "fmt"\n\nfunc twice(x int) int {\n\treturn x * 2\n}\n\nfunc main() {\n\ty := twice(1)\n\tfmt.Println("y is", y)\n}\n' > "$dir/go/main.go"
for d in c python go; do printf '()\n' > "$dir/$d/.heml-project"; done

tmux new-session -d -s "$session" -x 160 -y 40 \
     "PATH='$PATH' HEML_STATE_DIRECTORY='$dir/state' HEML_LSP_LOG='$log' $LISP $quiet \
        --eval '(asdf:load-system :heml.tty)' \
        --eval '(uiop:symbol-call :heml :heml nil :backend-type :tty :load-user-init nil)' \
        --eval '(progn (format t \"~%EDITOR-RETURNED~%\") (finish-output) (sleep 30))'"

expect "Heml CL-USER:" "the editor starts" 180

# wanted NAME PROGRAM...: whether NAME is to be checked: one of its programs
# is installed.
wanted() {
    name=$1
    shift
    for program in "$@"; do
        if command -v "$program" > /dev/null 2>&1; then
            echo "$name ($program)"
            return 0
        fi
    done
    if [ -n "${SMOKE_DAP_STRICT:-}" ]; then
        checks=$((checks + 1))
        failures=$((failures + 1))
        echo "  FAIL  $name: $1 is not installed"
    else
        skipped=$((skipped + 1))
        echo "  skip  $name: $1 is not installed"
    fi
    return 1
}

visit() {
    send C-x C-f
    sleep 0.5
    send C-a C-k
    type_text "$1"
    send Enter
}

# debug FILE LINE STOPPED STEPPED OUTPUT [PROGRAM]: a breakpoint on FILE's
# line LINE, and debugging, which stops there -- an arrow before the line
# STOPPED begins, after any indentation (as far as the language server's
# hints, which come after) -- and a step in, to the line STEPPED begins;
# STOPPED and STEPPED are basic regular expressions; x * 10
# evaluated there; and going on, which prints OUTPUT.  PROGRAM, when given,
# is what to debug, asked for.
debug() {
    file=$1
    visit "$file"
    expect "$(basename "$file")" "the program's source is visited"
    send 'M-<'
    n=$(( $2 - 1 ))
    while [ "$n" -gt 0 ]; do send C-n; n=$((n - 1)); done
    send C-c d b
    expect_re "● *$3" "C-c d b puts a breakpoint on its line"
    send C-c d d
    if [ -n "${6:-}" ]; then
        expect 'Program to debug:' "C-c d d asks what program to debug" 10
        send C-a C-k
        type_text "$6"
        send Enter
    fi
    expect_re "● ▶ *$3" "the debugger stops there, an arrow before the line" 90
    send C-c d s
    expect_re "▶ *$4" "C-c d s steps into the call" 30
    send C-c d e
    sleep 0.5
    send C-a C-k
    type_text 'x * 10'
    send Enter
    expect_re 'x \* 10 = .*10' "C-c d e evaluates an expression there" 30
    send C-c d c
    sleep 3
    send C-x b
    sleep 0.3
    send C-a C-k
    type_text 'Debug Output'
    send Enter
    expect "$5" "C-c d c lets it go on, and what it prints is in Debug Output" 30
    send C-x k
    sleep 0.3
    send Enter
    sleep 0.5
}

if wanted lldb lldb-dap xcrun; then
    if cc -g -O0 -o "$dir/c/hello" "$dir/c/hello.c"; then
        debug "$dir/c/hello.c" 8 'int y = twice(' 'return x' 'y is 2' "$dir/c/hello"
    fi
fi

if wanted debugpy debugpy-adapter; then
    debug "$dir/python/hello.py" 5 'y = twice(' 'return x' 'y is 2'
fi

if wanted delve dlv; then
    # Stepping into a Go function stops first at the line that declares it.
    debug "$dir/go/main.go" 10 'y := twice(' '\(func twice\|return x\)' 'y is 2'
fi

if [ "$failures" -gt 0 ] && [ -f "$log" ]; then
    echo "the end of $log:"
    tail -20 "$log" | cut -c1-400 | sed 's/^/        | /'
fi

echo "$checks checks, $failures failed, $skipped skipped"
[ "$failures" -eq 0 ]
