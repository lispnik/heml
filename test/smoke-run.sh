#!/bin/sh
# test/smoke-run.sh -- `make smoke-run`: running programs and their tests.
#
# In the TTY editor, in tmux, for Rust, Go, JavaScript and TypeScript: C-c
# C-c runs the program, C-c t t the test point is in, C-c t f all the tests,
# each into the compilation buffer, and C-x ` visits the place a failing
# test or a compiler names.  The projects are copied from test/fixtures/run/
# into build/smoke-run/: each prints "twice 21 is 42" and has a test that
# passes and one that fails, at a line this script knows; rust-broken does
# not compile.
#
# A toolchain that is not installed is skipped, and said to be; with
# SMOKE_RUN_STRICT=1 that is a failure, which is how CI runs it.

set -u

LISP=${LISP:-sbcl}
case $(basename "$LISP") in
    ecl*) quiet= ;;
    *) quiet=--noinform ;;
esac
session=heml-smoke-run-$$
skipped=0

cd "$(dirname "$0")/.." || exit 1

. test/tmux-lib.sh

dir=$PWD/build/smoke-run
rm -rf "$dir"
mkdir -p "$dir"
cp -R test/fixtures/run/. "$dir/"

tmux new-session -d -s "$session" -x 160 -y 50 \
     "PATH='$PATH' HEML_STATE_DIRECTORY='$dir/state' $LISP $quiet \
        --eval '(asdf:load-system :heml.tty)' \
        --eval '(eval (read-from-string \"(setf (hi:variable-value (quote heml::language-servers) :global) nil)\"))' \
        --eval '(uiop:symbol-call :heml :heml nil :backend-type :tty :load-user-init nil)' \
        --eval '(progn (format t \"~%EDITOR-RETURNED~%\") (finish-output) (sleep 30))'"

# No language server is started, so that only what is run is on the screen.
expect "Heml CL-USER:" "the editor starts" 180

# wanted NAME PROGRAM: whether NAME is to be checked: PROGRAM is installed.
wanted() {
    if command -v "$2" > /dev/null 2>&1; then
        echo "$1 ($2)"
        return 0
    fi
    if [ -n "${SMOKE_RUN_STRICT:-}" ]; then
        checks=$((checks + 1))
        failures=$((failures + 1))
        echo "  FAIL  $1: $2 is not installed"
    else
        skipped=$((skipped + 1))
        echo "  skip  $1: $2 is not installed"
    fi
    return 1
}

visit() {
    send C-x C-f
    sleep 0.5
    send C-a C-k
    type_text "$1"
    send Enter
    expect "$(basename "$1")" "$(basename "$1") is visited"
}

# goto LINE: point to the start of line LINE.
goto() {
    send 'M-<'
    n=$(( $1 - 1 ))
    while [ "$n" -gt 0 ]; do send C-n; n=$((n - 1)); done
    sleep 0.3
}

# visited AT DESCRIPTION: C-x ` visits the next place, where a marker typed
# is followed by AT, a basic regular expression; the marker is taken out.
visited() {
    send C-x '`'
    sleep 1
    type_text '@@'
    expect_re "@@$1" "$2" 10
    send BSpace BSpace
    sleep 0.3
}

# checks NAME FILE TESTS PASSING ONE ALL AT: run FILE and see it print, then
# in TESTS, the test on line PASSING alone, which prints ONE, then all, which print
# ALL, and C-x ` visits the failing one, at AT.  ONE, ALL and AT are basic
# regular expressions.
checks() {
    visit "$2"
    send C-c C-c
    expect "twice 21 is 42" "$1: C-c C-c runs the program" 180
    visit "$3"
    goto "$4"
    send C-c t t
    expect_re "$5" "$1: C-c t t runs the test point is in, and only it" 120
    send C-c t f
    expect_re "$6" "$1: C-c t f runs the tests, and one fails" 120
    visited "$7" "$1: C-x \` visits the failing test"
}

if wanted Rust cargo; then
    checks Rust "$dir/rust/src/main.rs" "$dir/rust/src/main.rs" 14 \
           '1 passed; 0 failed' 'fails_on_purpose ... FAILED' 'assert_eq!(twice(2), 5)'
    visit "$dir/rust-broken/src/main.rs"
    send C-c C-c
    expect 'mismatched types' "Rust: a compiler error is shown" 120
    visited 'x;' "Rust: C-x \` visits the error, at its column"
fi

if wanted Go go; then
    checks Go "$dir/go/main.go" "$dir/go/main_test.go" 5 \
           '^ok *hello' '^--- FAIL: TestFailsOnPurpose' ' *t.Errorf("twice(2) = %d, not 5"'
fi

if wanted JavaScript node; then
    checks JavaScript "$dir/js/hello.js" "$dir/js/hello.test.js" 6 \
           'pass 1' 'fail 1' 'test("fails on purpose"'
    # TypeScript runs in a node that strips types: 22.18 or later.
    if node -e 'const [a, b] = process.versions.node.split(".").map(Number); process.exit(a > 22 || (a == 22 && b >= 18) ? 0 : 1)'; then
        checks TypeScript "$dir/ts/hello.ts" "$dir/ts/hello.test.ts" 6 \
               'pass 1' 'fail 1' 'test("fails on purpose"'
    else
        wanted TypeScript node-with-type-stripping
    fi
fi

echo "$checks checks, $failures failed, $skipped skipped"
[ "$failures" -eq 0 ]
