#!/bin/sh
# test/smoke-cli.sh -- `make smoke-cli`: Xoamax.app's command, bin/xoamax,
# run from a shell as a person would run it.
#
# The bundle's executable is the SBCL runtime, which would take --help for
# itself and print its banner; the launcher must get the editor's options
# to the editor, and --tty must run the terminal editor from the app.
# APP names the bundle, build/Xoamax.app by default.  Exits 0 when every
# check passes, 1 otherwise.

set -u

cd "$(dirname "$0")/.." || exit 1
APP=${APP:-build/Xoamax.app}
xoamax=$PWD/$APP/Contents/Resources/bin/xoamax
session=xoamax-smoke-cli-$$
failures=0
checks=0

ok() { checks=$((checks + 1)); echo "  ok    $1"; }
fail() { checks=$((checks + 1)); failures=$((failures + 1)); echo "  FAIL  $1"; }

screen() { tmux capture-pane -p -t "$session"; }

expect() {
    tries=$(( ${3:-10} * 5 ))
    while [ "$tries" -gt 0 ]; do
        if screen | grep -qF -- "$1"; then
            ok "$2"
            return 0
        fi
        sleep 0.2
        tries=$((tries - 1))
    done
    fail "$2"
    echo "        (no \"$1\" on the screen:)"
    screen | sed 's/^/        | /'
    return 1
}

cleanup() { tmux kill-session -t "$session" 2>/dev/null; rm -rf "$scratch"; }
scratch=$(mktemp -d)
trap cleanup EXIT

if [ -x "$xoamax" ]; then ok "the bundle has an executable bin/xoamax"
else fail "the bundle has an executable bin/xoamax ($xoamax)"; fi

help=$("$xoamax" --help 2>&1)
if echo "$help" | grep -q -- '--tty' && ! echo "$help" | grep -q 'This is SBCL'; then
    ok "--help reaches the editor, with no SBCL banner"
else
    fail "--help reaches the editor, with no SBCL banner"
    echo "$help" | head -20 | sed 's/^/        | /'
fi

# The terminal editor, on a file named relative to the shell's directory,
# with an SBCL_HOME that would load another core if the launcher let it.
echo "cli-smoke-file-contents" > "$scratch/relative.txt"
tmux new-session -d -s "$session" -x 100 -y 30 -c "$scratch" \
     "SBCL_HOME=/nonexistent/sbcl '$xoamax' --tty relative.txt; echo CLI-EXITED=\$?; sleep 30"
expect 'cli-smoke-file-contents' "--tty edits a file named from the shell's directory" 60
expect 'relative.txt' "its name is in the modeline"
tmux send-keys -t "$session" C-x C-c
sleep 1
tmux send-keys -t "$session" n
expect 'CLI-EXITED=0' "C-x C-c leaves it, with status 0" 15

echo "$checks checks, $failures failed"
[ "$failures" -eq 0 ]
