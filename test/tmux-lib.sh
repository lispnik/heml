# test/tmux-lib.sh -- what the tests that drive the TTY editor in tmux share:
# test/smoke-tty.sh and test/smoke-lsp.sh source it, having set $session.
#
# Keys go in with `tmux send-keys`, and each check reads the screen back
# with `tmux capture-pane`.  $checks and $failures count what was checked.

failures=0
checks=0

screen() { tmux capture-pane -p -t "$session"; }
send() { tmux send-keys -t "$session" "$@"; }
# A ; ending an argument separates tmux commands: escape it to type it.
type_text() {
    case $1 in
        *\;) tmux send-keys -t "$session" -l "${1%;}\\;" ;;
        *) tmux send-keys -t "$session" -l "$1" ;;
    esac
}

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
