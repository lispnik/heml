# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

Xoamax (also spelled "Xomax" in docs) is an Emacs-style editor written in Common Lisp. It is a 2020 fork of Hemlock (originally part of CMUCL). Nearly all code, package names, and ASDF systems still use the `hemlock` name.

## Building and running

There is no test suite. Verify changes by loading the system and running the editor.

Dependencies come from ocicl: `make deps` (`ocicl install`) restores what `ocicl.csv` lists into `ocicl/`, which is gitignored. That includes `objc` and `asdf-macos-app`. On macOS, `make run` opens the Cocoa editor from a fresh SBCL, and `make app` builds `build/Xoamax.app` through `xoamax-app.asd`.

From a REPL (the usual development loop):

```lisp
(push #p"/path/to/xoamax/" asdf:*central-registry*)
(asdf:load-system :hemlock.clx)   ; or :hemlock.tty / :hemlock.qt
(hemlock:hemlock)                 ; or (ed) on SBCL/CCL
```

Quicklisp also works: `(ql:quickload :hemlock.clx :verbose t)`.

Standalone SBCL binary: `./build.sh [tty|clx|qt ...]` builds `./hemlock`, with tty and clx by default. It runs Lisp via `$SBCL`, which defaults to `clbuild lisp`, so set `SBCL=sbcl` if you don't use clbuild. Load order matters: the last backend loaded becomes the default, so clx must come after tty. `./hemlock --help` lists the options, including `--backend tty|clx|qt` (or `--tty`, `--clx`, `--qt`). `ttyhemlock.sh`, `hemlock.qt.sh` and `dist.sh` are legacy clbuild scripts.

`c/Makefile` builds `setpty`, a small helper for pty-backed subprocesses.

Runtime requirements: iolib needs `libfixposix`. If CFFI can't find it, run `(push "/usr/local/lib/" cffi:*foreign-library-directories*)`. The CLX backend needs an X server and `$DISPLAY`. Without `$DISPLAY`, the editor picks Cocoa if `hemlock.cocoa` is loaded, and TTY otherwise.

To check a Cocoa change without watching the window, post key descriptors from a helper thread with `hemlock.cocoa::post-to-editor`, for example `(list :char #\x '("Meta"))` or `:quit`. Then have the view write itself to a PNG on the main thread: `-bitmapImageRepForCachingDisplayInRect:` followed by `-cacheDisplayInRect:toBitmapImageRep:`. `screencapture` needs Screen Recording permission, which a terminal usually lacks.

## Architecture

**ASDF systems.** `hemlock.base.asd` is the backend-independent core. Each backend is its own system that depends on it:
- `hemlock.tty`: terminfo/termcap terminal display (`tty-*.lisp`, `terminfo.lisp`, `linedit.lisp`)
- `hemlock.clx`: X11 via CLX (`bit-*.lisp`, `bitmap-*.lisp`, `hunk-draw.lisp`)
- `hemlock.qt`: experimental CommonQt backend (`qt*.lisp`, `browser.lisp`, `graphics.lisp`)
- `hemlock.cocoa`: native macOS backend through the `objc` bridge (`cocoa-*.lisp`), SBCL only

`ioconnections.lisp` (the iolib event loop and connections) is not in `hemlock.base`. Every iolib-based backend (tty, clx, cocoa) lists it among its own components.

All sources live flat in `src/`. Module membership and load order are defined only in the `.asd` files. When you add a file, register it in the right module. `core-2` is `:serial t`, so position matters there. `hemlock.base.asd` also proclaims `(optimize (safety 3) (speed 0) (debug 3))` globally.

**Layers within `hemlock.base`.**
- `core-1`/`core-2`: the text model (`line`, `htext1-4`, `buffer`, `ring`), Hemlock variables (`vars`), the command interpreter (`interp`), `syntax`, search, and the redisplay model (`window`, `winimage`, `linimage`, `screen`, `display`, `cursor`). It also holds `connections.lisp` and `rompsite.lisp`, which are the event-loop and I/O abstractions.
- `wire`: an RPC layer (`wire`, `remote`, `port`) for talking to slave Lisps and eval servers.
- `root-2`: `main.lisp` (entry points and command-line parsing), `echo`, `streams`, `font`.
- `user-1`: all user-facing commands and modes, including file commands, Lisp mode, the REPL/eval server, spell, dired, bufed, shell and so on. `bindings.lisp` holds the default keymap and is loaded last.

**Packages** (`src/package.lisp`):
- `hemlock-internals` (nickname `hi`): the core.
- `hemlock-interface`: the public extension API, re-exported through `hi`.
- `hemlock`: commands and modes.
- `hemlock-ext`: portability shims.
- `hemlock.wire` (nickname `wire`), `hemlock.terminfo`, `hemlock.x11`, `hemlock.qt`, and `hemlock-user`, where user init code runs.

When you use a new internal symbol from another package, export it from `package.lisp`.

**Backend dispatch.** `call-with-editor` in `main.lisp` picks a backend keyword (`:tty`, `:clx`, `:qt`, `:cocoa`) and dispatches through generic functions specialized with `(eql :backend)`: `backend-init-raw-io`, `%init-screen-manager`, `make-event-loop`, `dispatch-events-with-backend`, and so on. TTY, CLX and Cocoa share the iolib event loop. Qt has its own. To see what a backend must provide, grep for `(eql :clx)`.

The backend keyword is also mapped to a connection backend in two `ecase` forms: `%call-with-editor` in `main.lisp` and `%start-slave` in `eval-server.lisp`. A new backend must be added to both.

**The Cocoa backend's threads.** AppKit must own the main thread, so `invoke-with-editor-thread :cocoa` (`cocoa-main.lisp`) runs `[NSApp run]` there and runs the whole editor session on a thread named "Hemlock". The two threads never share Hemlock state:
- Input: `-keyDown:` posts plain descriptors to an inbox and writes a byte to a pipe. The pipe's read end is an ordinary iolib connection, and its filter turns the descriptors into key-events on the editor thread.
- Output: the device's redisplay methods copy dis-lines into the screen (a locked grid of rows), and `-drawRect:` paints only that.
- Anything AppKit must do for the editor thread goes through `on-main-thread`.

Window geometry follows the TTY backend (`tty-screen.lisp`): a hunk's position is its modeline row, and its text starts at `text-position - text-height + 1`.

**Extending the editor.**
- Commands are defined with `defcommand "Name" (p) ...`, which produces a function called `name-command`.
- Editor variables are defined with `defhvar`.
- Keys are bound with `bind-key "Command Name" #k"control-x"`.
- Modeline fields come from `make-modeline-field` (`window.lisp`).

Useful globals include `hi::*buffer-list*` and `hi::*window-list*`.

**Text is ASCII.** Buffer lines are base strings, so `insert-character` rejects any character that is not a `base-char`, which on SBCL means anything outside ASCII. Key-event tables also cover only 16-bit keysyms.

**Font numbers are colours.** A font number in a font-change is an ANSI colour index, or a property list such as `(:fg 7 :bg 4 :bold t)` (`*modeline-font*` in `window.lisp`). The TTY and Cocoa backends both read them that way.

**Slave Lisps.** The binary re-executes itself with `--slave` to create eval-server slaves, which it talks to over `wire`. See `eval-server.lisp`, `lispeval.lisp` and `slave-list.lisp`.

## Other directories

- `unused/`: dead code (an elisp VM experiment, old spell, gosmacs). It isn't loaded by any system.
- `doc/`: the original Hemlock manuals in Scribe (`doc/user`, `doc/cim` = Command Implementor's Manual). This is the authoritative reference for the extension API.
- `doc/xomax-internals.org`: internal notes.
- `doc/roadmap/xomax-roadmap.org`: goals and the known bug list.
