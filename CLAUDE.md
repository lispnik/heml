# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

Xoamax (also spelled "Xomax" in docs) is an Emacs-style editor written in Common Lisp. It is a 2020 fork of Hemlock (originally part of CMUCL). Nearly all code, package names, and ASDF systems still use the `hemlock` name.

It supports SBCL and ECL. Reader conditionals name only `sbcl` and `ecl`; code for other Lisps was removed. The Cocoa backend is SBCL only.

## Building and running

There is no unit test suite. `make smoke-tty` (`test/smoke-tty.sh`) runs the TTY backend in a detached tmux session, types into it with `send-keys`, and checks the screen with `capture-pane`. It then does the same for `hemlock:repl`, the `:mini` line-editing backend. `make smoke` (`test/smoke.lisp`) drives the Cocoa editor end to end and checks what it holds after each step: typing, input methods, split windows, resizing, the mouse, the clipboard, opening files, fonts, a shell, and a slave. It exits non-zero on a failure and leaves a picture of each step in `build/smoke/`. It neither activates the application nor uses the real clipboard, so it can run while someone is working. Add a check there when you add behaviour to the Cocoa backend. Neither script can run a single check. Each always runs its whole sequence. `make smoke-tty-ecl` builds under ECL and runs the TTY checks there (`LISP=ecl test/smoke-tty.sh`). `make demo` records a video of the TTY editor with `vhs` into `build/demo/`, from `scripts/demo/xoamax-tty.tape`. It covers SBCL, `hemlock:repl` and ECL. `make demo-cocoa` drives the Cocoa editor as the smoke test does (`scripts/demo/cocoa.lisp`), has the view render a frame ten times a second, and assembles the frames with `ffmpeg`. `make demo-full` makes both and joins them with title cards into `build/demo/xoamax.mp4` (`scripts/demo/combine.sh`).

Dependencies come from ocicl: `make deps` runs `git submodule update --init` and `ocicl install`, which restores what `ocicl.csv` lists into `ocicl/` (gitignored). That includes `objc` and `asdf-macos-app`. `conium` is the exception: `vendor/conium` is a submodule of lispnik/conium, branch `ecl`, whose ECL backend works on current ECL. `hemlock.base.asd` pushes it onto `asdf:*central-registry*`, which ASDF searches before ocicl. On macOS, `make run` opens the Cocoa editor from a fresh SBCL. `make app` builds `build/Xoamax.app` with lispnik/asdf-macos-app, through `xoamax-app.asd`, which is kept separate so that loading `hemlock.cocoa` never needs asdf-macos-app. The bundle is signed ad hoc, or with `SIGN_IDENTITY` (`make app SIGN_IDENTITY="Developer ID Application: …"`), which reaches the `.asd` as `XOAMAX_SIGN_IDENTITY`. A stamp file tracks freshness, and a record of the identity makes a change of identity rebuild. `make check-app` verifies the signature and `Info.plist`, and reports whether anything is linked from outside the bundle. `make dmg` makes `dist/Xoamax-<version>-<arch>.dmg`. `make notarize`, `make notarize-dmg` and `make release` notarise through `macos-app:notarize` with the notarytool keychain profile `NOTARY_PROFILE` (default `xoamax`); `notarize` refuses an ad hoc bundle or one that links outside itself. `RELEASING.md` has the whole procedure. `make clean` removes `build/` and `dist/`.

AppKit needs the process's main thread. To run Cocoa from a REPL, use `(asdf:load-system :hemlock.cocoa)` and then `(hemlock:hemlock nil :backend-type :cocoa)`, in a terminal SBCL's REPL. A SLIME or Sly REPL thread won't work.

From a REPL (the usual development loop):

```lisp
(push #p"/path/to/xoamax/" asdf:*central-registry*)
(asdf:load-system :hemlock.tty)
(hemlock:hemlock)                 ; or (ed)
```

Quicklisp also works: `(ql:quickload :hemlock.tty :verbose t)`.

Standalone SBCL binary: `./build.sh` builds `./hemlock` with the TTY backend. It runs Lisp via `$SBCL`, which defaults to `clbuild lisp`, so set `SBCL=sbcl` if you don't use clbuild. `./hemlock --help` lists the options, including `--backend tty|cocoa` (or `--tty`, `--cocoa`). `ttyhemlock.sh` and `dist.sh` are legacy clbuild scripts.

`c/Makefile` builds `setpty`, a small helper for pty-backed subprocesses.

Runtime requirements: iolib needs `libfixposix` (`brew install libfixposix` on macOS). If CFFI can't find it, run `(push "/usr/local/lib/" cffi:*foreign-library-directories*)`. The editor picks Cocoa if `hemlock.cocoa` is loaded, and TTY otherwise (`choose-backend-type` in `rompsite.lisp`).

ECL notes:
- ECL reads every `--eval` on its command line before it evaluates any, so a later `--eval` can't name a Hemlock symbol. Use `uiop:symbol-call`, as `test/smoke-tty.sh` and the ECL slave command in `eval-server.lisp` do.
- ECL compiles through C, so a first build takes a minute or two.
- `hemlock.base.asd` works around two ECL problems. It gives ECL on macOS the `:bsd` feature that SBCL has, which osicat and the TTY backend expect; ASDF doesn't recompile when features change, so clear ECL's fasl cache if osicat was built without it. It also defines `ecl_to_cl_index` as `ecl_to_index` for the C compiler, working around an ECL 26.5.5 code-generation bug.
- CFFI on ECL passes variadic arguments as fixed ones, which breaks on arm64 macOS. `terminal-size` (`tty-disp-rt.lisp`) therefore calls `ioctl` from C through `ffi:c-inline`. Do the same for any new variadic C call on ECL.

`test/smoke.lisp` shows how to drive the editor without watching it. It posts descriptors with `hemlock.cocoa::post-to-editor`, such as `(list :char #\x '("Meta"))` or `:quit`. It sends real NSEvents to the window, and has the view render itself to a PNG. `screencapture` needs Screen Recording permission, which a terminal usually lacks. A synthesized key-down cannot enter a dead-key state, so dead keys need a real keyboard.

## Architecture

**ASDF systems.** `hemlock.base.asd` is the backend-independent core. Each backend is its own system that depends on it:
- `hemlock.tty`: terminfo/termcap terminal display (`tty-*.lisp`, `terminfo.lisp`, `linedit.lisp`)
- `hemlock.cocoa`: native macOS backend through the `objc` bridge (`cocoa-*.lisp`), SBCL only

`ioconnections.lisp` (the iolib event loop and connections) is the last module of `hemlock.base`. On SBCL it compiles at `(speed 2)`, from its own `declaim`.

All sources live flat in `src/`. Module membership and load order are defined only in the `.asd` files. When you add a file, register it in the right module. `core-2` is `:serial t`, so position matters there.

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
- `hemlock.wire` (nickname `wire`), `hemlock.terminfo`, and `hemlock-user`, where user init code runs.

When you use a new internal symbol from another package, export it from `package.lisp`.

**Backend dispatch.** `call-with-editor` in `main.lisp` picks a backend keyword (`:tty`, `:cocoa`) and dispatches through generic functions specialized with `(eql :backend)`: `backend-init-raw-io`, `%init-screen-manager`, `make-event-loop`, `dispatch-events-with-backend`, and so on. Both share the iolib event loop. To see what a backend must provide, grep for `(eql :cocoa)`.

**Redisplay is never incremental.** Each redisplay rebuilds every visible window's image from its display start (`update-window-image` in `winimage.lisp` fills the dis-lines through `compute-line-image`). The device then draws all of it through its single `device-redisplay` method. Nothing records what changed, so nothing can be left stale. `compute-line-image` calls `line-tag`, which keeps syntax highlighting current on every line shown. The TTY device writes every row and clears each one to the end of the line, with no clear beforehand, so it doesn't flicker. Its output goes to the terminal's fd synchronously (`write-and-maybe-wait`), because redisplay often runs inside an event handler (for example, a shell's output) where dispatching events again isn't safe. Redisplay entry points return NIL when they have drawn: the input loop redisplays again while one returns true.

Each pass is bracketed by `device-begin-redisplay` and `device-end-redisplay` (`with-device-redisplay` in `display.lisp`). On the TTY these wrap the frame in synchronized output (DEC mode 2026, turned off by `*tty-synchronized-output*`) and hide the cursor while drawing. Where `civis` is DECTCEM, the cursor is shown again with DECTCEM alone, because `cnorm` can do more, such as xterm's turning off a blinking cursor. Redisplays caused by events are at most `*redisplay-interval*` (1/60 s) apart. The input loop waits out the rest of that interval for more events before it draws again, and ordinary stream output (`redisplay-windows-from-mark` with THROTTLEP) skips drawing within it. An explicit `finish-output` or `force-output` still draws at once.

The backend keyword is also mapped to a connection backend in two `ecase` forms: `%call-with-editor` in `main.lisp` and `%start-slave` in `eval-server.lisp`. A new backend must be added to both.

**The Cocoa backend's threads.** AppKit must own the main thread, so `invoke-with-editor-thread :cocoa` (`cocoa-main.lisp`) runs `[NSApp run]` there and runs the whole editor session on a thread named "Hemlock". The two threads never share Hemlock state:
- Input: `-keyDown:` posts plain descriptors to an inbox and writes a byte to a pipe. The pipe's read end is an ordinary iolib connection, and its filter turns the descriptors into key-events on the editor thread.
- Output: the device's redisplay methods copy dis-lines into the screen (a locked grid of rows), and `-drawRect:` paints only that.
- Typing: named keys, and keys with Control or Meta, are posted directly from `-keyDown:`. Everything else goes through `-interpretKeyEvents:` and the view's `NSTextInputClient` methods, so dead keys and input methods work. Only the left Option key is Meta by default.
- The clipboard: `*interprogram-cut-function*` and `*interprogram-paste-function*` (`killcoms.lisp`) join the kill ring to the pasteboard. Kills and "Save Region" call the first; "Un-Kill" calls the second, and uses the pasteboard only if its change count shows another application wrote to it. The Edit menu posts Super keys, which `install-mac-bindings` binds.
- Menus: `*menu-bar*` and `*context-menu*` are tables. Each item's action is a Hemlock command, a function to run on the main thread, or an AppKit selector. The item's tag indexes `*menu-actions*`. A command is posted as `(:command name arg ...)`: the editor queues it in `*menu-commands*` and then the `Menucommand` key, and the "Menu Command" command runs it. So a menu item runs inside the command loop, with prompts, undo and errors behaving as they do for a typed key.
- Files from Finder arrive at the app delegate's `application:openURLs:` and are visited through `process-command-line-argument`.
- Fonts: fonts belong to the main thread. `change-font` re-measures the cell and posts a `:resize`, and it saves the choice in `NSUserDefaults`. The View menu's items target the app delegate, not the responder chain, so their shortcuts work whatever has focus.
- Anything AppKit must do for the editor thread goes through `on-main-thread`.

**Window layout** is shared by every device (`layout.lisp`). The windows other than the echo area tile a rectangle, and `device-layout` holds that as a tree: each leaf is a hunk, and each split divides its space into rows or side-by-side columns, with a size for each child. Side-by-side windows are divided by a column that neither owns, and each device draws a bar there. The shared `device-make-window`, `device-delete-window` and `device-enlarge-window` methods change the tree. `apply-layout` then sets every hunk's geometry, resizes each window's image to fit, and rebuilds the ring of hunks in tree order, which is the order `next-window` follows. `resize-device-layout` handles a new screen size. A device only supplies `device-make-hunk` and draws each hunk where the hunk says: `device-hunk-column` and `device-hunk-width` give its columns, its `position` is its modeline row, and its text starts at `text-position - text-height + 1`. `C-x 3` is "Split Window Horizontally", `C-x {` and `C-x }` shrink and enlarge a window horizontally, and `C-x +` is "Balance Windows" (`balance-layout`), which sizes each split's children by how many windows lie across each one.

**The mouse.** Hemlock represents the mouse as key-events with mouse keysyms: `Leftdown`, `Leftup`, `Leftdrag`, `Scrollup`, `Scrolldown` and the others in `keysym-defs.lisp`. A backend queues each one with `q-event`, passing the X and Y within the window's text (Y is NIL on a modeline) and the hunk. Pointer commands read them back with `last-key-event-cursorpos`. The Cocoa view posts grid cells, and `locate-cell` on the editor thread turns them into those coordinates. On its first entry, Cocoa rebinds the left button to the "Mouse ..." commands in `morecoms.lisp`, and gives the active region the `:selection` background.

**Extending the editor.**
- Commands are defined with `defcommand "Name" (p) ...`, which produces a function called `name-command`.
- Editor variables are defined with `defhvar`.
- Keys are bound with `bind-key "Command Name" #k"control-x"`.
- Modeline fields come from `make-modeline-field` (`window.lisp`).

Useful globals include `hi::*buffer-list*` and `hi::*window-list*`.

**Characters.** Buffer lines are full `character` strings, and files are read and written as UTF-8. Hemlock shadows `char-code-limit` as 256 in `hemlock-ext` and `hi`, so a table indexed by character code covers only the first 256 codes. Past that, character sets fall back to a hash table (`char-set-ref` in `charmacs.lisp`), and so do `char-key-event` and the Boyer-Moore jump tables, which use the low byte. Don't declare a `base-char`: on SBCL that means ASCII.

A key for a character past ASCII comes from `hemlock-ext:character-key-event`, which makes the key-event on first use. Its keysym is the code point for Latin-1, and `#x01000000` plus the code point past that, as in X11, so it can't collide with the special keys at `#xFF00`–`#xFFFF`. `*new-character-key-event-hook*` then binds it to Self Insert (`bindings.lisp`). Keysyms past 16 bits live in `*large-keysym-key-events*`. A wide character (East Asian Width W or F: CJK, fullwidth forms, most emoji) is displayed as itself followed by `wide-character-filler` (`linimage.lisp`). This goes through the same print-representation mechanism that shows a control character as `^X`, so everything that counts columns counts two. The character sets fill those entries lazily through `character-set-default-function` (`charmacs.lisp`). A device draws the character across both cells and nothing for the filler: TTY's `device-write-string` skips it, and so does Cocoa's `draw-text`.

**Font numbers are colours.** A font number in a font-change is an ANSI colour index, or a property list such as `(:fg 7 :bg 4 :bold t)` (`*modeline-font*` in `window.lisp`). The TTY and Cocoa backends both read them that way.

**Slave Lisps.** The binary re-executes itself with `--slave` to create eval-server slaves, which it talks to over `wire`. See `eval-server.lisp`, `lispeval.lisp` and `slave-list.lisp`.

**Terminals.** `hemlock.terminfo:tparm` evaluates terminfo string expressions, including the `%? %t %e %;` conditionals that every 256-colour terminal's `setaf` uses. Its binary operators pop their second operand first. The terminal's erase character (`*tty-erase-char*`, usually `^?`) is Backspace, whatever terminfo's `kbs` says. `setup-input` (`tty-disp-rt.lisp`) leaves `IEXTEN` on, so it disables VLNEXT, VDISCARD and VSTATUS itself. Otherwise the driver would swallow `C-v`, `C-o` and `C-t`. `linedit-device`, the `:mini` backend in `linedit.lisp`, has its own `device-init`, which starts without the alternate screen; the full-screen TTY editor uses the alternate screen. Shell buffers turn SGR colour sequences into font marks and drop other escape sequences (`write-shell-output` in `shell.lisp`).

**CI.** `.github/workflows/ci.yml` runs on a macOS runner, which has a window server. It checks out the submodule, sets up ocicl for SBCL and ECL, runs `make smoke`, `make smoke-tty` and `make smoke-tty-ecl`, builds and checks the app ad hoc, makes its disk image, and starts and quits the app. It uses Homebrew's SBCL, which links libzstd, so that bundle is for testing only. `.github/workflows/release.yml` runs on a tag `v*`, or by hand as a dry run. It builds SBCL `--without-sb-core-compression` (cached), then on arm64 and Intel runners signs with the Developer ID from the secrets, notarises and staples the app and the disk image, and publishes a release with both images.

## Other directories

- `unused/`: dead code (an elisp VM experiment, old spell, gosmacs). It isn't loaded by any system.
- `doc/`: the original Hemlock manuals in Scribe (`doc/user`, `doc/cim` = Command Implementor's Manual). This is the authoritative reference for the extension API.
- `doc/xomax-internals.org`: internal notes.
- `doc/roadmap/xomax-roadmap.org`: goals and the known bug list.
