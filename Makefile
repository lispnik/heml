LISP ?= sbcl --noinform --non-interactive

# Restore the dependencies ocicl.csv names into ocicl/, and vendor/conium.
deps:
	git submodule update --init
	ocicl install

# The editor in a window, from a fresh SBCL.  AppKit needs the main thread,
# so this runs from the terminal's REPL rather than from SLIME's.
run:
	sbcl --noinform --eval '(asdf:load-system :hemlock.cocoa)' \
	     --eval '(hemlock:hemlock nil :backend-type :cocoa)' --eval '(uiop:quit)'

# build/Xoamax.app.  MACOS_SIGNING_IDENTITY names a Developer ID; unset, the
# bundle is signed ad hoc and runs only on the machine that built it.
app:
	$(LISP) --eval '(asdf:make "xoamax-app")'

# build/Xoamax-<version>.dmg around the app, with a link to /Applications.
# With a Developer ID and notarization credentials in the environment (see
# scripts/notarize.sh), the app and the image are notarized and stapled.
dmg: app
	scripts/make-dmg.sh

# The Cocoa editor driven end to end, with checks, and a picture of each
# step in build/smoke/.  It neither takes the keyboard nor touches the
# clipboard, so it can run while you work.
smoke:
	$(LISP) --load test/smoke.lisp

# The TTY backend in a real terminal: tmux, driven with keys, its screen
# checked.  Needs tmux.
smoke-tty:
	test/smoke-tty.sh

# The same under ECL.  Built first: ECL compiles through C, and a first
# build takes longer than the checks wait for the editor to start.
smoke-tty-ecl:
	ecl --eval '(asdf:load-system :hemlock.tty)' --eval '(ext:quit)'
	LISP=ecl test/smoke-tty.sh

clean:
	rm -rf build

.PHONY: deps run app dmg smoke smoke-tty smoke-tty-ecl clean
