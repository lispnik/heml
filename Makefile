LISP ?= sbcl --noinform --non-interactive

# Restore the dependencies ocicl.csv names into ocicl/.
deps:
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

clean:
	rm -rf build

.PHONY: deps run app clean
