LISP ?= sbcl --noinform --non-interactive

# Restore the dependencies ocicl.csv names into ocicl/, and vendor/conium.
deps:
	git submodule update --init
	ocicl install

# The editor in a window, from a fresh SBCL.  AppKit needs the main thread,
# so this runs from the terminal's REPL rather than from SLIME's.
run:
	sbcl --noinform --eval '(asdf:load-system :heml.cocoa)' \
	     --eval '(heml:heml nil :backend-type :cocoa)' --eval '(uiop:quit)'

# The terminal editor, in this terminal.  LISP=ecl runs it under ECL.
run-tty:
	$(or $(LISP),sbcl) --eval '(asdf:load-system :heml.tty)' \
	     --eval '(uiop:symbol-call :heml :heml nil :backend-type :tty)' \
	     --eval '(uiop:quit)'

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
# The app's command, bin/heml, from a shell.  Builds the app first.
smoke-cli: app
	test/smoke-cli.sh

smoke-tty-ecl:
	ecl --eval '(asdf:load-system :heml.tty)' --eval '(ext:quit)'
	LISP=ecl test/smoke-tty.sh

# A video of the TTY editor: build/demo/heml-tty.mp4 and .gif.  Needs vhs.
demo:
	@mkdir -p build/demo
	$(LISP) --eval '(asdf:load-system :heml.tty)'
	ecl --eval '(asdf:load-system :heml.tty)' --eval '(ext:quit)'
	vhs scripts/demo/heml-tty.tape

# A video of the Cocoa editor: build/demo/heml-cocoa.mp4.  Needs ffmpeg.
demo-cocoa:
	@mkdir -p build/demo
	$(LISP) --load scripts/demo/cocoa.lisp
	ffmpeg -v error -y -framerate 10 -i build/demo/cocoa-frames/%05d.png \
	  -c:v libx264 -pix_fmt yuv420p build/demo/heml-cocoa.mp4

# Syntax highlighting, by mode and with tree-sitter:
# build/demo/heml-tree-sitter.mp4.  Needs ffmpeg and `make tree-sitter'.
demo-tree-sitter:
	@mkdir -p build/demo
	$(LISP) --load scripts/demo/tree-sitter.lisp
	ffmpeg -v error -y -framerate 10 -i build/demo/tree-sitter-frames/%05d.png \
	  -c:v libx264 -pix_fmt yuv420p build/demo/heml-tree-sitter.mp4

# Both, after title cards: build/demo/heml.mp4.
demo-full: demo demo-cocoa
	scripts/demo/combine.sh

# --- the bundle --------------------------------------------------------------
#
#   make app                          ad hoc: runs here, cannot be notarised
#   make app SIGN_IDENTITY="Developer ID Application: You (TEAMID)"
#   make release SIGN_IDENTITY=...    notarise the app and the disk image
#
# RELEASING.md has the whole procedure.

APP       = build/Heml.app
APP_STAMP = build/.app-stamp
DIST      = dist
VERSION   = $(shell sed -n 's/.*:version "\(.*\)".*/\1/p' heml-app.asd | head -1)
ARCH      = $(shell uname -m)
DMG       = $(DIST)/Heml-$(VERSION)-$(ARCH).dmg

# The codesigning identity.  Empty means ad hoc.  It reaches heml-app.asd
# through the environment.
SIGN_IDENTITY ?=
export HEML_SIGN_IDENTITY = $(SIGN_IDENTITY)

# The notarytool keychain profile, stored once with
#   xcrun notarytool store-credentials $(NOTARY_PROFILE) \
#     --apple-id <you> --team-id <your team>
# which prompts for an app-specific password from appleid.apple.com, not
# your Apple ID password.
NOTARY_PROFILE ?= heml

app: $(APP_STAMP)

# Who signed the bundle is part of what it is, so a change of SIGN_IDENTITY
# has to rebuild it, although no source changed.  The identity is recorded in
# a file that is rewritten only when it differs, so an unchanged one does not
# force a rebuild.
SIGN_RECORD = build/.sign-identity

$(SIGN_RECORD): FORCE
	@mkdir -p build
	@printf '%s' '$(SIGN_IDENTITY)' | cmp -s - $@ 2>/dev/null \
	  || printf '%s' '$(SIGN_IDENTITY)' > $@

$(APP_STAMP): $(wildcard *.asd src/*.lisp vendor/conium/*.lisp) resources/heml.png $(SIGN_RECORD)
	$(LISP) --eval '(asdf:make "heml-app")'
	@# asdf-macos-app copies resources without their mode.  Permissions
	@# are not part of the seal, so the signature stands.
	chmod 755 "$(APP)/Contents/Resources/bin/heml"
	@touch $(APP_STAMP)
	@echo "built $(APP)$(if $(SIGN_IDENTITY), signed by $(SIGN_IDENTITY), (ad hoc))"

run-app: app
	"$(APP)/Contents/MacOS/heml"

# Tree-sitter grammars for C, Markdown and Common Lisp, built from source
# into build/tree-sitter/.  The tree-sitter library, and Python's grammar,
# come from Homebrew: brew install tree-sitter tree-sitter-python.
tree-sitter:
	scripts/tree-sitter-grammars.sh

# The grammars where the installed app finds them: $XDG_DATA_HOME/heml/.
TREE_SITTER_DIR ?= $(or $(XDG_DATA_HOME),$(HOME)/.local/share)/heml/tree-sitter
install-tree-sitter: tree-sitter
	@mkdir -p "$(TREE_SITTER_DIR)"
	cp -R build/tree-sitter/lib build/tree-sitter/share "$(TREE_SITTER_DIR)/"
	@echo "installed $(TREE_SITTER_DIR)"

# The heml command, linked onto the PATH from the installed app.
CLI_DIR ?= $(HOME)/.local/bin
install-cli:
	@mkdir -p "$(CLI_DIR)"
	ln -sf "$(HOME)/Applications/Heml.app/Contents/Resources/bin/heml" "$(CLI_DIR)/heml"
	@echo "linked $(CLI_DIR)/heml"

install-app: app
	@mkdir -p $(HOME)/Applications
	rm -rf "$(HOME)/Applications/Heml.app"
	cp -R "$(APP)" "$(HOME)/Applications/"
	@echo "installed $(HOME)/Applications/Heml.app"

# Everything the bundle loads must be inside it.  The executable is only the
# SBCL runtime; libfixposix and libosicat are opened by CFFI and live in
# Contents/Frameworks, so that is walked too, and @loader_path is accepted
# only when the file it names is there.
#
# What this defends: an SBCL built with core compression links Homebrew's
# libzstd, and such a bundle notarises perfectly well and then dies with a
# dyld error on a Mac without Homebrew.  Apple checks the signature, not
# whether your dylibs exist on someone else's disk.
define check-links
	@echo "checking what $(1) loads"
	@fail=0; \
	for bin in "$(1)/Contents/MacOS/"* "$(1)/Contents/Frameworks/"*.dylib; do \
	  [ -f "$$bin" ] || continue; \
	  self=$$(otool -D "$$bin" 2>/dev/null | tail -1); \
	  otool -L "$$bin" | tail -n +2 | awk '{print $$1}' \
	    | grep -v -F -x "$${self:-/dev/null}" | while read dep; do \
	    case "$$dep" in \
	      /usr/lib/*|/System/*) ;; \
	      @loader_path/*|@rpath/*|@executable_path/*) \
	        name=$${dep##*/}; \
	        [ -f "$(1)/Contents/Frameworks/$$name" ] || { \
	          echo "  error: $$bin needs $$dep, which is not in Frameworks" >&2; \
	          exit 1; } ;; \
	      *) echo "  error: $$bin links $$dep, outside the bundle" >&2; exit 1 ;; \
	    esac; \
	  done || fail=1; \
	done; \
	[ $$fail -eq 0 ] && echo "  ok: nothing outside /usr/lib and /System"
endef

# The link guard on its own, so CI can report it without failing and
# `make notarize' can insist on it.
check-dist: app
	$(call check-links,$(APP))

# Structure, not distributability.  The link guard is reported but does not
# fail here: a bundle built with Homebrew's SBCL is fine for testing and not
# shippable, and CI builds with Homebrew's SBCL on purpose.  `make notarize'
# is where linkage is a hard failure.
check-app: app
	@$(MAKE) --no-print-directory check-dist || \
	  echo "  note: not distributable as built; make notarize is where that is enforced"
	codesign --verify --deep --strict "$(APP)"
	plutil -lint "$(APP)/Contents/Info.plist"
	@for key in CFBundleIdentifier NSPrincipalClass CFBundleDocumentTypes; do \
	  plutil -extract "$$key" raw "$(APP)/Contents/Info.plist" >/dev/null 2>&1 \
	    || plutil -extract "$$key" xml1 -o /dev/null "$(APP)/Contents/Info.plist" \
	    || { echo "error: Info.plist has no $$key" >&2; exit 1; }; \
	done
	@echo "  ok: Info.plist has its keys"

# --- distribution ------------------------------------------------------------

# Submit the bundle to Apple's notary service and staple the ticket to it.
# Both guards stop failures that are otherwise slow: Apple refuses an ad hoc
# signature only after the upload, and a bundle that loads something from
# outside itself notarises and then fails to launch elsewhere.
notarize: app
	@codesign -dvv "$(APP)" 2>&1 | grep -q adhoc && { \
	  echo "error: $(APP) is signed ad hoc, and Apple will refuse it." >&2; \
	  echo "  build with SIGN_IDENTITY=\"Developer ID Application: You (TEAMID)\"" >&2; \
	  exit 1; } || true
	$(call check-links,$(APP))
	$(LISP) --eval '(asdf:load-system :asdf-macos-app)' \
	  --eval '(macos-app:notarize "$(APP)" :keychain-profile "$(NOTARY_PROFILE)")'
	@echo
	@spctl -a -vvv -t install "$(APP)"
	@xcrun stapler validate "$(APP)"

# A disk image: the app and a link to /Applications.
dmg: $(DMG)

$(DMG): $(APP_STAMP)
	@mkdir -p $(DIST)
	rm -f "$(DMG)"
	rm -rf "$(DIST)/stage"
	mkdir -p "$(DIST)/stage"
	cp -R "$(APP)" "$(DIST)/stage/"
	ln -s /Applications "$(DIST)/stage/Applications"
	hdiutil create -volname "Heml" -srcfolder "$(DIST)/stage" \
	  -ov -format UDZO "$(DMG)"
	rm -rf "$(DIST)/stage"
	@if [ -n "$(SIGN_IDENTITY)" ]; then \
	  echo "signing the disk image"; \
	  codesign --force --sign "$(SIGN_IDENTITY)" --timestamp "$(DMG)"; \
	else \
	  echo "note: unsigned disk image (no SIGN_IDENTITY)"; \
	fi
	@echo "built $(DMG)"

# Notarise the disk image in its own right and staple the ticket to it.
# Stapling only the app leaves the download itself unrecognised, so the
# first thing a user touches is the thing Gatekeeper complains about.
# Notarising the app first is still required: the service checks what is
# inside.
notarize-dmg: $(DMG)
	xcrun notarytool submit "$(DMG)" --keychain-profile "$(NOTARY_PROFILE)" --wait
	xcrun stapler staple "$(DMG)"
	@echo
	@spctl -a -vvv -t open --context context:primary-signature "$(DMG)"

release: notarize notarize-dmg
	@echo "$(DMG) is signed, notarised and stapled."

clean:
	rm -rf build $(DIST)

FORCE:

.PHONY: demo-tree-sitter tree-sitter install-tree-sitter run-tty install-cli smoke-cli FORCE deps run smoke smoke-tty smoke-tty-ecl demo demo-cocoa demo-full app run-app install-app \
        check-dist check-app notarize dmg notarize-dmg release clean
