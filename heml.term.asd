;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;;; A terminal emulator in a buffer: a program on a pseudo-terminal of its
;;;; own, its screen emulated by libvterm (the library behind Neovim's
;;;; terminal and Emacs's vterm) and drawn as the buffer's text.  See
;;;; src/term.lisp.
;;;;
;;;; libvterm is reached through lispnik/vterm, from vendor/vterm, a
;;;; submodule, since ocicl has no vterm.  ASDF searches the central
;;;; registry before ocicl; vterm's own dependencies come from ocicl.  The
;;;; library itself is loaded when a terminal is first made (brew install
;;;; libvterm), so an editor without it only cannot make one.

(pushnew (merge-pathnames "vendor/vterm/"
                          (make-pathname :name nil :type nil :version nil
                                         :defaults (or *load-truename* *default-pathname-defaults*)))
         asdf:*central-registry* :test #'equal)

(asdf:defsystem :heml.term
  :depends-on (:heml.base :vterm :babel)
  :pathname "src/"
  :components ((:file "term")))
