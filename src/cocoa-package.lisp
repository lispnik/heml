;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :cl-user)

(defpackage :heml.cocoa
  (:use :common-lisp)
  (:export #:main
           ;; Heml as a guest in another program's application.
           #:start-hosted
           #:hosted-running-p
           #:hosted-quit-ok-p
           #:*font-name*
           #:*font-size*
           #:*option-is-meta*
           #:*right-option-is-meta*
           #:*initial-columns*
           #:*initial-lines*
           ;; Settings an init file may change.  Those an editor variable
           ;; holds -- Cursor Style, Pixel Scrolling, Sidebar Ignored and
           ;; the like -- are set as editor variables.
           #:*blink-interval*
           #:*overscroll-limit*
           #:*scroller-shown-for*
           #:*sidebar-width*
           #:*tabs-shown*
           #:*remember-chrome*
           #:*remember-window-frame*
           #:*palette-rows*
           #:*palette-width*
           #:*popup-padding*)
  (:documentation "The native macOS backend.

AppKit owns the main thread and runs its own event loop there.  Heml
runs its command loop on a thread of its own, with the iolib event loop
the TTY backend uses, so shells and slave Lisps work unchanged.
The two meet in two places:

  - Input.  The view's -keyDown: turns an NSEvent into a plain key
    descriptor, appends it to the inbox, and writes a byte to a pipe
    whose read end is one of Heml's connections.  The connection's
    filter, on the editor thread, turns descriptors into key-events.

  - Output.  The device's redisplay methods copy Heml's dis-lines
    into the screen, a grid of rows under a lock, and ask the main
    thread to redraw.  -drawRect: paints the screen and nothing else,
    so AppKit never looks at Heml's own data structures."))
