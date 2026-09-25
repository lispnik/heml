;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :cl-user)

(defpackage :hemlock.cocoa
  (:use :common-lisp)
  (:export #:main
           #:*font-name*
           #:*font-size*
           #:*option-is-meta*
           #:*right-option-is-meta*
           #:*initial-columns*
           #:*initial-lines*)
  (:documentation "The native macOS backend.

AppKit owns the main thread and runs its own event loop there.  Hemlock
runs its command loop on a thread of its own, with the iolib event loop
the TTY backend uses, so shells and slave Lisps work unchanged.
The two meet in two places:

  - Input.  The view's -keyDown: turns an NSEvent into a plain key
    descriptor, appends it to the inbox, and writes a byte to a pipe
    whose read end is one of Hemlock's connections.  The connection's
    filter, on the editor thread, turns descriptors into key-events.

  - Output.  The device's redisplay methods copy Hemlock's dis-lines
    into the screen, a grid of rows under a lock, and ask the main
    thread to redraw.  -drawRect: paints the screen and nothing else,
    so AppKit never looks at Hemlock's own data structures."))
