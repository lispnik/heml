;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;;; test/smoke.lisp -- `make smoke`: the Cocoa editor, driven end to end.
;;;;
;;;; A helper thread drives the editor the way AppKit would -- real NSEvents
;;;; sent to the window, the menus' key equivalents, the application
;;;; delegate's messages -- and checks the editor's state after each step:
;;;; the text of the buffer, the region, the windows, the pasteboard.  Each
;;;; step also leaves a picture in build/smoke/, since a check says what the
;;;; editor holds and only a picture says what it drew.
;;;;
;;;; The run does not take the keyboard (HEML.COCOA:*ACTIVATE* is off),
;;;; uses a private pasteboard rather than the clipboard, and does not save
;;;; its font sizes, so it can run while someone works.  It exits 0 when
;;;; every check passes, 1 when one fails, and 2 when the watchdog finds it
;;;; hung.

(asdf:load-system :heml.cocoa)

(defpackage :heml-smoke (:use :common-lisp))
(in-package :heml-smoke)

(defvar *out*
  (merge-pathnames "build/smoke/" (asdf:system-source-directory :heml.cocoa)))

(defvar *failures* '())
(defvar *checks* 0)
(defvar *finished* nil)

;;; Projects' sessions are kept here, not in ~/.heml, and start empty.
(let ((state (merge-pathnames "state/" *out*)))
  (uiop:delete-directory-tree state :validate t :if-does-not-exist :ignore)
  (setf heml::*project-state-directory* state))

(setf heml.cocoa::*activate* nil
      heml.cocoa::*pasteboard-name* "org.lispnik.heml.smoke"
      heml.cocoa::*remember-font* nil
      heml.cocoa:*font-size* 13
      heml.cocoa:*font-name* nil)

(defun note (format &rest arguments)
  (format t "~&~?~%" format arguments)
  (finish-output))

(defmacro check (name form)
  `(let ((result (ignore-errors ,form)))
     (incf *checks*)
     (cond (result (note "  ok    ~A" ,name))
           (t (push ,name *failures*)
              (note "  FAIL  ~A" ,name)))))


;;;; Driving the editor

(defmacro main (&body body)
  "BODY on the main thread, waiting for it."
  `(heml.cocoa::call-on-main-thread-and-wait (lambda () ,@body)))

(defun settle ()
  "Wait until the editor has taken everything posted and redisplayed."
  (loop repeat 200
        until (heml.cocoa::inbox-empty-p)
        do (sleep 0.05))
  (sleep 0.4))

(defun post (descriptor) (heml.cocoa::post-to-editor descriptor))

(defun post-text (string)
  (loop for c across string
        do (post (if (char= c #\Newline)
                     (list :named "Return" '())
                     (list :char c '())))))

(defun post-key (character &rest modifiers)
  (post (list :char character modifiers)))

(defun extended-command (name)
  (post-key #\x "Meta")
  (post-text name)
  (post-text (string #\Newline)))

(defun display () heml.cocoa::*display*)
(defun window () (heml.cocoa::display-window (display)))
(defun view () (heml.cocoa::display-view (display)))

(defparameter *key-codes*
  '((#\a . 0) (#\s . 1) (#\d . 2) (#\f . 3) (#\h . 4) (#\g . 5) (#\z . 6)
    (#\x . 7) (#\c . 8) (#\v . 9) (#\b . 11) (#\q . 12) (#\w . 13) (#\e . 14)
    (#\r . 15) (#\y . 16) (#\t . 17) (#\= . 24) (#\- . 27) (#\0 . 29)
    (#\o . 31) (#\u . 32) (#\i . 34) (#\p . 35) (#\l . 37) (#\j . 38)
    (#\k . 40) (#\n . 45) (#\m . 46) (#\Space . 49)))

(defconstant +option+ (ash 1 19))
(defconstant +command+ (ash 1 20))
(defconstant +left-option+ (logior +option+ #x20))
(defconstant +shift+ (ash 1 17))

(defun key-event (characters unmodified flags)
  (objc:invoke "NSEvent"
               "keyEventWithType:location:modifierFlags:timestamp:windowNumber:context:characters:charactersIgnoringModifiers:isARepeat:keyCode:"
               10 (vector 0d0 0d0) flags 0d0 (objc:invoke (window) "windowNumber") nil
               characters unmodified nil
               (or (cdr (assoc (char-downcase (char unmodified 0)) *key-codes*)) 0)))

(defun press (characters &key (unmodified characters) (flags 0))
  "A key-down NSEvent, sent to the window."
  (main (objc:invoke (window) "sendEvent:" (key-event characters unmodified flags))))

(defun press-menu (character)
  "Cmd-CHARACTER, through the menu bar's key equivalents."
  (main (objc:invoke (objc:invoke (objc.runloop:shared-application) "mainMenu")
                     "performKeyEquivalent:"
                     (key-event (string character) (string character) +command+))))

(defun mouse-event (type column line flags clicks)
  (let* ((display (display))
           (point (vector (float (+ heml.cocoa::*margin*
                                    (* (+ column 1/2) (heml.cocoa::display-char-width display)))
                                 1d0)
                          (float (+ heml.cocoa::*margin*
                                    (* (+ line 1/2) (heml.cocoa::display-char-height display)))
                                 1d0)))
           )
    (objc:invoke "NSEvent"
                 "mouseEventWithType:location:modifierFlags:timestamp:windowNumber:context:eventNumber:clickCount:pressure:"
                 (ecase type (:down 1) (:up 2) (:drag 6) (:right-down 3))
                 (objc:invoke (view) "convertPoint:toView:" point nil)
                 flags 0d0 (objc:invoke (window) "windowNumber") nil 0 clicks 1.0)))

(defun mouse (type column line &key (flags 0) (clicks 1))
  "A mouse NSEvent at the middle of cell COLUMN, LINE, sent to the window."
  (main (objc:invoke (window) "sendEvent:" (mouse-event type column line flags clicks))))

(defun choose-menu-item (menu title)
  "Choose the item TITLE of the menu bar's menu MENU, as a click on it would."
  (main (let* ((submenu (objc:invoke (objc:invoke (objc:invoke (objc.runloop:shared-application)
                                                                "mainMenu")
                                                   "itemWithTitle:" menu)
                                      "submenu"))
               (index (objc:invoke submenu "indexOfItemWithTitle:" title)))
          (assert (>= index 0) () "No item ~S in the ~A menu." title menu)
          (objc:invoke submenu "performActionForItemAtIndex:" index))))

(defvar *shot* 0)

(defun shot (name)
  (let ((path (namestring (merge-pathnames (format nil "~2,'0D-~A.png" (incf *shot*) name)
                                           *out*))))
    (main
      (let* ((bounds (objc:invoke (view) "bounds"))
             (rep (objc:invoke (view) "bitmapImageRepForCachingDisplayInRect:" bounds)))
        (objc:invoke (view) "cacheDisplayInRect:toBitmapImageRep:" bounds rep)
        (objc:invoke (objc:invoke rep "representationUsingType:properties:" 4
                                  (objc:invoke "NSDictionary" "dictionary"))
                     "writeToFile:atomically:" path t)))))

(defun wait-until (predicate &optional (seconds 20))
  (loop repeat (* seconds 10)
        when (ignore-errors (funcall predicate)) return t
        do (sleep 0.1)))


;;;; Reading the editor's state, after SETTLE, while it waits for input

;;; This thread only reads.  The editor keeps the line being edited in a
;;; cache, and LINE-STRING and REGION-TO-STRING put the cache back in the
;;; line first: done from here while the editor's thread is typing into the
;;; line, that corrupts it.  So a line's text is read where it is, cache or
;;; line, and a read the editor's thread overtakes is at worst wrong once.

(defun line-text (line)
  (if (eq line hi::open-line)
      (let ((chars hi::open-chars)
            (left hi::left-open-pos)
            (right hi::right-open-pos)
            (length hi::line-cache-length))
        (concatenate 'string (subseq chars 0 (min left (length chars)))
                     (subseq chars (min right (length chars)) (min length (length chars)))))
      (hi::line-chars line)))

(defun buffer-text (&optional (buffer (hi::current-buffer)))
  (with-output-to-string (out)
    (loop for line = (hi::mark-line (hi::buffer-start-mark buffer)) then next
          for next = (hi::line-next line)
          do (write-string (line-text line) out)
             (when next (terpri out))
          while next)))

(defun region-text () (hi::region-to-string (heml::current-region nil nil)))

(defun pasteboard-text ()
  (main (let ((string (objc:invoke (heml.cocoa::general-pasteboard)
                                   "stringForType:" "public.utf8-plain-text")))
          (and (not (cffi:null-pointer-p string)) (objc:ns-string-to-string string t)))))

(defun point-column () (hi::mark-column (hi::current-point)))

(defun menu-hidden-p (title)
  (main (objc:invoke-bool (objc:invoke (objc:invoke (objc.runloop:shared-application) "mainMenu")
                                       "itemWithTitle:" title)
                          "isHidden")))

(defun row-runs-containing (text)
  "The font runs of the first screen row that shows TEXT, or :NONE."
  (let ((row (find-if (lambda (row) (search text (heml.cocoa::row-text row)))
                      (heml.cocoa::screen-rows heml.cocoa::*screen*))))
    (if row (heml.cocoa::row-runs row) :none)))

(defun run-font-at (text offset)
  "The font drawn at OFFSET characters into TEXT, where TEXT is on screen."
  (let ((row (find-if (lambda (row) (search text (heml.cocoa::row-text row)))
                      (heml.cocoa::screen-rows heml.cocoa::*screen*))))
    (when row
      (let ((column (+ offset (search text (heml.cocoa::row-text row)))))
        (loop for (start end . font) in (heml.cocoa::row-runs row)
              when (and (<= start column) (< column end)) return font)))))


;;; RUN is one function, and as much as the compiler can take: these are
;;; parts of it, called where they come.

(defvar *queued-ran* nil)

(defvar *sync-result* nil)

;;; Random edits to a buffer, and after each a check that the change a
;;; server is told of, made to the text it had, is the buffer's text.  On
;;; the editor's thread, which a buffer belongs to.
(hi::defcommand "Smoke Sync Check" (p) "" ""
  (declare (ignore p))
  (let* ((buffer (hi::make-buffer "smoke sync check"))
         (state (sb-ext:seed-random-state 7))
         (pieces (vector "a" "bc" (string #\Newline) (format nil "x~%y") (string (code-char #x1F600)) ""))
         (document (heml::make-document 1 0))
         (heml::*encoding* :utf-16)
         (failures 0) (changes 0))
    (flet ((apply-change (old sl sc el ec text)
             (flet ((index (line character)
                      (let ((i 0) (units 0))
                        (dotimes (k line) (setf i (1+ (position #\Newline old :start i))))
                        (loop while (< units character)
                              do (incf units (if (> (char-code (char old i)) #xFFFF) 2 1))
                                 (incf i))
                        i)))
               (concatenate 'string (subseq old 0 (index sl sc)) text (subseq old (index el ec))))))
      (hi::insert-string (hi::buffer-point buffer) (format nil "one~%two~%three"))
      (multiple-value-bind (lines strings) (heml::buffer-snapshot buffer)
        (setf (heml::document-lines document) lines (heml::document-strings document) strings))
      (let ((old (hi::region-to-string (hi::buffer-region buffer))))
        (dotimes (round 400)
          (let* ((text (hi::region-to-string (hi::buffer-region buffer)))
                 (where (random (1+ (length text)) state)))
            (hi::with-mark ((m (hi::buffer-start-mark buffer) :left-inserting))
              (hi::character-offset m where)
              (if (and (plusp (length text)) (< (random 10 state) 4))
                  (hi::with-mark ((e m))
                    (hi::character-offset e (min (- (length text) where) (random 6 state)))
                    (hi::delete-region (hi::region m e)))
                  (hi::insert-string m (aref pieces (random (length pieces) state))))))
          (let ((new (hi::region-to-string (hi::buffer-region buffer))))
            (multiple-value-bind (sl sc el ec text) (heml::line-change document buffer)
              (if text
                  (progn (incf changes)
                         (unless (string= new (apply-change old sl sc el ec text)) (incf failures)))
                  (unless (string= old new) (incf failures))))
            (setf old new)))))
    (hi::delete-buffer buffer)
    (setf *sync-result* (list changes failures))))

(hi::defcommand "Smoke Queue" (p)
  "Queue something for the command loop to do." ""
  (declare (ignore p))
  (heml::queue-command (lambda () (setf *queued-ran* t))))

(defun language-server-feature-checks ()
  (extended-command "Smoke Sync Check")
  (check "a server is told of a change as the lines that changed, and they make the buffer's text"
         (wait-until (lambda () (and *sync-result* (plusp (first *sync-result*))
                                     (zerop (second *sync-result*))))
                     30))
  (extended-command "Smoke Queue")
  (check "what is queued for the command loop is done by it"
         (wait-until (lambda () *queued-ran*) 5))
  ;; The rest of what a server gives, in a file of its own.
  (with-open-file (out (merge-pathnames "more.pas" *out*) :direction :output :if-exists :supersede)
    (write-line "program more;" out)
    (write-line "  wrongthing here" out)
    (write-line "begin end." out))
  (ignore-errors (delete-file (merge-pathnames "more.pas.made" *out*)))
  (post (list :open (namestring (merge-pathnames "more.pas" *out*))))
  (settle)
  (flet ((point-line () (line-text (hi::mark-line (hi::current-point))))
         (row-p (text)
           (find text (map 'list #'heml.cocoa::row-text
                           (heml.cocoa::screen-rows heml.cocoa::*screen*))
                 :test #'search))
         (starts-p (text) (eql 0 (search text (buffer-text)))))
    (check "the modeline says what the server says it is doing, and how far it is"
           (wait-until (lambda () (row-p "(Indexing 50%)")) 30))
    (check "what a server says to the user is kept in a buffer"
           (wait-until (lambda ()
                         (let ((buffer (hi::getstring "Language Servers" hi::*buffer-names*)))
                           (and buffer (search "fake says hello" (buffer-text buffer)))))
                       10))
    (check "the other uses of the name at point are shown"
           (wait-until (lambda ()
                         (let ((font (run-font-at "program more" 8)))
                           (and (consp font) (getf font :bold) (getf font :underline))))
                       10))
    (check "the server's own reading of the text colours it"
           (wait-until (lambda () (eql 6 (run-font-at "begin end." 6))) 10))
    (extended-command "LSP Inlay Hints")
    (check "LSP Inlay Hints shows what the server infers, where it would be written"
           (wait-until (lambda () (row-p "argument: program more: hinted;")) 10))
    (check "in a font of its own, the text's own colours after it"
           (let ((hint (run-font-at "argument: program" 0))
                 (text (run-font-at "argument: program" 10)))
             (and (consp hint) (getf hint :italic) (not (equal hint text)))))
    ;; The cursor, and a click, are where the characters are drawn.
    (post-key #\< "Meta")
    (settle)
    (check "the cursor at a hint's place is before the hint"
           (eql 0 (heml.cocoa::screen-cursor-x heml.cocoa::*screen*)))
    (post-key #\f "Control")
    (settle)
    (check "and after the next character, past both"
           (eql 11 (heml.cocoa::screen-cursor-x heml.cocoa::*screen*)))
    (post-key #\e "Control")
    (settle)
    (check "and at the end of the line, at the end of its text"
           (eql (length "argument: program more: hinted;")
                (heml.cocoa::screen-cursor-x heml.cocoa::*screen*)))
    ;; The line being typed in is kept apart from the others, and is drawn
    ;; and measured its own way.
    (post-key #\< "Meta")
    (post-text "Z")
    (settle)
    (check "a line with hints is drawn right as it is typed in"
           (and (row-p "argument: Zprogram more: hinted;")
                (eql 11 (heml.cocoa::screen-cursor-x heml.cocoa::*screen*))))
    (post (list :named "Backspace" '()))
    (settle)
    ;; What a line with text among its characters shows, and where each
    ;; character is in that: a tab is as wide as where it comes makes it.
    (check "text among a line's characters moves what follows, tabs and wrapping too"
           (let ((line (hi::make-line :chars (coerce (format nil "abc~Cdef" #\Tab) 'simple-string)))
                 (inlines '((1 "XX" 8) (3 "Y" 8))))
             (multiple-value-bind (flat map)
                 (hi::flatten-inline-line line inlines 10000 0 7 t)
               (and (string= flat "aXXbcY  def")
                    (equalp map #(0 1 4 5 8 9 10 11))
                    ;; In rows five wide the tab comes at the second column
                    ;; of its row, and is seven spaces: sixteen columns.
                    (equal '(1 3) (multiple-value-list
                                   (hi::inline-line-length line inlines 5 0 7)))))))
    (let* ((rows (heml.cocoa::screen-shown-rows heml.cocoa::*screen*))
           (row (position-if (lambda (row) (search "argument: program" (heml.cocoa::row-text row)))
                             rows))
           (column (and row (search "argument: program" (heml.cocoa::row-text (svref rows row))))))
      (when row
        (mouse :down (+ column 12) row)
        (mouse :up (+ column 12) row)
        (settle))
      (check "a click on a character after a hint is on that character"
             (and row (eql 2 (hi::mark-charpos (hi::current-point)))))
      (when row
        (mouse :down (+ column 3) row)
        (mouse :up (+ column 3) row)
        (settle))
      (check "and a click on a hint is where the hint is"
             (and row (eql 0 (hi::mark-charpos (hi::current-point))))))
    (post-key #\< "Meta")
    (extended-command "LSP Code Lenses")
    (check "and LSP Code Lenses what it offers to do there, after the line's end"
           (wait-until (lambda () (row-p "hinted;  [Run the fake lens]")) 10))
    (extended-command "LSP Inlay Hints")
    (extended-command "LSP Code Lenses")
    (check "and each is taken away again"
           (wait-until (lambda () (not (row-p "hinted"))) 10))
    (post-key #\c "Control") (post-key #\f "Control")
    (check "C-c C-f folds what the server says can be folded, under its first line"
           (wait-until (lambda ()
                         (and (row-p "program more;  ... 1 line")
                              (not (row-p "wrongthing here"))))
                       10))
    (post-key #\n "Control")
    (settle)
    (check "C-n goes past a fold"
           (equal "begin end." (point-line)))
    (post-key #\p "Control")
    (post-key #\c "Control") (post-key #\f "Control")
    (check "and C-c C-f on its first line opens it"
           (wait-until (lambda () (row-p "wrongthing here")) 10))
    (post-key #\> "Meta")
    (post-key #\c "Control") (post-key #\t "Control")
    (check "C-c C-t goes to the definition of a type"
           (wait-until (lambda () (equal "program more;" (point-line))) 10))
    (extended-command "LSP Find Implementation")
    (check "LSP Find Implementation lists what implements a thing"
           (wait-until (lambda ()
                         (and (equal "*Implementations*" (hi::buffer-name (hi::current-buffer)))
                              (search "Implementations: 2" (buffer-text))))
                       10))
    (post-key #\x "Control") (post-key #\1)
    (post (list :open (namestring (merge-pathnames "more.pas" *out*))))
    (settle)
    (post-key #\c "Control") (post-key #\u "Control")
    (check "C-c C-u lists what calls a function"
           (wait-until (lambda ()
                         (and (equal "*Calls*" (hi::buffer-name (hi::current-buffer)))
                              (search "function fake_caller" (buffer-text))))
                       10))
    (post-key #\x "Control") (post-key #\1)
    (post (list :open (namestring (merge-pathnames "more.pas" *out*))))
    (settle)
    (extended-command "LSP Outgoing Calls")
    (check "and LSP Outgoing Calls what it calls"
           (wait-until (lambda ()
                         (and (equal "*Calls*" (hi::buffer-name (hi::current-buffer)))
                              (search "function fake_callee" (buffer-text))))
                       10))
    (post-key #\x "Control") (post-key #\1)
    (post (list :open (namestring (merge-pathnames "more.pas" *out*))))
    (settle)
    ;; A completion that is a snippet, and brings a line with it.
    (post-key #\> "Meta")
    (post-text "fake_")
    (post-key #\i "Control" "Meta")
    (wait-until (lambda () (row-p " fake_snippet ")) 10)
    (post-key #\h "Control")
    (check "C-h in the popup says what the server says of a completion"
           (wait-until (lambda () (row-p " fake documentation of fake_function")) 10))
    (post (list :named "Escape" '()))
    (settle)
    (post-text "s")
    (post (list :named "Return" '()))
    (check "a completion that is a snippet is put in with its places"
           (wait-until (lambda () (search "fake_snippet(first, second)" (buffer-text))) 10))
    (check "and what else the server says it needs is put in too"
           (starts-p "{ imported }"))
    (check "a variable in a snippet is what it stands for: here, the file's name"
           (search "{ first } more" (buffer-text)))
    (post-text "x")
    (post (list :named "Tab" '()))
    (post-text "y")
    (settle)
    (check "typing at a place replaces what it held, and Tab goes to the next"
           (search "fake_snippet(x, y)" (buffer-text)))
    (check "and what is typed at a place is typed where the snippet has it again"
           (search "fake_snippet(x, y) { x } more" (buffer-text)))
    (post (list :named "Tab" '()))
    (post-text ";")
    (settle)
    (check "and the last Tab leaves point after it"
           (search "fake_snippet(x, y) { x } more;" (buffer-text)))
    ;; Actions whose edits are asked for, and that make files.
    (post-key #\< "Meta")
    (post-key #\c "Control") (post-key #\a "Control")
    (wait-until (lambda () (row-p " 3  Work it out later")) 10)
    (post-key #\3)
    (check "an action whose edit the server works out when asked is asked about"
           (wait-until (lambda () (starts-p "{ resolved }")) 10))
    ;; A question from the server, for the user to answer.
    (post-key #\c "Control") (post-key #\a "Control")
    (wait-until (lambda () (row-p " 4  Ask a question")) 10)
    (post-key #\4)
    (check "what a server asks the user is asked, its choices in a popup"
           (wait-until (lambda () (and (row-p " 1  Yes") (row-p " 2  No"))) 10))
    (post-key #\2)
    (settle)
    (post-key #\c "Control") (post-key #\d "Control")
    (check "and the server is told the answer"
           (wait-until (lambda () (row-p " answered: No")) 10))
    (post (list :named "Escape" '()))
    (settle)
    (post-key #\c "Control") (post-key #\a "Control")
    (wait-until (lambda () (row-p " 5  Make a file")) 10)
    (post-key #\5)
    (check "and an action that makes a file makes it"
           (wait-until (lambda () (probe-file (merge-pathnames "more.pas.made" *out*))) 10))
    (post-key #\x "Control") (post-key #\1)
    (post (list :open (namestring (merge-pathnames "more.pas" *out*))))
    (settle)
    (post-key #\x "Control") (post-key #\h)
    (extended-command "LSP Format Region")
    (check "LSP Format Region lays the region out"
           (wait-until (lambda () (starts-p "{ ranged }")) 10))
    (setf (hi::variable-value 'heml::lsp-format-on-type :global) t)
    (post-key #\> "Meta")
    (post-text ";")
    (check "with LSP Format on Type, the server lays out what is typed"
           (wait-until (lambda () (starts-p "{ typed }")) 10))
    (setf (hi::variable-value 'heml::lsp-format-on-type :global) nil)
    (post-key #\< "Meta")
    (post-key #\c "Control") (post-key #\l "Control")
    (check "C-c C-l does what the server offers for the line"
           (wait-until (lambda () (starts-p "{ lens }")) 10))
    (settle)
    (setf (hi::buffer-modified (hi::current-buffer)) nil)
    (let ((made (hi::getstring "more.pas.made" hi::*buffer-names*)))
      (when made (setf (hi::buffer-modified made) nil)))))

(defun language-server-watch-checks ()
  ;; Files a server asks to hear of, and settings that change: in a
  ;; project of its own, outside this one, whose build directory git and
  ;; so Heml leave out.
  (let* ((directory (ensure-directories-exist
                     (merge-pathnames (format nil "heml-smoke-~D/" (isys:getpid))
                                      (uiop:temporary-directory))))
         (settings (merge-pathnames ".heml-project" directory))
         (file (merge-pathnames "watch.pas" directory)))
    (flet ((row-p (text)
             (find text (map 'list #'heml.cocoa::row-text
                             (heml.cocoa::screen-rows heml.cocoa::*screen*))
                   :test #'search))
           (write-to (file text)
             (with-open-file (out file :direction :output :if-exists :supersede)
               (write-string text out))))
      (setf heml::*lsp-watch-ticks* 1)
      (write-to settings "()")
      (write-to file (format nil "program watch;~%  wrongthing here~%begin end.~%"))
      (post (list :open (namestring file)))
      (settle)
      (wait-until (lambda () (getf (run-font-at "wrongthing here" 1) :underline)) 30)
      ;; The first look at the files is what the later ones are compared with.
      (sleep 2)
      (write-to (merge-pathnames "new.watched" directory) "x")
      (write-to settings "(:settings ((\"fake\" (\"greeting\" . \"changed settings\"))))")
      (sleep 2)
      (post-key #\c "Control") (post-key #\d "Control")
      (check "a server is told of the files it asked to hear of"
             (wait-until (lambda () (row-p " watched: new.watched 1")) 10))
      (check "and of its settings when the project's change"
             (row-p " config: changed settings"))
      (post-key #\g "Control")
      (settle)
      (setf heml::*lsp-watch-ticks* 10)
      (ignore-errors (uiop:delete-directory-tree directory :validate t)))))

(defun selection-checks ()
  (note "a selection over syntax colours")
  (let ((file (merge-pathnames "selected.lisp" *out*)))
    (with-open-file (out file :direction :output :if-exists :supersede)
      (format out "(defun twice (x)~%  \"Twice X.\"~%  (* x 2))~%"))
    (post (list :open (namestring file)))
    (settle)
    (flet ((colour-of (font) (if (consp font) (getf font :fg) font)))
    (when (wait-until (lambda () (not (member (colour-of (run-font-at "\"Twice X.\"" 0)) '(nil 0)))) 10)
      (let ((colour (colour-of (run-font-at "\"Twice X.\"" 0))))
        (post-key #\< "Meta")
        (post-key #\Space "Control")
        (post-key #\n "Control") (post-key #\n "Control") (post-key #\e "Control")
        (settle)
        (check "a selection's background shows over syntax colours"
               (wait-until (lambda ()
                             (let ((font (run-font-at "\"Twice X.\"" 1)))
                               (and (consp font) (eq (getf font :bg) :selection))))
                           5))
        (check "and the text keeps its colour"
               (eql (colour-of (run-font-at "\"Twice X.\"" 1)) colour))
        (shot "selection-over-colours")
        (check "the modelines are drawn in the system's accent colour"
               (let ((font (run-font-at "Heml CL-USER:" 0)))
                 (and (consp font) (eq (getf font :bg) :accent)
                      (equal (heml.cocoa::color-name-for :accent) "controlAccentColor")
                      (equal (heml.cocoa::color-name-for :accent-text)
                             "alternateSelectedControlTextColor"))))
        (post-key #\g "Control")
        (settle)
        (check "and with the selection gone, its colour alone"
               (wait-until (lambda ()
                             (let ((font (run-font-at "\"Twice X.\"" 1)))
                               (not (and (consp font) (getf font :bg)))))
                           5)))))))

(defvar *corpus-result* nil)

(defun lisp-edit-checks ()
  (note "structural editing in Lisp mode, from sexp-edit")
  (let ((file (merge-pathnames "sexp.lisp" *out*)))
    (with-open-file (out file :direction :output :if-exists :supersede))
    (post (list :open (namestring file)))
    (settle)
    (post-text "(setq foo \"bar")
    (settle)
    (check "( and \" put in their pairs in Lisp mode"
           (wait-until (lambda () (search "(setq foo \"bar\")" (buffer-text))) 5))
    (post-key #\e "Control")
    (post (list :named "Backspace" '()))
    (post-text "X")
    (settle)
    (check "Backspace after a closing paren moves inside the list"
           (wait-until (lambda () (search "(setq foo \"bar\"X)" (buffer-text))) 5))
    (post-key #\e "Control")
    (post-text (format nil "~%(list (a) b)"))
    (post-key #\b "Control") (post-key #\b "Control") (post-key #\b "Control")
    (post-key #\b "Control")
    (post-key #\) "Control")
    (settle)
    (check "C-) slurps the next form into the list"
           (wait-until (lambda () (search "(list (a b))" (buffer-text))) 5))
    ;; Every case of sexp-edit's corpus, through Heml's buffers.
    (setf *corpus-result* nil)
    (extended-command "Editor Evaluate Expression")
    (settle)
    (post-text "(setf heml-smoke::*corpus-result* (progn (load (asdf:system-relative-pathname \"sexp-edit\" \"tests/cases.lisp\")) (heml::sexp-corpus-failures (symbol-value (find-symbol \"*EDIT-CASES*\" \"SEXP-EDIT-TESTS\")))))")
    (post-text (string #\Newline))
    (check "Heml edits sexp-edit's corpus as the library does"
           (and (wait-until (lambda () *corpus-result*) 60)
                (search "cases, 0 failed" *corpus-result*)))
    (unless (and *corpus-result* (search "cases, 0 failed" *corpus-result*))
      (note "~A" *corpus-result*))
    (setf (hi::buffer-modified (hi::current-buffer)) nil)))

(defun terminal-checks ()
  (note "a terminal")
  (flet ((row-p (text)
           (find text (map 'list #'heml.cocoa::row-text
                           (heml.cocoa::screen-rows heml.cocoa::*screen*))
                 :test #'search)))
    (extended-command "Term")
    (check "M-x Term runs a shell in a terminal"
           (wait-until (lambda () (row-p "bash-")) 20))
    (post-text (format nil "printf '\\033[31mred\\033[0m \\033[38;5;208morange\\033[0m \\033[38;2;80;160;255mrgb\\033[0m \\033[1;4mbold\\033[0m\\n'~%"))
    (check "what the program prints is shown, in its colours"
           (wait-until (lambda () (row-p "red orange rgb bold")) 10))
    (check "the terminal is as big as the window"
           (progn (post-text (format nil "echo cols-$(tput cols)~%"))
                  (wait-until (lambda () (and (row-p "cols-") (not (row-p "cols-80")))) 10)))
    (shot "terminal")
    (post-text (format nil "printf 'first\\nsecond\\n' | less~%"))
    (check "less runs in it"
           (wait-until (lambda () (row-p "(END)")) 10))
    (shot "terminal-less")
    (post-key #\q)
    (post-text (format nil "sleep 100~%"))
    (sleep 1)
    (post-key #\c "Control") (post-key #\c "Control")
    (post-text (format nil "echo int-$((3*3))~%"))
    (check "C-c C-c interrupts what runs in it"
           (wait-until (lambda () (row-p "int-9")) 10))
    (post-text (format nil "exit~%"))
    (check "and its end is shown"
           (wait-until (lambda () (row-p "The program ended with code 0")) 10))
    (post-key #\x "Control") (post-key #\k) (post (list :named "Return" '()))
    (settle)
    ;; Gone before the projects' checks, which count the windows.
    (check "killing its buffer ends the terminal"
           (wait-until (lambda () (not (hi::getstring "*terminal*" hi::*buffer-names*))) 10))))

(defun git-checks ()
  (note "git")
  (let* ((repo (merge-pathnames "gitrepo/" *out*))
         (file (merge-pathnames "notes.txt" repo)))
    (uiop:delete-directory-tree repo :validate t :if-does-not-exist :ignore)
    (ensure-directories-exist repo)
    (flet ((sh (command)
             (uiop:run-program (list "/bin/sh" "-c" command) :directory (namestring repo)))
           (row-p (text)
             (find text (map 'list #'heml.cocoa::row-text
                             (heml.cocoa::screen-rows heml.cocoa::*screen*))
                   :test #'search)))
      (sh "git init -q -b main && git config user.email t@example.com && git config user.name Test")
      (sh "printf 'one\\ntwo\\nthree\\nfour\\nfive\\n' > notes.txt && git add notes.txt && git commit -q -m 'First commit'")
      (sh "printf 'one\\nTWO\\nthree\\nfive\\nsix\\n' > notes.txt")
      (post (list :open (namestring file)))
      (settle)
      (check "a file Git tracks has its changed lines marked in the fringe"
             (wait-until (lambda () (and (row-p "▎TWO") (row-p "▎six") (row-p "▁three"))) 10))
      (shot "git-fringe")
      (post-key #\x "Control") (post-key #\g)
      (check "C-x g shows the repository's status"
             (wait-until (lambda () (row-p "Unstaged changes (1)")) 10))
      (post-key #\< "Meta")
      (post-key #\n) (post-key #\n)
      (post (list :named "Tab" '()))
      (check "and Tab its hunk"
             (wait-until (lambda () (row-p "@@ -1,5 +1,5 @@")) 10))
      (shot "git-status")
      (post-key #\q)
      (post-key #\x "Control") (post-key #\1)
      ;; Out of the way of the projects' checks, which switch to the last.
      (settle)
      (extended-command "Forget Project")
      (post-key #\x "Control") (post-key #\k) (post (list :named "Return" '()))
      (settle))))

(defun debugger-checks ()
  (note "debugging")
  (let ((file (merge-pathnames "prog.py" *out*)))
    (with-open-file (out file :direction :output :if-exists :supersede)
      (format out "def caller():~%    x = 1~%    point = 2~%    print(x)~%    return x~%"))
    (post (list :open (namestring file)))
    (settle)
    (flet ((row-p (text)
             (find text (map 'list #'heml.cocoa::row-text
                             (heml.cocoa::screen-rows heml.cocoa::*screen*))
                   :test #'search))
           (point-line () (line-text (hi::mark-line (hi::current-point))))
           (buffer-named (name) (hi::getstring name hi::*buffer-names*)))
      (post-key #\< "Meta")
      (post-key #\n "Control") (post-key #\n "Control")
      (post (list :named "F9" '()))
      (check "F9 puts a breakpoint on a line, a dot in the fringe beside it"
             (wait-until (lambda () (row-p "● " )) 10))
      (check "and only that line"
             (and (row-p "●     point = 2") (not (row-p "●     x = 1"))))
      (post (list :named "F5" '()))
      (check "F5 debugs the file, which stops at the breakpoint, an arrow in the fringe beside its line"
             (wait-until (lambda () (row-p "●▶    point = 2")) 20))
      (shot "debugger-stopped")
      (check "with point on that line"
             (equal "    point = 2" (point-line)))
      (post-key #\c "Control") (post-key #\d) (post-key #\w)
      (check "C-c d w shows the stopped program's frames and variables"
             (wait-until (lambda ()
                           (let ((buffer (buffer-named "Debugger")))
                             (and buffer
                                  (search ">#0 fake_main  prog.py:3" (buffer-text buffer))
                                  (search "#1 fake_caller  prog.py:1" (buffer-text buffer))
                                  (search "x = 1  (int)" (buffer-text buffer))
                                  (search "point = {...}  (struct point)  ..." (buffer-text buffer)))))
                         10))
      ;; Return on a variable with parts shows them; on a frame, selects it.
      (post-key #\< "Meta")
      (loop repeat 8 do (post-key #\n "Control"))
      (post (list :named "Return" '()))
      (check "Return on a variable with parts shows them, under it"
             (wait-until (lambda ()
                           (search "    a = 2  (int)" (buffer-text (buffer-named "Debugger"))))
                         10))
      (post-key #\< "Meta")
      (loop repeat 4 do (post-key #\n "Control"))
      (post (list :named "Return" '()))
      (check "and Return on a frame selects it: its variables"
             (wait-until (lambda ()
                           (search "caller_var = 7" (buffer-text (buffer-named "Debugger"))))
                         10))
      (check "and its place"
             (wait-until (lambda () (row-p "▶def caller():")) 10))
      (post-key #\x "Control") (post-key #\1)
      (post (list :open (namestring file)))
      (settle)
      (post (list :named "F10" '()))
      (check "F10 goes on to the next line"
             (wait-until (lambda () (row-p "▶    print(x)")) 10))
      (post-key #\c "Control") (post-key #\d) (post-key #\e)
      (post-key #\a "Control") (post-key #\k "Control")
      (post-text "x
")
      (check "C-c d e evaluates an expression in the stopped program"
             (wait-until (lambda () (row-p "x = 42")) 10))
      (post (list :named "F5" '()))
      (check "and F5 lets it go on to the end, its output in Debug Output"
             (wait-until (lambda ()
                           (let ((buffer (buffer-named "Debug Output")))
                             (and buffer
                                  (search "hello from the fake program" (buffer-text buffer))
                                  (search "Debugging is over." (buffer-text buffer)))))
                         20))
      (check "and no arrow is left"
             (wait-until (lambda () (not (row-p "▶"))) 10))
      (post (list :open (namestring file)))
      (settle)
      (post-key #\< "Meta")
      (post-key #\n "Control") (post-key #\n "Control")
      (post (list :named "F9" '()))
      (check "F9 again takes the breakpoint away"
             (wait-until (lambda () (not (row-p "●"))) 10)))))

(defun language-server-kind-checks ()
  ;; Servers of other kinds, for YAML and JSON while these checks run: one
  ;; that says what is wrong only when asked, and one that keeps dying.
  (let ((servers heml::*language-servers*)
        (fake (namestring (merge-pathnames "test/fake-lsp.py"
                                           (asdf:system-source-directory :heml.cocoa)))))
    (flet ((file (name text)
             (with-open-file (out (merge-pathnames name *out*)
                                  :direction :output :if-exists :supersede)
               (write-string text out))
             (post (list :open (namestring (merge-pathnames name *out*))))
             (settle)
             (hi::current-buffer))
           (wrong (buffer)
             ;; What the server says is wrong in BUFFER, the first of it.
             (fourth (first (gethash buffer heml::*buffer-diagnostics*)))))
      (heml::define-language-server "YAML" (list (list "python3" fake "--pull")))
      (let ((first (file "pulled-a.yaml" (format nil "a: 1~%b: 2~%"))))
        (check "a server that says what is wrong only when asked is asked"
               (wait-until (lambda () (equal "pulled error: 10" (wrong first))) 30))
        (let ((second (file "pulled-b.yaml" (format nil "c: 3~%d: 4~%"))))
          (check "for each file it is told of"
                 (wait-until (lambda () (equal "pulled error: 20" (wrong second))) 30))
          (post-text "#")
          (check "and again when the file changes"
                 (wait-until (lambda () (equal "pulled error: 21" (wrong second))) 30))
          (check "about its other files too, which the change may have changed"
                 (wait-until (lambda () (equal "pulled error: 21" (wrong first))) 30))
          (settle)
          (setf (hi::buffer-modified second) nil)))
      ;; A second server for Pascal, beside the first: what is wrong is what
      ;; either finds.
      (heml::define-additional-language-server "Pascal" "second"
                                               (list (list "python3" fake "--pull")))
      (let ((buffer (file "two.pas" (format nil "program two;~%  wrongthing here~%begin end.~%"))))
        (check "a buffer with two servers shows what both find wrong"
               (wait-until (lambda ()
                             (let ((messages (mapcar #'fourth
                                                     (gethash buffer heml::*buffer-diagnostics*))))
                               (and (member "fake error" messages :test #'equal)
                                    (find "pulled error" messages :test #'search))))
                           30))
        (extended-command "LSP Diagnostics")
        (check "and LSP Diagnostics lists both"
               (wait-until (lambda ()
                             (and (search "error: fake error" (buffer-text))
                                  (search "error: pulled error" (buffer-text))))
                           10))
        (post-key #\x "Control") (post-key #\1)
        (post (list :open (namestring (merge-pathnames "two.pas" *out*))))
        (settle)
        (post-key #\c "Control") (post-key #\d "Control")
        (check "C-c C-d says what each of them says"
               (wait-until (lambda ()
                             (let ((rows (map 'list #'heml.cocoa::row-text
                                              (heml.cocoa::screen-rows heml.cocoa::*screen*))))
                               (and (find " fake hover text" rows :test #'search)
                                    (find " second hover" rows :test #'search))))
                           10))
        (post (list :named "Escape" '()))
        (settle)
        (post-key #\> "Meta")
        (post-text "fake_se")
        (post-key #\i "Control" "Meta")
        (check "and the completions are those of both"
               (wait-until (lambda () (search "fake_second" (buffer-text buffer))) 10))
        (settle)
        (setf (hi::buffer-modified buffer) nil))
      (setf heml::*additional-language-servers* '())
      (heml::define-language-server "JSON" (list (list "python3" fake "--crash")))
      (let ((buffer (file "crash.json" (format nil "{}~%"))))
        (check "a server that keeps dying is not started for ever"
               (wait-until (lambda () (heml::buffer-server-failed-p buffer)) 60))
        (check "and the modeline says there is none"
               (wait-until (lambda ()
                             (find "(no server)"
                                   (map 'list #'heml.cocoa::row-text
                                        (heml.cocoa::screen-rows heml.cocoa::*screen*))
                                   :test #'search))
                           10))))
    (setf heml::*language-servers* servers)))


;;;; The run

(defun run ()
  (ensure-directories-exist *out*)
  ;; No language server this machine has is started: the checks are of
  ;; Heml, and the only servers are the stand-ins named further on.  The
  ;; servers themselves are `make smoke-lsp`'s.
  (setf heml::*language-servers*
        (loop for (mode nil language group) in heml::*language-servers*
              collect (list mode '() language group)))
  (setf heml::*additional-language-servers* '())
  ;; Nor any debugger the machine has: the stand-in, test/fake-dap.py, is
  ;; Python's for the run.
  (setf heml::*debug-adapters* '())
  ;; A terminal's shell, without anyone's startup files.
  (setf (hi:variable-value 'heml::term-program :global) "/bin/bash --norc --noprofile")
  (heml::define-debug-adapter
   "fake" :modes '("Python")
   :commands (list (list "python3" (namestring (merge-pathnames "test/fake-dap.py"
                                                                (asdf:system-source-directory
                                                                 :heml.cocoa)))))
   :launch 'heml::file-launch)
  ;; Hints change what a line shows: off, until the checks of them.
  (setf (hi::variable-value 'heml::lsp-inlay-hints :global) nil)
  (sleep 2)
  (note "typing")
  (post-text "(defun hello (name)
  (format t \"Hello, ~A!~%\" name))
café λ 日本語 end")
  (settle)
  (check "posted text is in the buffer"
         (search "café λ 日本語 end" (buffer-text)))
  (check "a wide character takes two columns"
         (= (point-column) (+ (length "café λ ") (* 3 2) (length " end"))))
  (shot "typed")
  (check "a buffer that is not in Lisp mode is not coloured as Lisp"
         (null (row-runs-containing "(format t")))

  (note "key events")
  (post-text (string #\Newline))
  (loop for c across "keys ok" do (press (string c)))
  (settle)
  (check "NSEvent key-downs type through the input context"
         (search "keys ok" (buffer-text)))
  (press "≈" :unmodified "x" :flags +left-option+)
  (settle)
  (check "left Option is Meta: M-x prompts"
         (eq hi::*current-window* hi::*echo-area-window*))
  (shot "meta-x")
  (post-key #\g "Control")
  (settle)
  (main (objc:invoke (view) "setMarkedText:selectedRange:replacementRange:"
                     "にほん" (cons 3 0) (cons cocoa:ns-not-found 0)))
  (settle)
  (check "marked text is held, not inserted"
         (and (heml.cocoa::display-marked-text (display))
              (not (search "にほん" (buffer-text)))))
  (shot "marked-text")
  (main (objc:invoke (view) "insertText:replacementRange:" "日本" (cons cocoa:ns-not-found 0)))
  (settle)
  (check "committed text is inserted"
         (and (null (heml.cocoa::display-marked-text (display)))
              (search "keys ok日本" (buffer-text))))

  (note "windows")
  (post-key #\< "Meta")
  (post-key #\x "Control") (post-key #\2)
  (settle)
  (check "C-x 2 shows the buffer in two windows"
         (= 2 (length (hi::buffer-windows (hi::current-buffer)))))
  (shot "split")
  (let ((columns (heml.cocoa::screen-columns heml.cocoa::*screen*)))
    (main (objc:invoke (window) "setFrame:display:" (vector 100d0 100d0 640d0 520d0) t))
    (settle)
    (check "resizing the window changes the grid"
           (/= columns (heml.cocoa::screen-columns heml.cocoa::*screen*))))
  (shot "resized")
  (post-key #\x "Control") (post-key #\1)
  (main (objc:invoke (window) "setFrame:display:" (vector 100d0 100d0 900d0 700d0) t))
  (settle)

  (note "mouse")
  (mouse :down 7 0)
  (mouse :drag 10 0)
  (mouse :up 12 0)
  (settle)
  (check "a drag marks a region" (heml::region-active-p))
  (check "the region is what was dragged over" (equal (region-text) "hello"))
  (shot "drag")
  (mouse :down 3 1)
  (mouse :up 3 1)
  (mouse :down 3 1 :clicks 2)
  (mouse :up 3 1 :clicks 2)
  (settle)
  (check "a double click selects a word" (equal (region-text) "format"))
  (mouse :down 3 1 :clicks 3)
  (mouse :up 3 1 :clicks 3)
  (settle)
  (check "a triple click selects a line"
         (equal (region-text) (format nil "  (format t \"Hello, ~~A!~~%\" name))~%")))
  (shot "triple-click")
  (let ((menu (main (objc:invoke (view) "menuForEvent:" (mouse-event :right-down 5 1 0 1)))))
    (settle)
    (check "a right click has a menu"
           (and (not (cffi:null-pointer-p menu))
                (main (plusp (objc:invoke menu "numberOfItems"))))))
  (check "a right click in the selection keeps it"
         (and (heml::region-active-p) (search "format" (region-text))))
  (main (objc:invoke (view) "menuForEvent:" (mouse-event :right-down 2 0 0 1)))
  (settle)
  (check "a right click elsewhere moves point" (= (point-column) 2))
  (mouse :down 7 0) (mouse :drag 12 0) (mouse :up 12 0)
  (settle)

  (note "clipboard")
  (press-menu #\c)
  (settle)
  (check "Cmd-C puts the region on the pasteboard" (equal (pasteboard-text) "hello"))
  (main (heml.cocoa::write-pasteboard "from elsewhere")
        ;; As if another application had written it.
        (setf heml.cocoa::*pasteboard-change-count* nil))
  (post-key #\> "Meta")
  (press-menu #\v)
  (settle)
  (check "Cmd-V pastes what another application copied"
         (search "from elsewhere" (buffer-text)))
  (shot "pasted")

  (note "opening files")
  (let ((file (merge-pathnames "opened.lisp" *out*)))
    (with-open-file (out file :direction :output :if-exists :supersede
                              :external-format :utf-8)
      (write-line ";;; Opened from Finder, λ." out))
    (main (let ((url (objc:invoke "NSURL" "fileURLWithPath:" (namestring file))))
            (objc:invoke (objc:objc-object-pointer (heml.cocoa::display-app-delegate (display)))
                         "application:openURLs:"
                         (objc.runloop:shared-application)
                         (objc:invoke "NSArray" "arrayWithObject:" url))))
    (settle)
    (check "application:openURLs: visits the file"
           (and (equal (truename (hi::buffer-pathname (hi::current-buffer)))
                       (truename file))
                (search "Opened from Finder, λ." (buffer-text))))
    ;; The first buffer a grammar colours waits for the grammar to load.
    (check "a Lisp file is coloured: its comment is red"
           (wait-until (lambda () (eql 1 (run-font-at ";;; Opened" 0))) 10)))
  (shot "opened")

  (note "tree-sitter")
  (let ((file (merge-pathnames "opened.c" *out*)))
    (with-open-file (out file :direction :output :if-exists :supersede)
      (write-line "/* a C comment */" out)
      (write-line "int main(void) { return 0; }" out))
    (post (list :open (namestring file)))
    (settle)
    (check "a C file is in C mode"
           (equal "C" (hi::buffer-major-mode (hi::current-buffer))))
    (if (and (heml.tree-sitter:tree-sitter-available-p)
             (heml.tree-sitter::find-in-directories "lib/libtree-sitter-c.dylib"))
        (progn
          (check "tree-sitter colours its comment"
                 (eql 1 (run-font-at "/* a C comment" 0)))
          (check "and its type"
                 (eql 2 (run-font-at "int main" 0)))
          (shot "tree-sitter"))
        (note "  skip  tree-sitter colouring: no tree-sitter or C grammar (make tree-sitter)")))
  (let ((file (merge-pathnames "styles.md" *out*)))
    (with-open-file (out file :direction :output :if-exists :supersede)
      (write-line "Some *slanted* and **heavy** text." out))
    (post (list :open (namestring file)))
    (settle)
    (when (heml.tree-sitter::find-in-directories "lib/libtree-sitter-markdown_inline.dylib")
      (check "Markdown's emphasis is italic and its strong emphasis bold"
             (and (getf (run-font-at "*slanted*" 1) :italic)
                  (getf (run-font-at "**heavy**" 2) :bold)))))
  (let ((file (merge-pathnames "links.txt" *out*))
        (target (merge-pathnames "linked.txt" *out*)))
    (with-open-file (out target :direction :output :if-exists :supersede)
      (write-line "the linked file" out))
    (with-open-file (out file :direction :output :if-exists :supersede)
      (write-line "Go [there](linked.txt) now." out))
    (post (list :open (namestring file)))
    (settle)
    (check "a link is underlined, in the link colour"
           (let ((font (run-font-at "[there]" 1)))
             (and (getf font :underline) (getf font :link))))
    (let* ((rows (heml.cocoa::screen-shown-rows heml.cocoa::*screen*))
           (line (position-if (lambda (row) (search "[there]" (heml.cocoa::row-text row))) rows))
           (column (and line (search "[there]" (heml.cocoa::row-text (svref rows line))))))
      (when line
        (mouse :down (+ column 2) line :flags +command+)
        (mouse :up (+ column 2) line :flags +command+)
        (settle)))
    (check "Command-click follows it"
           (let ((pathname (hi::buffer-pathname (hi::current-buffer))))
             (and pathname (equal "linked.txt" (file-namestring pathname))))))
  (let ((file (merge-pathnames "greet" *out*)))
    (with-open-file (out file :direction :output :if-exists :supersede)
      (write-line "#!/bin/sh" out)
      (write-line "# say hello" out)
      (write-line "echo \"hello $1\"" out))
    (post (list :open (namestring file)))
    (settle)
    (check "a script without a type is in the mode its #! line names"
           (equal "Shell Script" (hi::buffer-major-mode (hi::current-buffer))))
    (when (heml.tree-sitter::find-in-directories "lib/libtree-sitter-bash.dylib")
      (check "tree-sitter colours a shell script's comment and string"
             (wait-until (lambda ()
                           (and (eql 1 (run-font-at "# say hello" 0))
                                (eql 4 (run-font-at "\"hello $1" 0))))
                         10))))
  (let ((file (merge-pathnames "hello.pas" *out*)))
    (with-open-file (out file :direction :output :if-exists :supersede)
      (write-line "program Hello;" out)
      (write-line "begin WriteLn('hi') end." out))
    (post (list :open (namestring file)))
    (settle)
    (check "a Pascal file is in Pascal mode"
           (equal "Pascal" (hi::buffer-major-mode (hi::current-buffer))))
    (when (heml.tree-sitter::find-in-directories "lib/libtree-sitter-pascal.dylib")
      (check "tree-sitter colours Pascal's keywords"
             (eql 5 (run-font-at "program Hello" 0)))))
  ;; The languages added with their grammars: each a mode by its file's
  ;; type, coloured by its grammar's query.
  (flet ((visit (name &rest lines)
           (with-open-file (out (merge-pathnames name *out*) :direction :output :if-exists :supersede)
             (dolist (line lines) (write-line line out)))
           (post (list :open (namestring (merge-pathnames name *out*))))
           (settle))
         (grammar-p (name)
           (heml.tree-sitter::find-in-directories (format nil "lib/libtree-sitter-~A.dylib" name))))
    (visit "hello.rs" "// a Rust comment" "fn main() { let s = \"text\"; }")
    (check "a .rs file is in Rust mode"
           (equal "Rust" (hi::buffer-major-mode (hi::current-buffer))))
    (when (grammar-p "rust")
      (check "tree-sitter colours Rust's comment and string"
             (and (eql 1 (run-font-at "// a Rust comment" 0))
                  (eql 4 (run-font-at "\"text\"" 1)))))
    (visit "hello.go" "// a Go comment" "package main")
    (when (grammar-p "go")
      (check "and Go's comment and keyword"
             (and (equal "Go" (hi::buffer-major-mode (hi::current-buffer)))
                  (eql 1 (run-font-at "// a Go comment" 0))
                  (eql 5 (run-font-at "package main" 0)))))
    (visit "hello.ts" "// a TypeScript comment" "function f(a: number): number { return a; }")
    (check "a .ts file is in TS mode"
           (equal "TS" (hi::buffer-major-mode (hi::current-buffer))))
    (when (grammar-p "typescript")
      (check "tree-sitter colours TypeScript's comment, keyword and type"
             (and (eql 1 (run-font-at "// a TypeScript comment" 0))
                  (eql 5 (run-font-at "function f" 0))
                  (eql 2 (run-font-at "number)" 0)))))
    (visit "hello.js" "// a JavaScript comment" "const s = 'text';")
    (when (grammar-p "javascript")
      (check "and JavaScript's comment and string"
             (and (equal "JavaScript" (hi::buffer-major-mode (hi::current-buffer)))
                  (eql 1 (run-font-at "// a JavaScript comment" 0))
                  (eql 4 (run-font-at "'text'" 1)))))
    (visit "data.json" "{\"key\": \"value\", \"n\": 12}")
    (when (grammar-p "json")
      (check "and JSON's string and number"
             (and (equal "JSON" (hi::buffer-major-mode (hi::current-buffer)))
                  (eql 4 (run-font-at "\"value\"" 1))
                  (eql 3 (run-font-at "12}" 0)))))
    ;; A Markdown code block that names its language is coloured as that
    ;; language; one that names none Heml knows stays as code.
    (visit "blocks.md" "# Blocks" "" "```c" "int x = 1; /* note */" "```" ""
           "```nosuchlanguage" "plain words" "```")
    (when (and (grammar-p "markdown") (grammar-p "c"))
      (check "a Markdown code block is coloured by the language it names"
             (and (eql 2 (run-font-at "int x = 1;" 0))
                  (eql 3 (run-font-at "int x = 1;" 8))
                  (eql 1 (run-font-at "/* note */" 0))))
      (check "and one in a language Heml has not is left as code"
             (eql 2 (run-font-at "plain words" 0))))
    ;; Files known by their names: a shell's own, which have no type and
    ;; no #! line.
    (visit ".bashrc" "# a bashrc comment" "export EDITOR=\"heml\"")
    (check "a file with a well-known name, .bashrc, is in its mode"
           (equal "Shell Script" (hi::buffer-major-mode (hi::current-buffer))))
    (when (grammar-p "bash")
      (check "and is coloured"
             (and (eql 1 (run-font-at "# a bashrc comment" 0))
                  (eql 4 (run-font-at "\"heml\"" 1)))))
    (setf (hi::variable-value 'heml::mode-from-file-name :global) nil)
    (visit ".zshenv" "# a zshenv comment")
    (check "unless Mode from File Name is off"
           (not (equal "Shell Script" (hi::buffer-major-mode (hi::current-buffer)))))
    (setf (hi::variable-value 'heml::mode-from-file-name :global) t)
    (heml::define-file-name-mode '("Smokefile" "smoke.*.conf") "Python")
    (visit "Smokefile" "x = 1")
    (check "a name of one's own is given a mode with define-file-name-mode"
           (equal "Python" (hi::buffer-major-mode (hi::current-buffer))))
    (visit "smoke.local.conf" "x = 1")
    (check "and a * in it stands for any characters"
           (equal "Python" (hi::buffer-major-mode (hi::current-buffer))))
    (visit "conf.yaml" "# a YAML comment" "key: value")
    (when (grammar-p "yaml")
      (check "and YAML's comment"
             (and (equal "YAML" (hi::buffer-major-mode (hi::current-buffer)))
                  (eql 1 (run-font-at "# a YAML comment" 0))))))
  (let ((file (merge-pathnames "block.lisp" *out*)))
    (with-open-file (out file :direction :output :if-exists :supersede)
      (write-line "#| a block comment |#" out)
      (write-line "(defun f (x &key y) (list x y :key))" out))
    (post (list :open (namestring file)))
    (settle)
    (if (heml.tree-sitter::find-in-directories "lib/libtree-sitter-commonlisp.dylib")
        (progn
          (check "tree-sitter colours Lisp: a block comment is red"
                 (eql 1 (run-font-at "#| a block" 0)))
          (check "and &key is a keyword"
                 (eql 5 (run-font-at "&key y" 0))))
        (check "without tree-sitter's grammar, Heml's own parser colours Lisp"
               (run-font-at "(defun f" 1))))

  (note "modes")
  (flet ((file-buffer (name)
           (find name hi::*buffer-list*
                 :key (lambda (b) (and (hi::buffer-pathname b) (file-namestring (hi::buffer-pathname b))))
                 :test #'equal))
         (point-line () (line-text (hi::mark-line (hi::current-point))))
         (write-file (name &rest lines)
           (with-open-file (out (merge-pathnames name *out*) :direction :output :if-exists :supersede)
             (dolist (line lines) (write-line line out)))
           (merge-pathnames name *out*)))
    (post (list :open (namestring (write-file "grep.txt" "alpha one" "beta two" "alpha three"))))
    (settle)
    (extended-command "Grep")
    ;; The prompt holds its default, grep -nH -e, to add to.
    (post-text "alpha grep.txt
")
    (check "Grep lists what it finds, and says how many"
           (wait-until (lambda () (search "Grep finished: 2 results." (buffer-text)))))
    (settle)
    (check "Grep colours the file, the line number and the match"
           (and (eql 5 (run-font-at "grep.txt:1:alpha" 0))
                (eql 2 (run-font-at "grep.txt:1:alpha" 9))
                (equal '(:fg 1 :bold t) (run-font-at "grep.txt:1:alpha" 11))))
    (post-key #\n)
    (post (list :named "Return" '()))
    (settle)
    (check "n and Return visit the first match"
           (and (eq (hi::current-buffer) (file-buffer "grep.txt"))
                (equal "alpha one" (point-line))))
    (post-key #\x "Control") (post-key #\`)
    (settle)
    (check "C-x ` visits the next"
           (equal "alpha three" (point-line)))
    (post-key #\x "Control") (post-key #\o)
    (post-key #\x "Control") (post-key #\q "Control")
    (settle)
    (check "C-x C-q makes the lines editable"
           (equal "Wgrep" (hi::buffer-major-mode (hi::current-buffer))))
    (post-key #\< "Meta")
    (extended-command "Replace String")
    (post-text "alpha
gamma
")
    (post-key #\c "Control") (post-key #\c "Control")
    (settle)
    (check "C-c C-c writes the lines changed to their file's buffer"
           (let ((text (buffer-text (file-buffer "grep.txt"))))
             (and (search "gamma one" text) (search "gamma three" text) (search "beta two" text))))
    (check "and goes back to Grep"
           (equal "Grep" (hi::buffer-major-mode (hi::current-buffer))))
    (post-key #\x "Control") (post-key #\1)
    (post (list :open (namestring (write-file "tb.py" "def f():" "    raise ValueError('x')" "" "f()"))))
    (settle)
    (extended-command "Compile")
    ;; Its default, make -k, goes.
    (post-key #\a "Control") (post-key #\k "Control")
    (post-text "python3 tb.py
")
    (check "Compile lists what a run prints"
           (wait-until (lambda () (search "Compilation finished" (buffer-text)))))
    (post-key #\x "Control") (post-key #\`)
    (settle)
    (check "C-x ` visits the place a traceback names"
           (and (eq (hi::current-buffer) (file-buffer "tb.py"))
                (member (point-line) '("f()" "    raise ValueError('x')") :test #'equal)))
    (post-key #\x "Control") (post-key #\1)
    (when (heml.tree-sitter::find-in-directories "lib/libtree-sitter-python.dylib")
      (extended-command "Outline")
      (settle)
      ;; As its grammar has them, or, where a Python language server is
      ;; installed and ready, as the server names them.
      (check "Outline lists a Python file's definitions"
             (and (equal "Outline" (hi::buffer-major-mode (hi::current-buffer)))
                  (or (search "def f():" (buffer-text))
                      (search "function f" (buffer-text)))))
      (post-key #\x "Control") (post-key #\1))
    (when (heml.tree-sitter::find-in-directories "lib/libtree-sitter-c.dylib")
      (post (list :open (namestring (write-file "defs.c" "int a(void) {" "  return 1;" "}"
                                                "int b(void) {" "  return 2;" "}"))))
      (settle)
      (post-key #\> "Meta")
      (post-key #\a "Control" "Meta")
      (settle)
      (check "C-M-a goes to the start of the C function before"
             (equal "int b(void) {" (point-line)))
      (post-key #\a "Control" "Meta")
      (post-key #\e "Control" "Meta")
      (settle)
      (check "C-M-e goes past the end of this one"
             (equal "int b(void) {" (point-line))))
    (when (probe-file "/opt/homebrew/bin/fpc")
      (post (list :open (namestring (write-file "bad.pas" "program bad;" "begin" "  x := 1;" "end."))))
      (settle)
      (post-key #\c "Control") (post-key #\c "Control")
      (check "C-c C-c compiles Pascal with fpc, staying in the file's window"
             (and (wait-until (lambda ()
                                (let ((compilation (hi::getstring "*compilation*" hi::*buffer-names*)))
                                  ;; This compilation's, not one before.
                                  (and compilation
                                       (search "fpc 'bad.pas'" (buffer-text compilation))
                                       (search "Compilation finished" (buffer-text compilation))))))
                  (eq (hi::current-buffer) (file-buffer "bad.pas"))))
      (post-key #\x "Control") (post-key #\`)
      (settle)
      (check "and C-x ` visits its error"
             (and (eq (hi::current-buffer) (file-buffer "bad.pas"))
                  (equal "  x := 1;" (point-line))))
      (post-key #\x "Control") (post-key #\1))
    (post (list :open (namestring (write-file "heads.md" "# One" "text" "## Two" "more"))))
    (settle)
    (post-key #\c "Control") (post-key #\n "Control")
    (settle)
    (check "C-c C-n goes to Markdown's next heading"
           (equal "## Two" (point-line)))
    (post-key #\c "Control") (post-key #\=)
    (settle)
    (check "C-c = demotes it"
           (equal "### Two" (point-line)))
    (post (list :open (namestring (write-file "fill.md" "Intro." ""
                                              (concatenate 'string "- " (format nil "~{~A~^ ~}" (loop repeat 30 collect "word")))
                                              "- next item"))))
    (settle)
    (post-key #\n "Control") (post-key #\n "Control")
    (post-key #\q "Meta")
    (settle)
    (check "M-q fills a Markdown list item, its lines hung under its text"
           (let ((lines (uiop:split-string (buffer-text) :separator (string #\Newline))))
             (and (every (lambda (l) (<= (length l) 75)) lines)
                  (eql 0 (search "- word" (third lines)))
                  (eql 0 (search "  word" (fourth lines)))
                  (member "- next item" lines :test #'equal))))
    (extended-command "Recursive Grep")
    (post-key #\a "Control") (post-key #\k "Control")
    (post-text "beta two
")
    (post (list :named "Return" '()))
    (check "Recursive Grep searches the directory"
           (wait-until (lambda () (and (search "Grep finished" (buffer-text))
                                       (search "grep.txt:2:beta two" (buffer-text))))))
    (post-key #\x "Control") (post-key #\1)
    ;; Completion at point: a popup under the word, which typing narrows.
    (with-open-file (out (merge-pathnames "comp.txt" *out*) :direction :output :if-exists :supersede)
      (write-line "zebraone zebratwo" out)
      (write-string "zeb" out))
    (post (list :open (namestring (merge-pathnames "comp.txt" *out*))))
    (settle)
    ;; To the end of zeb, on the last line but one.
    (post-key #\> "Meta")
    (post-key #\b "Control")
    (post-key #\i "Control" "Meta")
    (flet ((popup-row-font (text)
             ;; The font of the popup's row showing TEXT, a row that starts
             ;; nothing else, or NIL when there is no such row.
             (let* ((bare (string-trim " " text))
                    (row (find-if (lambda (row)
                                    (string= bare (string-trim " " (heml.cocoa::row-text row))))
                                  (heml.cocoa::screen-rows heml.cocoa::*screen*))))
               (when row
                 (let ((column (search bare (heml.cocoa::row-text row))))
                   (loop for (start end . font) in (heml.cocoa::row-runs row)
                         when (and (<= start column) (< column end)) return font))))))
      (check "C-M-i shows the completions in a popup under the word"
             (wait-until (lambda ()
                           (and (equal heml::*popup-selected-font* (popup-row-font " zebraone "))
                                (equal heml::*popup-font* (popup-row-font " zebratwo "))))
                         10))
      (shot "completion")
      (post-text "rat")
      (settle)
      (check "typing narrows them"
             (and (equal heml::*popup-selected-font* (popup-row-font " zebratwo "))
                  (null (popup-row-font " zebraone "))))
      (post (list :named "Return" '()))
      (settle)
      (check "and Return puts the chosen one in"
             (and (search (format nil "~%zebratwo") (buffer-text))
                  (null hi::*popup*)
                  (null (popup-row-font " zebratwo "))))
      ;; As one types, when that is asked for: Tab puts the choice in.
      (extended-command "Complete as You Type")
      (settle)
      (post-key #\e "Control")
      (post-text " zeb")
      (settle)
      (check "with Complete as You Type on, the popup comes up by itself"
             (wait-until (lambda () (popup-row-font " zebraone ")) 10))
      (post (list :named "Tab" '()))
      (settle)
      (check "and Tab puts the choice in"
             (search "zebratwo zebraone" (buffer-text)))
      (extended-command "Complete as You Type")
      (settle)
      ;; Lisp's symbols say what they name.
      (post (list :open (namestring (write-file "kinds.lisp" "(mapc"))))
      (settle)
      (post-key #\e "Control")
      (post-key #\i "Control" "Meta")
      (check "a Lisp symbol's completions say what each names"
             (wait-until (lambda ()
                           (find-if (lambda (row)
                                      (let ((text (heml.cocoa::row-text row)))
                                        (and (search " mapcar " text) (search "function" text))))
                                    (heml.cocoa::screen-rows heml.cocoa::*screen*)))
                         10))
      (post (list :named "Escape" '()))
      (settle)
      ;; A space after an operator shows what it takes.
      (post-text "ar ")
      (check "in Lisp, a space after an operator shows its arguments over the call"
             (wait-until (lambda ()
                           (find "(mapcar function list" (map 'list #'heml.cocoa::row-text
                                                               (heml.cocoa::screen-rows heml.cocoa::*screen*))
                                 :test #'search))
                         10))
      (post-key #\a "Control")
      (settle)
      (check "and going before the call takes them away"
             (null hi::*popup*))
      ;; The file prompt: the files that start so, at the foot of the window.
      (post-key #\x "Control") (post-key #\f "Control")
      (post-text "gre")
      (post (list :named "Tab" '()))
      (check "at a file prompt, Tab shows the files that start so in a popup"
             (wait-until (lambda () (and (popup-row-font " greet") (popup-row-font " grep.txt"))) 10))
      (post-text "p")
      (post (list :named "Return" '()))
      (post (list :named "Return" '()))
      (check "and the one chosen is visited"
             (wait-until (lambda ()
                           (equal (hi::buffer-pathname (hi::current-buffer))
                                  (merge-pathnames "grep.txt" *out*)))
                         10)))
    (post (list :open (namestring (write-file "words.txt" "Hello wrold here"))))
    (settle)
    (extended-command "Auto Spell Mode")
    (settle)
    (check "Spell mode underlines a misspelled word in red, and not the others"
           (and (equal '(:fg 1 :underline t) (run-font-at "wrold" 1))
                (null (run-font-at "Hello" 1)))))

  (note "language servers")
  ;; A stand-in server (test/fake-lsp.py), which says the same every time,
  ;; serves Pascal for the run: the second of two, the first refusing to
  ;; start, so that everything here is also the one giving way to the other.
  (let ((fake (namestring (merge-pathnames "test/fake-lsp.py"
                                           (asdf:system-source-directory :heml.cocoa)))))
    (heml::define-language-server
     "Pascal" (list (list "python3" fake "--refuse") (list "python3" fake))
     :language-id "pascal"))
  ;; A setting for it to ask for.
  (setf heml::*language-server-settings* '(("fake" ("greeting" . "hello from settings"))))
  (with-open-file (out (merge-pathnames "fake.pas" *out*) :direction :output :if-exists :supersede)
    (write-line "program fake;" out)
    (write-line "  wrongthing here" out)
    (write-line "begin end." out))
  (post (list :open (namestring (merge-pathnames "fake.pas" *out*))))
  (settle)
  (flet ((point-line () (line-text (hi::mark-line (hi::current-point)))))
    (check "a language server's error is underlined where it is"
           (wait-until (lambda ()
                         (and (getf (run-font-at "wrongthing here" 1) :underline)
                              (not (getf (run-font-at "wrongthing here" 7) :underline))))
                       30))
    (shot "language-server")
    (post-key #\n "Control")
    (post-key #\c "Control") (post-key #\d "Control")
    (check "C-c C-d says what is wrong there, and what the server says of it, in a popup"
           (wait-until (lambda ()
                         (let ((rows (map 'list #'heml.cocoa::row-text
                                          (heml.cocoa::screen-rows heml.cocoa::*screen*))))
                           (and (find " fake error" rows :test #'search)
                                (find " fake hover text" rows :test #'search))))
                       10))
    (check "the server was given the setting it asked for"
           (find " config: hello from settings"
                 (map 'list #'heml.cocoa::row-text
                      (heml.cocoa::screen-rows heml.cocoa::*screen*))
                 :test #'search))
    (post (list :named "Escape" '()))
    (settle)
    (check "which the next key puts away"
           (null hi::*popup*))
    (post-key #\. "Meta")
    (check "M-. goes to the definition the server names"
           (wait-until (lambda () (equal "begin end." (point-line))) 10))
    (post-key #\? "Meta")
    (check "M-? lists the references it names"
           (wait-until (lambda ()
                         (and (equal "*References*" (hi::buffer-name (hi::current-buffer)))
                              (search "fake.pas:1: program fake;" (buffer-text))
                              (search "fake.pas:3: begin end." (buffer-text))))
                       10))
    (post (list :named "Return" '()))
    (settle)
    (check "and Return visits one"
           (equal "program fake;" (point-line)))
    (post-key #\x "Control") (post-key #\1)
    (post-key #\> "Meta")
    (post-text "fake_")
    (post-key #\i "Control" "Meta")
    (check "the server's completions are in the popup, each with its kind"
           (wait-until (lambda ()
                         (find-if (lambda (row)
                                    (let ((text (heml.cocoa::row-text row)))
                                      (and (search " fake_function " text) (search "function" text :start2 16))))
                                  (heml.cocoa::screen-rows heml.cocoa::*screen*)))
                       10))
    (post (list :named "Return" '()))
    (settle)
    (check "and the one chosen is put in"
           (search "fake_function" (buffer-text)))
    ;; Only the span that changed is sent: the server's copy keeps up, here
    ;; through a line added at the end with a character past the BMP, then
    ;; one changed at the start.
    (check "the change between two texts is one span of the old"
           (flet ((changed (old new)
                    ;; OLD with the span TEXT-CHANGE names replaced.
                    (multiple-value-bind (sl sc el ec text) (heml::text-change old new)
                      (flet ((index (line character)
                               (let ((i 0) (units 0))
                                 (dotimes (k line) (setf i (1+ (position #\Newline old :start i))))
                                 (loop while (< units character)
                                       do (incf units (if (> (char-code (char old i)) #xFFFF) 2 1))
                                          (incf i))
                                 i)))
                        (concatenate 'string (subseq old 0 (index sl sc)) text
                                     (subseq old (index el ec)))))))
             (let ((face (string (code-char #x1F600))))
               (every (lambda (pair) (string= (second pair) (changed (first pair) (second pair))))
                      (list (list "abc" "abXc")
                            (list (format nil "one~%two~%three") (format nil "one~%2~%three"))
                            (list (format nil "one~%two~%") (format nil "one~%"))
                            (list "" "new")
                            (list "same" "same")
                            (list (format nil "a~Ab~%c" face) (format nil "a~Ab~%Xc" face))
                            (list (format nil "aaa~%aaa") (format nil "aaa~%aaa~%aaa")))))))
    (post-key #\> "Meta")
    (post-text (format nil "~%tail ~A end" (code-char #x1F600)))
    (post-key #\< "Meta")
    (post-text "X")
    (post-key #\c "Control") (post-key #\d "Control")
    (check "after edits, the server has the text the buffer has"
           (wait-until (lambda ()
                         (let ((rows (map 'list #'heml.cocoa::row-text
                                          (heml.cocoa::screen-rows heml.cocoa::*screen*)))
                               (lines (uiop:split-string (buffer-text) :separator (string #\Newline))))
                           (and (find (format nil " first: ~A" (first lines)) rows :test #'search)
                                (find (format nil " length: ~D" (length (buffer-text))) rows
                                      :test #'search))))
                       10))
    (post (list :named "Escape" '()))
    (settle)
    (post-key #\d "Control")
    (settle)
    (extended-command "LSP Rename")
    (post-key #\a "Control") (post-key #\k "Control")
    (post-text "renamed
")
    (check "LSP Rename makes the server's edits"
           (wait-until (lambda () (search "renamedgram fake;" (buffer-text))) 10))
    ;; What the server can do about the error: an edit, and a command of
    ;; its own, which asks for its edit to be made.
    (flet ((popup-row-p (text)
             (find text (map 'list #'heml.cocoa::row-text
                             (heml.cocoa::screen-rows heml.cocoa::*screen*))
                   :test #'search)))
      (post-key #\< "Meta")
      (post-key #\n "Control")
      (post-key #\c "Control") (post-key #\a "Control")
      (check "C-c C-a offers what the server can do, in a popup"
             (wait-until (lambda () (and (popup-row-p " 1  Fix the fake error")
                                         (popup-row-p " 2  Run a command")))
                         10))
      (post (list :named "Return" '()))
      (check "choosing a fix makes its edit"
             (wait-until (lambda () (search "  rightthing here" (buffer-text))) 10))
      (post-key #\c "Control") (post-key #\a "Control")
      (wait-until (lambda () (popup-row-p " 2  Run a command")) 10)
      (post-key #\2)
      (check "and choosing a command makes the edit the server asks for"
             (wait-until (lambda () (eql 0 (search "{ done }" (buffer-text)))) 10)))
    ;; What a call takes, shown as it is typed, the argument being typed marked.
    (flet ((signature-font (text)
             ;; The font TEXT is drawn in, in the row showing the signature.
             (let ((row (find-if (lambda (row) (search " fake_function(int a, int b) "
                                                       (heml.cocoa::row-text row)))
                                 (heml.cocoa::screen-rows heml.cocoa::*screen*))))
               (when row
                 (let ((column (search text (heml.cocoa::row-text row))))
                   (loop for (start end . font) in (heml.cocoa::row-runs row)
                         when (and (<= start column) (< column end)) return font))))))
      (post-key #\> "Meta")
      (post-text (format nil "~%fake_function("))
      (check "typing a call's parenthesis shows what it takes, the first argument marked"
             (wait-until (lambda ()
                           (and (equal heml::*popup-selected-font* (signature-font "int a"))
                                (equal heml::*popup-font* (signature-font "int b"))))
                         10))
      (shot "signature")
      (post-text "1,")
      (check "a comma marks the next"
             (wait-until (lambda ()
                           (and (equal heml::*popup-selected-font* (signature-font "int b"))
                                (equal heml::*popup-font* (signature-font "int a"))))
                         10))
      (post-text (format nil "2)~%"))
      (settle)
      (check "and closing the call takes it away"
             (and (null hi::*popup*) (null heml::*signature*))))
    ;; From one error to the next, and the modeline's count of them.
    (post-key #\< "Meta")
    (post-key #\n "Meta")
    (settle)
    (check "M-n goes to the next error and says what it is"
           (and (search "fake error" (buffer-text hi::*echo-area-buffer*))
                (search "thing here" (point-line))))
    (check "the modeline counts the errors"
           (find "(1 error)" (map 'list #'heml.cocoa::row-text
                                  (heml.cocoa::screen-rows heml.cocoa::*screen*))
                 :test #'search))
    (post-key #\c "Control") (post-key #\s "Control")
    (post-key #\a "Control") (post-key #\k "Control")
    (post-text "fake
")
    (check "C-c C-s lists the project's symbols the server finds"
           (wait-until (lambda ()
                         (and (equal "*Symbols*" (hi::buffer-name (hi::current-buffer)))
                              (search "function fake_symbol" (buffer-text))))
                       10))
    (post-key #\x "Control") (post-key #\1)
    (post (list :open (namestring (merge-pathnames "fake.pas" *out*))))
    (settle)
    ;; Laid out by the server as it is saved, when that is asked for.
    (setf (hi::variable-value 'heml::lsp-format-on-save :global) t)
    (post-key #\x "Control") (post-key #\s "Control")
    (check "with LSP Format on Save, saving formats first"
           (wait-until (lambda ()
                         (eql 0 (search "{ formatted }"
                                        (uiop:read-file-string (merge-pathnames "fake.pas" *out*)))))
                       10))
    (setf (hi::variable-value 'heml::lsp-format-on-save :global) nil)
    (extended-command "Outline")
    (check "Outline lists the symbols the server names, with their kinds"
           (wait-until (lambda ()
                         (and (equal "*Outline*" (hi::buffer-name (hi::current-buffer)))
                              (search "function fake_symbol" (buffer-text))
                              (search "  variable inner" (buffer-text))))
                       10))
    (post-key #\x "Control") (post-key #\1)
    (post (list :open (namestring (merge-pathnames "fake.pas" *out*))))
    (settle)
    ;; Left unsaved, and not asked about on the way out.
    (setf (hi::buffer-modified (hi::current-buffer)) nil))

  (language-server-feature-checks)
  (language-server-watch-checks)
  (language-server-kind-checks)
  (debugger-checks)
  (git-checks)
  (selection-checks)
  (lisp-edit-checks)
  (terminal-checks)

  (note "projects")
  (let ((root (merge-pathnames "proj/" *out*)))
    (flet ((file (name &rest lines)
             (let ((path (merge-pathnames name root)))
               (ensure-directories-exist path)
               (with-open-file (out path :direction :output :if-exists :supersede)
                 (dolist (line lines) (write-line line out)))
               path))
           (file-shown-p (name)
             (find name (remove hi::*echo-area-window* hi::*window-list*)
                   :key (lambda (w) (let ((p (hi::buffer-pathname (hi::window-buffer w))))
                                      (and p (file-namestring p))))
                   :test #'equal))
           (current-line-text () (line-text (hi::mark-line (hi::current-point))))
           (current-file () (let ((p (hi::buffer-pathname (hi::current-buffer))))
                              (and p (file-namestring p)))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)
      (ensure-directories-exist (merge-pathnames ".git/" root))
      (file "src/a.c" "int a(void) {" "  return 1;" "}")
      (file "src/b.c" "int b(void) {" "  return 2;" "}")
      (file "README.md" "# proj")
      (file ".heml-project"
            "(:name \"Proj X\" :ignore (\"README.md\") :variables ((\"Fill Column\" . 42)))")
      (post-key #\x "Control") (post-key #\1)
      (post (list :open (namestring (merge-pathnames "src/a.c" root))))
      (settle)
      (check "a project's file names the project in its modeline, as its settings name it"
             (not (eq :none (row-runs-containing "[Proj X]"))))
      (check "its settings' variables are set in its buffers"
             (eql 42 (hi::variable-value 'heml::fill-column :buffer (hi::current-buffer))))
      ;; Asked of the editor's thread: listing files runs a program, and so
      ;; may the editor at any moment, which two threads must not do at once.
      (setf *project-files* :unknown)
      (post (list :command "Smoke Project Files"))
      (check "and its files are found, less those its settings ignore"
             (wait-until (lambda ()
                           (and (listp *project-files*)
                                (member "src/a.c" *project-files* :test #'equal)
                                (not (member "README.md" *project-files* :test #'equal))))
                         20))
      (check "text finds files with its characters in order, the best first"
             (equal "src/tree-sitter-modes.lisp"
                    (first (heml::fuzzy-file-matches
                            "tsm" '("test/smoke.lisp" "notes/tasks-more.md"
                                    "src/tree-sitter-modes.lisp")))))
      (post-key #\x "Control") (post-key #\p) (post-key #\f)
      (post-text "sbc
")
      (settle)
      (check "C-x p f finds a file from text in its name"
             (wait-until (lambda () (equal "b.c" (current-file))) 10))
      (post-key #\x "Control") (post-key #\p) (post-key #\f)
      (post-text ".c
")
      (settle)
      (check "and lists the files when several have it"
             (wait-until (lambda ()
                           (and (equal "*Project Files*" (hi::buffer-name (hi::current-buffer)))
                                (search "src/a.c" (buffer-text)) (search "src/b.c" (buffer-text))))
                         10))
      (post-key #\q)
      (post-key #\x "Control") (post-key #\p) (post-key #\g)
      (post-key #\a "Control") (post-key #\k "Control")
      (post-text "return
")
      (check "C-x p g searches the project, from its root"
             (wait-until (lambda () (and (search "Grep finished: 2 results." (buffer-text))
                                         (search (namestring root) (buffer-text))))))
      ;; A session: a.c at its second line, beside b.c.
      (post-key #\x "Control") (post-key #\1)
      (post (list :open (namestring (merge-pathnames "src/a.c" root))))
      (settle)
      (post-key #\n "Control")
      (post-key #\x "Control") (post-key #\3)
      (post-key #\x "Control") (post-key #\p) (post-key #\f)
      (post-text "src/b.c
")
      (settle)
      (extended-command "Save Project Session")
      (post-key #\x "Control") (post-key #\1)
      (extended-command "Kill Project Buffers")
      (post-key #\y)
      ;; An :open is taken at once, ahead of keys still queued: wait for them.
      (settle)
      (settle)
      (check "Kill Project Buffers kills them"
             (not (find "a.c" hi::*buffer-list* :key #'hi::buffer-name :test #'search)))
      (post (list :open (namestring (merge-pathnames "README.md" root))))
      (settle)
      (extended-command "Restore Project Session")
      (settle)
      (check "Restore Project Session reopens the files, in their windows"
             (and (file-shown-p "a.c") (file-shown-p "b.c")
                  (= 2 (length (remove hi::*echo-area-window* hi::*window-list*)))))
      (check "with their points"
             (let ((w (file-shown-p "a.c")))
               (and w (equal "  return 1;"
                             (line-text (hi::mark-line (hi::window-point w)))))))
      ;; A window on one of the project's directories comes back too.
      (post-key #\x "Control") (post-key #\p) (post-key #\d)
      (settle)
      (extended-command "Save Project Session")
      (post-key #\x "Control") (post-key #\1)
      (extended-command "Kill Project Buffers")
      (post-key #\y)
      ;; An :open is taken at once, ahead of keys still queued: wait for them.
      (settle)
      (post (list :open (namestring (merge-pathnames "README.md" root))))
      (settle)
      (extended-command "Restore Project Session")
      (settle)
      (check "a Dired window of the project is reopened with its session"
             (find "Dired" (remove hi::*echo-area-window* hi::*window-list*)
                   :key (lambda (w) (hi::buffer-major-mode (hi::window-buffer w)))
                   :test #'equal))
      ;; Back to the two files side by side, for what follows.
      (post-key #\x "Control") (post-key #\1)
      (post (list :open (namestring (merge-pathnames "src/a.c" root))))
      (settle)
      (post-key #\x "Control") (post-key #\3)
      (post-key #\x "Control") (post-key #\p) (post-key #\f)
      (post-text "src/b.c
")
      (settle)
      (extended-command "Save Project Session")
      ;; Switching away and back reopens it without asking.
      (extended-command "Kill Project Buffers")
      (post-key #\y)
      ;; An :open is taken at once, ahead of keys still queued: wait for them.
      (settle)
      (post-key #\x "Control") (post-key #\1)
      (post (list :open (namestring (merge-pathnames "README.org"
                                                     (asdf:system-source-directory :heml.cocoa)))))
      (settle)
      (post-key #\x "Control") (post-key #\p) (post-key #\p)
      (post (list :named "Return" '()))
      (settle)
      (check "C-x p p back to the project reopens its session"
             (and (file-shown-p "a.c") (file-shown-p "b.c")))
      ;; The project's compile commands are remembered, for M-p at the prompt.
      (dolist (command '("echo one" "echo two"))
        (post-key #\x "Control") (post-key #\p) (post-key #\c)
        (post-key #\a "Control") (post-key #\k "Control")
        (post-text (format nil "~A~%" command))
        (wait-until (lambda () (search "Compilation finished" (buffer-text))) 20)
        (settle))
      (check "C-x p c keeps the project's commands"
             (equal '("echo two" "echo one")
                    (heml::project-property (namestring root) :compile-history)))
      (post-key #\x "Control") (post-key #\p) (post-key #\c)
      (post-key #\a "Control") (post-key #\k "Control")
      (post-key #\p "Meta") (post-key #\p "Meta")
      (settle)
      (check "and M-p at its prompt goes back through them"
             (search "echo one" (buffer-text hi::*echo-area-buffer*)))
      (post-key #\g "Control")
      (settle)
      (post-key #\x "Control") (post-key #\1)
      ;; Its shell's inputs come back with its session.
      (post (list :open (namestring (merge-pathnames "src/a.c" root))))
      (settle)
      (post-key #\x "Control") (post-key #\p) (post-key #\s)
      (settle)
      (post-text "echo hist-$((40+2))
")
      (wait-until (lambda () (search "hist-42" (buffer-text))) 20)
      (extended-command "Save Project Session")
      (settle)
      (extended-command "Kill Project Buffers")
      (post-key #\y)
      (settle)
      (post (list :open (namestring (merge-pathnames "src/a.c" root))))
      (settle)
      (post-key #\x "Control") (post-key #\p) (post-key #\s)
      (settle)
      (post-key #\p "Meta")
      (check "a project's shell remembers what was typed to it before"
             (wait-until (lambda () (search "echo hist-$((40+2))" (buffer-text))) 10))
      (post-key #\i "Meta")
      (settle)
      ;; Replacing through the project: both files have a return.
      (post-key #\x "Control") (post-key #\b)
      (post-key #\a "Control") (post-key #\k "Control")
      (post-text "a.c
")
      (settle)
      (post-key #\x "Control") (post-key #\p) (post-key #\r)
      (post-key #\a "Control") (post-key #\k "Control")
      (post-text "return
")
      (post-text "give_back
")
      (post-key #\y)
      (post-key #\y)
      (check "C-x p r replaces a string in every file of the project that has it"
             (wait-until (lambda ()
                           (and (equal "*Project Replace*" (hi::buffer-name (hi::current-buffer)))
                                (search "src/a.c:2: give_back 1;" (buffer-text))
                                (search "src/b.c:2: give_back 2;" (buffer-text))))
                         20))
      (check "and saves them"
             (search "give_back 1;" (uiop:read-file-string (merge-pathnames "src/a.c" root))))
      ;; Recent files: the one before this is offered first.
      (post-key #\x "Control") (post-key #\1)
      (post (list :open (namestring (merge-pathnames "src/b.c" root))))
      (post (list :open (namestring (merge-pathnames "src/a.c" root))))
      (settle)
      (post-key #\x "Control") (post-key #\r "Control")
      (post (list :named "Return" '()))
      (check "C-x C-r offers the files visited lately, the last first"
             (wait-until (lambda () (equal "b.c" (current-file))) 10))
      (extended-command "Kill Project Buffers")
      (post-key #\y)
      ;; An :open is taken at once, ahead of keys still queued: wait for them.
      (settle)
      (post-key #\x "Control") (post-key #\1)
      ;; Back to where the checks after these expect to be.
      (post (list :open (namestring (merge-pathnames "words.txt" *out*))))
      (settle)))

  (note "menus")
  (choose-menu-item "View" "Split Window")
  (settle)
  (check "View > Split Window splits it"
         (= 2 (length (hi::buffer-windows (hi::current-buffer)))))
  (choose-menu-item "View" "Delete Window")
  (settle)
  (check "View > Delete Window deletes one"
         (= 1 (length (hi::buffer-windows (hi::current-buffer)))))

  (note "side by side")
  (choose-menu-item "View" "Split Window Side by Side")
  (settle)
  (let* ((hunks (sort (mapcar #'hi::window-hunk (hi::buffer-windows (hi::current-buffer)))
                      #'< :key #'hi::device-hunk-column))
         (left (first hunks))
         (right (second hunks)))
    (check "View > Split Window Side by Side puts two windows side by side"
           (and (= 2 (length hunks))
                (= (hi::device-hunk-position left) (hi::device-hunk-position right))
                (= (hi::device-hunk-column right)
                   (+ 1 (hi::device-hunk-column left) (hi::device-hunk-width left)))))
    (check "a bar divides them"
           (let ((text (heml.cocoa::row-text
                        (svref (heml.cocoa::screen-rows heml.cocoa::*screen*) 0))))
             (char= #\│ (char text (hi::device-hunk-width left)))))
    (shot "side-by-side")
    (mouse :down 2 0) (mouse :up 2 0)
    (settle)
    (check "the screen is not cleared again after the split has been drawn"
           (not hi::*screen-image-trashed*))
    (check "a click in the left window goes to it"
           (eq (hi::window-hunk hi::*current-window*) left))
    (let ((column (+ 3 (hi::device-hunk-column right))))
      (mouse :down column 0) (mouse :up column 0))
    (settle)
    (check "a click in the right window lands where it is in that window"
           (and (eq (hi::window-hunk hi::*current-window*) right)
                (= (point-column) 3)))
    (post-key #\u "Control") (post-key #\8) (post-key #\x "Control") (post-key #\})
    (settle)
    (check "C-x } widens the window"
           (> (hi::device-hunk-width right) (+ 4 (hi::device-hunk-width left))))
    (choose-menu-item "View" "Balance Windows")
    (settle)
    (check "View > Balance Windows makes them as wide as each other"
           (<= (abs (- (hi::device-hunk-width right) (hi::device-hunk-width left))) 1))
    (check "the bar has a resize cursor"
           (let ((bar (+ (hi::device-hunk-column left) (hi::device-hunk-width left))))
             (find-if (lambda (border)
                        (and (eq (first border) :columns) (= (second border) bar)))
                      (heml.cocoa::screen-shown-borders heml.cocoa::*screen*))))
    (let ((bar (+ (hi::device-hunk-column left) (hi::device-hunk-width left)))
          (before (hi::device-hunk-width left))
          (point (point-column)))
      (mouse :down bar 3) (mouse :drag (+ bar 3) 3) (mouse :drag (+ bar 6) 3) (mouse :up (+ bar 6) 3)
      (settle)
      (check "dragging the bar between them widens the window on its left"
             (= (hi::device-hunk-width left) (+ before 6)))
      (check "and leaves point alone"
             (and (eq (hi::window-hunk hi::*current-window*) right)
                  (= (point-column) point)))))
  (choose-menu-item "View" "Delete Window")
  (settle)
  (check "deleting one of them leaves one window across the screen"
         (let ((windows (hi::buffer-windows (hi::current-buffer))))
           (and (= 1 (length windows))
                (= (hi::device-hunk-width (hi::window-hunk (first windows)))
                   (heml.cocoa::screen-columns heml.cocoa::*screen*)))))
  (check "the view shows the frame the editor finished"
         (let ((screen heml.cocoa::*screen*))
           (equalp (map 'list #'heml.cocoa::row-text (heml.cocoa::screen-shown-rows screen))
                   (map 'list #'heml.cocoa::row-text (heml.cocoa::screen-rows screen)))))
  (choose-menu-item "View" "Split Window")
  (settle)
  (choose-menu-item "View" "Split Window Side by Side")
  (settle)
  (let ((current hi::*current-window*))
    (choose-menu-item "View" "Delete Other Windows")
    (settle)
    (check "View > Delete Other Windows leaves the current window, filling the screen"
           (let ((windows (remove hi::*echo-area-window* hi::*window-list*)))
             (and (equal windows (list current))
                  (= (hi::device-hunk-width (hi::window-hunk current))
                     (heml.cocoa::screen-columns heml.cocoa::*screen*))))))
  (choose-menu-item "View" "Split Window")
  (settle)
  (let* ((bottom (hi::window-hunk hi::*current-window*))
         (top (hi::device-hunk-previous bottom))
         (modeline (hi::device-hunk-position top))
         (before (hi::device-hunk-height top)))
    (mouse :down 5 modeline) (mouse :drag 5 (+ modeline 2)) (mouse :drag 5 (+ modeline 4))
    (mouse :up 5 (+ modeline 4))
    (settle)
    (check "dragging a modeline down makes its window taller"
           (and (= (hi::device-hunk-height top) (+ before 4))
                (= (hi::device-hunk-position top) (+ modeline 4))))
    (check "and selects that window"
           (eq (hi::window-hunk hi::*current-window*) top)))
  (choose-menu-item "View" "Delete Other Windows")
  (settle)
  (press-menu #\b)
  (settle)
  (check "Cmd-B prompts for a buffer" (eq hi::*current-window* hi::*echo-area-window*))
  (shot "switch-buffer")
  (post-key #\g "Control")
  (settle)

  (note "fonts")
  (press-menu #\=) (settle)
  (check "Cmd-= makes the font bigger" (= heml.cocoa:*font-size* 14))
  (shot "bigger")
  (press-menu #\-) (press-menu #\-) (settle)
  (check "Cmd-- makes it smaller" (= heml.cocoa:*font-size* 12))
  (press-menu #\0) (settle)
  (check "Cmd-0 goes back to the default" (= heml.cocoa:*font-size* 13))

  (note "dired")
  ;; A directory of its own: flagging a file must not risk a real one.
  (let ((directory (merge-pathnames "dired/" *out*)))
    (ensure-directories-exist (merge-pathnames "a-directory/" directory))
    (with-open-file (out (merge-pathnames "a-file.txt" directory)
                         :direction :output :if-exists :supersede)
      (write-line "x" out))
    (check "the Dired menu is hidden outside Dired" (menu-hidden-p "Dired"))
    (post (list :command "Dired" (namestring directory)))
    (settle)
    (check "and shown in it" (not (menu-hidden-p "Dired")))
    (choose-menu-item "Dired" "Mark")
    (settle)
    (check "Dired > Mark marks the file under point"
           (not (eq :none (row-runs-containing "* "))))
    (choose-menu-item "Dired" "Unmark All")
    (settle)
    (check "Dired lists a directory, with a header"
           (and (equal "Dired" (hi::buffer-major-mode (hi::current-buffer)))
                (not (eq :none (row-runs-containing "(2 entries, ")))))
    (check "a directory is blue and bold"
           (equal '(:fg 4 :bold t) (run-font-at "a-directory/" 0)))
    ;; C-d flags the file on point's line and C-u clears it, neither moving.
    (post-key #\d "Control")
    (settle)
    (check "a file flagged for deletion is red"
           (eql 1 (run-font-at "D " 0)))
    (post-key #\u "Control")
    (settle)
    ;; A double click on a file visits it.
    (let* ((rows (heml.cocoa::screen-shown-rows heml.cocoa::*screen*))
           (line (position-if (lambda (row) (search "a-file.txt" (heml.cocoa::row-text row))) rows))
           (column (and line (search "a-file.txt" (heml.cocoa::row-text (svref rows line))))))
      (when line
        (mouse :down (+ column 2) line) (mouse :up (+ column 2) line)
        (mouse :down (+ column 2) line :clicks 2) (mouse :up (+ column 2) line :clicks 2)
        (settle))
      (check "a double click in Dired visits the file"
             (let ((pathname (hi::buffer-pathname (hi::current-buffer))))
               (and pathname (equal "a-file.txt" (file-namestring pathname)))))
      (check "and the Dired menu is hidden again there" (menu-hidden-p "Dired")))
    (post-key #\x "Control") (post-key #\k)
    (settle)
    (post (list :named "Return" (quote ())))
    (settle)
    (check "and nothing was deleted"
           (and (probe-file (merge-pathnames "a-file.txt" directory))
                (probe-file (merge-pathnames "a-directory/" directory)))))

  (note "menus from lisp")
  (heml-interface:define-menu "Smoke" ()
    ("Split Side by Side" "Split Window Horizontally"))
  (settle)
  (check "define-menu puts a menu in the menu bar" (not (menu-hidden-p "Smoke")))
  (choose-menu-item "Smoke" "Split Side by Side")
  (settle)
  (check "and its item runs its command"
         (= 2 (length (remove hi::*echo-area-window* hi::*window-list*))))
  (heml-interface:add-menu-item "Smoke" '("One Window" "Delete Other Windows"))
  (settle)
  (choose-menu-item "Smoke" "One Window")
  (settle)
  (check "add-menu-item adds an item that works"
         (= 1 (length (remove hi::*echo-area-window* hi::*window-list*))))
  (heml-interface:remove-menu "Smoke")
  (settle)
  (check "remove-menu takes it away"
         (main (cffi:null-pointer-p
                (objc:invoke (objc:invoke (objc.runloop:shared-application) "mainMenu")
                             "itemWithTitle:" "Smoke"))))

  (note "bufed")
  ;; The list is most recent first: a buffer just visited, then left, is
  ;; near its top, on the screen however many buffers there are.
  (post (list :open (namestring (merge-pathnames "opened.c" *out*))))
  (post (list :open (namestring (merge-pathnames "words.txt" *out*))))
  (settle)
  (post (list :command "Bufed"))
  (settle)
  (let* ((rows (heml.cocoa::screen-shown-rows heml.cocoa::*screen*))
         (line (position-if (lambda (row) (search "opened.c" (heml.cocoa::row-text row))) rows))
         (column (and line (search "opened.c" (heml.cocoa::row-text (svref rows line))))))
    (check "Bufed lists the buffers, under a header"
           (and line (not (eq :none (row-runs-containing "Buffers  (")))))
    (when line
      (mouse :down (+ column 2) line) (mouse :up (+ column 2) line)
      (mouse :down (+ column 2) line :clicks 2) (mouse :up (+ column 2) line :clicks 2)
      (settle))
    (check "a double click in Bufed visits the buffer"
           (let ((pathname (hi::buffer-pathname (hi::current-buffer))))
             (and pathname (equal "opened.c" (file-namestring pathname))))))

  (note "text styles")
  (let ((file (merge-pathnames "styles.txt" *out*)))
    (with-open-file (out file :direction :output :if-exists :supersede)
      (write-line "plain italic under both" out))
    (post (list :open (namestring file)))
    (settle)
    ;; The editor is idle, waiting for input, while the marks are made.
    (let ((line (hi::mark-line (hi::buffer-start-mark (hi::current-buffer)))))
      (hi::font-mark line 6 '(:italic t))
      (hi::font-mark line 12 0)
      (hi::font-mark line 13 '(:underline t))
      (hi::font-mark line 18 0)
      (hi::font-mark line 19 '(:bold t :italic t :underline t)))
    (post-key #\l "Control")
    (settle)
    (check "styles reach the screen"
           (and (equal '(:italic t) (run-font-at "plain italic" 6))
                (equal '(:underline t) (run-font-at "plain italic" 13))))
    (check "italic text is drawn italic, or slanted where the font has none"
           (main (let* ((attributes (heml.cocoa::text-attributes (display) "textColor" nil t))
                        (font (objc:invoke attributes "objectForKey:" "NSFont")))
                   (or (logtest 1 (objc:invoke (objc:invoke font "fontDescriptor") "symbolicTraits"))
                       (not (cffi:null-pointer-p
                             (objc:invoke attributes "objectForKey:" "NSObliqueness")))))))
    (check "an underline is drawn under underlined text, and not under plain"
           (let* ((row (position-if (lambda (row) (search "plain italic under" (heml.cocoa::row-text row)))
                                    (heml.cocoa::screen-shown-rows heml.cocoa::*screen*)))
                  (display (display)))
             (and row
                  (main
                    (let* ((view (view))
                           (bounds (objc:invoke view "bounds"))
                           (rep (objc:invoke view "bitmapImageRepForCachingDisplayInRect:" bounds))
                           (scale (/ (objc:invoke rep "pixelsWide") (aref bounds 2)))
                           (y (floor (* scale (+ (heml.cocoa::cell-y display row)
                                                 (heml.cocoa::display-char-ascent display) 1)))))
                      (objc:invoke view "cacheDisplayInRect:toBitmapImageRep:" bounds rep)
                      (flet ((ink (column)
                               (let* ((x (floor (* scale (+ (heml.cocoa::cell-x display column)
                                                            (/ (heml.cocoa::display-char-width display) 2)))))
                                      (color (objc:invoke (objc:invoke rep "colorAtX:y:" x y)
                                                          "colorUsingColorSpace:"
                                                          (objc:invoke "NSColorSpace" "sRGBColorSpace"))))
                                 (+ (objc:invoke color "redComponent")
                                    (objc:invoke color "greenComponent")
                                    (objc:invoke color "blueComponent")))))
                        ;; A space under "under" (column 15, the "d") and in
                        ;; "plain" (column 2, the "a") at the underline's row.
                        (> (abs (- (ink 15) (ink 2))) 0.5)))))))
    (shot "styles"))

  (note "shell")
  (extended-command "Shell")
  (settle)
  ;; Tab completes a file name, from the shell's directory.
  (post-text "ls gre")
  (post (list :named "Tab" '()))
  (settle)
  (post-text "e")
  (post (list :named "Return" '()))
  (check "in a shell, Tab completes a file's name from a popup"
         (wait-until (lambda () (search "ls greet" (buffer-text))) 10))
  (post (list :named "Return" '()))
  (settle)
  (post-text "echo smoke-$((6*7)); printf '\\033[31mred\\033[0m plain\\n'
")
  (check "a shell runs in a buffer"
         (wait-until (lambda () (search "smoke-42" (buffer-text)))))
  (check "colour codes from a shell are not text"
         (wait-until (lambda () (search (format nil "~%red plain") (buffer-text)))))
  (check "a colour code becomes a font"
         (let ((line (hi::mark-line (hi::buffer-start-mark (hi::current-buffer)))))
           (loop while line
                 thereis (and (search "red plain" (line-text line))
                              (some (lambda (m) (and (hi::fast-font-mark-p m)
                                                     (eql (hi::font-mark-font m) 1)))
                                    (hi::line-marks line)))
                 do (setf line (hi::line-next line)))))
  (post-text "echo first-in
")
  (wait-until (lambda () (<= 2 (count-matches "first-in" (buffer-text)))))
  (post-key #\c "Control") (post-key #\p "Control")
  (settle)
  (check "C-c C-p goes back to the last input"
         (search "echo first-in" (line-text (hi::mark-line (hi::current-point)))))
  (post (list :named "Return" '()))
  (check "Return on it sends it again"
         (wait-until (lambda () (<= 4 (count-matches "first-in" (buffer-text))))))
  (shot "shell")

  (note "slave")
  (extended-command "Start Slave Thread")
  (settle)
  (wait-until (lambda () (search "CL-USER>" (buffer-text))))
  (post-text "(* 6 7)
")
  (check "a slave Lisp evaluates"
         (wait-until (lambda () (search (format nil "~%42") (buffer-text)))))
  (shot "slave")

  (note "lists")
  (extended-command "List Slaves")
  (settle)
  (check "List Slaves shows the slave, idle"
         (and (equal "Slave-List" (hi::buffer-major-mode (hi::current-buffer)))
              (search "idle" (buffer-text))))
  (post-key #\c "Control") (post-key #\?)
  (post-key #\a "Control") (post-key #\k "Control")
  (post-text "mapcar
")
  (check "Slave Apropos lists the symbols"
         (wait-until (lambda () (and (equal "Apropos" (hi::buffer-major-mode (hi::current-buffer)))
                                     (search "MAPCAR" (buffer-text))))))
  (settle)
  (check "with what each names, coloured"
         (and (search "Function:" (buffer-text))
              (eql 6 (run-font-at "  Function:" 2))))
  (post (list :command "Smoke Debug Buffer"))
  (settle)
  (check "the debugger lists frames, coloured"
         (and (equal "Debug" (hi::buffer-major-mode (hi::current-buffer)))
              (eql 2 (run-font-at "  0: (FOO 1)" 2))))
  (post (list :named "Return" '()))
  (settle)
  (check "Return shows a frame's locals"
         (search "X = 1" (buffer-text)))
  (post (list :command "Smoke Completions"))
  (settle)
  (check "a completion list shows what the completions share and the next character"
         (and (eql 7 (run-font-at "mapcar" 0))
              (equal (quote (:fg 4 :bold t)) (run-font-at "mapcar" 4))))

  (note "file names and the environment")
  (let ((home (string-right-trim "/" (namestring (user-homedir-pathname)))))
    (check "~ and ~user start a file name with a home directory"
           (and (equal (heml-ext:expand-file-name "~/notes") (concatenate 'string home "/notes"))
                (equal (heml-ext:expand-file-name (format nil "~~~A/.profile" (uiop:getenv "USER")))
                       (concatenate 'string home "/.profile"))
                (equal (heml-ext:expand-file-name "~") home)))
    (check "a ~ after a /, or a second /, starts the name again"
           (and (equal (heml-ext:expand-file-name "/some/dir/~/notes")
                       (concatenate 'string home "/notes"))
                (equal (heml-ext:expand-file-name "/some/dir//etc/hosts") "/etc/hosts")))
    (check "and other names are left as they are"
           (and (equal (heml-ext:expand-file-name "/tmp/backup~") "/tmp/backup~")
                (equal (heml-ext:expand-file-name "~no-such-user-here/x") "~no-such-user-here/x")
                (equal (heml-ext:expand-file-name "relative/file.txt") "relative/file.txt")))
    ;; Typed at the file prompt, after the directory it offers.
    (let ((out (namestring *out*)))
      (when (eql 0 (search home out))
        (post-key #\x "Control") (post-key #\f "Control")
        (post-text (format nil "~~~Agrep.txt~%" (subseq out (length home))))
        (check "C-x C-f takes a ~/ name typed after the directory offered"
               (wait-until (lambda ()
                             (equal (hi::buffer-pathname (hi::current-buffer))
                                    (merge-pathnames "grep.txt" *out*)))
                           10)))))
  ;; A stand-in for the user's shell, which says what its environment is.
  (let ((shell (merge-pathnames "fake-shell" *out*))
        (real (uiop:getenv "SHELL")))
    (with-open-file (out shell :direction :output :if-exists :supersede)
      (format out "#!/bin/sh~%echo 'noise from a startup file'~%echo 'PATH=/fake/bin:/usr/bin'~%echo 'HEML_SMOKE_VARIABLE=from the shell'~%"))
    (sb-posix:chmod (namestring shell) #o755)
    (setf (uiop:getenv "SHELL") (namestring shell))
    (unwind-protect
         (progn
           (check "the login shell's environment is read"
                  (equal (hi::shell-environment '("PATH" "HEML_SMOKE_VARIABLE"))
                         '(("PATH" . "/fake/bin:/usr/bin")
                           ("HEML_SMOKE_VARIABLE" . "from the shell"))))
           (hi::import-shell-environment '("HEML_SMOKE_VARIABLE"))
           (check "and its variables are set in the editor's process"
                  (equal "from the shell" (uiop:getenv "HEML_SMOKE_VARIABLE")))
           (check "a shell is the user's own, not /bin/bash"
                  (eql 0 (search (namestring shell) (heml::get-command-line)))))
      (setf (uiop:getenv "SHELL") (or real "/bin/sh"))))

  (note "settings")
  ;; A home of its own, so that the real init file is not touched.
  (let ((home (merge-pathnames "home/" *out*)))
    (uiop:delete-directory-tree home :validate t :if-does-not-exist :ignore)
    (ensure-directories-exist home)
    (sb-posix:setenv "HOME" (namestring home) 1)
    (sb-posix:unsetenv "XDG_CONFIG_HOME")
    (choose-menu-item "Heml" "Settings…")
    (settle)
    (check "Settings… opens the init file, ~/.config/heml/init.lisp"
           (wait-until (lambda ()
                         (equal (hi::buffer-pathname (hi::current-buffer))
                                (merge-pathnames ".config/heml/init.lisp" home)))
                       10))
    (check "in a directory made for it"
           (probe-file (merge-pathnames ".config/heml/" home))))

  (note "quitting")
  (post :quit)
  ;; Answer whatever Save All Files and Exit asks.
  (loop repeat 10
        while heml.cocoa::*editor-running-p*
        do (sleep 1)
           (when heml.cocoa::*editor-running-p* (post-key #\n)))
  (check "the editor exits" (not heml.cocoa::*editor-running-p*))
  (setf *finished* t))

(defun count-matches (text string)
  (loop with start = 0
        for at = (search text string :start2 start)
        while at count t do (setf start (1+ at))))

;;; What the lists would show without a slave in the debugger or completing.
(heml::defcommand "Smoke Debug Buffer" (p) "" ""
  (declare (ignore p))
  (heml::make-debug-buffer nil '(("(FOO 1)" nil (("X" . "1")) nil) ("(BAR)" nil nil nil))
                           "SBCL" "smoke"))

(defvar *project-files* :unknown)

(heml::defcommand "Smoke Project Files" (p) "" ""
  (declare (ignore p))
  (setf *project-files*
        (heml::project-files (heml::current-project-root))))

(heml::defcommand "Smoke Completions" (p) "" ""
  (declare (ignore p))
  (heml::make-completelist-buffer
   (mapcar #'heml::make-completelist-entry '("mapcar" "mapcan" "mapc"))))

(defvar *driver*)

(setf *driver*
 (bt:make-thread
 (lambda ()
   (handler-case (run)
     (error (condition)
       (push (format nil "the run itself: ~A" condition) *failures*)
       (note "  FAIL  the run itself: ~A" condition)
       (post :quit))))
 :name "smoke"))

(bt:make-thread
 (lambda ()
   (sleep 600)
   (note "smoke: no result after ten minutes")
   (sb-ext:exit :code 2 :abort t))
 :name "smoke watchdog")

(heml:heml nil :backend-type :cocoa :load-user-init nil)
;; The driver makes its last check after the editor has exited.
(bt:join-thread *driver*)

(note "~&~D check~:P, ~D failed~@[: ~{~A~^; ~}~]~%pictures in ~A"
      *checks* (length *failures*) (reverse *failures*) (namestring *out*))
(sb-ext:exit :code (if (and *finished* (null *failures*)) 0 1) :abort t)
