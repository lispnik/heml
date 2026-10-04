;;;; scripts/demo/git.lisp -- `make demo-git': Git in the editor -- the
;;;; fringe's marks, the status buffer, staging, committing, the log, a
;;;; commit's diff and the blame -- against a repository made for it in
;;;; build/demo/git/.

(load (merge-pathnames "driver.lisp" *load-truename*))
(in-package :heml-demo)

(defvar *repo* (fresh-directory "git"))

(write-file (merge-pathnames "README.md" *repo*)
            "# Inventory" ""
            "A small inventory, kept in Git." ""
            "## Fruit" ""
            "- apples: 3" "- pears: 5" ""
            "## Vegetables" ""
            "- leeks: 4" "- onions: 10" "")
(sh *repo* "git init -q -b main && git config user.email demo@example.com && git config user.name 'Sam Rivera' && git add README.md && git commit -q -m 'Start the inventory'")
(write-file (merge-pathnames "README.md" *repo*)
            "# Inventory" ""
            "A small inventory, kept in Git." ""
            "## Fruit" ""
            "- apples: 3" "- pears: 5" "- plums: 2" ""
            "## Vegetables" ""
            "- leeks: 4" "- onions: 10" ""
            "## Notes" ""
            "Counted on Monday.")
(sh *repo* "git add README.md && GIT_AUTHOR_NAME='Alex Chen' git commit -q -m 'Count the plums, and say when'")
(write-file (merge-pathnames "TODO.txt" *repo*) "Count the herbs.")

(defun steps ()
  (caption "A file Git tracks: lines that differ from the last commit are marked in the fringe")
  (open-file (merge-pathnames "README.md" *repo*))
  (pause 1)
  ;; Onions, line 14, taken out; figs added after plums, line 9; pears,
  ;; line 8, changed -- from the bottom up, so the numbers hold.
  (keys '(#\< "Meta"))
  (dotimes (i 13) (keys '(#\n "Control")))
  (keys '(#\a "Control") '(#\k "Control") '(#\k "Control"))
  (pause 1.2)
  (keys '(#\< "Meta"))
  (dotimes (i 8) (keys '(#\n "Control")))
  (keys '(#\e "Control") "Return")
  (type-text "- figs: 1")
  (pause 1.2)
  (keys '(#\< "Meta"))
  (dotimes (i 7) (keys '(#\n "Control")))
  (keys '(#\e "Control") "Backspace")
  (type-text "7")
  (wait-for "▎- pears: 7" 5)
  (caption "Green: added.  Blue: changed.  Red: lines taken out.  As you type, before saving")
  (pause 3)
  (keys '(#\x "Control") '(#\s "Control"))

  (caption "C-x g: the repository's status, as Magit shows it")
  (keys '(#\x "Control") #\g)
  (wait-for "Unstaged changes" 10)
  (pause 2.5)
  (caption "Tab shows a file's hunks; s stages a hunk, or a file, or a whole section")
  (keys '(#\< "Meta") #\n #\n #\n #\n)
  (pause 0.5)
  (keys "Tab")
  (wait-for "@@" 5)
  (pause 2)
  (keys #\n)
  (pause 0.6)
  (keys #\s)
  (wait-for "Staged changes" 5)
  (pause 2.5)

  (caption "c commits: the message is written in a buffer of its own; C-c C-c commits")
  (keys #\c)
  (wait-for "commit's message" 5)
  (pause 1)
  (type-text "Restock the fruit, and sell the onions")
  (pause 1.5)
  (keys '(#\c "Control") '(#\c "Control"))
  (wait-for "Restock the fruit" 5)
  (pause 2.5)

  (caption "C-x v l: the file's commits.  Return shows one: its message and its diff")
  (keys #\q)
  (pause 0.5)
  (keys '(#\x "Control") #\v #\l)
  (wait-for "Start the inventory" 5)
  (pause 1.5)
  (keys '(#\< "Meta") '(#\n "Control") "Return")
  (wait-for "Count the plums" 5)
  (pause 3)
  (caption "In a diff, n and p go by hunks, and Return visits the line in the file")
  (keys '(#\x "Control") #\o)
  (keys #\n)
  (pause 1)
  (keys '(#\n "Control") '(#\n "Control") '(#\n "Control") '(#\n "Control") '(#\n "Control"))
  (pause 0.8)
  (keys "Return")
  (pause 2)
  (keys '(#\x "Control") #\1)

  (caption "C-x v g: who last changed each line, and when.  Return shows the commit")
  (keys '(#\x "Control") #\v #\g)
  (wait-for "Sam Rivera" 10)
  (pause 6))

(run-demo "git" #'steps)
