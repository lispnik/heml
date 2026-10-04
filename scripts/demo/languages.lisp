;;;; scripts/demo/languages.lisp -- `make demo-languages': a language server
;;;; at work -- clangd, on a small C project made in build/demo/shapes/ --
;;;; and tree-sitter's folding.

(load (merge-pathnames "driver.lisp" *load-truename*))
(in-package :heml-demo)

(defvar *project* (fresh-directory "shapes"))

(write-file (merge-pathnames ".heml-project" *project*) "(:name \"shapes\")")
(write-file (merge-pathnames "shapes.h" *project*)
            "#ifndef SHAPES_H" "#define SHAPES_H" ""
            "/* The area of a rectangle W wide and H high. */"
            "int rectangle_area(int w, int h);" ""
            "/* The distance around a rectangle W wide and H high. */"
            "int rectangle_perimeter(int w, int h);" ""
            "#endif")
(write-file (merge-pathnames "shapes.c" *project*)
            "#include \"shapes.h\"" ""
            "int rectangle_area(int w, int h) {" "    return w * h;" "}" ""
            "int rectangle_perimeter(int w, int h) {"
            "    int sides = w + h;" "    return 2 * sides;" "}")
;; So that clangd indexes the whole project, and finds a definition in a
;; file not yet opened.
(write-file (merge-pathnames "compile_commands.json" *project*)
            (format nil "[~{~A~^,~%~}]"
                    (loop for file in '("main.c" "shapes.c")
                          collect (format nil "{\"directory\": \"~A\", \"file\": \"~A\", \"arguments\": [\"cc\", \"-c\", \"~A\"]}"
                                          (namestring *project*) file file))))
(write-file (merge-pathnames "main.c" *project*)
            "#include <stdio.h>" "#include \"shapes.h\"" ""
            "int main(void) {"
            "    int width = 6;"
            "    int height = 4;"
            "    int area = rectangle_area(width, height);"
            "    printf(\"area: %d\\n\", area);"
            "    return 0;"
            "}")

(defun goto (line column)
  "Point to LINE, from 1, and COLUMN, from 0."
  (keys '(#\< "Meta"))
  (dotimes (i (1- line)) (post-key #\n "Control") (sleep 0.03))
  (keys '(#\a "Control"))
  (dotimes (i column) (post-key #\f "Control") (sleep 0.02))
  (settle))

(defun steps ()
  (caption "A language server for each language: here clangd, for C, started by itself")
  (open-file (merge-pathnames "main.c" *project*))
  (wait-for "w: width" 40)
  (caption "Inlay hints: the names of a call's parameters, shown in place")
  (pause 3)

  (caption "C-c C-d: what the server says of what point is on")
  (goto 7 19)
  (keys '(#\c "Control") '(#\d "Control"))
  (wait-for "area of a rectangle" 10)
  (pause 3)
  (keys '(#\g "Control"))

  (caption "Completion from the server, with each candidate's kind")
  (goto 8 0)
  (keys '(#\e "Control") "Return")
  (type-text "int edge = re")
  (keys '(#\i "Control" "Meta"))
  (wait-for "rectangle_perimeter" 10)
  (pause 2.5)
  (keys '(#\g "Control"))
  (caption "Signature help as a call is typed, and errors marked as you type")
  (type-text "ctangle_perimeter(" :pause 0.06)
  (wait-for "int w, int h" 10)
  (pause 1.5)
  (type-text "width);")
  (wait-for "1 error" 15)
  (pause 1.5)
  (caption "M-n goes to the next error and says what it is")
  (keys '(#\< "Meta") '(#\n "Meta"))
  (pause 3.5)

  (caption "M-. goes to a definition, in whichever file it is")
  (goto 7 19)
  (keys '(#\. "Meta"))
  (wait-for "return w * h" 10)
  (pause 2.5)
  (caption "M-? lists every use; Return on one visits it")
  (keys '(#\? "Meta"))
  (wait-for "main.c" 10)
  (pause 3)
  (keys '(#\x "Control") #\1)

  (caption "C-c C-f folds a function under its first line, and opens it again")
  (open-file (merge-pathnames "shapes.c" *project*))
  (goto 7 0)
  (keys '(#\c "Control") '(#\f "Control"))
  (wait-for "lines }" 5)
  (pause 2.5)
  (keys '(#\c "Control") '(#\f "Control"))
  (pause 1.5)

  (caption "LSP Rename: a name changed everywhere the server finds it")
  (open-file (merge-pathnames "main.c" *project*))
  (goto 5 9)
  (extended-command "LSP Rename")
  (pause 0.6)
  (keys '(#\a "Control") '(#\k "Control"))
  (type-text "side")
  (keys "Return")
  (wait-for "int side = 6" 10)
  (pause 4))

(run-demo "languages" #'steps)
