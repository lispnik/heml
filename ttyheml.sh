#!/bin/bash
clbuild lisp <<EOF
(asdf:operate 'asdf:load-op :ttyheml)
(hi::old-heml)
EOF
