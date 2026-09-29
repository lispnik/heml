#!/bin/sh
# scripts/tree-sitter-grammars.sh -- `make tree-sitter`: the tree-sitter
# grammars Heml highlights with, built from source into build/tree-sitter/:
# C, Markdown, Pascal, Bash (for shell scripts) and Common Lisp.
#
# Each grammar is fetched at a pinned tag, compiled into
# lib/libtree-sitter-<name>.dylib, and its highlight query copied to
# share/tree-sitter/queries/<name>/highlights.scm: the layout Homebrew uses,
# so that heml.tree-sitter looks in both the same way.  The tree-sitter
# library itself comes from Homebrew (brew install tree-sitter), as does
# Python's grammar (brew install tree-sitter-python).
#
# Needs git and a C compiler.  OUT overrides the destination.

set -eu

cd "$(dirname "$0")/.." || exit 1
OUT=${OUT:-$PWD/build/tree-sitter}
SRC=$OUT/src
mkdir -p "$OUT/lib" "$SRC"

# grammar NAME REPOSITORY TAG [SUBDIRECTORY [QUERY-URL]]
#
# QUERY-URL, when given, is where the highlight query comes from instead of
# the grammar's own queries/ directory.
grammar() {
    name=$1 repo=$2 tag=$3 sub=${4:-.} query=${5:-}
    dir=$SRC/$name
    if [ ! -d "$dir/.git" ] || [ "$(git -C "$dir" describe --tags 2>/dev/null)" != "$tag" ]; then
        rm -rf "$dir"
        git -c advice.detachedHead=false clone --quiet --depth 1 --branch "$tag" \
            "https://github.com/$repo.git" "$dir"
    fi
    g=$dir/$sub
    sources=$g/src/parser.c
    [ -f "$g/src/scanner.c" ] && sources="$sources $g/src/scanner.c"
    # shellcheck disable=SC2086
    cc -O2 -shared -fPIC -std=c11 -I "$g/src" $sources \
       -o "$OUT/lib/libtree-sitter-$name.dylib"
    mkdir -p "$OUT/share/tree-sitter/queries/$name"
    if [ -n "$query" ]; then
        curl -sfL -o "$OUT/share/tree-sitter/queries/$name/highlights.scm" "$query"
    else
        cp "$g/queries/highlights.scm" "$OUT/share/tree-sitter/queries/$name/highlights.scm"
    fi
    echo "built $name $tag"
}

grammar c tree-sitter/tree-sitter-c v0.24.2
grammar markdown tree-sitter-grammars/tree-sitter-markdown v0.5.3 tree-sitter-markdown
grammar pascal Isopod/tree-sitter-pascal v0.10.2
grammar bash tree-sitter/tree-sitter-bash v0.25.1
# The Common Lisp grammar has no highlight query of its own; Neovim's is the
# fullest.  It is written for Neovim's rule that a later pattern overrides an
# earlier one, which heml.tree-sitter knows.
grammar commonlisp theHamsta/tree-sitter-commonlisp v0.4.1 . \
        https://raw.githubusercontent.com/nvim-treesitter/nvim-treesitter/cf12346a3414fa1b06af75c79faebe7f76df080a/queries/commonlisp/highlights.scm
