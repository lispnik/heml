#!/bin/sh
# scripts/tree-sitter-grammars.sh -- `make tree-sitter`: the tree-sitter
# grammars Heml highlights with, built from source into build/tree-sitter/:
# C, Markdown (its block and inline grammars), Pascal, Bash (for shell
# scripts), Common Lisp, Rust, Go, JavaScript, TypeScript and TSX, JSON and
# YAML.
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

# grammar NAME REPOSITORY TAG [SUBDIRECTORY [QUERY]]
#
# QUERY, when given, is where the highlight query comes from instead of the
# grammar's own queries/highlights.scm: a URL, or files in the grammar's
# repository, put together in the order given (a grammar that builds on
# another's lists its own query and then the other's).
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
    case $query in
        '') cp "$g/queries/highlights.scm" "$OUT/share/tree-sitter/queries/$name/highlights.scm" ;;
        http*) curl -sfL -o "$OUT/share/tree-sitter/queries/$name/highlights.scm" "$query" ;;
        *)
            : > "$OUT/share/tree-sitter/queries/$name/highlights.scm"
            for file in $query; do
                case $file in
                    /*) cat "$file" ;;
                    *) cat "$dir/$file" ;;
                esac >> "$OUT/share/tree-sitter/queries/$name/highlights.scm"
            done ;;
    esac
    echo "built $name $tag"
}

# indents NAME SOURCE...: the indentation query for a language, from URLs or
# files in the repository, put together in the order given: Neovim's query
# for a language that builds on another says "inherits", and here the
# queries it inherits are listed before it.
indents() {
    name=$1
    shift
    mkdir -p "$OUT/share/tree-sitter/queries/$name"
    : > "$OUT/share/tree-sitter/queries/$name/indents.scm"
    for source in "$@"; do
        case $source in
            http*) curl -sfL "$source" ;;
            *) cat "$source" ;;
        esac >> "$OUT/share/tree-sitter/queries/$name/indents.scm"
    done
    echo "indents for $name"
}

grammar c tree-sitter/tree-sitter-c v0.24.2
grammar markdown tree-sitter-grammars/tree-sitter-markdown v0.5.3 tree-sitter-markdown
grammar markdown_inline tree-sitter-grammars/tree-sitter-markdown v0.5.3 tree-sitter-markdown-inline
grammar pascal Isopod/tree-sitter-pascal v0.10.2
grammar bash tree-sitter/tree-sitter-bash v0.25.1
# The Common Lisp grammar has no highlight query of its own; Neovim's is the
# fullest.  It is written for Neovim's rule that a later pattern overrides an
# earlier one, which heml.tree-sitter knows.
grammar commonlisp theHamsta/tree-sitter-commonlisp v0.4.1 . \
        https://raw.githubusercontent.com/nvim-treesitter/nvim-treesitter/cf12346a3414fa1b06af75c79faebe7f76df080a/queries/commonlisp/highlights.scm

grammar rust tree-sitter/tree-sitter-rust v0.24.2
grammar go tree-sitter/tree-sitter-go v0.25.0
grammar javascript tree-sitter/tree-sitter-javascript v0.25.0 . \
        "queries/highlights-jsx.scm queries/highlights-params.scm queries/highlights.scm"
# TypeScript's query adds to JavaScript's, which is built just above, but
# not to its query for parameters: TypeScript's are not JavaScript's, and
# its own query has them.
JS="$SRC/javascript/queries"
grammar typescript tree-sitter/tree-sitter-typescript v0.23.2 typescript \
        "queries/highlights.scm $JS/highlights.scm"
grammar tsx tree-sitter/tree-sitter-typescript v0.23.2 tsx \
        "queries/highlights.scm $JS/highlights-jsx.scm $JS/highlights.scm"
grammar json tree-sitter/tree-sitter-json v0.24.8
grammar yaml tree-sitter-grammars/tree-sitter-yaml v0.7.2

# Indentation: Neovim's queries for C, Pascal and Python (whose grammar comes from
# Homebrew), and Heml's own for shell scripts, which Neovim has none for.
NVIM=https://raw.githubusercontent.com/nvim-treesitter/nvim-treesitter/cf12346a3414fa1b06af75c79faebe7f76df080a/queries
indents c $NVIM/c/indents.scm
indents python $NVIM/python/indents.scm
indents pascal $NVIM/pascal/indents.scm
cat scripts/tree-sitter-queries/pascal/indents-extra.scm >> "$OUT/share/tree-sitter/queries/pascal/indents.scm"
indents bash scripts/tree-sitter-queries/bash/indents.scm
indents rust $NVIM/rust/indents.scm
indents go $NVIM/go/indents.scm
indents javascript $NVIM/ecma/indents.scm $NVIM/jsx/indents.scm $NVIM/javascript/indents.scm
indents typescript $NVIM/ecma/indents.scm $NVIM/typescript/indents.scm
indents tsx $NVIM/ecma/indents.scm $NVIM/typescript/indents.scm $NVIM/jsx/indents.scm $NVIM/tsx/indents.scm
indents json $NVIM/json/indents.scm
indents yaml $NVIM/yaml/indents.scm
