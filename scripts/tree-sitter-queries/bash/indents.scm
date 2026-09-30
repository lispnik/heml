; Heml's indentation for shell scripts, read by heml.tree-sitter the way
; Neovim reads its indents.scm: a block indents the lines inside it, and a
; line that starts with a keyword ending or continuing the block (else, fi,
; done, esac, }) goes back out.  Neovim has no query for Bash.

[
  (if_statement)
  (do_group)
  (compound_statement)
  (subshell)
  (case_statement)
  (case_item)
] @indent.begin

[
  (elif_clause)
  (else_clause)
  "fi"
  "done"
  "}"
  ")"
  "esac"
] @indent.branch

; While a block is being typed, before its end is, the parser's recovery
; leaves an ERROR: the keyword that opened it still indents what follows.
(ERROR "then" @indent.begin)
(ERROR "do" @indent.begin)
(ERROR "{" @indent.begin)
(ERROR "else" @indent.begin)

(heredoc_body) @indent.auto
(comment) @indent.auto
