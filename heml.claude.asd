;;; Claude Code in Heml: a terminal of its own, and Heml as its IDE.

(asdf:defsystem :heml.claude
  :depends-on (:heml.base :heml.lsp :heml.term :babel :com.inuoe.jzon)
  :pathname "src/"
  :serial t
  :components ((:file "claude-ide")
               (:file "claude")))
