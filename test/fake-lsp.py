#!/usr/bin/env python3
"""test/fake-lsp.py -- a language server that says the same things every
time, for `make smoke` to check Heml's client against (test/smoke.lisp).

It speaks the Language Server Protocol on its standard input and output:
an error on the second line of any file opened, two completions, a
definition on the third line, references on the first and third, a hover,
and a rename of the first three characters of the file.

It offers two code actions: one an edit, replacing the second line's
characters 2 to 12 with "rightthing", and one a command, which it carries
out by asking the editor to put a line at the start of the file.  Its
document symbols are a function on the third line with a variable in it.

Its signature for any call is fake_function(int a, int b), the argument
being typed the one after as many commas as the line has before the place
asked about.  It formats a file by putting a line before its first, and
its workspace symbols are the one function, in the first file opened.

It takes changes incrementally, as spans of the text it holds, and its
hover ends with that text's first and last lines and its length, so that a
test can see the server has what the editor has.

On opening a file it asks the editor for a setting, "fake.greeting", and
its hover ends with what it was given, when it was given something.

It also does a little of everything else the client can ask for, the same
for any file: a third completion that is a snippet, and brings a line of
its own with it; what its completions are, when asked; two more code
actions, one whose edit it gives only when asked and one that makes a
file; a declaration, a type's definition and two implementations; a caller
and something called; the uses of a name; one token to colour; a hint; a
lens; a fold; formatting of a region, and after a semicolon is typed; and
what can be renamed.  When a file is opened it says "fake says hello", and
that it is indexing, until it is first asked for a hover; it asks to hear
of files named *.watched, and its hover says which it has heard of.

Its arguments make it another server:

  --refuse  answers initialize with an error and goes, as a server that
            cannot start does;
  --crash   goes as soon as a file is opened, as one that keeps dying does;
  --pull    says nothing is wrong until it is asked (textDocument/diagnostic),
            and then that the second line is: "pulled error: N", N being
            the characters in all the files it holds, so that a change to
            one file changes what is wrong with another."""

import json
import sys


def read_message():
    length = None
    while True:
        line = sys.stdin.buffer.readline()
        if not line:
            return None
        line = line.strip()
        if not line:
            break
        name, _, value = line.partition(b":")
        if name.lower() == b"content-length":
            length = int(value)
    if length is None:
        return None
    return json.loads(sys.stdin.buffer.read(length).decode("utf-8"))


def send(message):
    body = json.dumps(message).encode("utf-8")
    sys.stdout.buffer.write(b"Content-Length: %d\r\n\r\n" % len(body) + body)
    sys.stdout.buffer.flush()


documents = {}
options = sys.argv[1:]
settings = {}
watched = []


def offset(text, line, character):
    """The index in TEXT of the protocol's position: LINE, and CHARACTER
    UTF-16 code units into it."""
    index = 0
    for _ in range(line):
        index = text.index("\n", index) + 1
    units = 0
    while units < character:
        units += 2 if ord(text[index]) > 0xFFFF else 1
        index += 1
    return index


def change(uri, changes):
    text = documents.get(uri, "")
    for c in changes:
        if "range" in c:
            start = offset(text, c["range"]["start"]["line"], c["range"]["start"]["character"])
            end = offset(text, c["range"]["end"]["line"], c["range"]["end"]["character"])
            text = text[:start] + c["text"] + text[end:]
        else:
            text = c["text"]
    documents[uri] = text


def place(uri, line, start, end):
    return {"uri": uri,
            "range": {"start": {"line": line, "character": start},
                      "end": {"line": line, "character": end}}}


def answer(method, params):
    uri = (params or {}).get("textDocument", {}).get("uri")
    if method == "initialize":
        pull = ({"diagnosticProvider": {"interFileDependencies": True,
                                        "workspaceDiagnostics": False}}
                if "--pull" in options else {})
        return {"capabilities": {**pull,
                                 "textDocumentSync": 2,
                                 "completionProvider": {"resolveProvider": True},
                                 "definitionProvider": True, "referencesProvider": True,
                                 "declarationProvider": True, "typeDefinitionProvider": True,
                                 "implementationProvider": True,
                                 "documentHighlightProvider": True,
                                 "callHierarchyProvider": True,
                                 "documentRangeFormattingProvider": True,
                                 "documentOnTypeFormattingProvider":
                                 {"firstTriggerCharacter": ";"},
                                 "semanticTokensProvider":
                                 {"legend": {"tokenTypes": ["function"], "tokenModifiers": []},
                                  "full": True},
                                 "inlayHintProvider": True,
                                 "codeLensProvider": {"resolveProvider": True},
                                 "foldingRangeProvider": True,
                                 "hoverProvider": True,
                                 "renameProvider": {"prepareProvider": True},
                                 "codeActionProvider": {"resolveProvider": True},
                                 "signatureHelpProvider": {"triggerCharacters": ["(", ","]},
                                 "documentFormattingProvider": True,
                                 "workspaceSymbolProvider": True,
                                 "documentSymbolProvider": True,
                                 "executeCommandProvider": {"commands": ["fake.command", "fake.lens"]}}}
    if method == "textDocument/codeAction":
        return [{"title": "Fix the fake error", "kind": "quickfix",
                 "edit": {"changes": {uri: [{"range": place(uri, 1, 2, 12)["range"],
                                             "newText": "rightthing"}]}}},
                {"title": "Run a command",
                 "command": {"title": "Run a command", "command": "fake.command",
                             "arguments": [uri]}},
                {"title": "Work it out later", "kind": "refactor", "data": uri},
                {"title": "Make a file", "kind": "refactor",
                 "edit": {"documentChanges": [
                     {"kind": "create", "uri": uri + ".made"},
                     {"textDocument": {"uri": uri + ".made", "version": None},
                      "edits": [{"range": place(uri, 0, 0, 0)["range"],
                                 "newText": "made by the fake\n"}]}]}}]
    if method == "codeAction/resolve":
        target = params["data"]
        return dict(params, edit={"changes": {target: [
            {"range": place(target, 0, 0, 0)["range"], "newText": "{ resolved }\n"}]}})
    if method in ("textDocument/declaration", "textDocument/typeDefinition"):
        return place(uri, 0, 0, 7)
    if method == "textDocument/implementation":
        return [place(uri, 0, 0, 7), place(uri, 2, 0, 5)]
    if method == "textDocument/documentHighlight":
        return [{"range": place(uri, 0, 8, 12)["range"], "kind": 1}]
    if method == "textDocument/prepareCallHierarchy":
        whole = place(uri, 2, 0, 5)["range"]
        return [{"name": "fake_symbol", "kind": 12, "uri": uri,
                 "range": whole, "selectionRange": whole}]
    if method == "callHierarchy/incomingCalls":
        target = params["item"]["uri"]
        first = place(target, 0, 0, 7)["range"]
        return [{"from": {"name": "fake_caller", "kind": 12, "uri": target,
                          "range": first, "selectionRange": first},
                 "fromRanges": [first]}]
    if method == "callHierarchy/outgoingCalls":
        target = params["item"]["uri"]
        last = place(target, 2, 0, 5)["range"]
        return [{"to": {"name": "fake_callee", "kind": 12, "uri": target,
                        "range": last, "selectionRange": last},
                 "fromRanges": []}]
    if method == "textDocument/semanticTokens/full":
        # "end", on the third line, is a function.
        return {"data": [2, 6, 3, 0, 0]}
    if method == "textDocument/inlayHint":
        # After the name on the first line, which ends at its semicolon.
        name_end = documents.get(uri, "").split("\n")[0].find(";")
        return [{"position": {"line": 0, "character": name_end if name_end >= 0 else 12},
                 "label": ": hinted", "kind": 1},
                {"position": {"line": 0, "character": 0}, "label": "argument:", "kind": 2,
                 "paddingRight": True}]
    if method == "textDocument/codeLens":
        return [{"range": place(uri, 0, 0, 7)["range"], "data": uri}]
    if method == "codeLens/resolve":
        return dict(params, command={"title": "Run the fake lens", "command": "fake.lens",
                                     "arguments": [params["data"]]})
    if method == "textDocument/foldingRange":
        return [{"startLine": 0, "endLine": 1}]
    if method == "textDocument/rangeFormatting":
        start = params["range"]["start"]["line"]
        return [{"range": place(uri, start, 0, 0)["range"], "newText": "{ ranged }\n"}]
    if method == "textDocument/onTypeFormatting":
        return [{"range": place(uri, 0, 0, 0)["range"], "newText": "{ typed }\n"}]
    if method == "textDocument/prepareRename":
        return {"range": place(uri, 0, 0, 3)["range"], "placeholder": "pro"}
    if method == "completionItem/resolve":
        item = dict(params, documentation="fake documentation of " + params["label"])
        if params["label"] == "fake_snippet":
            item["additionalTextEdits"] = [{"range": place(uri, 0, 0, 0)["range"],
                                            "newText": "{ imported }\n"}]
        return item
    if method == "workspace/executeCommand":
        # The edit is the editor's to make: it is asked for.
        target = params["arguments"][0]
        text = "{ lens }\n" if params["command"] == "fake.lens" else "{ done }\n"
        send({"jsonrpc": "2.0", "id": "edit-1", "method": "workspace/applyEdit",
              "params": {"edit": {"changes": {target: [{"range": place(target, 0, 0, 0)["range"],
                                                        "newText": text}]}}}})
        return None
    if method == "textDocument/signatureHelp":
        position = params["position"]
        line = documents.get(uri, "").split("\n")[position["line"]]
        return {"signatures": [{"label": "fake_function(int a, int b)",
                                "parameters": [{"label": "int a"}, {"label": [21, 26]}]}],
                "activeSignature": 0,
                "activeParameter": line[:position["character"]].count(",")}
    if method == "textDocument/formatting":
        return [{"range": place(uri, 0, 0, 0)["range"], "newText": "{ formatted }\n"}]
    if method == "workspace/symbol":
        first = next(iter(documents), None)
        return [dict(place(first, 2, 0, 5), **{})] and [
            {"name": "fake_symbol", "kind": 12, "location": place(first, 2, 0, 5)}]
    if method == "textDocument/documentSymbol":
        whole = place(uri, 2, 0, 5)["range"]
        return [{"name": "fake_symbol", "kind": 12, "range": whole, "selectionRange": whole,
                 "children": [{"name": "inner", "kind": 13, "range": whole,
                               "selectionRange": whole}]}]
    if method == "textDocument/completion":
        return {"isIncomplete": False,
                "items": [{"label": "fake_function", "kind": 3},
                          {"label": "fake_variable", "kind": 6},
                          {"label": "fake_snippet", "kind": 15, "insertTextFormat": 2,
                           "detail": "a snippet",
                           "insertText": "fake_snippet(${1:first}, ${2:second})$0"}]}
    if method == "textDocument/definition":
        return place(uri, 2, 0, 5)
    if method == "textDocument/references":
        return [place(uri, 0, 0, 7), place(uri, 2, 0, 5)]
    if method == "textDocument/diagnostic":
        return {"kind": "full",
                "items": [dict(place(uri, 1, 0, 1), severity=1,
                               message="pulled error: %d"
                               % sum(len(text) for text in documents.values()))]}
    if method == "textDocument/hover":
        lines = documents.get(uri, "").split("\n")
        greeting = settings.get("greeting")
        send({"jsonrpc": "2.0", "method": "$/progress",
              "params": {"token": "fake-progress", "value": {"kind": "end"}}})
        return {"contents": {"kind": "plaintext",
                             "value": "fake hover text\nfirst: %s\nlast: %s\nlength: %d%s%s"
                             % (lines[0], lines[-1], len(documents.get(uri, "")),
                                "\nconfig: %s" % greeting if greeting else "",
                                "".join("\nwatched: %s" % w for w in watched))}}
    if method == "textDocument/rename":
        return {"changes": {uri: [{"range": place(uri, 0, 0, 3)["range"],
                                   "newText": params["newName"]}]}}
    return None


while True:
    message = read_message()
    if message is None:
        break
    method = message.get("method")
    if method == "exit":
        break
    if "id" in message and method is None:
        # The editor's answer to something asked of it.
        if message["id"] == "config-1" and isinstance(message.get("result"), list):
            settings["greeting"] = message["result"][0]
        continue
    if method == "initialize" and "--refuse" in options:
        send({"jsonrpc": "2.0", "id": message["id"],
              "error": {"code": -32603, "message": "This server will not start."}})
        break
    if "id" in message:
        send({"jsonrpc": "2.0", "id": message["id"],
              "result": answer(method, message.get("params"))})
    elif method == "initialized":
        send({"jsonrpc": "2.0", "id": "watch-1", "method": "client/registerCapability",
              "params": {"registrations": [
                  {"id": "fake-watch", "method": "workspace/didChangeWatchedFiles",
                   "registerOptions": {"watchers": [{"globPattern": "**/*.watched"}]}}]}})
    elif method == "workspace/didChangeWatchedFiles":
        for c in message["params"]["changes"]:
            watched.append("%s %d" % (c["uri"].rsplit("/", 1)[-1], c["type"]))
    elif method == "workspace/didChangeConfiguration":
        fake = (message["params"].get("settings") or {}).get("fake") or {}
        if "greeting" in fake:
            settings["greeting"] = fake["greeting"]
    elif method == "textDocument/didChange":
        change(message["params"]["textDocument"]["uri"], message["params"]["contentChanges"])
    elif method == "textDocument/didOpen":
        uri = message["params"]["textDocument"]["uri"]
        if "--crash" in options:
            break
        documents[uri] = message["params"]["textDocument"]["text"]
        send({"jsonrpc": "2.0", "id": "config-1", "method": "workspace/configuration",
              "params": {"items": [{"section": "fake.greeting"}]}})
        send({"jsonrpc": "2.0", "method": "window/showMessage",
              "params": {"type": 3, "message": "fake says hello"}})
        send({"jsonrpc": "2.0", "id": "progress-1", "method": "window/workDoneProgress/create",
              "params": {"token": "fake-progress"}})
        send({"jsonrpc": "2.0", "method": "$/progress",
              "params": {"token": "fake-progress",
                         "value": {"kind": "begin", "title": "Indexing", "percentage": 50}}})
        if "--pull" not in options:
            send({"jsonrpc": "2.0", "method": "textDocument/publishDiagnostics",
                  "params": {"uri": uri,
                             "diagnostics": [dict(place(uri, 1, 2, 7), severity=1,
                                                  message="fake error")]}})
