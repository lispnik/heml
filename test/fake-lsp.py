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

It takes changes incrementally, as spans of the text it holds, and its
hover ends with that text's first and last lines and its length, so that a
test can see the server has what the editor has."""

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
        return {"capabilities": {"textDocumentSync": 2, "completionProvider": {},
                                 "definitionProvider": True, "referencesProvider": True,
                                 "hoverProvider": True, "renameProvider": True,
                                 "codeActionProvider": True,
                                 "documentSymbolProvider": True,
                                 "executeCommandProvider": {"commands": ["fake.command"]}}}
    if method == "textDocument/codeAction":
        return [{"title": "Fix the fake error", "kind": "quickfix",
                 "edit": {"changes": {uri: [{"range": place(uri, 1, 2, 12)["range"],
                                             "newText": "rightthing"}]}}},
                {"title": "Run a command",
                 "command": {"title": "Run a command", "command": "fake.command",
                             "arguments": [uri]}}]
    if method == "workspace/executeCommand":
        # The edit is the editor's to make: it is asked for.
        target = params["arguments"][0]
        send({"jsonrpc": "2.0", "id": "edit-1", "method": "workspace/applyEdit",
              "params": {"edit": {"changes": {target: [{"range": place(target, 0, 0, 0)["range"],
                                                        "newText": "{ done }\n"}]}}}})
        return None
    if method == "textDocument/documentSymbol":
        whole = place(uri, 2, 0, 5)["range"]
        return [{"name": "fake_symbol", "kind": 12, "range": whole, "selectionRange": whole,
                 "children": [{"name": "inner", "kind": 13, "range": whole,
                               "selectionRange": whole}]}]
    if method == "textDocument/completion":
        return {"isIncomplete": False,
                "items": [{"label": "fake_function", "kind": 3},
                          {"label": "fake_variable", "kind": 6}]}
    if method == "textDocument/definition":
        return place(uri, 2, 0, 5)
    if method == "textDocument/references":
        return [place(uri, 0, 0, 7), place(uri, 2, 0, 5)]
    if method == "textDocument/hover":
        lines = documents.get(uri, "").split("\n")
        return {"contents": {"kind": "plaintext",
                             "value": "fake hover text\nfirst: %s\nlast: %s\nlength: %d"
                             % (lines[0], lines[-1], len(documents.get(uri, "")))}}
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
        continue                # the editor's answer to something asked of it
    if "id" in message:
        send({"jsonrpc": "2.0", "id": message["id"],
              "result": answer(method, message.get("params"))})
    elif method == "textDocument/didChange":
        change(message["params"]["textDocument"]["uri"], message["params"]["contentChanges"])
    elif method == "textDocument/didOpen":
        uri = message["params"]["textDocument"]["uri"]
        documents[uri] = message["params"]["textDocument"]["text"]
        send({"jsonrpc": "2.0", "method": "textDocument/publishDiagnostics",
              "params": {"uri": uri,
                         "diagnostics": [dict(place(uri, 1, 2, 7), severity=1,
                                              message="fake error")]}})
