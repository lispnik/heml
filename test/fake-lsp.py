#!/usr/bin/env python3
"""test/fake-lsp.py -- a language server that says the same things every
time, for `make smoke` to check Heml's client against (test/smoke.lisp).

It speaks the Language Server Protocol on its standard input and output:
an error on the second line of any file opened, two completions, a
definition on the third line, references on the first and third, a hover,
and a rename of the first three characters of the file."""

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


def place(uri, line, start, end):
    return {"uri": uri,
            "range": {"start": {"line": line, "character": start},
                      "end": {"line": line, "character": end}}}


def answer(method, params):
    uri = (params or {}).get("textDocument", {}).get("uri")
    if method == "initialize":
        return {"capabilities": {"textDocumentSync": 1, "completionProvider": {},
                                 "definitionProvider": True, "referencesProvider": True,
                                 "hoverProvider": True, "renameProvider": True}}
    if method == "textDocument/completion":
        return {"isIncomplete": False,
                "items": [{"label": "fake_function", "kind": 3},
                          {"label": "fake_variable", "kind": 6}]}
    if method == "textDocument/definition":
        return place(uri, 2, 0, 5)
    if method == "textDocument/references":
        return [place(uri, 0, 0, 7), place(uri, 2, 0, 5)]
    if method == "textDocument/hover":
        return {"contents": {"kind": "plaintext", "value": "fake hover text"}}
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
    if "id" in message:
        send({"jsonrpc": "2.0", "id": message["id"],
              "result": answer(method, message.get("params"))})
    elif method == "textDocument/didOpen":
        uri = message["params"]["textDocument"]["uri"]
        send({"jsonrpc": "2.0", "method": "textDocument/publishDiagnostics",
              "params": {"uri": uri,
                         "diagnostics": [dict(place(uri, 1, 2, 7), severity=1,
                                              message="fake error")]}})
