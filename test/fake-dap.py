#!/usr/bin/env python3
"""test/fake-dap.py -- a debug adapter that pretends to run a program, for
the smoke tests to check Heml's debugger client against (src/dap.lisp).

It speaks the Debug Adapter Protocol on its standard input and output.  The
program it pretends to run is the file it is asked to launch, and running
it means stopping at the first breakpoint set in that file, if there is
one.  Stopped, its thread has two frames, fake_main at the line it stopped
at and fake_caller at line 1; fake_main's variables are x, 1, and point,
whose part is a, 2; fake_caller's is caller_var, 7.  Next and step in go
on a line, step out goes to line 1, and anything evaluated is 42.  Going
on to the end prints "hello from the fake program" and exits."""

import json
import sys

seq = 0
program = None
breakpoints = {}
line = None


def read_message():
    length = None
    while True:
        header = sys.stdin.buffer.readline()
        if not header:
            return None
        header = header.strip()
        if not header:
            break
        name, _, value = header.partition(b":")
        if name.lower() == b"content-length":
            length = int(value)
    if length is None:
        return None
    return json.loads(sys.stdin.buffer.read(length).decode("utf-8"))


def send(message):
    global seq
    seq += 1
    message["seq"] = seq
    body = json.dumps(message).encode("utf-8")
    sys.stdout.buffer.write(b"Content-Length: %d\r\n\r\n" % len(body) + body)
    sys.stdout.buffer.flush()


def respond(request, body=None, success=True, message=None):
    reply = {"type": "response", "request_seq": request["seq"],
             "command": request["command"], "success": success, "body": body or {}}
    if message:
        reply["message"] = message
    send(reply)


def event(name, body=None):
    send({"type": "event", "event": name, "body": body or {}})


def stop(reason):
    event("stopped", {"reason": reason, "threadId": 1, "allThreadsStopped": True})


def finish():
    event("output", {"category": "stdout", "output": "hello from the fake program\n"})
    event("exited", {"exitCode": 0})
    event("terminated")


while True:
    request = read_message()
    if request is None:
        break
    command = request.get("command")
    arguments = request.get("arguments") or {}
    if command == "initialize":
        respond(request, {"supportsConfigurationDoneRequest": True})
        event("initialized")
    elif command == "launch":
        program = arguments.get("program")
        respond(request)
    elif command == "setBreakpoints":
        lines = [b["line"] for b in arguments.get("breakpoints", [])]
        breakpoints[arguments["source"]["path"]] = lines
        respond(request, {"breakpoints": [{"verified": True, "line": n} for n in lines]})
    elif command == "setExceptionBreakpoints":
        respond(request)
    elif command == "configurationDone":
        respond(request)
        lines = sorted(breakpoints.get(program, []))
        if lines:
            line = lines[0]
            stop("breakpoint")
        else:
            finish()
    elif command == "threads":
        respond(request, {"threads": [{"id": 1, "name": "main"}]})
    elif command == "stackTrace":
        respond(request, {"stackFrames": [
            {"id": 1, "name": "fake_main", "source": {"path": program}, "line": line, "column": 1},
            {"id": 2, "name": "fake_caller", "source": {"path": program}, "line": 1, "column": 1}],
            "totalFrames": 2})
    elif command == "scopes":
        reference = 10 if arguments.get("frameId") == 1 else 20
        respond(request, {"scopes": [{"name": "Locals", "variablesReference": reference,
                                      "expensive": False}]})
    elif command == "variables":
        reference = arguments.get("variablesReference")
        variables = {
            10: [{"name": "x", "value": "1", "type": "int", "variablesReference": 0},
                 {"name": "point", "value": "{...}", "type": "struct point",
                  "variablesReference": 11}],
            11: [{"name": "a", "value": "2", "type": "int", "variablesReference": 0}],
            20: [{"name": "caller_var", "value": "7", "type": "int", "variablesReference": 0}],
        }.get(reference, [])
        respond(request, {"variables": variables})
    elif command in ("next", "stepIn"):
        respond(request)
        event("continued", {"threadId": 1})
        line += 1
        stop("step")
    elif command == "stepOut":
        respond(request)
        line = 1
        stop("step")
    elif command == "continue":
        respond(request, {"allThreadsContinued": True})
        finish()
    elif command == "pause":
        respond(request)
        stop("pause")
    elif command == "evaluate":
        respond(request, {"result": "42", "variablesReference": 0})
    elif command == "disconnect":
        respond(request)
        break
    else:
        respond(request, success=False, message="The fake does not do " + str(command))
