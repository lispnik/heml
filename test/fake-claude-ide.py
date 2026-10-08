#!/usr/bin/env python3
"""A stand-in for Claude Code's side of its IDE connection, for the smoke
tests: it finds the editor's lock file in CONFIG_DIR/ide/, connects to its
WebSocket with the token there, initializes MCP and calls the tools, each
result printed as one line, `TOOL TEXT', the tool's text flattened.

  fake-claude-ide.py CONFIG_DIR [--bad-token] [--diff FILE CONTENTS]

--bad-token connects with the wrong token and prints `refused STATUS'.
--diff asks openDiff to change FILE to CONTENTS and waits for the answer.
Only Python's standard library: no WebSocket package is needed."""

import base64, glob, json, os, socket, struct, sys


def lock(config_dir):
    files = sorted(glob.glob(os.path.join(config_dir, "ide", "*.lock")), key=os.path.getmtime)
    if not files:
        sys.exit("no lock file in %s/ide" % config_dir)
    with open(files[-1]) as f:
        data = json.load(f)
    return int(os.path.basename(files[-1])[:-5]), data


def connect(port, token):
    s = socket.create_connection(("127.0.0.1", port))
    key = base64.b64encode(os.urandom(16)).decode()
    request = ("GET / HTTP/1.1\r\nHost: 127.0.0.1:%d\r\nUpgrade: websocket\r\n"
               "Connection: Upgrade\r\nSec-WebSocket-Key: %s\r\nSec-WebSocket-Version: 13\r\n"
               "Sec-WebSocket-Protocol: mcp\r\nX-Claude-Code-Ide-Authorization: %s\r\n\r\n"
               % (port, key, token))
    s.sendall(request.encode())
    head = b""
    while b"\r\n\r\n" not in head:
        chunk = s.recv(1)
        if not chunk:
            break
        head += chunk
    return s, head.decode("latin-1").split("\r\n")[0]


def send(s, obj):
    payload = json.dumps(obj).encode()
    mask = os.urandom(4)
    n = len(payload)
    if n < 126:
        header = struct.pack("!BB", 0x81, 0x80 | n)
    elif n < 65536:
        header = struct.pack("!BBH", 0x81, 0x80 | 126, n)
    else:
        header = struct.pack("!BBQ", 0x81, 0x80 | 127, n)
    s.sendall(header + mask + bytes(b ^ mask[i % 4] for i, b in enumerate(payload)))


def recv_exact(s, n):
    data = b""
    while len(data) < n:
        chunk = s.recv(n - len(data))
        if not chunk:
            raise EOFError
        data += chunk
    return data


def receive(s):
    """The next text message, a JSON object."""
    while True:
        b0, b1 = recv_exact(s, 2)
        n = b1 & 0x7F
        if n == 126:
            n = struct.unpack("!H", recv_exact(s, 2))[0]
        elif n == 127:
            n = struct.unpack("!Q", recv_exact(s, 8))[0]
        payload = recv_exact(s, n)
        if b0 & 0x0F == 1:
            return json.loads(payload.decode())


def answer(s, id):
    """The response to request ID, past any notifications."""
    while True:
        message = receive(s)
        if message.get("id") == id:
            return message


counter = [0]


def call(s, method, params=None):
    counter[0] += 1
    send(s, {"jsonrpc": "2.0", "id": counter[0], "method": method, "params": params or {}})
    return answer(s, counter[0])


def tool(s, name, arguments=None):
    reply = call(s, "tools/call", {"name": name, "arguments": arguments or {}})
    if "error" in reply:
        text = "ERROR " + reply["error"]["message"]
    else:
        text = " | ".join(c["text"] for c in reply["result"]["content"])
    print(name, text.replace("\n", "\\n"), flush=True)


def main():
    config_dir = sys.argv[1]
    port, data = lock(config_dir)
    print("lock", data["ideName"], data["transport"], ",".join(data["workspaceFolders"]), flush=True)
    if "--bad-token" in sys.argv:
        s, status = connect(port, "wrong")
        print("refused", status, flush=True)
        return
    s, status = connect(port, data["authToken"])
    print("status", status, flush=True)
    reply = call(s, "initialize", {"protocolVersion": "2024-11-05", "capabilities": {},
                                   "clientInfo": {"name": "fake-claude", "version": "1"}})
    print("initialize", reply["result"]["serverInfo"]["name"], reply["result"]["protocolVersion"], flush=True)
    send(s, {"jsonrpc": "2.0", "method": "notifications/initialized", "params": {}})
    tools = call(s, "tools/list")["result"]["tools"]
    print("tools", ",".join(sorted(t["name"] for t in tools)), flush=True)
    if "--diff" in sys.argv:
        i = sys.argv.index("--diff")
        tool(s, "openDiff", {"old_file_path": sys.argv[i + 1], "new_file_path": sys.argv[i + 1],
                             "new_file_contents": sys.argv[i + 2], "tab_name": "fake-change"})
        return
    tool(s, "getWorkspaceFolders")
    tool(s, "getOpenEditors")
    tool(s, "getCurrentSelection")
    tool(s, "getDiagnostics")
    tool(s, "nonesuch")


if __name__ == "__main__":
    main()
