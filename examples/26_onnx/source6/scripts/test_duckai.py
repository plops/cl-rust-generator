#!/usr/bin/env python3
"""test_duckai.py — Duck.ai-Integrationstest via Chrome DevTools Protocol.

Ablauf (wie von Wol Pumba vorgegeben):
  1. Auf duck.ai gehen.
  2. Auf "Ask anything privately" klicken.
  3. "Tell me a joke about programming" eingeben.
  4. Auf "Ask" (unten rechts am Eingabefeld) klicken.
  5. Antwort prüfen: "Anonymized by DuckDuckGo"-Hinweis + Witz.

Klicks laufen über echte CDP-Maus-Events (Input.dispatchMouseEvent) auf
Koordinaten aus getBoundingClientRect — derselbe Pfad (Koordinaten ->
Klick), den später die source6-XTEST-Automation nutzt. Nur Python-Stdlib,
keine Zusatzabhängigkeiten.

Aufruf:
  test_duckai.py [--cdp URL] [--out DIR] [--timeout S]

Umgebung: Chrome muss mit --remote-debugging-port laufen (s. test_duckai.sh).
"""
import base64
import hashlib
import json
import os
import socket
import struct
import sys
import time
import urllib.request

WS_GUID = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"


class CdpError(Exception):
    pass


class Ws:
    """Minimaler WebSocket-Client (Text-Frames, stdlib only)."""

    def __init__(self, host, port, path):
        self.sock = socket.create_connection((host, port), timeout=15)
        key = base64.b64encode(os.urandom(16)).decode()
        req = (
            f"GET {path} HTTP/1.1\r\nHost: {host}:{port}\r\n"
            "Upgrade: websocket\r\nConnection: Upgrade\r\n"
            f"Sec-WebSocket-Key: {key}\r\nSec-WebSocket-Version: 13\r\n\r\n"
        )
        self.sock.sendall(req.encode())
        head = b""
        while b"\r\n\r\n" not in head:
            chunk = self.sock.recv(4096)
            if not chunk:
                raise CdpError("websocket handshake failed (eof)")
            head += chunk
        accept = hashlib.sha1((key + WS_GUID).encode()).digest()
        if base64.b64encode(accept).decode() not in head.decode():
            raise CdpError("websocket handshake failed (bad accept)")
        self.buf = b""

    def _recv_frame(self):
        while len(self.buf) < 2:
            self.buf += self.sock.recv(65536)
        b1, b2 = self.buf[0], self.buf[1]
        opcode = b1 & 0x0F
        length = b2 & 0x7F
        idx = 2
        if length == 126:
            while len(self.buf) < 4:
                self.buf += self.sock.recv(65536)
            (length,) = struct.unpack(">H", self.buf[2:4])
            idx = 4
        elif length == 127:
            while len(self.buf) < 10:
                self.buf += self.sock.recv(65536)
            (length,) = struct.unpack(">Q", self.buf[2:10])
            idx = 10
        # Server -> Client ist unmaskiert.
        while len(self.buf) < idx + length:
            self.buf += self.sock.recv(65536)
        payload = self.buf[idx:idx + length]
        self.buf = self.buf[idx + length:]
        return opcode, payload

    def recv_text(self):
        parts = []
        while True:
            opcode, payload = self._recv_frame()
            if opcode == 0x8:
                raise CdpError("websocket closed by peer")
            if opcode == 0x9:  # ping -> pong
                self.send_raw(0xA, payload)
                continue
            if opcode in (0x1, 0x0):
                parts.append(payload)
                if opcode == 0x1 or True:
                    # Chrome fragmentiert CDP-Antworten praktisch nie;
                    # FIN-Bit wird der Einfachheit halber nicht geprüft.
                    return b"".join(parts).decode("utf-8", "replace")

    def send_raw(self, opcode, payload):
        if isinstance(payload, str):
            payload = payload.encode()
        head = bytes([0x80 | opcode])
        n = len(payload)
        if n < 126:
            head += bytes([0x80 | n])
        elif n < 65536:
            head += bytes([0x80 | 126]) + struct.pack(">H", n)
        else:
            head += bytes([0x80 | 127]) + struct.pack(">Q", n)
        mask = os.urandom(4)
        masked = bytes(b ^ mask[i % 4] for i, b in enumerate(payload))
        self.sock.sendall(head + mask + masked)

    def send_text(self, text):
        self.send_raw(0x1, text)

    def close(self):
        try:
            self.send_raw(0x8, b"")
            self.sock.close()
        except OSError:
            pass


class Cdp:
    def __init__(self, ws_url):
        # ws://host:port/path
        rest = ws_url.split("://", 1)[1]
        hostport, path = rest.split("/", 1)
        host, port = hostport.split(":")
        self.ws = Ws(host, int(port), "/" + path)
        self.next_id = 0
        self.events = []  # gemerkte Events (bounded)

    def call(self, method, params=None, timeout=20):
        self.next_id += 1
        mid = self.next_id
        self.ws.send_text(json.dumps({"id": mid, "method": method,
                                      "params": params or {}}))
        deadline = time.time() + timeout
        while True:
            left = deadline - time.time()
            if left <= 0:
                raise CdpError(f"cdp timeout: {method}")
            self.ws.sock.settimeout(left)
            try:
                msg = json.loads(self.ws.recv_text())
            except socket.timeout:
                raise CdpError(f"cdp timeout: {method}")
            if msg.get("id") == mid:
                if "error" in msg:
                    raise CdpError(f"{method}: {msg['error']}")
                return msg.get("result", {})
            # Events merken (bounded), sonst verwerfen.
            self.events.append(msg)
            del self.events[:-50]

    def main_frame_context(self):
        """uniqueContextId des Default-Kontexts im Haupt-Frame (oder None).

        Nötig, weil die Seite iframes (about:srcdoc) enthält: ein
        kontextloses evaluate kann im falschen Frame landen.
        """
        try:
            tree = self.call("Page.getFrameTree", timeout=10)
            main_id = tree["frameTree"]["frame"]["id"]
        except CdpError:
            return None
        for ev in reversed(self.events):
            if ev.get("method") == "Runtime.executionContextCreated":
                ctx = ev["params"]["context"]
                aux = ctx.get("auxData", {})
                if aux.get("frameId") == main_id and aux.get("isDefault"):
                    return ctx.get("uniqueId")
        return None

    def eval(self, js, timeout=20):
        params = {"expression": js, "returnByValue": True,
                  "awaitPromise": True}
        uid = self.main_frame_context()
        if uid:
            params["uniqueContextId"] = uid
        try:
            r = self.call("Runtime.evaluate", params, timeout=timeout)
        except CdpError:
            if not uid:
                raise
            # Kontext veraltet (Navigation) -> ohne Kontext wiederholen.
            del params["uniqueContextId"]
            r = self.call("Runtime.evaluate", params, timeout=timeout)
        # call() liefert bereits msg["result"]; darin liegt RemoteObject
        # unter "result", Ausnahmen unter "exceptionDetails".
        try:
            inner = r["result"]
        except (KeyError, TypeError) as e:
            raise CdpError(
                f"unexpected evaluate response for {js!r}: {r!r}") from e
        if r.get("exceptionDetails") or inner.get("subtype") == "error":
            raise CdpError(f"js failed: {r!r}")
        return inner.get("value")

    def close(self):
        self.ws.close()


FIND_CLICK = """(phrase) => {
  const norm = s => (s || "").replace(/\\s+/g, " ").trim();
  const vis = el => {
    const r = el.getBoundingClientRect();
    if (r.width <= 0 || r.height <= 0) return false;
    const st = getComputedStyle(el);
    return st.visibility !== "hidden" && st.display !== "none";
  };
  const cands = [...document.querySelectorAll(
    "button, a, [role=button], input[type=submit]")];
  const q = phrase.toLowerCase();
  for (const el of cands) {
    const t = norm(el.innerText || el.value ||
                   el.getAttribute("aria-label") || el.title);
    if (t.toLowerCase().includes(q) && vis(el)) {
      const r = el.getBoundingClientRect();
      return {x: r.x + r.width / 2, y: r.y + r.height / 2, text: t};
    }
  }
  // Fallback: beliebiges sichtbares Element mit dem Text.
  const all = [...document.querySelectorAll("body *")];
  for (const el of all) {
    if (el.children.length === 0 && vis(el) &&
        norm(el.textContent).toLowerCase().includes(q) &&
        norm(el.textContent).length < phrase.length + 40) {
      const r = el.getBoundingClientRect();
      return {x: r.x + r.width / 2, y: r.y + r.height / 2,
              text: norm(el.textContent)};
    }
  }
  return null;
}"""

FIND_INPUT = """() => {
  const sels = ["textarea", "input[type=text]", "input:not([type])",
                "[contenteditable=true]", "[role=textbox]"];
  for (const s of sels) {
    for (const el of document.querySelectorAll(s)) {
      const r = el.getBoundingClientRect();
      if (r.width > 50 && r.height > 10 && el.offsetParent !== null) {
        return {x: r.x + r.width / 2, y: r.y + r.height / 2,
                top: r.top, tag: el.tagName};
      }
    }
  }
  return null;
}"""

FIND_SUBMIT = """(top) => {
  const norm = s => (s || "").replace(/\\s+/g, " ").trim();
  const box = el => {
    const r = el.getBoundingClientRect();
    return {x: r.x + r.width / 2, y: r.y + r.height / 2, top: r.top};
  };
  const btns = [...document.querySelectorAll(
      "button, [role=button], input[type=submit]")].filter(el => {
    const r = el.getBoundingClientRect();
    const st = getComputedStyle(el);
    return r.width > 0 && r.height > 0 &&
           st.visibility !== "hidden" && st.display !== "none";
  });
  // 1. Exakter "Ask"-Text (Bottom-right am Eingabefeld).
  let c = btns.map(el => ({el, t: norm(el.innerText || el.value || "")}))
    .filter(o => o.t.toLowerCase() === "ask" ||
                 o.t.toLowerCase().startsWith("ask "));
  let how = "text";
  // 2. aria-label ask/send/submit (Icon-Buttons ohne Text).
  if (!c.length) {
    how = "aria-label";
    c = btns.map(el => ({el, t: norm(el.getAttribute("aria-label") || "")}))
      .filter(o => /ask|send|submit/i.test(o.t));
  }
  if (c.length) {
    c.sort((a, b) => Math.abs(
      a.el.getBoundingClientRect().top - top) -
      Math.abs(b.el.getBoundingClientRect().top - top));
    return {...box(c[0].el), text: c[0].t, how};
  }
  // 3. Fallback: Button im Composer, am weitesten rechts unten.
  const near = btns.filter(el =>
    Math.abs(el.getBoundingClientRect().top - top) < 120);
  const pool = near.length ? near : btns;
  if (!pool.length) return null;
  const sorted = [...pool].sort((a, b) => {
    const ra = a.getBoundingClientRect(), rb = b.getBoundingClientRect();
    return (rb.right + rb.top) - (ra.right + ra.top);
  });
  return {...box(sorted[0]), text: "(fallback)", how: "fallback"};
}"""

def dom_text(cdp):
    """Seittext über die DOM-Domain (Haupt-Frame, kein JS-Kontext nötig)."""
    import re
    import html as htmlmod
    doc = cdp.call("DOM.getDocument", {"depth": 0}, timeout=15)
    resolved = cdp.call("DOM.resolveNode",
                        {"nodeId": doc["root"]["nodeId"]}, timeout=15)
    out = cdp.call("DOM.getOuterHTML",
                   {"objectId": resolved["object"]["objectId"]},
                   timeout=30)["outerHTML"]
    out = re.sub(r"<script.*?</script>", " ", out,
                 flags=re.S | re.I)
    out = re.sub(r"<style.*?</style>", " ", out, flags=re.S | re.I)
    out = re.sub(r"<[^>]+>", " ", out)
    out = htmlmod.unescape(out)
    return re.sub(r"\s+", " ", out).strip()[:20000]


def mouse_click(cdp, x, y):
    cdp.call("Input.dispatchMouseEvent",
             {"type": "mouseMoved", "x": x, "y": y})
    time.sleep(0.15)
    cdp.call("Input.dispatchMouseEvent",
             {"type": "mousePressed", "x": x, "y": y, "button": "left",
              "clickCount": 1})
    time.sleep(0.1)
    cdp.call("Input.dispatchMouseEvent",
             {"type": "mouseReleased", "x": x, "y": y, "button": "left",
              "clickCount": 1})
    time.sleep(0.4)


def wait_for(cdp, js, timeout, desc):
    deadline = time.time() + timeout
    while time.time() < deadline:
        v = cdp.eval(f"({js})")
        if v:
            return v
        time.sleep(1.0)
    raise CdpError(f"timeout waiting for: {desc}")


def main():
    import argparse
    ap = argparse.ArgumentParser()
    ap.add_argument("--cdp", default="http://127.0.0.1:9222")
    ap.add_argument("--out", default="/tmp/duckai-test")
    ap.add_argument("--timeout", type=float, default=120)
    args = ap.parse_args()
    os.makedirs(args.out, exist_ok=True)

    with urllib.request.urlopen(args.cdp + "/json/list",
                                timeout=15) as r:
        targets = json.load(r)
    pages = [t for t in targets if t.get("type") == "page"]
    if not pages:
        raise CdpError("no page target found")
    cdp = Cdp(pages[0]["webSocketDebuggerUrl"])
    try:
        cdp.call("Page.enable")
        cdp.call("Runtime.enable")
        print("navigate: https://duck.ai", flush=True)
        cdp.call("Page.navigate", {"url": "https://duck.ai"})
        wait_for(cdp, "document.readyState === 'complete'", 30,
                 "page load")
        time.sleep(3)

        # Evtl. Consent-Banner wegklicken (nur wenn sichtbar).
        for phrase in ["Accept", "Agree", "Got it", "Akzeptieren"]:
            try:
                hit = cdp.eval(f"({FIND_CLICK})({phrase!r})", timeout=10)
            except CdpError:
                hit = None
            if hit:
                print(f"consent dismissed: {hit['text']}", flush=True)
                mouse_click(cdp, hit["x"], hit["y"])
                time.sleep(1)
                break

        shot = lambda n: open(  # noqa: E731
            f"{args.out}/{n}.png", "wb").write(base64.b64decode(
                cdp.call("Page.captureScreenshot")["data"]))
        shot("01_landing")

        # "Ask anything privately" ist der Platzhalter des Eingabefelds
        # (kein Button): Klick ins Eingabefeld = Klick auf den Platzhalter.
        print("click: 'Ask anything privately' (= chat input)", flush=True)
        box = wait_for(cdp, f"({FIND_INPUT})()", 30, "chat input")
        print(f"  input @ ({box['x']:.0f},{box['y']:.0f})", flush=True)
        mouse_click(cdp, box["x"], box["y"])
        question = "Tell me a joke about programming"
        cdp.call("Input.insertText", {"text": question})
        time.sleep(1.5)
        shot("02_question_typed")

        print("click: 'Ask' button (bottom right of input)", flush=True)
        ask = wait_for(
            cdp, f"({FIND_SUBMIT})({box['top']})", 30, "'Ask' button")
        print(f"  found via {ask['how']}: {ask['text']!r} @ "
              f"({ask['x']:.0f},{ask['y']:.0f})", flush=True)
        mouse_click(cdp, ask["x"], ask["y"])

        print("wait: answer", flush=True)
        deadline = time.time() + args.timeout
        text = ""
        while time.time() < deadline:
            time.sleep(3)
            try:
                text = dom_text(cdp)
            except CdpError as e:
                print(f"  (dom read retry: {e})", flush=True)
                continue
            low = text.lower()
            if "anonymized by duckduckgo" in low and (
                    "dark mode" in low or "programmer" in low
                    or "bug" in low):
                break
        else:
            shot("99_timeout")
            raise CdpError("answer did not arrive in time.\n"
                           f"--- page text (tail) ---\n{text[-2000:]}")
        shot("03_answer")

        with open(f"{args.out}/answer.txt", "w") as f:
            f.write(text)
        print("--- answer (tail) ---", flush=True)
        print(text[-1500:], flush=True)
        assert "anonymized by duckduckgo" in text.lower(), \
            "anonymity notice missing"
        print("PASS: anonymity notice + joke received", flush=True)
    finally:
        cdp.close()


if __name__ == "__main__":
    try:
        main()
    except (CdpError, AssertionError, OSError) as e:
        print(f"FAIL: {e}", flush=True)
        sys.exit(1)
