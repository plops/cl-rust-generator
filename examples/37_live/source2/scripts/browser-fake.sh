#!/usr/bin/env bash
# browser-fake.sh — automatischer Browser-Test ohne echtes Gemini/Netz.
# Startet fake_gemini (cargo example) + live-silk (--gemini-url auf den Fake,
# --key-file mit Dummy-Key) und faehrt browser-fake.js (zwei Tabs: A Opus,
# B PCM). Werkzeuge aus $HIL_DIR (scripts/browser-setup.sh vorher ausfuehren).
set -euo pipefail

HERE="$(dirname "$0")"
ROOT="$(cd "$HERE/.." && pwd)"
HIL_DIR="${HIL_DIR:-/tmp/live-hil}"
PORT="${PORT:-3311}"
FAKEPORT="${FAKEPORT:-8900}"
export HIL_BASE="http://127.0.0.1:$PORT"
export FAKE_WS="ws://127.0.0.1:$FAKEPORT"
export HIL_ROOM="${HIL_ROOM:-fake-room-1}"
export HIL_SECONDS="${HIL_SECONDS:-20}"
export HIL_OUT="${HIL_OUT:-$HIL_DIR}"
export HIL_CHROME="$HIL_DIR/chrome-linux64/chrome"
export HIL_WAV="$HIL_DIR/mic-zh.wav"
export NODE_PATH="$HIL_DIR/node_modules:${NODE_PATH:-}"

[ -x "$HIL_CHROME" ] || { echo "FEHLT Chrome: scripts/browser-setup.sh ausfuehren"; exit 2; }
[ -f "$HIL_WAV" ] || { echo "FEHLT Fake-Mikro: scripts/browser-setup.sh ausfuehren"; exit 2; }
[ -d "$HIL_DIR/node_modules/playwright-core" ] || { echo "FEHLT Treiber: scripts/browser-setup.sh ausfuehren"; exit 2; }
mkdir -p "$HIL_OUT"

# Dummy-Key-Datei (nie echter Key) — im Fake-Modus egal, Server reicht ihn
# nur an den Fake weiter, der ihn ignoriert.
KEYFILE="$(mktemp)"
echo "dummy-fake-key" > "$KEYFILE"

echo "-- baue live-silk + fake_gemini ..."
( cd "$ROOT" && cargo build --release 2>&1 | tail -1 )
( cd "$ROOT" && cargo build --release --example fake_gemini 2>&1 | tail -1 )

FAKE_PID=""; SRV_PID=""
cleanup() {
  [ -n "$SRV_PID" ] && kill "$SRV_PID" 2>/dev/null || true
  [ -n "$FAKE_PID" ] && kill "$FAKE_PID" 2>/dev/null || true
  rm -f "$KEYFILE"
}
trap cleanup EXIT

echo "-- starte fake_gemini auf :$FAKEPORT ..."
"$ROOT/target/release/examples/fake_gemini" --listen "127.0.0.1:$FAKEPORT" >>"$HIL_OUT/fake.log" 2>&1 &
FAKE_PID=$!

echo "-- starte live-silk auf :$PORT (gemini → fake) ..."
"$ROOT/target/release/live-silk" --listen "127.0.0.1:$PORT" --dir "$ROOT/public" \
  --gemini-url "$FAKE_WS" --key-file "$KEYFILE" >>"$HIL_OUT/relay.log" 2>&1 &
SRV_PID=$!

for _ in $(seq 1 50); do curl -sf "$HIL_BASE/healthz" >/dev/null 2>&1 && break; sleep 0.2; done
curl -sf "$HIL_BASE/healthz" >/dev/null 2>&1 || { echo "relay startet nicht"; cat "$HIL_OUT/relay.log"; exit 1; }
echo "-- relay live: $HIL_BASE"

set +e
node "$HERE/browser-fake.js"
RC=$?
set -e
exit $RC
