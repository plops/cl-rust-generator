#!/usr/bin/env bash
# hil-browser.sh — Chrome-HIL: ein Tab spricht (Fake-Mikro), Peer B hoert zu.
# Startet bei Bedarf ein eigenes Relay (sonst wird ein laufendes benutzt).
# Umgebung (alles mit Default):
# HIL_DIR (Werkzeuge, Default /tmp/live-hil), PORT (Default 3201),
# HIL_ROOM, HIL_MODE (Default zh-to-de), HIL_SECONDS (Default 75),
# HIL_KEY_FILE (Default ../../api_key.txt neben cl-rust-generator),
# HIL_OUT (Screenshots/Logs). Key wird nie geloggt (nur maskiert).
set -euo pipefail

HERE="$(dirname "$0")"
HIL_DIR="${HIL_DIR:-/tmp/live-hil}"
PORT="${PORT:-3201}"
export HIL_BASE="http://127.0.0.1:$PORT"
export HIL_ROOM="${HIL_ROOM:-hil-zh-de-1}"
export HIL_MODE="${HIL_MODE:-zh-to-de}"
export HIL_SECONDS="${HIL_SECONDS:-75}"
export HIL_OUT="${HIL_OUT:-$HIL_DIR}"
export HIL_KEY_FILE="${HIL_KEY_FILE:-/workspace/src/cl-rust-generator/api_key.txt}"
export HIL_CHROME="$HIL_DIR/chrome-linux64/chrome"
export HIL_WAV="$HIL_DIR/mic-zh.wav"
export NODE_PATH="$HIL_DIR/node_modules:${NODE_PATH:-}"

[ -f "$HIL_KEY_FILE" ] || { echo "FEHLT Key-Datei: $HIL_KEY_FILE (HIL_KEY_FILE setzen)"; exit 2; }
[ -x "$HIL_CHROME" ] || { echo "FEHLT Chrome: HIL_DIR/hil-setup.sh ausfuehren"; exit 2; }
[ -f "$HIL_WAV" ] || { echo "FEHLT Fake-Mikro: HIL_DIR/hil-setup.sh ausfuehren"; exit 2; }
[ -d "$HIL_DIR/node_modules/playwright-core" ] || { echo "FEHLT Treiber: HIL_DIR/hil-setup.sh ausfuehren"; exit 2; }

MINE=""
if ! curl -sf "$HIL_BASE/healthz" >/dev/null 2>&1; then
  echo "-- baue + starte Relay auf :$PORT ..."
  (cd "$HERE/.." && cargo build --release 2>&1 | tail -1)
  setsid -f "$HERE/../target/release/live-relay" --listen "127.0.0.1:$PORT" --dir "$HERE/../public" </dev/null >>"$HIL_OUT/relay.log" 2>&1
  MINE="ja"
  for i in $(seq 1 50); do curl -sf "$HIL_BASE/healthz" >/dev/null 2>&1 && break; sleep 0.2; done
fi
echo "-- relay: $HIL_BASE (eigenes: ${MINE:-nein})"

set +e
node "$HERE/hil-driver.js"
RC=$?
set -e
[ -n "$MINE" ] && pkill -f "live-relay.*$PORT" 2>/dev/null || true
exit $RC
