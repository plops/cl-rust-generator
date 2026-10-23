#!/usr/bin/env bash
# hil-content.sh — bidirektionaler Inhalts-Nachweis ohne Browser:
# Gemini-TTS-Referenz -> Live-Uebersetzung -> Relay -> Gemini-STT-Check.
# Braucht Netz + Key (HIL_KEY_FILE), Relay (wird bei Bedarf gestartet).
# Cloud-TTS/STT (403 ohne Console-Freischaltung) werden NICHT benutzt.
set -euo pipefail

HERE="$(dirname "$0")"
HIL_DIR="${HIL_DIR:-/tmp/live-hil}"
PORT="${PORT:-3201}"
export HIL_BASE="http://127.0.0.1:$PORT"
export HIL_OUT="${HIL_OUT:-$HIL_DIR}"
export HIL_KEY_FILE="${HIL_KEY_FILE:-/workspace/src/cl-rust-generator/api_key.txt}"
export NODE_PATH="$HIL_DIR/node_modules:${NODE_PATH:-}"

[ -f "$HIL_KEY_FILE" ] || { echo "FEHLT Key-Datei: $HIL_KEY_FILE"; exit 2; }
command -v ffmpeg >/dev/null || { echo "FEHLT: ffmpeg"; exit 2; }
[ -d "$HIL_DIR/node_modules/ws" ] || { echo "FEHLT Treiber: HIL_DIR/hil-setup.sh ausfuehren"; exit 2; }

MINE=""
if ! curl -sf "$HIL_BASE/healthz" >/dev/null 2>&1; then
  echo "-- baue + starte Relay auf :$PORT ..."
  (cd "$HERE/.." && cargo build --release 2>&1 | tail -1)
  setsid -f "$HERE/../target/release/live-relay" --listen "127.0.0.1:$PORT" --dir "$HERE/../public" </dev/null >>"$HIL_OUT/relay.log" 2>&1
  MINE="ja"
  for i in $(seq 1 50); do curl -sf "$HIL_BASE/healthz" >/dev/null 2>&1 && break; sleep 0.2; done
fi

set +e
node "$HERE/hil-content.js"
RC=$?
set -e
[ -n "$MINE" ] && pkill -f "live-relay.*$PORT" 2>/dev/null || true
exit $RC
