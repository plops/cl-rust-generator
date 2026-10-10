#!/bin/sh
# E2E-Smoke mit Recording: Xvfb + xterm + lbw-server --record + Probe --record,
# danach lbw-logstat über beide Logs + lbw-replay des Server-Logs.
# Aufruf aus source10_instrumentation/: ./scripts/smoke_record.sh
# Env: DISPLAY_NR (Default 98), PORT (Default 17879), MODELS (Default models).
set -eu

DISPLAY_NR="${DISPLAY_NR:-98}"
PORT="${PORT:-17879}"
MODELS="${MODELS:-models}"
DISP=":$DISPLAY_NR"
SRV_LOG="/tmp/smoke_rec_server.lbwlog"
CLI_LOG="/tmp/smoke_rec_client.lbwlog"

for f in PP-OCRv6_small_det.onnx PP-OCRv6_small_rec.onnx inference.yml; do
  if [ ! -f "$MODELS/$f" ]; then
    echo "smoke_record: Modell fehlt: $MODELS/$f" >&2
    exit 2
  fi
done
rm -f "$SRV_LOG" "$CLI_LOG"

cleanup() {
  kill "$SERVER_PID" "$XTERM_PID" "$XEV_PID" 2>/dev/null || true
  sleep 0.5
  kill "$XVFB_PID" 2>/dev/null || true
}
trap cleanup EXIT

Xvfb "$DISP" -screen 0 1280x720x24 &
XVFB_PID=$!
sleep 1
stdbuf -o0 -e0 env DISPLAY="$DISP" xev -geometry 640x160+0+0 > /tmp/smoke_rec_xev.log 2>&1 &
XEV_PID=$!
DISPLAY="$DISP" xterm -geometry 100x30+0+200 -e sh -c 'echo SMOKE-RECORD-720; exec sleep 300' &
XTERM_PID=$!
sleep 1

cargo build --release -p lbw-server -p lbw-client -p lbw-log 2>&1 | tail -n 2
DISPLAY="$DISP" ./target/release/lbw-server \
  --listen "127.0.0.1:$PORT" --models "$MODELS" --record "$SRV_LOG" -v > /tmp/smoke_rec_server.log 2>&1 &
SERVER_PID=$!
sleep 2

cargo run --release -p lbw-client --example probe -- "127.0.0.1:$PORT" 3 --record "$CLI_LOG" 2>&1 | tail -n 14

STAT_OUT="$(./target/release/lbw-logstat "$SRV_LOG" "$CLI_LOG" 2>&1)"
echo "$STAT_OUT" | tail -n 30
echo "$STAT_OUT" | grep -q "Frames: " || { echo "smoke_record: FEHLER — keine Frames im Server-Log" >&2; exit 1; }
echo "$STAT_OUT" | grep -q "Decode (Client)" || { echo "smoke_record: FEHLER — keine Decode-Records" >&2; exit 1; }
./target/release/lbw-logstat --json "$SRV_LOG" "$CLI_LOG" | grep -q '"clock_offset_ms"' || { echo "smoke_record: FEHLER — JSON ohne clock_offset" >&2; exit 1; }

REPLAY_OUT="$(./target/release/lbw-replay "$SRV_LOG" 2>&1)"
echo "$REPLAY_OUT" | tail -n 3
echo "$REPLAY_OUT" | grep -q "Kacheln" || { echo "smoke_record: FEHLER — Replay ohne Kacheln" >&2; exit 1; }
echo "$REPLAY_OUT" | grep -q "(0 Fehler" || { echo "smoke_record: FEHLER — Replay mit Fehlern" >&2; exit 1; }

if grep -q "Session-Fehler" /tmp/smoke_rec_server.log; then
  echo "smoke_record: FEHLER — Server meldet Session-Fehler" >&2
  exit 1
fi
if ! grep -q "ButtonPress" /tmp/smoke_rec_xev.log; then
  echo "smoke_record: FEHLER — kein Mausklick im xev-Fenster angekommen" >&2
  exit 1
fi
echo "smoke_record: OK (Logs: $SRV_LOG $CLI_LOG)"
