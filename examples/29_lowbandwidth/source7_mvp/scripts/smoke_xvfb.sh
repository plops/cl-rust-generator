#!/bin/sh
# E2E-Smoke: Xvfb + xterm + lbw-server (echte Modelle) + Headless-Probe.
# Aufruf aus source7_mvp/: ./scripts/smoke_xvfb.sh
# Env: DISPLAY_NR (Default 99), PORT (Default 17878),
#      MODELS (Default ../source6/models).
set -eu

DISPLAY_NR="${DISPLAY_NR:-99}"
PORT="${PORT:-17878}"
MODELS="${MODELS:-../source6/models}"
DISP=":$DISPLAY_NR"

for f in PP-OCRv6_small_det.onnx PP-OCRv6_small_rec.onnx inference.yml; do
  if [ ! -f "$MODELS/$f" ]; then
    echo "smoke: Modell fehlt: $MODELS/$f (vgl. source6/scripts/fetch_models.sh)" >&2
    exit 2
  fi
done

cleanup() {
  kill "$SERVER_PID" "$XTERM_PID" "$XEV_PID" 2>/dev/null || true
  # Xvfb erst nach den Clients beenden.
  sleep 0.5
  kill "$XVFB_PID" 2>/dev/null || true
}
trap cleanup EXIT

Xvfb "$DISP" -screen 0 1280x1024x24 &
XVFB_PID=$!
sleep 1
# xev oben (fängt den Probe-Klick bei 100,100), xterm mit Text darunter.
stdbuf -o0 -e0 env DISPLAY="$DISP" xev -geometry 640x160+0+0 > /tmp/smoke_xev.log 2>&1 &
XEV_PID=$!
DISPLAY="$DISP" xterm -geometry 100x30+0+200 -e sh -c 'echo SMOKE-TEST-640; exec sleep 300' &
XTERM_PID=$!
sleep 1

cargo build --release -p lbw-server -p lbw-client 2>&1 | tail -n 2
DISPLAY="$DISP" ./target/release/lbw-server \
  --listen "127.0.0.1:$PORT" --models "$MODELS" -v > /tmp/smoke_server.log 2>&1 &
SERVER_PID=$!
sleep 2

cargo run --release -p lbw-client --example probe -- "127.0.0.1:$PORT" 2>&1 | tail -n 12

if grep -q "Session-Fehler" /tmp/smoke_server.log; then
  echo "smoke: FEHLER — Server meldet Session-Fehler:" >&2
  grep "Session-Fehler" /tmp/smoke_server.log >&2
  exit 1
fi
if ! grep -q "ButtonPress" /tmp/smoke_xev.log; then
  echo "smoke: FEHLER — kein Mausklick im xev-Fenster angekommen" >&2
  exit 1
fi
if ! grep -q "\[input\] Button" /tmp/smoke_server.log; then
  echo "smoke: FEHLER — Server loggt keine Eingabe-Events (-v)" >&2
  exit 1
fi
if ! grep -q "Monitor-Ursprung +0+0" /tmp/smoke_server.log; then
  echo "smoke: FEHLER — Server loggt keinen RandR-Monitor-Ursprung" >&2
  exit 1
fi
echo "smoke: OK (Server-Log: /tmp/smoke_server.log)"
