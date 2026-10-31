#!/bin/sh
# HIL-Smoke (source10_b): Xvfb + xterm + echte CPU-Modelle + lbw-server
# hinter einem 6-kB/s-Drossel-Proxy. Während im xterm ein Bildlauf läuft
# (großer Bildwechsel), wird per xdotool getippt; der Probe-Client nimmt
# auf, logstat wertet Erst-Echo-Latenz und Kanal-Simulation aus.
# Aufruf aus source10_instrumentation_b/: ./scripts/smoke_hil.sh
# Env: DISPLAY_NR (97), PORT (17881), BUDGET (6000), MODELS (models),
# SERVER_BIN (Default ./target/release/lbw-server; z. B. source10 für A/B),
# ANIM (1 = ffplay-Testbild-Animation 400×300 als Bildlast, wie die
# Browser-Animation aus dem 0614-Log; 0 = nur xterm-Bildlauf).
set -eu

DISPLAY_NR="${DISPLAY_NR:-97}"
PORT="${PORT:-17881}"
PROXY_PORT=$((PORT + 1))
BUDGET="${BUDGET:-6000}"
MODELS="${MODELS:-models}"
DISP=":$DISPLAY_NR"
SRV_LOG="/tmp/smoke_hil_server.lbwlog"
CLI_LOG="/tmp/smoke_hil_client.lbwlog"

for f in PP-OCRv6_small_det.onnx PP-OCRv6_small_rec.onnx inference.yml; do
  [ -f "$MODELS/$f" ] || { echo "smoke_hil: Modell fehlt: $MODELS/$f" >&2; exit 2; }
done
for b in Xvfb xterm xdotool; do
  command -v "$b" >/dev/null || { echo "smoke_hil: $b fehlt (apt install xvfb xterm xdotool)" >&2; exit 2; }
done
rm -f "$SRV_LOG" "$CLI_LOG"

cleanup() {
  kill "${SERVER_PID:-}" "${PROXY_PID:-}" "${XTERM_PID:-}" "${TYPER_PID:-}" "${ANIM_PID:-}" 2>/dev/null || true
  sleep 0.5
  kill "${XVFB_PID:-}" 2>/dev/null || true
}
trap cleanup EXIT

cargo build --release -p lbw-server -p lbw-client -p lbw-log --bins --examples 2>&1 | tail -n 1

Xvfb "$DISP" -screen 0 1280x720x24 &
XVFB_PID=$!
sleep 1
# xterm mit Shell: erst still, später Bildlauf + Tippen.
DISPLAY="$DISP" xterm -geometry 120x40+0+0 -fa Monospace -fs 11 -e sh &
XTERM_PID=$!
sleep 1
if [ "${ANIM:-1}" = 1 ] && command -v ffplay >/dev/null; then
  # Startet erst nach dem Verbindungsaufbau (sleep), läuft dann dauerhaft.
  (sleep 6; exec env DISPLAY="$DISP" SDL_AUDIODRIVER=dummy ffplay -loglevel error -an -noborder \
    -left 860 -top 380 -f lavfi -i "testsrc2=size=400x300:rate=10") &
  ANIM_PID=$!
fi

DISPLAY="$DISP" "${SERVER_BIN:-./target/release/lbw-server}" --listen "127.0.0.1:$PORT" --models "$MODELS" \
  --budget "$BUDGET" --record "$SRV_LOG" > /tmp/smoke_hil_server.log 2>&1 &
SERVER_PID=$!
./target/release/examples/throttle "127.0.0.1:$PROXY_PORT" "127.0.0.1:$PORT" "$BUDGET" \
  > /tmp/smoke_hil_proxy.log 2>&1 &
PROXY_PID=$!
sleep 3

# Last: lokal (am Server, nicht über den Kanal) alle paar Sekunden ein
# farbiger Bildlauf im xterm — große Bildwechsel wie die Browser-Animation.
(
  sleep 6
  DISPLAY="$DISP" xdotool mousemove 300 300
  for i in 1 2 3 4 5 6; do
    DISPLAY="$DISP" xdotool type --delay 5 "ls --color=always -la /usr/lib | head -60"
    DISPLAY="$DISP" xdotool key Return
    sleep 4
  done
) &
TYPER_PID=$!

# Probe hält 30 s die Verbindung (über den Proxy), tippt dabei Wörter
# (Client→Server-Pfad, gedrosselt) und nimmt auf.
./target/release/examples/probe "127.0.0.1:$PROXY_PORT" 30 --record "$CLI_LOG" \
  --type "alpha bravo charlie delta echo foxtrot golf hotel" 2>&1 | tail -n 12
wait "$TYPER_PID" 2>/dev/null || true
kill "$SERVER_PID" 2>/dev/null || true
sleep 1

STAT="$(./target/release/lbw-logstat "$SRV_LOG" "$CLI_LOG" 2>&1)"
echo "$STAT" | grep -E "^==|Kanal-Sim|Zustellung|Input→|Erst-Echo|Durchsatz|encode:|det:|rec:" || true
echo "$STAT" | grep -q "Frames: " || { echo "smoke_hil: FEHLER — keine Frames" >&2; exit 1; }
MAXB="$(echo "$STAT" | sed -n 's/.*größte Nachricht \([0-9]*\) B.*/\1/p' | head -n 1)"
[ -n "$MAXB" ] && [ "$MAXB" -le 2440 ] || { echo "smoke_hil: FEHLER — größte Nachricht ${MAXB:-?} B" >&2; exit 1; }
grep -q "Session-Fehler" /tmp/smoke_hil_server.log && { echo "smoke_hil: FEHLER — Session-Fehler" >&2; exit 1; }
echo "smoke_hil: OK (größte Nachricht $MAXB B; Logs $SRV_LOG $CLI_LOG)"
