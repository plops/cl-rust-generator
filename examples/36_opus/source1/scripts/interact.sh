#!/bin/sh
# HIL: Viewer-Interaktion (xdotool) unter Xvfb + Screenshots + Asserts.
# Aufruf: scripts/interact.sh  (braucht xdotool, scrot, Xvfb)
set -eu
cd "$(dirname "$0")/.."
BIN=target/release/opus-vis
DEMO=/tmp/hil_demo.opus
./scripts/mkdemo.sh "$DEMO" >/dev/null
rm -f /tmp/hil_*.png

Xvfb :42 -screen 0 1400x900x24 >/dev/null 2>&1 &
XVFB=$!
trap 'kill $XVFB 2>/dev/null || true' EXIT INT TERM
sleep 1
export DISPLAY=:42

# timeout = Fangnetz (Escape muss normal beenden, sonst Fail statt Hang).
timeout 90 ./$BIN "$DEMO" --show --frames 100000 -o /tmp/hil.png &
VIEWER=$!
sleep 2
WID=$(xdotool search --name "opus-vis" | head -n 1)
echo "fenster: $WID"
xdotool windowfocus "$WID"
eval "$(xdotool getwindowgeometry --shell "$WID")"
# Plot-Mitte in Screen-Koords (Plot bei +56/+30, 1200x256).
PX=$((X + 56 + 600))
PY=$((Y + 30 + 128))
echo "plot-mitte: $PX,$PY"

echo "== start (ohne overlay) =="
xdotool mousemove 5 5 sleep 1
scrot /tmp/hil_base.png

echo "== hover =="
xdotool mousemove "$PX" "$PY" sleep 1
scrot /tmp/hil_hover.png

echo "== rad-zoom hinein (5x hoch) =="
xdotool click --repeat 5 --delay 100 4 sleep 1
scrot /tmp/hil_zoom.png

echo "== drag-pan nach links =="
xdotool mousedown 1 mousemove_relative -- -300 0 sleep 0.5 mouseup 1 sleep 1
scrot /tmp/hil_pan.png

echo "== reset (R halten) =="
xdotool keydown --window "$WID" R sleep 0.4
xdotool keyup --window "$WID" R sleep 0.5 || true
scrot /tmp/hil_reset.png

echo "== ende (Escape halten) =="
xdotool keydown --window "$WID" Escape sleep 0.4
# keyup kann fehlschlagen (Fenster schon zu) — kein Fehler.
xdotool keyup --window "$WID" Escape || true
wait "$VIEWER"
echo "viewer-exit: 0"

echo "== asserts (bilder unterscheiden sich) =="
differ() {
  if cmp -s "$1" "$2"; then
    echo "GLEICH (fehler): $1 $2"
    exit 1
  else
    echo "ok: $1 != $2"
  fi
}
differ /tmp/hil_base.png /tmp/hil_hover.png
differ /tmp/hil_hover.png /tmp/hil_zoom.png
differ /tmp/hil_zoom.png /tmp/hil_pan.png
differ /tmp/hil_pan.png /tmp/hil_reset.png
echo "shots: /tmp/hil_*.png"
