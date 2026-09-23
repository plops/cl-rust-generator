#!/bin/bash
# smoke_xvfb.sh — Xvfb-Rauchtest für den source5-Live-OCR-Viewer.
#
# Was er beweist:
#   1. Der Viewer startet unter einem virtuellen X-Server (braucht keine GPU,
#      dank Software-GL), zeigt sein Fenster und OCRt echten Bildschirmtext.
#   2. Pfeiltasten verschieben die ROI, `1`/`2` zoomen (HUD vorher/nachher
#      in den Screenshots vergleichen).
#   3. `Escape` beendet das Programm sauber (Exit-Code 0).
#
# Aufruf (aus dem source5-Verzeichnis):
#   ./scripts/smoke_xvfb.sh [target/debug/x11_ppocrv6] [out-dir]
#
# Beispiele:
#   ./scripts/smoke_xvfb.sh
#   ./scripts/smoke_xvfb.sh target/release/x11_ppocrv6 /tmp/ocr-smoke
#
# Voraussetzungen (einmalig): siehe scripts/README.md.
set -u

BIN="${1:-target/debug/x11_ppocrv6}"
OUT="${2:-/tmp/ocr-smoke}"
DISP="${DISPLAY_NUM:-99}"

mkdir -p "$OUT"
# scrot schreibt nie über existierende Dateien (weicht auf _NNN aus),
# daher alte Shots vorab entfernen.
rm -f "$OUT/before.png" "$OUT/after.png"
export DISPLAY=":$DISP"

cleanup() {
  kill "$VIEWER" "$XTERM" "$XVFB" 2>/dev/null
}
trap cleanup EXIT

Xvfb ":$DISP" -screen 0 1280x1024x24 >"$OUT/xvfb.log" 2>&1 &
XVFB=$!
sleep 2

xterm -geometry 90x24+0+0 -fa Monospace -fs 24 -bg white -fg black \
  -e bash -c 'printf "HELLO OCR WORLD 123\nSECOND LINE ABC xyz\n"; sleep 300' &
XTERM=$!
sleep 2

# Software-GL: Xvfb hat keine Hardware-Beschleunigung.
LIBGL_ALWAYS_SOFTWARE=1 "$BIN" >"$OUT/stdout.log" 2>"$OUT/stderr.log" &
VIEWER=$!
sleep 15 # Session-Aufbau + erste Inferenz (Debug-Build)

# Ohne Window-Manager schlägt Fokussieren fehl; Tasten gehen daher per
# --window direkt ans Viewer-Fenster.
WIN=$(xdotool search --name "PP-OCRv6" | head -n 1)
echo "viewer window: $WIN"
test -n "$WIN"
scrot "$OUT/before.png"

# Pan: 5x rechts, 3x runter (Schritt 40 px bei ROI 640) -> (200,120).
for _ in $(seq 1 5); do xdotool key --window "$WIN" Right; sleep 0.5; done
for _ in $(seq 1 3); do xdotool key --window "$WIN" Down; sleep 0.5; done
sleep 5
# Zoom hinein: 640 -> 480.
xdotool key --window "$WIN" 1
sleep 8
scrot "$OUT/after.png"
# Zoom heraus: 480 -> 640.
xdotool key --window "$WIN" 2
sleep 5

# Escape kurz halten: Einzel-Taps können bei langsamen (Debug-)Frames
# in eine Frame-Lücke fallen; gehalten trifft garantiert.
xdotool keydown --window "$WIN" Escape
sleep 2
xdotool keyup --window "$WIN" Escape
wait "$VIEWER"
code=$?
echo "viewer exit: $code"

echo "--- OCR-Treffer auf Testtext ---"
grep -c "HELLO\|HEI0\|SECOND" "$OUT/stdout.log"
echo "--- Artefakte ---"
ls -la "$OUT/before.png" "$OUT/after.png"

trap - EXIT
kill "$XTERM" "$XVFB" 2>/dev/null
exit "$code"
