#!/bin/bash
# smoke_xvfb.sh — Xvfb-Rauchtest für das source9-UI.
#
# Was er beweist:
#   1. Das Fenster startet unter einem virtuellen X-Server (Software-GL),
#      zeigt gerenderte Schrift und OCRt sie live (Worker-Thread).
#   2. Tasten wirken: Right (Sprache), G (Generator), V/V (Boxen/nichts),
#      M (Modell), Up (Größe), Space (Pause), N (ein Sample).
#      Screenshots halten Text-, Boxen- und Nichts-Modus fest.
#   3. Gehaltenes `Escape` beendet sauber (Exit 0 + Report auf stdout).
#
# Aufruf (aus source9/ oder beliebig):
#   ./scripts/smoke_xvfb.sh [target/debug/unicode_ocr] [out-dir]
#
# Voraussetzungen (einmalig): siehe scripts/README.md.
set -u

SRC_DIR="$(cd "$(dirname "$0")/.." && pwd)"
BIN="${1:-target/debug/unicode_ocr}"
OUT="${2:-/tmp/unicode-smoke}"
DISP="${DISPLAY_NUM:-99}"

[[ "$BIN" = /* ]] || BIN="$SRC_DIR/$BIN"
mkdir -p "$OUT"
# scrot schreibt nie über existierende Dateien (weicht auf _NNN aus),
# daher alte Shots vorab entfernen.
rm -f "$OUT/text.png" "$OUT/boxes.png" "$OUT/hidden.png"
export DISPLAY=":$DISP"

cleanup() {
  kill "$VIEWER" "$XVFB" 2>/dev/null
}
trap cleanup EXIT

Xvfb ":$DISP" -screen 0 1280x1024x24 >"$OUT/xvfb.log" 2>&1 &
XVFB=$!
sleep 2

# Modelle/Korpus liegen relativ zu source9/; dort starten.
cd "$SRC_DIR"
# Software-GL: Xvfb hat keine Hardware-Beschleunigung.
LIBGL_ALWAYS_SOFTWARE=1 "$BIN" >"$OUT/stdout.log" 2>"$OUT/stderr.log" &
VIEWER=$!
sleep 20 # Warmup + erste Samples (Debug-Build)

# Ohne Window-Manager schlägt Fokussieren fehl; Tasten gehen daher per
# --window direkt ans Fenster.
WIN=$(xdotool search --name "Unicode OCR Round Trip" | head -n 1)
echo "viewer window: $WIN"
test -n "$WIN"
scrot "$OUT/text.png" # Modus Text (Startzustand)

xdotool key --window "$WIN" Right; sleep 1 # fr
xdotool key --window "$WIN" g; sleep 1 # words
xdotool key --window "$WIN" v; sleep 2 # Boxen
scrot "$OUT/boxes.png"
xdotool key --window "$WIN" v; sleep 2 # nichts
scrot "$OUT/hidden.png"
xdotool key --window "$WIN" m; sleep 1 # universal
xdotool key --window "$WIN" Up; sleep 1 # größer
xdotool key --window "$WIN" space; sleep 1 # Pause
xdotool key --window "$WIN" n; sleep 3 # ein Sample in Pause

# Escape halten: Einzel-Taps können bei langsamen (Debug-)Frames
# in eine Frame-Lücke fallen; gehalten trifft garantiert.
xdotool keydown --window "$WIN" Escape
sleep 2
xdotool keyup --window "$WIN" Escape
wait "$VIEWER"
code=$?
echo "viewer exit: $code"

echo "--- Report auf stdout ---"
grep -c "| Lang | Modell |" "$OUT/stdout.log"
echo "--- Artefakte ---"
ls -la "$OUT/text.png" "$OUT/boxes.png" "$OUT/hidden.png"

trap - EXIT
kill "$XVFB" 2>/dev/null
exit "$code"
