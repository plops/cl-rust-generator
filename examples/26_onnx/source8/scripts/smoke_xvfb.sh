#!/bin/bash
# smoke_xvfb.sh — End-to-end-Nachweis unter Xvfb: echte X11-Fenster
# (xcalc, xterm, xclock) → `gui_detect detect <model> x11` → Boxen > 0,
# annotiertes PPM, Exit-Code 0. Danach kurzer X11-End-to-end-Bench und
# 20 Frames Live-Modus (eigenes Fenster).
#
# Aufruf (aus source8/):  ./scripts/smoke_xvfb.sh [MODEL] [OUT-DIR]
set -euo pipefail

MODEL="${1:-models/gpa_384x640_int8.onnx}"
OUT="${2:-/tmp/gui-detect-smoke}"
BIN="${BIN:-target/release/gui_detect}"
DISP="${DISPLAY_NUM:-98}"
mkdir -p "$OUT"
export DISPLAY=":$DISP"

Xvfb ":$DISP" -screen 0 1920x1080x24 >/dev/null 2>&1 &
PIDS=($!)
trap 'kill "${PIDS[@]}" 2>/dev/null || true' EXIT
sleep 1.5
xcalc -geometry 300x420+100+100 >/dev/null 2>&1 & PIDS+=($!)
xclock -geometry 200x200+500+100 >/dev/null 2>&1 & PIDS+=($!)
xterm -geometry 80x24+800+100 -e bash -c 'ls -la /; sleep 120' >/dev/null 2>&1 & PIDS+=($!)
sleep 2

"$BIN" detect "$MODEL" x11 --device cpu --out "$OUT/annotated.ppm" >"$OUT/boxes.tsv"
n=$(awk '$5 >= 0.25' "$OUT/boxes.tsv" | wc -l)
echo "boxen (score >= 0.25): $n"
test "$n" -gt 0
"$BIN" bench x11 "$MODEL" --device cpu --iters 10 --warmup 2

# Live-Modus: 640×640-Ausschnitt ohne Skalierung, Fenster daneben, 20 Frames.
"$BIN" live models/gpa_640_int8.onnx --device cpu --frames 20 | tee "$OUT/live.txt"
grep -q "frames=20" "$OUT/live.txt"
echo "ok: $OUT/annotated.ppm"
