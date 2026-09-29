#!/bin/bash
# make_screens.sh — erzeugt GUI-Screenshots unter Xvfb (1920×1080) als PPM:
# Kalibrierdaten für die INT8-Quantisierung und Testbilder für den Smoke.
#
# Aufruf (aus source8/):  ./scripts/make_screens.sh [BINARY] [OUT-DIR]
# Default: target/release/gui_detect, models/screens/
# Voraussetzungen: apt-get install xvfb xterm x11-apps xdotool
set -euo pipefail

BIN="${1:-target/release/gui_detect}"
OUT="${2:-models/screens}"
DISP="${DISPLAY_NUM:-97}"
mkdir -p "$OUT"
export DISPLAY=":$DISP"

Xvfb ":$DISP" -screen 0 1920x1080x24 >/dev/null 2>&1 &
XVFB=$!
PIDS=()
cleanup() { kill "${PIDS[@]}" "$XVFB" 2>/dev/null || true; }
trap cleanup EXIT
sleep 1.5

spawn() { "$@" >/dev/null 2>&1 & PIDS+=($!); }
clear_apps() { kill "${PIDS[@]}" 2>/dev/null || true; PIDS=(); sleep 0.5; }

# Szene 1: Terminals + Rechner + Uhr (hell/dunkel gemischt).
spawn xterm -geometry 100x30+10+10 -bg white -fg black -fa Monospace -fs 11 \
  -e bash -c 'ls -la /usr/bin | head -60; sleep 600'
spawn xterm -geometry 80x20+1000+500 -bg black -fg green -fa Monospace -fs 13 \
  -e bash -c 'top -b -n 1 | head -40; sleep 600'
spawn xcalc -geometry 260x380+1500+40
spawn xclock -geometry 200x200+1000+60 -update 1
sleep 2; "$BIN" grab "$OUT/scene1.ppm"; clear_apps

# Szene 2: viele kleine Fenster/Icons-artige Elemente.
for i in $(seq 0 7); do
  spawn xlogo -geometry 120x120+$((40 + i * 230))+40
  spawn xeyes -geometry 120x80+$((40 + i * 230))+220
done
spawn xcalc -geometry 300x420+40+360
spawn xcalc -geometry 300x420+380+360 -rpn
spawn xterm -geometry 90x25+760+360 -bg '#202830' -fg '#e0e0e0' -fa Monospace -fs 12 \
  -e bash -c 'man ls 2>/dev/null | head -40 || cat /etc/os-release; sleep 600'
sleep 2; "$BIN" grab "$OUT/scene2.ppm"; clear_apps

# Szene 3: Editor-artiges Terminal-Layout mit Farben + Uhr + Rechner.
spawn xterm -geometry 210x60+0+0 -bg '#fdf6e3' -fg '#073642' -fa Monospace -fs 10 \
  -e bash -c 'for f in /etc/*.conf; do echo "== $f"; head -5 "$f"; done; sleep 600'
spawn xcalc -geometry 240x340+1640+700
spawn xclock -geometry 150x150+1700+40 -digital -update 1
sleep 2; "$BIN" grab "$OUT/scene3.ppm"; clear_apps

ls -la "$OUT"
