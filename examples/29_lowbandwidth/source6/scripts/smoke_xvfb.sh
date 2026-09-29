#!/bin/bash
# smoke_xvfb.sh — End-to-end-Nachweis mit echten Modellen auf zwei Displays.
#
#   :99  „entfernter“ Rechner: xterm + lbw-server (Capture 640² bei 0,0)
#   :98  „lokaler“ Rechner:    lbw-client (Software-GL)
#   dazwischen lbw-throttle: 6000 B/s, 50 ms Latenz, 60 s Blackout, Abriss
#
# Nachweise (Exit 0 nur wenn alle gelten):
#   1. OCR-Text des xterm kommt im Client an (--dump-text-Log)
#   2. Tippen im Client-Fenster landet per XTEST im xterm und kommt als Text zurück
#   3. Mausbewegung im Client bewegt den Zeiger auf :99
#   4. Durchsatz je Sekunde ≤ Rate + Burst (Proxy-Log)
#   5. 60 s Blackout: keine Trennung, danach kommt neuer Text an
#   6. Abriss: Client verbindet sich neu (CONNECTED erneut)
#   Screenshots beider Displays in $OUT.
#
# Aufruf (aus source6): ./scripts/smoke_xvfb.sh [out-dir]   (BLACKOUT=10 für kurz)
set -u
cd "$(dirname "$0")/.."
OUT="${1:-/tmp/lbw-smoke}"
RATE="${RATE:-6000}"
BLACKOUT="${BLACKOUT:-60}"
B=target/release
mkdir -p "$OUT"
rm -f "$OUT"/*.png "$OUT"/*.log

cargo build --release -q -p lbw-server -p lbw-client -p lbw-throttle || exit 1
./scripts/fetch_models.sh >/dev/null || exit 1

PIDS=()
cleanup() { kill "${PIDS[@]}" 2>/dev/null; wait 2>/dev/null; }
trap cleanup EXIT

Xvfb :99 -screen 0 1280x1024x24 >"$OUT/xvfb99.log" 2>&1 & PIDS+=($!)
Xvfb :98 -screen 0 800x800x24 >"$OUT/xvfb98.log" 2>&1 & PIDS+=($!)
sleep 2
# xterm füllt den 640²-Ausschnitt; Text steht schon da, bevor der Server startet.
DISPLAY=:99 xterm -geometry 70x30+0+0 -fa Monospace -fs 13 -bg white -fg black \
  -e bash --norc -c 'echo HELLO LOWBANDWIDTH 123; exec bash --norc -i' >"$OUT/xterm.log" 2>&1 & PIDS+=($!)
sleep 3

$B/lbw-server --display :99 --listen 127.0.0.1:17878 --threads 8 -v >"$OUT/server.log" 2>&1 & PIDS+=($!)
T0=$(date +%s)
$B/lbw-throttle --listen 127.0.0.1:17879 --to 127.0.0.1:17878 --rate "$RATE" --delay 50 \
  --blackout "40:$BLACKOUT" --cut-at $((BLACKOUT + 65)) --log >"$OUT/throttle.log" 2>&1 & PIDS+=($!)
sleep 1
DISPLAY=:98 LIBGL_ALWAYS_SOFTWARE=1 $B/lbw-client --connect 127.0.0.1:17879 --dump-text -v \
  >"$OUT/client.log" 2>"$OUT/client.err" & PIDS+=($!)

at() { local d=$(( $1 - ($(date +%s) - T0) )); [ "$d" -gt 0 ] && sleep "$d"; }
FAIL=0
check() { if eval "$2"; then echo "OK   $1"; else echo "FAIL $1"; FAIL=1; fi; }
wait_for() { # timeout-s grep-muster datei
  local end=$(( $(date +%s) + $1 ))
  while [ "$(date +%s)" -lt "$end" ]; do grep -q "$2" "$3" && return 0; sleep 0.5; done
  return 1
}

check "1 OCR-Text erreicht den Client" 'wait_for 30 "LOWBANDWIDTH" "$OUT/client.log"'
DISPLAY=:98 import -window root "$OUT/client_initial.png"

WIN=$(DISPLAY=:98 xdotool search --name lbw-client | head -n1)
echo "Client-Fenster: $WIN"
DISPLAY=:98 xdotool windowfocus "$WIN" 2>/dev/null
at 20
T_TYPE=$(date +%s.%N)
DISPLAY=:98 xdotool type --delay 40 'echo typed via lbw'
DISPLAY=:98 xdotool key Return
check "2 Tippen im Client → xterm → OCR → Client" 'wait_for 20 "\"typed via lbw\"" "$OUT/client.log"'
T_SEEN=$(date +%s.%N)
echo "     Rundreise Tippen→Text: $(echo "$T_SEEN - $T_TYPE" | bc) s (inkl. 0,8 s Tippdauer)"

DISPLAY=:98 xdotool mousemove --window "$WIN" 200 150
sleep 2
LOC=$(DISPLAY=:99 xdotool getmouselocation)
check "3 Maus im Client bewegt Zeiger auf :99 ($LOC)" '[[ "$LOC" == "x:200 y:150 "* ]]'

at 45 # im Blackout tippen: Eingaben stauen sich im Proxy
DISPLAY=:98 xdotool type --delay 40 'echo after blackout'
DISPLAY=:98 xdotool key Return
at $((40 + BLACKOUT + 2))
check "5a Blackout ${BLACKOUT}s ohne Trennung" '! grep -q DISCONNECTED "$OUT/client.log"'
check "5b Text nach dem Blackout" 'wait_for 20 "after blackout\"" "$OUT/client.log"'
DISPLAY=:98 import -window root "$OUT/client_after_blackout.png"
DISPLAY=:99 import -window root -crop 640x640+0+0 "$OUT/server_capture.png"

at $((BLACKOUT + 68))
reconnected() { local end=$(( $(date +%s) + 20 ))
  while [ "$(date +%s)" -lt "$end" ]; do [ "$(grep -c '^CONNECTED' "$OUT/client.log")" -ge 2 ] && return 0; sleep 0.5; done; return 1; }
check "6 Reconnect nach Abriss" reconnected
grep "CONNECTED" "$OUT/client.log" | sed 's/^/     /'

# 7. Schwere Szene: Webseiten-Screenshot (viel Text + Icons) über das xterm.
T7=$(grep -c . "$OUT/client.log")
DISPLAY=:99 display -geometry +0+0 -crop 640x640+0+0 models/test_screen.ppm & PIDS+=($!)
check "7a Text der Webseite kommt an" 'wait_for 30 "Hacker News new" "$OUT/client.log"'
sleep 25
tail -n +"$T7" "$OUT/client.log" >"$OUT/page.log"
TT=$(grep -m1 "Hacker News new" "$OUT/page.log" | grep -o "t=[0-9.]*" | cut -c3-)
TS=$(grep -m1 "^STATS" "$OUT/page.log" | grep -o "t=[0-9.]*" | cut -c3-)
LAST=$(grep "^TILE" "$OUT/page.log" | tail -1 | grep -o "t=[0-9.]*" | cut -c3-)
PB=$(awk '/^TILE/{s+=$6} END{print s+0}' "$OUT/page.log")
echo "     Seite: Text nach $(echo "$TT - $TS + 2" | bc) s (±2 s), letzte Kachel $(echo "$LAST - $TT" | bc) s nach dem Text, $PB B AV1, $(grep -c '^TEXT +' "$OUT/page.log") Textelemente"
DISPLAY=:98 import -window root "$OUT/client_page.png"
DISPLAY=:99 import -window root -crop 640x640+0+0 "$OUT/server_page.png"

MAX=$(grep -o "↓ [0-9]* B/s" "$OUT/throttle.log" | awk '{print $2}' | sort -n | tail -1)
check "4 max. Durchsatz ${MAX} B/s ≤ $((RATE + 512))" '[ "${MAX:-0}" -le $((RATE + 512)) ]'
TOTAL=$(grep -c "^TILE" "$OUT/client.log"); TB=$(awk '/^TILE/{s+=$6} END{print s+0}' "$OUT/client.log")
echo "     Kacheln: $TOTAL ($TB B AV1), Textelemente: $(grep -c '^TEXT +' "$OUT/client.log")"
grep "\[pipe\]" "$OUT/server.log" | head -4 | sed 's/^/     /'
exit $FAIL
