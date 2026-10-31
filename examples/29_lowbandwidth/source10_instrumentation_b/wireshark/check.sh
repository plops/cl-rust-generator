#!/bin/sh
# Prüft den LBW-Dissector: bauen, sample.pcap zerlegen, alle 10 Varianten
# per tshark-Feld behaupten (exakte Multimenge — fängt auch fehlende oder
# doppelte Nachrichten). Braucht tshark + libwireshark-dev (der Version
# wegen); fehlt das, Exit 2 mit Hinweis (wie Modell-Checks in den Smokes).
# Als root läuft tshark als `nobody` (root ignoriert User-Plugins).
# Aufruf aus source10_instrumentation/wireshark/.
set -eu
cd "$(dirname "$0")"
command -v tshark >/dev/null || { echo "check: tshark fehlt (apt install tshark)" >&2; exit 2; }
pkg-config --exists wireshark || { echo "check: libwireshark-dev fehlt" >&2; exit 2; }
command -v cmake >/dev/null || { echo "check: cmake fehlt" >&2; exit 2; }

cmake -S . -B /tmp/lbw-ws-build > /tmp/lbw-ws-cmake.log 2>&1
cmake --build /tmp/lbw-ws-build 2>&1 | tail -n 2
SO=$(ls /tmp/lbw-ws-build/lbw.so)
VER=$(pkg-config --modversion wireshark | cut -d. -f1,2)

UH=/tmp/lbw-u
rm -rf "$UH"
mkdir -p "$UH/.local/lib/wireshark/plugins/$VER/epan"
cp "$SO" "$UH/.local/lib/wireshark/plugins/$VER/epan/"
./gen_sample.py "$UH/sample.pcap"

# tshark-Aufruf je Identität (nobody braucht les/schreibbares HOME).
if [ "$(id -u)" -eq 0 ] && su -s /bin/sh nobody -c true 2>/dev/null; then
  chown -R nobody:nogroup "$UH"
  T() { su -s /bin/sh nobody -c "HOME=$UH tshark -r $UH/sample.pcap $*"; }
elif [ "$(id -u)" -eq 0 ]; then
  GDIR="/usr/lib/x86_64-linux-gnu/wireshark/plugins/$VER/epan"
  cp "$SO" "$GDIR/"
  trap 'rm -f "$GDIR/lbw.so"' EXIT
  T() { tshark -r "$UH/sample.pcap" "$@"; }
else
  T() { env HOME="$UH" tshark -r "$UH/sample.pcap" "$@"; }
fi

OUT=$(T -T fields -e lbw.msg 2>/dev/null)
echo "$OUT"
# Unbekannte Variante setzt kein lbw.msg (leere Zeile, wie Rumpf-Segment);
# Hello-mit-Rest zählt als drittes Hello.
EXP="AddText Button ClearText Hello Hello Hello Key MouseMove RemoveText Text Tile"
# Leere Zeile = Rumpf-Segment der gesplitteten Kachel (keine Nachricht).
GOT=$(echo "$OUT" | grep -v '^$' | LC_ALL=C sort | tr '\n' ' ' | sed 's/ $//')
[ "$GOT" = "$EXP" ] || { echo "check: FEHLER — Varianten: [$GOT] != [$EXP]" >&2; exit 1; }
# Vollbaum-Pass (wie Klick in der GUI — fängt Dissector-Bugs, die -T fields
# wegen tree==NULL nicht sieht): kein Abort, keine CRITICAL/Fehler auf stderr.
T -V > /dev/null 2>/tmp/lbw_v.log || { echo "check: FEHLER — tshark -V brach ab" >&2; tail -n 5 /tmp/lbw_v.log >&2; exit 1; }
grep -E "Dissector bug|CRITICAL|ERROR" /tmp/lbw_v.log && { echo "check: FEHLER — Baum-Pass meldet Dissector-Fehler" >&2; exit 1; } || true
# Stichproben: Version, Mehrbyte-Varint (w=300, 251-Marker), Kachel, UTF-8.
T -T fields -e lbw.version 2>/dev/null | grep -qx 3 || { echo "check: FEHLER — lbw.version" >&2; exit 1; }
T -T fields -e lbw.text.w 2>/dev/null | grep -qx 300 || { echo "check: FEHLER — lbw.text.w" >&2; exit 1; }
T -T fields -e lbw.tile.len 2>/dev/null | grep -qx 4 || { echo "check: FEHLER — lbw.tile.len" >&2; exit 1; }
T -T fields -e lbw.text.str 2>/dev/null | grep -qx "n€u" || { echo "check: FEHLER — lbw.text.str" >&2; exit 1; }
echo "check: OK (alle 10 Varianten + Felder zerlegt)"
