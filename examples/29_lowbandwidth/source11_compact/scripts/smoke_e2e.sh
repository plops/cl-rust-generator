#!/bin/bash
# smoke_e2e.sh — Ende-zu-Ende durch die echte SSH-Kette (D1).
#
# Xvfb :99 (1280×720, xterm mit bekanntem Text) → lbw-server (echte
# PP-OCRv6-Modelle, --cpu, --budget 6000) → Ziel-sshd → Jump-sshd →
# lbw-client --via/--jump (macroquad-Fenster auf demselben Xvfb, --tap
# für den Klartext-Mitschnitt). xdotool tippt ins xterm; der Tap beweist
# Text-Echo und Kachel-Nachschub.
#
# Aufruf (aus source11_compact, als root im Container):
#   ./scripts/smoke_e2e.sh [out-dir]
set -u
cd "$(dirname "$0")/.."
OUT="${1:-/tmp/lbw11-e2e}"
B=target/debug
K="$OUT/keys"
TOKEN="e2e-token-geheim"
mkdir -p "$OUT" "$K"
rm -f "$OUT"/*.log

cargo build -q -p lbw-server -p lbw-client || exit 1

PIDS=()
cleanup() { kill "${PIDS[@]}" 2>/dev/null; wait 2>/dev/null; }
trap cleanup EXIT

# Zwei sshd: Jump (2322) + Ziel (2323, wie tinyeu:2332 → hpz6o).
[ -f "$K/jump" ] || ssh-keygen -q -t ed25519 -N "" -f "$K/jump"
[ -f "$K/target" ] || ssh-keygen -q -t ed25519 -N "" -f "$K/target"
[ -f "$K/user" ] || ssh-keygen -q -t ed25519 -N "" -f "$K/user"
cp "$K/user.pub" "$K/authorized_keys"
chmod 700 "$K"; chmod 600 "$K/authorized_keys"
mkdir -p /run/sshd
/usr/sbin/sshd -D -p 2322 -h "$K/jump" \
  -o ListenAddress=127.0.0.1 -o AuthorizedKeysFile="$K/authorized_keys" \
  -o PasswordAuthentication=no -o PermitRootLogin=prohibit-password \
  -o StrictModes=no -o AllowTcpForwarding=yes \
  -E "$OUT/sshd-2322.log" & PIDS+=($!)
/usr/sbin/sshd -D -p 2323 -h "$K/target" \
  -o ListenAddress=127.0.0.1 -o AuthorizedKeysFile="$K/authorized_keys" \
  -o PasswordAuthentication=no -o PermitRootLogin=prohibit-password \
  -o StrictModes=no -o AllowTcpForwarding=yes \
  -E "$OUT/sshd-2323.log" & PIDS+=($!)
JFP=$(ssh-keygen -lf "$K/jump.pub" | awk '{print $2}')
TFP=$(ssh-keygen -lf "$K/target.pub" | awk '{print $2}')

Xvfb :99 -screen 0 1280x720x24 >/dev/null 2>&1 & PIDS+=($!)
sleep 2
export DISPLAY=:99
xterm -geometry 100x30+10+10 -e "printf 'SMOKE-TEST-720\n'; sleep 60" & PIDS+=($!)
sleep 1

# Dissector systemweit (root) einhängen, damit der Tap zerlegt wird.
WSVER=$(pkg-config --modversion wireshark | cut -d. -f1,2 2>/dev/null || echo "4.6")
GDIR="/usr/lib/x86_64-linux-gnu/wireshark/plugins/$WSVER/epan"
if [ ! -f "$GDIR/lbw.so" ]; then
  cmake -S wireshark -B /tmp/lbw11-e2e-ws >/dev/null 2>&1
  cmake --build /tmp/lbw11-e2e-ws >/dev/null 2>&1
  cp /tmp/lbw11-e2e-ws/lbw.so "$GDIR/lbw.so"
fi

# Server mit echten Modellen (CPU — kein GPU-Zwang im Smoke).
./$B/lbw-server --token "$TOKEN" --budget 6000 --cpu >"$OUT/server.log" 2>&1 & PIDS+=($!)
for i in $(seq 1 24); do
  grep -q "lauscht" "$OUT/server.log" && break
  sleep 5
done
grep -q "lauscht" "$OUT/server.log" || { echo "E2E-FEHLER: Server lauscht nicht"; tail -n 20 "$OUT/server.log"; exit 1; }

# Client durch die Kette (Jump 2322 → Ziel-sshd 2323), Tap für Wireshark.
./$B/lbw-client --token "$TOKEN" \
  --via "$(whoami)@127.0.0.1:2323" --via-identity "$K/user" \
  --jump "$(whoami)@127.0.0.1:2322" --jump-identity "$K/user" \
  --target-sshd 127.0.0.1:2323 \
  --pin "$JFP" --pin "$TFP" \
  --tap 17879 >"$OUT/client.log" 2>&1 & PIDS+=($!)
sleep 6

# Tap-Mitschnitt auf dem fixen Forward-Port (voller Klartext-Strom).
timeout 25 tshark -i lo -f "tcp port 17879" -a duration:20 -w "$OUT/tap.pcap" >/dev/null 2>&1 &
sleep 2
# Tippen ins xterm (fokussieren, Text, Scroll auslösen).
WID=$(xdotool search --name "SMOKE-TEST-720" 2>/dev/null | head -1 || true)
[ -n "$WID" ] || WID=$(xdotool search --class xterm | head -1)
xdotool windowactivate "$WID" 2>/dev/null; sleep 1
xdotool type --delay 40 "hello lbw11 scroll test 中文 persevere" 2>/dev/null
xdotool key Return 2>/dev/null
sleep 12

MSGS=$(tshark -r "$OUT/tap.pcap" -d "tcp.port==17879,lbw" -T fields -e lbw.msg 2>/dev/null | sort | uniq -c)
echo "$MSGS" | tee "$OUT/msgs.txt"
echo "$MSGS" | grep -q "NewText" || { echo "E2E-FEHLER: kein NewText im Tap"; exit 1; }
echo "$MSGS" | grep -q "Tile" || { echo "E2E-FEHLER: kein Tile im Tap"; exit 1; }
echo "E2E-OK: Tunnel-Kette + Text + Tile fließen"
