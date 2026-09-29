#!/bin/bash
# ssh_tunnel.sh — Nachweis: Client ↔ Server über `ssh -L` (Verschlüsselung
# und NAT-Traversal macht SSH), inkl. Tunnel-Abbruch und Neuaufbau.
#
# Startet einen lokalen sshd (Port 2222, Wegwerf-Schlüssel), den Server auf
# Xvfb :99 (xterm) und den Client auf Xvfb :98 über den Tunnel
# 127.0.0.1:17880 → 127.0.0.1:17878. Der Tunnel wird nach dem ersten Text
# beendet und 5 s später neu aufgebaut; der Client muss sich selbst wieder
# verbinden (Resume ohne Voll-Refresh).
#
# Produktiv (Client-Seite), mit Reconnect des Tunnels:
#   autossh -M 0 -N -o ServerAliveInterval=15 -o ServerAliveCountMax=8 \
#     -o ExitOnForwardFailure=yes -L 7878:127.0.0.1:7878 user@remote
#   lbw-client --connect 127.0.0.1:7878
# Server hinter NAT ohne eingehende Ports: vom Server aus `ssh -R 7878:127.0.0.1:7878 user@client`.
#
# Aufruf (aus source6, als root im Container): ./scripts/ssh_tunnel.sh [out-dir]
set -u
cd "$(dirname "$0")/.."
OUT="${1:-/tmp/lbw-ssh}"
B=target/release
K="$OUT/keys"
mkdir -p "$OUT" "$K"
rm -f "$OUT"/*.log
cargo build --release -q -p lbw-server -p lbw-client || exit 1

PIDS=()
cleanup() { kill "${PIDS[@]}" 2>/dev/null; kill "$TUN" 2>/dev/null; wait 2>/dev/null; }
trap cleanup EXIT

[ -f "$K/host" ] || ssh-keygen -q -t ed25519 -N "" -f "$K/host"
[ -f "$K/user" ] || ssh-keygen -q -t ed25519 -N "" -f "$K/user"
cp "$K/user.pub" "$K/authorized_keys"
chmod 700 "$K"; chmod 600 "$K/authorized_keys"
mkdir -p /run/sshd
/usr/sbin/sshd -D -p 2222 -h "$K/host" -o ListenAddress=127.0.0.1 \
  -o AuthorizedKeysFile="$K/authorized_keys" -o PasswordAuthentication=no \
  -o PermitRootLogin=prohibit-password -o StrictModes=no -o AllowTcpForwarding=yes \
  -E "$OUT/sshd.log" & PIDS+=($!)

tunnel() {
  ssh -F /dev/null -N -p 2222 -i "$K/user" -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null \
    -o ServerAliveInterval=15 -o ExitOnForwardFailure=yes -o LogLevel=ERROR \
    -L 17880:127.0.0.1:17878 root@127.0.0.1 >>"$OUT/ssh.log" 2>&1 &
  TUN=$!
}

Xvfb :99 -screen 0 1280x1024x24 >/dev/null 2>&1 & PIDS+=($!)
Xvfb :98 -screen 0 800x800x24 >/dev/null 2>&1 & PIDS+=($!)
sleep 2
DISPLAY=:99 xterm -geometry 70x30+0+0 -fa Monospace -fs 13 -bg white -fg black \
  -e bash --norc -c 'echo VIA SSH TUNNEL; exec bash --norc -i' & PIDS+=($!)
$B/lbw-server --display :99 --listen 127.0.0.1:17878 >"$OUT/server.log" 2>&1 & PIDS+=($!)
sleep 1
tunnel
sleep 1
DISPLAY=:98 LIBGL_ALWAYS_SOFTWARE=1 $B/lbw-client --connect 127.0.0.1:17880 --dump-text \
  >"$OUT/client.log" 2>"$OUT/client.err" & PIDS+=($!)

FAIL=0
check() { if eval "$2"; then echo "OK   $1"; else echo "FAIL $1"; FAIL=1; fi; }
wait_for() { local end=$(( $(date +%s) + $1 ))
  while [ "$(date +%s)" -lt "$end" ]; do grep -q "$2" "$3" && return 0; sleep 0.5; done; return 1; }
count_ge() { local end=$(( $(date +%s) + $1 ))
  while [ "$(date +%s)" -lt "$end" ]; do [ "$(grep -c "$2" "$3")" -ge "$4" ] && return 0; sleep 0.5; done; return 1; }

check "Text über den SSH-Tunnel" 'wait_for 40 "VIA SSH TUNNEL" "$OUT/client.log"'
check "Verbindung läuft über sshd" 'ss -tnp 2>/dev/null | grep -q ":17878.*sshd"'
kill "$TUN"; wait "$TUN" 2>/dev/null
echo "     Tunnel beendet, Neuaufbau in 5 s"
sleep 5
tunnel
check "Client verbindet sich nach Tunnel-Neuaufbau selbst" 'count_ge 20 "^CONNECTED" "$OUT/client.log" 2'
check "Resume ohne Voll-Refresh" 'wait_for 5 "CONNECTED resumed=true" "$OUT/client.log"'
grep "CONNECTED" "$OUT/client.log" | sed 's/^/     /'
exit $FAIL
