#!/bin/bash
# test_sshd.sh — throwaway OpenSSH server for tunnel tests (needs root).
#
#   scripts/test_sshd.sh start   → prints `export LBW_SSHD_…` lines
#   scripts/test_sshd.sh stop
#
# Listens on 127.0.0.1 and 0.0.0.0:${LBW_SSHD_PORT:-2222} (the Android
# emulator reaches it as 10.0.2.2). Host keys: ed25519 + ecdsa.
# Auth: ecdsa client key for $USER (authorized_keys in $DIR) and, if
# LBW_SSHD_PASSWORD is set, password for the user `lbwtest` (created).
set -eu
DIR="${LBW_SSHD_DIR:-/tmp/lbw-sshd}"
PORT="${LBW_SSHD_PORT:-2222}"
case "${1:-start}" in
stop)
    [ -f "$DIR/sshd.pid" ] && kill "$(cat "$DIR/sshd.pid")" 2>/dev/null || true
    rm -f "$DIR/sshd.pid"
    exit 0 ;;
start) ;;
*) echo "usage: $0 start|stop" >&2; exit 2 ;;
esac
SSHD=$(command -v sshd || echo /usr/sbin/sshd)
[ -x "$SSHD" ] || { echo "sshd fehlt (apt install openssh-server)" >&2; exit 1; }
mkdir -p "$DIR" /run/sshd
for t in ed25519 ecdsa; do
    [ -f "$DIR/host_$t" ] || ssh-keygen -q -t "$t" -N '' -f "$DIR/host_$t"
done
[ -f "$DIR/id_ecdsa" ] || ssh-keygen -q -t ecdsa -b 256 -m PEM -N '' -f "$DIR/id_ecdsa"
cp "$DIR/id_ecdsa.pub" "$DIR/authorized_keys"
# Throwaway test key: readable for a non-root test runner (CI uses sudo here)
chmod 755 "$DIR"
chmod 644 "$DIR/id_ecdsa"
chmod 644 "$DIR/authorized_keys"
if [ -n "${LBW_SSHD_PASSWORD:-}" ]; then
    id lbwtest >/dev/null 2>&1 || useradd -M -s /bin/sh lbwtest
    echo "lbwtest:$LBW_SSHD_PASSWORD" | chpasswd
fi
cat > "$DIR/sshd_config" <<CFG
Port $PORT
ListenAddress 0.0.0.0
HostKey $DIR/host_ed25519
HostKey $DIR/host_ecdsa
PidFile $DIR/sshd.pid
AuthorizedKeysFile $DIR/authorized_keys
StrictModes no
PermitRootLogin prohibit-password
PasswordAuthentication yes
KbdInteractiveAuthentication no
UsePAM no
AllowTcpForwarding local
X11Forwarding no
LogLevel VERBOSE
CFG
"$0" stop
"$SSHD" -f "$DIR/sshd_config" -E "$DIR/sshd.log"
for _ in $(seq 50); do [ -f "$DIR/sshd.pid" ] && break; sleep 0.1; done
[ -f "$DIR/sshd.pid" ] || { cat "$DIR/sshd.log" >&2; exit 1; }
echo "export LBW_SSHD_PORT=$PORT LBW_SSHD_USER=$(id -un) LBW_SSHD_KEY=$DIR/id_ecdsa"
[ -z "${LBW_SSHD_PASSWORD:-}" ] || echo "export LBW_SSHD_PWUSER=lbwtest"
