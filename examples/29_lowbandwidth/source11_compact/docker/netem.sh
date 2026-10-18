#!/bin/bash
# netem.sh — begrenztes Bandbreiten-Profil auf einer Schnittstelle (D1).
# Opt-in (braucht NET_ADMIN): ./docker/netem.sh [iface] [rate-kbit] [delay-ms] [loss-%]
# Beispiel: ./docker/netem.sh eth0 48 60 1   (≈ 6 kB/s + 60 ms + 1 % Verlust)
# Zurück:   ./docker/netem.sh --clear [iface]
set -u
IFACE="${2:-eth0}"
if [ "${1:-}" = "--clear" ]; then
  tc qdisc del dev "$IFACE" root 2>/dev/null || true
  echo "netem: $IFACE frei"; exit 0
fi
RATE="${2:-48}"
DELAY="${3:-60}"
LOSS="${4:-1}"
tc qdisc del dev "$IFACE" root 2>/dev/null || true
tc qdisc add dev "$IFACE" root netem rate "${RATE}kbit" delay "${DELAY}ms" loss "${LOSS}%"
echo "netem: $IFACE rate=${RATE}kbit delay=${DELAY}ms loss=${LOSS}%"
