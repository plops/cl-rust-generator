#!/usr/bin/env bash
# Smoke: build + serve + /healthz + Seite enthaelt alle 4 Modus-IDs.
set -euo pipefail
cd "$(dirname "$0")/.."
cargo build --release
PORT="${PORT:-3101}"
./target/release/live-relay --listen "127.0.0.1:$PORT" --dir public &
SRV=$!
trap 'kill $SRV 2>/dev/null || true' EXIT
for i in $(seq 1 50); do
  curl -sf "http://127.0.0.1:$PORT/healthz" >/dev/null 2>&1 && break
  sleep 0.1
done
echo "-- /healthz: $(curl -s http://127.0.0.1:$PORT/healthz)"
echo "-- /api/rooms: $(curl -s http://127.0.0.1:$PORT/api/rooms)"
echo "-- /api/modes: $(curl -s http://127.0.0.1:$PORT/api/modes)"
for m in de-to-zh zh-to-de en-to-zh zh-to-en; do
  curl -s "http://127.0.0.1:$PORT/" | grep -q "$m" || { echo "FEHLT Modus $m"; exit 1; }
done
echo "-- frontend enthaelt alle 4 modi: ok"
./target/release/live-relay --check "http://127.0.0.1:$PORT"
echo "SMOKE OK"
