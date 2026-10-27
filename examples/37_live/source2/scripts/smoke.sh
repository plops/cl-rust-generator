#!/usr/bin/env bash
# Smoke: release-build, serve, /healthz, /api/languages >=78 codes,
# /api/rooms leer, --check meldet kein raum. Gibt am Ende `SMOKE OK`.
set -euo pipefail
cd "$(dirname "$0")/.."
cargo build --release
PORT="${PORT:-3111}"
./target/release/live-silk --listen "127.0.0.1:$PORT" --dir public &
SRV=$!
trap 'kill $SRV 2>/dev/null || true' EXIT

# Warten, bis der Server lauscht.
for _ in $(seq 1 50); do
  curl -sf "http://127.0.0.1:$PORT/healthz" >/dev/null 2>&1 && break
  sleep 0.1
done

HEALTH="$(curl -s "http://127.0.0.1:$PORT/healthz")"
echo "-- /healthz: $HEALTH"
[ "$HEALTH" = "ok" ] || { echo "healthz != ok"; exit 1; }

CODES="$(curl -s "http://127.0.0.1:$PORT/api/languages" | grep -o '"code"' | wc -l)"
echo "-- /api/languages codes: $CODES"
[ "$CODES" -ge 78 ] || { echo "weniger als 78 codes"; exit 1; }

ROOMS="$(curl -s "http://127.0.0.1:$PORT/api/rooms")"
echo "-- /api/rooms: $ROOMS"
[ "$(echo "$ROOMS" | tr -d '[:space:]')" = "{}" ] || { echo "rooms nicht leer"; exit 1; }

CHECK="$(./target/release/live-silk --check "http://127.0.0.1:$PORT")"
echo "-- --check: $CHECK"
echo "$CHECK" | grep -q "kein raum aktiv" || { echo "check meldet keinen leeren zustand"; exit 1; }

# Frontend: Seite enthaelt die Transport-Auswahl, app.js wird ausgeliefert.
INDEX="$(curl -s "http://127.0.0.1:$PORT/")"
echo "$INDEX" | grep -q 'name="transport"' || { echo "transport-auswahl fehlt in /"; exit 1; }
echo "-- / enthaelt transport-auswahl"
APPJS="$(curl -s -o /dev/null -w '%{http_code}' "http://127.0.0.1:$PORT/app.js")"
echo "-- /app.js: HTTP $APPJS"
[ "$APPJS" = "200" ] || { echo "app.js wird nicht ausgeliefert"; exit 1; }

echo "SMOKE OK"
