#!/usr/bin/env bash
# hil-real.sh — HIL gegen das ECHTE Gemini (Key aus Datei, maskiert geloggt).
#
# Ablauf:
#  1. Einmalig per Gemini-TTS eine deutsche Referenz erzeugen und als
#     48-kHz-mono-WAV nach $HIL_WAV_DE (/tmp/live-hil/mic-de.wav) legen
#     (3x derselbe Satz mit Pausen). Vorhandene Datei wird wiederverwendet.
#  2. live-silk (release) mit --key-file <api_key.txt> starten — ECHTES
#     Gemini, Default-URL (kein --gemini-url).
#  3. hil-real.js faehrt zwei SEPARATE Chrome-Prozesse (A Opus/Chinesisch,
#     B PCM/Deutsch) im selben Raum und misst/prueft (~45-60 s).
#
# Der Key wird NIE ausgegeben (nur maskiert), nie committet, nie in
# Screenshots/Logs. Werkzeuge aus $HIL_DIR (scripts/browser-setup.sh vorher).
set -euo pipefail

HERE="$(dirname "$0")"
ROOT="$(cd "$HERE/.." && pwd)"
HIL_DIR="${HIL_DIR:-/tmp/live-hil}"
PORT="${PORT:-3312}"
KEYFILE="${HIL_KEY_FILE:-/workspace/src/cl-rust-generator/api_key.txt}"

export HIL_BASE="http://127.0.0.1:$PORT"
export HIL_ROOM="${HIL_ROOM:-real-room-$$}"
export HIL_SECONDS="${HIL_SECONDS:-50}"
export HIL_OUT="${HIL_OUT:-$HIL_DIR}"
export HIL_CHROME="$HIL_DIR/chrome-linux64/chrome"
export HIL_WAV_ZH="${HIL_WAV_ZH:-$HIL_DIR/mic-zh.wav}"
export HIL_WAV_DE="${HIL_WAV_DE:-$HIL_DIR/mic-de.wav}"
export HIL_KEY_FILE="$KEYFILE"
export NODE_PATH="$HIL_DIR/node_modules:${NODE_PATH:-}"

mask() { local k; k="$(tr -d '\n' < "$1")"; echo "${k:0:4}…${k: -4}"; }

[ -f "$KEYFILE" ] || { echo "FEHLT Key-Datei: $KEYFILE"; exit 2; }
[ -x "$HIL_CHROME" ] || { echo "FEHLT Chrome: scripts/browser-setup.sh ausfuehren"; exit 2; }
[ -f "$HIL_WAV_ZH" ] || { echo "FEHLT Fake-Mikro zh: scripts/browser-setup.sh ausfuehren"; exit 2; }
[ -d "$HIL_DIR/node_modules/playwright-core" ] || { echo "FEHLT Treiber: scripts/browser-setup.sh ausfuehren"; exit 2; }
command -v ffmpeg >/dev/null 2>&1 || { echo "FEHLT ffmpeg"; exit 2; }
mkdir -p "$HIL_OUT"

echo "== HIL-REAL (echtes Gemini) == key: $(mask "$KEYFILE")"

# --- 1. Deutsche TTS-Referenz einmalig erzeugen -------------------------
if [ ! -f "$HIL_WAV_DE" ]; then
  echo "-- erzeuge deutsche TTS-Referenz (Gemini) → $HIL_WAV_DE ..."
  RAW24="$HIL_DIR/mic-de-raw24.pcm"
  # Gemini-TTS (gemini-2.5-flash-preview-tts) → PCM16LE 24 kHz mono.
  # Inline-Node (kein eigenes Skript); Key bleibt in der Datei, wird nicht geloggt.
  HIL_KEY_FILE="$KEYFILE" TTS_RAW="$RAW24" node --input-type=module -e '
    import fs from "node:fs";
    const key = fs.readFileSync(process.env.HIL_KEY_FILE, "utf8").trim();
    const text = "Sprich freundlich und deutlich auf Deutsch: Guten Morgen! Heute ist ein schöner Tag. Wie geht es dir?";
    const url = "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash-preview-tts:generateContent?key=" + key;
    const body = { contents: [{ parts: [{ text }] }], generationConfig: { responseModalities: ["AUDIO"], speechConfig: { voiceConfig: { prebuiltVoiceConfig: { voiceName: "Charon" } } } } };
    const r = await fetch(url, { method: "POST", headers: { "Content-Type": "application/json" }, body: JSON.stringify(body) });
    const j = await r.json();
    const p = j?.candidates?.[0]?.content?.parts?.[0]?.inlineData;
    if (!p || !p.data) { console.error("TTS-Fehler:", JSON.stringify(j).slice(0, 200)); process.exit(1); }
    fs.writeFileSync(process.env.TTS_RAW, Buffer.from(p.data, "base64")); // PCM16LE 24 kHz mono
    console.error("-- TTS ok:", fs.statSync(process.env.TTS_RAW).size, "B roh (24 kHz mono16)");
  '
  # 3x derselbe Satz mit 0,6 s Stille dazwischen, dann auf 48 kHz mono.
  SIL="$HIL_DIR/sil.pcm"
  # 0,6 s Stille bei 24 kHz mono16 = 24000*0.6*2 Bytes.
  head -c 28800 /dev/zero > "$SIL"
  CAT24="$HIL_DIR/mic-de-3x.pcm"
  cat "$RAW24" "$SIL" "$RAW24" "$SIL" "$RAW24" > "$CAT24"
  ffmpeg -y -v error -f s16le -ar 24000 -ac 1 -i "$CAT24" \
    -ar 48000 -ac 1 -c:a pcm_s16le "$HIL_WAV_DE"
  rm -f "$SIL" "$CAT24"
  echo "-- mic-de.wav: $(ffmpeg -v error -i "$HIL_WAV_DE" -f null - 2>&1 | head -1 || true) $(wc -c < "$HIL_WAV_DE") B"
else
  echo "-- mic-de.wav vorhanden ($(wc -c < "$HIL_WAV_DE") B)"
fi

# --- 2. live-silk (release) mit echtem Gemini starten -------------------
echo "-- baue live-silk (release) ..."
( cd "$ROOT" && cargo build --release 2>&1 | tail -1 )

SRV_PID=""
cleanup() { [ -n "$SRV_PID" ] && kill "$SRV_PID" 2>/dev/null || true; }
trap cleanup EXIT

echo "-- starte live-silk auf :$PORT (echtes Gemini, Default-URL) ..."
"$ROOT/target/release/live-silk" --listen "127.0.0.1:$PORT" --dir "$ROOT/public" \
  --key-file "$KEYFILE" >>"$HIL_OUT/relay-real.log" 2>&1 &
SRV_PID=$!

for _ in $(seq 1 50); do curl -sf "$HIL_BASE/healthz" >/dev/null 2>&1 && break; sleep 0.2; done
curl -sf "$HIL_BASE/healthz" >/dev/null 2>&1 || { echo "relay startet nicht"; tail -5 "$HIL_OUT/relay-real.log"; exit 1; }
echo "-- relay live: $HIL_BASE (room $HIL_ROOM)"

# --- 3. Browser-Lauf ----------------------------------------------------
set +e
node "$HERE/hil-real.js"
RC=$?
set -e

# Sicherheitsnetz: Key darf in KEINEM Log stehen.
KEYVAL="$(tr -d '\n' < "$KEYFILE")"
if grep -qF "$KEYVAL" "$HIL_OUT/relay-real.log" 2>/dev/null; then
  echo "!! WARNUNG: Key im relay-real.log gefunden — das ist ein Fehler."
  RC=1
fi
exit $RC
