#!/bin/bash
# test_duckai.sh — Duck.ai-Integrationstest unter Xvfb mit echtem Chrome.
#
# Was er beweist: Klick auf "Ask anything privately", Texteingabe,
# Klick auf "Ask" und eine anonymisierte Witz-Antwort — gefahren über
# echte Maus-Events auf DOM-Koordinaten (derselbe Pfad wie später die
# source6-XTEST-Automation: Koordinaten -> Klick).
#
# Aufruf (aus dem source6-Verzeichnis):
#   ./scripts/test_duckai.sh [OUT-DIR]
#
# Voraussetzungen (einmalig, als root):
#   apt-get install xvfb
#   Chrome-for-Testing nach /opt/chrome-test entpackt (s. scripts/README.md)
#   Internet-Zugriff auf https://duck.ai
set -u

OUT="${1:-/tmp/duckai-test}"
DISP="${DISPLAY_NUM:-99}"
CHROME="${CHROME_BIN:-/opt/chrome-test/chrome-linux64/chrome}"
CDP_PORT="${CDP_PORT:-9222}"
PROF="$OUT/chrome-profile"

mkdir -p "$OUT"
# Frisches Browser-Profil je Lauf: der Test ist sonst vom Vorlauf abhängig
# (z. B. andere Composer-Buttons bei bestehendem Chat-Verlauf).
rm -rf "$PROF"
mkdir -p "$PROF"
rm -f "$OUT"/[0-9]*.png "$OUT/answer.txt"
export DISPLAY=":$DISP"

cleanup() {
  kill "$CHROME_PID" "$XVFB_PID" 2>/dev/null
}
trap cleanup EXIT

Xvfb ":$DISP" -screen 0 1280x1024x24 >"$OUT/xvfb.log" 2>&1 &
XVFB_PID=$!
sleep 2

"$CHROME" \
  --user-data-dir="$PROF" \
  --remote-debugging-port="$CDP_PORT" \
  --no-first-run --no-default-browser-check \
  --no-sandbox \
  --disable-search-engine-choice-screen \
  --window-size=1280,1024 \
  --disable-gpu \
  about:blank >"$OUT/chrome.log" 2>&1 &
CHROME_PID=$!
sleep 3

python3 "$(dirname "$0")/test_duckai.py" \
  --cdp "http://127.0.0.1:$CDP_PORT" --out "$OUT"
code=$?
echo "test exit: $code"

trap - EXIT
kill "$CHROME_PID" "$XVFB_PID" 2>/dev/null
exit "$code"
