#!/bin/bash
# smoke_xvfb.sh — Xvfb-Rauchtest für source6 (TUI + XTEST + OCR-Pipeline).
#
# Was er beweist:
#   1. XTEST-Pfad (Keymap, Klick, Tippen) gegen echten X-Server
#      (ignorierter Rust-Test, nur unter Xvfb).
#   2. Das Release-Binary startet im --dry-run --headless-frames 1, OCRt
#      echten Bildschirmtext (xterm) und druckt den Frame als Klartext
#      (pty-frei; das TUI-Render selbst decken Unit-Tests ab).
#   3. Fehlerpfade ohne X-Server: --help (Exit 0), kaputte TOML (Exit 2),
#      fehlendes Display (Exit 1).
#
# Aufruf (aus dem source6-Verzeichnis):
#   ./scripts/smoke_xvfb.sh [BINARY] [OUT-DIR]
#
# Voraussetzungen (einmalig): siehe scripts/README.md.
set -u

BIN="${1:-target/release/x11_ocr_automation}"
OUT="${2:-/tmp/source6-smoke}"
DISP="${DISPLAY_NUM:-98}"

mkdir -p "$OUT"
rm -f "$OUT/typescript" "$OUT/tui.png"
export DISPLAY=":$DISP"

cleanup() {
  kill "$XTERM" "$XVFB" 2>/dev/null
}
trap cleanup EXIT

fail() { echo "SMOKE-FAIL: $1" >&2; exit 1; }

echo "--- [1/4] XTEST-Pfad (ignorierter Test unter Xvfb) ---"
Xvfb ":$DISP" -screen 0 1280x1024x24 >"$OUT/xvfb.log" 2>&1 &
XVFB=$!
sleep 2

xterm -geometry 90x24+0+0 -fa Monospace -fs 28 -bg white -fg black \
  -e bash -c 'printf "HELLO OCR WORLD 123\nSECOND LINE ABC xyz\n"; sleep 300' &
XTERM=$!
sleep 2

cargo test --quiet -- --ignored xtest_path_against_real_server \
  >"$OUT/xtest.log" 2>&1 || fail "XTEST-Test (s. $OUT/xtest.log)"
echo "XTEST-Test: PASS"

echo "--- [2/4] --dry-run mit OCR-Nachweis (Batch-Modus, pty-frei) ---"
# --headless-frames braucht kein Terminal. Genau 1 geänderter Frame:
# Bei statischem Testbild gibt es nur einen (der erste); danach wäre der
# Lauf per Design endlos — dafür ist der TUI-Modus da.
timeout 120 "$BIN" --dry-run --headless-frames 1 \
  >"$OUT/typescript" 2>"$OUT/headless-stderr.log" \
  || fail "Batch-Lauf (Exit $?)"
sleep 1
scrot "$OUT/tui.png"
grep -a -c "HELLO\|SECOND" "$OUT/typescript" \
  | grep -q -v "^0$" || fail "OCR-Text fehlt im Batch-Mitschrieb"
echo "OCR im Batch-Mitschrieb: PASS"

echo "--- [3/4] Fehlerpfade ---"
"$BIN" --help >/dev/null 2>&1 || fail "--help"
printf 'schema_version = 99\n' >"$OUT/bad.toml"
"$BIN" --rules "$OUT/bad.toml" >/dev/null 2>&1
test $? -eq 2 || fail "kaputte TOML muss Exit 2 geben"
DISPLAY= "$BIN" --dry-run >/dev/null 2>&1
test $? -eq 1 || fail "fehlendes Display muss Exit 1 geben"
echo "Fehlerpfade (--help=0, TOML=2, Display=1): PASS"

echo "--- [4/4] Artefakte ---"
ls -la "$OUT/typescript" "$OUT/tui.png"
echo "SMOKE: PASS"

trap - EXIT
kill "$XTERM" "$XVFB" 2>/dev/null
