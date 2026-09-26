#!/bin/bash
# sticker_single.sh — Ein Meta-AI-Sticker für einen freien Namen (source6).
#
# Ablauf: NAME wird als `<placeholder>` in `meta_sticker_request.toml`
# eingesetzt, das x11_ocr-Programm einmal mit der generierten Regel
# gefahren, danach per `meta_url.toml` die URL zurückgesetzt. Beide
# Schritte laufen als frischer Prozess mit `-a` (scharf ohne Tastendruck)
# unter `timeout`: Die `once`-Regel feuert genau einmal, danach wäre der
# Lauf per Design endlos — `timeout` (Exit 124) ist das erwartete Ende.
#
# Aufruf (aus dem source6-Verzeichnis):
#   ./scripts/sticker_single.sh [--dry-run] [--bin PFAD]
#     [--sticker-secs N] [--url-secs N] NAME
#
# Echter Lauf braucht: X11-Display mit geöffnetem Meta-AI-Browserfenster +
# Meta-Account. NAME darf alles außer `|` enthalten (sed-Trennzeichen).
#
# Nur Kern-Werkzeuge: bash, sed, mktemp, timeout (coreutils).
set -euo pipefail

SRC_DIR="$(cd "$(dirname "$0")/.." && pwd)"
BIN="$SRC_DIR/target/release/x11_ocr_automation"
TEMPLATE="$SRC_DIR/meta_sticker_request.toml"
RESET_RULES="$SRC_DIR/meta_url.toml"
STICKER_SECS=120
URL_SECS=60
DRY_RUN=()

usage() {
  sed -n '2,/^$/p' "$0" | sed 's/^# \?//'
}

NAME=""
while [ $# -gt 0 ]; do
  case "$1" in
    -h | --help) usage; exit 0 ;;
    --dry-run) DRY_RUN=(--dry-run); shift ;;
    --bin) BIN="$2"; shift 2 ;;
    --sticker-secs) STICKER_SECS="$2"; shift 2 ;;
    --url-secs) URL_SECS="$2"; shift 2 ;;
    --) shift; [ $# -gt 0 ] && NAME="$1" && shift; break ;;
    -*) echo "unbekannte Option: $1 (s. --help)" >&2; exit 2 ;;
    *) NAME="$1"; shift ;;
  esac
done
[ -n "$NAME" ] || { echo "FEHLER: NAME fehlt (s. --help)" >&2; exit 2; }
case "$NAME" in
  *"|"*) echo "FEHLER: NAME darf kein '|' enthalten" >&2; exit 2 ;;
esac

[ -x "$BIN" ] || { echo "FEHLER: Binary fehlt: $BIN" >&2; exit 2; }
[ -f "$TEMPLATE" ] || { echo "FEHLER: Template fehlt: $TEMPLATE" >&2; exit 2; }
grep -q '<placeholder>' "$TEMPLATE" || {
  echo "FEHLER: kein <placeholder> in $TEMPLATE" >&2
  exit 2
}
[ -f "$RESET_RULES" ] || { echo "FEHLER: Reset-Regel fehlt: $RESET_RULES" >&2; exit 2; }
"$BIN" --help 2>&1 | grep -q '\[-a\]' || {
  echo "FEHLER: $BIN kennt -a nicht (neu bauen?)" >&2
  exit 2
}

TMPDIR_RUN="$(mktemp -d)"
echo $TMPDIR_RUN
trap 'rm -rf "$TMPDIR_RUN"' EXIT
sed "s|<placeholder>|$NAME|g" "$TEMPLATE" >"$TMPDIR_RUN/rules.toml"

run_step() { # secs toml beschreibung
    local code=0
    echo "$BIN" -a --rules "$2" "${DRY_RUN[@]}"
    #timeout "$1"
    "$BIN" -a --rules "$2" "${DRY_RUN[@]}" || code=$?
  if [ "$code" -eq 124 ]; then
    echo "ok (einmal gefeuert, Timeout): $3"
  elif [ "$code" -eq 0 ]; then
    echo "ok (beendet): $3"
  else
    echo "FEHLER ($code): $3" >&2
    return "$code"
  fi
}

echo "--- Motiv: $NAME ---"
run_step "$STICKER_SECS" "$TMPDIR_RUN/rules.toml" "Sticker '$NAME'"
run_step "$URL_SECS" "$RESET_RULES" "URL-Reset nach '$NAME'"
echo "FERTIG: Sticker '$NAME' angestoßen."
