#!/bin/bash
# sticker_batch.sh — Meta-AI-Sticker im Stapel erzeugen (source6).
#
# Ablauf pro Motiv: Sticker-Prompt (Template `meta_sticker_request.toml`,
# `<placeholder>` wird per sed durchs Motiv ersetzt) absenden, danach per
# `meta_url.toml` die URL zurücksetzen, damit der nächste Sticker starten
# kann. Jeder Schritt läuft als frischer Prozess mit `-a` (Automation
# startet scharf, kein Tastendruck nötig) unter `timeout`: Die `once`-Regel
# feuert genau einmal, danach wäre der Lauf per Design endlos — `timeout`
# (Exit 124) ist hier das erwartete Ende, kein Fehler.
#
# Aufruf (aus dem source6-Verzeichnis):
#   ./scripts/sticker_batch.sh [--dry-run] [--bin PFAD]
#     [--sticker-secs N] [--url-secs N] [--list] [--self-test] [motiv ...]
#
# Ohne Motiv-Argumente laufen alle eingebauten Motive (aus cute_things.md).
# Mit Argumenten nur die genannten (Teilstring, case-insensitiv).
# Echter Lauf braucht: X11-Display mit geöffnetem Meta-AI-Browserfenster +
# Meta-Account. Ohne Account: --self-test (prüft Substitution + TOML-Parse
# + `-a`-Flag, ganz ohne X11/Meta).
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
SUBJECTS=(
#  "baby otter"
#  "red panda cub"
#  "lop-eared bunny"
#  "fluffy calico kitten"
#  "golden retriever puppy"
#  "harp seal pup"
  "chubby baby panda"
#  "sleeping baby capybara"
#  "shiba inu puppy"
#  "tiny dormouse"
#  "baby sloth"
#  "curled-up hedgehog"
)

usage() {
  sed -n '2,/^$/p' "$0" | sed 's/^# \?//'
}

SELF_TEST=0
LIST_ONLY=0
FILTER=()
while [ $# -gt 0 ]; do
  case "$1" in
    -h | --help) usage; exit 0 ;;
    --dry-run) DRY_RUN=(--dry-run); shift ;;
    --bin) BIN="$2"; shift 2 ;;
    --sticker-secs) STICKER_SECS="$2"; shift 2 ;;
    --url-secs) URL_SECS="$2"; shift 2 ;;
    --list) LIST_ONLY=1; shift ;;
    --self-test) SELF_TEST=1; shift ;;
    --) shift; while [ $# -gt 0 ]; do FILTER+=("$1"); shift; done ;;
    -*) echo "unbekannte Option: $1 (s. --help)" >&2; exit 2 ;;
    *) FILTER+=("$1"); shift ;;
  esac
done

if [ "$LIST_ONLY" -eq 1 ]; then
  printf '%s\n' "${SUBJECTS[@]}"
  exit 0
fi

# Motive filtern (Teilstring, case-insensitiv); ohne Filter alle.
SELECTED=()
for s in "${SUBJECTS[@]}"; do
  if [ "${#FILTER[@]}" -eq 0 ]; then
    SELECTED+=("$s")
  else
    for f in "${FILTER[@]}"; do
      if [[ "${s,,}" == *"${f,,}"* ]]; then SELECTED+=("$s"); break; fi
    done
  fi
done
if [ "${#SELECTED[@]}" -eq 0 ]; then
  echo "FEHLER: kein Motiv passt zu: ${FILTER[*]}" >&2
  exit 2
fi

# Einmalige Vorab-Prüfungen (auch für --self-test).
[ -x "$BIN" ] || { echo "FEHLER: Binary fehlt: $BIN" >&2; exit 2; }
[ -f "$TEMPLATE" ] || { echo "FEHLER: Template fehlt: $TEMPLATE" >&2; exit 2; }
grep -q '<placeholder>' "$TEMPLATE" || {
  echo "FEHLER: kein <placeholder> in $TEMPLATE" >&2
  exit 2
}
"$BIN" --help 2>&1 | grep -q '\[-a\]' || {
  echo "FEHLER: $BIN kennt -a nicht (neu bauen?)" >&2
  exit 2
}

# Generierte Regeldatei für ein Motiv schreiben (stdout = Pfad).
render_rules() { # motiv zieldatei
  sed "s|<placeholder>|$1|g" "$TEMPLATE" >"$2"
}

# TOML-Parse prüfen ganz ohne X11/Meta: Ohne DISPLAY scheitert erst der
# X11-Connect (Exit 1); kaputtes TOML scheitert früher (Exit 2).
assert_parses() { # toml-pfad beschreibung
  if DISPLAY= "$BIN" --rules "$1" "${DRY_RUN[@]}" >/dev/null 2>&1; then
    echo "SELF-TEST-FEHLER: $2 unerwartet Exit 0" >&2
    return 1
  else
    local code=$?
    if [ "$code" -ne 1 ]; then
      echo "SELF-TEST-FEHLER: $2 parst nicht (Exit $code)" >&2
      return 1
    fi
  fi
}

if [ "$SELF_TEST" -eq 1 ]; then
  tmp="$(mktemp -d)"
  trap 'rm -rf "$tmp"' EXIT
  fail=0
  for s in "${SUBJECTS[@]}"; do
    render_rules "$s" "$tmp/rules.toml"
    grep -q "<placeholder>" "$tmp/rules.toml" &&
      { echo "SELF-TEST-FEHLER: Platzhalter übrig bei '$s'" >&2; fail=1; }
    grep -qF "$s" "$tmp/rules.toml" ||
      { echo "SELF-TEST-FEHLER: Motiv fehlt bei '$s'" >&2; fail=1; }
    assert_parses "$tmp/rules.toml" "generiert ($s)" || fail=1
  done
  assert_parses "$RESET_RULES" "meta_url.toml" || fail=1
  if [ "$fail" -eq 0 ]; then
    echo "SELF-TEST: PASS (${#SUBJECTS[@]} Motive + Reset-Regel)"
  else
    echo "SELF-TEST: FAIL" >&2
    exit 1
  fi
  exit 0
fi

# Echter Stapel-Lauf (braucht Display + Meta-Account).
TMPDIR_RUN="$(mktemp -d)"
trap 'rm -rf "$TMPDIR_RUN"' EXIT
run_step() { # secs toml beschreibung
  local code=0
  echo timeout "$1" "$BIN" -a --rules "$2" "${DRY_RUN[@]}" || code=$?
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

n=0
for s in "${SELECTED[@]}"; do
  n=$((n + 1))
  echo "--- [$n/${#SELECTED[@]}] Motiv: $s ---"
  render_rules "$s" "$TMPDIR_RUN/rules.toml"
  run_step "$STICKER_SECS" "$TMPDIR_RUN/rules.toml" "Sticker '$s'"
  run_step "$URL_SECS" "$RESET_RULES" "URL-Reset nach '$s'"
done
echo "FERTIG: $n Sticker angestoßen."
