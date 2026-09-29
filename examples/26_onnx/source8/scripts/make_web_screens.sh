#!/bin/bash
# make_web_screens.sh — Browser-Screenshots unter Xvfb (1920×1080) als PPM:
# Firefox und Chrome öffnen echte Websites. Kalibrier- und Test-Websites
# sind disjunkt (andere Domains), damit die INT8-Auswertung nicht auf den
# Kalibrierbildern selbst stattfindet.
#
# Aufruf (aus source8/):  ./scripts/make_web_screens.sh [BINARY] [OUT-DIR]
# Default: target/release/gui_detect, models/screens → calib/ + eval/
# Voraussetzungen: ./scripts/fetch_browsers.sh, xvfb, Internetzugang
set -uo pipefail

BIN="${1:-target/release/gui_detect}"
OUT="${2:-models/screens}"
BR="${BROWSERS:-/opt/browsers}"
WAIT="${WAIT:-9}"
DISP="${DISPLAY_NUM:-95}"
mkdir -p "$OUT/calib" "$OUT/eval"
export DISPLAY=":$DISP"

CALIB=(
  "https://en.wikipedia.org/wiki/Rust_(programming_language)"
  "https://github.com/rust-lang/rust"
  "https://docs.rs/ort/latest/ort/"
  "https://stackoverflow.com/questions"
  "https://news.ycombinator.com/"
  "https://www.python.org/"
  "https://developer.mozilla.org/en-US/docs/Web/HTML"
  "https://huggingface.co/Salesforce/GPA-GUI-Detector"
  "https://www.openstreetmap.org/"
  "https://crates.io/"
  "https://www.heise.de/"
  "https://www.bbc.com/news"
)
EVAL=(
  "https://de.wikipedia.org/wiki/Linux"
  "https://gitlab.com/explore"
  "https://www.reddit.com/r/rust/"
  "https://www.amazon.de/"
  "https://www.spiegel.de/"
  "https://duckduckgo.com/?q=onnx+runtime"
)

Xvfb ":$DISP" -screen 0 1920x1080x24 >/dev/null 2>&1 &
XVFB=$!
trap 'kill $XVFB 2>/dev/null; pkill -f "$BR/" 2>/dev/null' EXIT
sleep 1.5

# Firefox-Profil ohne Erststart-Seiten/Hinweisleisten.
FFP=$(mktemp -d)
cat >"$FFP/user.js" <<'EOF'
user_pref("browser.aboutwelcome.enabled", false);
user_pref("browser.startup.homepage_override.mstone", "ignore");
user_pref("datareporting.policy.dataSubmissionPolicyBypassNotification", true);
user_pref("browser.shell.checkDefaultBrowser", false);
user_pref("trailhead.firstrun.didSeeAboutWelcome", true);
EOF

# shot <browser> <url> <ziel.ppm> <geometrie WxH+X+Y>
shot() {
  local b="$1" url="$2" dest="$3" geo="$4"
  local w="${geo%%x*}" rest="${geo#*x}"
  local h="${rest%%+*}" pos="${geo#*+}"
  local x="${pos%%+*}" y="${pos#*+}"
  if [ "$b" = firefox ]; then
    "$BR/firefox/firefox" --no-remote --profile "$FFP" --width "$w" --height "$h" "$url" >/dev/null 2>&1 &
  else
    "$BR/chrome-linux64/chrome" --no-sandbox --no-first-run --no-default-browser-check \
      --disable-search-engine-choice-screen --user-data-dir="$(mktemp -d)" \
      --window-size="$w,$h" --window-position="$x,$y" "$url" >/dev/null 2>&1 &
  fi
  local pid=$!
  sleep "$WAIT"
  "$BIN" grab "$dest" >/dev/null && echo "ok: $dest ($b, $geo)"
  kill "$pid" 2>/dev/null; pkill -f "$BR/" 2>/dev/null; sleep 1
}

# Abwechselnd Vollbild und kleineres Fenster (Desktop-Hintergrund sichtbar).
run() { # set-name urls...
  local set="$1"; shift
  local i=0
  for url in "$@"; do
    for b in firefox chrome; do
      local geo="1920x1080+0+0"
      [ $(((i + ${#b}) % 2)) = 1 ] && geo="1400x900+200+80"
      local name
      name=$(echo "$url" | sed -E 's#https?://(www\.)?##; s#[^A-Za-z0-9]+#_#g; s#_$##' | cut -c1-40)
      shot "$b" "$url" "$OUT/$set/${b}_${name}.ppm" "$geo"
    done
    i=$((i + 1))
  done
}

run calib "${CALIB[@]}"
run eval "${EVAL[@]}"
ls "$OUT/calib" | wc -l
ls "$OUT/eval" | wc -l
