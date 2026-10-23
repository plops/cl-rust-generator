#!/usr/bin/env bash
# hil-setup.sh — richtet die Browser-HIL-Werkzeuge ein (idempotent).
# Alles landet in $HIL_DIR (Default /tmp/live-hil), NICHT im Repo:
# Chrome-for-testing, npm-Treiber (playwright-core, ws), Fake-Mikro-WAV.
# Standard-Tools: node, ffmpeg, curl, unzip.
set -euo pipefail

HIL_DIR="${HIL_DIR:-/tmp/live-hil}"
CHINESE_SRC="${CHINESE_SRC:-/workspace/src/cl-rust-generator/chinese/十萬個為什麼 (Tell Me Why) ｜ 中文 (Mandarin)｜ 第1積 [BkTXRhCAfNw].webm}"

need() { command -v "$1" >/dev/null 2>&1 || { echo "FEHLT: $1 bitte installieren"; exit 1; }; }
need node; need ffmpeg; need curl; need unzip
mkdir -p "$HIL_DIR"
cd "$HIL_DIR"

if [ ! -x chrome-linux64/chrome ]; then
  echo "-- lade Chrome-for-testing (stable) ..."
  curl -s --max-time 60 https://googlechromelabs.github.io/chrome-for-testing/last-known-good-versions-with-downloads.json -o cft.json
  URL=$(node -e "const d=require('./cft.json');for(const x of d.channels.Stable.downloads.chrome){if(x.platform==='linux64'){console.log(x.url);break;}}")
  curl -sL --max-time 280 -o chrome.zip "$URL"
  unzip -q -o chrome.zip
  rm -f chrome.zip
else
  echo "-- chrome vorhanden: $(./chrome-linux64/chrome --version)"
fi

if [ ! -d node_modules/playwright-core ] || [ ! -d node_modules/ws ]; then
  echo "-- installiere playwright-core + ws ..."
  [ -f package.json ] || npm init -y >/dev/null 2>&1
  npm i --no-audit --no-fund playwright-core ws 2>&1 | tail -1
else
  echo "-- npm-treiber vorhanden"
fi

if [ ! -f mic-zh.wav ]; then
  echo "-- extrahiere Fake-Mikro (45 s Mandarin, 48 kHz mono16) ..."
  ffmpeg -y -v error -ss 30 -t 45 -i "$CHINESE_SRC" -ac 1 -ar 48000 -c:a pcm_s16le mic-zh.wav
else
  echo "-- mic-zh.wav vorhanden"
fi

echo "== HIL-Setup OK in $HIL_DIR =="
./chrome-linux64/chrome --version
node --version
ffmpeg -version 2>/dev/null | head -1
