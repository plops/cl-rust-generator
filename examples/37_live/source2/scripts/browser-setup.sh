#!/usr/bin/env bash
# browser-setup.sh — richtet die Browser-Test-Werkzeuge ein (idempotent).
# Alles landet in $HIL_DIR (Default /tmp/live-hil), NICHT im Repo:
# Chrome-for-Testing, npm-Treiber (playwright-core, ws), Fake-Mikro-WAV.
# Werkzeuge: node, ffmpeg, curl, unzip (Standard-Tools).
set -euo pipefail

HIL_DIR="${HIL_DIR:-/tmp/live-hil}"
CHINESE_SRC="${CHINESE_SRC:-/workspace/src/cl-rust-generator/chinese/十萬個為什麼 (Tell Me Why) ｜ 中文 (Mandarin)｜ 第1積 [BkTXRhCAfNw].webm}"

need() { command -v "$1" >/dev/null 2>&1 || { echo "FEHLT: $1 bitte installieren"; exit 1; }; }
need node; need ffmpeg; need curl; need unzip
mkdir -p "$HIL_DIR"
cd "$HIL_DIR"

if [ ! -x chrome-linux64/chrome ]; then
  echo "-- lade Chrome-for-Testing (stable) ..."
  curl -s --max-time 60 https://googlechromelabs.github.io/chrome-for-testing/last-known-good-versions-with-downloads.json -o cft.json
  URL=$(node -e "const d=require('./cft.json');for(const x of d.channels.Stable.downloads.chrome){if(x.platform==='linux64'){console.log(x.url);break;}}")
  curl -sL --max-time 280 -o chrome.zip "$URL"
  unzip -q -o chrome.zip
  rm -f chrome.zip
else
  echo "-- chrome vorhanden: $(./chrome-linux64/chrome --version 2>/dev/null || echo '?')"
fi

# Fehlende System-Libs fuer Headless-Chrome nachziehen (Debian/Ubuntu).
# Chrome meldet fehlende .so beim Start; wir installieren die ueblichen.
if command -v apt-get >/dev/null 2>&1; then
  if ! ./chrome-linux64/chrome --version >/dev/null 2>&1; then
    echo "-- installiere Chrome-System-Libs per apt-get ..."
    export DEBIAN_FRONTEND=noninteractive
    apt-get update -qq || true
    apt-get install -y -qq \
      libnss3 libnspr4 libatk1.0-0 libatk-bridge2.0-0 libcups2 libdrm2 \
      libxkbcommon0 libxcomposite1 libxdamage1 libxfixes3 libxrandr2 \
      libgbm1 libasound2t64 libpango-1.0-0 libcairo2 libatspi2.0-0 \
      2>&1 | tail -2 || echo "   (apt-get teilweise fehlgeschlagen — Libs ggf. schon da)"
  fi
fi

if [ ! -d node_modules/playwright-core ] || [ ! -d node_modules/ws ]; then
  echo "-- installiere playwright-core + ws ..."
  [ -f package.json ] || npm init -y >/dev/null 2>&1
  npm i --no-audit --no-fund playwright-core ws 2>&1 | tail -1
else
  echo "-- npm-treiber vorhanden"
fi

# Fake-Mikro: 30 s Mandarin, 16 kHz mono (Chrome mag WAV als Capture-Quelle).
if [ ! -f mic-zh.wav ]; then
  echo "-- extrahiere Fake-Mikro (30 s Mandarin, 16 kHz mono) ..."
  ffmpeg -y -v error -ss 30 -t 30 -i "$CHINESE_SRC" -ac 1 -ar 16000 -c:a pcm_s16le mic-zh.wav
else
  echo "-- mic-zh.wav vorhanden"
fi

echo "== Browser-Setup OK in $HIL_DIR =="
./chrome-linux64/chrome --version 2>/dev/null || echo "chrome: Version nicht abrufbar"
node --version
ffmpeg -version 2>/dev/null | head -1
