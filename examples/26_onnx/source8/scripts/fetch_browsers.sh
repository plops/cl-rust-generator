#!/bin/bash
# fetch_browsers.sh — installiert Firefox (Mozilla-Tarball) und Chrome for
# Testing nach /opt/browsers plus die nötigen System-Bibliotheken/Fonts.
# Ubuntus apt-"firefox" ist nur ein Snap-Stub und läuft im Container nicht.
#
# Aufruf (als root):  ./scripts/fetch_browsers.sh [ZIEL]   (Default /opt/browsers)
# Idempotent: vorhandene Installationen werden übersprungen.
set -euo pipefail

DEST="${1:-/opt/browsers}"
mkdir -p "$DEST"

if [ "$(id -u)" = "0" ] && command -v apt-get >/dev/null; then
  DEBIAN_FRONTEND=noninteractive apt-get install -y -qq --no-install-recommends \
    unzip xz-utils libgtk-3-0t64 libdbus-glib-1-2 libasound2t64 libx11-xcb1 \
    libnss3 libgbm1 libxss1 libxtst6 libxrandr2 libxdamage1 libxcomposite1 \
    libcups2t64 libatk-bridge2.0-0t64 libpango-1.0-0 libxkbcommon0 \
    fonts-dejavu-core fonts-liberation fonts-noto-core fonts-noto-color-emoji >/dev/null
fi

if [ ! -x "$DEST/firefox/firefox" ]; then
  echo "download: Firefox (latest, de)"
  curl -fsSL --retry 3 -o "$DEST/firefox.tar.xz" \
    "https://download.mozilla.org/?product=firefox-latest-ssl&os=linux64&lang=de"
  tar -xJf "$DEST/firefox.tar.xz" -C "$DEST" && rm "$DEST/firefox.tar.xz"
fi
echo "ok: $("$DEST/firefox/firefox" --version 2>/dev/null)"

if [ ! -x "$DEST/chrome-linux64/chrome" ]; then
  url=$(curl -fsSL https://googlechromelabs.github.io/chrome-for-testing/last-known-good-versions-with-downloads.json |
    python3 -c "import sys,json; d=json.load(sys.stdin)['channels']['Stable']['downloads']['chrome']; print(next(x['url'] for x in d if x['platform']=='linux64'))")
  echo "download: $url"
  curl -fsSL --retry 3 -o "$DEST/chrome.zip" "$url"
  unzip -q -o "$DEST/chrome.zip" -d "$DEST" && rm "$DEST/chrome.zip"
fi
echo "ok: $("$DEST/chrome-linux64/chrome" --version 2>/dev/null)"
