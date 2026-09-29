#!/bin/bash
# fetch_font.sh — puts GNU Unifont into the APK assets (not tracked in git).
#
# Source: Debian/Ubuntu package fonts-unifont (apt install fonts-unifont).
# Without it the app falls back to Typeface.MONOSPACE.
set -eu
cd "$(dirname "$0")/.."
DST=android-app/app/src/main/assets/fonts/unifont.otf
SRC="${UNIFONT_OTF:-/usr/share/fonts/opentype/unifont/unifont.otf}"
if [ ! -f "$SRC" ]; then
    echo "fetch_font: $SRC fehlt (apt install fonts-unifont) — App nutzt MONOSPACE" >&2
    exit 0
fi
mkdir -p "$(dirname "$DST")"
cmp -s "$SRC" "$DST" 2>/dev/null || cp "$SRC" "$DST"
echo "fetch_font: $DST ($(stat -c %s "$DST") B)"
