#!/bin/bash
# fetch_assets.sh — lädt Modelle + Wörterbuch für den source5-Viewer
# und stellt die Unifont-Schrift sicher.
#
# Aufruf (aus dem source5-Verzeichnis):
#   ./scripts/fetch_assets.sh [ZIEL-VERZEICHNIS]
#
# Ohne Argument landen die Dateien neben diesem Skript in source5/
# (dort erwartet sie der Build per include_bytes!/include_str!).
# Bereits vorhandene Dateien mit korrekter Prüfsumme werden übersprungen.
#
# Quellen (Apache-2.0):
#   https://huggingface.co/PaddlePaddle/PP-OCRv6_small_det_onnx
#   https://huggingface.co/PaddlePaddle/PP-OCRv6_small_rec_onnx
set -euo pipefail

SRC_DIR="$(cd "$(dirname "$0")/.." && pwd)"
OUT="${1:-$SRC_DIR}"

DET_URL="https://huggingface.co/PaddlePaddle/PP-OCRv6_small_det_onnx/resolve/main/inference.onnx"
REC_URL="https://huggingface.co/PaddlePaddle/PP-OCRv6_small_rec_onnx/resolve/main/inference.onnx"
YML_URL="https://huggingface.co/PaddlePaddle/PP-OCRv6_small_rec_onnx/resolve/main/inference.yml"

DET_SHA="d73e0058b7a8086bbd57f3d10b8bcd4ff95363f67e06e2762b5e814fe9c9410e"
REC_SHA="5435fd747c9e0efe15a96d0b378d5bd157e9492ed8fd80edf08f30d02fa24634"
YML_SHA="ab078671bb49f06228eadccd34f1bb501e157f7a047095ffb943ba81512c77d1"

mkdir -p "$OUT"

fetch() { # url sha ziel
  local url="$1" sha="$2" dest="$3"
  if [ -f "$dest" ] && echo "$sha  $dest" | sha256sum -c --status -; then
    echo "ok (cached): $dest"
    return 0
  fi
  echo "download: $url -> $dest"
  curl -sSL --fail --retry 3 -o "$dest.tmp" "$url"
  if ! echo "$sha  $dest.tmp" | sha256sum -c --status -; then
    echo "FEHLER: Prüfsumme falsch für $dest" >&2
    rm -f "$dest.tmp"
    return 1
  fi
  mv "$dest.tmp" "$dest"
  echo "ok (verified): $dest"
}

fetch "$DET_URL" "$DET_SHA" "$OUT/PP-OCRv6_small_det.onnx"
fetch "$REC_URL" "$REC_SHA" "$OUT/PP-OCRv6_small_rec.onnx"
fetch "$YML_URL" "$YML_SHA" "$OUT/inference.yml"

# Schrift (Laufzeit-Dep, kein Build-Input): Suchliste aus 05_overlay.rs.
for p in /usr/share/fonts/opentype/unifont/unifont.otf \
         /usr/share/fonts/unifont/unifont.otf \
         /usr/share/fonts/truetype/unifont/unifont.ttf; do
  if [ -f "$p" ]; then
    echo "ok (font): $p"
    exit 0
  fi
done

echo "font missing: GNU Unifont nicht gefunden."
if command -v apt-get >/dev/null && [ "$(id -u)" = "0" ]; then
  echo "install: apt-get install -y fonts-unifont"
  apt-get install -y fonts-unifont
else
  echo "bitte installieren: apt-get install fonts-unifont" >&2
  exit 1
fi
