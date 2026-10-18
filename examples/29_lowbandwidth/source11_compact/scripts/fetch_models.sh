#!/bin/bash
# fetch_models.sh — legt die Laufzeit-Modelle in source11_compact/models/ ab.
#
#   PP-OCRv6 det/rec + inference.yml: aus ../../26_onnx/source5 kopieren,
#     sonst von HuggingFace laden (SHA256 geprüft, URLs/SHAs wie source6).
#   gpa_640_int8.onnx (GUI-Detektor): aus ../../26_onnx/source8/models
#     kopieren; fehlt es, dort `./scripts/export_models.sh` ausführen.
#     Ohne dieses Modell läuft der Server trotzdem (nur ohne GUI-Pfad).
#
# Aufruf (aus source11_compact): ./scripts/fetch_models.sh
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
OUT="$ROOT/models"
S5="$ROOT/../../26_onnx/source5"
S8="$ROOT/../../26_onnx/source8/models"
mkdir -p "$OUT"

DET_SHA="d73e0058b7a8086bbd57f3d10b8bcd4ff95363f67e06e2762b5e814fe9c9410e"
REC_SHA="5435fd747c9e0efe15a96d0b378d5bd157e9492ed8fd80edf08f30d02fa24634"
YML_SHA="ab078671bb49f06228eadccd34f1bb501e157f7a047095ffb943ba81512c77d1"
HF="https://huggingface.co/PaddlePaddle"

get() { # name sha lokale-quelle url
  local dest="$OUT/$1"
  if [ -f "$dest" ] && echo "$2  $dest" | sha256sum -c --status -; then
    echo "ok (cached): $1"; return
  fi
  if [ -f "$3" ] && echo "$2  $3" | sha256sum -c --status -; then
    cp "$3" "$dest"; echo "ok (copied): $1"; return
  fi
  echo "download: $4"
  curl -sSL --fail --retry 3 -o "$dest.tmp" "$4"
  echo "$2  $dest.tmp" | sha256sum -c --status - || { echo "FEHLER: SHA256 $1" >&2; rm -f "$dest.tmp"; exit 1; }
  mv "$dest.tmp" "$dest"; echo "ok (verified): $1"
}

get PP-OCRv6_small_det.onnx "$DET_SHA" "$S5/PP-OCRv6_small_det.onnx" "$HF/PP-OCRv6_small_det_onnx/resolve/main/inference.onnx"
get PP-OCRv6_small_rec.onnx "$REC_SHA" "$S5/PP-OCRv6_small_rec.onnx" "$HF/PP-OCRv6_small_rec_onnx/resolve/main/inference.onnx"
get inference.yml "$YML_SHA" "$S5/inference.yml" "$HF/PP-OCRv6_small_rec_onnx/resolve/main/inference.yml"

if [ -f "$S8/gpa_640_int8.onnx" ]; then
  cp -u "$S8/gpa_640_int8.onnx" "$OUT/"; echo "ok (copied): gpa_640_int8.onnx"
else
  echo "hinweis: gpa_640_int8.onnx fehlt (26_onnx/source8: ./scripts/export_models.sh)"
fi
exit 0
