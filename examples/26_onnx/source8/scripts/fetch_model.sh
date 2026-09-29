#!/bin/bash
# fetch_model.sh — lädt die PyTorch-Gewichte von Salesforce/GPA-GUI-Detector
# (MIT-Lizenz) plus ein Beispiel-Screenshot aus dem HF-Repo.
#
# Aufruf (aus source8/):  ./scripts/fetch_model.sh [ZIEL-VERZEICHNIS]
# Default-Ziel: source8/models/ (steht in .gitignore, wird nie committet).
# Idempotent: vorhandene Dateien mit korrekter SHA256 werden übersprungen.
set -euo pipefail

SRC_DIR="$(cd "$(dirname "$0")/.." && pwd)"
OUT="${1:-$SRC_DIR/models}"
# Commit gepinnt → reproduzierbar, auch wenn das HF-Repo später wächst.
REV="d04be6b715acb517068ca15d4b79159d26292713"
BASE="https://huggingface.co/Salesforce/GPA-GUI-Detector/resolve/$REV"

mkdir -p "$OUT"

fetch() { # pfad-im-repo sha256 ziel
  local path="$1" sha="$2" dest="$3"
  if [ -f "$dest" ] && echo "$sha  $dest" | sha256sum -c --status -; then
    echo "ok (cached): $dest"
    return 0
  fi
  echo "download: $path -> $dest"
  curl -fsSL --retry 3 -o "$dest.tmp" "$BASE/$path"
  if ! echo "$sha  $dest.tmp" | sha256sum -c --status -; then
    echo "FEHLER: Prüfsumme falsch für $dest" >&2
    rm -f "$dest.tmp"
    return 1
  fi
  mv "$dest.tmp" "$dest"
  echo "ok (verified): $dest"
}

fetch model.pt dd404f25b7f329998c7ed97a67827a174dd626dc2683f6844ab33a5219c05f71 "$OUT/model.pt"
# Beispielbild: SHA wird beim ersten Lauf nicht erzwungen (Screenshot ist
# nur Test-Fixture); siehe unten.
if [ ! -f "$OUT/example_input.png" ]; then
  curl -fsSL --retry 3 -o "$OUT/example_input.png" "$BASE/images/example_input.png"
fi
echo "ok: $OUT/example_input.png"
