#!/usr/bin/env bash
# Lädt SCRFD- und ArcFace-ONNX-Modelle aus den yakhyo-Releases.
# Keine Binärmodelle ins Git committen (siehe .gitignore).
set -euo pipefail

MODELS_DIR="${1:-.}"
BASE="https://github.com/yakhyo/face-reidentification/releases/download/v0.0.1"

mkdir -p "$MODELS_DIR"

fetch() {
    local name="$1" min_bytes="$2"
    local dest="$MODELS_DIR/$name"
    if [ -f "$dest" ] && [ "$(stat -c%s "$dest")" -ge "$min_bytes" ]; then
        echo "ok: $dest bereits vorhanden"
        return 0
    fi
    echo "lade $name ..."
    curl -fSL --retry 3 -o "$dest" "$BASE/$name"
    local size
    size="$(stat -c%s "$dest")"
    if [ "$size" -lt "$min_bytes" ]; then
        echo "FEHLER: $dest zu klein ($size Bytes)" >&2
        rm -f "$dest"
        return 1
    fi
    echo "ok: $dest ($size Bytes)"
}

fetch "det_500m.onnx" 2000000
fetch "w600k_mbf.onnx" 12000000
echo "alle Modelle bereit in $MODELS_DIR"
