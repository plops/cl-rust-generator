#!/bin/sh
# Demo-Datei bauen: 3 s Sprache (SILK) + 3 s Musik (CELT).
# Aufruf: scripts/mkdemo.sh [ZIEL]  (Default: /tmp/demo.opus)
set -eu
cd "$(dirname "$0")/.."
ZIEL="${1:-/tmp/demo.opus}"
cargo run -q --release --example mkdemo -- "$ZIEL"
