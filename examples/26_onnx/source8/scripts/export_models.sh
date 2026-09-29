#!/bin/bash
# export_models.sh — komplette Asset-Kette: Modell laden, Screens erzeugen,
# ONNX-Varianten exportieren/quantisieren (uv-verwaltetes Python).
#
# Aufruf (aus source8/):  ./scripts/export_models.sh
# Voraussetzungen: uv, cargo, xvfb/xterm/x11-apps (siehe scripts/make_screens.sh)
set -euo pipefail
cd "$(dirname "$0")/.."

./scripts/fetch_model.sh
cargo build --release
[ -d models/screens ] || ./scripts/make_screens.sh
(cd python && uv sync --frozen && uv run python export.py --models ../models)
ls -la models/*.onnx
