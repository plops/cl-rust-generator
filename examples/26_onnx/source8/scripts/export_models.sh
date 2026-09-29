#!/bin/bash
# export_models.sh — komplette Asset-Kette: Modell laden, Screens erzeugen,
# ONNX-Varianten exportieren/quantisieren (uv-verwaltetes Python).
#
# Aufruf (aus source8/):  ./scripts/export_models.sh
# Voraussetzungen: uv, cargo, xvfb/xterm/x11-apps (siehe scripts/make_screens.sh),
# Browser (scripts/fetch_browsers.sh) + Internet für die Website-Screens
set -euo pipefail
cd "$(dirname "$0")/.."

./scripts/fetch_model.sh
cargo build --release
[ -f models/screens/calib/scene1.ppm ] || ./scripts/make_screens.sh
[ -d models/screens/eval ] || ./scripts/make_web_screens.sh
(cd python && uv sync --frozen && uv run python export.py --models ../models)
ls -la models/*.onnx
