# source8 — GUI-Element-Detektion mit Salesforce/GPA-GUI-Detector

PoC: YOLO11m (1 Klasse `icon`) als ONNX in Rust (`ort`), CPU und CUDA,
fp32/fp16/int8. Plan und Walkthrough: `../plan/20260929_01_gui_elements/`.
Messwerte: [bench.md](bench.md).

```sh
# Assets (nie im Git): model.pt laden, Xvfb-Screens, ONNX-Export/Quantisierung
./scripts/export_models.sh

# Detektion (TSV x1 y1 x2 y2 score auf stdout, annotiertes PPM optional)
cargo run --release -- detect models/gpa_384x640_int8.onnx models/example_input.ppm --out /tmp/a.ppm
cargo run --release -- detect models/gpa_384x640_int8.onnx x11          # Live-Bildschirm

# Benchmarks / GPU
cargo run --release -- bench models/example_input.ppm models/*.onnx --device cpu --threads 8
cargo run --release --features cuda -- bench x11 models/gpa_384x640_fp16.onnx --device cuda
./scripts/bench.sh 30

# Tests
cargo test                         # Unit + CLI-Smoke (ohne Modelle)
cargo test --release -- --ignored  # Parität zu Ultralytics (braucht models/)
./scripts/smoke_xvfb.sh            # X11-End-to-end unter Xvfb
```

Skripte: `fetch_model.sh` (HF-Download, SHA256), `fetch_browsers.sh`
(Firefox + Chrome for Testing nach `/opt/browsers`), `make_screens.sh`
(X11-Szenen → `models/screens/calib`), `make_web_screens.sh` (Firefox/Chrome
auf Websites → `calib/` bzw. disjunkt `eval/`), `export_models.sh`
(komplette Kette), `bench.sh` (Matrix), `smoke_xvfb.sh` (Nachweis). Python läuft ausschließlich über
`uv` in `python/` (nur Build-Zeit).

Empfehlung: CPU → `gpa_384x640_int8.onnx` mit `--threads 8`;
GPU → `gpa_384x640_fp16.onnx`.
