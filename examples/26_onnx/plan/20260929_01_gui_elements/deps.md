# deps.md — 20260929_01_gui_elements

Registry für DeepWiki-Abfragen: GitHub-Notation `<organization>/<projekt>`.
Nur direkt genutzte Deps. Stand: 2026-09-29.

## Rust (Laufzeit, `source8/Cargo.toml`)

| Crate | Org/Projekt | Version | Zweck hier |
|---|---|---|---|
| ort (no-default + `download-binaries`, `copy-dylibs`, `tls-native`; optional `cuda`) | pykeio/ort | 2.0.0-rc.13 (lädt ONNX Runtime 1.28.0) | Session, CPU-/CUDA-EP, Inferenz |
| x11rb | psychon/x11rb | 0.14.0 | Root-Window-Grab (Z-Pixmap BGRA) |

## Python (nur Build-Zeit, `source8/python/pyproject.toml`, via `uv`)

| Paket | Org/Projekt | Version (uv.lock) | Zweck hier |
|---|---|---|---|
| ultralytics | ultralytics/ultralytics | 8.4.165 | `.pt` laden, ONNX-Export, Referenz-Inferenz |
| torch / torchvision (CPU-Index) | pytorch/pytorch, pytorch/vision | 2.14.0 / 0.29.0 | Tracing für den Export |
| onnx | onnx/onnx | 1.23.0 | Graph laden/prüfen/speichern |
| onnxslim | inisis/OnnxSlim | 0.1.97 | Graph-Vereinfachung (`simplify=True`) |
| onnxruntime | microsoft/onnxruntime | 1.30.0 | FP16-Konvertierung, INT8-Quantisierung, Python-Parität |
| opencv-python-headless | opencv/opencv-python | 5.0.0.93 | ersetzt `opencv-python` (kein libGL im Container) |

## Modell / Referenzen

| Asset | Quelle | Zweck |
|---|---|---|
| `model.pt` (YOLO11m, 1 Klasse `icon`, 40,6 MB, MIT) | HF `Salesforce/GPA-GUI-Detector` @ `d04be6b7` | per `scripts/fetch_model.sh`, nie im Git |
| OmniParser (Vorgänger-Ökosystem) | microsoft/OmniParser | Referenz für `predict_yolo`-Parameter |
| Vorgänger im Repo | plops/cl-rust-generator (`source5`, `source7`) | Capture-/ort-Muster, CUDA-Probe |

## System (apt)

`xvfb`, `xterm`, `x11-apps`, `xdotool` (Headless-Tests, Screenshot-Erzeugung).

DeepWiki-Abfrage-Muster: `pykeio/ort` (EP-Registrierung, `TensorRef`),
`ultralytics/ultralytics` (Export, LetterBox, Output-Layout),
`microsoft/onnxruntime` (`quantize_static`, QDQ, CUDA-EP fp16).

NICHT eingeführt (bewusst): `image`/`png` (PPM-I/O + X11 sind Handcode;
PNG→PPM erledigt Python beim Export), `ndarray` (`TensorRef` aus
`(shape, &[f32])`), `clap` (CLI per `std::env`), `onnxconverter-common`
(FP16 via `onnxruntime.transformers.float16`).
