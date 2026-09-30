# deps.md — 20260930_01_unicode (source9)

Registry für DeepWiki-Abfragen in `<organization>/<projekt>`-Notation.
Nur direkt genutzte Abhängigkeiten. Stand: 2026-09-30.

## Rust-Crates (direkt in `source9/Cargo.toml`)

| Crate | Org/Projekt | Version | Zweck |
|---|---|---|---|
| ort | pykeio/ort | 2.0.0-rc.13 (RC — API-Drift einplanen) | ONNX-Runtime-Sessions für Detektion + Erkennung (`commit_from_file`, `inputs!`, `TensorRef`) |
| fontdue | mooman219/fontdue | 0.9.4 (bereits transitiv via macroquad → keine zusätzliche Crate im Baum) | CPU-Rasterung von GNU Unifont in den 640×640-Canvas (Ground-Truth-Bild) |
| macroquad | not-fl3/macroquad | 0.4.16 (`default-features = false`) | Fenster, Textur, Tasten, HUD (nur interaktiver Modus) |

Bewusst **nicht** eingeführt: `rand` (SplitMix64 als Handcode, 20 Zeilen),
`unicode-normalization`/`unicode-segmentation` (Vergleich auf Codepoints,
Vollbreiten-Faltung als Handcode), `serde`/`serde_yaml` (Wörterbuch-Parser aus
source5), `x11rb` (kein Screen-Capture nötig — Bild entsteht im Speicher),
`reqwest` (Korpus-Download per Python-Stdlib-Skript).

## Modelle (HuggingFace, Apache-2.0, per `scripts/fetch_models.sh`, gepinnte Commits)

| Modell | HF-Repo | Einsatz |
|---|---|---|
| Detektion | PaddlePaddle/PP-OCRv6_small_det_onnx | DBNet, alle Sprachen |
| Erkennung universal | PaddlePaddle/PP-OCRv6_small_rec_onnx | de, fr, en, es, pl, ja, zh (+ Vergleich für alle) |
| Erkennung Latein | PaddlePaddle/latin_PP-OCRv5_mobile_rec_onnx | Alternative für de, fr, es, pl |
| Erkennung Ostslawisch | PaddlePaddle/eslav_PP-OCRv5_mobile_rec_onnx | ru, uk |
| Erkennung Griechisch | PaddlePaddle/el_PP-OCRv5_mobile_rec_onnx | el |
| Erkennung Koreanisch | PaddlePaddle/korean_PP-OCRv5_mobile_rec_onnx | ko |
| Erkennung Thai | PaddlePaddle/th_PP-OCRv5_mobile_rec_onnx | th |
| Erkennung Arabisch | PaddlePaddle/arabic_PP-OCRv5_mobile_rec_onnx | ar |
| Erkennung Devanagari | PaddlePaddle/devanagari_PP-OCRv5_mobile_rec_onnx | hi |
| Erkennung Tamil | PaddlePaddle/ta_PP-OCRv5_mobile_rec_onnx | ta |

Upstream-Code (Pre-/Postprocessing-Referenz): `PaddlePaddle/PaddleOCR`
(`ppocr/postprocess/rec_postprocess.py`: `CTCLabelDecode`, `pred_reverse`).

## System / Werkzeuge

| Paket | Quelle | Zweck |
|---|---|---|
| GNU Unifont | system (apt `fonts-unifont`) → `/usr/share/fonts/opentype/unifont/unifont.otf` | Schrift für Canvas + HUD |
| uv | astral-sh/uv | `uv run scripts/fetch_corpus.py` (nur Python-Stdlib) |
| Wikipedia-API | wikimedia/mediawiki (`action=query&prop=extracts`) | Trainingstexte für Markov-Ketten (CC BY-SA, nicht committet) |
| Xvfb, xdotool, scrot | system (apt `xvfb xdotool scrot`) | Xvfb-Nachweis (Tasten, Screenshots) |

DeepWiki-Muster: `mooman219/fontdue` (Metrics/Baseline), `pykeio/ort`
(Session-API), `not-fl3/macroquad` (Window::from_config, Texturen, Input),
`PaddlePaddle/PaddleOCR` (Rec-Pre-/Postprocessing). Repo-Kontext:
`plops/cl-rust-generator` (DeepWiki indiziert `examples/26_onnx` derzeit
nicht — lokale Dateien lesen).
