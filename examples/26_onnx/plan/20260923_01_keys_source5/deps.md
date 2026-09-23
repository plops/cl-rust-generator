# deps.md — 20260923_01_keys_source5

Registry für DeepWiki-Abfragen: GitHub-Notation `<organization>/<projekt>`.
Nur direkt genutzte Deps. Stand: 2026-09-23, aus `source5/Cargo.lock`.
Keine neue Abhängigkeit für das Pan/Zoom-Feature eingeführt
(Tasten/Clamp/Nearest sind Handcode).

| Crate/System | Org/Projekt bzw. Paket | Version (Stand) | Zweck |
|---|---|---|---|
| ort | pykeio/ort | 2.0.0-rc.13 (RC — Drift einplanen) | PP-OCRv6 det-/rec-Sessions, `inputs!`, `TensorRef` |
| x11rb | psychon/x11rb | 0.14.0 | X11-Connect + `get_image` ZPixmap mit ROI-Offsets |
| macroquad | not-fl3/macroquad | 0.4.16 (`default-features = false`) | Fenster, Textur-Blit, Tasten (`is_key_down`/`is_key_pressed`, `KeyCode`) |
| PP-OCRv6_small_det_onnx | PaddlePaddle/PP-OCRv6_small_det_onnx (HuggingFace, Apache-2.0) | `inference.onnx` (9,9 MB) | DB-Detektionsmodell (`include_bytes!`) |
| PP-OCRv6_small_rec_onnx | PaddlePaddle/PP-OCRv6_small_rec_onnx (HuggingFace, Apache-2.0) | `inference.onnx` (21 MB) + `inference.yml` (`character_dict`) | CTC-Erkennung + Wörterbuch (`include_bytes!`/`include_str!`) |
| GNU Unifont | system (apt: `fonts-unifont`, OpenType) | system (`/usr/share/fonts/opentype/unifont/unifont.otf`) | CJK-fähiges Label-Rendering |
| Xvfb | system (apt: `xvfb`) | system | Test-Display für Tasten-/E2E-Smoke |

DeepWiki-Abfrage-Muster: `psychon/x11rb` (GetImage-/Setup-API),
`not-fl3/macroquad` (Input-/Textur-API), `pykeio/ort` (Session-API).
Repo-Kontext: `plops/cl-rust-generator`.
NICHT eingeführt (bewusst): `winit`/`pixels`-Stack (Hunderte Crates),
`image`-Crate für Resize (Ein-Zweck-Nearest als Handcode),
Maus-/Wheel-Deps (außer Scope).
