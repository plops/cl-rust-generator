# deps.md — Abhängigkeiten (GitHub `org/projekt` für DeepWiki)

| Crate | Version | GitHub | Wo | Wofür |
|---|---|---|---|---|
| `rav1e` | 0.8.1 | `xiph/rav1e` | server | AV1-Still-Picture-Encoder (Features `threading`, `asm` → braucht `nasm`) |
| `ort` | 2.0.0-rc.13 | `pykeio/ort` | server | ONNX Runtime für PP-OCRv6 und GPA-GUI-Detector |
| `x11rb` | 0.14.0 | `psychon/x11rb` | server | X11-Capture (`GetImage`) und Eingabe-Injektion (Feature `xtest`) |
| `macroquad` | 0.4.16 | `not-fl3/macroquad` | client | Fenster, Textur, Text (fontdue), Eingabe; ohne Default-Features |
| `miniquad` | 0.4.x (via macroquad) | `not-fl3/miniquad` | client | Clipboard (`window::clipboard_get/set`) |
| `rav1d` | 1.1.0 | `memorysafety/rav1d` | client | AV1-Decoder in reinem Rust (ohne `asm`, nur `bitdepth_8`) |

Modelle (Laufzeit, nicht im Git):

| Datei | Quelle |
|---|---|
| `PP-OCRv6_small_det.onnx`, `PP-OCRv6_small_rec.onnx`, `inference.yml` | HuggingFace `PaddlePaddle/PP-OCRv6_small_{det,rec}_onnx` (GitHub `PaddlePaddle/PaddleOCR`) |
| `gpa_640_int8.onnx` | exportiert in `26_onnx/source8` aus `Salesforce/GPA-GUI-Detector` (Ultralytics YOLO11, GitHub `ultralytics/ultralytics`) |

Schrift: GNU Unifont (`fonts-unifont`, `/usr/share/fonts/opentype/unifont/unifont.otf`).

DeepWiki-Beispiel: `ask_wiki_question(repoName="memorysafety/rav1d", question="...")`.
