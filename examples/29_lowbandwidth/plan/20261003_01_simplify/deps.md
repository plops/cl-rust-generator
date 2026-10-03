# deps.md — Abhängigkeiten von `source7_mvp` (GitHub `org/projekt` für DeepWiki)

| Crate | Version (MVP-Start) | GitHub | Wo | Wofür (ersetzt in `source6`) |
|---|---|---|---|---|
| `serde` | 1.0.x | `serde-rs/serde` | common, server, client | Derive für Protokoll-Typen (ersetzt Hand-Codec) |
| `bincode` | 2.x | `bincode-org/bincode` | common | Binärkodierung `encode_to_vec`/`decode_from_slice` mit `config::standard()` (ersetzt `common/02_codec.rs`) |
| `clap` | 4.5.x | `clap-rs/clap` | server, client | `#[derive(Parser)]` CLI (ersetzt `01_config.rs` handgeparst) |
| `image` | 0.25.x | `image-rs/image` | server, client | `RgbImage`/`RgbaImage`, Crop, PPM (ersetzt `server/03_image.rs`) |
| `xcap` | 0.4.x/0.5.x | `nashaofu/xcap` | server | `Monitor::capture_region` (ersetzt `x11rb`-`GetImage` in `02_capture.rs`) |
| `enigo` | 0.6.x | `enigo-rs/enigo` | server | Maus/Tastatur-Injektion (ersetzt `x11rb`-XTEST in `13_input.rs`) |
| `serde_yaml` | 0.9.x | `dtolnay/serde-yaml` | server | `character_dict` aus `inference.yml` (ersetzt `load_dict`-Handparser) |
| `rav1e` | 0.8.x | `xiph/rav1e` | server | AV1-Still-Picture-Encoder, **ohne** `asm` (kein `nasm` nötig) |
| `rav1d` | 1.x | `memorysafety/rav1d` | client | AV1-Decoder (`bitdepth_8`), wie bisher |
| `ort` | 2.0.0-rc.x | `pykeio/ort` | server | ONNX Runtime für PP-OCRv6 (nur Detektion+Erkennung, kein YOLO mehr) |
| `macroquad` | 0.4.x | `not-fl3/macroquad` | client | Fenster, Textur, Default-Font-Text, Eingabe (ohne Unifont, ohne Clipboard) |

Bewusst **nicht** übernommen (evaluiert, verworfen):

| Crate | GitHub | Grund |
|---|---|---|
| `governor` | `antifuchs/governor` | Kein Rate-Limit im MVP nötig; direktes TCP-Schreiben genügt |
| `scrap` | `quadrupleslap/scrap` | Alternative zu `xcap`; `xcap` liefert direkt `image::RgbaImage` + `capture_region` |
| `imageproc` | `image-rs/imageproc` | Maskieren sind 10 Zeilen mit `image` allein; keine extra Dep |
| `x11rb` | `psychon/x11rb` | Ersetzt durch `xcap` (Capture) + `enigo` (Input) |
| `miniquad` (direkt) | `not-fl3/miniquad` | Nur transitiv via `macroquad`; kein Clipboard im MVP |

Modelle (Laufzeit, nicht im Git — aus `source6/models` wiederverwendet):

| Datei | Quelle |
|---|---|
| `PP-OCRv6_small_det.onnx`, `PP-OCRv6_small_rec.onnx`, `inference.yml` | HuggingFace `PaddlePaddle/PP-OCRv6_small_{det,rec}_onnx` (GitHub `PaddlePaddle/PaddleOCR`) |

Gestrichen: `gpa_640_int8.onnx` (YOLO/GUI-Detektor entfällt), GNU Unifont
(Client nutzt den eingebauten `macroquad`-Font).

DeepWiki-Beispiel: `ask_wiki_question(repoName="enigo-rs/enigo", question="...")`.
Systempakete für Build/Laufzeit: keine neuen (kein `nasm`, keine X11-Header;
nur X-Laufzeitlibs, die mit `xvfb` ohnehin kommen). Für Tests: `xvfb`, `xterm`.
