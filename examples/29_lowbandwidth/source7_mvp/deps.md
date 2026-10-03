# deps.md — Abhängigkeiten von `source7_mvp` (GitHub `org/projekt` für DeepWiki)

Stand: 2026-10-03, alle auf neuester lauffähiger Version (`cargo upgrade`).

| Crate | Version | GitHub | Wo | Wofür (ersetzt in `source6`) |
|---|---|---|---|---|
| `serde` | 1.0.229 | `serde-rs/serde` | common, server | Derive für Protokoll-Typen (ersetzt Hand-Codec) |
| `bincode` | 2.0.1 | `bincode-org/bincode` | common | `bincode::serde::{encode_to_vec, decode_from_slice}` mit `config::standard()` (ersetzt `common/02_codec.rs`) |
| `clap` | 4.6.7 | `clap-rs/clap` | server, client | `#[derive(Parser)]` CLI (ersetzt `01_config.rs` handgeparst) |
| `image` | 0.25.10 | `image-rs/image` | server | `RgbImage`, Crop, PPM (ersetzt `server/03_image.rs`) |
| `scrap` | 0.5.0 | `quadrupleslap/scrap` | server | MIT-SHM-Capture, BGRX→RGB (ersetzt `x11rb`-`GetImage` in `02_capture.rs`) |
| `enigo` | 0.6.1 | `enigo-rs/enigo` | server | Maus/Tastatur-Injektion (ersetzt `x11rb`-XTEST in `13_input.rs`) |
| `serde_yaml` | 0.9.34 | `dtolnay/serde-yaml` | server | `character_dict` aus `inference.yml` (ersetzt `load_dict`-Handparser) |
| `rav1e` | 0.8.1 | `xiph/rav1e` | server | AV1-Still-Picture-Encoder, **ohne** `asm` (kein `nasm` nötig) |
| `rav1d` | 1.1.0 | `memorysafety/rav1d` | client | AV1-Decoder (`bitdepth_8`), wie bisher |
| `ort` | 2.0.0-rc.13 | `pykeio/ort` | server | ONNX Runtime für PP-OCRv6 (nur Detektion+Erkennung, kein YOLO mehr) |
| `macroquad` | 0.4.16 | `not-fl3/macroquad` | client | Fenster, Textur, Default-Font-Text, Eingabe (ohne Unifont, ohne Clipboard) |

Bekannte Auffälligkeiten:

- `bincode` 3.0.0 existiert auf crates.io, ist aber ein kaputter Stub
  (einzige Zeile: `compile_error!("https://xkcd.com/2347/")`). Wir bleiben
  bewusst auf 2.0.1, bis ein lauffähiges 3.x erscheint.
- `image` liegt doppelt im Baum (0.25.10 direkt, 0.24.9 via `macroquad`);
  es werden keine `image`-Typen über die macroquad-Grenze gereicht (nur
  `Vec<u8>`), daher unkritisch.
- `ort` ist weiterhin nur als Release-Candidate aktuell (rc.13).

Bewusst **nicht** übernommen (evaluiert, verworfen):

| Crate | GitHub | Grund |
|---|---|---|
| `governor` | `antifuchs/governor` | Kein Rate-Limit im MVP nötig; direktes TCP-Schreiben genügt |
| `xcap` | `nashaofu/xcap` | Verworfene Alternative zu `scrap`: 0.9 braucht Wayland+EGL-Systemlibs ohne Feature-Gate; `scrap` braucht nur libxcb |
| `imageproc` | `image-rs/imageproc` | Maskieren sind 10 Zeilen mit `image` allein; keine extra Dep |
| `x11rb` | `psychon/x11rb` | Ersetzt durch `scrap` (Capture) + `enigo` (Input) |
| `miniquad` (direkt) | `not-fl3/miniquad` | Nur transitiv via `macroquad`; kein Clipboard im MVP |

Modelle (Laufzeit, nicht im Git — aus `../source6/models` wiederverwendet):

| Datei | Quelle |
|---|---|
| `PP-OCRv6_small_det.onnx`, `PP-OCRv6_small_rec.onnx`, `inference.yml` | HuggingFace `PaddlePaddle/PP-OCRv6_small_{det,rec}_onnx` (GitHub `PaddlePaddle/PaddleOCR`) |

Gestrichen: `gpa_640_int8.onnx` (YOLO/GUI-Detektor entfällt), GNU Unifont
(Client nutzt den eingebauten `macroquad`-Font).

Systempakete für Build/Laufzeit: `libxcb1-dev`, `libxcb-shm0-dev`,
`libxcb-randr0-dev` (nur Link-Symlinks für `scrap`; kein `nasm`, keine
X11-Header). Für Tests/Smoke: `xvfb`, `xterm`.

DeepWiki-Beispiel: `ask_wiki_question(repoName="enigo-rs/enigo", question="...")`.
