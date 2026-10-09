# deps.md — Abhängigkeiten von `source9_gpu` (GitHub `org/projekt` für DeepWiki)

Stand: 2026-10-09, alle auf neuester tragfähiger Version (per crates.io-API
geprüft; identischer Satz wie `source7_mvp`, keine neuen Crates — CUDA kommt
nur als `ort`-Feature dazu).

| Crate | Version | GitHub | Wo | Wofür |
|---|---|---|---|---|
| `serde` | 1.0.229 | `serde-rs/serde` | common, server | Derive für Protokoll-Typen |
| `bincode` | 2.0.1 | `bincode-org/bincode` | common | `bincode::serde::{encode_to_vec, decode_from_slice}` mit `config::standard()` |
| `clap` | 4.6.7 | `clap-rs/clap` | server, client | `#[derive(Parser)]` CLI (Server: `--cpu`-Flag dazu) |
| `image` | 0.24.9 | `image-rs/image` | server | `RgbImage`, Crop, Kanten-Padding. Bewusst 0.24 wie `macroquad`: eine Version im Baum. `default-features = false`; `png`/`pnm` nur als Dev-Dep für Tests |
| `scrap` | 0.5.0 | `quadrupleslap/scrap` | server | MIT-SHM-Capture, BGRX→RGB (1280×720-Ausschnitt) |
| `enigo` | 0.6.1 | `enigo-rs/enigo` | server | Maus/Tastatur-Injektion |
| `serde_yaml` | 0.9.34 | `dtolnay/serde-yaml` | server | `character_dict` aus `inference.yml` |
| `rav1e` | 0.8.1 | `xiph/rav1e` | server | AV1-Still-Picture-Encoder, **ohne** `asm` (kein `nasm` nötig) |
| `rav1d` | 1.1.0 | `memorysafety/rav1d` | client | AV1-Decoder (`bitdepth_8`) |
| `ort` | 2.0.0-rc.13 | `pykeio/ort` | server | ONNX Runtime für PP-OCRv6, Features `+cuda`: lädt ORT-1.28-Binary mit CUDA-13-EP (`dist.tsv`: `x86_64-unknown-linux-gnu+cuda13,tensorrt,nvrtx`); Detektor auf CUDA, Erkenner auf CPU (Hybrid, gemessen) |
| `macroquad` | 0.4.16 | `not-fl3/macroquad` | client | Fenster (1280×720), Textur, Default-Font-Text, Eingabe |
| `x11rb` | 0.13.2 | `psychon/x11rb` | server | RandR-`GetMonitors` für den Maus-Ursprung |

Bekannte Auffälligkeiten (aus `source7_mvp` übernommen, erneut geprüft):

- `bincode` 3.0.0 existiert auf crates.io, ist aber ein kaputter Stub
  (`compile_error!`). Wir bleiben bewusst auf 2.0.1.
- `image` 0.25.10 existiert, wir bleiben bewusst auf 0.24.9 (eine Version im
  Baum via `macroquad`; keine `image`-Typen über die macroquad-Grenze).
- `x11rb` 0.14.0 existiert, bleibt verworfen: `protocol::randr` löst dort
  nicht mehr auf (eine Zeile in `06_input.rs`).
- `ort` ist weiterhin nur als Release-Candidate aktuell (rc.13 ist max).
- `serde_yaml` trägt upstream das Tag `+deprecated` (dtolnay stellt die
  Pflege ein); Ersatzevaluierung steht aus, betrifft nur `load_dict`.
- `block` 0.1.6 (via `scrap`): Future-Incompat-Warnung bei jedem Build,
  harmlos (Pfad auf Linux ungenutzt), nicht per Upgrade behebbar.
- Verbleibende Doppelversionen sind rein transitiv (`cargo tree -d`):
  `bitflags` 1/2, `miniz_oxide` 0.8/0.9, `hashbrown` 0.15/0.17, `cfg-if`
  0.1/1, `syn` 2/3. Keine betrifft ein direktes Dep.

Bewusst **nicht** übernommen (evaluiert, verworfen):

| Crate | GitHub | Grund |
|---|---|---|
| `governor` | `antifuchs/governor` | Kein Rate-Limit nötig; direktes TCP-Schreiben genügt |
| `xcap` | `nashaofu/xcap` | 0.9 braucht Wayland+EGL-Systemlibs ohne Feature-Gate |
| `imageproc` | `image-rs/imageproc` | Maskieren/Padden sind wenige Zeilen mit `image` allein |
| `miniquad` (direkt) | `not-fl3/miniquad` | Nur transitiv via `macroquad`; kein Clipboard |

Modelle (Laufzeit, nicht im Git — Symlink `models/` → `../source7_mvp/models`):

| Datei | Quelle |
|---|---|
| `PP-OCRv6_small_det.onnx`, `PP-OCRv6_small_rec.onnx`, `inference.yml` | HuggingFace `PaddlePaddle/PP-OCRv6_small_{det,rec}_onnx` (GitHub `PaddlePaddle/PaddleOCR`) |

Gestrichen (wie MVP): `gpa_640_int8.onnx` (kein YOLO), Unifont (Client nutzt
den eingebauten `macroquad`-Font).

Systemvoraussetzungen Build/Laufzeit: `libxcb1-dev`, `libxcb-shm0-dev`,
`libxcb-randr0-dev` (nur Link-Symlinks); CUDA 13 + cuDNN 9 auf der GPU-Seite
(im Laufzeit-Image bereits vorhanden — kein apt nötig, `ldd` der
CUDA-Provider-`.so` findet alles). Für Tests/Smoke: `xvfb`, `xterm`.

DeepWiki-Beispiel: `ask_wiki_question(repoName="pykeio/ort", question="...")`.
