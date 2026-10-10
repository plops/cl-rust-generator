# deps.md — Abhängigkeiten von `source10_instrumentation` (GitHub `org/projekt` für DeepWiki)

Stand: 2026-10-10. **Keine neuen externen Abhängigkeiten** gegenüber
`source9_gpu` (dort Stand 2026-10-09, erneut geprüft: `serde` 1.0.229 ist max,
`bincode` 3.0.0 bleibt kaputter Stub → 2.0.1). Neu sind nur interne
Pfad-Crates (`lbw-log`) und deren Nutzung in Server/Client.

## Externe Crates (identisch zu `../source9_gpu/deps.md`)

| Crate | Version | GitHub | Wo | Wofür |
|---|---|---|---|---|
| `serde` | 1.0.229 | `serde-rs/serde` | common, server, log | Derive für Protokoll- + Log-Typen |
| `bincode` | 2.0.1 | `bincode-org/bincode` | common, log | `serde::{encode_to_vec, decode_from_slice}` mit `config::standard()` (Protokoll + `.lbwlog`-Records) |
| `clap` | 4.6.7 | `clap-rs/clap` | server, client, logstat | `#[derive(Parser)]` CLI (`--record`, `--json`, `--realtime`) |
| `image` | 0.24.9 | `image-rs/image` | server | `RgbImage`, Crop, Kanten-Padding (`default-features = false`; `png`/`pnm` nur als Dev-Dep) |
| `scrap` | 0.5.0 | `quadrupleslap/scrap` | server | MIT-SHM-Capture, BGRX→RGB (1280×720-Ausschnitt) |
| `enigo` | 0.6.1 | `enigo-rs/enigo` | server | Maus/Tastatur-Injektion |
| `serde_yaml` | 0.9.34 | `dtolnay/serde-yaml` | server | `character_dict` aus `inference.yml` |
| `rav1e` | 0.8.1 | `xiph/rav1e` | server | AV1-Still-Picture-Encoder, **ohne** `asm` |
| `rav1d` | 1.1.0 | `memorysafety/rav1d` | client | AV1-Decoder (`bitdepth_8`), auch im Replay genutzt |
| `ort` | 2.0.0-rc.13 | `pykeio/ort` | server | ONNX Runtime, Features `+cuda` (Detektor CUDA, Erkenner CPU) |
| `macroquad` | 0.4.16 | `not-fl3/macroquad` | client | Fenster (1280×720), Textur, Text, Eingabe |
| `x11rb` | 0.13.2 | `psychon/x11rb` | server | RandR-`GetMonitors` für den Maus-Ursprung |

Bekannte Auffälligkeiten: siehe `../source9_gpu/deps.md` (bincode-Stub,
image-0.24-Pin, x11rb-0.13-Pin, ort-rc, serde_yaml-deprecated, `block`-Warnung,
transitive Doppelversionen) — alle erneut geprüft, keine Änderung.

Bewusst **nicht** eingeführt (evaluiert, verworfen):

| Crate | GitHub | Grund |
|---|---|---|
| `serde_json` o. ä. | `serde-rs/json` | JSON-Export ist ~20 Zeilen Hand-Code (flache Summary) — kein Dep wert |
| `flate2`/`zstd` | `rust-lang/flate2`, `facebook/zstd` | Log-Kompression unnötig (Kanal ≤ 6 kB/s → Log ≈ 25 MB/h); `gzip` extern genügt |
| `clap` in `common`/`log`-Lib | `clap-rs/clap` | Nur die `logstat`-Bin nutzt `clap`; Lib bleibt CLI-frei |
| `chrono`/`time` | `chronotope/chrono`, `time-rs/time` | `SystemTime`/`Instant` aus `std` genügen (µs-Stempel) |
| `sha2`/`xxhash` | diverse | Dedup-Hash ist FNV-1a (5 Zeilen, stabil — `std`-Hash wäre pro Lauf anders) |

Interne Crates (Pfad-Deps, kein GitHub):

| Crate | Pfad | Nutzer |
|---|---|---|
| `lbw-common` | `common/` | server, client, log, Tests |
| `lbw-log` | `log/` | server (`--record`), client (`--record`), `lbw-logstat`, `lbw-replay` |

Modelle (Laufzeit, nicht im Git — Symlink `models/` → `../source7_mvp/models`):

| Datei | Quelle |
|---|---|
| `PP-OCRv6_small_det.onnx`, `PP-OCRv6_small_rec.onnx`, `inference.yml` | HuggingFace `PaddlePaddle/PP-OCRv6_small_{det,rec}_onnx` (GitHub `PaddlePaddle/PaddleOCR`) |

Systemvoraussetzungen Build/Laufzeit/Test: wie source9 (`libxcb1-dev`,
`libxcb-shm0-dev`, `libxcb-randr0-dev`; CUDA 13 + cuDNN 9; `xvfb`, `xterm`,
`x11-utils` für Smoke/Debug; `libxkbcommon0` zur Laufzeit für macroquad-Fenster
— belegt durch `scripts/render_check.sh`).

DeepWiki-Beispiele: `ask_wiki_question(repoName="serde-rs/serde",
question="...")`, `ask_wiki_question(repoName="bincode-org/bincode",
question="...")`. Transpiler (nicht genutzt, s. Plan §7):
`ask_wiki_question(repoName="plops/cl-rust-generator", question="...")`.
