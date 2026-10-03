# Implementierungsplan: `source7_mvp` — Minimal Viable Product aus `source6`

Ziel: lauffähiger Remote-Desktop für extrem schmale Bandbreite (Text als
Vektordaten + Rest als AV1-Kacheln) mit ca. 800 statt ca. 6000 Zeilen,
direkt in Rust (Edition 2024) geschrieben, im neuen Workspace
`source7_mvp/` (Mitglieder `common`, `server`, `client`).

## 1. Was der MVP können muss (Requirements)

1. Server captured einen `size×size`-Ausschnitt (Default 640) des X11-Desktops.
2. Text wird per PP-OCRv6 erkannt und als (Box, Farben, String) übertragen;
   erkannte Stellen werden im Bild mit ihrer Hintergrundfarbe maskiert.
3. Der Rest wird in einem festen 64er-Raster auf Änderung geprüft; nur
   geänderte Kacheln gehen als AV1-Still-Picture über TCP.
4. Der Desktop-Client zeigt Bild + Text an und schickt Maus/Tastatur zurück.
5. Ohne Modelle läuft der Server trotzdem (nur Kacheln, kein Text) — wichtig
   für Tests und Smoke ohne 30-MB-Downloads.
6. `cargo test --workspace` ist ohne X11 und ohne Modelle grün; ein
   Xvfb-Smoke beweist Ende-zu-Ende-Betrieb mit echten Modellen.

## 2. Bewusste Kürzungen gegenüber `source6`

Android-Client, YOLO-GUI-Detektor, Scheduler/Drosselung, Text-Diff mit
stabilen IDs, Connected-Components-Dirty, Tile-Chunking
(`TileStart`/`TileData`), Resume/Ack/Ping/Pong/Stats, Clipboard/Auswahl
(F2/F3), Unifont, `throttle`-Crate. Details siehe `reduction-proposal.md`.

Offene Punkte, die der Prompt nicht nennt, die ich empfehle:

- Text nur bei Änderung senden (Hash-Vergleich), nicht jedes Frame —
  sonst Dauerlast bei statischem Bild. (Umgesetzt.)
- AV1 ohne `asm`-Feature bauen: kein `nasm` nötig, reine Rust-Toolchain.
  (Umgesetzt.)
- Client mit eingebautem `macroquad`-Font statt Unifont-Datei. (Umgesetzt.)
- Kein Auth — Server bindet per Default nur an `127.0.0.1` (Hinweis im
  `--help` und Warnung bei öffentlichem Listen, wie in `source6`).

## 3. Kontext-Dateien für den ausführenden Agenten

| Datei | Warum lesen |
|---|---|
| `plan/20261003_01_simplify/reduction-proposal.md` | Architektur-Vorschlag, Server-/Client-Schleife, Code-Skizzen |
| `plan/20261003_01_simplify/deps.md` | Neue Abhängigkeiten + GitHub-Pfade für DeepWiki |
| `plan/misc/bincode.md`, `clap.md`, `enigo.md`, `image.md`, `xcap.md`, `scrap.md`, `serde.md`, `serde-yaml.md` | Kurz-Dokus der Kandidaten-Crates |
| `source6/common/src/01_types.rs` | Protokoll-Semantik (Rect, TextItem, Hello) |
| `source6/common/src/05_yuv.rs` | YUV-Formeln (1:1 übernehmen, beide Seiten identisch) |
| `source6/server/src/09_av1.rs` | rav1e-Parameter (still picture, Quantizer) |
| `source6/server/src/04_ocr_detect.rs` | DBNet-Nachverarbeitung (Schwellen, Unclip) |
| `source6/server/src/05_ocr_recognize.rs` | CTC-Dekodierung, Preprocessing-Geometrie |
| `source6/client/src/02_av1.rs` | rav1d-`unsafe`-Kapselung (eine Stelle!) |
| `source6/client/src/04_scene.rs` | Canvas+Text-Modell, Blit mit Clipping |
| `source6/server/src/02_capture.rs` | `FrameSource`-Trait + `SharedSource`-Testmuster |

## 4. Neue Modulstruktur (Datei-Aufteilung verbindlich)

Zweistellige Nummern in Datenfluss-Reihenfolge; `lib.rs`/`main.rs` nur
Deklaration + Verdrahtung; keine Datei über ca. 600 Zeilen.

```text
source7_mvp/
  Cargo.toml                  # Workspace, Edition 2024
  common/src/
    lib.rs                    # nur mod-Deklarationen
    01_types.rs               # Rect, TextItem, ServerMsg, ClientMsg (serde)
    02_framing.rs             # u32-Längenrahmen + bincode-Codec, FrameReader
    03_yuv.rs                 # BT.601 Full-Range RGB↔YUV420 (aus source6)
  server/src/
    lib.rs, main.rs           # nur Verdrahtung
    01_config.rs              # clap-Config (listen, x/y/size, quantizer, models, no-ocr, no-input)
    02_capture.rs             # FrameSource-Trait, XcapSource, SharedSource
    03_ocr.rs                 # serde_yaml-Dict, Detector, Recognizer, Ocr (optional)
    04_tiles.rs               # Fest-Raster-Dirty (64px) + Maskierung
    05_av1.rs                 # rav1e-Still-Picture (Av1Params, encode_rgb)
    06_input.rs               # enigo-Injector
    07_session.rs             # handle_client: Input-Thread + Capture/OCR/Tile-Schleife
  client/src/
    lib.rs, main.rs
    01_config.rs              # clap-Config (connect, zoom)
    02_av1.rs                 # rav1d-Decoder → RGBA
    03_net.rs                 # Reconnect-Thread, Events, send()
    04_scene.rs               # Canvas + Texte, apply(), blit()
    05_app.rs                 # macroquad-Schleife (Rendern + Eingabe)
  scripts/smoke_xvfb.sh       # E2E: Xvfb + Server + Headless-Probe
```

## 5. Protokoll (neu, nicht kompatibel zu `source6`)

```rust
// Server → Client
enum ServerMsg {
    Hello { w: u16, h: u16 },
    ClearText,
    AddText(TextItem),              // TextItem { rect, fg, bg, text } — keine ID
    Tile { x: u16, y: u16, w: u16, h: u16, data: Vec<u8> }, // ganze Kachel
}
// Client → Server
enum ClientMsg {
    Hello { version: u16 },
    MouseMove { x: u16, y: u16 },   // im Capture-Raum
    Button { button: u8, down: bool },
    Text(String),                   // getippter Text / Paste
    Key { key: String, down: bool },// Sonder-Tasten als Name ("Enter","Esc",...)
}
```

Leitung: `[u32 LE Länge][bincode-2 `config::standard()`-Body]`, max. 8 MiB.
Client sendet `Hello` zuerst; Server antwortet mit `Hello` + Vollbild, danach
nur Deltas. Reconnect = frischer Zustand (Server hält keinen Client-State).

## 6. Usage-Beispiele der neuen Crates (per DeepWiki verifiziert)

bincode 2.x (Framing selbst mit Längenpräfix, bincode kennt innen Varint):

```rust
let cfg = bincode::config::standard();
let body = bincode::encode_to_vec(&msg, cfg)?;
stream.write_all(&(body.len() as u32).to_le_bytes())?;
stream.write_all(&body)?;
let (msg, _): (ServerMsg, usize) = bincode::decode_from_slice(&body, cfg)?;
```

clap 4 (beide Binaries):

```rust
#[derive(clap::Parser, Debug)]
#[command(name = "lbw-server", about = "Minimal Low-Bandwidth Remote Desktop Server")]
struct Config {
    #[arg(long, default_value = "127.0.0.1:7878")]
    listen: String,
    #[arg(long, default_value_t = 640)]
    size: usize,
}
```

xcap (liefert direkt `image::RgbaImage`, Region mit Bounds-Check):

```rust
let mon = xcap::Monitor::all()?.into_iter().find(|m| m.is_primary().unwrap_or(false)).unwrap();
let img: image::RgbaImage = mon.capture_region(x as i32, y as i32, w, h)?;
```

enigo (X11-Display via `Settings::x11_display` setzbar):

```rust
let mut e = enigo::Enigo::new(&enigo::Settings::default())?;
e.move_mouse(x, y, enigo::Coordinate::Abs)?;
e.button(enigo::Button::Left, enigo::Direction::Click)?;
e.text("hello")?;
```

serde_yaml (Dict-Struktur aus `inference.yml`):

```rust
#[derive(serde::Deserialize)]
struct Dict { character_dict: Vec<String> }
let d: Dict = serde_yaml::from_str(&std::fs::read_to_string(path)?)?;
```

## 7. Commit-Konvention (Conventional Commits, verbindlich)

- Format: `<typ>(<scope>): <kurz>` + Leerzeile + ausführlicher Body
  (Was/Warum, Tests, Bezug zu Tasks). Typen: `feat`, `fix`, `test`,
  `refactor`, `docs`, `chore`.
- Scopes: `common`, `server`, `client`, `tests`, `scripts`, `plan`.
- Ein Commit pro `task.md`-Schritt (mindestens), nur bei grünen Tests des
  betroffenen Pakets. Beispiel:

```text
feat(server): capture via xcap with fixed-grid dirty tiles

Ersetzt x11rb-GetImage durch xcap::Monitor::capture_region und die
Connected-Components-Analyse durch ein 64px-Fest-Raster (04_tiles.rs).
Tests: SharedSource-Loopback, dirty_tiles-Units. Vgl. task.md T4.
```

## 8. Validierung

- `cargo fmt --all -- --check`, `cargo clippy --workspace -- -D warnings`
- `cargo test --workspace` (ohne X11/Modelle grün)
- `cargo test --release -p lbw-server --test models -- --ignored`
  (echte OCR-Modelle, falls vorhanden)
- `scripts/smoke_xvfb.sh` (E2E mit Xvfb; kurz via `BLACKOUT=0`)
- `cargo upgrade --dry-run` zur Abnahme (neueste Deps dokumentiert)
