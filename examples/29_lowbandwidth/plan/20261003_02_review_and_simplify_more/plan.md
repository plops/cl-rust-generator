# Implementierungsplan: `source7_mvp` weiter vereinfachen (Review + Padding)

Ziel: den MVP aus `20261003_01_simplify` (2.763 Zeilen, ohne Tests/Target)
nach `review.md` und dem Padding-Befund aus `26_onnx` weiter auf das
Mindeste reduzieren — bei gleichzeitig **niedrigerer** Bandbreite durch
eine einzige Bounding-Box pro Frame statt des 64er-Rasters und durch
gepaddete OCR-Boxen.

Hinweis zur Prompt-Ortsangabe: dort steht `source6/android_client` — das
ist ein Copy-Paste-Rest aus dem Android-Plan. Gearbeitet wird in
`source7_mvp/` (Server/Client/Common), wie es dem Ziel („MVP verbessern“)
entspricht.

## 1. Was umgesetzt wird (Requirements)

1. **OCR wird Pflicht** (`review.md` §1): `enum Ocr` auflösen (nur noch
   die Struktur, `Ocr::load` scheitert hart bei fehlenden Modellen),
   `--no-ocr` streichen. Ohne Maskierung sprengt reines AV1-Textbild das
   6-kB/s-Budget — der Fallback ist nutzlos.
2. **Single Bounding Box** (`review.md` §2): `dirty_tiles() -> Vec<Rect>`
   wird `dirty_bbox() -> Option<Rect>` (min/max über alle Änderungen,
   gerade Kanten, mind. 16×16). Pro Frame genau **ein** AV1-Encode und
   **ein** Header-Overhead (~50 B statt N×50 B). Client-`blit` wird
   dynamisch (Größe aus dem dekodierten Bild, nicht mehr fest 64).
   Protokoll unverändert: `Tile{x, y, data}` trägt schon immer nur
   Position + Bytes — variable Größen brauchen kein neues Feld.
3. **Box-Padding** (Befund aus `26_onnx`, `walkthrough.md` §2.1): enge
   Detektions-Boxen kosten dort 25 CER-Punkte; hier zusätzlich AV1-Reste
   an xterm-Glyphenrändern. Zwei getrennte Paddings:
   - `REC_PAD` (Erkennung): Box vor dem CTC-Crop aufweiten → bessere
     CER, kein Bandbreiten-Effekt.
   - `MASK_PAD` (Maskierung): Box vor `fill_rect` aufweiten → löscht
     Glyphen-Fransen, spart AV1-Bytes.
   Gute Werte werden per Test mit Xvfb + xterm gefunden (echte Modelle).
4. **Weitere Kürzungen** (eigene Analyse, je ≤ ~15 Zeilen): `Av1Params`
   → nur `quantizer`, `Scene::{last_rx, Down(Instant)}` → ohne Zeit,
   `FrameSource::size` und `FrameReader::rx_bytes` streichen,
   `ScrapSource::fh` streichen, `Decoder::new()` ohne Thread-Parameter,
   `TILE`-Konstante entfernen. Details in §5 mit Risiko.
5. Grün bleiben: `cargo test --workspace` (ohne X11/Modelle),
   Modell-Tests (mit Modellen), Xvfb-Smoke, `cargo fmt --check`,
   `cargo clippy -- -D warnings`.

## 2. Bewusst nicht umgesetzt / Risiken beim Weglassen

| Feature/Kürzung | Vorschlag | Risiko beim Weglassen |
|---|---|---|
| OCR-Fallback (`Disabled`, `--no-ocr`) | streichen (Review) | Server startet nicht ohne Modelle; Tests ohne Modelle bleiben grün (Stub-OCR), nur E2E braucht Downloads. Akzeptiert: Fallback war Bandbreiten-technisch nutzlos. |
| 64er-Raster | durch BBox ersetzen (Review) | Zwei weit entfernte Änderungen spannen eine große Box (z. B. Cursor oben + Uhr unten). AV1 kodiert den unveränderten Rest als Skip-Blöcke (~0 B) — unterm Strich billiger als N×Header. |
| `Av1Params::{speed, threads}` | fest verdrahten (10, 4) | Nicht mehr tunbar; Werte waren ohnehin nie per CLI erreichbar. |
| `Scene::last_rx`, `Down`-Zeitpunkt | streichen | HUD zeigt keine „offline seit“-Dauer — zeigte es auch bisher nicht (Feld wurde nur geschrieben). |
| `--dump`, `--no-input`, `-v`, `is_public`-Warnung | **behalten** | Je ~5 Zeilen, aber Diagnose/Sicherheit/Tests (`no_input` macht Loopback displaylos). Streichen spart fast nichts. |
| `ClientMsg::Hello{version}` | **behalten** | Eine Zeile je Seite gegen Protokoll-Mix. |
| Heartbeat, Zoom, Clipboard, YOLO, Auth | **nicht** einbauen | Wie im MVP: dokumentierte Nicht-Ziele (vgl. Walkthrough §3). |

Offene Punkte aus dem Prompt, beantwortet:

- „Noch andere Requirements?“ — Ja: BBox-Pixelvergleich muss zeilenweise
  über Slices laufen (nicht `get_pixel` je Pixel wie in der
  Review-Skizze — 400k Aufrufe/Frame wären ~10× langsamer). Und: erster
  Frame = Vollbild, leere Änderung = `None` (Funkstille bei Standbild
  bleibt).
- Neue Abhängigkeiten? — Keine nötig. Alles ist Hand-Code-Reduktion;
  `deps.md` bleibt bis auf Versionspflege identisch.

## 3. Kontext-Dateien für den ausführenden Agenten

| Datei | Warum lesen |
|---|---|
| `plan/20261003_02_review_and_simplify_more/review.md` | Die zwei Review-Forderungen (OCR-Pflicht, BBox) mit Code-Skizzen |
| `plan/20261003_01_simplify/walkthrough.md` | MVP-Architektur, Protokoll, Messwerte, Dockerfile-Pakete |
| `examples/26_onnx/plan/20260930_01_unicode/walkthrough.md` §2.1 | Padding-Befund: +30 % Höhe (min. 4 px) senkt CER 69→4 % |
| `source7_mvp/server/src/03_ocr.rs` | `Ocr`-Enum, `Detector::detect`, `Recognizer::preprocess`, `sample_colors` |
| `source7_mvp/server/src/04_tiles.rs` | `dirty_tiles`, `fill_rect`, `crop_rgb` → wird `dirty_bbox` + `pad_rect` |
| `source7_mvp/server/src/07_session.rs` | Schleife: OCR → Text-Delta → Maske → Encode → `Tile` |
| `source7_mvp/server/src/05_av1.rs` | `Av1Params`, `encode_rgb` |
| `source7_mvp/server/src/01_config.rs`, `main.rs` | `--no-ocr` und `Ocr::Disabled`-Verdrahtung |
| `source7_mvp/client/src/04_scene.rs`, `03_net.rs` | `blit` (fest 64) + `Event::Tile` (ohne Größe) |
| `source7_mvp/common/src/01_types.rs` | `TILE`-Konstante, `ServerMsg::Tile` |
| `source7_mvp/server/tests/loopback.rs`, `client/tests/loopback.rs` | Erwartungen (Kachelzahlen!) müssen auf BBox umgestellt werden |
| `source7_mvp/scripts/smoke_xvfb.sh` | E2E-Nachweis, ggf. um Byte-Zähler erweitern |

## 4. Neue Modulstruktur

Keine neuen Dateien, keine Splits (größte Datei `03_ocr.rs` bleibt unter
600 Zeilen; Detektor + Erkenner + Farben sind zusammengehörig). Nur
Verhalten in bestehenden Dateien:

```text
server/src/
  01_config.rs   --no-ocr streichen (+ Tests anpassen)
  02_capture.rs  fh-Feld + size()-Methode streichen
  03_ocr.rs      enum Ocr → struct Ocr; REC_PAD in preprocess/sample_colors
  04_tiles.rs    dirty_tiles → dirty_bbox; pad_rect + MASK_PAD neu
  05_av1.rs      Av1Params → encode_rgb(rgb, w, h, quantizer)
  07_session.rs  eine BBox pro Frame; maskiert mit MASK_PAD-Boxen
  main.rs        Ocr::load ohne Fallback-Zweig
client/src/
  02_av1.rs      Decoder::new() ohne Parameter
  03_net.rs      Event::Tile bekommt w/h aus Rgba
  04_scene.rs    blit(x, y, w, h, rgba); last_rx + Down-Zeit streichen
common/src/
  01_types.rs    TILE streichen (SIZE bleibt)
server/tests/
  loopback.rs    Vollbild = 1 Tile; Delta = 1 BBox-Tile
  padding.rs     NEU (ignored): Xvfb/xterm-Sweep über MASK_PAD/REC_PAD
```

## 5. Protokoll

Unverändert (Version bleibt 1): `Tile{x, y, data}` beschreibt schon heute
Position + beliebig viele Bytes — ob `data` 64×64 oder eine BBox zeigt,
entscheidet allein der AV1-Bitstrom, den der Client ohnehin dekodiert.
Server alt → Client neu (und umgekehrt) bleiben kompatibel, solange beide
Seiten 640×640 sprechen.

## 6. Commit-Konvention (Conventional Commits, verbindlich)

- Format: `<typ>(<scope>): <kurz>` + Leerzeile + Body (Was/Warum, Tests,
  Bezug zu Tasks). Typen: `feat`, `fix`, `test`, `refactor`, `docs`,
  `chore`. Scopes: `common`, `server`, `client`, `tests`, `scripts`,
  `plan`.
- Ein Commit pro `task.md`-Schritt (mindestens), nur bei grünen Tests des
  betroffenen Pakets.

## 7. Validierung

- `cargo fmt --all -- --check`, `cargo clippy --workspace -- -D warnings`
- `cargo test --workspace` (ohne X11/Modelle grün)
- `cargo test --release -p lbw-server --test models -- --ignored`
  (echte OCR-Modelle) + `--test padding -- --ignored` (Xvfb/xterm-Sweep)
- `scripts/smoke_xvfb.sh` (E2E mit Xvfb)
- `cargo upgrade --dry-run` zur Abnahme (neueste Deps dokumentiert)
