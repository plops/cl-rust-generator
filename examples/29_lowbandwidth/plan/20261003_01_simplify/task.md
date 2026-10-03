# task.md — Serielle MVP-Tasks (`source7_mvp`)

Jeder Task: implementieren → Tests des Pakets grün → `cargo fmt` →
Commit (Conventional Commits, siehe `plan.md` §7). Kein Task beginnt bei
rotem Baum. Modi-Reihenfolge: erst `common`, dann `server`, dann `client`,
danach Smoke/Doku. (TUI entfällt im MVP ersatzlos — kein Fluch-Auswahlmodus.)

- [ ] **T1 Gerüst + `common`**: Workspace `source7_mvp` (Edition 2024,
  Mitglieder `common`/`server`/`client`), `common`: `01_types.rs`
  (serde-Protokoll), `02_framing.rs` (u32-Rahmen + bincode 2), `03_yuv.rs`
  (aus `source6` übernommen). Unit-Tests: Roundtrip aller Varianten,
  Framing mit Teil-Reads/Timeouts, YUV-Roundtrip. Validierung:
  `cargo test -p lbw-common`, `cargo clippy -p lbw-common`.
- [ ] **T2 Server-Basis**: `01_config.rs` (clap), `02_capture.rs`
  (`FrameSource`, `XcapSource`, `SharedSource`), `04_tiles.rs`
  (Fest-Raster-Dirty + Maskierung), `05_av1.rs` (rav1e ohne `asm`).
  Unit-Tests: Config-Defaults, SharedSource, dirty-Raster, flache Kachel
  klein, Quantizer-Monotonie. Validierung: `cargo test -p lbw-server`.
- [ ] **T3 Server-OCR**: `03_ocr.rs` (serde_yaml-Dict, Detector,
  Recognizer, optionales `Ocr`: fehlt das Modellverzeichnis, läuft der
  Server ohne Text weiter). Unit-Tests: Dict-YAML, CTC-Dekodierung
  (Blank/Duplikat/Leerzeichen/Konfidenz). Modell-Test
  (`tests/models.rs`, `#[ignored]`): Detektion auf
  `../../source6/models/test_screen.ppm` findet Text. Validierung:
  `cargo test -p lbw-server` + `cargo test --release -p lbw-server
  --test models -- --ignored` (falls Modelle da).
- [ ] **T4 Server-Session**: `06_input.rs` (enigo-Injector mit
  Capture-Offset), `07_session.rs` (`handle_client`: Input-Thread,
  Hello, Text-nur-bei-Änderung, maskierte Dirty-Kacheln, direktes TCP),
  `main.rs`-Verdrahtung. Loopback-Test mit `SharedSource` + Stub-OCR
  über echtes TCP: Hello, ClearText/AddText, Tile für geänderte Kachel,
  keine Tile für unveränderte. Validierung: `cargo test -p lbw-server`.
- [ ] **T5 Client-Kern**: `01_config.rs` (clap, ohne Zoom),
  `02_av1.rs` (rav1d-Decoder), `03_net.rs` (Reconnect mit Backoff,
  Event-Kanal, `send`), `04_scene.rs` (fest 640×640, Texte ohne IDs,
  Blit ohne Clipping). Tests: Config, Müll-Dekodierung ist Fehler (kein
  Crash), Scene-Clear/Add/Blit/Link-Zustand, Loopback gegen Stub-Server
  (Hello/Text/echte AV1-Kachel, Reconnect nach Abriss). Validierung:
  `cargo test -p lbw-client` (headless, ohne Display).
- [ ] **T6 Client-App**: `05_app.rs` + `main.rs` (macroquad: festes
  640×640-Fenster, Textur + Default-Font-Text + HUD, Maus/Tasten/
  `Text`-Eingabe ohne Skalierung). Validierung: Build + Clippy
  (Laufzeit-Test im Xvfb-Smoke T7).
- [ ] **T7 Integration + Smoke**: `scripts/smoke_xvfb.sh`
  (Xvfb-Server-Display, `xterm`-Inhalt, `lbw-server` mit echten Modellen,
  Headless-Probe liest Hello/Text/Tile und schickt Input; Abbruch sauber).
  Validierung: `cargo test --workspace` komplett grün,
  `scripts/smoke_xvfb.sh` erfolgreich, `cargo clippy --workspace --
  -D warnings`, `cargo fmt --all -- --check`.
- [ ] **T8 Abnahme + Doku**: `cargo upgrade` auf neueste Deps (ggf.
  anpassen, Tests erneut grün), `README.md` + `deps.md` (Kopie der
  Plan-Deps) in `source7_mvp/`, `walkthrough.md` (Deutsch, Mermaid,
  Code-Beispiele, Learnings, Dockerfile-Pakete) in diesem Plan-Ordner.
  Validierung: Frisch-Checkout-Build `cargo build --release`,
  alle Commits vorhanden (`git log --oneline`).
