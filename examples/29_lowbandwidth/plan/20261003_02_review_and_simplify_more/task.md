# task.md — Serielle Tasks (Review + Padding + Simplify)

Jeder Task: implementieren → Tests des Pakets grün → `cargo fmt` →
Commit (Conventional Commits, siehe `plan.md` §6). Kein Task beginnt bei
rotem Baum. Basis: `source7_mvp` aus `20261003_01_simplify` (40 Tests
grün + 1 ignoriert).

- [x] **T1 OCR-Pflicht**: `enum Ocr` → `struct Ocr` (ehem. `OcrInner`,
  ohne `Box`), `Ocr::load` gibt hartes `Err` bei fehlenden Modellen,
  `--no-ocr` aus `01_config.rs` + `main.rs`-Fallback streichen, Tests
  (`disabled_ocr_yields_no_text`, `missing_models_disable…`) durch
  „fehlende Modelle sind Fehler“ ersetzen. Validierung:
  `cargo test -p lbw-server` (ohne Modelle grün).
- [x] **T2 Single Bounding Box**: `04_tiles.rs`: `dirty_tiles` →
  `dirty_bbox` (zeilenweiser Slice-Vergleich, gerade Kanten, mind.
  16×16, `None` bei Standbild, Vollbild beim ersten Frame);
  `07_session.rs`: ein Encode pro Frame; `common`: `TILE` streichen;
  Client: `Event::Tile` + `blit` mit `w/h` aus `Rgba`; Loopback-Tests
  beider Seiten auf 1-Tile-Erwartungen umstellen; Unit-Test „10 verteilte
  Pixel → 1 Box“ + „Standbild → None“. Validierung:
  `cargo test --workspace`.
- [x] **T3 Box-Padding**: `pad_rect(r, pad, w, h)` in `04_tiles.rs`
  (reine Funktion + Unit-Tests); `REC_PAD` vor `Recognizer::preprocess`
  und `sample_colors`, `MASK_PAD` vor `fill_rect` in der Session;
  Default-Startwerte aus `26_onnx` (30 % der Höhe, min. 4 px).
  Validierung: `cargo test -p lbw-server`.
- [x] **T4 Padding-Sweep (Xvfb/xterm)**: `server/tests/padding.rs`
  (`#[ignore]`): Xvfb-Display mit xterm-Inhalt capturen (echte Modelle),
  Sweep über `MASK_PAD`/`REC_PAD` (0/2/4/8 px), messen: erkannte Zeichen,
  AV1-Bytes der BBox nach Maskierung; Bestwerte in `04_tiles.rs` als
  Konstanten übernehmen + im Walkthrough begründen. Validierung:
  `cargo test --release -p lbw-server --test padding -- --ignored` grün,
  danach `cargo test --workspace` grün.
- [x] **T5 Weitere Kürzungen**: `Av1Params` → `encode_rgb(rgb, w, h,
  quantizer)` (speed 10, threads 4 fest); `Scene::last_rx` +
  `Down`-Zeitpunkt streichen; `FrameSource::size`, `FrameReader::rx_bytes`,
  `ScrapSource::fh` streichen; `Decoder::new()` ohne Parameter; betroffene
  Tests anpassen. Validierung: `cargo test --workspace`,
  `cargo clippy --workspace -- -D warnings`.
- [x] **T6 Abnahme + Doku**: `cargo upgrade` (neueste Deps, Tests erneut
  grün), `deps.md` (Plan + `source7_mvp/` synchron), `scripts/smoke_xvfb.sh`
  erfolgreich, `walkthrough.md` (Deutsch, Mermaid, Code-Beispiele,
  Architektur-Änderungen, Learnings, Dockerfile-Pakete) in diesem
  Plan-Ordner. Validierung: `cargo build --release`,
  `cargo fmt --all -- --check`, `git log --oneline` zeigt T1–T6-Commits.
