# task.md — 20260920_01_capture: seriell abarbeitbare Schritte

Jeder Schritt endet mit Gates. Erst bei grünen Gates committen (s. `plan.md` Kap. 10)
und zum nächsten Schritt. Code: `examples/26_onnx/source0/` (in S0 neu anlegen).
Dateiregeln aus dem Prompt gelten ab der ersten angefassten Datei
(`NN_name.rs`, ≤~300 Zeilen, `main.rs`/`lib.rs` nur Verdrahtung, vorher/nachher grün).
Stufen = Capture/Inferenz/View (keine Modi, kein TUI — s. `plan.md` Kap. 2).

## S0 — Scaffold + CLI + xcap-Spike (Basis)

- `cargo new source0 --bin`, Deps neueste: `ort`, `xcap`, `pixels`, `winit`,
  `clap` (derive), `image`, `ndarray`, `anyhow`. `deps.md`-Einträge nach `plan.md` Kap. 5.
- Module anlegen (leer, aber verdrahtet): `01_cli.rs`, `02_capture.rs`,
  `03_infer.rs`, `04_draw.rs`, `05_view.rs`; `main.rs`/`lib.rs` nur Deklaration + Dispatch.
- CLI (`01_cli.rs`): `--x/--y/--w/--h` (Default 0/0/800/600), `--monitor` (Default 0),
  `--zoom` (Default 1), `--win-w/--win-h` (optional), `--fps` (Default 5, clamp 1–30),
  `--model` (Default YOLOv8m-CDN-URL), `--conf` (0,5), `--nms` (0,7),
  `--headless`, `--save-frame`. Ungültige Region (w/h ≤ 0) → Exit ≠ 0 + Meldung.
- xcap-Spike: per `cargo doc -p xcap` Monitor-/Regions-API pinnen (docs.rs baut
  0.9.8 nicht — raten verboten); erster echter Regions-Screenshot unter Xvfb.
- Unit-Tests: FPS-Clamp, Regions-Validierung (w/h ≤ 0 abgelehnt), CLI-Defaults.
- Gates: `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`,
  `cargo test` grün, `cargo run -- --help` grün.
- Commit: `chore(scaffold): numbered modules cli capture infer draw view`.

## S1 — Capture (Region, Clamp, Farbraum)

- `02_capture.rs`: Monitor wählen (`--monitor`), Region auf Monitor-Geometrie
  clampen (Überlauf schneidet ab, leere Schnittmenge → Fehler statt Panic),
  Screenshot → RGB-Bytes (Konvertierung BGRA→RGB als eigene getestete Funktion).
- Tests (ohne X11/Modell): Clamp-Matrix (innen/außen/teilüberlappend/komplett draußen),
  BGRA→RGB an synthetischem 2×2-Bild (byte-exakt).
- Gates analog S0 + neue Tests ≥5. Kein Display nötig.
- Commit: `feat(capture): clamped x11 region screenshot`.

## S2 — Inferenz (Preprocess, Decode, NMS; ohne Modell-Download)

- `03_infer.rs` nach Referenzbeispiel: Letterbox-Resize auf 640×640 (Seitenverhältnis
  erhalten + Padding), RGB/255 → `(1,3,640,640)`-f32; `output0`-Decode
  (xc,yc,w,h + 80 Scores), Conf-Filter, NMS (IoU), Rücktransformation
  (Padding abziehen, auf Region-Koordinaten skalieren), 80 COCO-Labels.
- Session-Aufbau an installierte ort-Version anpassen (RC-API-Drift einplanen).
- Tests (ohne Modell/X11): Letterbox-Rückprojektion an synthetischer Geometrie
  (exakte Pixel-Asserts), Decode+NMS an synthetischem `output0`-Tensor
  (doppelte Box → eine überlebt; schwache Klasse gefiltert).
- Gates analog + `tests/geometry.rs` grün (always-green-Basis ohne Netz/GPU).
- Commit: `feat(infer): yolov8 decode with nms`.

## S3 — Overlay + View (Draw-Rasterizer, pixels-Fenster, Zoom)

- `04_draw.rs`: eigener CPU-Rasterizer — Rechteck-Rahmen + Label-Zeile ins
  RGBA-Frame (kein `raqote`/`show_image`); reine Funktion (Boxen + Frame → Frame),
  Clippen am Bildrand.
- `05_view.rs`: `pixels`+`winit`-Fenster nach Minimal-Beispiel; Capture-Bild
  × `--zoom` (nearest, ganzzahlig) bzw. `--win-w/--win-h`; Resize via
  `pixels.resize_surface`; `--headless` (kein Fenster) und `--save-frame`
  (annotiertes PNG schreiben) als Datei-Pfad (wird in S4 unter Xvfb geprüft).
- Tests (ohne X11/GPU/Modell): Box-Rasterizer an synthetischem Frame
  (Eckpixel gesetzt, außerhalb unverändert, Clipping ohne Panic); Zoom-Math.
- Gates analog.
- Commit: `feat(view): overlay boxes in pixels window`.

## S4 — Pipeline + FPS-Pacer + Xvfb-E2E (Live-Nachweis)

- Verdrahtung in `main.rs`: pro `--fps`-Tick Capture → Inferenz → Draw → View;
  Timing pro Frame (Capture-/Inferenz-ms) auf stderr; sauberer Exit bei
  X11-/Modell-Fehler (Meldung + Exit ≠ 0).
- E2E unter Xvfb (Systempakete s. `deps.md`): Testbild anzeigen
  (z. B. COCO-ähnliches Foto), Viewer mit `--save-frame /tmp/e2e.png`
  laufen lassen, PNG-Nachweis (Datei valide, Maße = Region × Zoom).
- `tests/cli_smoke.rs`: `--help` Exit 0, ungültige Region Exit ≠ 0,
  `--save-frame`-Smoke unter Xvfb (Modell-Download nur hier, nicht in Unit-Tests).
- Gates: `fmt --check`, `clippy --all-targets -D warnings`,
  `cargo test --release` grün, E2E-PNG liegt vor.
- Commit: `feat(pipeline): fps-paced live viewer with xvfb proof`.

## T1 — Härtung (Fehlerpfade, Modell-Cache)

- Modell als Datei cachen (kein Re-Download pro Start); fehlendes/inkompatibles
  Modell (falscher Output-Layer) → klare Meldung statt falscher Boxen.
- Dauerlauf-Smoke (N Frames headless ohne Peak-/Speicher-Drift im Log),
  falscher `--monitor` → Exit ≠ 0 + Meldung, `--fps 999` clampet auf 30.
- Gates: alle Gates S0–S4 erneut grün + Dauerlauf-Log für Walkthrough sichern.
- Commit: `fix(pipeline): model cache and error hardening`.

## T2 — Upgrade + Deps + Abschluss

- `cargo upgrade` (danach voller Testlauf), `deps.md` final,
  `fmt`/`clippy`/`test --release` final grün, `git log` sammeln.
- `plan/20260920_01_capture/walkthrough.md` schreiben (implementiert vs. Plan,
  Abweichungen — u. a. xcap-API-Befund aus S0, ort-RC-Befund —, Xvfb-/E2E-Logs,
  Learnings, Erweiterungen wie Tracking/Video, Docker-Pakete).
  (Hinweis: der Prompt nennt fälschlich `plan/20260912_02_full/walkthrough.md`
  — gemeint ist unser Ordner `plan/20260920_01_capture/`.)
- Commit: `docs(plan): walkthrough for x11 yolo viewer`.
