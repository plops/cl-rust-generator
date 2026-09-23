# task.md — 20260923_01_keys_source5: seriell abarbeitbare Schritte

ROI-Pan (Pfeiltasten) + Zoom (`1`/`2`) für `examples/26_onnx/source5/`.
Jeder Schritt endet mit Gates. Erst bei grünen Gates committen (s.
`plan.md`, Commit-Konvention) und zum nächsten Schritt. Dateiregeln aus dem
Prompt gelten ab der ersten angefassten Datei (`NN_name.rs`, ≤~300 Zeilen,
`main.rs`/`lib.rs` nur Verdrahtung, vorher/nachher grün).

## S0 — Beschaffung + Build grün (Basis) ✅

- Modelle von HuggingFace (offiziell, Apache-2.0):
  `PaddlePaddle/PP-OCRv6_small_det_onnx` → `source5/PP-OCRv6_small_det.onnx`
  (9,9 MB), `PaddlePaddle/PP-OCRv6_small_rec_onnx` →
  `source5/PP-OCRv6_small_rec.onnx` (21 MB) + `source5/inference.yml`
  (enthält `character_dict:`, passt zu `load_dict`).
- System: `apt-get install xvfb fonts-unifont` (Font liegt unter
  `/usr/share/fonts/opentype/unifont/unifont.otf` — NICHT unter dem im Code
  hartcodierten `/usr/share/fonts/unifont/…`, wird in S4 per Suchliste
  robust gemacht).
- Gates: Dateien vorhanden, `cargo build` grün.
- Commit: `chore(source5): procure pp-ocrv6 models and dict`.

## S1 — `01_view.rs` (ROI-State, rein, ohne X11)

- `View { x, y, size }`, Stufen `[320, 480, 640, 960, 1280]`, Start
  `640×640 @ (0,0)`; `pan(dx, dy, screen)`, `zoom_in/out(screen)`,
  Schritt `max(8, size/16)`, Clamp auf Screen; ROI→Anzeige-Mapping
  (1:1 = Identität).
- Tests (ohne X11/Modell): Clamp-Matrix, Stufen-Up/Down inkl. Grenzen,
  1:1-Mapping-Identität. ≥6 Tests.
- Gates: `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`,
  `cargo test` grün.
- Commit: `feat(source5): roi view state with pan zoom clamp`.

## S2 — `02_capture.rs` + Fast-Path-Trennung

- x11rb-Setup-Größe lesen, `get_image` mit ROI-Offsets, `resize_nearest`
  NUR für `size != 640`; bei 640 heutige `prepare_inputs`-Schleife direkt
  (kein Resize-Code im Pfad); ROI-Wechsel invalidiert
  `prev_screen_bytes`/`cached_boxes`/`prev_printed_lines`.
- Tests (ohne X11/Modell): 1:1-Byte-Test (Fast-Path == generischer Pfad bei
  640), `resize_nearest` an synthetischem 2×2→4×4 (exakte Pixel),
  Invalidierungs-Flag.
- Gates analog S1.
- Commit: `feat(source5): variable roi capture with 1x1 fast path`.

## S3 — `03_detect.rs` / `04_recognize.rs` (Extraktion ohne Verhalten)

- Unveränderten Code aus `main.rs` verschieben (Session-Aufbau, DBNet,
  Crop, CTC, Dict); Konstanten/Schwellen identisch.
- Tests: synthetischer Tensor-Decode (schwache Klasse gefiltert, Duplikat
  kollabiert), Dict-Parser an Mini-YAML.
- Gates analog + vorher/nachher grün.
- Commit: `refactor(source5): split detect and recognize modules`.

## S4 — `05_overlay.rs` + Tasten + HUD + Font-Fallback

- Box-/Label-Render mit ROI-Skalierung, `set_filter(Nearest)`,
  `draw_texture` bei 1:1 sonst `draw_texture_ex`; Tasten
  (Pfeile/`1`/`2`/`Escape`), HUD (ROI, Stufe, Hilfe); Font-Suchliste
  (opentype-Pfad zuerst, klare Fehlermeldung).
- Tests: Mapping-Identität bei 1:1, Skalierung bei 320/1280 ohne Panic.
- Xvfb-Smoke: Fenster öffnet, synthetische Tasten-Events ändern ROI.
- Gates analog.
- Commit: `feat(source5): pan zoom keys with nearest display`.

## T1 — Härtung + E2E

- Dauerlauf-Smoke (Pan/Zoom-Spam, kein Drift), Fehlerpfade (X11 weg →
  Meldung + Exit ≠ 0), OCR-Plausibilität auf bekanntem Testbild unter Xvfb.
- Gates: alle Gates S0–S4 erneut grün + E2E-Log für Walkthrough sichern.
- Commit: `fix(source5): harden roi loop and error paths`.

## T2 — Upgrade-Check + Deps + Abschluss

- Keine neue Dep eingeführt → `cargo upgrade`-Check entfällt (nur Verweis),
  `deps.md` final, `fmt`/`clippy`/`test` final grün.
- `plan/20260923_01_keys_source5/walkthrough.md` schreiben.
- Commit: `docs(plan): walkthrough for source5 pan zoom`.
