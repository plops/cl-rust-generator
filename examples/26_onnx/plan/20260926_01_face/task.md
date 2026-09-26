# task.md — 20260926_01_face: seriell abarbeitbare Schritte

Live-Face-Re-ID in `examples/26_onnx/source7/` (Plan:
`plan/20260926_01_face/implementation_plan.md`). Jeder Schritt endet mit
Gates (`cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`,
`cargo test` grün). Erst bei grünen Gates weiter. Datei-Regeln aus dem
Prompt gelten ab der ersten Datei (nummeriert, ≤~300 Zeilen, eine
Zuständigkeit).

## R0 — Plan-Docs (Basis)

- `plan/20260926_01_face/{implementation_plan,task,deps}.md` liegen vor und
  sind konsistent (Modulnummern, Schwellen 0.45/0.65/0.88, Modell-URLs).
- Kein Code, keine Gates nötig.
- Commit: `docs(plan): face re-id implementation plan and tasks`.

## R1 — Gerüst + Typen

- Implementierung: `source7/Cargo.toml` (ort ohne Default, `cuda`-Feature
  optional; x11rb; macroquad no-default; serde; bincode; Release-Profil aus
  source2), `.gitignore` (`*.onnx`, `faces_db.bin`, `target/`),
  `download_models.sh` (curl beider Modelle aus yakhyo `v0.0.1` + Größen-
  Check), `deps.md`, `src/01_types.rs` (`BBox`, `Landmarks5`,
  `Embedding512`, `Exemplar`, `PersonRecord`, `FaceDetection`).
- Host-Tests: `cargo test` (Serde-Roundtrip, `Embedding512::zeros` ist
  nicht normiert / `normalize` → Norm 1); `bash -n download_models.sh`.
- Validierung: Gates grün; Datei ≤~300 Zeilen.
- Commit: `feat(source7): scaffold cargo, download script and types`.

## R2 — Capture + Alignment (ohne Modell)

- Implementierung: `02_screen_capture.rs` (X11-Root-Grab 640×640 Z-Pixmap →
  RGB-Puffer, reine BGRA→RGB-Konvertierung), `03_alignment.rs`
  (ArcFace-Template-Konstante, Least-Squares-Fit 4 Unbekannte + 4×4-Gauß,
  2×3-Invers, Bilinear-Warp 112×112 mit Border 0).
- Host-Tests: Warp-Identität (Einheits-Trafo reproduziert Pixel), Fit auf
  Template aus synthetisch transformierten Punkten (<1e-3), BGRA→RGB-
  Vektorvergleich; X11-Grab nur im Xvfb-Smoke (R7).
- Validierung: Gates grün; beide Dateien ≤~300 Zeilen.
- Commit: `feat(source7): x11 capture and umeyama alignment`.

## R3 — SCRFD-Detektor

- Implementierung: `04_scrfd_detector.rs` (Session-Aufbau mit Provider-
  String, Preprocessing `(x-127.5)/128` BGR→RGB planar, Anchor-Center pro
  Stride mit Duplikation ×2, `distance2bbox`, `distance2kps`, Greedy-NMS
  mit `+1`, `conf=0.5`, `iou=0.4`, Rescale per `det_scale`).
- Host-Tests (ohne `.onnx`): synthetische Logits/BBox/Kps-Tensoren →
  exakte Box-/Keypoint-Koordinaten; NMS-Beispiel (stärkste Box überlebt,
  IoU>0.4 verworfen); Anchor-Count `Σ h·w·2` für 640/8/16/32.
- Validierung: Gates grün; Datei ≤~300 Zeilen.
- Commit: `feat(source7): scrfd detector with anchor decoding and nms`.

## R4 — ArcFace-Embedding

- Implementierung: `05_arcface_embed.rs` (Session-Aufbau, 112-Preprocessing
  `(x-127.5)/127.5` RGB planar, Forward → 512D, zwingend L2-Norm).
- Host-Tests (ohne `.onnx`): L2-Norm (`norm==1±1e-5`, Zero-Vektor bleibt
  Zero ohne NaN), Preprocessing-Pixelvergleich, Cosine=Dot an zwei
  normierten Vektoren.
- Validierung: Gates grün; Datei ≤~300 Zeilen.
- Commit: `feat(source7): arcface embedding with l2 norm`.

## R5 — Exemplar-Datenbank

- Implementierung: `06_face_database.rs` (`FaceDatabase`, Dot-Similarity,
  `update`: ≥0.65 bekannt (0.65–0.88 Exemplar anhängen, FIFO max 5;
  >0.88 redundant), <0.45 neu, 0.45–0.65 ambig/nur tracken;
  `save`/`load` via bincode, Thumbnails 112×112 RGB pro Exemplar).
- Host-Tests: Schwellen-Matrix (0.44/0.45/0.64/0.65/0.88/0.89-Grenzen),
  FIFO-Überlauf (6. Exemplar verdrängt ältestes), Bincode-Roundtrip im
  TempDir (Personen + Thumbnails byte-identisch).
- Validierung: Gates grün; Datei ≤~300 Zeilen.
- Commit: `feat(source7): exemplar database with bincode persistence`.

## R6 — Engine-Orchestrierung

- Implementierung: `07_engine.rs` (`Engine`: Detect→Align→Embed→Match→Update
  pro Frame, `provider`-String, Zähler Personen/Exemplare; Traits für
  Detector/Embedder damit Tests ohne ONNX laufen).
- Host-Tests: Fake-Detector (fixe Box+Landmarks) + Fake-Embedder (fixe
  Vektoren) → bekannte Person wiedererkannt, neue Person angelegt,
  Ambig-Bereich legt nichts an.
- Validierung: Gates grün; Datei ≤~300 Zeilen.
- Commit: `feat(source7): pipeline engine with cuda fallback`.

## R7 — UI + Headless-Smoke

- Implementierung: `main.rs` (CLI `--models/--db/--conf`, EP-Aufbau
  CUDA→CPU mit Fallback + Provider-Name, Macroquad 820×640: Feed mit
  Boxen/Keypoints/IDs, Sidebar mit Live-Crop/Galerie/HUD),
  `tests/headless_pipeline.rs` (DB-Roundtrip, Engine-Stubs, X11-Grab
  unter Xvfb als ignored ohne Display).
- Host-Tests: `./download_models.sh` (beide `.onnx` vorhanden, Größen
  plausibel); `xvfb-run -a cargo test` grün; `xvfb-run -a timeout 10
  cargo run -- --help` Exit 0; Smoke mit `--max-frames N` beendet sauber.
- Validierung: alle Gates grün; `git status` zeigt keine `*.onnx`.
- Commit: `feat(source7): macroquad ui with sidebar and hud`.

## R8 — Gates + Walkthrough

- Alle Gates final grün (`fmt --check`, `clippy --all-targets -D warnings`,
  `cargo test`, Xvfb-Smoke); FPS-Messung CPU (und CUDA falls verfügbar).
- `plan/20260926_01_face/walkthrough.md` (System, Entscheidungen,
  Testergebnisse, CUDA-vs-CPU, Learnings, neue Systempakete fürs Dockerfile).
- Commit: `docs(plan): walkthrough with cuda vs cpu measurements`.
