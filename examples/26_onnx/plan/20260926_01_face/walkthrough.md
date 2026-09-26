# Walkthrough — 20260926_01_face

Live-Gesichtserkennung und Re-ID in `examples/26_onnx/source7/`
(Binary `x11_face_reid`). Stand: 2026-09-26, alle Gates grün.

## System

Pipeline pro Frame: X11-Root-Grab (640×640, `x11rb`, BGRA→RGB) → SCRFD
(`det_500m.onnx`, 9 Outputs: Scores/BBox/Kps × Strides 8/16/32, Anchor-
Decode mit Stride-Skalierung, Greedy-NMS IoU 0.4) → Umeyama-Alignment
(Least-Squares-Fit auf ArcFace-Template, Bilinear-Warp 112×112) →
ArcFace (`w600k_mbf.onnx`, 512D, L2-normiert) → Exemplar-DB (Dot-Product,
Schwellen 0.45/0.65/0.88, max. 5 Exemplare/Person mit Thumbnails,
`faces_db.bin` via serde+bincode) → Macroquad-UI (820×640: Feed mit
Boxen/Keypoints/IDs, Sidebar mit Live-Crop, Galerie, HUD mit Personen,
Exemplaren, FPS, Provider). EP-Strategie: CUDA zuerst inkl.
Warmup-Validierung, bei jedem Fehler CPU-Fallback (multi-threaded).

## Code-Entscheidungen (vs. Plan)

- Ähnlichkeit als lineare Least-Squares (4 Unbekannte, Gauß 4×4) statt
  SVD-Umeyama — wie geplant, ohne Mathe-Dep, Test gegen bekannte Trafo.
- `session_with_fallback` liest Modelle in den Speicher und nutzt
  `commit_from_memory` (wie `source2`), weil `commit_from_file` das
  `std`-Feature in `ort` verlangt. Kein Nachteil: Modelle sind klein.
- `ort::execution_providers::{CPU, CUDA}` (nicht `*ExecutionProvider`) —
  Namen aus `ort 2.0.0-rc.13`-Quelle verifiziert; `BuilderResult` braucht
  `Ok(...?)`-Form im `and_then`.
- Warmup-Pflicht für CUDA (Abweichung vom Plan, begründet): Commit
  gelingt auch ohne cuDNN, erst der erste Conv-Knoten scheitert. Daher
  beweist `try_cuda` den EP mit einer Null-Inferenz; Fehlschlag →
  CPU-Fallback. Ohne diesen Fix wäre der CUDA-Build zur Laufzeit
  abgestürzt statt zurückzufallen (per Test nachgewiesen).
- Stride-Skalierung der Regressionen (`preds * stride`) — erster E2E-Lauf
  zeigte 5px-Geisterboxen statt Gesicht; Referenzcode-Abgleich fand den
  fehlenden Faktor. E2E-Test pinnt jetzt exakt 1 Box im Referenzporträt.
- `bincode` mit `serde`-Feature (`bincode::serde::*`), manuelle
  Serde-Impls für `[f32; 512]` (serde kann nur Arrays ≤32).
- Trait-Impls (`Detector`/`Embedder` für echte Typen) liegen in
  `07_engine.rs`, nicht `main.rs` — damit der E2E-Test sie nutzt.
- Stats-Zeile am Ende (`stats frames=… faces=… persons=… exemplars=…
  provider=…`) für headless Nachweisbarkeit und Automation.
- Datei-Regel eingehalten: größte Datei 270 Zeilen (`04`), Rest ≤260.

## Testergebnisse

- `cargo fmt --check`: sauber. `cargo clippy --all-targets -- -D warnings`:
  sauber, sowohl Default- als auch `--features cuda`-Build.
- `cargo test`: 20 Unit + 1 Integration grün (Umeyama-Fit, Warp-Identität,
  Ankerzahlen, Decode-Exaktheit, NMS, L2-Norm, Schwellen-Matrix,
  FIFO-Cap, Bincode-Roundtrips, Engine mit Fakes).
- Ignored (brauchen Modelle): `with_models` (SCRFD 9 Outputs mit
  [12800,3200,800]/4×/10×-Layouts, ArcFace 512D≠0) grün; E2E
  `real_models_detect_and_reidentify_face` (Ross-Porträt → 1 Box
  158×243px Score 0.82, Person 0, Re-ID im 2. Frame redundant) grün —
  auch im `--features cuda`-Build (Fallback → CPU).
- `xvfb-run` Smoke: Default-Build 5 Frames Exit 0; CUDA-Build 3 Frames
  Exit 0 mit `provider=CPU` (cuDNN-Fehler sauber abgefangen).
- `git check-ignore`: `models/*.onnx` ignoriert, `tests/assets/face640.ppm`
  (1,2 MB, generiert aus yakhyo `assets/faces/Ross.png`) bleibt Test-Asset.

## Performance (Release, Threadripper PRO 7955WX, CPU)

- 100 Frames blank (Capture+Detect+Render, inkl. Startup): 1036 ms Wall
  → ~96 FPS effektiv; Inferenz ist kein Engpass bei det_500m.
- E2E mit Gesicht (2 Sessions laden + 2× Detect+Align+Embed+DB): 0,09 s.
- CUDA vs. CPU: kein CUDA-Messwert möglich — im Container fehlt cuDNN
  (`libcudnn.so` nicht vorhanden), der CUDA-EP scheitert am ersten Conv-
  Knoten und der Fallback greift (nachgewiesen: `provider=CPU`). Auf der
  Workstation mit cuDNN ist allein der EP-Wechsel nötig
  (`--features cuda`); die Architektur (kleine Modelle, 640 nativ, kein
  Resize) ist GPU-freundlich. Erwartung: mehrere hundert FPS Inferenz,
  limitiert von X11-Grab + V-Sync wie in `source2/doc.md` beschrieben.

## Learnings

1. ONNX-Runtime-CUDA-Commit ≠ Lauffähigkeit: EP-Verfügbarkeit erst per
   Probe-Inferenz behaupten, sonst kracht es im Hot-Path.
2. SCRFD ohne `* stride` sieht plausibel aus (Scores ok), liefert aber
   Pixelkrümel — E2E mit Echtgesicht ist der einzige ehrliche Decode-Test.
3. `ort`-API-Drift: Typnamen (`CPU`/`CUDA`) und `BuilderResult`-Fehler-
   typ nur aus der Registry-Quelle verlässlich; Doku-Seiten waren 404.
4. `chunks_exact` mit Konstanten löst in Rust 1.98 den
   `as_chunks`-Lint aus; `frame % 300 == 0` will `is_multiple_of`.

## Neu installierte Systempakete (fürs Dockerfile)

- `libxkbcommon0 libgl1 libxi6 libxcursor1` — miniquad-X11-Backend
  (vorher `DlOpenError libxkbcommon.so`), inkl. gezogener
  `libxfixes3`-Dep.
- `python3-pil` — nur Test-Asset-Erzeugung (PNG→PPM, System-`/usr/bin/
  python3`, nicht venv); kein Laufzeit-Bedarf, optional im Dockerfile.
- Bereits vorhanden und genutzt: `xvfb`/`xvfb-run`, CUDA-Runtime-Libs
  (13.x, ohne Compiler/cuDNN), Rust 1.98.1.
- Modelle (nicht im Git): `det_500m.onnx` (2,5 MB), `w600k_mbf.onnx`
  (13,6 MB) aus yakhyo `face-reidentification` Release `v0.0.1` per
  `source7/download_models.sh`.
