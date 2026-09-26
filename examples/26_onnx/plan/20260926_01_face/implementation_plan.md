# Implementierungsplan — 20260926_01_face

Live-Gesichtserkennung und Re-ID in `examples/26_onnx/source7/`: X11-Capture
(640×640) → SCRFD (`det_500m.onnx`) → Umeyama-Alignment (112×112) → ArcFace
(`w600k_mbf.onnx`, 512D L2-normiert) → Online-Exemplar-Clustering mit
Pure-Rust-Persistenz (`faces_db.bin` via serde/bincode) → Macroquad-UI
(820×640: Feed + Sidebar mit Preview, Galerie, HUD).

Auftrag: Wol Pumba (wolpumba@gmail.com) · Linux (Ubuntu 26, Docker, X11) ·
Prompt: `plan/20260926_01_face/prompt.txt` · Basis: `source2/{Cargo.toml,
src/main.rs}` (X11 + ort + Macroquad-Minimal) · Referenzen:
`yakhyo/face-reidentification` (SCRFD-Post-Processing, ArcFace-Pipeline),
`deepinsight/insightface` (ArcFace-Template, Umeyama) · Zielplattformen:
Workstation (Threadripper PRO 7955WX + RTX A4000, CUDA 13.3) und Laptop
(Ryzen 7 7735HS, CPU/AVX2) · Stil: direkt nativer Rust-Code, modular,
≤~300 Zeilen/Datei, minimale Deps, Vorgängerpläne `plan/2026092*/`.

## Goal

Ein startbares Rust-Binary, das den X11-Root-Window live abgreift, Gesichter
detektiert (BBox + 5 Landmarks), auf 112×112 alignt, 512D-Embeddings
extrahiert und Personen über eine Exemplar-Bank (max. 5/Person, inkl.
Thumbnails) wiedererkennt — mit CUDA-first/CPU-Fallback bei `ort`, ohne
FAISS, ohne eingecheckte Binärmodelle (`download_models.sh` lädt sie per
curl aus den yakhyo-Releases).

## Success Criteria

1. `cargo fmt --check` und `cargo clippy --all-targets -- -D warnings`
   fehlerfrei; keine Quelldatei über ~300 Zeilen.
2. `cargo test` grün: Unit-Tests für Umeyama/Warp, Anchor-Decoding,
   Distance2BBox/Kps, NMS, L2-Norm, Cosine/Dot, Clustering-Schwellen
   (0.45/0.65/0.88), Bincode-Roundtrip.
3. `xvfb-run` Headless-Nachweis: Binary startet, Capture→Detect→Align→
   Embed-Pfad läuft ohne Display-Hardware (Modelle via `download_models.sh`).
4. UI zeigt 640×640-Feed mit Boxen/Keypoints/IDs plus 180px-Sidebar
   (Live-Crop, Galerie, HUD: Personen, Exemplare, FPS, Provider).
5. `faces_db.bin` persistiert Exemplare (Vektor + 112×112-Thumbnail);
   Schwellenlogik aus dem Prompt ist exakt umgesetzt.
6. Walkthrough mit CUDA-vs-CPU-Messung, Learnings und Dockerfile-Paketen.

## Context And Current Facts

- `source2` (gelesen): X11-Z-Pixmap-Grab (BGRA→planar f32), ort-Session aus
  Memory, Macroquad-Loop mit V-Sync-Obergrenze; Doku `doc.md` beschreibt
  NCHW-Split, Inferenz-Engpass, FPS-Bestimmung.
- SCRFD (via DeepWiki + `scrfd.py`/`helpers.py`, verifiziert): Input 640×640,
  Norm `(x-127.5)/128`, BGR→RGB; 9 Outputs (Scores/BBox/Kps × Strides
  8/16/32, `fmc=3`); `_num_anchors=2` (Anchor-Center dupliziert);
  `distance2bbox` (l,t,r,b vom Center), `distance2kps` (Offset addiert);
  Greedy-NMS mit `+1`-Flächen, Default `conf=0.5`, `iou=0.4`.
- ArcFace (via DeepWiki + `arcface.py`, verifiziert): Template
  `[[38.2946,51.6963],[73.5318,51.5014],[56.0252,71.7366],[41.5493,92.3655],
  [70.7299,92.2041]]`; Norm `(x-127.5)/127.5`; 512D + L2-Norm; Cosine =
  Dot-Product bei normierten Vektoren.
- Modelle (GitHub-API, verifiziert): Release `v0.0.1` in
  `yakhyo/face-reidentification` enthält `det_500m.onnx` (2.5 MB) und
  `w600k_mbf.onnx` (13.6 MB) mit stabilen `browser_download_url`s.
- `ort 2.0.0-rc.13` (Registry, verifiziert): Features u.a. `cuda`,
  `download-binaries`, `copy-dylibs`; EP-Reihenfolge CUDA→CPU wie im
  Python-Referenzcode (`providers=[CUDA, CPU]`).
- Umgebung (verifiziert): RTX A4000 + CUDA-UMD 13.3, kein `nvcc` (nur
  Runtime-Libs), `Xvfb`/`xvfb-run` vorhanden, Rust 1.98.1, 32 Cores.

## Constraints And Non-goals

- Direkt Rust, keine Lisp-Generator-Artefakte; nummerierte Module gemäß
  Datenfluss (`01_types` … `07_engine`, `main.rs` nur CLI/UI-Verdrahtung).
- Kein FAISS/C++-VDB (Prompt-Verbot: Overkill + Build-Risiko); keine
  Bild-Crates (kein `image`/`ndarray`/`nalgebra`) — Alignment/NMS sind
  ~100 Zeilen Handcode.
- Keine Binärmodelle im Git (`.gitignore`: `*.onnx`, `faces_db.bin`).
- Non-goals: kein Tracking über Frames (nur DB-Match), kein Wayland, kein
  Multi-Kamera, kein Web-Export, kein Trainingscode.

## Key Decisions

1. **Ähnlichkeitstransformation als lineare Least-Squares (4 Unbekannte),
   nicht SVD-Umeyama.** Ansatz `x'=a·x−b·y+tx`, `y'=b·x+a·y+ty`, 10
   Gleichungen → Normalgleichung 4×4 → Gauß. Exakt die gesuchte
   Ähnlichkeit (Skale+Rotation+Translation, kein Shear), ~40 Zeilen ohne
   Mathe-Dep. Verworfene Alternative: `nalgebra`-SVD — schwerer + Overkill.
2. **Warp über inverse Matrix mit bilinearer Interpolation, Border 0.**
   Entspricht `cv2.warpAffine(M, borderValue=0)`; pro Zielpixel ein
   Lookup — O(112²), cache-freundlich, deterministisch testbar.
3. **SCRFD-Decode 1:1 zum Referenzcode** (Anchor-Duplikation pro Location,
   `+1`-NMS, Rescale per `det_scale`). Der Prompt verlangt 640×640 nativen
   Capture, daher `det_scale=1` im Normalfall; Letterbox-Pfad entfällt
   bewusst (kein Resize nötig).
4. **EP-Strategie: CUDA zuerst versuchen, Fehler fangen, CPU-Fallback.**
   `Session::builder().with_execution_providers([CUDA, CPU])` analog zum
   Python-Referenzcode; aktiver Provider wird als String fürs HUD
   gespeichert. `cuda` ist Cargo-Feature (Workstation baut damit, Laptop
   ohne) — cfg-gated, damit CPU-Builds ohne CUDA-Toolkit kompilieren.
5. **DB als `Vec<PersonRecord>` mit serde+bincode.** Cosine=Dot bei
   L2-normierten Vektoren; Schwellen exakt aus dem Prompt
   (0.45/0.65/0.88), max. 5 Exemplare (FIFO bei Überlauf). Kein Index —
   wenige tausend × 512 Dot-Products sind <1 ms.
6. **UI ein Fenster 820×640 aus einem Macroquad-Image.** Links Feed,
   rechts Sidebar (Preview oben, Galerie darunter, HUD unten). Thumbnails
   als rohe RGB-Texturen ohne Bild-Dep.

## Relevante Dateien (mit Begründung)

- `source7/Cargo.toml` — Deps: `ort` (CUDA-Feature optional), `x11rb`,
  `macroquad` (no-default, ohne Audio), `serde`, `bincode`; Release-Profil
  aus `source2` (LTO, `codegen-units=1`) für CPU-Laptop-Speed.
- `source7/download_models.sh` — curl aus yakhyo-Release `v0.0.1`,
  prüft Größe, kein Commit von `.onnx` (Prompt-Pflicht).
- `source7/.gitignore` — `*.onnx`, `faces_db.bin`, `target/`.
- `source7/deps.md` — GitHub-Registry `<org>/<projekt>` (Prompt-Pflicht).
- `source7/src/01_types.rs` — `BBox`, `Landmarks5`, `Embedding512`,
  `Exemplar`, `PersonRecord`, `FaceDetection`; serde-Schemata + Tests.
- `source7/src/02_screen_capture.rs` — X11-Root-Grab 640×640 Z-Pixmap →
  RGB; einziger X11-Code neben `main` (Test via Xvfb).
- `source7/src/03_alignment.rs` — ArcFace-Template, Least-Squares-Fit,
  Invers, Bilinear-Warp; rein, ohne X11/Modell testbar.
- `source7/src/04_scrfd_detector.rs` — Session, Preprocessing, Anchor-
  Decoding, NMS; synthetische Tensor-Tests ohne `.onnx`.
- `source7/src/05_arcface_embed.rs` — Session, 112-Preprocessing, 512D
  L2-Norm; Norm-Tests ohne Modell.
- `source7/src/06_face_database.rs` — Dot-Similarity, Clustering-Update,
  Bincode-I/O; Schwellen-Tests + Roundtrip im TempDir.
- `source7/src/07_engine.rs` — Orchestrierung Capture→DB; Fake-Detector/
  Embedder-Tests ohne ONNX.
- `source7/src/main.rs` — CLI, EP-Aufbau mit Fallback, Macroquad-Loop,
  Sidebar-Rendering; Headless-Smoke via `xvfb-run`.
- `source7/tests/headless_pipeline.rs` — Integration: Capture unter Xvfb,
  DB-Roundtrip, Engine mit Stubs (ignored ohne Modelle).
- `plan/20260926_01_face/{implementation_plan,task,deps,walkthrough}.md`
  — Deliverables aus dem Prompt.

## Commit-Strategie (Conventional Commits)

1. `docs(plan): face re-id implementation plan and tasks` — Plan-Docs
   (dieses File + `task.md` + `deps.md`), keine Code-Änderung.
2. `feat(source7): scaffold cargo, download script and types` — Cargo.toml,
   `.gitignore`, `download_models.sh`, `deps.md`, `01_types.rs` + Tests.
3. `feat(source7): x11 capture and umeyama alignment` — `02_`, `03_` +
   Unit-Tests (Warp-Identität, Template-Fit).
4. `feat(source7): scrfd detector with anchor decoding and nms` — `04_` +
   synthetische Decode/NMS-Tests.
5. `feat(source7): arcface embedding with l2 norm` — `05_` + Norm-Tests.
6. `feat(source7): exemplar database with bincode persistence` — `06_` +
   Schwellen-/Roundtrip-Tests.
7. `feat(source7): pipeline engine with cuda fallback` — `07_` + Stub-Tests.
8. `feat(source7): macroquad ui with sidebar and hud` — `main.rs` +
   `headless_pipeline.rs` (xvfb).
9. `test(source7): xvfb headless gates green` — Modell-Download im CI-
   Cache, Smoke-Nachweise, Fixups (`fix(source7): …` bei Bedarf).
10. `docs(plan): walkthrough with cuda vs cpu measurements` — Walkthrough,
    Docker-Pakete, Learnings. Scope-Prefixe `feat/fix/refactor/test/perf`
    je nach Inhalt; jeder Commit lässt `fmt`, `clippy -D warnings`,
    `cargo test` grün.

## Risiken

- `ort/cuda`-Build lädt große GPU-Binaries und braucht cuDNN zur Laufzeit;
  Mitigation: `cuda` als optionales Feature, CPU-Pfad immer grün, HUD zeigt
  aktiven Provider ehrlich an.
- SCRFD-Output-Namen/Shape variieren je Export; Mitigation: Outputs per
  Index (`idx`, `idx+fmc`, `idx+2·fmc`) lesen, Shapes zur Laufzeit prüfen,
  synthetische Tests fixieren die Decode-Mathematik unabhängig vom Modell.
- X11-`get_image` ist synchroner IPC-Engpass (~ms); Mitigation: 640×640
  nativ (kein Resize), Single-Pass BGRA→Planar wie in `source2`.
