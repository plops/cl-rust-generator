# Plan: GPU-Beschleunigung (RTX A4000) + 1280×720 — `source9_gpu`

Stand: 2026-10-09. Basis: `source7_mvp` (fest 640×640, CPU-ORT).
Ziel: `source9_gpu` — gleiche Architektur, aber ONNX auf der NVIDIA RTX A4000
(CUDA-EP) und 1280×720-Capture. Direkt in Rust geschrieben (kein Transpiler).

## 1. Befund: Was die Hardware hergibt (verifiziert, nicht geraten)

- `nvidia-smi`: RTX A4000, 16 GB, Treiber 615.78, CUDA-UMD 13.4, keine Prozesse.
- `nvcc`: CUDA 13.4; cuDNN 9 (`libcudnn_*.so.9`) systemweit vorhanden.
- `ort 2.0.0-rc.13` (aktuellste Version auf crates.io) lädt via
  `download-binaries` für `x86_64-linux-gnu` + `cuda`-Feature ein
  ONNX-Runtime-1.28-Binary mit **CUDA 13** (`dist.tsv`: `cuda13,tensorrt,nvrtx`).
  Das passt exakt zum installierten CUDA 13.4 + cuDNN 9 — **kein apt-Paket nötig**.
- Testbild `test_screen.ppm` ist 1920×1080 → ein 1280×720-Crop für Tests geht.
- Kein X-Server auf diesem Rechner (`DISPLAY` leer) → Smoke via Xvfb.

## 2. Implementierungsvorschlag

1. `source7_mvp` nach `source9_gpu` kopieren (ohne `target/`), Protokoll auf
   v2 heben: `SIZE` (640) → `WIDTH=1280`, `HEIGHT=720` in `common`.
2. Server: `ort` um Feature `cuda` erweitern; Sessions bekommen
   `ep::CUDA::default().with_device_id(0)` (mit `error_on_failure`, damit ein
   fehlendes CUDA explizit scheitert statt still auf CPU zu fallen). CLI bekommt
   `--cpu` als erzwungenen CPU-Fallback; Default ist CUDA-mit-Fallback-auf-CPU
   plus Startup-Log des aktiven EPs. Im Verbose-Log: Detektor-/Erkenner-Zeiten
   in ms als GPU-Nachweis.
3. Detektor-Eingabe: DBNet braucht 32er-Vielfache. 1280 ok, 720 nicht
   (22,5×32). Lösung: Frame auf **1280×736 padden** (16 px unten, mit der
   dominanten Randfarbe), Boxen danach auf 720 clippen. **Kein Tiling nötig**:
   DBNet ist voll-convolutional (beliebige 32er-Größe), die A4000 hat 16 GB —
   Tiling bräuchte Box-Stitching/NMS und wäre nur Komplexität ohne Nutzen.
   Der Erkenner arbeitet ohnehin auf Crops und bleibt unverändert.
4. Client: Fenster/Szene 1280×720, Rest (AV1, Framing, Input) unverändert.
5. `models/` als Symlink auf `../source7_mvp/models` (dort liegen die echten
   Dateien); `--models` bleibt.
6. Tests: Unit (Pad/Clip), Loopback ohne GPU, `models`-Test (ignored, echte
   Modelle + GPU, assertiert CUDA-EP + Text + Zeit), Smoke-Skript mit
   1280×720-Xvfb.

## 3. Offene Requirements / Vorschläge (mit Empfehlung)

| # | Frage | Empfehlung |
|---|---|---|
| 1 | Stiller CPU-Fallback oder hart scheitern ohne GPU? | Fallback mit Warnung (Smoke bleibt portabel); `--cpu` erzwingt CPU für Vergleichsmessung |
| 2 | Mehrere GPUs (`--device-id`)? | Nein — genau eine A4000 im Rechner; `device_id(0)` fest |
| 3 | VRAM-Limit / FP16 / TensorRT-EP? | Nein — Modelle sind klein (10+21 MB), FP32 reicht; TRT bräuchte `libnvinfer` (nicht installiert) |
| 4 | Bandbreiten-Budget bei 720p (3,1× Pixel von 640²)? | Nur beobachten: Smoke meldet B/Frame; Quantizer bleibt 180 |
| 5 | Tiling statt Padding? | Nein (s. Punkt 3 oben) |
| 6 | Client-Skalierung (Fenster ≠ 1280×720)? | Nein — fest wie im MVP |
| 7 | YOLO-Detektor (`gpa_640_int8.onnx`) zurückholen? | Nein — MVP hat ihn bewusst gestrichen; GPU ändert nichts daran |

## 4. Kontext-Dateien für den ausführenden Agenten

MVP-Referenz (`source7_mvp/`, lesen vor dem Ändern):

- `common/src/01_types.rs` — Protokolltypen, `SIZE`, `PROTO_VERSION` (→ v2, W/H).
- `common/src/02_framing.rs`, `03_yuv.rs` — unverändert übernehmen (Verständnis).
- `server/src/01_config.rs` — CLI (→ `--cpu`-Flag dazu).
- `server/src/02_capture.rs` — Scrap-Capture (→ 1280×720-Ausschnitt).
- `server/src/03_ocr.rs` — Detektor/Erkenner (→ CUDA-EP, Padding, Timing).
- `server/src/04_tiles.rs` — BBox + Maske (→ Größen-Generalisierung prüfen).
- `server/src/05_av1.rs` — Encoder (unverändert, Größen kommen aus BBox).
- `server/src/06_input.rs` — unverändert (Koordinaten sind relativ).
- `server/src/07_session.rs` — Schleife (→ EP-Log, Timing-Log).
- `server/src/main.rs`, `lib.rs` — Verdrahtung (→ W/H, EP-Wahl).
- `server/tests/models.rs|padding.rs|loopback.rs` — Testmuster (→ anpassen).
- `client/src/*`, `client/examples/probe.rs` — Fenster/Szene (→ 1280×720).
- `scripts/smoke_xvfb.sh`, `deps.md`, `README.md` — Vorlagen für source9.
- `Cargo.toml`-Dateien — Workspace/Deps (→ `cuda`-Feature bei `ort`).

Externe Referenzen:

- `ort`-CUDA-Doku: `ep::CUDA::default().with_device_id(0).build()`,
  `SessionBuilder::with_execution_providers`, `.error_on_failure()`
  (DeepWiki `pykeio/ort`, verifiziert in `ort-2.0.0-rc.13/src/ep/cuda.rs`).
- `ort-sys/dist.tsv`: CUDA-13-Binary für Linux x64 (verifiziert im .crate).
- Docker-Referenz: `/workspace/src/cl-cl-generator/example/05_dockerfile_meta/
  source01/examples/03_ai_env/Dockerfile` (GPU-Container-Setup).

## 5. Commit-Konvention (verbindlich)

Conventional Commits (`feat:`/`fix:`/`docs:`/`test:`), deutsche Messages,
Betreff ≤ 72 Zeichen, Body mit Was/Warum/Verifikation:

```
feat: GPU-Server mit 1280x720-Capture in source9_gpu

Was: ...
Warum: ...
Verifikation: cargo test --workspace, smoke_xvfb.sh, nvidia-smi zeigt Prozess.
```

Geplante Commits: (1) `docs: Plan ...`, (2) `feat: ... source9_gpu`,
ggf. (3) `fix: ...`, (4) `docs: Walkthrough ...`.
Jeder Commit baut (`cargo build --release`) und hält Tests grün.
