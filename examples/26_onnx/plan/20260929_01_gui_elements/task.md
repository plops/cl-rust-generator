# task.md — 20260929_01_gui_elements: seriell abarbeitbare Schritte

PoC GPA-GUI-Detector in `examples/26_onnx/source8/` (Plan:
`implementation_plan.md`). Rust-Gates nach jedem Rust-Schritt:
`cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`,
`cargo test`. Erst bei grünen Gates weiter. Vor jedem Commit:
`git status` — keine `*.onnx`/`*.pt`/`*.ppm`/`.venv` im Index.

## T0 — Plan-Docs

- `implementation_plan.md`, `task.md`, `deps.md` konsistent.
- Commit: `docs(plan): gpa gui detector poc plan, tasks and deps`.

## T1 — Python-Umgebung + Modell-Download

- Implementierung: `source8/.gitignore`, `python/pyproject.toml` (uv,
  torch-CPU-Index, opencv-headless-Override), `uv.lock`,
  `scripts/fetch_model.sh` (gepinnter HF-Commit, SHA256).
- Test: `./scripts/fetch_model.sh` zweimal (2. Lauf „cached“);
  `uv run python -c "from ultralytics import YOLO; print(YOLO('../models/model.pt').names)"` → `{0: 'icon'}`.
- Commit: `build(source8): uv export env and model fetch script`.

## T2 — Rust-Grundlagen: Bild, Capture, Letterbox

- Implementierung: `Cargo.toml` (ort no-default, x11rb, Feature `cuda`,
  `embed`), `src/01_image.rs`, `src/02_capture.rs`, `src/03_letterbox.rs`,
  `src/lib.rs`, minimaler `src/main.rs` mit `grab`-Kommando (X11 → PPM).
- Host-Tests: PPM-Roundtrip, BGRA→RGB, Letterbox 1920×1080→640² (r=1/3,
  Pad oben 140), Box-Rücktransformation, Konstantbild bleibt konstant.
- Xvfb-Nachweis: `xvfb-run gui_detect grab out.ppm` → Größe = Screen.
- Commit: `feat(source8): image io, x11 capture and letterbox`.

## T3 — Kalibrier-/Testbilder + ONNX-Export

- Implementierung: `scripts/make_screens.sh` (Xvfb-Szenen, `grab`),
  `python/export.py`, `scripts/export_models.sh`.
- Ausgabe `models/`: `gpa_{640,384x640}_{fp32,fp16,int8}.onnx`,
  `example_input.ppm`, `reference.tsv` (Ultralytics-fp32-ONNX-Boxen).
- Test: `export.py` prüft jede Variante mit onnxruntime-Python
  (Shape `[1,5,N]`, Recall@IoU0.5 vs. fp32 wird geloggt).
- Commit: `feat(source8): onnx export with fp16 and int8 qdq variants`.

## T3b — Browser-Kalibrierung (Nachtrag nach Review)

- Implementierung: `scripts/fetch_browsers.sh` (Firefox-Tarball, Chrome for
  Testing, Libs/Fonts), `scripts/make_web_screens.sh` (12 Kalibrier- und 6
  disjunkte Test-Websites × Firefox/Chrome); `export.py` kalibriert nur auf
  `screens/calib/`, wertet nur auf `screens/eval/` + Beispielbild aus.
- Test: Kontaktbogen aller Screenshots prüfen (keine leeren Seiten);
  Export-Report; Vergleich alte vs. neue INT8-Kalibrierung auf dem Testset;
  Paritätstests weiter grün.
- Commit: `feat(source8): calibrate int8 on browser screenshots with held-out eval`.

## T4 — Session, Decode, Detector

- Implementierung: `src/04_session.rs` (CPU/CUDA, Threads, Warmup-Probe),
  `src/05_decode.rs` (Decode, IoU, NMS), `src/06_detector.rs`.
- Host-Tests: synthetischer `[1,5,N]`-Tensor → exakte Boxen; NMS behält
  stärkste Box; Score-Filter.
- Commit: `feat(source8): ort session, yolo decode and detector`.

## T5 — CLI detect + bench

- Implementierung: `src/07_cli.rs`, `src/08_bench.rs`, `main.rs`-Verdrahtung.
- Host-Tests: Parser-Tests, Median/p90; `tests/cli_smoke.rs`.
- Nachweis: `gui_detect detect models/gpa_640_fp32.onnx models/example_input.ppm --out /tmp/a.ppm`.
- Commit: `feat(source8): detect and bench cli`.

## T6 — Parität + Xvfb-Smoke

- `tests/parity.rs` (`#[ignore]`), `cargo test -- --ignored` grün.
- Xvfb: `make_screens.sh`-Szene + `detect x11` → Boxen > 0.
- Commit: `test(source8): parity against ultralytics and xvfb smoke`.

## T7 — Benchmarks + Quantisierungsfrage

- `scripts/bench.sh`: CPU (32/8/4 Threads) und `--features cuda` ×
  Varianten; Binärgrößen (ohne/mit `embed`); Ergebnisse in
  `source8/bench.md`.
- Commit: `perf(source8): benchmark cpu vs cuda and quantized variants`.

## T8 — Aufräumen + Walkthrough

- `cargo upgrade` (cargo-edit), fmt/clippy/test erneut; Dateigrößen ≤~300.
- `walkthrough.md` (Deutsch, Mermaid, Struktur 1–4 laut Prompt).
- Commit: `docs(plan): walkthrough for gui element detection poc`.

## T9 — Live-Modus (Nachtrag)

- Implementierung: `02_capture::grab_region`, `09_window.rs` (X11-Fenster
  per x11rb, PutImage in Streifen, HUD, Escape/q/WM_DELETE_WINDOW),
  `10_live.rs` (Ausschnitt = Modell-Input, keine Skalierung),
  `07_cli.rs` nur noch Parser, Ausführung nach `11_run.rs` (ohne
  Verhaltensänderung).
- Host-Tests: Parser `live` (Default-Modell, `--x/--y/--frames`),
  Fensterposition, BGRX-Swizzle; alle bisherigen Tests grün.
- Xvfb-Nachweis: `smoke_xvfb.sh` (20 Frames Live), manuell Firefox im
  Ausschnitt + `xdotool key q` → Exit 0; Region außerhalb → Exit 1.
- Commit: `feat(source8): live window with detection overlay`.
