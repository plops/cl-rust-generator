# Tasks: `source9_gpu` — seriell abarbeiten, jeder Schritt validiert

Jeder Task endet mit seinem Nachweis. Abbruch bei Rot, kein Weitergehen.

## T1 — Gerüst: Kopie + Protokoll v2

- `source7_mvp` → `source9_gpu` kopieren ohne `target/`, ohne `models/`-Dateien;
  `models` als Symlink auf `../source7_mvp/models`.
- `common`: `SIZE` → `WIDTH=1280`, `HEIGHT=720`; `PROTO_VERSION` 1 → 2.
- Alle `SIZE`/640-Stellen umstellen (Client-Fenster/Szene, Server-Capture,
  Tests, Smoke-Skript mit 1280×720-Xvfb).
- Nachweis: `cargo build --workspace` grün (noch CPU-ORT).

## T2 — CUDA-EP in `ort` verdrahten

- `server/Cargo.toml`: `ort`-Feature `cuda` dazu (Version bleibt rc.13).
- `03_ocr.rs`: `session()` baut mit `ep::CUDA` (`device_id(0)`,
  `error_on_failure`); `Ocr::load` bekommt EP-Wahl (`--cpu` ⇒ CPU, sonst CUDA
  mit Fallback auf CPU + Warnung); aktiver EP wird geloggt.
- `01_config.rs`: `--cpu`-Flag; `main.rs`/`07_session.rs`: durchreichen + loggen.
- Nachweis: `cargo build --release -p lbw-server`; Start-Log zeigt EP.

## T3 — Detektor-Padding 1280×720 → 1280×736

- `pad_to_32` (Auffüllen unten, Randfarbe) + Box-Clip auf 720 in `03_ocr.rs`;
  `detect` bleibt für 32er-Vielfache generisch.
- Unit-Tests: Pad-Größe, Clip am Rand, leere/volle Frames.
- Nachweis: `cargo test -p lbw-server --lib` grün.

## T4 — Verbose-Timing als GPU-Nachweis

- `Ocr::text` misst Detektor-/Erkenner-ms; Session loggt sie bei `-v`.
- Nachweis: Smoke-Log zeigt Zeiten; `nvidia-smi` zeigt `lbw-server`-Prozess.

## T5 — GPU-Modelltest (ignored)

- `server/tests/models.rs`: 1280×720-Crop aus `test_screen.ppm`, assertiert
  CUDA-EP (außer `LBW_CPU=1`), Text > 20 Zeichen, druckt Zeiten.
- `padding.rs`: Display/Größen auf 720p heben (Marker-Text, Sweep unverändert).
- Nachweis: `cargo test --release -p lbw-server --test models -- --ignored`.

## T6 — Loopback + Client-Tests

- Server-/Client-Loopback mit Dummy-OCR auf 1280×720; Client-Unit-Tests.
- Nachweis: `cargo test --workspace` grün (ohne X11/GPU).

## T7 — Smoke E2E (Xvfb 1280×720)

- `scripts/smoke_xvfb.sh`: Screen 1280×720, xterm-Geometrie, Probe wie bisher.
- `xvfb`/`xterm` per apt installieren (falls fehlend).
- Nachweis: `./scripts/smoke_xvfb.sh` → `smoke: OK`.

## T8 — Hygiene, Docs, Commits

- `cargo fmt`, `cargo clippy -- -D warnings` (eigener Code), Deps auf neueste
  Versionen prüfen (keine neuen Crates erwartet).
- `README.md`, `deps.md` (GitHub `org/projekt`), `collect.sh` für source9.
- Commits nach `plan.md` §5 (feat + docs), Historie grün.
- Nachweis: `git log --oneline -3`, `cargo test --workspace` nochmals grün.

## T9 — Walkthrough

- `plan/20261009_01_gpu_bigger/walkthrough.md` nach den strikten Regeln aus
  `prompt.txt` (deutsch, didaktisch, Mermaid, Code-Beispiele, Learnings,
  Dockerfile-Pakete) + Commit.
