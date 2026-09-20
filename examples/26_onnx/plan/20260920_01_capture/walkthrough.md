# Walkthrough — 20260920_01_capture

X11-Live-Capture → YOLOv8 (ort) → Overlay-Anzeige (pixels). Code:
`examples/26_onnx/source0/`, Tasks: `task.md`, Deps: `deps.md`.
Stand: 2026-09-20, alle Gates grün, alles committet (7 Commits, s. unten).

## Was implementiert wurde (vs. Plan)

- `01_cli.rs`: alle Prompt-Flags (`--x/--y/--w/--h`, `--zoom`, `--fps`)
  plus Vorschläge aus Plan Kap. 3 (`--monitor`, `--win-w/--win-h`,
  `--model`, `--conf`, `--nms`, `--headless`, `--save-frame`).
- `02_capture.rs`: Monitor-Wahl, Clamp, RGBA→RGB (xcap liefert
  `RgbaImage`, kein BGRA — verifiziert, Plan-Annahme korrigiert).
- `03_infer.rs`: Letterbox-Preprocess, `output0`-Decode, Conf-Filter,
  NMS, Rückprojektion, 80 COCO-Labels, Datei- oder URL-Modell.
- `04_draw.rs`: eigener CPU-Rasterizer (Rahmen + Label-Balken,
  Klassen-Palette); `show_image`/`raqote` wie geplant NICHT eingeführt.
- `05_view.rs`: pixels+winit-Fenster mit Zoom-Blit und fps-Pacer,
  plus `--headless`/`--save-frame` für CI ohne GPU.
- `main.rs`: Pipeline-Verdrahtung mit Timing-Log pro Frame.
- Tests: 39 Unit + `cli_smoke` (3, davon 2 Xvfb-E2E ignored) +
  `geometry` (3, Public-API-Koordinatenvertrag).

## Stellen, die Tests/Realität erzwungen haben

1. **Rank-4-Input**: Preprocess lieferte erst `(3,640,640)` — das Modell
   verlangt `[1,3,640,640]` (`Invalid rank for input`, E2E-Feher). Fix:
   `Array4` mit Batch-Dim (Test-Shape angepasst).
2. **ort `fetch-models`**: `commit_from_url` steckt hinter dem
   Nicht-Default-Feature `fetch-models` (in `Cargo.toml` aktiviert).
   `Session::builder()` braucht in ort 2.x keine Env (Plan-Spike bestätigt).
3. **winit 0.30**: `run()`/`create_window()` sind deprecated; `run_app`
   braucht self-referenziellen `Pixels`-Zustand. Upstream-pixels-Muster
   übernommen: alte API mit gezieltem `#[allow(deprecated)]`.
4. **clippy-Lints (1.98)**: `chunks_exact_to_as_chunks` (`as_chunks`
   genutzt), `too_many_arguments` (`draw_box` nimmt `&BoundingBox`).
5. **System-Libs**: EGL/pipewire/libclang/gbm/drm-Header mussten per apt
   nachinstalliert werden (s. Docker-Liste); Link braucht `-lgbm`.
6. **Eigenfehler**: `tests/` wurde in S4/T1 nicht gestaged (nur getrackte
   Dateien committet) — per eigenem `test`-Commit nachgeholt, ohne
   History-Rewrite.

## Abweichungen vom Plan (bewusst)

- `Cargo.lock` wird NICHT committet: Repo-`.gitignore` (`*.lock`) plus
  Sibling-Präzedenz (`25_dnb/source0` ohne Lock) schlagen die
  Plan-Formulierung. Reproduzierbarkeit via `cargo upgrade`-Pin in `deps.md`.
- `--win-w` ohne `--win-h` (und umgekehrt) fällt auf Zoom zurück
  (statt Fehler) — dokumentiert in `zoomed_size`.
- Foto-E2E (bus.jpg) blieb manueller Nachweis; der committete E2E
  prüft die Pipeline (PNG-Maße), Exaktheit deckt `geometry.rs` ab.

## Nachweis-Logs (Xvfb :99, 1280x1024)

- `save_frame_under_xvfb` + `bad_model_file_fails_under_xvfb`: ok (ignored).
- Foto-E2E (`bus.jpg` via `feh --fullscreen`): `frame 1280x1024:
  5 detections`, PNG 1,6 MB (vs. 27 kB Schwarzbild).
- Dauerlauf headless (~25 s, debug): ~630 ms/Frame, kein Drift.
  Debug-Inferenz (~630–830 ms) sättigt den 5-fps-Pacer; Release: 118 ms
  bei 5 Detektionen — Live-Betrieb braucht `--release`.
- Modell-Cache: ort cached per SHA256 (`~/.cache/ort.pyke.io/models`,
  104 MB), mtime bei Re-Run unverändert, kein Re-Download.
- Final: `fmt --check`, `clippy --all-targets -D warnings`,
  `cargo test` (45 pass), `--release`-Tests (45 pass), `cargo upgrade`
  no-op (alles schon latest).

## Learnings

- Bei ort-2.x immer erst `SessionBuilder`-API in den Registry-Quellen
  pinnen (Beispiel nutzt Env-Helfer, 2.x nicht; URL braucht Feature).
- xcap-`capture_region` ist monitor-relativ mit internem Offset
  (`src/linux/capture.rs`) — der eigene Clamp muss dieselbe
  Konvention nutzen, sonst Off-by-Monitor-Bug.
- `cargo test` merged stdout beider Bin-Targets; `-- --ignored`
  braucht Display — lokale Gates strikt in offline (immer grün) und
  Xvfb (explizit) trennen.
- `git commit -- <pfad>` committet nur Gestagtes: neue Dateien immer
  erst `git add`en (S4/T1-Lektion).

## Mögliche Erweiterungen

- Frame-Tracking (Box-IDs über Frames), Video-Aufnahme (xcap
  `VideoRecorder`), Wayland-Pfad testen, GPU-Backend (CUDA/TensorRT
  via ort-Features), Label-Glyphen (Font-Dep), `--frames N` für
  begrenzte Headless-Läufe, TUI-Status (außer Prompt-Scope).

## Docker-Pakete (fürs Image vormerken)

Laufzeit/Build: `libegl-dev libx11-dev libxkbcommon-dev
libwayland-dev libpipewire-0.3-dev libclang-dev libgbm-dev
libdrm-dev pkg-config build-essential ca-certificates`.
Tests: `xvfb` (Pflicht), `feh` (nur manueller Foto-E2E).

## Commits

`8cb08ae` chore scaffold (S0) · `35bdd5c` capture (S1) · `1bd92fc`
infer (S2) · `b36316f` draw/view-Helfer (S3) · `a4f1b36` pipeline+E2E
(S4) · `bdef7fb` hardening (T1) · `e89b5c0` test-suites tracking.
