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

## source1: x11rb + macroquad (schlanke Zweitimplementierung)

Motivation: source0 zieht 456 Crates (xcap: pipewire/Wayland-Stack
inkl. System-Libs; pixels: wgpu). source1 (`examples/26_onnx/source1/`,
Bin `x11_rb_mq_viewer`) ersetzt nur Capture und Display und landet bei
**128 Crates** — kein wgpu/pipewire/wayland/rav1e, keine EGL-Header zum
Bauen, Laufzeit nur X11-Client-Libs (`libxi6`, von miniquad per dlopen).

Architektur (Datenfluss identisch zu source0):

```
CLI → x11rb-Capture → ort-Inferenz → CPU-Overlay → macroquad-Textur
```

- `01_cli.rs`, `03_infer.rs`, `04_draw.rs`: aus source0 kopiert
  (bewusst kein Pfad-Dep — sonst würden xcap/pixels/wgpu mitgebaut).
- `02_capture.rs` (neu): `x11rb::connect` → X-Screen per `--monitor`,
  Region-Clamp (gleiche Semantik wie source0), `GetImage`-ZPixmap aufs
  Root-Window, BGRX→RGB. x11rb-Funde: `ImageFormat::Z_PIXMAP` ist eine
  Konstante (kein Enum), `Drawable`/`Window` sind `u32`-Aliase,
  `setup()` leiht (Werte vor dem Move kopieren).
- `05_view.rs` (neu): reine Helfer (Zoom/Blit/PNG/Pacer) wie source0,
  plus `run_window_mq` — pro Tick `pump`, `Texture2D::update`,
  `draw_texture_ex` mit Zoom-`dest_size`, `FilterMode::Nearest`.
- `main.rs`: plain `fn main` (kein macroquad-Attribut, damit
  `--headless`/`--save-frame` fensterlos bleiben); Fenster via
  `macroquad::Window::from_config` (Groesse kommt aus den Flags).

Nachweise: `fmt`, `clippy -D warnings`, 44 Tests + 2 ignored E2E
grün unter Xvfb; bus.jpg: 5 Detektionen, PNG **md5-identisch** zu
source0 (BGRX-Reihenfolge damit pixel-exakt bewiesen); macroquad-
Fenster pumpt Frames unter Xvfb.

Release-Lauf des Users (Screen 1920x1200, Region 1024x512, 3 fps):

```
$ target/release/x11_rb_mq_viewer --w 1024 --h 512 --fps 3
capture: screen 0 (1920x1200), region (0, 0, 1024, 512)
model: https://cdn.pyke.io/0/pyke:ort-rs/example-models@0.0.0/yolov8m.onnx
view: 1024x512 @ 3 fps
frame 1024x512: 0 detections, infer 278.1 ms
frame 1024x512: 0 detections, infer 251.8 ms
...
frame 1024x512: 0 detections, infer 248.0 ms
frame 1024x512: 1 detections, infer 247.7 ms
```

Lesart: Release-Inferenz stabil bei ~247 ms (erster Frame 278 ms —
Session-Warmup), Pacer-Ziel 333 ms bei 3 fps wird eingehalten; die
späte Detektion zeigt, dass Overlay + Timing auch bei Treffern stabil
bleiben. Größere Regionen skalieren linear über Preprocess/Inferenz;
wer schneller will, braucht kleinere `--w/--h` oder ein kleineres
Modell (yolov8n).

## Commits

`8cb08ae` chore scaffold (S0) · `35bdd5c` capture (S1) · `1bd92fc`
infer (S2) · `b36316f` draw/view-Helfer (S3) · `a4f1b36` pipeline+E2E
(S4) · `bdef7fb` hardening (T1) · `e89b5c0` test-suites tracking ·
`2c2ed56` walkthrough (T2) · `5374ee6` slimming + `setup_release_min.sh`
· `581b927` release-script-fixes · `744db49` source1-Variante.
Diese Sektion: Folge-Commit.
