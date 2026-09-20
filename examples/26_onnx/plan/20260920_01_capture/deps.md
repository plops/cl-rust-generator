# deps.md — 20260920_01_capture

Registry für DeepWiki-Abfragen: GitHub-Notation `<organization>/<projekt>`.
Nur direkt genutzte Deps (transitive nur wenn Abfrage-relevant).
Stand: 2026-09-20, final nach `cargo upgrade` in T2 (no-op — alle schon latest).

| Crate/System | Org/Projekt bzw. Paket | Version (Stand) | Zweck |
|---|---|---|---|
| ort | pykeio/ort | 2.0.0-rc.13 (RC — Drift einplanen) | YOLOv8-Session, `inputs!`, `TensorRef`, `output0`-Decode |
| xcap | nashaofu/xcap | 0.9.8 (docs.rs-Build defekt — API in S0 per `cargo doc` pinnen) | X11-Regions-Screenshot |
| pixels | parasyte/pixels | 0.17.2 | GPU-Framebuffer fürs Anzeige-Fenster |
| winit | rust-windowing/winit | 0.30 (via pixels-Beispiel), minimal: `default-features=false, features=[rwh_06, x11]` | Fenster/Event-Loop (mit pixels eingeführt; Wayland-Stack auf unserer Seite abgewählt) |
| clap | clap-rs/clap | 4.6.7 (derive) | CLI (`--x/--y/--w/--h/--zoom/--fps/…`) |
| image | image-rs/image | 0.25+ (via ort-Beispiel/xcap-Dep), minimal: `default-features=false, features=[png]` (wie xcap; spart u. a. rav1e) | Preprocess, `--save-frame`-PNG |
| ndarray | rust-ndarray/ndarray | 0.17 (via ort-Beispiel) | `(1,3,640,640)`-Input-Tensor |
| anyhow | dtolnay/anyhow | 1.x | Fehler-Propagierung CLI/Pipeline |
| Xvfb | system (apt: `xvfb`) | system | Test-Display für Capture-/E2E-Smoke (Task S0/S4) |
| ca-certificates | system (apt) | system | Modell-Download (HTTPS-CDN) in S4 |
| pkg-config, build-essential | system (apt) | system | Sys-Builds (xcb/wgpu-Header je nach Befund S0) |

DeepWiki-Abfrage-Muster: `pykeio/ort` (Session-/Input-API, YOLOv8-Beispiel),
`nashaofu/xcap` (Monitor-/Regions-API), `parasyte/pixels` (Framebuffer-/Resize-API),
`clap-rs/clap` (derive-CLI). Repo-Kontext: `plops/cl-rust-generator`
(Plan-/Task-Stil, `source0`-Konvention).
NICHT eingeführt (bewusst): `show_image` + `raqote` (Referenz-Display, ersetzt durch
`pixels`-Rasterizer), `rand` (kein Bedarf), Video-/Tracking-Crates (außer Scope).

## source1-Variante (`examples/26_onnx/source1/`, x11rb + macroquad statt xcap + pixels)

Gleiche CLI/Inferenz/Overlay (`01_cli`, `03_infer`, `04_draw` kopiert),
Capture via GetImage-ZPixmap, Display via miniquad-Textur. 128 statt
456 Crates, kein wgpu/pipewire/wayland-Stack, keine EGL-Header zum
Bauen; Laufzeit braucht nur X11-Client-Libs (u. a. `libxi6`, von
miniquad per dlopen geladen). PNGs byte-identisch zu source0
(md5-verifiziert auf bus.jpg, je 5 Detektionen).

| Crate/System | Org/Projekt bzw. Paket | Version | Zweck |
|---|---|---|---|
| x11rb | psychon/x11rb | 0.14.0 | Reines-Rust-X11 (connect, GetImage ZPixmap) statt xcap |
| macroquad | not-fl3/macroquad | 0.4.16 | Fenster + Textur-Blit statt pixels/wgpu (`Window::from_config`, `Texture2D::update`) |
| libxi6 | system (apt, Laufzeit) | system | miniquad-dlopen unter X11 |
