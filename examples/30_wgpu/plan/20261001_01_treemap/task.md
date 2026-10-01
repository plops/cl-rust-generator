# Aufgabenliste: Treemap-Disk-Visualisierer (wgpu + winit)

Legende: `- [ ]` offen, `- [x]` erledigt. Serielle Abarbeitung.

## Phase 0: Recherche & Planung

- [x] DeepWiki-Recherche `gfx-rs/wgpu` (minimale Vulkan-Features, Instance/Device/Surface)
- [x] DeepWiki-Recherche `gfx-rs/wgpu` (Render-Pipeline, Surface-Configure, Submit)
- [x] DeepWiki-Recherche `rust-windowing/winit` (`ApplicationHandler`, Events, Features)
- [x] `font8x8` via docs.rs/crates.io verifizieren (GitLab-Hosting, kein DeepWiki möglich)
- [x] Toolchain prüfen (rustc 1.99, cargo-edit), Vulkan-Treiber + xvfb installieren
- [x] `plan.md` schreiben
- [x] `task.md` schreiben (diese Datei)
- [x] `deps.md` schreiben

## Phase 1: Typen, Scan, Layout, Farbe

- [x] API-/Typ-Definition: `Node`, `Rect`, `Rgb`, `format_bytes` (`01_types.rs`)
- [x] Implementierung: `scan_tree`/`scan_entry` (`02_scan.rs`, Port des macroquad-MVP)
- [x] Implementierung: `worst`/`layout_row`/`squarify` + `pick` (`03_layout.rs`, Port des MVP)
- [x] Implementierung: `color_for_path`/`hash_color` (`04_color.rs`, Port des MVP)
- [x] Unit-Tests: `format_bytes`, Scan auf Fixture-Baum, Flächen-Erhaltung, Farb-Mapping (13 Tests grün)
- [x] `cargo fmt`, `cargo clippy --all-targets --all-features -- -D warnings`, `cargo test`
- [x] Commit `feat(scan): add scanner, squarified layout and filetype colors`

## Phase 2: Text-Atlas & wgpu-Renderer

- [ ] API-/Typ-Definition: Atlas-Layout, Instanz-Formate, Uniform (`05_text.rs`, `06_render.rs`)
- [ ] Implementierung: font8x8-Atlas (`ASCII 32..127`, 16x6-Grid, RGBA-Textur)
- [ ] Implementierung: WGSL-Shader (`treemap.wgsl` mit Cushion-Shading, `text.wgsl`)
- [ ] Implementierung: `WgpuState` (Vulkan-Instance, Device, Surface, Pipelines, Buffer)
- [ ] Implementierung: eigener `block_on`-Mini-Executor (kein `pollster`)
- [ ] Unit-Tests: Atlas-Maße/Glyphen-Bits, Instanz-Aufbau, `block_on`
- [ ] `cargo fmt`, Clippy (`-D warnings`), `cargo test`
- [ ] Commit `feat(render): add vulkan-only wgpu renderer with cushion shader`

## Phase 3: winit-App & Verdrahtung

- [ ] API-/Typ-Definition: `App`-Zustand, Hover-Picking, Header-Text (`07_app.rs`, `main.rs`)
- [ ] Implementierung: `ApplicationHandler` (resumed/window_event/about_to_wait)
- [ ] Implementierung: Hover-Picking (kleinstes enthaltendes Rechteck), `set_title`
- [ ] Implementierung: Scan-Thread + mpsc, Resize-Relayout, On-Demand-Redraw
- [ ] Implementierung: `main.rs` (CLI-Arg, EventLoop, Exit-Codes)
- [ ] Unit-/Integrationstests: Picking-Logik, Header-Trunkierung, Scan→Layout-Pipeline
- [ ] `cargo fmt`, Clippy (`-D warnings`), `cargo test`
- [ ] Commit `feat(app): add winit event loop with hover picking and header text`

## Phase 4: Verifikation & Abschluss

- [ ] `cargo upgrade` (neueste Versionen prüfen), `cargo tree`-Kontrolle (Dep-Zahl)
- [ ] Release-Build + Binary-Größe messen (`strip`, Größe dokumentieren)
- [ ] Smoke-Test unter `xvfb` mit llvmpipe-Vulkan (Screenshot/Exit-Code)
- [ ] `walkthrough.md` schreiben (Deutsch, Mermaid, feste Gliederung)
- [ ] Commit `docs(walkthrough): add german walkthrough with diagrams`
