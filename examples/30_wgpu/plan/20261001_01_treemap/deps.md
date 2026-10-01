# Abhängigkeiten (`deps.md`)

Jede Crate in GitHub-Notation `<owner>/<repo>`, mit Einsatzzweck und den
via DeepWiki (bzw. dokumentierter Ausnahme) ermittelten Eigenheiten.

## `gfx-rs/wgpu` — Vulkan-Rendering

```toml
wgpu = { version = "30", default-features = false, features = ["vulkan", "wgsl"] }
```

- **Einsatzzweck**: gesamtes Rendering (Treemap-Rechtecke + Text-Header)
  über genau ein Vulkan-Device, eine Surface, zwei Render-Pipelines.
- **DeepWiki-Erkenntnisse**:
  - Minimale Linux-Einbindung: `default-features = false` plus
    `["vulkan", "wgsl"]`; vermeidet den GLES-Stapel (`glow`, `glutin`,
    `khronos-egl`) und WebGPU/WASM-Abhängigkeiten.
  - `Instance::new` mit `backends: Backends::VULKAN` erzwingt Vulkan
    auch zur Laufzeit; `PowerPreference::HighPerformance` für die
    Adapter-Wahl.
  - `Surface::configure(&device, &SurfaceConfiguration)` initialisiert
    die Swapchain; `Surface::get_current_texture()` liefert seit
    wgpu 29 ein `CurrentSurfaceTexture`-Enum (Zustände matchen!).
  - Vertex-Shader erzeugen Quad-Ecken ohne Vertex-Buffer direkt aus
    `@builtin(vertex_index)` — Muster für beide Pipelines.
  - `Queue::submit(iterable)` nimmt Command-Buffer entgegen und liefert
    einen `SubmissionIndex`.
- **Version**: 30.0.1 (neueste stabile).

## `rust-windowing/winit` — Fenster & Events

```toml
winit = { version = "0.30", default-features = false, features = ["x11"] }
```

- **Einsatzzweck**: Fenster-Erstellung, Maus-Hover (`CursorMoved`),
  Resize, Redraw-Scheduling, Fenster-Titel als Hover-Anzeige.
- **DeepWiki-Erkenntnisse**:
  - Seit 0.30: `ApplicationHandler`-Trait statt Closure-`run()`; Fenster
    wird in `resumed()` via `ActiveEventLoop::create_window` erzeugt.
  - Events in `window_event()`: `CloseRequested`, `Resized`,
    `CursorMoved`, `RedrawRequested`, `KeyboardInput`.
  - Default-Features sind `x11` + `wayland (+dlopen, +csd-adwaita)`;
    wir aktivieren bewusst nur `x11` (X11/Xvfb-Testbarkeit, ~halber
    Plattform-Code, kleinere Binary).
  - `EventLoop::run_app(&mut handler)` startet die Schleife;
    `ControlFlow::Wait` + `request_redraw()` = On-Demand-Rendering.
- **Version**: 0.30.13 (neueste **stabile**; `0.31-beta` bewusst
  ausgeschlossen, da wgpu 30 gegen die stabile rwh-0.6-Linie getestet
  ist — dokumentierte Ausnahme von „absolut neueste Version“).

## `saibatizoku/font8x8-rs` — Bitmap-Font (Ausnahme: GitLab)

```toml
font8x8 = "0.3"
```

- **Einsatzzweck**: einzige Text-Quelle für die Hover-Headerzeile.
  95 druckbare ASCII-Glyphen werden einmalig in eine RGBA-Atlas-Textur
  (16×6-Grid aus 8×8-Zellen) gerastert; pro Zeichen eine instanziierte
  Quad-Instanz.
- **Recherche (kein DeepWiki möglich)**: Das Repo liegt auf GitLab
  (`gitlab.com/saibatizoku/font8x8-rs`), DeepWiki indexiert nur GitHub.
  Stattdessen via docs.rs 0.3.1 verifiziert:
  - `use font8x8::{BASIC_FONTS, UnicodeFonts};`
  - `BASIC_FONTS.get('A') -> Option<[u8; 8]>` (1 Byte = 1 Zeile,
    MSB = links).
  - **Null** transitive Abhängigkeiten (siehe crates.io-Dependencies:
    leer), nur Konstanten + Trait — ideal für das Minimal-Ziel.
- **Verworfen stattdessen**: `wgpu_glyph`/`glyphon` (ziehen
  `glyph_brush`/`ab_glyph`/`ttf-parser` + Threading-Stack für eine
  einzige Textzeile — widerspricht dem Minimal-Auftrag).
- **Version**: 0.3.1 (neueste).

## Bewusst NICHT eingeführt

| Kandidat | Grund |
|---|---|
| `pollster` | `block_on` als ~20-Zeilen-`std::task`-Executor selbst geschrieben |
| `glam` / `nalgebra` | `Rect`/`Rgb` sind Trivialtypen; eigene Definition |
| `egui` / `eframe` / `iced` | Vollständige GUI-Stacks: hunderte Deps, große Binary |
| `wgpu_glyph` / `glyphon` | Font-Shaping-Stacks für eine Hover-Zeile überdimensioniert |
| `tracing` / `log` / `env_logger` | `eprintln!` genügt für Skip-Meldungen und Status |
| `clap` | Genau ein optionales Positions-Argument: `std::env::args` reicht |
