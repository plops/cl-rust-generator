# Implementierungsplan: Treemap-Disk-Visualisierer (wgpu + winit, Vulkan-only)

## 1. Ziel

Eigenständiges Linux-Programm in Rust (Edition 2024), das Datei- und
Verzeichnisgrößen als **squarified Treemap** visualisiert. Rendering via
**wgpu** (ausschließlich Vulkan-Backend), Fenster/Event-Handling via
**winit**. Absolut minimal: kleinstmögliche Dependency-Zahl, kleine
Binary, restriktive Feature-Flags in `Cargo.toml`.

Pflicht-Features:

- Rechtecke anzeigen (squarified Treemap).
- Mouse-Hover zeigt Größe + Verzeichnis-/Dateinamen des darunterliegenden
  Eintrags (On-Screen-Headerzeile **und** Fenster-Titel).

Nice-to-have (wird mit umgesetzt):

- Farbe kodiert den Dateityp (Extension-Mapping + Hash-Fallback).
- Cushion-Treemap via Fragment-Shader (parabolische Helligkeit + Kanten).

## 2. Kontext-Dateien

| Datei | Beschreibung |
|---|---|
| `/workspace/src/rs_disk_treemap/03_mvp_lisp/src/main.rs` | Referenz-MVP (macroquad): Scan-, Squarify-, Farb- und Hover-Logik zum Portieren |
| `plan/misc/wgpu_minimal_deps.md` | Recherche: `wgpu` mit `default-features=false`, nur `vulkan` + `wgsl` |
| `plan/misc/fensterverwaltung.md` | Recherche: winit als Fenster/Event-Standard, `ApplicationHandler` |
| `plan/misc/wgpu_glyph.md` | Recherche: GPU-Text-Rendering (Analyse, warum wir es **nicht** verwenden, s. u.) |
| `plan/misc/egui.md`, `eframe.md`, `iced.md`, `gpui-kit.md` | GUI-Alternativen, verworfen (zu viele Deps, zu große Binary) |
| `plan/misc/glam.md`, `nalgebra.md`, `magnitude.md` | Mathe-Crates, verworfen (eigene 20-Zeilen-Typen reichen) |
| `plan/misc/sysinfo.md`, `clash-verge-rev.md`, `repowiki.md`, `wgpu_algo.md` | Hintergrund, nicht direkt verwendet |
| `plan/20261001_01_treemap/prompt.txt` | Verbindlicher Projektauftrag (Teil 1: Qualitätsrahmen, Teil 2: Aufgabe) |

## 3. Dependency-Plan (alle via DeepWiki/MCP bzw. docs.rs geprüft)

| Crate | GitHub-Notation | Version | Features | Zweck |
|---|---|---|---|---|
| `wgpu` | `gfx-rs/wgpu` | 30.x (neueste) | `default-features=false`, `vulkan`, `wgsl` | Vulkan-Rendering, WGSL-Shader |
| `winit` | `rust-windowing/winit` | 0.30.x stable (neueste) | `default-features=false`, `x11` | Fenster, Maus-/Resize-Events |
| `font8x8` | `saibatizoku/font8x8-rs` (GitLab) | 0.3.x | keine | 8x8-Bitmap-Font, null transitive Deps |

Begründungen:

- **wgpu**: DeepWiki bestätigt `default-features=false` + `vulkan` + `wgsl`
  als minimalen Linux-Pfad. `gles`/`dx12`/`metal`/`webgpu` entfallen,
  dadurch kein `glow`/`glutin`-Stapel. Zusätzlich wird zur Laufzeit
  `Backends::VULKAN` erzwungen.
- **winit**: DeepWiki bestätigt `ApplicationHandler`-API (winit 0.30) und
  Default-Features `x11`/`wayland`. Wir aktivieren nur `x11` — das
  halbiert den Plattform-Code und genügt für X11/Xvfb. Bewusst **nicht**
  `0.31-beta`: wgpu 30 ist gegen die stabile rwh-0.6-Linie getestet.
- **font8x8**: `wgpu_glyph`/`glyphon` (s. `wgpu_glyph.md`) würden
  `glyph_brush`/`ab_glyph`/`ttf-parser` + Multithreading-Stack
  reinziehen — für eine einzige Hover-Zeile inakzeptabel. `font8x8`
  hat **null** transitive Abhängigkeiten und liefert `[u8; 8]`-Glyphs
  via `UnicodeFonts::get(char)`. DeepWiki hostet nur GitHub-Repos;
  `font8x8-rs` liegt auf GitLab, daher via docs.rs/crates.io verifiziert
  (Abweichung dokumentiert, „blind“ ist die Nutzung nicht).
- **Kein `pollster`**: `block_on` für die wgpu-Initialisierung wird als
  ~20-Zeilen-Executor mit `std::task` selbst geschrieben.
- **Kein Mathe-Crate**: `Rect`/Farben sind wenige Zeilen eigener Code.

Erwarteter Dependency-Graph: `cargo tree` sollte deutlich unter 200
Crates bleiben (wgpu-Vulkan-Kern + winit-X11).

## 4. Architektur-Entscheidungen

Module in Datenfluss-Reihenfolge (Nummernpräfix, 300-Zeilen-Regel):

```text
main.rs        Verdrahtung: CLI-Arg, EventLoop, Exit-Codes (keine Logik)
01_types.rs    Node, Rect, Rgb, format_bytes, Hover-Info
02_scan.rs     scan_tree/scan_entry: rekursiver Scan, Symlink-Skip,
               /proc-/sys-/dev-Skip, Fehler -> stderr
03_layout.rs   squarify: worst/layout_row/squarify (Port des MVP)
04_color.rs    color_for_path (Extension-Mapping), hash_color (Fallback)
05_text.rs     Atlas-Bau aus font8x8: ASCII-Grid -> RGBA, Char->UV-Mapping
06_render.rs   WgpuState: Instance/Device/Surface, 2 Pipelines
               (Rect + Text), Instanz-Buffer, block_on
07_app.rs      winit ApplicationHandler: Fenster, Hover-Picking,
               Header-Text, Resize, Scan-Thread-Anbindung
shaders/treemap.wgsl   Rechteck-Shader mit Cushion-Shading
shaders/text.wgsl      Text-Shader (Atlas-Sampling, Alpha-Blend)
```

Datenfluss:

```text
Scan-Thread --Node--> mpsc --> App --layout--> Instanzen --upload--> GPU
Maus-Position --pick--> Hover-Text --upload--> GPU + set_title
```

Kernentscheidungen:

1. **Zwei Pipelines, ein Render-Pass, zwei Draws**: Rechtecke (instanziierte
   Quads, Cushion-Fragment-Shader) und Text (instanziierte Glyphen-Quads
   über statischem Atlas). Kein Vertex-Buffer — Ecken werden im
   Vertex-Shader aus `vertex_index` erzeugt (DeepWiki-Muster).
2. **Pixel-Koordinaten + Uniform**: Instanzen tragen Pixel-Rechtecke; ein
   `screen_size`-Uniform rechnet nach NDC um. Vereinfacht Layout und
   Picking (identisches Koordinatensystem).
3. **On-Demand-Rendering**: `ControlFlow::Wait`, Redraw nur bei
   Resize/Hover/Scan-Fertig. KeinIdle-Stromverbrauch.
4. **Hover-Picking auf CPU**: flache Liste aller Rechtecke, Treffer =
   kleinstes enthaltendes Rechteck (tiefster Treemap-Knoten).
5. **Header-Leiste**: 36 px, Hintergrund-Rechteck + bis zu ~120 Glyphen
   (8x8, 2x skaliert). Text zusätzlich via `set_title` ins Fenster.
6. **Obergrenzen**: Rekursion nur für Rechtecke > 4 px (MVP-Regel),
   Instanz-Cap (65536) begrenzt GPU-Buffer.

## 5. Git-Strategie (Conventional Commits, atomar)

1. `docs(plan): add treemap implementation plan and task list`
2. `feat(scan): add scanner, squarified layout and filetype colors`
3. `feat(render): add vulkan-only wgpu renderer with cushion shader`
4. `feat(app): add winit event loop with hover picking and header text`
5. `docs(walkthrough): add german walkthrough with diagrams`

Vor jedem Commit: `cargo fmt --all`, `cargo clippy --all-targets
--all-features -- -D warnings`, `cargo test`. Es werden ausschließlich
eigene Dateien committet (keine fremden/ungestagten Artefakte).

## 6. Risiken & Gegenmaßnahmen

| Risiko | Maßnahme |
|---|---|
| wgpu-30-API driftet ggü. DeepWiki (0.20/0.25) | Signaturen an lokaler Registry-Quelle verifizieren, Compiler als Orakel |
| `winit` ohne Default-Features baut nicht | Schrittweise: erst `x11`, bei Fehlern gezielt ergänzen |
| Kein Display im Container | `xvfb` + llvmpipe-Vulkan für Smoke-Test installiert |
| Riesen-Verzeichnisse (>100k Dateien) | Instanz-Cap + 4-px-Rekursionsregel |
| `Surface::get_current_texture`-API (wgpu 29+) | Enum-Varianten aus Registry-Quelle übernehmen |
