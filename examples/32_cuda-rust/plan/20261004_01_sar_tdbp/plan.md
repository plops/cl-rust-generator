# Implementierungsplan: SAR Time-Domain Backprojection (`plan.md`)

Ziel: Range-komprimierte SAR-Simulation mit GPU-TDBP (`cuda-oxide`) und
interaktiver Macroquad-Visualisierung. Deutschsprachiges Abschlussdokument:
`walkthrough.md` (wird nach allen grünen Tests erstellt).

## 1. Relevante Dateien

Bestehend (lesen, nicht ändern — Referenz):

- `my_first_kernel/src/main.rs` — verifiziertes Minimal-Template (1D-`#[kernel]`,
  `LaunchConfig1D`, `prepare_*` + `PreparedLaunch`, `DeviceBuffer`-Roundtrip;
  `PASSED` auf RTX A4000).
- `my_first_kernel/rust-toolchain.toml` — `nightly-2026-08-28` + Komponenten
  (`rust-src`, `rustc-dev`, `llvm-tools`, …). Wird kopiert.
- `my_first_kernel/Cargo.lock` — pinnt `NVlabs/cuda-oxide` auf Rev `a0cc6cc…`.
- `plan/20261004_01_sar_tdbp/prompt.txt` — Aufgabenstellung (verbindlich).
- `plan/20261004_01_sar_tdbp/deps.md` — Abhängigkeitstabelle.

Neu (Crate `sar_tdbp/`, Edition 2024, `cargo oxide`-Projekt):

- `sar_tdbp/Cargo.toml` — siehe deps.md; `[workspace]` (isoliert wie Template).
- `sar_tdbp/rust-toolchain.toml` — Kopie des Templates.
- `sar_tdbp/src/lib.rs` — nur `mod 01_…`-Deklarationen + Re-Exports.
- `sar_tdbp/src/main.rs` — nur CLI-Parsing + Dispatch (`--gui` / `--headless`).
- `sar_tdbp/src/01_types.rs` — `Complex32` (`repr(C)`, `DeviceCopy`),
  `Vec3`, `RadarParams` (f0, B, c, lambda), `SceneGeometry`
  (Bildgröße, Pixelraster, Pulspositionen), dB/Colormap-Hilfen (reine Fns).
- `sar_tdbp/src/02_phantom.rs` — `PointTarget`, Szenarien: `single_point()`
  (PSF-Nachweis), `grid_5x5()`, `rust_text()` (Schriftzug aus Punkten).
- `sar_tdbp/src/03_simulator.rs` — Vorwärtssimulation (CPU): range-komprimierte
  Echos `s(p,t) = Σ σ·sinc(B·(t−τ))·exp(−j·2π·f0·τ)` als
  `[num_pulses × num_samples]`-Matrix, Pulspositionen, Zeitachse `(t0, dt)`.
- `sar_tdbp/src/04_kernel.rs` — `#[cuda_module] mod kernels` mit `tdbp`-Kernel
  (1D-Grid über Pixel, `DisjointSlice<Complex32>`, `pulse_limit`-Parameter für
  inkrementelle Apertur) + `#[device]`-Helfer + CPU-Referenz `tdbp_cpu()`.
- `sar_tdbp/src/05_pipeline.rs` — Host-Orchestrierung: `SarPipeline::new()`
  (Kontext, Stream, Upload), `run(pulse_limit) -> Vec<Complex32>` (Kernel-Launch
  + Download), Fehler-Typ.
- `sar_tdbp/src/06_gui.rs` — Macroquad-App: dB-Bild + Turbo-Colormap,
  Pfeiltasten variieren `pulse_limit` (Re-Fokussierung live), Mausklick zeigt
  Range-/Azimuth-Schnitt, `--frames N --screenshot out.png` für xvfb-Tests.
- `sar_tdbp/tests/*.rs` — GPU-Integrationstests (Kernel ≡ CPU-Referenz,
  Puls-Limit-Monotonie); Unit-Tests liegen in den Modulen (Host-only).

## 2. Architektur

```text
Phantom (02) ──► Vorwärtssim (03, CPU) ──► raw[p,s], plat[p], (t0,dt)
                                                    │ 05_pipeline
                                                    ▼
GUI (06) ◄── dB+Turbo ◄── Vec<Complex32> ◄── TDBP (04, GPU) ◄── Upload
  │                                                        ▲
  └── pulse_limit ─────────────────────────────────────────┘
```

- Ein Thread pro Pixel (1D-Grid, `LaunchConfig1D`), kein Shared Memory nötig:
  TDBP ist „embarrassingly parallel“ mit lesendem Zugriff auf `raw`/`plat`.
- Inkrementelle Apertur ohne Re-Upload: `pulse_limit` begrenzt die
  Puls-Schleife im Kernel; GUI ruft `run(limit)` erneut auf.
- CPU-Referenz `tdbp_cpu()` teilt sich die Interpolations-/Phasen-Formeln als
  reine Funktionen mit dem Kernel (Bit-Nähe für Test-Toleranzen).

## 3. Datenstrukturen

```rust
#[repr(C)] #[derive(Clone, Copy, DeviceCopy)]
pub struct Complex32 { pub re: f32, pub im: f32 }

#[repr(C)] #[derive(Clone, Copy, DeviceCopy)]
pub struct Vec3 { pub x: f32, pub y: f32, pub z: f32 }

pub struct RadarParams { pub f0: f32, pub bandwidth: f32 } // c, λ abgeleitet
pub struct SceneGeometry {
    pub width: u32, pub height: u32,      // Pixel
    pub x0: f32, pub y0: f32,             // Szenen-Ursprung (m)
    pub dx: f32, pub dy: f32,             // Pixelabstand (m)
    pub platform_height: f32, pub aperture_len: f32, pub num_pulses: u32,
}
```

Kernel-Skizze (volle Signatur in `04_kernel.rs`):

```rust
#[kernel]
#[launch_bounds(256)]
#[launch_contract(domain = 1, block = (256, 1, 1))]
pub fn tdbp(raw: &[Complex32], plat: &[Vec3],
            mut out: DisjointSlice<Complex32>, width: u32, /* … */ pulse_limit: u32) {
    let idx = thread::index_1d().get() as u32;
    let (px, py) = (idx % width, idx / width);
    if px >= width || py >= height { return; }
    let mut acc = Complex32::zero();
    for p in 0..pulse_limit.min(num_pulses) {
        let d = dist(plat[p], pixel_pos(px, py));
        let s = interp(raw, p, (2.0 * d / C - t0) / dt);
        acc += s * matched(4.0 * PI * d / LAMBDA);
    }
    if let Some(o) = out.get_mut(thread::index_1d()) { *o = acc; }
}
```

Physik-Default: X-Band f0 = 10 GHz (λ = 3 cm), B = 300 MHz
(ΔR = 0,5 m), h = 100 m, Szene 40×40 m @ 256×256 im Seitenblick
(Szenenmitte 60 m neben der Flugbahn — reines Nadir hätte keine
Boden-Range-Auflösung), 1024 Pulse (Azimuth-Abtasttheorem: Gitterkeulen
außerhalb der Szene). Details und Messwerte in `walkthrough.md`.

## 4. Commit-Regel

**Conventional Commits** mit ausführlicher Beschreibung, z. B.
`feat(sim): range-komprimierte Echo-Simulation` + Body (Was/Warum/Tests).
Ein Commit pro `task.md`-Phase, nur bei grünen Tests der Phase.
