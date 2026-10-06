# Aufgabenliste: GPU-SPH-Fluidsimulation (35_sph)

Sequentieller Plan: Modellierung → GPU-Kernels → Spatial Hash → Renderer →
Interaktion → Benchmarks. Jeder Schritt endet mit einem verifizierten
Conventional Commit (siehe `plan.md`, Abschnitt B.6).

## Phase 0 – Gerüst (docs/chore)

- [x] 0.1 Umgebung verifizieren (`cargo oxide doctor`, vecadd-Probe PASSED)
- [x] 0.2 `plan.md` Teil B, `task.md`, `deps.md` schreiben
- [x] 0.3 Cargo-Gerüst: `Cargo.toml`, `rust-toolchain.toml`, `.gitignore`,
      `src/lib.rs`, `src/main.rs` (nur Verdrahtung)

## Phase 1 – Modellierung (CPU-Basis, testbar ohne GPU)

- [x] 1.1 `01_types.rs`: `Particle`, `SphParams`, `GridMeta` (Pod/Zeroable)
- [x] 1.2 `02_params.rs`: `SimConfig`-Defaults + lexopt-CLI
      (`--headless --steps --particles --substeps`)
- [x] 1.3 `03_sph_math.rs`: Poly6, Spiky-Gradient, Viskositäts-Laplacian als
      gerätekompatible reine Funktionen + Unit-Tests (analytische Werte)
- [x] 1.4 `04_spatial_grid.rs`: Hash-Funktion + CPU-Counting-Sort +
      Nachbarschafts-Iterator + Tests

## Phase 2 – GPU-Kernels (cuda-oxide)

- [x] 2.1 `05_gpu_kernels.rs`: `#[cuda_module]` mit `k_hash`, `k_scan`,
      `k_reorder` (Voll-GPU Uniform Grid mit `DeviceAtomicU32`)
- [x] 2.2 `05_gpu_kernels.rs`: `k_density`, `k_force` (Poly6/Spiky/Viskosität)
- [x] 2.3 `05_gpu_kernels.rs`: `k_integrate` (Symplectic Euler, Wände,
      Obstacle, Maus-Interaktion als uniforme Parameter)
- [x] 2.4 `06_backend.rs`: `Backend`-Trait + `GpuBackend` (Kontext, SoA-Buffer,
      Launch-Sequenz mit Blockgröße 256, Download für Renderer)

## Phase 3 – CPU-Fallback + Headless

- [x] 3.1 `06_backend.rs`: `CpuBackend` (gleiche Mathematik, Zelle-Grid)
- [x] 3.2 `09_headless.rs`: Headless-Runner mit NaN/Inf- und
      Wand-Validierung, Ausgabe ms/FPS/Particles-s
- [x] 3.3 GPU-Headless-Lauf: 500 Schritte ohne Tunneln/Explosion
      (`cargo oxide run -- --headless --steps 500`)

## Phase 4 – Renderer + Interaktion (macroquad)

- [x] 4.1 `07_renderer.rs`: Partikel (Farbmodi Dichte/Geschwindigkeit),
      Obstacle, HUD-Text
- [x] 4.2 `08_app.rs`: Dam-Break-Init, Sub-Steps, Linksklick-Wirbel,
      Rechtsklick-Strahl, Tasten R/Space/G/C, Pause/Step
- [x] 4.3 GUI-Smoke-Test mit `DISPLAY=:0` (Start + Frames belegen)

## Phase 5 – Tests, Benchmarks, Gates

- [x] 5.1 `tests/sph_math.rs`: Kernel gegen analytische Werte
- [x] 5.2 `tests/spatial_grid.rs`: Hash/Nachbarschaft
- [x] 5.3 `tests/cpu_stability.rs`: 500 CPU-Steps, kein Tunneln
- [x] 5.4 Benchmark: CPU-Basis + GPU-Durchsatz auf RTX A4000
      (`--headless --steps 2000 --bench`)
- [x] 5.5 Gates grün: `cargo fmt --check`,
      `cargo clippy --all-targets -- -D warnings`, `cargo test`

## Phase 6 – Abschluss

- [x] 6.1 `walkthrough.md` (deutsch, Mermaid, Code-Beispiele, Learnings,
      Dockerfile-Pakete) im Plan-Verzeichnis
- [x] 6.2 `plan_effort.md` (Tokens, Turns, Subagents) im Plan-Verzeichnis
- [x] 6.3 Finale Commit-Hygiene: `git log` prüfen, nur grüne Stände
