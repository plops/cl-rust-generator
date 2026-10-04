# Aufgabenliste (`task.md`)

Seriell abarbeiten. Jeder Schritt: implementieren → Tests schreiben →
`cargo oxide test` (bzw. `cargo test` für reine Host-Phasen) grün →
`cargo fmt` → Commit. Kein Schritt ohne grüne Tests.

## Phase 1 — Scaffold + Typen + Phantom

1. `sar_tdbp/`-Crate anlegen (Edition 2024, `[workspace]`, Toolchain-Kopie,
   `cuda-*`-Git-Deps mit gepinnter Rev, `macroquad`, `image/png`).
   `cargo oxide doctor` + `cargo oxide build` erfolgreich.
2. `01_types.rs`: `Complex32` (+ Add/Mul/Norm), `Vec3`, `RadarParams`,
   `SceneGeometry` (+ `pixel_pos`, `pulse_pos`). Unit-Tests: Arithmetik,
   Geometrie-Raster (Ecken, Mitte).
3. `02_phantom.rs`: `single_point()`, `grid_5x5()`, `rust_text()` (5×7-Font,
   Wort „RUST“). Unit-Tests: Anzahlen, Bounds, Zentrierung.

## Phase 2 — Vorwärtssimulation + CPU-Referenz

4. `03_simulator.rs`: `simulate()` → `RawData { samples, num_pulses,
   num_samples, t0, dt }`. Unit-Tests: Matrix-Dims; Peak-Sample ≈ 2R/c
   (Single-Point); Phase `−2πf0τ` am Peak; leeres Phantom → Nullsignal.
5. `04_kernel.rs` (Host-Teil): `tdbp_cpu()` inkl. `interpolate()` + Matched
   Filter als reine Fns. Unit-Tests: Single-Point fokussiert genau am
   Streuer-Pixel (Peak-Lage ≤ 1 px); PSF-3-dB-Breite ≈ Theorie
   (Boden-Range ≈ ΔR·R/y_c im Seitenblick, Azimuth ≈ λR/2L) mit
   ±40 % Toleranz; Sidelobes < −8 dB. ✅ (Seitenblick statt Nadir:
   Nadir hat keine Boden-Range-Auflösung, siehe walkthrough.md)

## Phase 3 — GPU-Kernel + Pipeline

6. `04_kernel.rs` (Device-Teil): `#[cuda_module]`, `tdbp`-Kernel,
   `launch_contract`. Test: `cargo oxide inspect` zeigt PTX ohne Fehler.
7. `05_pipeline.rs`: `SarPipeline::{new, run(pulse_limit)}`.
   Integrationstests (GPU): 32×32-Szene, 16 Pulse — max. Abweichung
   Kernel vs. `tdbp_cpu` < 1e-3 relativ; `pulse_limit`-Monotonie
   (Peak wächst mit Limit); Full-Apertur-Peaklage ≡ CPU.
8. `main.rs`/`lib.rs` + `--headless out.png`: PNG-Export + PSF-Kennzahlen
   auf stdout. Test: läuft auf GPU, PNG existiert, Peak-Kennzahl plausibel.

## Phase 4 — Macroquad-UI + Headless-GUI-Test

9. `06_gui.rs`: dB-Bild + Turbo-Colormap, `→/←` ändert `pulse_limit`
   (Re-Launch), Klick → Range-/Azimuth-Profil-Overlay, Legende.
   Unit-Tests (host-only): dB-Skalierung, Colormap-Endpunkte, Profil-Extrakt.
10. `--frames N --screenshot`: rendert N Frames, speichert PNG, exit 0.
    Test unter `xvfb-run`: Screenshot existiert, Bild ungleich einfarbig
    (Varianz > 0), Log enthält Apertur-Stand.

## Phase 5 — Abschluss

11. `cargo fmt --check`, `cargo clippy` (Warnungen = 0, `cargo oxide`
    -Build eingeschlossen), `cargo upgrade --dry-run`-Sichtung,
    Voll-Lauf aller Tests. `walkthrough.md` (Deutsch, Mermaid, Snippets,
    Dockerfile-Paketliste) schreiben.
