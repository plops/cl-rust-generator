# sar_tdbp — SAR Time-Domain Backprojection mit cuda-oxide

Range-komprimierte SAR-Simulation (Phantom → Echos → GPU-TDBP → Bild),
interaktiv visualisiert mit Macroquad. Getestet auf NVIDIA RTX A4000,
CUDA 13.4, `nightly-2026-08-28`.

## Bauen & Testen

```bash
cargo oxide build          # Host + Device (PTX) bauen
cargo oxide test           # alle 28 Tests (Host + GPU + xvfb-GUI)
cargo oxide inspect        # generiertes PTX ansehen
cargo clippy --all-targets # 0 Warnungen
cargo fmt -- --check
```

## Benutzung

```bash
# Headless: rechnen, PNG + Kennzahlen + ASCII-Vorschau
cargo oxide run -- --headless bild.png --phantom rust
cargo oxide run -- --headless bild.png --phantom grid --size 128 --pulses 512 --limit 64

# Interaktiv: ←/→ Apertur, R = voll, Klick = Schnitte, Esc = Ende
cargo oxide run -- --phantom rust

# Benchmark: CPU (1/2/4/8/alle Stränge) vs. GPU, feste Last
cargo oxide run -- --bench

# GUI-Headless (Tests/CI): N Frames, Screenshot, Exit 0
xvfb-run -a ./target/release/sar_tdbp --frames 5 --screenshot shot.png
```

Optionen: `--phantom single|grid|rust`, `--size N`, `--pulses N`,
`--limit K`, `--headless OUT.png`, `--frames N --screenshot F`, `--help`.

## Module (Datenfluss-Reihenfolge)

`01_types` → `02_phantom` → `03_simulator` → `04_kernel` → `05_pipeline` →
`06_gui`. Details: `../plan/20261004_01_sar_tdbp/{plan,task,deps,walkthrough}.md`.
