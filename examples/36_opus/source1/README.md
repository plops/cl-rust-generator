# `opus-vis` — Opus-Dateien visualisieren

Liest `.opus` (Ogg), zeigt pro Paket Modus (SILK/Hybrid/CELT),
Bandbreite und Bitrate, rendert einen Spektrum-Wasserfall als PNG.

```sh
cargo run --release -- in.opus -o out.png --json out.json
cargo run --release --features show -- in.opus --show
```

Details: `../plan/20261011_01_init/plan.md`, Abhaengigkeiten: `deps.md`.
