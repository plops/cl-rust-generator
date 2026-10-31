# `opus-vis` (source2) — Opus-Dateien bis zum letzten Bit verstehen

Liest `.opus` (Ogg) und erklaert die Datei als deutschen **Text-Report**,
als **Wasserfall-PNG** (plus X11-Viewer) und als **JSON (Schema 3)**.

Neu gegenueber `source1`: Der Bitstrom-Walk liest *jedes* Bit jedes
Pakets in Decoder-Reihenfolge — SILK (mono/stereo, FEC/LBRR, 10–120 ms),
CELT (inkl. PVQ-Banddaten, Anti-Collapse, Finalise), Hybrid und die
Redundanz-Frames bei Moduswechseln. Beweis: Der Range-Coder-Endzustand
(`final_range`) stimmt fuer jedes Paket mit opus-pure, libopus und dem
libopus-Encoder ueberein. Daraus folgt das **Bit-Budget**: wohin jedes
Bit ging (Budget-Lane im Bild, `budget_bits` im JSON, `bits:` im Report).

```sh
cargo run --release -- song.opus -o song.png             # Report + PNG
cargo run --release -- song.opus -o d.png --view dichte  # gelesene Bits je Bin
cargo run --release -- song.opus --json song.json --quiet
cargo run --release --features show -- song.opus --show  # Viewer
cargo run --release --example oracle -- *.opus           # Orakel je Datei
python3 scripts/libopus_oracle.py *.opus                 # libopus-Gegenprobe
scripts/mkfixtures.sh                                    # Real-Daten-Matrix
scripts/smoke.sh                                         # alles inkl. HIL
```

Ausfuehrliche Bedienung und Lese-Anleitung: **`HANDBUCH.md`**.
Review, Plan und Walkthrough: `../plan/20261031_07_review/`.
Abhaengigkeiten: `deps.md`. Module (Datenfluss 01→13): `src/lib.rs`.
