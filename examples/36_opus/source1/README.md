# `opus-vis` — Opus-Dateien visualisieren

Liest `.opus` (Ogg), zeigt pro Paket Modus (SILK/Hybrid/CELT),
Bandbreite und Bitrate, rendert einen Spektrum-Wasserfall als PNG.

```sh
# PNG + JSON erzeugen:
cargo run --release -- song.opus -o song.png --json song.json
# Von stdin, PNG nach stdout:
cat song.opus | cargo run --release -- - -o - > song.png
# Bild im X11-Fenster zeigen:
cargo run --release --features show -- song.opus --show
```

Fenster-Steuerung: Rad = Zeit-Zoom um Cursor, Links-Drag = Pan,
`+`/`-`/Pfeile ebenso, `R` = Gesamtansicht, `ESC` = Ende. Hover ueber
dem Wasserfall zeigt Zeit, Frequenz, Pegel und Frame-Details.

Details: `../plan/20261011_01_init/plan.md`, Abhaengigkeiten: `deps.md`.
Modul-Uebersicht: `src/lib.rs` (Datenfluss 01→09).
