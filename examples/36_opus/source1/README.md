# `opus-vis` — Opus-Dateien visualisieren

Liest `.opus` (Ogg), zeigt pro Paket Modus (SILK/Hybrid/CELT),
Bandbreite und Bitrate, rendert einen Spektrum-Wasserfall als PNG.
Der eingebaute CELT-Bitstrom-Parser liest zusaetzlich Band-Energien
und Bit-Allokation aus den Paket-Bytes — vier Ansichten:

- **Spektrum** (1): decodiertes STFT-Spektrum in dB.
- **Bitstrom** (2): Encoder-Band-Energien in dB.
- **Divergenz** (3): STFT − Bitstrom in dB (divergierend, ±12 dB).
- **Dichte** (4): Bits je Bin (Informationsdichte, linear normiert).

SILK/Hybrid-Frames sind nicht exakt parbar (gemeinsamer Range-Coder)
und erscheinen als Boden/Grau (`KEIN CELT` im Hover).

```sh
# PNG + JSON erzeugen:
cargo run --release -- song.opus -o song.png --json song.json
# Andere Ansicht als PNG:
cargo run --release -- song.opus -o div.png --view divergenz
# Von stdin, PNG nach stdout:
cat song.opus | cargo run --release -- - -o - > song.png
# Bild im X11-Fenster zeigen:
cargo run --release --features show -- song.opus --show
```

Fenster-Steuerung: Rad = Zeit-Zoom um Cursor, Links-Drag = Pan,
`+`/`-`/Pfeile ebenso, `1`–`4` = Ansicht, `R` = Gesamtansicht,
`ESC` = Ende. Hover ueber dem Wasserfall zeigt Zeit, Frequenz, Wert
und Frame-Details plus CELT-Zeile (Transient/Signal-Bits).

Details: `../plan/20261011_01_init/plan.md`,
Bitstrom-Plan: `../plan/20261011_03_bitstream/plan.md`,
Abhaengigkeiten: `deps.md`. Modul-Uebersicht: `src/lib.rs`
(Datenfluss 01→09).
