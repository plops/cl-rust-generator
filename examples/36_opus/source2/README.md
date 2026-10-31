# `opus-vis` — Opus-Dateien verstehen

Liest `.opus` (Ogg) und erklaert die Datei in drei Formen: als
deutschen **Text-Report** (Standard bei jedem Lauf), als
**Spektrum-Wasserfall** (PNG plus optionales X11-Fenster) und als
**JSON** (Schema 2, Kopf/Tags/Fakten/Frames).

Der Report zeigt Kopf und Tags (Encoder-Vendor, Pre-Skip, Gain),
Bitraten-Statistik (Ø, Min, p50/p95, Max, VBR-Index), Modus- und
Bandbreiten-Anteile, Moduswechsel-Zeitachse, Stille/DTX, Transienten,
Signal-Pegel (Peak, RMS, Clipping, Stereo-Korrelation,
Spektral-Schwerpunkt) und 3–5 Einordnungs-Saetze (z. B. „spricht fuer
Sprache"). Das JSON enthaelt dieselben Fakten maschinenlesbar, je
Frame mit Config, Packing, Pad und — wo CELT — Band-Energien/Bits.

Der eingebaute CELT-Bitstrom-Parser liest Band-Energien und
Bit-Allokation aus den Paket-Bytes — vier Ansichten:

- **Spektrum** (1): decodiertes STFT-Spektrum in dB.
- **Bitstrom** (2): Encoder-Band-Energien in dB.
- **Divergenz** (3): STFT − Bitstrom in dB (divergierend, ±12 dB).
- **Dichte** (4): Bits je Bin (Informationsdichte, linear normiert).

SILK-Frames walkt ein eigener Header-Walk (Signaltyp, Gains, Pitch,
LPC-Huelle aus NLSF); Hybrid-Frames teilen sich einen Coder (SILK
unten, CELT ab Band 17 oben). Ansicht 2 zeigt Huelle bzw. die
Kombination, der Hover die Stimm-Klasse plus SILK-Bit-Anteil.

```sh
# Report auf stdout + PNG erzeugen:
cargo run --release -- song.opus -o song.png
# Leise (nur PNG, kein Report):
cargo run --release -- song.opus -o song.png --quiet
# PNG + JSON (Schema 2) erzeugen:
cargo run --release -- song.opus -o song.png --json song.json
# Andere Ansicht als PNG:
cargo run --release -- song.opus -o div.png --view divergenz
# Von stdin, PNG nach stdout (Report dann nach stderr):
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
Verstehens-Plan: `../plan/20261013_05_verstehen/plan.md`,
SILK-Plan: `../plan/20261013_06_silk/plan.md`,
Abhaengigkeiten: `deps.md`. Modul-Uebersicht: `src/lib.rs`
(Datenfluss 01→09, SILK in `03f`, Hybrid in `05c`,
Fakten in `03e`, Report in `08b`).
