# Tasks: Bitstrom-Sicht — `03_bitstream`

Seriell; jeder Schritt endet gruen (Test + `fmt` + `clippy`).

## B0 — Recherche + Doku

- [x] Library-Oberflaechen pruefen (opus-pure probe, rusty-opus pubs,
      opus-decoder lasts, libopus-ctls) → Entscheid in plan.md §5.
- [x] `plan/20261011_03_bitstream/{prompt,plan,task}.md` anlegen.

Verifikation: Doku-Commit. (Ist: `c374a5b`.)

## B1 — Splitter (Implementierung + Tests)

- [x] `03b_split.rs`: `split(packet)` → Ranges, `vbr`, `pad_bytes`,
      Frame-Groessen (§3.2, Codes 0–3, Selbst-Delimitierung nein).
- [x] Tests: Hand-Vektoren je Code; Partition (Summe = Paket);
      Sweep ueber Demo+Ref+YouTube-Ausschnitt (kein Fehler, Summe ok).

Verifikation: `cargo test split` gruen. (Ist: `6f0e490`.)

## B2 — CELT-Header + Allokation (Implementierung + Tests)

- [x] `03c_range.rs`: Range-Reader (bit-exakt, `tell()==1`-Init).
- [x] `03d_alloc.rs`: Baender-Grid, Dynalloc/Trim-Mid-Section,
      Intensitaet/Dual/Stereo-Reads, Allokations-Port (Bits/Band).
- [x] `03c_celt.rs`: silence/prefilter/transient/intra; Coarse-+Fine-
      Energien (21, stereo, mit Zustand, Reset bei Moduswechsel);
      Hybrid/SILK → nicht parbar (gemeinsamer Range-Coder, B4-Markierung).
- [x] Tests: Allokations-Identitaet exakt (Fuzz + 18 784 Vektor-Frames);
      Korrelation Energie↔PCM r = 0,936; Sweep-Peak ±1 Band;
      Determinismus; ref.opus = rein Hybrid (0 CELT-Frames).

Verifikation: `cargo test celt` gruen.

## B3 — Bitstrom-Spektrum + Dichte (Modell, Implementierung + Tests)

- [x] `05b_bitstream.rs`: Energien → dB-Spalten (21 Baender → 256 Bins,
      kalibriert: CAL −76,5 dB + De-Emphasis); Bits/Band →
      Dichte-Spalten (Summe = Frame-Bits); Divergenz = STFT − Bitstrom
      (NaN wo unverfuegbar); Hop-Mapping ueber Pre-Skip + Paketgrenzen.
- [x] Tests: Sweep-Peaks ±1 Band; Dichte exakt = Allokation (rel. < 1e-4);
      Skalen-Anker Sinus ±2 dB; Divergenz-Spitzen-Median ±4 dB
      (Rest methodisch: Fenster + Sweep-Smear), global ±6 dB.

Verifikation: `cargo test spectrum` gruen (7/7).

## B4 — Views + Hover (Implementierung + Tests)

- [x] `Source`/`Sources` (05b): 4 Quellen, Divergenz vorberechnet,
      Dichte-Max; `--view` (+1–4-Alias) + Tasten 1–4; Kopf/Legende/Hover
      quellen-bewusst (dB/DIV/BITS, Packing/Pad, TRANS/SIG, KEIN CELT).
- [x] Render: Pooling (Max / betrags-groesst), divergierende Colormap
      (±12 dB), NaN-Grau, lineare Dichte-Norm, CELT-Bandgitter.
- [x] Bugfix Allokation: Cap-`<< lm` fehlte (per Trace gegen opus-pure
      verifiziert, jetzt Puls-fuer-Puls identisch); Bilanz-Drain
      (`Rest/min(3,Restbaender)`) fliesst in die Dichte ein.
- [x] Tests: 4-View-Pixel-Smoke (paarweise > 5 % Diff), NaN-Grau,
      Hover-Text, Pooling-Unit, Cap-/Drain-Units.

Verifikation: `cargo test --all-features` gruen (71).

## B5 — HIL + Hygiene + Walkthrough

- [x] Alle Views headless rendern + `ffprobe` (4× png 1264×402);
      Viewer-Screenshots (Tasten 1–4 via xdotool) gesichtet;
      `interact.sh` erweitert (8/8 Asserts).
- [x] `fmt`, `clippy -D warnings`, `upgrade --dry-run` (aktuell),
      README, `walkthrough.md`, Commits je Task.

Verifikation: `smoke.sh` gruen, `git log` zeigt B0–B5.
