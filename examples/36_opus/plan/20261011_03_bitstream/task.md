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

- [ ] Energien → dB-Spalten (21 Baender → 256 Bins); Bits/Band →
      Dichte-Spalten; Divergenz = STFT − Bitstrom (dB).
- [ ] Tests: Sweep liefert wandernde Bitstrom-Peaks; Dichte ≈
      Allokation; Divergenz ≈ 0 dB auf Fixture (±3 dB Median).

Verifikation: `cargo test spectrum` gruen.

## B4 — Views + Hover (Implementierung + Tests)

- [ ] Render: Quellen-Umschaltung, divergierende Colormap, Band-Achse,
      `--view` + Tasten 1–4, Hover-Plus (Packing/Pad/Transient/Bits).
- [ ] Tests: alle 4 Views rendern (Pixel-Smoke je View); Hover-Text.

Verifikation: `cargo test --all-features` gruen.

## B5 — HIL + Hygiene + Walkthrough

- [ ] Alle Views headless rendern + `ffprobe`; Viewer-Screenshots
      (Tasten 1–4 via xdotool) sichten; `interact.sh` erweitern.
- [ ] `fmt`, `clippy -D warnings`, `upgrade --dry-run`, README,
      `walkthrough.md`, Commits je Task.

Verifikation: `smoke.sh` gruen, `git log` zeigt B0–B5.
