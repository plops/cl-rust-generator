# Tasks: Bitstrom-Sicht — `03_bitstream`

Seriell; jeder Schritt endet gruen (Test + `fmt` + `clippy`).

## B0 — Recherche + Doku

- [ ] Library-Oberflaechen pruefen (opus-pure probe, rusty-opus pubs,
      opus-decoder lasts, libopus-ctls) → Entscheid in plan.md §5.
- [ ] `plan/20261011_03_bitstream/{prompt,plan,task}.md` anlegen.

Verifikation: Doku-Commit.

## B1 — Splitter (Implementierung + Tests)

- [ ] `03b_split.rs`: `split(packet)` → Ranges, `vbr`, `pad_bytes`,
      Frame-Groessen (§3.2, Codes 0–3, Selbst-Delimitierung nein).
- [ ] Tests: Hand-Vektoren je Code; Partition (Summe = Paket);
      Sweep ueber Demo+Ref+YouTube-Ausschnitt (kein Fehler, Summe ok).

Verifikation: `cargo test split` gruen.

## B2 — CELT-Header (Implementierung + Tests)

- [ ] `03c_celt.rs`: Range-Reader; transient/intra; Coarse-Energien
      (21, stereo-fach); Trim/Stereo/Dynalloc; Allokations-Port
      (Bits/Band); Hybrid-Grenze (§6); SILK → Markierung ohne Details.
- [ ] Tests: Musik-Fixture (CELT-only) — Allokations-Summe ≈ Bits
      (±5 %), Energien endlich, Korrelation Energie↔STFT > 0,7;
      Hybrid-Fixture — Summe stimmt; Sweep ohne Fehler.

Verifikation: `cargo test celt` gruen.

## B3 — Views + Hover (Implementierung + Tests)

- [ ] Render: Quellen-Umschaltung, divergierende Colormap, Band-Achse,
      `--view` + Tasten 1–4, Hover-Plus (Packing/Pad/Transient/Bits).
- [ ] Tests: alle 4 Views rendern (Pixel-Smoke je View); Hover-Text.

Verifikation: `cargo test --all-features` gruen.

## B4 — HIL + Hygiene + Walkthrough

- [ ] Alle Views headless rendern + `ffprobe`; Viewer-Screenshots
      (Tasten 1–4 via xdotool) sichten; `interact.sh` erweitern.
- [ ] `fmt`, `clippy -D warnings`, `upgrade --dry-run`, README,
      `walkthrough.md`, Commits je Task.

Verifikation: `smoke.sh` gruen, `git log` zeigt B0–B4.
