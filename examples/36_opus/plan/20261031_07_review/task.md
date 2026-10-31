# Tasks: Bis zum letzten Bit — `07_review` (source2)

Seriell; jeder Schritt endet gruen (`cargo test --all-features`,
`cargo fmt --check`, `cargo clippy --all-targets --all-features -- -D
warnings`). Keine neue Crate; jede Datei < 600 Zeilen;
`forbid(unsafe_code)`. Ein Commit je Schritt (Conventional Commits).

## R0 — Basis

- [x] `source2` aus `source1` (ohne `target/`), Module nach Datenfluss
      umnummeriert (`02b_split`, `04_range`, `05a–d` CELT, `06a–e` SILK,
      `07_walk`, `08_decode`, `09/09b`, `10*`, `11*`, `12_cli`, `13_show`).
- [x] `scripts/mkfixtures.sh`: Fixture-Matrix aus `silk.webm` +
      Synthese-Toenen (22 Dateien, < 1 MB) nach `tests/data/gen/`.

Verifikation: Matrix erzeugt, `opusinfo`/Oracle-Beispiel laufen.

## R1 — CELT vollstaendig

- [x] `05c_celt_bands.rs`: `quant_all_bands`-Bit-Walk (Theta: Stufe/
      uniform/Dreieck, Intensitaets-Inversionsbit, Splits mit
      Rebalance, PVQ `dec_uint(V(N,K))`, N=1-Vorzeichen), Pulse-Cache.
- [x] `05d_celt_frame.rs`: Anti-Collapse-Bit, `unquant_energy_finalise`,
      Prefilter-Werte, `CeltBits` je Gruppe, Bits/Pulse je Band.
- [x] Unit-Tests: `V(N,K)` gegen geschlossene Form, Cache-Monotonie,
      Bit-Gruppen summieren auf `tell_frac`.

Verifikation: `final_range` CELT-only 100 % (Fixtures + YouTube).

## R2 — SILK vollstaendig

- [x] `06d_silk_frame.rs`: drei Kodier-Stufen, LBRR-Variante ohne
      Gedaechtnis-Eingriff, 1/8-Bit.
- [x] `06e_silk_dec.rs`: VAD/LBRR je Kanal, LBRR-Muster (40/60 ms),
      LBRR-Frames, Stereo-Praediktor + Mid-only, Seitenkanal-Reset.
- [x] Unit-Tests: Posten summieren exakt (mono 20 ms, stereo 60 ms).

Verifikation: `final_range` SILK 100 % (inkl. FEC, Stereo, 10–120 ms).

## R3 — Opus-Walk + Orakel

- [x] `07_walk.rs`: Modus-Arme, Resets (SILK nur nach CELT-only, CELT
      ausser nach SILK→CELT-Redundanz), Redundanz beidseitig inkl.
      Storage-Kuerzung, Hybrid→SILK-Ausblend-Frame, PLC-Frames, `Budget`.
- [x] `08_decode.rs`: `final_range` je Paket.
- [ ] `tests/oracle.rs`: alle Fixtures (immer), grosse Dateien
      (`#[ignore]`), Budget-Identitaet, `opusenc --save-range`.

Verifikation: `cargo test --test oracle` gruen; `--ignored` in Release gruen.

## R4 — Modell

- [ ] `09b_model.rs`: `HopInfo` (ein Struct je Hop) statt 13 Vektoren,
      Paket-Zusammenfassung (`PacketInfo`), Spalten Bitstrom/Dichte
      (gemessen), Divergenz; Hybrid ohne Sonderpfad.
- [ ] Bestehende Werte-Orakel (Pitch, Huelle, Energie↔PCM) portieren.

## R5 — Darstellung

- [ ] Renderer: Budget-Lane (gestapelte Anteile je Hop), Legende.
- [ ] Viewer-Hover: Budget-Zeile, Redundanz/LBRR/Stereo-Marker.
- [ ] JSON-Schema 3 (`final_range`, `budget_bits`, `pulses`), Writer mit
      Listen-Helfer statt Hand-Kommas.
- [ ] Report: Budget-Anteile, FEC/Redundanz/Stereo-Zeilen, Regel-Tests.
- [ ] `CliConfig` nach `12_cli.rs`.

## R6 — Gegenproben

- [ ] `scripts/libopus_oracle.py` (`ctypes` auf `libopus.so.0`):
      JSON-`final_range` gegen libopus je Paket, alle Fixtures.
- [ ] `opusenc --save-range` in `tests/oracle.rs`.

## R7 — Hygiene + Smoke/HIL

- [ ] `fmt`, `clippy -D warnings`, `cargo upgrade --dry-run`, Dateigroessen.
- [ ] `scripts/smoke.sh` + `interact.sh` unter Xvfb gruen.

## R8 — Doku + Commits

- [ ] `source2/HANDBUCH.md`, `README.md`, `deps.md`.
- [ ] `plan/20261031_07_review/walkthrough.md`.
- [ ] Commits je Schritt pruefen (`git log --oneline`).
