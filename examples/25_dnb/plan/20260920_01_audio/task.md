# task.md — 20260920_01_audio: seriell abarbeitbare Schritte
Jeder Schritt endet mit Gates. Erst bei grünen Gates committen (s. `plan.md` Kap. 10)
und zum nächsten Schritt. Code: `examples/25_dnb/source0/`.
Dateiregeln aus dem Prompt gelten ab der ersten angefassten Datei
(`NN_name.rs`, ≤~300 Zeilen, `main.rs`/`lib.rs` nur Verdrahtung, vorher/nachher grün).

## S0 — Scaffold + Backend-Hülle (Basis)

- `cargo new source0 --bin` (bzw. vorhandenes Gerüst übernehmen), Deps neueste:
  `fundsp 0.23`, `cpal 0.18`, `clap 4` (derive), `hound 3`, `anyhow 1`.
  `doc/deps.md`-Einträge nach `plan.md` Kap. 6.
- Module anlegen (leer, aber verdrahtet): `01_clock.rs`, `02_bass.rs`,
  `03_drums.rs`, `04_arrange.rs`, `05_mix.rs`, `06_backend.rs`;
  `main.rs`/`lib.rs` nur Deklaration + CLI-Parsing + dispatch.
- CLI: `--device`, `--bpm` (default 174.0), `--bars` (default 52),
  `--gain-db` (default −6.0, clamp +6), `--render-wav`, `--list-devices`,
  `--preset`/`--save-preset`/`--dump-preset`, `PRESET_VER = 1`.
- Backend: Sample-Format-Match F32/I16/U16 (generisches `run::<T>`,
  s. `plan.md` Kap. 7); Callback ohne Alloc/Panic; Fehler → Exit-Code ≠ 0.
  Stille (`--bars 0` rendert leere Datei) als Null-Nachweis.
- Unit-Tests: BPM→16th-Hz (174 → 11,6), Gain-Clamp, Preset-Verweigerung
  bei falscher `ver`.
- Gates: `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`,
  `cargo test` grün, `cargo run --release -- --help` grün,
  `--render-wav /tmp/s0.wav --bars 0` schreibt valides WAV (stille).
- Commit: `chore(scaffold): numbered modules cli backend shell`.

## S1 — Clock + Reese/808 (Rolling-Fundament)

- `01_clock.rs`: Frame-Clock (`u64`-Zähler, `sample_rate`-abgeleitet),
  `step_index` (16th), `bar`, `section_at(bar)` nach Form
  Intro 4 / A 16 / Bridge 8 / Riser 4 / Tearout 16 / Outro 4, Swing (default 0).
- `02_bass.rs` (fundsp, s. `plan.md` Kap. 7): Reese
  `(constant(50.0) >> saw() & constant(50.8) >> saw()) >> lowpass_hz(280, 1)`
  `>> shape(Tanh(2.5))`; 808-Layer (Pitch-Sweep 150→45 Hz, Decay 0,4 s,
  Root-Noten-folgend). `set_sample_rate` + `allocate` im Konstruktor.
- Tests: Clock-Step-/Bar-Math (u.a. bar = 1,379 s @174/44,1 kHz),
  Sektions-Grenzen, Reese-RMS > 0 + Peak < 1.0 (pre-Limiter), 808-Decay monoton.
- Gates: `fmt`, `clippy -D warnings`, `cargo test` grün (neu ≥6 Tests).
  Kein ALSA nötig.
- Commit: `feat(bass): reese plus 808 with frame clock`.

## S2 — Drums + Breaks (Kick/Snare/Hat, fractured Pattern)

- `03_drums.rs`: Kick (Sine-Drop 150→45 Hz, ~0,25 s), Snare (Noise + 180-Hz-Tone,
  ~0,2 s), Closed-Hat (HP-Noise, ~0,05 s, Offbeat), Ghost-Notes + Velocity;
  Default-Pattern `1000 0010 0010 0100` (Kicks auf 0/13, Snares auf 6/10,
  plus Fracture-Variante ab Bar 8). Deterministischer xorshift-Noise (Seed),
  KEIN `rand`-Crate.
- Tests: Trigger-Positionen pro Bar, Kick-Pitch fällt, Snare-Noise-Anteil > 0,
  Seed-Reproduzierbarkeit (zwei Renders bitgleich).
- Gates analog S1 + Render-Snapshot (2 bars Drums-solo): RMS/Peak-Fenster,
  keine NaN/DC (Mean < 0,01).
- Commit: `feat(drums): fractured break sequencer`.

## S3 — Arrangement A/Bridge/Riser (Rolling → Half-Time → Uplift)

- `04_arrange.rs`: Sektionen A (rolling, voll), Bridge (half-time: Kick/Snare
  halbiert, Reese-LP 160 Hz, Pad-Akkord, 8 bars), Riser (Siren 400→2400 Hz
  über 2 bars + Downlifter + Fill); Wechsel NUR an Bar-Grenzen,
  Declick-Fade 5 ms (`ARR_VER = 1`).
- `05_mix.rs` (teil): Bus-Summe + Pegel-Automatisierung pro Sektion.
- Tests: `section_at`-Matrix, Bar-Grenzen-Wechsel (kein Mid-Bar-Switch),
  Riser-Monotonie (Pitch steigt), Bridge-Drum-Dichte = ½ von A.
- Gates analog + 12-bar-Render (A+Bridge+Riser): Sektions-RMS unterscheidet
  sich plausibel (Bridge leiser als A), `--calibrate`-Framezahl exakt.
- Commit: `feat(arrange): rolling bridge riser sections`.

## S4 — Tearout + Studio-Mix + ALSA-Nachweis (C/Outro, Limiter, HW-Smoke)

- Tearout C (16 bars): Double-Kick-Layer, 808 länger, Siren-Stabs,
  +2 dB bis in den Limiter; Outro (4 bars, Fade-out).
- `05_mix.rs` (fertig): `declick`, `dcblock`, `limiter_stereo`,
  Bass-mono, Ziel −1 dBFS Peak; `--gain-db`-Clamp (+6), Default −6 dBFS Start.
- `06_backend.rs` (fertig): 2-s-Anspiel-Smoke wenn ALSA-Gerät vorhanden
  (`--list-devices` zeigt mind. ein Gerät; sonst Render-Fallback dokumentieren).
- Tests (`tests/render_snapshot.rs`): Full-Render (alle Sektionen):
  Peak ≤ −1 dBFS, Mean ≈ 0 (DC-Block), Stereo-Δ klein im Bass,
  kein NaN/Inf; (`tests/cli_smoke.rs`): `--help`, `--list-devices`-Exit 0,
  `--render-wav`-Smoke.
- Gates: `fmt --check`, `clippy --all-targets -D warnings`,
  `cargo test --release` grün, Full-Render-WAV schreibbar + abspielbar
  (`aplay` wenn Gerät da, sonst `soxi`/Datei-Check), `probe`-frei.
- Commit: `feat(mix): tearout drop with limiter and alsa proof`.

## T1 — Preset-Format + Dauerlauf/Härtung

- `PRESET_VER`-Loader (TOML: bpm, pattern, sections, gain),
  `--dump-preset`/`--save-preset`; unbekannte `ver` → klare Fehlermeldung.
- Dauerlauf: 52-bar-Render ohne Peak-Drift; Geräte-Fehler-Pfad (falsches
  `--device` → Exit ≠ 0 + Meldung); `--gain-db 99` clampet auf +6.
- Gates: alle Gates S0–S4 erneut grün + Dauerlauf-Log im Walkthrough.
- Commit: `fix(backend): preset versioning and error hardening`.

## T2 — TUI (ratatui, NACH CLI, kein Logik-Duplikat)

- Neues Bin-Target/Modul (nutzt lib): Transport (Play/Stop),
  Sektions-Anzeige, Pegel-Meter, `PRESET_VER`-Mismatch-Hinweis.
  Erst nach S4/T1 (stabile lib-Schnittstelle).
- Gates: `fmt`, `clippy`, `cargo test` (Parser-/Anzeige-Logik), CLI-Parität.
- Commit: `feat(tui): transport and meter screens`.

## T3 — Upgrade + Deps + Abschluss

- `cargo upgrade` (danach voller Testlauf), `deps.md` final,
  `fmt`/`clippy`/`test --release` final grün, `git log` sammeln.
- `plan/20260920_01_audio/walkthrough.md` schreiben (implementiert vs. Plan,
  Abweichungen, Render-/ALSA-Logs, Learnings, Erweiterungen wie Amen-Slicing
  / MIDI-Tracker aus der Prompt-Schlussfrage, Docker-Pakete).
- Commit: `docs(plan): walkthrough for dnb synth and alsa`.
