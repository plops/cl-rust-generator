# Walkthrough — 20260920_01_audio (Plan + S0-Gerüst)

Stand: 2026-09-20 · Auftrag: Wol Pumba · `examples/25_dnb/`
Plan: `plan/20260920_01_audio/{plan,task,deps}.md` · Code: `source0/`

## Was wirklich implementiert wurde

- **Plan-Artefakte** (diese Phase war plan-first): `plan.md` (Genre→Technik-Mapping,
  Architektur, Usage-Examples, Commit-Konvention), `task.md` (S0–S4, T1–T3 seriell
  mit Gates), `deps.md` (Org/Projekt-Notation für DeepWiki).
- **S0-Gerüst** (`source0/`, Binary `dark_dnb_synth`): nummerierte Module
  `01_clock`–`06_backend` nach Datenfluss-Reihenfolge, `main.rs`/`lib.rs` nur
  Verdrahtung, alle Dateien < 200 Zeilen. CLI: `--bpm/--bars/--gain-db/`
  `--render-wav/--list-devices`. Kein `rand`-Crate (deterministischer xorshift-Noise).
- **Gates grün** (verifiziert): `cargo fmt --check` sauber,
  `cargo clippy --all-targets -- -D warnings` sauber,
  `cargo test --release`: 18 Unit + 1 Render-Snapshot + 2 CLI-Smoke = 21/21.
- **Smokes**: `--render-wav /tmp/dnb_demo.wav --bars 4` → 243312 Frames, 973 kB;
  `--list-devices` zeigt Default + HDA-NVidia-HDMI-Geräte.

## Stellen, die wegen Tests/Befund anders wurden als im Prompt

1. **Prompt-`Block`-API existiert nicht** (fundsp 0.23 hat kein `Block::new`).
   Ersetzt durch kanonischen `beep.rs`-Pfad: `set_sample_rate` + `allocate` +
   pro Frame `get_mono()`/`get_stereo()`. In `plan.md` Kap. 2 dokumentiert.
2. **Prompt-`rand::random` ohne Dep** → eigener `Noise::next_sample`
   (xorshift64*, Seed 42). Zwei Renders sind bitgleich (Test).
3. **Prompt baut Stream nur für F32** → cpal-0.18-Befund: `SampleRate = u32`
   (kein `.0`), Gerätename via `description()` (kein `Device::name`),
   `build_output_stream` nimmt `StreamConfig` by value. `play_live` nutzt das;
   Format-Match F32/I16/U16 steht als Task S4-Arbeit im Plan.
4. **`Noise::next` triggert `clippy::should_implement_trait`** → umbenannt in
   `next_sample`; `% 2 == 0` → `is_multiple_of` (Clippy-Rust-1.98-Lints).
5. **Leeres WAV (`--bars 0`) ist Header-only (44 B)** — Test assertiert jetzt
   `len >= 44` + `hound`-Readback mit 0 Samples statt `> 44`.
6. **Binary-Name**: Paket `source0`, Binary `dark_dnb_synth` via `[[bin]]`,
   damit `CARGO_BIN_EXE_dark_dnb_synth` in Integrationstests existiert.

## Learnings

- fundsp-Dok-Beispiele (`examples/beep.rs`) sind verlässlicher als generierte
  Code-Vorschläge: Oszillatoren nehmen Frequenz als Input (`constant >> saw`),
  die Stereo-Schutzkette (`pan → declick/dcblock → limiter_stereo`) ist
  copy-paste-fähig, der Rest (Sequencer-Timing) gehört in eigenen Rust-Code.
- cpal 0.18 hat Breaking Changes gegenüber älteren Snippets (`SampleRate`-Alias,
  `description()`-API) — bei `cargo upgrade` (Task T3) erneut prüfen.
- Offline-Render (`hound`) entkoppelt DSP-Tests von ALSA-Hardware und macht
  CI ohne Soundkarte möglich; ALSA bleibt reiner Smoke (`--list-devices`).

## Mögliche Erweiterungen (nicht in den Tasks)

- Amen-Break-Slicing / Custom-Sampler-Node und MIDI-Tracker (Prompt-Schlussfrage).
- TUI (Task T2, ratatui) erst nach stabiler lib.
- Siren-Riser mit Hard-Clip-Vorschau, 808-Pitch-Automatisierung, Hats/Velocity.

## Docker-Pakete (in den Container aufnehmen)

Installiert und verifiziert: `libasound2-dev` (cpal-ALSA-Build),
`alsa-utils` (`aplay -l`), `pkg-config`, `build-essential` (gcc).
`aplay -l` zeigt: HDA NVidia (HDMI 0–3+), HD-Audio Generic.

## S1–S4 / T1–T3: implementiert (2026-09-20, alle Gates gruen)

- **S1** (`02_bass.rs`): 808-Sub (`sub_808_sample`, Sweep 120→45 Hz, Tail ~0,4 s),
  folgt Kick-Triggern im Render. Test: Peak-Fenster + Decay + Bound 0.6.
- **S2** (`03_drums.rs`): Closed-Hats (HP-Differenz, Offbeat Steps 2/6/10/14),
  Fracture-Variante `pattern_for` (Ghost-Kick Step 7, Ghost-Snare Step 14 ab
  Takt 8 je 16er-Block). Tests: HP-Null bei Konstanz, Ghost-Matrix.
- **S3** (`03_drums.rs` + Render): Rave-Sirene (`siren_sample`, Saw 400→2400 Hz),
  Riser-Uplift ueber 4 Bars + Tearout-Stabs. Test: Zero-Crossing-Rate steigt 6×.
- **S4** (`05_mix.rs`, `06_backend.rs`): `Master`-Kette (DC-Block + Tanh-Brickwall
  auf −1 dBFS), Render läuft durch `Master` (L/R getrennt); `play_live` ist
  format-generisch (F32/I16/U16 via `SizedSample + FromSample<f32>`).
  Render-Snapshot: Peak ≤ 0.892, Bridge-RMS < Rolling-RMS ≤ Tearout-RMS.
- **T1** (`00_preset.rs`, laedt zuerst): `Preset` (TOML, `PRESET_VER = 1`),
  `--preset/--save-preset/--dump-preset`, `--calibrate` (52 Bars: erwartet
  3163056 = gerendert 3163056), CLI-Flags ueberschreiben Preset
  (Default ← Preset ← CLI). Tests: Roundtrip, `ver = 99` → Err, BPM-Range.
- **T2** (`src/bin/dnb_tui.rs`, ratatui/crossterm): Sektions-Liste, Render-Gauge
  (R), Quit (Q); ohne TTY: Tabellen-Fallback Exit 0 (Smoke-Test mit "Tearout").
- **T3**: `cargo update` → 0 Pakete (Lockfile bereits neueste),
  `fmt --check`/`clippy -D warnings`/`cargo test --release` final gruen:
  27 Unit + 4 CLI + 2 Render = **33/33**.
- Full-Render 52 Bars: 3163056 Frames, 12,6 MB WAV.

## ALSA-Hardware-Befund (ehrlich)

- `--list-devices`: Default + HDA-NVidia-Geraete sichtbar.
- Live-Playback `--bars 1` oeffnet den Stream, scheitert aber am Treiber:
  `snd_pcm_dmix_open: unable to open slave` → sauberes `Err` (kein Panic),
  weil im Container keine bespielbare PCM-Senke existiert (nur HDMI ohne
  Display). Code-Pfad bis `build_output_stream`/`play` ist vollstaendig und
  format-generisch; hoerbarer Nachweis braucht Host mit Lautsprecher/Kopfhoerer.
- Antwort auf die Prompt-Schlussfrage: Amen-Slicing und MIDI-Tracker bleiben
  Erweiterungen (s. plan.md Kap. 5.9), kein Scope dieser Phase.

## Fix-Batch 2026-09-20 (Laptop-Feedback von Wol Pumba)

1. **`--device` wählt jetzt das ALSA-Gerät** (`find_output_device`,
   `device_matches` in `06_backend.rs`): cpal-Default zerfiel auf dem Laptop
   zu `default:1` (`Unknown PCM default:1`); mpv braucht dort ebenfalls
   `--audio-device=alsa/sysdefault:CARD=Generic_1`. Matching ist
   case-insensitiver Substring, mpv-Schreibweise (`alsa/…`, `CARD=…`)
   wird akzeptiert; Trefferlosigkeit listet (deduped) Geräte + Hinweis.
   Wichtig: mpv-CARD-Namen (`Generic_1`) stehen NICHT in cpals Aufzählung —
   dort heißt das Gerät `HD-Audio Generic, CX11880 Analog`, also
   `--device CX11880` (oder `Analog`) verwenden. `--list-devices` ist
   jetzt ebenfalls deduped.
2. **Underrun-Härtung**: Callback verteilt Stereo auf beliebig viele
   Geräte-Kanäle (`push_frame`, vorher Stille bei `channels != 2`),
   angeforderter ALSA-Puffer `Fixed(8192)` mit Fallback auf Geräte-Default
   bei Ablehnung. Reine Pump-Funktion `pump()` ist hardware-frei testbar.
3. **Musicality-Pass** („wouldn't call it DnB", Laptop-Boxen): Mid-Reese
   eine Oktave höher (`build_reese_mid`, 0.14 im Mix), Snare mit
   180-Hz-Korpus (war im Plan, fehlte), Kick mit Trigger-Phase-Click.
   Snapshot-Gates halten (Peak ≤ −1 dBFS, Bridge < Rolling ≤ Tearout).
4. Gates: `fmt --check`, `clippy --all-targets -D warnings`,
   `cargo test --release` **39/39** (32 Unit + 5 CLI + 2 Render).
