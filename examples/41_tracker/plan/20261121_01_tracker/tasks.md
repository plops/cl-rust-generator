# Tasks `dnb-tracker`

Jede Aufgabe endet mit: `cargo fmt && cargo clippy --all-targets -- -D warnings && cargo test`
(Arbeitsverzeichnis `examples/41_tracker/tracker`). Erst weiter, wenn grün.

## T0 Umgebung
- [x] `apt-get install libasound2-dev alsa-utils pkg-config`
- [x] `cargo install cargo-edit --locked` (für `cargo upgrade`)
- [x] `cargo new --name dnb-tracker tracker`, Deps laut `deps.md`
- [x] Audio prüfen: `aplay -l`; `aplay -D plughw:1,0 …` muss laufen

## T1 Typen (`01_types.rs`)
- [x] Konstanten `CHANNELS=8`, `ROWS=64`, `MAX_PATTERNS=32`, `MAX_ORDER=64`, `MAX_INSTR=16`
- [x] `Note` (u8: 0 leer, 1..=120 Note, 255 Off), `note_name()`/`parse_note()` (`C-4`, `C#4`, `===`, `---`)
- [x] `Cell { note, instr, vol, fx, arg }` mit `EMPTY`; `Pattern` = `[[Cell; CHANNELS]; ROWS]` geboxt
- [x] `Kind` (Kick, Snare, Hat, Reese, Sub, Bell, Lead, Sample) + `Patch`-Parameter
- [x] `Song { bpm, speed, order[MAX_ORDER], order_len, patterns, instruments, pan }`, `Song::new()` alloziert alles vor
- [x] Tests: Notenname-Roundtrip für alle 120 Noten

## T2 DSP (`02_dsp.rs`)
- [x] `Rng` (xorshift32), `poly_blep`, `Saw`/`Square`-Oszillator, `Biquad` (LP/HP/BP, RBJ Cookbook), `Env` (Attack/Decay/Sustain/Release, linear-exp), `soft_clip` (tanh)
- [x] Tests: LP @500 Hz dämpft 8 kHz-Sinus um >30 dB, lässt 100 Hz durch; Saw bleibt in ±1.1

## T3 Stimme (`03_voice.rs`)
- [x] `Voice::trigger(note, patch, vel)`, `release()`, `next(&patches) -> f32`
- [x] Synthese je `Kind`; Glide (`glide_to`), Pitch-Hüllkurve für Kick/Sub
- [x] Tests: jede Kind erzeugt Energie, klingt nach Release aus, keine NaN

## T4 Protokoll (`04_protocol.rs`)
- [x] `Command` (SetCell, SetBpm, SetOrder, Play{from_order, loop_pattern}, Stop, Preview, LoadSong), `Status`
- [x] `link(cap) -> (UiLink, AudioLink)` mit 3 rtrb-Ringen

## T5 Engine (`05_engine.rs`)
- [x] `Engine::new(song, sr)`, `apply(cmd)`, `render(&mut [f32])` (stereo interleaved)
- [x] Tick-/Zeilen-Akkumulator, Effekte C/D/G/R/K/T, Panning, Master-Soft-Clip, Peak-Meter
- [x] `poll(&mut AudioLink)` – Commands holen, Status senden
- [x] Tests: Zeilentiming exakt, Determinismus, Retrigger-Onsets, leerer Song = Stille

## T6 Frühe Musik (`07_wav.rs`, `09_demo_songs.rs`, `06_audio_out.rs`, `main.rs`)
- [x] `demo_dnb()` (174 BPM, Two-Step, Reese) und `demo_phonk()` (140 BPM, Cowbell, 808-Slides)
- [x] `render_wav(song, secs, path)`; CLI `--render out.wav --seconds N --demo dnb|phonk`
- [x] cpal-Ausgabe: `--list-devices`, `--device <teilstring>`, Default bevorzugt `CX11880`/analog vor HDMI
- [x] CLI `--play` (ohne TUI) → anhören!

## T7 Song-IO (`08_song_io.rs`)
- [x] Textformat (`bpm`, `speed`, `order`, `instr`-Zeilen mit `key=value`, `pattern`/Zeilen mit Zellen)
- [x] Tests: Roundtrip beider Demos

## T8 TUI (`10_ui_state.rs`, `11_ui_view.rs`, `12_ui_runner.rs`)
- [x] App-State: Cursor (Zeile, Kanal, Spalte 0..4), Edit-Modus, Oktave, Instrument, Step
- [x] Klaviertastatur (`z s x …` / `q 2 w …`), Hex-Eingabe, Effekt-Buchstabe, Entf, Note-Off `1`
- [x] Space Song Play/Stop, Enter Pattern-Loop, F1/F2 Oktave, F3/F4 Instrument, F5/F6 Pattern, F7/F8 BPM, Ctrl+S Speichern, Ctrl+Q Ende
- [x] View: Header, Pattern-Gitter (horizontal scrollend), Instrumentliste, Pegel, Hilfezeile
- [x] Tests: Tasten → Zelle + Command; `TestBackend` 80×24 und 120×40

## T9 Integration & Doku
- [x] `tests/render_demos.rs`, `tests/real_samples.rs` (skip ohne Daten)
- [x] `cargo upgrade`, Gates grün
- [x] `walkthrough.md`, `plan_effort.md`, Commits laut `plan.md`

## T10 Klang (nach Hör-Feedback „8-bit, kein Bass, blechern“)
- [x] Spike `spikes/spectrum.py` (Bänder, Schwerpunkt, Crest), `examples/stems.rs` (Kanäle solo)
- [x] Tone-Tiefpass nach Sättigung, sanftere Sättigung, cleaner Reese-Sub, 808-Hats, Cowbell-Tiefpass
- [x] `04_master_fx.rs`: Freeverb-Send + Limiter; Module ab 04 umnummeriert
- [x] Tests: Reverb-Ausklang, Limiter, Reese-Sub-Anteil, Hat-Luftanteil

## T11 Samples und Mixed-Demos
- [x] Amen-WAVs nach `samples/amen`, Phonk-Toolkit nach `samples/phonk_src/repo` (git-ignoriert)
- [x] `Patch.root` (Grundton, gemessen mit `spikes/pitch.py`), `Oxx` Slice-Offset, `Rxx` ohne Note rollt Slice
- [x] `Song.title` + berechnetes Badge `Song::sources()` (synth / samples / synth+samples) in TUI und CLI
- [x] Demos `synth-dnb`, `synth-phonk`; `songs/mixed_amen_jungle.txt`, `songs/mixed_drift_phonk.txt` via `examples/write_songs.rs`
- [x] Echtdaten-Tests rekursiv, Mixed-Songs müssen alle Samples finden
