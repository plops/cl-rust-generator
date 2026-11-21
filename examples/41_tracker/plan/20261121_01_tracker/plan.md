# Implementierungsplan `dnb-tracker` (MVP)

Ziel: Ein Terminal-Tracker (TUI) für Drum and Bass (DnB, ~170–176 BPM)
und Phonk (~130–150 BPM, Cowbell-Melodien, verzerrte 808-Bässe).
Musik so früh wie möglich hörbar: erst Engine + Offline-WAV-Render +
`--play`, danach die TUI.

## 1. Kontext für einen unabhängigen Agenten

| Pfad | Inhalt |
|---|---|
| `examples/41_tracker/plan/20261121_01_tracker/prompt.txt` | Auftrag + verbindliche Regeln (Dateiaufteilung, Gates, Walkthrough) |
| `…/deps.md` | Crates mit DeepWiki-Slugs und verifizierten API-Fakten |
| `…/tasks.md` | Abarbeitbare Einzelschritte |
| `examples/40_gentoo_deps/gentoo-dep-tui/src/lib.rs` | Vorbild für nummerierte Module per `#[path]` |
| `examples/41_tracker/tracker/` | Das Crate (eigenständig, kein Workspace) |

Audio im Container: `/dev/snd` ist durchgereicht, Host-Karte 1
(`CX11880 Analog`, ALSA `hw:1,0`) ist frei und spielt direkt auf der
Host-Hardware (verifiziert über `/proc/asound/card1/pcm0p/sub0/status`).
Karte 0 sind nur HDMI-Ausgänge.

## 2. Architektur

```
UI-Thread (ratatui/crossterm)          Audio-Thread (cpal-Callback)
  App-State (eigene Song-Kopie)          Engine (eigene Song-Kopie)
     │ Command ──rtrb──────────────────────▶ apply()
     │ ◀──────────────rtrb── Status (Playhead, Pegel)
     │ ◀──────────────rtrb── Box<Song> (alter Song zum Droppen)
```

- **Zwei Song-Kopien**: UI editiert ihre Kopie und schickt die gleiche
  Änderung als kleines `Command` (z. B. `SetCell`). Der Audio-Thread
  alloziert nie: Patterns sind auf `MAX_PATTERNS` vorallokiert, die
  Order-Liste ist ein festes Array.
- **Song-Tausch** (`LoadSong(Box<Song>)`): der alte Song geht über einen
  dritten Ring zurück und wird im UI-Thread gedroppt.
- **Engine** ist reine Funktion von (Song, Commands, Samplerate) →
  `render(&mut [f32])`. Damit gleich nutzbar für cpal, WAV-Export und
  Tests (deterministisch, kein Gerät nötig).
- **Kanalmodell**: 8 monophone Kanäle (Tracker-typisch) + 1 Preview-
  Stimme fürs Anspielen.
- **Timing**: 4 Zeilen pro Beat, `speed` Ticks pro Zeile (Default 6),
  Zeilenlänge `sr*60/(bpm*4)` Samples mit `f64`-Akkumulator (kein Drift).

### Module (`tracker/src/`)

| Datei | Zuständigkeit |
|---|---|
| `01_types.rs` | Konstanten, `Note`, `Cell`, `Pattern`, `Patch`/`Kind`, `Song`, Notennamen |
| `02_dsp.rs` | PolyBLEP-Oszillatoren, RBJ-Biquad, Hüllkurve, Rauschen, Soft-Clip |
| `03_voice.rs` | Eine Stimme: Trigger, Glide, Retrigger, Synthese je `Kind` |
| `04_protocol.rs` | `Command`, `Status`, Ring-Paare (`UiLink`/`AudioLink`) |
| `05_engine.rs` | Sequencer + Mixer + Effekt-Spalte, `render()` |
| `06_audio_out.rs` | cpal: Geräteauswahl, Stream, Formatkonvertierung |
| `07_wav.rs` | Offline-Render nach WAV, WAV-Samples laden |
| `08_song_io.rs` | Textformat speichern/laden |
| `09_demo_songs.rs` | DnB- und Phonk-Demo, per Schleifen erzeugt |
| `10_ui_state.rs` | Cursor, Edit-Modus, Tastenbelegung → `Command`s (ohne Terminal testbar) |
| `11_ui_view.rs` | ratatui-Zeichnen (Header, Pattern, Instrumente, Pegel) |
| `12_ui_runner.rs` | Terminal-Setup, Event-Loop |
| `lib.rs` / `main.rs` | nur Moduldeklarationen bzw. CLI-Verdrahtung |

### Effekt-Spalte (je Zelle ein Befehl + 2 Hex-Ziffern)

| Befehl | Wirkung | DnB/Phonk-Nutzen |
|---|---|---|
| `Cxx` | Filter-Cutoff (exponentiell 40 Hz … 18 kHz) | Reese-Bass-Sweeps |
| `Dxx` | Drive (Sättigung) | verzerrte 808 |
| `Gxx` | Glide zur neuen Note, Geschwindigkeit xx | 808-Slides (Phonk) |
| `Rxx` | Retrigger alle x Ticks | Snare-Rolls (DnB) |
| `Kxx` | Note abschneiden nach xx Ticks | Stutter, kurze Hats |
| `Txx` | Tempo setzen (BPM = xx, hex) | Tempowechsel |

## 3. Vorschläge über das MVP hinaus (nicht implementiert, priorisiert)

1. Instrument-Parameter in der TUI editieren (Cutoff, Decay, Drive …).
2. Order-Liste als eigener Editor-Bereich.
3. Sidechain-Ducking (Bass duckt unter der Kick) – typisch für beide Genres.
4. Breakbeat-Slicing (Amen-Break aus WAV in Zeilen zerlegen).
5. Undo/Redo, Copy/Paste von Blöcken.
6. MIDI-Eingabe, Master-Effekte (Reverb, Delay).

## 4. Tests / Gates

- Unit: DSP (Filterdämpfung, Wertebereiche), Engine (Zeilentiming
  samplegenau, Determinismus, Retrigger-Onsets, Note-Off), Song-IO
  (Roundtrip), UI-State (Tasten → Zellen/Commands), View (`TestBackend`).
- Integration (`tests/`): beide Demos rendern, RMS/Peak/NaN prüfen,
  WAV-Roundtrip.
- Regression auf echten Daten: WAVs aus `TRACKER_SAMPLES` bzw.
  `tracker/samples/` laden und abspielen; ohne Daten übersprungen.
- Gates: `cargo test`, `cargo clippy --all-targets -- -D warnings`,
  `cargo fmt --check`.

## 5. Commit-Plan

1. `docs(tracker): add plan, tasks and deps for dnb tracker`
2. `feat(tracker): add dsp, voices and engine with offline wav render`
3. `feat(tracker): play demo songs through cpal`
4. `feat(tracker): add song text format and demo songs`
5. `feat(tracker): add ratatui pattern editor`
6. `docs(tracker): add walkthrough and effort report`
