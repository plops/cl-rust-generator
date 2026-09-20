# Implementierungsplan — 20260920_01_audio
Dark DnB Synth (174 BPM, instrumental) — Standalone-Rust-Engine direkt auf ALSA

Auftrag: Wol Pumba (wolpumba@gmail.com) · Linux (Ubuntu 26, Docker) · ALSA-Ausgabe
Prompt: `plan/20260920_01_audio/prompt.txt` · Tasks: `plan/20260920_01_audio/task.md`
Deps: `plan/20260920_01_audio/deps.md` · Code: `examples/25_dnb/source0/`
Vorgänger-Muster: `examples/24_embassy_stm32g4/plan/20260912_02_full/{plan,task}.md`

Stack (neueste, verifiziert 2026-09-20): `fundsp 0.23.0` (DSP-Graph),
`cpal 0.18.2` (ALSA-Stream), `alsa 0.11` (transitiv via cpal).
System: `libasound2-dev`, `alsa-utils` (`aplay`/`arecord` zum Nachweis).
Host-Tests ohne Hardware: Offline-WAV-Render (`hound`), kein ALSA nötig.

## 1. Was ist gefragt (Checkliste aus dem Prompt)

1. Linux-Programm, das Drum and Bass (instrumental) auf ein ALSA-Gerät ausgibt.
2. Genre-Vorgabe (verbindlich, s. Kap. 3): 174 BPM, rolling jump-up,
   dark jungle reese, distorted 808 sub, crisp fractured breaks,
   clipped rave-siren transitions, half-time reflective bridge,
   tightened studio mix, maximum final-tearout drop.
3. Technik: FunDSP-Synthèse-Graph + CPAL als ALSA-Hardware-Stream
   (`cargo new dark_dnb_synth`, `cargo add fundsp cpal`, Release-Build).
4. Datei-Aufteilung: >~300 Zeilen oder >1 Zuständigkeit → splitten;
   neue Dateien `NN_name.rs` in Initialisierungs-/Datenfluss-Reihenfolge;
   `main.rs`/`lib.rs` nur Modul-Deklaration + Verdrahtung, kein Verhalten.
5. Tooling: `cargo fmt`, `cargo clippy -D warnings`, neueste Deps
   (`cargo upgrade` bei Einführung), neue Deps in `deps.md` (Org/Projekt-Notation).
6. Usage-Examples vorab in diesem Plan (Kap. 7, aus fundsp-`beep.rs` + cpal-Dok).
7. Neue Unit-/Integration-Tests wie erforderlich, ausführen, grün.
8. Deliverables: dieser Plan, `task.md` (serielle Tasks mit Test-Gates),
   am Ende `plan/20260920_01_audio/walkthrough.md` + Docker-Paketliste.
   (Hinweis: der Prompt nennt fälschlich `plan/20260912_02_full/walkthrough.md`
   — gemeint ist unser Ordner `plan/20260920_01_audio/`.)

## 2. Befund zum Prompt-Beispielcode (warum nicht 1:1 übernehmen)

Der Code in `prompt.txt` (§„The Rust Implementation") kompiliert so nicht
und ist klanglich unvollständig. Der Plan fixt das (Details in Tasks S0–S2):

- `rand::random` ohne `rand`-Dep → ersetzen durch deterministischen
  Noise (LCG/xorshift im Drum-Modul, seedbar für reproduzierbare Tests).
- `Block::new(Box::new(...))` existiert in fundsp 0.23 nicht. Kanonisch
  (fundsp-`examples/beep.rs`): Graph direkt halten, `set_sample_rate()`,
  `allocate()`, pro Frame `get_stereo()` (bzw. `get_mono()`) aufrufen —
  kein `Block`, kein `node.process(1, &[], &mut [l, r])`.
- `build_output_stream` nur für `f32` gebaut → auf manchen ALSA-Geräten
  (I16/U16-Default) Panic. Kanonisch: `sample_format()` matchen
  (F32/I16/U16, generisches `run::<T>` mit `SizedSample + FromSample<f32>`).
- `lfo(|t| …)` als Drum-Sequencer trägt Audio-Samples auf der Hüllkurven-
  Zeitbasis aus; Timing driftet gegen den CPAL-Callback. Plan: Sample-Zähler
  im Backend (`u64`-Frame-Clock, aus `sample_rate` abgeleitet), Pattern per
  16th-Note-Index, Hüllkurven als reine Funktionen der Phase seit Trigger.
- Klanglich fehlen: 808-Sub, Hats/Break-Chops, Rave-Sirene, Arrangement
  (Rolling → Half-Time-Bridge → Tearout), Limiter/DC-Block (Clipping-Schutz).
  Alles in Kap. 3/4 als Module eingeplant — kein Scope-Creep, sondern die
  Genre-Zeile des Prompts.

## 3. Genre → Technik-Mapping (verbindlich für den Mix)

| Genre-Phrase | BPM/Zeit | Umsetzung |
|---|---|---|
| 174 BPM rolling jump-up | 16th = 174/60·4 = 11,6 Hz; bar = 1,379 s | `01_clock.rs`: Frame-Clock, `step_index`, `bar/section`-Phasen, Swing-Parameter (default 0) |
| dark jungle reese | ~50–55 Hz, 2× detuned saw | `02_bass.rs` (fundsp): `constant(50.0) >> saw()` & `constant(50.8) >> saw()` → `lowpass_hz(280, 1)` → `shape(Tanh(2.5))`, Gain-Staging |
| distorted 808 sub | 45–65 Hz pitch-sweep sine + tanh | `02_bass.rs`: 808-Layer (Kick-folgend, Decay ~0,4 s), nur Root-Noten |
| crisp fractured breaks | 16-Step `1000 0010 0010 0100` + Chops | `03_drums.rs`: Kick (sine-drop 150→45 Hz), Snare (noise+tone 180 Hz), Hats (HP-noise, offbeat), Velocity/Ghost-Notes |
| clipped rave-siren transitions | 2-/4-bar Uplift | `04_arrange.rs`: Siren-Riser (Sägezahn 400→2400 Hz, 2 bars, hard-clip-Vorschau) + Downlifter + Fill |
| half-time reflective bridge | halbes Feel, weniger Drums | `04_arrange.rs`: Sektion B (Drums halbiert, Reese-LP auf 160 Hz, Pad-Akkord, 8 bars) |
| tightened studio mix | kein Clip, Mono-Bass | `05_mix.rs`: `declick`, `dcblock`, `limiter_stereo`, Bass-mono <120 Hz (hier: schmaler Stereo-Wert), Ziel −1 dBFS Peak |
| maximum final-tearout drop | letzte 16 bars lauter/dichter | `04_arrange.rs`: Sektion C (Double-Kick-Layer, 808 länger, Siren-Stabs, +2 dB bis Limiter) |

Form: Intro (4 bars, Riser) → Rolling A (16) → Bridge B half-time (8) →
Re-Riser (4) → Tearout C (16) → Outro (4). Loop oder `--bars N` begrenzt.

## 4. Architektur (Zielbild, `source0/`)

```
source0/                         Plan-Ordner (dieser Plan)
├── Cargo.toml                   bin dark_dnb_synth; fundsp 0.23, cpal 0.18, clap 4, hound 3
├── src/
│   ├── main.rs                  NUR Args + Verdrahtung (init, stream/render, join)
│   ├── lib.rs                   Modul-Deklarationen + Re-Exporte (für Tests)
│   ├── 01_clock.rs              BPM→Hz, Step-/Bar-/Sektions-Clock, Swing (rein, std-frei testbar)
│   ├── 02_bass.rs               Reese- + 808-Graph (fundsp; f32-seitig, s. Kap. 7)
│   ├── 03_drums.rs              Kick/Snare/Hat/Siren-Synth + Pattern (deterministisch, ohne rand)
│   ├── 04_arrange.rs            Sektions-State-Machine (Intro/A/Bridge/Riser/Tearout/Outro)
│   ├── 05_mix.rs                Bus-Summe, Soft-Clip, Declick/DC-Block/Limiter-Hülle, Pegel
│   └── 06_backend.rs            CPAL-ALSA-Stream (F32/I16/U16) + --render-wav + --list-devices
└── tests/
    ├── render_snapshot.rs       Offline-Render: 4 bars → WAV, RMS/Peak/Stereo-Asserts
    └── cli_smoke.rs             --help, --list-devices (ohne HW grün), --render-wav smoke
```

- Datenfluss: `clock → arrange → (bass, drums) → mix → backend`.
  Nummern folgen Init-/Signal-Reihenfolge (Clock vor Stimmen, Mix vor Backend).
- DSP-Typ: fundsp-`prelude32` (f32) im Graph; CPAL-Callback konvertiert per
  `FromSample<f32>` auf das Geräteformat. Keine `Mutex`-pur-1-Sample-Schleife
  aus dem Prompt — der Graph ist ` Send`-fähig im Callback; Alternative
  (falls fundsp-Typ kein `Send`): lock-freie `ringbuf`-Übergabe, kein
  `std::sync::Mutex` im Audio-Thread (Begründung in Task S4).
- Kein `rand`-Crate: Noise per xorshift64* mit Seed (reproduzierbar,
  snapshot-testbar). `rand` wird NICHT eingeführt.

## 5. Berücksichtigte Querschnitts-Punkte (Prompt §„beruecksichtige dabei")

Der Prompt nennt fünf Firmware-Punkte (Messgenauigkeit, Schutzbeschaltung,
TUI-Protokoll, Modus-Wechsel, persistente Konfig). Sie sind wörtlich auf
Audio nicht anwendbar; hier die verbindliche Audio-Übertragung (Vorschlag,
von Wol Pumba zu bestätigen — Defaults im Plan eingearbeitet):

1. **Messgenauigkeit/Kalibrierung** → Sample-Clock-Genauigkeit: `sample_rate`
   vom Gerät übernehmen (nicht hardcoden), BPM aus Frame-Zähler ableiten;
   `--calibrate` gibt 1-kHz-/60-s-Report (erwartete vs. gerenderte Frames).
   Keine Quarz-/VDDA-Kalibrierung (kein ADC im Scope).
2. **Schutzbeschaltung/Limits (§4, max 3,3 V)** → Gehör-/Boxen-Schutz:
   Master-Limiter + `--gain-db` mit Clamp (max +6 dB), Default-Pegel −6 dBFS
   Start + Warnung bei `--gain-db > 0`; Bass-mono, DC-Block (kein DC-Offset
   auf die Endstufe). Keine Hardware-Limits (reine Software-Ausgabe).
3. **TUI-Protokoll-Versionierung** → Preset-/Pattern-Format `PRESET_VER = 1`
   (TOML: bpm, pattern, sections, gain). Loader lehnt unbekannte `ver` mit
   klarer Meldung ab statt abzustürzen; `--dump-preset` dokumentiert das Format.
4. **Fehlerverhalten bei Modus-Wechsel während laufender Messung** →
   Sektions-Wechsel während Playback: State-Machine schaltet nur an Bar-
   Grenzen, Hüllkurven werden nicht abgeschnitten (Declick-Fade 5 ms);
   Geräte-Fehler im Callback → `eprintln!` + sauberer Exit-Code ≠ 0,
   kein Panic im Audio-Thread.
5. **Persistente Konfiguration** → Vorschlag: **kein Schreiben ohne Flag**;
   Defaults eingebaut, `--preset file.toml` lädt, `--save-preset` schreibt
   explizit. Firmware-Flash-Analogie entfällt (kein Flash-Verschleiß-Thema).

Eigene Vorschläge (noch nicht bestätigt, im Plan mit Default entschieden):

6. CLI zuerst (`clap`: `--device`, `--bpm`, `--bars`, `--gain-db`,
   `--render-wav out.wav`, `--list-devices`, `--preset`); TUI (ratatui)
   erst danach als separater Task T2 — kein Logik-Duplikat (TUI ruft lib).
7. Offline-Render ist der HIL-Ersatz: jeder Sound-Task braucht einen
   Render-Snapshot (RMS/Peak/Nullstellen-Asserts), ALSA-Nachweis nur als
   Smoke (`--list-devices`, 2-s-Anspiel wenn Gerät da).
8. Arrangement-Version `ARR_VER = 1` in `04_arrange.rs`; neue Sektionen nur
   mit Bump + Migrations-Notiz im Walkthrough.
9. Keine MIDI-/Sampler-Erweiterung (Amen-Break-Slicing, MIDI-Tracker aus der
   Prompt-Schlussfrage) in dieser Phase — als „mögliche Erweiterung" in den
   Walkthrough, nicht in die Tasks.

## 6. Abhängigkeiten (neu, in `deps.md` nachtragen, jeweils neueste nehmen)

| Crate | Org/Projekt | Zweck |
|---|---|---|
| fundsp | SamiPerttu/fundsp | Reese-/808-/Sirene-DSP-Graph (s. Kap. 7) |
| cpal | RustAudio/cpal | ALSA-Output-Stream (s. Kap. 7) |
| alsa | diwic/alsa | transitiv via cpal (ALSA-Backend) |
| clap | clap-rs/clap | CLI (`--device/--render-wav/…`, derive) |
| hound | ruvec/hound | Offline-WAV-Render für Tests |
| anyhow | dtolnay/anyhow | Fehler-Propagierung im Backend/CLI |

System (Docker/Ubuntu 26): `libasound2-dev` (cpal-ALSA-Build),
`alsa-utils` (`aplay -l`, `speaker-test`-Smoke), `pkg-config`, `build-essential`.
Bestehende Deps: keine (Greenfield-`source0`). `cargo upgrade`-Regel: bei
Einführung neueste nehmen; danach nur via Task T3.

## 7. Usage-Examples (verifiziert: fundsp 0.23 `examples/beep.rs`, cpal-Dok)

Reese-Bass (fixe Frequenz → Oszillator; `dc`/`constant` als Quelle):

```rust
use fundsp::prelude32::*;
let reese = (constant(50.0) >> saw() & constant(50.8) >> saw())
    >> lowpass_hz(280.0, 1.0)
    >> shape(Shape::Tanh(2.5));
```

Stereo-Kette mit Schutz (Pan → Declick/DC-Block → Limiter — aus `beep.rs`):

```rust
let mut c = c >> pan(0.0)
    >> (declick() | declick())
    >> (dcblock() | dcblock())
    >> limiter_stereo(1.0, 5.0);
c.set_sample_rate(sample_rate);
c.allocate();
let mut next = move || c.get_stereo();
```

CPAL-Stream format-agnostisch (Pflicht statt Prompt-F32-only):

```rust
use cpal::traits::{DeviceTrait, HostTrait};
use cpal::{FromSample, SizedSample};
match config.sample_format() {
    cpal::SampleFormat::F32 => run::<f32>(&device, &config.into()),
    cpal::SampleFormat::I16 => run::<i16>(&device, &config.into()),
    cpal::SampleFormat::U16 => run::<u16>(&device, &config.into()),
    _ => panic!("unsupported format"),
}
// im Callback: T::from_sample(s) pro Kanal, Channels aus config
```

Referenzen: `SamiPerttu/fundsp` (`examples/beep.rs`, `sequence.rs`),
`RustAudio/cpal` (Stream-Config-/Callback-Dok, ALSA-Host).

## 8. Risiken

- Prompt-`Block`-API existiert nicht → Task S0 baut sofort den
  `get_stereo()`-Pfad nach `beep.rs`; kein Fix-Versuch an `Block`.
- `rand` im Audio-Callback (nicht deterministisch, Dep fehlt) → eigener
  xorshift-Noise; Snapshot-Tests würden sonst flakken.
- ALSA-Gerät exklusiv/belegt oder kein Gerät im Container → Backend fällt
  auf `--list-devices`-Fehlertext + `--render-wav` zurück; CI hängt nie am
  Hardware-Stream (Render-Tests sind die Gates).
- Echtzeit-Xruns bei `--release`-Vergessen → alle Play-/Render-Gates mit
  `--release`; Doku-Hinweis im Walkthrough.
- `cargo upgrade` nach Einführung kann fundsp/cpal-APIs brechen → nur in
  Task T3, mit vollem Testlauf.

## 9. Kontext für einen unabhängigen Agenten (Pflichtlektüre)

1. `plan/20260920_01_audio/prompt.txt` — dieser Auftrag (Genre, Stack, Datei-/Tool-Regeln).
2. `plan/20260920_01_audio/plan.md` (diese Datei) + `task.md` — was/wie zu tun ist.
3. `plan/20260920_01_audio/deps.md` — Dependency-Registry (Org/Projekt-Notation).
4. `examples/25_dnb/source0/src/{main,lib,01_clock,02_bass,03_drums,04_arrange,05_mix,06_backend}.rs`
   — Implementierung (nicht neu schreiben, erweitern).
5. `examples/25_dnb/source0/tests/{render_snapshot,cli_smoke}.rs` — Vertrags-Tests.
6. fundsp 0.23 `examples/beep.rs` (lokal: Cargo-Registry) — kanonischer
   CPAL+fundsp-Pfad (`set_sample_rate`, `allocate`, `get_stereo`, Limiter-Kette).
7. cpal 0.18-Dok (`docs.rs/cpal`, `RustAudio/cpal`) — Sample-Format-Match,
   ALSA-Host, Callback-Vertrag (kein Panic/Alloc im Callback).
8. Live-Hardware: `aplay -l`, `/proc/asound/cards` (hier: 2× HDA-Intel),
   `lsusb`; Kernel-Log `dmesg` bei Geräte-Zweifeln.
9. `examples/24_embassy_stm32g4/plan/20260912_02_full/{plan,task}.md` —
   Vorlage für Plan-/Task-Stil (nicht für Inhalte).

## 10. Commit-Konvention (für alle Tasks)

Format: Conventional Commits, ein logischer Schritt pro Commit:
`feat|fix|test|docs|chore|refactor(<scope>): <kurz, imperativ, ≤72 Zeichen>`.
Body: was + warum, betroffene Peripherie/Modul, Validierung
(`cargo fmt --check`, `cargo clippy`, `cargo test`, Render-/ALSA-Ergebnis).
Footer: `Refs: plan/20260920_01_audio/task.md <ID>`.
Beispiele: `feat(clock): 174 bpm frame clock with bar sections`,
`feat(bass): detuned reese plus 808 sub`, `test(render): snapshot rms peak asserts`,
`docs(plan): walkthrough for dnb synth and alsa`.
Nie committen ohne grüne Gates des Tasks; keine fremden/untracked Dateien
anfassen; `Cargo.lock` (Binary) wird mitcommittet.
