# deps.md — 20260920_01_audio
Registry für DeepWiki-Abfragen: GitHub-Notation `<organization>/<projekt>`.
Nur direkt genutzte Deps (transitive nur wenn Abfrage-relevant).
Stand: 2026-09-20, jeweils neueste beim Einführen (Task S0/T3).

| Crate/System | Org/Projekt bzw. Paket | Version (S0) | Zweck |
|---|---|---|---|
| fundsp | SamiPerttu/fundsp | 0.23.0 | DSP-Graph: Reese (saw+lowpass+tanh), 808, Sirene; `set_sample_rate`/`allocate`/`get_stereo` |
| cpal | RustAudio/cpal | 0.18.2 | ALSA-Output-Stream, Sample-Format-Match F32/I16/U16 |
| alsa | diwic/alsa | 0.11 (transitiv) | cpal-ALSA-Backend auf Linux |
| clap | clap-rs/clap | 4.x | CLI mit derive (`--device/--render-wav/…`) |
| hound | ruvec/hound | 3.x | Offline-WAV-Render für Tests ohne Hardware |
| anyhow | dtolnay/anyhow | 1.x | Fehler-Propagierung Backend/CLI |
| serde | serde-rs/serde | 1.x (derive) | Preset-TOML (De-)Serialisierung |
| toml | toml-rs/toml | 1.x | Preset-Datei lesen/schreiben |
| ratatui | ratatui/ratatui | 0.29+ | TUI Screens/Transport (Bin `dnb_tui`) |
| crossterm | crossterm-rs/crossterm | 0.28+ | TUI-Backend (Bin `dnb_tui`) |
| libasound2-dev | Debian/alsa-lib (apt) | system | cpal-ALSA-Build-Header (`sudo apt install libasound2-dev`) |
| alsa-utils | Debian/alsa-utils (apt) | system | `aplay -l`, Anspiel-Smoke (Task S4) |
| pkg-config, build-essential | system (apt) | system | cpal/alsa-Sys-Build |

DeepWiki-Abfrage-Muster: `SamiPerttu/fundsp` (Graph-/Filter-API, `beep.rs`-Pfad),
`RustAudio/cpal` (Stream-/Callback-Vertrag, ALSA-Host), `diwic/alsa` nur bei
ALSA-Fehlersuche, `clap-rs/clap` (derive-CLI), `ruvec/hound` (WAV-Schreiben).
NICHT eingeführt (bewusst): `rand` (deterministischer Eigen-Noise statt
Prompt-`rand::random`), `ratatui` erst in Task T2.
