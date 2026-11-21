# Abhängigkeiten (`deps.md`)

Alle Crates im Format `<organization>/<project>`, damit der DeepWiki-MCP
(`mcp__deepwiki__ask_wiki_question`, Parameter `repoName`) direkt befragt
werden kann. Versionen = Stand 2026-11-21 (`cargo search`, danach
`cargo upgrade`).

| Crate | Version | DeepWiki-Slug | GitHub | Rolle |
|---|---|---|---|---|
| `ratatui` | 0.30 | `ratatui/ratatui` | https://github.com/ratatui/ratatui | TUI-Widgets, `TestBackend` für Render-Tests |
| `crossterm` | 0.29 | `crossterm-rs/crossterm` | https://github.com/crossterm-rs/crossterm | Terminal-Backend, Tastatur-Events (`event::poll/read`) |
| `cpal` | 0.18 | `RustAudio/cpal` | https://github.com/RustAudio/cpal | Audio-Ausgabe (ALSA-Host unter Linux), `build_output_stream` |
| `rtrb` | 0.4 | `mgeier/rtrb` | https://github.com/mgeier/rtrb | Lock-freier SPSC-Ringpuffer UI ⇄ Audio-Thread |
| `hound` | 3.5 | `ruuda/hound` | https://github.com/ruuda/hound | WAV schreiben (Offline-Render) und lesen (Sample-Instrumente) |
| `anyhow` | 1 | `dtolnay/anyhow` | https://github.com/dtolnay/anyhow | Fehlerbehandlung im Binary |

Bewusst **nicht** verwendet:

| Kandidat | Slug | Warum nicht |
|---|---|---|
| `fundsp` | `SamiPerttu/fundsp` | Mächtige DSP-Graph-Bibliothek, aber für Oszillator + Biquad + Waveshaper überdimensioniert; eigener DSP-Code bleibt lesbar und testbar. |
| `clap` | `clap-rs/clap` | CLI hat fünf Optionen – Handparser reicht. |
| `serde` | `serde-rs/serde` | Song-Format ist ein eigenes, tracker-typisches Textformat. |

Wichtige API-Fakten (aus Quellcode in `~/.cargo/registry` verifiziert):

- cpal 0.18: `SampleRate = u32`, `Device::description()`/`Device::id()`
  ersetzen `name()`; Fehler sind ein einheitliches `cpal::Error` mit
  `kind()` (`ErrorKind::Xrun`, …). Siehe `examples/beep.rs` im Crate.
- rtrb 0.4: `RingBuffer::<T>::new(cap) -> (Producer, Consumer)`,
  `push` liefert `PushError::Full(v)`, `pop` liefert `PopError::Empty`.
  Push/Pop allozieren nicht; das Droppen eines Endes ist **nicht** als
  echtzeitsicher dokumentiert (DeepWiki-Antwort) → Endpunkte nie im
  Audio-Callback droppen.
- Systempaket: `libasound2-dev` (ALSA-Header für `alsa-sys`).
