# Abhaengigkeiten (`<organization>/<projekt>`)

Zwei Crates, sonst nur `std`: TOC-Parser, FFT, PNG-Writer, Font und
CLI-Parsing sind eigener Code. `cargo upgrade --dry-run` (2026-10-11):
beide auf `latest`.

## `stephenberry/opus-pure` — Ogg-Demux + Opus-Codec (Pflicht)

```toml
opus-pure = "0.2"   # gelockt: 0.2.2, null transitive Deps
```

- **Einsatzzweck**: Ogg lesen (`OggOpusReader`), decodieren
  (`OpusDecoder`, `Trim`), Fixtures encodieren (`OpusEncoder`,
  `OggOpusWriter`, auch als Dev-Dep fuer Tests/Example).
- **Warum diese**: rein-Rust, Edition 2024, null transitive Deps;
  validiert per Kreuzvergleich (Bit-identisch zu `rusty-opus`,
  1e-5 zu `opus-decoder`, Sinus-RMS exakt) und gegen `opusenc`-Datei
  plus 12-min-YouTube-Datei (38655 Pakete, decodiert sauber).
- **API-Muster**: `OggOpusReader::new(r)` → `head()` →
  `head.decoder(48_000)` + `Trim::new(&head, 48_000, ch)` →
  `read_packet()` → `decode(&data, MAX_PACKET_SAMPLES, &mut block)`
  → `trim.keep(&packet, &block)`.
- **DeepWiki**: Repo nicht indexiert (MCP: „Repository not found“) —
  dokumentierte Ausnahme; stattdessen Quell-Inspektion + Spikes.
- **Version**: 0.2.2 (neueste, `cargo upgrade` bestaetigt).

## `emoon/rust_minifb` — X11-Fenster (optional, Feature `show`)

```toml
minifb = { version = "0.29", default-features = false,
           features = ["x11"], optional = true }
```

- **Einsatzzweck**: `--show`-Fenster (`Window::new`,
  `update_with_buffer`, ESC/schliessen = Ende).
- **Minimalitaet**: nur `x11` (zieht `x11-dl` + `libc`), kein Wayland;
  `x11-dl` laedt libX11 erst zur Laufzeit (Build ohne X11-Header ok).
- **API-Muster**: Puffer `Vec<u32>` in `0x00RRGGBB`, `set_target_fps(30)`.
- **Version**: 0.29.0 (neueste, `cargo upgrade` bestaetigt).

## Abgelehnte Kandidaten (Protokoll, keine Deps)

- `Remade-With-Rust/rusty-opus` (1.0.1, Decoder ok, aber ohne Ogg-Schicht).
- `TadeuszWolfGang/Rusopus` (`opus-decoder` 0.1.1, nur Decoder).
- `RustAudio/ogg` (0.9.2, nur Container — `opus-pure` kann beides).
- `SpaceManiac/opus-rs` (`opus` 0.4.0, braucht System-`libopus`).
