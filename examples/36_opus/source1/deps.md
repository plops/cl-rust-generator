# Abhaengigkeiten (`<organization>/<projekt>`)

Zwei Crates, sonst nur `std`: TOC-Parser, FFT, PNG-Writer, Font und
CLI-Parsing sind eigener Code. `cargo upgrade --dry-run` (2026-10-11):
beide auf `latest`. Stand 2026-10-13 (`05_verstehen`: Fakten, Report,
JSON-Schema-2 — alles `std`-only): weiter genau diese zwei, `cargo
tree` ohne `show` zeigt nur `opus-pure 0.2.2`.

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
- **DeepWiki** (`stephenberry/opus-pure`, abgefragt 2026-10-11):
  bestaetigt unser Muster 1:1 (Reader → Decoder+Trim → `read_packet`
  → `decode` → `trim.keep`); `OggPacket` = `{data, page_granule,
  end_of_stream}`; Pre-Skip/End-Trim via `Trim`; `OpusMSDecoder`
  fuer Multistream (wir lehnen >2ch weiter ab);
  `read_packet_into()` als alloc-freie Reserve (ungenutzt);
  `Error::InvalidStream` bei defektem Header, CRC-Fehler = Fehler.
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

## SILK-Fixtures (`06_silk`, keine Dep-Aenderung)

- `tests/data/silk_sample.opus` (30 KB, committet): 10 s Sprache aus
  `silk.webm`-Minute 1, 501 Pakete alle Config 9 (SILK WB 20 ms,
  mono, -25,6 dB mittlerer Pegel, keine Stille).
- Voll-Datei `silk_silk.opus` (3,3 MB, ungecommittet, neben der Quelle):
  61641 Pakete, alle Config 9, kodiert mit
  `ffmpeg -i silk.webm -map 0:a -ac 1 -ar 16000 -c:a libopus
  -application voip -b:a 24k -frame_duration 20 silk_silk.opus`
  (16 kHz erzwingt SILK-only; Gegenprobe `silk.opus` = 100 % Hybrid).
- `tests/data/hybrid_sample.opus` (30 KB, committet): 10 s Sprache aus
  `silk.opus`-Minute 1, 501 Pakete alle Config 15 (Hybrid FB 20 ms,
  mono) — Orakel-Fixture fuer S3 (Split, Cutoff-Stille, Determinismus).

Stand 2026-10-13 (`06_silk` S3: Hybrid-Modul `05c`, Puls-Fix): weiter
genau diese zwei Crates; `cargo tree` (Default-Features) zeigt nur
`opus-pure 0.2.2` (`minifb` optional, deaktiviert).
