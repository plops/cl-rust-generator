# Abhaengigkeiten (`<organization>/<projekt>`)

## `stephenberry/opus-pure` — Ogg-Demux + Opus-Codec (Pflicht)

```toml
opus-pure = "0.2"
```

- **Einsatzzweck**: Ogg lesen (`OggOpusReader`), decodieren
  (`OpusDecoder`, `Trim`), Fixtures encodieren (`OpusEncoder`,
  `OggOpusWriter`). Einzige Laufzeit-Dep des Kerns.
- **DeepWiki**: Repo nicht indexiert (MCP: „Repository not found“) —
  stattdessen Quell-Inspektion + eigene Spikes (siehe `plan.md` §5).
- **Version**: 0.2.2 (neueste).

## `emoon/rust_minifb` — X11-Fenster (optional, Feature `show`)

```toml
minifb = { version = "0.29", default-features = false,
           features = ["x11"], optional = true }
```

- **Einsatzzweck**: `--show`-Fenster (Puffer einblenden, ESC = Ende).
- **Minimalitaet**: nur `x11` (zieht `x11-dl` + `libc`), kein Wayland.
- **Version**: 0.29.0 (neueste).
