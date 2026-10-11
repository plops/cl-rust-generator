# Tasks: Opus-Visualisierung — `source1`

Seriell abarbeiten; jeder Schritt endet gruen (`cargo test` +
`cargo fmt --check` + `cargo clippy`). „Modi“ aus der Prompt-Vorlage
sind hier die Ausgabemodi Kern/PNG/JSON/Show: jeder bekommt
Implementierung + Host-Tests, Show zusaetzlich den HIL-Nachweis
(xvfb); TUI-tasks entfallen (keine TUI).

## T0 — Geruest + Toolchain

- [ ] `source1/` als Cargo-Bin+Lib (`opus-vis`, Edition 2024)
      anlegen; Module `01`–`08` als Skelette, `09_show.rs` hinter
      Feature `show`.
- [ ] `cargo add opus-pure@0.2` (default, keine Features);
      `cargo add minifb@0.29 --optional --no-default-features
      --features x11`, Feature `show = ["dep:minifb"]`.
- [ ] `rustc/cargo --version` notieren; `cargo upgrade --dry-run`
      (cargo-edit ist installiert) muss „alles aktuell“ melden.
- [ ] `README.md`, `deps.md`-Skelett, `.gitignore` (`target/`,
      `*.png`, `*.json`-Artefakte) anlegen.

Verifikation: `cargo build` und `cargo build --features show` gruen.

## T1 — Modus Kern: TOC + Analyse (Implementierung)

- [ ] `01_types.rs`: `Error`, `Mode`, `Bandwidth`, `FrameMeta`,
      `Summary`, `CliConfig`.
- [ ] `02_toc.rs`: 32er-Tabelle (Modus/BW/Dauer), Kanaele aus Bit 2,
      leere Pakete ablehnen.
- [ ] `03_analyze.rs`: `analyze(path|reader)` — Ogg lesen, Zeitachse,
      kbps, Summary (Faktor, Overhead, Modusanteile).
- [ ] Unit-Tests: alle 32 Configs, Stereo-Bit, Summary-Rechenpfad mit
      synthetischen Frames.

Verifikation: `cargo test -p opus-vis toc` + `analyze` gruen.

## T2 — Modus Kern (Host-Tests)

- [ ] `tests/common/mod.rs`: Fixture-Generator (opus-pure Encoder;
      SILK via Voip+WB-erzwungen, Hybrid via Voip, CELT via Audio).
- [ ] `tests/analyze.rs`: generierte Datei → alle Frames erkannt,
      kbps ≈ Ziel-Bitrate (±30 %), Summary-Felder plausibel.
- [ ] `tests/data/ref.opus` (opusenc, committet) → Oeffnen, decodiert
      ohne Fehler, Config-Histogramm dokumentiert.

Verifikation: `cargo test` gruen (ohne `show`-Feature).

## T3 — Spektrum: Decode + STFT (Implementierung)

- [ ] `04_decode.rs`: `decode_mono()` — Decoder aus Head, Trim,
      Mono-Mix; Fehler bei >2 Kanaelen.
- [ ] `05_spectrum.rs`: Hann, Radix-2-FFT, Log-Binning (256 Bins,
      50 Hz–20 kHz), `stft()`-Iterator (Hop 960).
- [ ] Unit-Tests: FFT-vs.-DFT (N=64, 1e-4), 440-Hz-Sinus-Peak,
      Parseval (Fenster-Energie ±5 %).

Verifikation: `cargo test -p opus-vis spectrum decode` gruen.

## T4 — Spektrum (Host-Tests)

- [ ] `tests/spectrum.rs`: Sweep-Fixture (200→4000 Hz) → Peak-Bin
      steigt monoton; Rausch-Fixture → flaches Spektrum (max/min
      < 12 dB ueber Mittenband).
- [ ] Modi-Toenungs-Mapping: jeder STFT-Hop findet seinen Frame
      (Zeitgrenzen-Test mit 10-ms- und 20-ms-Paketen).

Verifikation: `cargo test` gruen.

## T5 — Modus PNG (Implementierung + Host-Tests)

- [ ] `07_png.rs`: Writer + Tests (Magic, IHDR, CRC, Mini-Roundtrip).
- [ ] `06_render.rs`: Layout (Kopf, Wasserfall, Bitraten-Lane,
      Legende), Colormap, 5x7-Font, Modus-Toenung, BW-Linie.
- [ ] `tests/render.rs`: 2-s-Fixture → PNG geschrieben; Pixel-Tests
      (Kopfzeile hell, Lane enthaelt Balken, Modus-Farbe vorhanden).

Verifikation: `cargo test` gruen; PNG per `ffprobe` lesbar.

## T6 — Modus JSON + CLI (Implementierung + Host-Tests)

- [ ] `08_cli.rs`: `-o`, `--json`, `--width`, `--show`, `-` (stdin);
      binaerer `main.rs`-Dispatch.
- [ ] `tests/cli.rs`: JSON-Schema (frames[], summary{}), stdout-PNG
      (`-o -`), Fehlerfaelle (fehlende Datei, kein Opus, >2ch).
- [ ] `README.md`: Verwendung + Beispiel-Kommandozeilen.

Verifikation: `cargo test` gruen; `cargo run -- demo.opus` erzeugt PNG.

## T7 — Modus Show (Implementierung + HIL-Nachweis)

- [ ] `09_show.rs`: Fenster (Titel = Dateiname), ESC/schliessen =
      Ende, `--frames N` (Test-Haken, Default u64::MAX).
- [ ] `scripts/smoke.sh`: Demo rendern → `ffprobe` prueft PNG;
      `xvfb-run opus-vis --show --frames 30` terminiert 0;
      `--json` gegen `python3 -m json.tool` validieren.
- [ ] `scripts/mkdemo.sh`: Demo-Builder (Voip-Segment + Audio-Segment
      via Binaer/Beispiel, oder opusenc-Fallback).

Verifikation: `scripts/smoke.sh` gruen (mit `--features show`).

## T8 — Hygiene + Commits + Walkthrough

- [ ] `cargo fmt`, `cargo clippy -- -D warnings`,
      `cargo upgrade` (alles aktuell), finale `cargo test --all-features`.
- [ ] Commits nach `plan.md` §7 (einer pro T-Schritt, CC-Format).
- [ ] `walkthrough.md` (Deutsch, Mermaid, Glossar, §1–4 aus Prompt).
- [ ] `deps.md` finalisieren (Versionen, DeepWiki-Notizen, Ausnahme
      opus-pure-nicht-indexiert).

Verifikation: `git log --oneline` zeigt T0…T8-Commits; alle Checks gruen.
