# source9 — Unicode-OCR-Roundtrip

Rendert Schriftproben in 15 Sprachen mit GNU Unifont in ein 640×640-Bild,
liest sie mit PaddleOCR (ONNX) wieder ein und misst Fehler (CER) und
Geschwindigkeit. Interaktives Fenster oder headless Benchmark.

```bash
./scripts/fetch_models.sh          # ONNX-Modelle (~96 MB, nach models/)
uv run scripts/fetch_corpus.py     # Wikipedia-Texte (~2,3 MB, nach corpus/)

cargo run -q --release -- bench --gen all --lang all --samples 5 --lines 4
cargo run -q --release             # Fenster (Tasten siehe unten)
```

## Tasten (Fenster)

| Taste | Wirkung |
|---|---|
| `→` / `←` | nächste / vorige Sprache |
| `R` | Zufallssprache pro Sample an/aus |
| `G` | Generator: pangram → words → markov → chars |
| `V` | Anzeige: Text → Text + Boxen → nichts |
| `↑` / `↓` | Schriftgröße 16/24/32/48 |
| `M` | Modell: auto (sprachspezifisch) ↔ universal (PP-OCRv6) |
| `Space` | Pause / weiter |
| `N` | ein Sample (auch in Pause) |
| `C` | Statistik löschen |
| `Esc` / `Q` | Ende, Report auf stdout |

## Aufbau (`src/`, Nummern = Datenfluss)

| Datei | Inhalt |
|---|---|
| `01_rng.rs` | SplitMix64 (statt `rand`-Crate) |
| `02_lang.rs` | Sprachtabelle (Schrift, Modell, Pangramme) |
| `03_corpus.rs` | Zeichensatz-Schnitt + Korpus-Tokens |
| `04_markov.rs` | Zeichen-Trigramm mit Backoff |
| `05_generate.rs` | Modi pangram/words/markov/chars + Umbruch |
| `06_render.rs` | Unifont-Rasterung, Canvas, Ground Truth |
| `07_detect.rs` | DBNet-Detektion (aus source5) |
| `08_recognize.rs` | CTC-Erkennung + Wörterbuch + Konfidenz |
| `09_models.rs` | Modellpfade, auto/universal, lazy Cache |
| `10_metrics.rs` | CER, Alignment, Box-Zuordnung |
| `11_stats.rs` | Aggregation, Zeichen-Statistik, Report |
| `12_engine.rs` | ein Sample: erzeugen → OCR → messen |
| `13_cli.rs` | Argumente (handgeschrieben) |
| `14_bench.rs` | Benchmark-Schleife + TSV-Log |
| `15_ui_state.rs` | reine Tasten-Logik |
| `16_ui_draw.rs` | Bild, Boxen, HUD, Panel |
| `17_ui_loop.rs` | Fenster-Schleife + Worker-Thread |

## Tests & Nachweise

```bash
cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test
./scripts/smoke_xvfb.sh   # Fenster unter Xvfb (braucht xvfb, xdotool, scrot)
```

`bench.md` hält die Release-Messwerte fest (alle Sprachen × Generatoren).
Plan und Walkthrough: `plan/20260930_01_unicode/`.
