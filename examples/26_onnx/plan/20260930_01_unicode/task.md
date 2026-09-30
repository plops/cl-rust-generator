# task.md — 20260930_01_unicode: seriell abarbeitbare Schritte

Unicode-OCR-Roundtrip in `examples/26_onnx/source9/` (Plan:
`implementation_plan.md`, Deps: `deps.md`). Nach **jedem** Rust-Schritt die
Gates: `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`,
`cargo test`. Erst bei grünen Gates committen. Vor jedem Commit
`git status`: keine `*.onnx`, `models/`, `corpus/`, `target/` im Index.
„Nachweis“ = realer Lauf mit echten Modellen (headless `bench`) bzw. unter
Xvfb (UI). Jeder Generator-Modus bekommt Implementierung, Host-Tests und
Nachweis; die UI-(TUI-)Tasks kommen erst danach.

## T0 — Plan-Docs

- `implementation_plan.md`, `task.md`, `deps.md` konsistent.
- Commit: `docs(plan): unicode ocr round-trip plan, tasks and deps`.

## T1 — Assets: Modelle + Korpus

- Implementierung: `source9/.gitignore`, `scripts/fetch_models.sh`
  (gepinnte HF-Commits, SHA256, idempotent, Unifont-Check),
  `scripts/fetch_corpus.py` (uv, nur Stdlib: Wikipedia-Extrakte je Sprache,
  ~150 kB, User-Agent, Pausen), `scripts/README.md`.
- Test: `fetch_models.sh` zweimal (2. Lauf „cached“),
  `uv run scripts/fetch_corpus.py` → `corpus/<lang>.txt` für alle 15 Sprachen.
- Commit: `build(source9): model and wikipedia corpus fetch scripts`.

## T2 — Grundlagen: RNG, Sprachtabelle, Renderer

- Implementierung: `Cargo.toml` (ort, fontdue, macroquad), `01_rng.rs`,
  `02_lang.rs`, `06_render.rs` (+ vorläufig `03_corpus.rs`-Charset-Teil),
  `lib.rs`, Minimal-`main.rs`.
- Host-Tests: RNG deterministisch; Sprachcodes eindeutig; Renderer: Tinte
  liegt in der GT-Box, Rand bleibt weiß, RTL-Zeile ist gespiegelt,
  fehlende Glyphen werden erkannt.
- Nachweis: `cargo run -- render-test de /tmp/de.ppm` (Testbild ansehen).
- Commit: `feat(source9): rng, language table and unifont renderer`.

## T3 — OCR: Detektion, Erkennung, Modell-Registry

- Implementierung: `07_detect.rs` (aus source5, Eingang RGBA),
  `08_recognize.rs` (Pfad-Laden, Dict mit YAML-Escapes, Konfidenz),
  `09_models.rs` (auto/universal, lazy Cache).
- Host-Tests: DBNet-Synthetik (aus source5), CTC, Dict-Escape `''''` → `'`,
  Modellwahl je Sprache.
- Nachweis: Integrationstest liest ein gerendertes „Hallo Welt“ korrekt.
- Commit: `feat(source9): paddleocr detection, recognition and model registry`.

## T4 — Metriken + Statistik

- Implementierung: `10_metrics.rs`, `11_stats.rs`.
- Host-Tests: Levenshtein (Einfügen/Löschen/Ersetzen), Alignment liefert
  `ß→B`, Normalisierung (Leerzeichen, Vollbreite), Box-Zuordnung (Split-Box,
  FP), Aggregation + Markdown-Report.
- Commit: `feat(source9): cer, alignment, box matching and statistics`.

## T5 — Modus `pangram` + Engine

- Implementierung: `05_generate.rs` (GenMode::Pangram), `12_engine.rs`.
- Host-Tests: Zeilen passen in die Breite; Pangramme nur aus Charset.
- Nachweis: `tests/roundtrip.rs` — alle 15 Sprachen laufen durch; de/en/fr
  Pangramm bei 32 px CER < 5 %.
- Commit: `feat(source9): pangram generator and round-trip engine`.

## T6 — Modus `words` (Zufallswörter aus dem Korpus)

- Implementierung: Korpus-Laden + Token-Filter (`03_corpus.rs`), GenMode::Words.
- Host-Tests: nur erlaubte Tokens; leerer Korpus → Fallback Pangramm.
- Nachweis: `bench --gen words --lang de,ru,th --samples 5`.
- Commit: `feat(source9): corpus loader and random word generator`.

## T7 — Modus `markov`

- Implementierung: `04_markov.rs`, GenMode::Markov.
- Host-Tests: Training/Sampling deterministisch, nur gelernte Übergänge,
  Backoff bei unbekanntem Kontext.
- Nachweis: `bench --gen markov --lang all --samples 3` (Beispielzeilen im TSV
  ansehen: plausibel?).
- Commit: `feat(source9): character n-gram markov text generator`.

## T8 — Modus `chars` (Sonderzeichen gleichverteilt)

- Implementierung: GenMode::Chars.
- Host-Tests: nur Charset-Zeichen, Pseudowörter 2–8 Zeichen.
- Nachweis: `bench --gen chars --lang de --samples 20` → Zeichen-Statistik
  zeigt Umlaute/ß.
- Commit: `feat(source9): uniform character generator for special glyphs`.

## T9 — Headless-Benchmark-CLI

- Implementierung: `13_cli.rs`, `14_bench.rs`, `main.rs`-Verdrahtung (TSV).
- Host-Tests: Parser (Defaults, Listen, Fehler); `tests/cli.rs` (Binary,
  Exit 0, Tabellenkopf).
- Nachweis: Release-Benchmark aller Sprachen × Generatoren → Zahlen für den
  Walkthrough (`source9/bench.md`).
- Commit: `feat(source9): headless bench command with markdown and tsv report`.

## T10 — UI-Zustand (Tasten-Logik)

- Implementierung: `15_ui_state.rs` (`Settings`, `Action`, `apply`).
- Host-Tests: Sprache zyklisch, Größenstufen geklemmt, Modi zyklisch, Pause.
- Commit: `feat(source9): pure ui state machine for key actions`.

## T11 — UI-Fenster + Xvfb-Nachweis

- Implementierung: `16_ui_draw.rs`, `17_ui_loop.rs` (Worker-Thread),
  `scripts/smoke_xvfb.sh`.
- Xvfb-Nachweis: Start, Tasten `→ G V V M ↑ Space N`, Screenshots
  (Text, Boxen, nichts), `Escape` gehalten → Exit 0 + Report auf stdout.
- Commit: `feat(source9): interactive window with live statistics`.

## T12 — Aufräumen

- `cargo upgrade` (cargo-edit), fmt/clippy/test, Dateien ≤~300 Zeilen,
  `README.md` (source9 + `26_onnx/README.md`-Eintrag).
- Commit: `chore(source9): upgrade dependencies and polish docs`.

## T13 — Walkthrough

- `plan/20260930_01_unicode/walkthrough.md` (Deutsch, Mermaid, Code;
  Struktur: 1. implementiert, 2. geänderte Entscheidungen, 3. Learnings +
  Erweiterungen, 4. Docker-Pakete).
- Commit: `docs(plan): german walkthrough for unicode ocr round-trip`.
