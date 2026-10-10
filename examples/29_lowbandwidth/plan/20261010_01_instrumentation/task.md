# Tasks: Instrumentierung — `source10_instrumentation`

Seriell abarbeiten; jeder Schritt endet grün (Tests + `cargo fmt --check` +
`cargo clippy`). Modi/TUI aus der Prompt-Vorlage entfallen hier — stattdessen:
Gerüst → Crate → Server → Client → Tools → Smoke → Walkthrough.

## T0 — Umgebung + Bestandsaufnahme

- [ ] `cargo --version`, `rustc --version` notieren; `cargo upgrade --dry-run`
      (via `cargo install cargo-edit` falls nötig) zur Dep-Prüfung.
- [ ] `cargo test --workspace` in `source9_gpu` grün (Baseline, ohne X11).
- [ ] `plan.md` gelesen; offene Fragen aus §4 entschieden.

Verifikation: Baseline-Testergebnis im Terminal gesehen.

## T1 — Gerüst: Copy + Workspace + Modelle + Doku-Skelett

- [ ] `source9_gpu` → `source10_instrumentation` kopieren (ohne `target/`,
      ohne `.git`-Anteile); `models` → Symlink auf `../source7_mvp/models`.
- [ ] Root-`README.md` (source10-Zeile) + `.gitignore` (models-Pfad) pflegen.
- [ ] `source10_instrumentation/README.md` + `deps.md` anlegen (Dep-Tabelle =
      source9-Stand, keine neuen Deps, DeepWiki-Muster).
- [ ] `cargo build --workspace` grün.

Verifikation: Build grün, `ls models/PP-OCRv6_small_det.onnx` ok.

## T2 — `lbw-log`: Record-Format + IO + Unit-Tests (TDD)

- [ ] `log/Cargo.toml` (nur serde/bincode-Pfade aus dem Baum), `src/lib.rs`,
      `01_record.rs` (`Stamp`, `Dir`, `MsgKind`, `LogRecord`, FNV-1a),
      `02_io.rs` (`Writer`/`Reader`, Magic `LBWLOG10`).
- [ ] Unit-Tests: Record-Roundtrip aller Varianten, korrupte Magic/Länge =
      Fehler, partielles File-Ende = sauberes EOF.
- [ ] In Workspace-`Cargo.toml` als Member eintragen.

Verifikation: `cargo test -p lbw-log` grün.

## T3 — Server-Recording (Implementierung)

- [ ] `server/01_config.rs`: `--record <pfad>` (Option, Default None).
- [ ] `server/08_record.rs`: teilbarer no-op-fähiger `Recorder`
      (Arc/Mutex/BufWriter), `Session`/`Msg`/`Frame`/`Inject`/`Gap`/`End`.
- [ ] `server/07_session.rs`: Pipeline-Timings pro Frame (Capture/det/rec/
      Mask+Diff/Encode/Send) + `Msg`-Logs (Senden re-encodiert, Empfang mit
      Body); `input_loop` misst Empfang→Injektion.
- [ ] `server/main.rs`: Recorder aus `--record` bauen, pro Verbindung
      `Session`/`End` schreiben.
- [ ] `server/Cargo.toml`: Dep auf `lbw-log` (Pfad).

Verifikation: `cargo build -p lbw-server` grün.

## T4 — Server-Recording (Host-Tests)

- [ ] `server/tests/loopback.rs`: Variante mit Recording in Temp-Datei
      (`std::env::temp_dir`, kein neues Dep): Session gesehen, Vollbild-Frame
      mit Tile-Stat, `End` vorhanden; Bodies re-dekodierbar zu `ServerMsg`.
- [ ] Bestehende Server-Tests unverändert grün (Signatur-Updates ok, keine
      abgeschwächten Assertions).

Verifikation: `cargo test -p lbw-server` grün.

## T5 — Client-Recording (Implementierung + Host-Tests)

- [ ] `client/01_config.rs`: `--record <pfad>`.
- [ ] `client/06_record.rs`: `Recorder` (wie Server, Client-Seite).
- [ ] `client/03_net.rs`: `Net::connect_recorded`, Gap-Events mit Down-Dauer,
      Empfangs-`Msg` + Decode-ms, Sende-`Msg` mit Zeit.
- [ ] `client/main.rs`: `--record` verdrahten.
- [ ] `client/tests/loopback.rs`: Recording-Assertions (Connected/Gap,
      Tile-`Msg` + `Decode`, gesendete Inputs im Log).
- [ ] `client/Cargo.toml`: Dep auf `lbw-log` (Pfad).

Verifikation: `cargo test -p lbw-client` grün.

## T6 — Tools: `lbw-logstat` + `lbw-replay` (Implementierung)

- [ ] `log/03_stats.rs`: Durchsatz (gesamt/Art/s-Timeline, 6-kB/s-Budget-Quote),
      Gaps (Liste, Down-Summe), Dedup (Text-/Tile-Hash-Top-N, Byte-Verschwendung),
      Latenzen (Server-Stufen, Client Input→sichtbar, Offset-Schätzer),
      Tile-Bewegung (BBox-Deltas).
- [ ] `log/src/bin/lbw-logstat.rs`: Human-Summary + `--json` (+ `--csv`).
- [ ] `client/src/bin/lbw-replay.rs`: echter `Decoder` + `Scene`,
      Canvas-Hash, `--ppm <dir>`, `--realtime`.
- [ ] `client/examples/probe.rs`: `--record`-Option.

Verifikation: beide Bins bauen; `logstat --help`/`replay --help` laufen.

## T7 — Tools (Host-Tests)

- [ ] `log`-Tests für `03_stats.rs` auf synthetischem Log (deterministische
      Zahlen: B/s-Timeline, Gap-Dauer, Dedup-Top-1, Latenz-p50).
- [ ] `logstat`-Golden-Test: fixer Input → erwartete Summary-Zeilen.
- [ ] Replay-Determinismus-Test: Server-Loopback-Log → zweimal replayen →
      gleicher Canvas-Hash (Test in `client/tests/replay.rs`, Headless).

Verifikation: `cargo test --workspace` grün.

## T8 — Smoke (HIL-Nachweis) + Hygiene

- [ ] `xvfb`/`xterm` installieren (apt); `scripts/smoke_record.sh`:
      Xvfb + xterm/xev + Server/Probe mit `--record` + `logstat` beider Logs
      + `replay` des Server-Logs.
- [ ] `scripts/smoke_xvfb.sh` (alt) weiter grün (unverändert).
- [ ] `cargo fmt`, `cargo clippy --workspace -- -D warnings` (eigener Code),
      `cargo upgrade` prüfen, `collect.sh` auf neue Dateien erweitern.

Verifikation: beide Smokes OK; fmt/clippy sauber.

## T9 — Commits + Walkthrough

- [ ] Commits nach `plan.md` §6 (Conventional, deutsch, jeder grün).
- [ ] `plan/20261010_01_instrumentation/walkthrough.md` (deutsch, didaktisch,
      Mermaid, Code-Beispiele; Struktur: 1. Implementiert, 2. Umentscheidungen,
      3. Learnings/Erweiterungen, 4. Dockerfile-Pakete).
- [ ] Abschluss: `cargo test --workspace` + beide Smokes grün, alles committed.

Verifikation: `git log --oneline` zeigt die Kette; `git status` sauber.
