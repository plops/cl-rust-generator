# Plan: Instrumentierung + Session-Recording — `source10_instrumentation`

Stand: 2026-10-10. Basis: `source9_gpu` (Protokoll v2, 1280×720, Hybrid-OCR).
Ziel: `source10_instrumentation` — derselbe Remote-Desktop, aber jede
übertragene Nachricht plus Timing-/Performance-Metriken landet in einer
Logdatei, die späteres Wiederabspielen und Offline-Analyse erlaubt. Damit soll
der ~6-kB/s-Kanal (komprimierte SSH-Strecke, tlw. stockend) charakterisiert und
später Latenz, Robustheit und Datenrate optimiert werden. Direkt in Rust
geschrieben (kein Transpiler, s. §7).

## 1. Befund: Was source9 an Messpunkten bereits hergibt (verifiziert)

- Server (`07_session.rs`): OCR-Zeiten via `Ocr::last_ms` (det/rec in ms),
  Kachel-Bytezähler im `-v`-Log; Eingaben laufen im eigenen Thread.
- Client (`03_net.rs`): `Event::Tile` trägt `bytes` (AV1-Nutzdaten);
  `04_scene.rs` zählt Kacheln/Gesamtbytes fürs HUD.
- Framing (`02_framing.rs`): `write_msg` liefert Leitungsbytes; `FrameReader`
  liefert rohe Bodies — beide Seiten können also alles mitschneiden, ohne das
  Protokoll zu ändern.
- Protokoll v2 bleibt unangetastet: `common/` wird **nicht** geändert
  (Nachweis per `diff -r` im Walkthrough).

## 2. Implementierungsvorschlag

1. `source9_gpu` nach `source10_instrumentation` kopieren (ohne `target/`),
   `models/` als Symlink auf `../source7_mvp/models` (echte Dateien dort).
   `.gitignore` + Root-`README.md` ergänzen.
2. Neue Crate `lbw-log` (nur `serde`/`bincode` aus dem Baum, **keine neuen
   externen Deps**):
   - `01_record.rs`: `LogRecord`-Enum (`Session`, `Msg`, `Frame`, `Decode`,
     `Inject`, `Gap`, `End`), `Stamp` (Wall-µs + Mono-µs), FNV-1a-Hash.
   - `02_io.rs`: Datei = Magic `LBWLOG10` + `[u32-LE-Länge][bincode-Body]`-Strom
     (selbes Framing-Prinzip wie das Protokoll); `Writer`/`Reader`.
   - `03_stats.rs`: Offline-Auswertung (Durchsatz, Gaps, Dedup, Latenzen,
     Tile-Bewegung), JSON-Export.
   - Binär `lbw-logstat`: Human-Summary + `--json` (+ `--csv`-Timeline).
3. Server: `--record <pfad>` (eine Datei pro Lauf, mehrere Verbindungen als
   `Session`/`End`-Abschnitte). Neues Modul `08_record.rs` (teilbarer,
   no-op-fähiger Recorder). `serve_client` misst pro Frame: Capture-,
   Det-, Rec-, Mask/Diff-, Encode-, Sende-ms + Text-/Tile-Statistik (auch bei
   Standbild-Stille, damit „gesunde Stille“ von „Ausfall“ unterscheidbar ist);
   `input_loop` misst Empfang→Injektion je Eingabe.
4. Client: `--record <pfad>`, neues Modul `06_record.rs`. Der Netz-Thread
   loggt Connects/Disconnects (mit Down-Dauer), jede empfangene Nachricht
   (mit AV1-Decode-ms) und jede gesendete Eingabe (mit Sendezeit).
5. Binär `lbw-replay` (Client-Paket): spielt `Msg`-Records durch echten
   `Decoder` + `Scene`, meldet Canvas-Hash + Statistik, optional `--ppm`
   (Rohbilder ohne neue Deps) und `--realtime`.
6. `probe`-Example: `--record`-Option; neues Skript `scripts/smoke_record.sh`
   (Xvfb + Server/Probe mit Recording + `logstat` + `replay`).
7. Tests: Unit (Record-Roundtrip, Stats auf synthetischen Logs), Loopback mit
   Recording (Server + Client), Replay-Determinismus, `logstat`-Golden-Test.

## 3. Latenz-Definitionen (alle ohne Uhrensynchronisation messbar)

| Latenz | Uhr | Definition |
|---|---|---|
| Input→Photon (Client) | Client-mono | letzte Eingabe-Sendung → nächste sichtbare Änderung (Tile/Text) |
| Bild-Pipeline (Server) | Server-mono | Capture → Tile-gesendet, mit Stufen (det/rec/encode/…) |
| Text-Pipeline (Server) | Server-mono | Capture → AddText-gesendet |
| Input→Injektion (Server) | Server-mono | Empfang → enigo-fertig |
| Ende-zu-Ende (geschätzt) | beide | via Offset-Schätzung (Min-Filter über gematchte Inputs, NTP-Prinzip) |

Nicht messbar ohne Protokolländerung: echte Einweg-Laufzeit pro Paket.
Bewusst verzichtet (s. §7): Der Schätzer reicht zur Charakterisierung.

## 4. Offene Requirements / Vorschläge (mit Empfehlung)

| # | Frage | Empfehlung |
|---|---|---|
| 1 | Protokoll v3 mit Seq-Nummern/Ping für exakte RTT? | Nein — v2 bleibt; Offset-Schätzung genügt, kein Kompatibilitätsbruch |
| 2 | Auch SSH-komprimierte Echtbytes messen (Kanal sieht weniger als TCP)? | Nur dokumentieren; echte Messung braucht Paketmitschnitt (Follow-up) |
| 3 | AV1-Motion-Compensation schon einbauen? | Nein — erst `logstat`-Bewegungsdaten sammeln, dann entscheiden |
| 4 | Text-Dedup schon sendeseitig (Hash-Cache)? | Nein — erst messen, wie oft sich Texte wiederholen |
| 5 | Aufnahme am Server, Client oder beiden? | Beide (je `--record`); Server sieht Pipeline, Client sieht Kanal + Gaps |
| 6 | Live-Metriken (HUD/Endpoint) statt nur Datei? | Nein — Datei first; HUD kann später aus denselben Records lesen |
| 7 | TUI zur Analyse? | Nein — `logstat` + JSON für externe Plots genügen |
| 8 | Video-Replay (MP4) statt PPM? | Nein — keine neuen Deps; PPM reicht für Stichproben |

## 5. Kontext-Dateien für den ausführenden Agenten

Referenz (`source9_gpu/`, lesen vor dem Ändern):

- `common/src/01_types.rs` — Protokolltypen v2 (bleiben gleich).
- `common/src/02_framing.rs` — Framing-Muster (Vorbild für `02_io.rs`).
- `common/src/03_yuv.rs`, `lib.rs` — Verständnis (unverändert übernehmen).
- `server/src/01_config.rs` — CLI-Muster (→ `--record` dazu).
- `server/src/02_capture.rs` — `FrameSource`-Trait (Messpunkt Capture).
- `server/src/03_ocr.rs` — `last_ms`-Timing (nur lesen, **nicht** anfassen:
  638 Zeilen, Split-Regel!); Padding/CTC-Verständnis.
- `server/src/04_tiles.rs` — `dirty_bbox`/Maske (Messpunkt Diff).
- `server/src/05_av1.rs` — Encoder (Messpunkt Encode).
- `server/src/06_input.rs` — `Injector::handle` (Messpunkt Injektion).
- `server/src/07_session.rs` — Schleife (→ Recorder-Hooks, Pipeline-Timings).
- `server/src/main.rs`, `lib.rs` — Verdrahtung (→ `--record`-Datei).
- `server/tests/loopback.rs` — Session-Testmuster (→ Recording-Assertions).
- `server/tests/models.rs`, `padding.rs` — GPU-Testmuster (unverändert).
- `client/src/01_config.rs` — CLI (→ `--record` dazu).
- `client/src/02_av1.rs` — `Decoder` (→ Decode-Timing, Replay-Nutzung).
- `client/src/03_net.rs` — Netz-Thread (→ Recorder-Hooks, Gap-Events).
- `client/src/04_scene.rs` — `Scene` (Replay-Nutzung, Hash für Determinismus).
- `client/src/05_app.rs` — UI-Schleife (unverändert lassen).
- `client/src/main.rs`, `lib.rs` — Verdrahtung.
- `client/tests/loopback.rs` — Net-Testmuster (→ Recording-Assertions).
- `client/examples/probe.rs` — Headless-Probe (→ `--record`-Option).
- `scripts/smoke_xvfb.sh` — E2E-Vorlage (→ `smoke_record.sh`).
- `Cargo.toml`-Dateien, `deps.md`, `README.md` — Workspace/Dep-Vorlagen.

Externe Referenzen:

- Keine neuen Crates → keine neuen DeepWiki-Abfragen nötig. Query-Muster in
  `source10_instrumentation/deps.md` (z. B.
  `ask_wiki_question(repoName="pykeio/ort", question="...")`).
- Transpiler-Doku (`plops/cl-rust-generator`) nur falls §7 revidiert wird.

## 6. Commit-Konvention (verbindlich)

Conventional Commits (`feat:`/`fix:`/`docs:`/`test:`), deutsche Messages,
Betreff ≤ 72 Zeichen, Body mit Was/Warum/Verifikation:

```
feat: lbw-log-Crate mit Record-Format und IO in source10

Was: ...
Warum: ...
Verifikation: cargo test -p lbw-log, cargo clippy, cargo fmt --check.
```

Geplante Commits: (1) `docs: Plan/Tasks ...`, (2) `feat: source10-Gerüst
(Copy + lbw-log-Format)`, (3) `feat: Server-Recording ...`, (4) `feat:
Client-Recording ...`, (5) `feat: logstat + replay ...`, (6) `test: ...`
(falls separat), (7) `docs: Walkthrough ...`. Jeder Commit baut
(`cargo build --release`) und hält Tests grün.

## 7. Festlegungen (begründet)

- **Kein Protokollbruch (v2 bleibt):** Alle Latenzen sind einseitig auf einer
  Uhr messbar; Kreuz-Uhr-Werte werden per Min-Filter geschätzt. Ein v3 mit
  Seq/Ping würde Client+Server koppeln und alle Tests umbrechen — ohne
  Mehrwert für die Charakterisierung.
- **Kein Transpiler:** `source9` ist direkter Rust-Code (Präzedenz aus
  `plan/20261009_01_gpu_bigger`). Die Änderung ist chirurgisch (Hooks +
  eine neue Crate); Lisp-Generierung brächte Indirektion ohne Gewinn.
  Nummerierte Dateien + knappe Module folgen dem bestehenden Stil.
- **Bodies werden nach-kodiert, nicht mitgeschnitten:** `bincode::standard`
  ist deterministisch — Re-Encode der dekodierten Nachricht == gesendete
  Bytes. Dadurch **keine** Änderung in `common/` nötig.
- **Eine Datei pro Lauf** (statt pro Verbindung): `Session`/`End`-Marker
  trennen Verbindungen; `logstat` mergt mehrere Dateien nach Wall-Clock.
- **Keine neuen externen Deps:** Hash = FNV-1a (5 Zeilen), Plots = JSON +
  externe Tools, Bilder = PPM per Hand. `cargo upgrade` läuft trotzdem zur
  Prüfung (s. task.md T0).
