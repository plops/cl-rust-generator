# Tasks: Bunching + Eingabe-Priorität — `source10_instrumentation`

Seriell abarbeiten; jeder Schritt endet grün (`cargo test --workspace`
im `source10_instrumentation`-Verzeichnis + `cargo fmt --check` +
`cargo clippy --workspace -- -D warnings`). Basis: `plan.md` im selben
Ordner (Befund-Zahlen aus `20261017_0614.lbwlog`).

## T0 — Umgebung + Bestandsaufnahme

- [ ] `cargo --version`, `rustc --version` notieren.
- [ ] `cargo test --workspace` in `source10_instrumentation` grün
      (Baseline, ohne X11; 90 passed erwartet).
- [ ] `plan.md` gelesen; Befund-Zahlen per
      `lbw-logstat 20261017_0614.lbwlog` + `--deep` nachvollzogen
      (Queue ~2 MB, 90 s über Budget, `cli_bps = 0` in Animation).

Verifikation: Baseline-Testergebnis im Terminal gesehen.

## T1 — Pacer-Modul `11_pace.rs` (TDD)

- [ ] `server/src/11_pace.rs`: `Pacer::new(budget_bps)` +
      `pace(&mut self, nbytes: usize) -> Duration` (Leaky Bucket:
      Guthaben-Cap 1 s, Idle füllt auf, nie negativ). `lib.rs`:
      `pub mod pace` als `11_pace.rs`-Pfadmodul.
- [ ] Unit-Tests in derselben Datei: erster Send wartet 0; Burst von
      `budget` Bytes kostet ~1 s; Idle-Zeit halbiert die nächste Wartezeit
      (gedeckelt); 0-Byte wartet 0; kleine Texte (~50 B bei 6000 B/s)
      kosten < 10 ms.

Verifikation: `cargo test -p lbw-server pace` grün.

## T2 — Session: Tile-Pacing + Text-Priorität + `--budget`

- [ ] `server/01_config.rs`: `--budget <B/s>` (Default 6000),
      `validate()` lehnt 0 ab; Config-Tests ergänzen.
- [ ] `server/10_session.rs`: vor Tile-`write_msg` die Pacer-Dauer
      abwarten (in ≤50-ms-Scheiben, Abbruch wenn Reader-Thread fertig);
      Text-Nachrichten (`ClearText`/`AddText`/`RemoveText`) und `Hello`
      ohne Warten senden. Wartezeit in neues `FrameMs.pace`-Feld
      schreiben (`send` bleibt memcpy); `log/`-Statistik (Human, JSON)
      um die Stufe erweitern; `LOG_VERSION` 2 → 3.
      `main.rs`: `set_nodelay(true)` am Accept-Socket (Fehler → Warnung,
      kein Abbruch).
- [ ] Bestehende Server-Tests unverändert grün (Default-`--budget`
      6000 drosselt Loopback-Tiles von ~1 kB nicht spürbar).

Verifikation: `cargo test -p lbw-server` grün.

## T3 — Client: Nagle aus, Flush schneller

- [ ] `client/03_net.rs`: `set_nodelay(true)` nach Connect (Fehler →
      `session`-Abbruch wie bei Timeout-Flag? Nein: Warnung per
      `eprintln`, weiter — Verbindung ist wichtiger als das Flag);
      Lese-Timeout 50 ms → 10 ms (Kommentar: Eingabe-Wartezeit 0–10 ms).
- [ ] Bestehende Client-Tests unverändert grün.

Verifikation: `cargo test -p lbw-client` grün.

## T4 — Host-Tests: Priorität + Paced-Lieferung

- [ ] `server/tests/loopback.rs` (neu, Muster aus `common.rs`):
      `texts_jump_the_tile_queue` — Budget winzig (z. B. 200 B/s),
      Stub-OCR liefert Text + Bildänderung im 1. Frame: `AddText`
      trifft innerhalb von 2 s ein, obwohl das Tile noch gepacet wird
      (deterministisch: Tile ≥ einige kB ≫ Budget-Fenster).
- [ ] `server/tests/loopback.rs`: `paced_tiles_arrive_whole` — alle
      Bytes kommen vollständig und in Reihenfolge an (kein Verwerfen),
      Session mit `max_frames` endet sauber.
- [ ] `log`-Golden-Test unverändert (kein Formatbruch — Assertions
      bleiben, wie sie sind).

Verifikation: `cargo test --workspace` grün (≥ 93 Tests).

## T5 — Soak-Nachweis (HIL oder synthetisch) + Hygiene

- [ ] Animations-Soak: falls Xvfb/Modelle verfügbar, Server mit
      `--budget 6000` + bewegtem Inhalt (z. B. scrollendes xterm)
      aufzeichnen und `queue_peak_b` + Budget-Sekunden gegen die
      0614er-Werte halten (Ziel: < 30 kB / ~0 s). Falls kein
      HIL möglich: Loopback-Soak mit großem `SharedSource`-Bild und
      kleinem Budget als Ersatz — im Walkthrough ehrlich als
      Ersatzversuch kennzeichnen.
- [ ] `cargo fmt`, `cargo clippy --workspace -- -D warnings`
      (eigener Code) sauber; alle Dateien < 600 Zeilen; keine neuen Deps
      (`git status` zeigt keine `Cargo.lock`-Dep-Zuwächse außer Version).

Verifikation: Kennzahlen im Terminal gesehen; fmt/clippy sauber.

## T6 — Commits + Walkthrough

- [ ] Commits nach `plan.md` §6 (Conventional, deutsch, jeder grün).
- [ ] `plan/20261017_01_bunching/walkthrough.md` (deutsch, didaktisch,
      Mermaid, Code-Beispiele; Struktur: 1. Implementiert,
      2. Umentscheidungen, 3. Learnings/Erweiterungen,
      4. Dockerfile-Pakete).
- [ ] Abschluss: `cargo test --workspace` grün, alles committed
      (`git status` sauber außer ggf. `.lbwlog`-Ignoriertes).

Verifikation: `git log --oneline` zeigt die Kette.
