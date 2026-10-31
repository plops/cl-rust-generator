# Tasks: `source10_instrumentation_b`

Seriell; jeder Schritt endet grün (`cargo test --workspace`,
`cargo fmt --check`, `cargo clippy --workspace --all-targets -- -D warnings`).

- [x] T0 Baseline: source10 `cargo test --workspace` grün (105), Logs per
      `lbw-logstat` nachgemessen, CPU-Modelltest (det ms) notiert.
- [x] T1 Gerüst: Kopie ohne `target/`/Logs, `models`-Symlink, README/deps,
      `.gitignore`; CPU-only (ort ohne `cuda`, `Provider`/`--cpu` raus);
      Modelltest CPU grün.
- [x] T2 logstat: v2-Logs lesbar (Test mit synthetischem v2-Record +
      echtem 0614-Log, falls vorhanden); Erst-Echo-Metrik (Test: 5 Tasten,
      1 Echo → 5 Samples, keine Altlast); `09_channel.rs` Kanal-Sim
      (Tests: 12 kB-Kachel blockiert Text 2 s; kleine Texte allein ~0 s).
- [x] T3 Pacer `wait_for_at` + Sender-Thread `12_send.rs` (Unit: Express
      überholt wartende Kachel; Abbruch bei Schreibfehler).
- [x] T4 Streifen + Cap: `stripes`, `encode_capped` (Unit: Bänder,
      unveränderte Bänder fallen weg, Rauschbild 1280×720 → alle Teile
      ≤ Cap, Mosaik dekodiert = Original-Region).
- [x] T5 Session-Umbau: Schattenbild, Encode nur bei freier Spur,
      `--max-tile`; bestehende Loopback-Tests grün; neu:
      `texts_overtake_big_change` (Rauschbild + Text bei 2 kB/s: Text < 1 s).
- [x] T6 Client: Maus zusammenfassen (Unit), Loopback grün.
- [x] T7 Prüfstand: `bench.rs` gegen `_b` und `tools/bench10` gegen
      source10 auf demselben 0839-Fenster; Integrationstest (überspringt
      ohne Log) prüft max. Nachricht ≤ Cap + Hülle.
- [x] T8 HIL: `throttle.rs` + `smoke_hil.sh` (Xvfb, CPU-Modelle,
      6000 B/s, xdotool) — OK + Kennzahlen.
- [x] T9 Hygiene: fmt/clippy, Dateien < 600 Zeilen, `cargo upgrade`
      geprüft.
- [x] T10 Doku: `handbuch.md` (Bedienung + Konzepte) und `walkthrough.md`
      (Implementiert / Umentscheidungen / Learnings / Docker-Pakete),
      Commits.
