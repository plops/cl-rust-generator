# task.md — 20260925_01_robust_automation: seriell abarbeitbare Schritte

Robuste Automation in `examples/26_onnx/source6/`: Fuzzy-Matcher +
Fokus-Automatik + Refactors (Prototyp-Bausteine 1–6). Jeder Schritt endet
mit Gates (`fmt --check`, `clippy --all-targets -- -D warnings`,
`cargo test` grün). Erst bei grünen Gates weiter. Datei-Regeln aus dem
Prompt gelten ab der ersten Datei.

## R0 — Plan-Docs (Basis)

- `plan/20260925_01_robust_automation/{plan,task,deps}.md` liegen vor und
  sind in sich konsistent (Modulnummern, keine neue Dep).
- Kein Code, keine Gates nötig.
- Commit: `docs(plan): robust automation matcher and focus`.

## R1 — `07_match.rs` + Umbenennung (rein, ohne X11)

- Neu `07_match.rs`: `norm_ocr_str`, `levenshtein_substring`,
  `fuzzy_ocr_match` (Prototyp-Fälle als Tests, inkl. `Cancel`/`Confirm`
  negativ); Pattern-Speicherung in `06_config` roh (kein Lowercase mehr —
  Norm macht das); `evaluate` nutzt `fuzzy_ocr_match`.
- Umbenennung: `07_rules.rs`→`08_rules.rs`, `08_tui.rs`→`09_tui.rs`,
  `09_canvas.rs`→`10_canvas.rs` (+ `mod`-Pfade/`crate::`-Pfade in `main.rs`,
  Modulnamen `rules`/`tui`/`canvas` bleiben).
- Tests (ohne X11/Modell): beide Prototyp-Fuzzy-Tests wortwörtlich +
  bestehende Suite grün.
- Gates analog.
- Commit: `feat(source6): fuzzy ocr matcher with glyph folding`.

## R2 — Fokus + Klick-Pfad (mit X11-Nachweis)

- `05_input.rs`: `focus_at(x, y)` (`translate_coordinates` →
  Kind/Root, `set_input_focus(PARENT)`, `_NET_ACTIVE_WINDOW` `[1, NOW, …]`,
  alles best-effort); `click()` ruft `focus_at` und sendet Button-Events
  mit `(x, y)`; `FOCUS_SETTLE_MS = 100`.
- `08_rules.rs` (nach R1): `Sink::click_and_type`-Default (Klick → 100 ms
  → Tippen); Engine nutzt sie für `ClickAndType`.
- `main.rs`: Post-Type-Sleep + `CLICK_TYPE_DELAY_MS` entfernen.
- Tests: Ignored-XTEST-Test läuft über den neuen Pfad (`Ok` + Skip 0).
- Gates analog + Xvfb-Nachweis (Test grün).
- Commit: `fix(source6): auto focus window before click and type`.

## R3 — Refactors ohne Verhalten

- `02_capture.rs`: `norm_rgb`-Helper (eine Formel statt zwei Blöcke).
- `04_recognize.rs`: `load_dict` per `trim_matches` vereinfacht.
- `06_config.rs`: `as_u32` in Kettenform.
- Tests: keine neuen — bestehende (1:1-Byte-Gate, Dict-Test,
  TOML-Fehler-Tests) sind das Orakel und bleiben grün.
- Gates analog.
- Commit: `refactor(source6): deduplicate normalization and parsers`.

## R4 — `rules.metaai.toml`

- Datei wie im Prototyp (Pattern `Ask Meta AI...`, eine
  `click_and_type`-Regel mit Red-Panda-Prompt, Cooldown 5 s).
- Nachweis ohne X11: Binary mit `--rules` parsen (Exit 1 = TOML ok, da nur
  X11 fehlt; Exit 2 = TOML kaputt).
- Gates: Parse-Nachweis + `cargo test` grün.
- Commit: `feat(source6): meta ai joke rule example`.

## T1 — Gates + Smokes + Walkthrough

- Alle Gates final grün; `smoke_xvfb.sh` + `test_duckai.sh` erneut PASS
  (Regression der unveränderten Flächen).
- `plan/20260925_01_robust_automation/walkthrough.md` (implementiert vs.
  Plan/Prototyp, Abweichungen, Messungen, Docker-Pakete, Learnings,
  Erweiterungen).
- Commit: `docs(plan): walkthrough for robust automation`.
