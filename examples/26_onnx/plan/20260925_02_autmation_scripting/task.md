# task.md — 20260925_02_autmation_scripting: seriell abarbeitbare Schritte

Stapel-Skripting in `examples/26_onnx/source6/`: `-a`-Flag + Batch-Skript
(R1/R2), danach Gates + Xvfb + Walkthrough (T1). Jeder Schritt endet mit
Gates (`fmt --check`, `clippy --all-targets -- -D warnings`, `cargo test`
grün, Skript: `bash -n` + `--self-test`). Erst bei grünen Gates weiter.
Datei-Regeln aus dem Prompt gelten ab der ersten Datei.

## R0 — Plan-Docs (Basis)

- `plan/20260925_02_autmation_scripting/{plan,task,deps}.md` liegen vor und
  sind in sich konsistent (keine neue Dep, Modulnummern, Timeouts).
- Kein Code, keine Gates nötig.
- Commit: `docs(plan): scripting autostart flag and sticker batch`.

## R1 — `-a`-Flag (mit sofortigem Test)

- Neu `00_args.rs`: `Args` + `auto_start`, `parse_args`/`parse_args_from`,
  `-a`/`--auto`, Hilfe-Text mit `[-a]`; 5 Tests (Defaults AUS, kurz, lang,
  kombiniert mit `--dry-run/--rules/--headless-frames`, Fehlerpfade).
- `main.rs`: `mod args` + `use`, alte `Args`/`parse_args` entfernt
  (311 → ~280 Zeilen), `set_enabled(true)` + stderr-Hinweis bei `-a`.
- Tests: `cargo test` (neue + bestehende Suite grün, ohne X11/Modell-Änderung).
- Gates analog.
- Commit: `feat(source6): autostart automation with -a flag`.

## R2 — Batch-Skript (ohne Account testbar)

- Neu `scripts/sticker_batch.sh` (+x): 12 Motive, `sed`-Substitution von
  `<placeholder>`, pro Motiv Sticker (`meta_sticker_request.toml`-Derivat)
  + Reset (`meta_url.toml`) als je ein frischer `-a`-Prozess unter `timeout`
  (124 = erwartet); `--list`, Filter-Argumente, `--dry-run`-Passthrough,
  `--bin`, `--sticker-secs/--url-secs`, `--self-test`.
- Tests (ohne X11/Meta): `bash -n` + `--self-test` PASS (12 Substitutionen
  ohne Rest-Platzhalter, alle TOMLs parsen per Exit-1-Trick, `-a` in `--help`).
- Gates analog (Skript-Seite).
- Commit: `feat(source6): sticker batch script with self-test`.

## T1 — Gates + Xvfb + Walkthrough

- Alle Gates final grün; Xvfb-Kontrolle: `-a`-Lauf → `SCHARF` + OCR-Zeilen,
  Kontroll-Lauf ohne Flag → `AUS` (Kontrast = Beweis ohne Account).
- `plan/20260925_02_autmation_scripting/walkthrough.md` (implementiert vs.
  Plan, Abweichungen, Messungen, Docker-Pakete, Learnings, Erweiterungen).
- Commit: `docs(plan): walkthrough for scripting`.
