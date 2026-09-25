# Implementierungsplan — 20260925_02_autmation_scripting

Stapel-Skripting in `source6/`: `-a`/`--auto`-Flag (Automation startet
scharf, kein Tastendruck) + `scripts/sticker_batch.sh` (Motive aus
`cute_things.md` als `<placeholder>`-Substitution in
`meta_sticker_request.toml`, danach URL-Reset per `meta_url.toml`, je Motiv
ein frischer Prozess unter `timeout`). Kein neues Verhalten außer dem Flag;
keine neuen Deps.

Auftrag: Wol Pumba (wolpumba@gmail.com) · Linux (Ubuntu 26, Docker, X11)
· Prompt: `plan/20260925_02_autmation_scripting/prompt.txt` · Basis/Ziel:
`examples/26_onnx/source6/` (direkt Rust, Stand 2026-09-25: 46 Tests grün,
Xvfb-Smoke PASS) · Pfad `examples/25../source6` aus dem Prompt existiert
nicht — gemeint ist `examples/26_onnx/source6` (wie im Vorgängerplan) ·
Konventionen: Vorgängerpläne `plan/20260925_01_robust_automation/` und
`plan/20260924_01_automation_tui/`

## Goal

Ein Shell-Skript erzeugt pro Motiv (süße Tiere/Dinge aus `cute_things.md`)
genau einen Sticker-Prompt und setzt danach die URL zurück, sodass der
nächste Sticker starten kann — ohne dass je ein `a`-Tastendruck nötig ist.
Der Code bleibt klein, lesbar und ohne neue Abhängigkeiten; Test im Docker
ohne Meta-Account per `--self-test` und Xvfb-Headless-Lauf.

## Success Criteria

1. `x11_ocr_automation -a|--auto` startet mit scharfer Automation
   (Dashboard `SCHARF`, Hinweis auf stderr); ohne Flag bleibt `AUS`.
2. `sticker_batch.sh` ersetzt `<placeholder>` je Motiv (kein Rest-Platzhalter,
   Motiv im Text) und fährt pro Motiv Sticker-Regel + URL-Reset als je einen
   frischen, per `timeout` begrenzten Prozess (`timeout`-Exit 124 = erwartet).
3. `sticker_batch.sh --self-test` ist PASS ohne X11/Meta-Account
   (Substitution + TOML-Parse per Exit-Code-Trick + `-a`-Nachweis).
4. Xvfb-Kontrolle: mit `-a` → `SCHARF` + OCR-Text im Frame; ohne → `AUS`.
5. Gates grün: `cargo fmt --check`,
   `cargo clippy --all-targets -- -D warnings`, `cargo test`
   (inkl. neuer Args-Tests); Datei-Regel: `main.rs` wieder ≤ ~300 Zeilen
   (280), neue Datei nummeriert (`00_args.rs`); danach Walkthrough.

## Context And Current Facts

- `meta_sticker_request.toml` (gelesen): eine `click_and_type`-Regel mit
  `once = true`, Text enthält wörtlich `<placeholder>` — das Template.
- `meta_url.toml` (gelesen): eine `click_and_type`-Regel (`once`), tippt
  `https://meta.ai` auf Pattern `meta.ai/prompt/` — der Reset-Schritt.
- `cute_things.md` (gelesen): ~40 Motive in 6 Gruppen; Top-Tier sind 12
  Baby-Tiere — die Skript-Auswahl (12 Motive, je Gruppe vertreten).
- `main.rs` (verifiziert): `Automation::from_config` startet AUS, nur Taste
  `a` (`ToggleAutomation`) schaltet scharf; `parse_args` kennt kein `-a`.
- `collect.sh`-/Prototyp-Herkunft: Vorgängerplan hat Matcher+Fokus gelöst;
  hier bleibt nur noch der Skript-Betrieb (Flag + Substitution).
- Umgebung: Xvfb/xterm per apt nachinstalliert (2026-09-25); Meta-Account
  fehlt per Prompt-Aussage — E2E mit Browser bleibt Handtest beim Nutzer.

## Constraints And Non-goals

- Keine neuen Deps (Flag = Handcode, Substitution = `sed`); direkt Rust;
  klein/lesbar; Datei-Regeln aus dem Prompt (`00_args.rs` vor `01_view.rs`,
  `main.rs` nur Verdrahtung, Aufteilung ohne Verhalten — Tests beweisen).
- `timeout`-Exit 124 ist erwartetes Laufzeitende (kein Fehler), weil die
  `once`-Regel nach genau einem Feuern per Design idle bleibt.
- Non-goals: kein TOML-Templating in Rust (kein `{{var}}`/`--set`; `sed`
  genügt), kein Parallel-Betrieb mehrerer Browser, kein Upload/Tracking
  erzeugter Sticker, kein Wayland, keine Tastatur-Layout-Erweiterung.

## Key Decisions

1. **`-a`/`--auto` statt Config-Feld.** Der Skript-Betrieb startet pro Regel
   einen frischen Prozess — Schalter gehört auf die Kommandozeile, nicht in
   die TOML (kein Schema-Bruch, `schema_version` bleibt 1). Verworfene
   Alternative: `autostart`-Feld in TOML — hätte `06_config.rs` (+319 Zeilen,
   über Limit) angefasst und pro Motiv eine eigene TOML-Sektion erzwungen.
2. **Args nach `00_args.rs` (123 Zeilen), `main.rs` 311 → 280.** Reine
   Verschiebung + `parse_args_from`-Hülle für Testbarkeit; `--help`-Exit
   unverändert (darum in Tests nicht aufgerufen). Touch-Regel aus dem Prompt:
   angefasst → geteilt, ohne Verhalten (Suite grün vorher/nachher).
3. **Substitution in Shell (`sed`), nicht in Rust.** Template bleibt
   Templat (wörtlich `<placeholder>` eincheckt); generierte Dateien leben nur
   in `mktemp`-Dirs (nie committet). Verworfene Alternative: Rust-Flag
   `--subject` — hätte Config/Action-Pipeline + `06_config` angefasst.
4. **Ein Prozess pro Schritt unter `timeout`.** Jeder Schritt feuert seine
   `once`-Regel genau einmal; 124 = Erfolg. Verworfene Alternative:
   Dauer-Prozess mit Regelwechsel zur Laufzeit — bräuchte IPC/Reload-Pfad.
5. **Querschnitts-Bullets (Firmware-Herkunft), übertragen:** (a)
   Messgenauigkeit/Kalibrierung → OCR-Toleranz unverändert (Fuzzy-Matcher aus
   Vorgängerplan; Xvfb-Kontrolle zeigt Live-OCR mit Leserauschen). (b)
   3,3-V-Limits → unverändert Clamps/Deckel. (c) TUI-Protokoll →
   `schema_version` unverändert (kein TOML-Feld nötig). (d) Modus-Wechsel →
   entfällt (frischer Prozess je Schritt, kein Wechsel). (e) Persistenz →
   unverändert keine (Motive stehen im Skript, Config in TOMLs).
6. **`cargo upgrade` entfällt:** keine neue Dep (Begründung in `deps.md`).

## Recommended Approach

Kleinster beweisender Weg: R0 Docs (`plan/task/deps.md`), R1 `-a`-Flag +
`00_args` + Args-Tests (reine Unit-Ebene), R2 `sticker_batch.sh` +
`--self-test` (ohne X11/Meta beweisbar), danach T1 Gates + Xvfb-Kontrolle
(SCHARF/AUS-Kontrast) + Walkthrough. Pro Schritt `fmt`/`clippy`/`test` grün.

## Work Plan

- **R0 — Plan-Docs.** Diese Datei + `task.md` (R1/R2/T1) + `deps.md`.
- **R1 — `-a`-Flag (`00_args.rs`, `main.rs`).** `Args.auto_start`,
  `-a`/`--auto`, Hilfe-Text, `set_enabled(true)` + stderr-Hinweis in `run()`;
  `parse_args_from`-Hülle; 5 Args-Tests (Defaults, kurz, lang, kombiniert,
  Fehlerpfade). Orakel: neue Tests + bestehende Suite.
- **R2 — `scripts/sticker_batch.sh`.** 12 Motive, `sed`-Substitution,
  Sticker+Reset je als `timeout`-Prozess mit `-a`, Filter/`--list`/
  `--dry-run`-Passthrough, `--self-test` (Substitution + Parse-Trick ohne
  X11). Orakel: `--self-test` PASS + `bash -n`.
- **T1 — Gates + Xvfb + Walkthrough.** `fmt --check`/`clippy -D warnings`/
  `test` final, Xvfb-Kontrolle (`-a` → SCHARF + OCR, ohne → AUS),
  `walkthrough.md` (Abweichungen, Messungen, Docker-Pakete, Learnings).

## Validation Plan

- R1-Gate: `cargo test` (5 neue + 41 bestehende grün), `clippy -D warnings`.
- R2-Gate: `--self-test` PASS in Docker ohne Account/X11; `bash -n` sauber.
- T1-Gate: Xvfb + xterm mit Testtext: `-a`-Lauf zeigt `SCHARF` + OCR-Zeilen,
  Kontroll-Lauf `AUS`; generierte TOMLs parsen (Exit-1-Trick schon in R2).
- Manuell (Nutzer, mit Account): `sticker_batch.sh [--dry-run] [motiv]`
  gegen echten Meta-AI-Browser; Dauer pro Sticker via `--sticker-secs`.

## Risks / Rollback

- **Meta-UI ändert Platzhalter/URL-Pattern:** Regeln feuern nie (Cooldown
  läuft ins `timeout`). Schaden begrenzt (nur Log, `once`); Anpassung = zwei
  TOML-Zeilen. Rollback: Skript läuft weiter, sobald TOMLs stimmen.
- **Falsches Motiv tippt Sonderzeichen:** XTEST-Skip-Zähler fängt es
  (`skipped`, sichtbar im Dashboard); Motive sind ASCII (kein Umlaut).
- **`sed`-Trennzeichen kollidiert mit Motivtext:** `|` als Trenner; Motive
  ohne `|` (Ordnung per `--self-test` bewiesen). Rollback: Trenner wechseln.
- Pro Schritt ein Commit (Conventional, `Refs: task.md <ID>`); Revert je
  Schritt möglich. (Ob überhaupt committet wird, entscheidet Wol Pumba —
  kein Commit ohne Aufforderung.)

## Open Questions

1. Sticker-Dauer pro Motiv (Default 120 s + 60 s Reset): als Erfahrungswert
   vom Nutzer am echten Account kalibrieren?
2. Alle ~40 Motive aus `cute_things.md` übernehmen oder 12er-Auswahl reicht
   (Rest via Filter-Argumente ad hoc)?
3. Fehlende Requirements, ergänzt (bitte streichen was nicht gewünscht):
   `--self-test` als Docker-Nachweis ohne Account; `timeout`-124 als
   dokumentiertes Erfolgsende; generierte TOMLs nie committen.

## Kontext für einen unabhängigen Agenten (Pflichtlektüre)

1. `plan/20260925_02_autmation_scripting/prompt.txt` — dieser Auftrag.
2. `plan/20260925_02_autmation_scripting/plan.md` (diese Datei) + `task.md`.
3. `plan/20260925_02_autmation_scripting/deps.md` — Org/Projekt-Notation.
4. `examples/26_onnx/source6/src/{00_args.rs,main.rs}` (+ Tests) — R1.
5. `examples/26_onnx/source6/{meta_sticker_request,meta_url}.toml`,
   `cute_things.md` — R2-Vorlagen und Motivquelle.
6. `examples/26_onnx/source6/scripts/sticker_batch.sh` — R2.
7. `plan/20260925_01_robust_automation/{plan,task,deps}.md` — Vorgänger
   (Matcher, Fokus, Konventionen, Smoke-Harness).
8. Extern: Repo-Kontext `plops/cl-rust-generator` (nur Namensraum; kein
   Transpiler-Code — laut Prompt direkt Rust).

## Commit-Konvention

Conventional Commits, ein logischer Schritt pro Commit:
`feat|fix|test|docs|chore|refactor(<scope>): <kurz, imperativ, ≤72 Zeichen>`.
Body: was + warum, betroffenes Modul, Validierung
(`cargo fmt --check`, `cargo clippy`, `cargo test`, Xvfb-/Self-Test-Ergebnis).
Footer: `Refs: plan/20260925_02_autmation_scripting/task.md <ID>`.
Binaries/Modelle/generierte TOMLs nie committen (`.gitignore` beachten). Nie
ohne grüne Gates committen; keine fremden/untracked Dateien anfassen. (Ob
überhaupt committet wird, entscheidet Wol Pumba — kein Commit ohne
Aufforderung.)
