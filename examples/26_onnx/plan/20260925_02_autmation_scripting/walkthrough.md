# Walkthrough — 20260925_02_autmation_scripting

Stapel-Skripting für Meta-AI-Sticker: `-a`/`--auto`-Flag +
`scripts/sticker_batch.sh`. Alle Tests grün, alles committet (4 Commits,
s. unten). Echter Browser-Lauf mit Meta-Account bleibt Handtest beim
Nutzer (Wol Pumba) — dafür ist das Skript gebaut.

## Was implementiert wurde

- **R1 — `-a`-Flag** (`source6/src/00_args.rs` neu, 123 Zeilen;
  `source6/src/main.rs` 311 → 280 Zeilen): `Args.auto_start`, Parsing von
  `-a`/`--auto`, Hilfe-Text `[-a]`, `set_enabled(true)` + stderr-Hinweis in
  `run()`. `parse_args_from`-Hülle macht das Parsing ohne Prozess testbar
  (`--help`-Exit unverändert). 5 neue Unit-Tests (Defaults AUS, kurz, lang,
  kombiniert, Fehlerpfade).
- **R2 — `source6/scripts/sticker_batch.sh`** (168 Zeilen, +x, nur
  bash/sed/timeout/mktemp): 12 Motive aus `cute_things.md`, ersetzt
  `<placeholder>` in `meta_sticker_request.toml` per `sed` in ein
  `mktemp`-Derivat, fährt Sticker + URL-Reset (`meta_url.toml`) als je einen
  frischen `-a`-Prozess unter `timeout` (124 = erwartetes Ende, weil
  `once`-Regeln nach einem Feuern idle bleiben). `--list`, Filter-Argumente,
  `--dry-run`-Passthrough, `--bin`, `--sticker-secs/--url-secs`,
  `--self-test` (Substitution + TOML-Parse per Exit-1-Trick + `-a`-Nachweis,
  ganz ohne X11/Meta).
- **R0/T1 — Docs** (`plan/20260925_02_autmation_scripting/`):
  `plan.md`, `task.md`, `deps.md`, dieses `walkthrough.md`.

## Bezug zu Tests / Messungen

- `cargo test`: 46 passed, 0 failed, 1 ignored (der XTEST-Server-Test, wie
  bisher — braucht echten X-Server per Ignored-Flag).
- `cargo fmt --check` / `cargo clippy --all-targets -- -D warnings`: sauber.
- `sticker_batch.sh --self-test`: PASS (12 Motive + Reset-Regel) — lief im
  Docker ohne Account/X11; hat dabei sogar korrekt einen veralteten
  Release-Binary ohne `-a` abgewiesen (danach neu gebaut).
- Xvfb-Kontrolle (dazu `xvfb`/`xterm` per apt nachinstalliert, s. Pakete):
  `-a --dry-run --headless-frames 1` → `Automation: [SCHARF]` + Live-OCR
  (`HELT0 0CR WORLD 123`, `SECOND LINE ABC XVZ` — übliches Leserauschen,
  dafür existiert der Fuzzy-Matcher); Kontrolllauf ohne Flag → `AUS`.
  Inference ~88 ms/Frame (Det ~51 + Rec ~37) auf dieser Maschine.

## Untergrund / Learnings

- Der `pkill -f "sleep 120"`-Cleanup hat die eigene Shell gekillt (Pattern
  matchte die eigene Kommandozeile). Merke: `pkill -f` nur mit Mustern, die
  nicht im eigenen Befehlstext vorkommen — oder PIDs aus `setsid` merken.
- `timeout`-Exit 124 als Erfolg zu werten fühlt sich erst falsch an, ist
  aber die ehrliche Abbildung des `once`-Designs ( feuern → idle ). Im
  Skript explizit verzweigt (124/0/Fehler), damit späteres Debugging nicht
  raten muss.
- Der Exit-Code-Trick (1 = TOML ok/X11 fehlt, 2 = TOML kaputt) aus dem
  Vorgängerplan trägt auch hier: `--self-test` beweist 13 TOML-Parses ohne
  Display. Wiederverwendbares Muster für künftige Regel-Generatoren.
- Datei-Regel aus dem Prompt griff sofort: `main.rs` war mit 311 Zeilen
  über dem Limit, also wanderte das Args-Parsing nach `00_args.rs` — die
  Nummerierung „Args zuerst" passt zur Datenfluss-Reihenfolge.

## Abweichungen vom Plan

- Keine inhaltlichen Abweichungen. Nur Reihenfolge: R1/R2-Code entstand
  vor dem Niederschreiben der R0-Docs (gleiche Sitzung, Docs danach auf den
  fertigen Stand geschrieben statt umgekehrt).

## Mögliche Erweiterungen

1. `--sticker-secs` am echten Account kalibrieren (Default 120 s + 60 s
   Reset sind Schätzwerte); evtl. Erfolgs-Erkennung statt fester Zeit
   (Log auf „Klick + Eingabe" pollen, dann früher abbrechen).
2. Restliche ~28 Motive aus `cute_things.md` übernehmen oder per `--list`
   + Filter ad hoc fahren (offene Frage aus dem Plan).
3. `06_config.rs` (319) und `05_input.rs` (308) stehen noch über ~300 —
   unberührt gelassen (Touch-Regel); beim nächsten Anfassen teilen.
4. `smoke_xvfb.sh` könnte den SCHARF/AUS-Kontrast als Stufe 5 übernehmen
   (Befehle aus dieser Sitzung: Xvfb + xterm mit Testtext, je ein
   `--headless-frames 1`-Lauf mit/ohne `-a`; Logs lagen unter `/tmp`).

## Neue Programme im Docker-Image

Für dieses Vorhaben nachinstalliert und künftig empfehlenswert: `xvfb`,
`xterm` (beide apt, für Headless-Nachweise ohne echten Bildschirm).
Weiter gültig aus dem Vorgänger-Walkthrough: `ca-certificates`
(Modell-Download), Chrome-for-Testing nur für `test_duckai.sh`.
Meta-Account-Daten gehören NICHT ins Image (Nutzer testet selbst).

## Commits (diese Sitzung)

- `docs(plan): scripting autostart flag and sticker batch`
- `feat(source6): autostart automation with -a flag`
- `feat(source6): sticker batch script with self-test`
- `docs(plan): walkthrough for scripting`
