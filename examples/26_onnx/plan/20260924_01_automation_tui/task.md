# task.md — 20260924_01_automation_tui: seriell abarbeitbare Schritte

Neues Programm `examples/26_onnx/source6/`: X11-Live-OCR (PP-OCRv6) +
TUI-Dashboard (crossterm) + Button-Klick-/Text-Automation (x11rb/XTEST),
ohne Bildübertragung. Jeder Schritt endet mit Gates. Erst bei grünen Gates
committen (s. `plan.md`, Commit-Konvention) und zum nächsten Schritt.
Dateiregeln aus dem Prompt gelten ab der ersten Datei (`NN_name.rs`,
≤~300 Zeilen, `main.rs` nur Verdrahtung, vorher/nachher grün).

## S0 — Gerüst + Deps + Modelle (Basis)

- `source6/`-Crate anlegen (`main.rs` nur Deklaration + Verdrahtung);
  `Cargo.toml` mit neuesten Versionen (`cargo upgrade`-Protokoll im
  Commit-Body): `crossterm` (Prototyp: 0.28, nimm neueste),
  `x11rb` mit Feature `xtest`, `ort` wie source5 (`2.0.0-rc.13`);
  `macroquad`/`xdotool`/Wayland-Stack NICHT einführen.
- Modelle/Dict aus `source5/` nutzbar machen (kopieren oder Pfad —
  Entscheidung in `plan.md`-S0 bzw. Commit-Body dokumentieren;
  keine Binaries committen ohne `.gitignore`-Regel).
- System: `apt-get install xvfb` (Prompt erlaubt Installation),
  `DISPLAY`-Notiz für Smokes.
- Gates: `cargo build` (auch `--release`-Check), `cargo fmt --check`,
  `cargo clippy --all-targets -- -D warnings`, `cargo test` (leer grün).
- Commit: `chore(source6): scaffold crate with tui automation deps`.

## S1 — `01_view.rs` (ROI-State + Screen-Projektion, rein, ohne X11)

- `View { x, y, size }` + Stufen aus source5, `pan`/`zoom_in`/`zoom_out`
  mit Screen-Clamp; neu: `to_screen_rect` (640-Raum → absolute Pixel),
  `center`, Klick-Clamp auf Screen.
- Tests (ohne X11/Modell): Identität bei 640, Skalierung 320/960/1280,
  Clamp am Rand, Mitte-in-Box. ≥5 Tests.
- Gates analog S0.
- Commit: `feat(source6): roi view with screen projection`.

## S2 — `05_input.rs` (XTEST-Input, ohne OCR)

- `X11Input`: Keymap aus `get_keyboard_mapping` (ASCII 0x20–0x7e,
  Shift/Return-Erkennung), `click(x, y)`, `type_text(text, hit_enter)`
  mit Delays als Konstanten, Skip-Zähler für nicht-abbildbare Zeichen;
  alles `Result`-propagierend (kein `unwrap`/`expect`); Extension-Check
  mit klarer Meldung + `--dry-run`-Hinweis.
- Tests: Keymap-Aufbau an synthetischem Mapping (ohne X11);
  Konstanten-/Clamp-Tests.
- Xvfb-Nachweis: Klick auf Testfenster-Button (Callback feuert) +
  getippter ASCII-Text erscheint im Eingabefeld (Fenster-Inhalt = Orakel,
  nicht das eigene Log).
- Gates analog + Smoke-Log sichern.
- Commit: `feat(source6): xtest click and type input`.

## S3 — `02/03/04_capture+detect+recognize` (Übernahme ohne Verhalten)

- `01_view`-kompatibel aus source5 übernehmen: Capture (`get_image` mit
  ROI-Offsets, 1:1-Fast-Path), DBNet-Detect (Schwellen identisch),
  CTC-Recognize (`load_dict`, `ctc_decode`); Change-Detect (`memcmp`) +
  ROI-Wechsel-Invalidierung wie source5.
- Tests: mitgebrachte source5-Tests (Detect-Postprocessing, CTC, Dict,
  1:1-Byte-Gate) grün, ohne X11/Modell.
- Gates analog + vorher/nachher grün.
- Commit: `refactor(source6): adopt capture detect recognize from source5`.

## S4 — `06_rules.rs` (Regel-Engine + TOML, ohne X11)

- TOML laden (`rules.toml`, Vorlage `rules.example.toml`, `schema_version`
  prüfen; Parser-Dep `toml`, neueste Version per `cargo upgrade`):
  `[pan]` (`step_divisor`, `step_min_px`, `roi_steps`, `default_size`) +
  `[[rule]]` (`name`, `pattern`, `action = "click" | "click_and_type"`,
  `text`, `press_enter`, `cooldown_secs`); ohne Datei eingebaute Defaults.
- `Rule`-Engine: case-insensitiver Substring, `enabled`-Flag (Default aus),
  eine Aktion pro Zyklus, Log-Deckel 10.
- Tests (ohne X11, injizierte Uhr + Fake-Input): TOML-Parsen (Beispiel-Datei
  + fehlende Datei → Defaults + falsche `schema_version` → Fehler),
  Treffer → Aktion, kein Treffer → keine Aktion, `enabled=false` →
  keine Aktion, Cooldown blockt Zweit-Feuer, Log-Deckel hält.
- Gates analog.
- Commit: `feat(source6): rule engine with toml config and cooldowns`.

## S5 — `07_tui.rs` (Dashboard, ohne Terminal testbar)

- Reine Render-Funktion (ROI/Status/Automation/ms/Boxen-Tabelle/Log als
  String) + Tasten-Mapping (Pfeile/`1`/`2`/`a`/`q`/Esc/Ctrl-C) +
  Restore-Guard (`disable_raw_mode` + Cursor-Show auch im Fehlerpfad).
- Tests: Render-Strings (alle Status-Kombinationen), Event-Mapping,
  Log-Trunkierung; kein echtes Terminal nötig.
- Gates analog.
- Commit: `feat(source6): tui dashboard with restore guard`.

## S6 — Loop-Verdrahtung

- Capture → Inferenz → Engine → TUI-Render; frame-konsistente ROI-Kopie;
  Automation pausiert einen Zyklus nach ROI-Wechsel; `--dry-run`-Flag
  (loggt ohne `fake_input`); `q`/Esc/Ctrl-C → sauberer Exit.
- Tests: Verdrahtungs-Smoke (Komponenten zusammenspielend, Engine aus).
- Gates analog.
- Commit: `feat(source6): wire capture infer automate tui loop`.

## T1 — Härtung + E2E (Browser-Szenario im Xvfb)

- Duck.ai-Szenario (`source6/scripts/test_duckai.sh`, bereits grün):
  „Ask anything privately" → Frage tippen → „Ask" → Anonymitäts-Hinweis +
  Witz in der Antwort; zusätzlich Testseite mit bekanntem Button-Text →
  Regel feuert, Klick landet (Seiten-Effekt als Orakel); Eingabefeld →
  getippter Text steht im Feld; `--dry-run`-Lauf zeigt Log ohne Effekt;
  Cooldown: genau ein Feuer pro Fenster; Skip-Zähler provoziert + geloggt.
- Fehlerpfade (X11 weg, XTEST fehlt, Modell fehlt → Meldung + Exit ≠ 0,
  Terminal restored); Dauerlauf (Pan/Zoom-Spam, kein Drift).
- Gates: alle Gates S0–S6 erneut grün + E2E-Logs für Walkthrough sichern.
- Commit: `fix(source6): harden automation loop and error paths`.

## T2 — Upgrade-Check + Deps + Abschluss

- `cargo upgrade`-Check (nur bei eingeführten Deps relevant),
  `fmt`/`clippy`/`test` final grün, `deps.md` final.
- `plan/20260924_01_automation_tui/walkthrough.md` schreiben (implementiert
  vs. Plan, Abweichungen vom Prototyp, Xvfb-Logs, Docker-Pakete, Learnings,
  Erweiterungen).
- Commit: `docs(plan): walkthrough for source6 tui automation`.
