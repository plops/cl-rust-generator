# Implementierungsplan — 20260924_01_automation_tui

X11-Live-OCR (PP-OCRv6 via ort) + TUI-Dashboard (crossterm) + Button-Klick-
und Text-Automation (x11rb/XTEST) als neues Programm in `source6/` —
ohne Bildübertragung, nur Koordinaten → Klick/Tastatur.

Auftrag: Wol Pumba (wolpumba@gmail.com) · Linux (Ubuntu 26, Docker, X11,
Kernel-/USB-Zugriff vom Host) · Prompt:
`plan/20260924_01_automation_tui/prompt.txt` · Prototyp:
`plan/20260924_01_automation_tui/prototype.md` · Basis:
`examples/26_onnx/source5/` · Ziel: `examples/26_onnx/source6/` (neu, direkt
Rust, kein Transpiler-Input) · Konventionen: Vorgängerpläne
`plan/20260920_01_capture/` und `plan/20260923_01_keys_source5/`
(`{plan,task,deps}.md`), Stil-Vorlage `examples/25_dnb`, Patterns
`examples/26_onnx/source0/`

## Goal

Auf Basis von source5 (Screenshot → PP-OCRv6-Textfindung) entsteht in
`source6/` ein eigenständiges, kleines Programm, das zusätzlich automatisch
Buttons anklickt und Text eingibt — z. B. um einen Browser fernzusteuern,
ohne das Bild selbst zu übertragen. Anzeige als Terminal-Dashboard (TUI)
statt Fenster-Overlay: erkannte Textboxen mit absoluten Bildschirmkoordinaten,
Automation-Regeln (Substring → Klick / Klick+Type), scharf/unscharf schaltbar,
mit Cooldowns und Aktions-Log. Kleiner, übersichtlicher, effizienter Code mit
minimalen Abhängigkeiten.

## Success Criteria

1. `source6/` baut als eigenes Crate (`cargo build --release`) und läuft
   unter X11/Xvfb: ROI-Capture → Detektion → Erkennung → TUI-Tabelle mit
   Text + absoluten Klick-Koordinaten pro Box.
2. Automation: Eine Regel mit Substring-Treffer löst genau einen
   XTEST-Klick auf die Box-Mitte aus; eine ClickAndType-Regel klickt und
   tippt danach den konfigurierten Text (+ optional Enter). Genau eine
   Aktion pro Frame-Zyklus, Cooldown pro Regel wird eingehalten.
3. Sicherheit: Automation startet AUS (Default); erst Taste `a` schaltet sie
   scharf. Trockenlauf (`--dry-run`) loggt Aktionen ohne XTEST-Events.
   Klick-Koordinaten sind auf die Bildschirmgeometrie geclampt.
4. TUI: ROI-Position/Größe, Status (IDLE/DETECTING), Automation-Zustand,
   Inferenz-ms, Boxen-Tabelle (max. ~12 Zeilen), Aktions-Log; Tasten:
   Pfeile = Pan, `1`/`2` = Zoom, `a` = scharf/unscharf, `q`/Esc = sauberer
   Exit mit Terminal-Restore (auch bei Fehler).
5. Keine Bildübertragung: Der Automation-Pfad nutzt nur Box-Koordinaten +
   Text; es wird kein Screenshot versendet oder gespeichert (außer
   explizitem `--save-frame`-Debug-Flag, falls übernommen).
6. Datei-Regeln eingehalten: nummerierte Module in Datenfluss-Reihenfolge,
   keine Datei deutlich über ~300 Zeilen, `main.rs` nur Modul-Deklaration +
   Verdrahtung, Tests bei ihren Typen.
7. Gates grün: `cargo fmt --check`,
   `cargo clippy --all-targets -- -D warnings`, `cargo test` (davon ein
   always-green Kern ohne X11/Modell), Xvfb-Smoke mit synthetischem Fenster
   (Klick landet, getippter Text kommt an); `deps.md` (Org/Projekt-Notation),
   `task.md` (serielle Schritte), danach Walkthrough unter
   `plan/20260924_01_automation_tui/walkthrough.md` inkl. Docker-Paketliste.

## Context And Current Facts

- `source5/src/` (gelesen, Stand 2026-09-23, je 156–228 Zeilen):
  `01_view.rs` (ROI-State `View {x,y,size}`, Stufen
  `[320,480,640,960,1280]`, Pan/Zoom mit Screen-Clamp, 9 Unit-Tests),
  `02_capture.rs` (`get_image`-ROI-Capture, 1:1-Fast-Path `prepare_native`
  vs. `resize_nearest_planar`, 5 Tests), `03_detect.rs` (DBNet-Session,
  `postprocess_dbnet` mit `DET_THRESH 0.3`/`BOX_THRESH 0.6`/`UNCLIP_RATIO 1.5`,
  3 Tests), `04_recognize.rs` (CTC-Session, `load_dict` ohne YAML-Dep,
  `ctc_decode`, `REC_H 48`/`MAX_REC_LINES 64`, 4 Tests), `05_overlay.rs`
  (macroquad-Render + Unifont-Suchliste, 3 Tests), `main.rs` (nur
  Verdrahtung, 207 Zeilen). `source5/Cargo.toml`: `macroquad 0.4.16`
  (default-features=false), `ort 2.0.0-rc.13`, `x11rb 0.14.0`.
  Modelle + Dict liegen in `source5/` vor (`PP-OCRv6_small_det.onnx` 9,9 MB,
  `PP-OCRv6_small_rec.onnx` 21 MB, `inference.yml`) — kein Download nötig,
  per `include_bytes!`/`include_str!` einbettbar.
- `prototype.md` (gelesen, 6 Abschnitte): schlägt vor, macroquad durch
  `crossterm` zu ersetzen, `x11rb` mit Feature `xtest` für
  `xtest::fake_input` (Motion/Button/Key-Press/Release) zu nutzen,
  Modell→Screen-Projektion in `View::to_screen_rect`, Zero-Copy-BGRA-Pipeline,
  `X11Input` (ASCII-Keymap 0x20–0x7e aus `get_keyboard_mapping`, Shift- und
  Return-Erkennung), `AutomationEngine` (Regeln mit Substring, Cooldown,
  eine Aktion pro Zyklus) und TUI-Loop (Raw-Mode, Pan/Zoom/`a`/`q`,
  Change-Detect per `memcmp`). Der Prototyp-Code ist Startpunkt, kein
  Copy-Paste-Ziel: Fehler-Propagierung (`expect`/`unwrap` → `Result`),
  Tasten-Mapping-Lücken (nur ASCII) und fehlende Clamps/Tests sind in der
  Implementierung zu schließen.
- `README.md`: `source6 .. text detection, TUI display, automation` — der
  Ordner ist vorgesehen, existiert aber noch nicht (verifiziert:
  `ls source6` → fehlt). Pfad `examples/25../source0` aus dem Prompt
  existiert nicht — gemeint ist `examples/26_onnx/source6` (analog zu den
  Vorgänger-Klärungen `25../source0` → `26_onnx/source0`/`source5`).
- Umgebung (verifiziert): `rustc/cargo 1.98.1`, `DISPLAY` leer, `Xvfb` NICHT
  installiert (`which Xvfb` leer), `libx11` vorhanden, Fonts unter
  `/usr/share/fonts/{opentype,truetype}` vorhanden. Für Tests: `xvfb`
  per apt nachinstallieren (Prompt erlaubt das ausdrücklich).
- Extern verifiziert: `x11rb 0.14.0` hat das Feature `xtest`
  (docs.rs-Featureliste); `fake_input(conn, type_, detail, time, root,
  root_x, root_y, deviceid) -> Result<VoidCookie, ConnectionError>`
  (DeepWiki `psychon/x11rb`, sync + async) — passt zu den
  Prototyp-Konstanten (`MOTION_NOTIFY`, `BUTTON_PRESS/RELEASE`,
  `KEY_PRESS/RELEASE` aus `xproto`). `ort` latest = `2.0.0-rc.13`
  (docs.rs, Stand Juli 2026) — identisch zum source5-Pin. `crossterm`
  latest = `0.29.0` (docs.rs, Sept. 2026) — Prototyp nennt `0.28`; bei
  Einführung ist die neueste zu nehmen (Upgrade-Regel aus dem Prompt).

## Constraints And Non-goals

- Direkt Rust erzeugen (kein Lisp-Transpiler-Input); klein/übersichtlich/
  effizient; minimalste Abhängigkeiten: `ort` + `x11rb[xtest]` +
  `crossterm` (neueste Versionen bei Einführung, auch bei Warnung).
  `macroquad`, `xdotool`, Wayland-Stack, YAML-/Bild-Crates werden NICHT
  eingeführt (Dict-Parser bleibt Handcode wie in source5).
- Datei-Regeln aus dem Prompt (verbindlich): `NN_name.rs` mit zweistelliger
  Nummer, aufsteigend = Init-/Datenfluss-Reihenfolge, Zusammengehöriges
  (Typ + Tests) nicht auseinanderreißen, `main.rs` nur Deklaration +
  Verdrahtung, ≤~300 Zeilen pro Datei, Aufteilung ohne Verhaltensänderung
  (vorher/nachher: alle Tests + Smoke grün).
- Tooling: `cargo fmt`, `cargo clippy --all-targets -- -D warnings`,
  `cargo upgrade` bei Deps-Einführung, neue Deps sofort in `deps.md`
  (`<organization>/<projekt>`-Notation für spätere DeepWiki-Abfragen).
- Non-goals: kein Bild-Streaming (bewusst aus dem Auftrag: nur Koordinaten
  steuern, kein Bild übertragen), kein Wayland-Pfad, keine
  Fernsteuer-Netzwerkschnittstelle (lokaler X11-Client), kein
  Unicode-Full-Layout (Tastatur-Mapping startet ASCII + dokumentierte Lücke),
  keine Maus-Drag/Gesten, kein Multi-Screen-Offset-Management (nur der
  eine Screen aus dem x11rb-Setup).

## Key Decisions

1. **source6 als neues Crate, OCR-Module aus source5 kopiert (kein
   Workspace-Refactor).** `01_view`/`02_capture`/`03_detect`/`04_recognize`
   kommen fast unverändert mit (inkl. ihrer Tests); `05_overlay.rs`
   (macroquad) entfällt und wird durch TUI-Module ersetzt. Begründung:
   source5 bleibt lauffähig, source6 ist unabhängig review- und
   rückbaubar. Verworfene Alternative: source5 umbauen — vermischt zwei
   Anzeige-Paradigmen in einem Binary.
2. **macroquad → crossterm (neueste, docs.rs-Stand 0.29.0).** Terminal-Tabelle
   statt Fenster: headless/SSH-fähig, Sekunden-Builds, keine
   OpenGL/GLX/Wayland-Transitiva. Raw-Mode + `cursor::Hide/Show` +
   garantierter `disable_raw_mode` auch im Fehlerpfad (Guard/`Drop`).
3. **`x11rb` mit `xtest`-Feature statt `xdotool`.** Ein Prozess, eine
   X11-Verbindung, Latenz im Sub-ms-Bereich, kein Shell-Out. `fake_input`
   mit `Time::CURRENT_TIME`; nach Motion 20 ms, nach Tastendruck 15 ms,
   vor Enter 30 ms + 50 ms Click→Type-Delay (Prototyp-Werte als Konstanten
   mit Tests an der Konstanten-Ebene, Tuning im Xvfb-Smoke).
4. **Koordinaten: Detektion im 640-Raum → absolute Screen-Pixel in einer
   Funktion (`to_screen_rect`), Klick = Box-Mitte, geclampt auf Screen.**
   Die Projektion ist rein und unit-testbar (Identität bei 640, Skalierung
   sonst, Clamp am Rand). Unscharfe Boxen (< Mindestgröße, unter
   `BOX_THRESH`) erzeugen nie Aktionen — Filter vor der Engine.
5. **Regel-Engine: Substring (case-insensitiv) → `Click` /
   `ClickAndType { text, press_enter }`, pro Regel Cooldown, genau eine
   Aktion pro Zyklus, Log auf 10 Einträge gedeckelt.** Default AUS, `a`
   schaltet scharf; `--dry-run` loggt ohne `fake_input`. Regeln UND
   Pan-Einstellungen werden als TOML-Datei eingegeben
   (`source6/rules.example.toml` als Vorlage, Schema-Version im Feld
   `schema_version`); ohne Datei gelten eingebaute Defaults. Dafür wird
   genau eine Parser-Dep eingeführt (`toml`, s. `deps.md`).
   Verworfene Alternative: Regex/OCR-Fuzzy — mehr Fehlerfläche ohne Nutzen
   im Prototyp-Stadium.
6. **Tastatur-Mapping: ASCII 0x20–0x7e aus `get_keyboard_mapping`
   (wie Prototyp), Shift/Return erkannt; alles andere wird übersprungen +
   gezählt (sichtbarer `skipped`-Zähler im Log).** Begründung: deterministisch
   ohne xkb-Dep; dokumentierte Lücke statt stiller Falscheingabe. Erweiterung
   (xkbcommon/de) erst nach Xvfb-Nachweis des ASCII-Pfads.
7. **Querschnitts-Bullets aus dem Prompt (Firmware-Herkunft), übertragen wie
   in den Vorgängerplänen — bitte bestätigen:** (a) Messgenauigkeit →
   OCR-Genauigkeit bei skalierten ROIs: Nearest-Resize wie source5, kein
   Kalibrier-Flag; Eichung per Referenzbild im Xvfb-Smoke. Referenzspannung/
   Quarz-Toleranz entfallen (keine ADC-Hardware). (b) 3,3-V-Limits →
   Ressourcen-Clamps: ROI/Screen-Clamp, `MAX_REC_LINES 64`, Log-Deckel,
   kein OOM durch Riesen-ROI. (c) TUI-Protokoll-Versionierung → TOML-Schema
   (`schema_version` in `rules.toml`) als einzige versionierte Größe
   (TUI-Layout selbst ist unversioniert, stdout-Format stabil). (d) Modus-Wechsel während Messung →
   ROI-Wechsel invalidiert Caches + Automation pausiert einen Zyklus
   (frame-konsistente ROI-Kopie, kein Klick auf veraltete Box). (e)
   Persistente Konfiguration → keine (immer Default-Start 640 @ 0,0,
   Automation aus); Regeln sind Code, keine Config-Datei.
8. **Modul-Schnitt (Zielbild `source6/src/`):** `01_view.rs` (ROI-State +
   `to_screen_rect`), `02_capture.rs` (ROI-Capture + BGRA→Planar),
   `03_detect.rs` + `04_recognize.rs` (aus source5), `05_input.rs`
   (XTEST-Keymap + `click`/`type_text`, fehlerpropagierend), `06_rules.rs`
   (TOML-Laden + Regeln, Cooldown, Engine — ohne X11 testbar per
   Fake-Clock/Injektion), `07_tui.rs` (Dashboard-Render als reine
   String-Funktion + Event-Mapping, ohne Terminal testbar), `main.rs` nur
   Verdrahtung. Jede Datei ≤~300 Zeilen. Pan-Einstellungen (`step_divisor`,
   `step_min_px`, `roi_steps`, `default_size`) kommen aus derselben
   TOML-Datei (`01_view` liest sie, Defaults bei fehlender Datei).

## Recommended Approach

Kleinster beweisender Weg: S0 Gerüst + Deps grün (neueste Versionen),
S1 Geometrie (`to_screen_rect` + Clamp, rein testbar), S2 Input an
Xvfb-Fenster (Keymap, Click, Type — ohne OCR), S3 OCR-Übernahme aus source5
(unverändert, Tests mitgebracht), S4 Regel-Engine (ohne X11 testbar),
S5 TUI (Render als String-Funktion testbar, Terminal-Restore-Guard),
S6 Verdrahtung im Loop + Change-Detect + Cache-Invalidierung, danach T1
Härtung/E2E (Browser-Szenario im Xvfb: Treffer → Klick landet, Type kommt an,
`--dry-run` feuert nie) und T2 Abschluss (`deps.md`, Walkthrough,
Docker-Pakete). Pro Schritt `fmt`/`clippy -D warnings`/`test` grün →
Conventional Commit mit `Refs: …/task.md <ID>`.

## Work Plan

- **S0 — Gerüst + Deps + Modelle (Gate für alles).** `source6/`-Crate mit
  `main.rs`-Verdrahtung, `Cargo.toml` (neueste `crossterm`/`x11rb[xtest]`/
  `ort`, `cargo upgrade`-Protokoll), Modelle/Dict aus `source5/` referenziert
  (kopieren oder Pfad — Entscheidung dokumentieren), `cargo build` grün,
  `apt-get install xvfb` + `DISPLAY`-Notiz.
- **S1 — `01_view` + Projektion (rein, ohne X11).** `to_screen_rect`,
  `center`, Screen-Clamp; Unit-Tests (Identität bei 640, Skalierung
  320/960/1280, Clamp am Rand, Mitte-in-Box).
- **S2 — `05_input` (XTEST, ohne OCR).** Keymap-Aufbau, `click`,
  `type_text` (Shift/Enter, Skip-Zähler), Fehler als `Result`;
  Xvfb-Nachweis: Klick auf Testfenster-Button + getippter ASCII-Text kommt
  im Fenster an (Event-Log des Testfensters als Orakel).
- **S3 — `02/03/04_capture+detect+recognize` (Übernahme ohne Verhalten).**
  Code aus source5, Konstanten/Schwellen identisch; mitgebrachte Tests grün;
  Change-Detect (`memcmp`) + ROI-Wechsel-Invalidierung wie source5.
- **S4 — `06_rules` (Engine, ohne X11).** `Rule { pattern, action,
  cooldown }`, case-insensitiver Substring, eine Aktion/Zyklus, Log-Deckel;
  Tests mit injizierter Uhr + Fake-Input (Cooldown, kein Treffer →
  keine Aktion, `enabled=false` → keine Aktion).
- **S5 — `07_tui` (ohne Terminal testbar).** Render-Funktion
  (ROI/Status/Automation/ms/Tabelle/Log) + Tasten-Mapping + Restore-Guard;
  Tests auf Strings/Events, kein echtes Terminal nötig.
- **S6 — Loop-Verdrahtung.** Capture → Inferenz → Engine → TUI-Render;
  frame-konsistente ROI-Kopie; Automation-Pause einen Zyklus nach ROI-Wechsel.
- **T1 — Härtung + E2E.** Browser-Szenario im Xvfb (Testseite mit bekanntem
  Button-Text → Klick-Nachweis; Eingabefeld → Type-Nachweis), `--dry-run`
  feuert nie, Fehlerpfade (X11 weg, XTEST fehlt, Modell fehlt → Meldung +
  Exit ≠ 0, Terminal restored), Dauerlauf (kein Drift bei Pan/Zoom-Spam).
- **T2 — Abschluss.** `fmt`/`clippy`/`test` final grün, `deps.md` final,
  `plan/20260924_01_automation_tui/walkthrough.md` (implementiert vs. Plan,
  Abweichungen vom Prototyp, Xvfb-Logs, Docker-Pakete, Learnings,
  Erweiterungen).

## Validation Plan

- Pro Schritt: `cargo fmt --check`,
  `cargo clippy --all-targets -- -D warnings`, `cargo test` grün →
  committen. Reine Tests (S1-Geometrie, S4-Engine, S5-Render) laufen ohne
  X11/GPU/Netz/Modell (always-green-Basis).
- Höchst-Risiko-Gate (S2/T1): echter XTEST-Nachweis unter Xvfb — kein
  Self-Orakel: Klick/Typ-Erfolg meldet das *Zielfenster* (Button-Callback /
  Eingabefeld-Inhalt), nicht die Automation selbst. Skipped-Zähler für
  nicht-abbildbare Zeichen wird im Smoke provoziert und geloggt.
- E2E (T1): Xvfb + Testseite mit bekanntem Text → Regel feuert genau einmal
  pro Cooldown-Fenster; `--dry-run`-Lauf zeigt Log ohne Fenster-Effekt.
- Manuell (mit Bildschirm): 60-Sekunden-Session — Pan/Zoom, `a`-Toggle,
  `q`-Exit mit sauberem Terminal (`reset` nie nötig).

## Risks / Rollback

- **XTEST nicht verfügbar / vom Server abgelehnt:** `X11Input::new` prüft
  die Extension und meldet klar (`--dry-run` als Fallback); kein stilles
  Nichts-Tun. Rollback: S2-Revert, Rest (OCR+TUI) bleibt nutzbar.
- **Tastatur-Layout ≠ US-ASCII:** Keysym-Annahme bricht für Umlaute/CJK —
  dokumentierte Lücke mit Skip-Zähler statt Falscheingabe; Fallback: nur
  `Click`-Regeln nutzen.
- **Klick trifft, aber Fokus/Timing fehlt:** Click→Type-Delay als Konstante
  (50 ms Startwert), im Smoke messbar; bei Flakiness erhöhen statt
  raten.
- **Fehlklicks auf produktivem Desktop:** Default-Aus + Cooldown +
  `--dry-run` + ein-Aktion-pro-Zyklus; E2E läuft ausschließlich im Xvfb.
- **`ort` RC-Drift:** Pin aus source5 übernehmen (`2.0.0-rc.13`); Upgrade nur
  in S0 mit vollem Testlauf.
- **Prototyp-`unwrap`s im X11-Pfad:** werden zu `Result` + Exit-Code —
  Panic im Loop würde das Terminal roh hinterlassen (Restore-Guard fängt es).
- Rollback pro Schritt: jeder Task ein eigener Commit — `git revert` des
  jeweiligen `feat`-Commits stellt den Vor-Stand her.

## Open Questions

1. (entschieden 2026-09-24) Regeln + Pan-Einstellungen als TOML-Datei
   (`rules.example.toml` liegt vor); Parser-Dep `toml`.
2. Tasten `1` = hinein / `2` = heraus und `a` = scharf/unscharf wie im
   Prototyp angenommen — bestätigen oder tauschen?
3. Click→Type-Delay 50 ms / Motion 20 ms / Key 15 ms als Startwerte ok?
4. Pan-Schritt und ROI-Stufen aus source5 übernehmen (`max(8, size/16)`,
   `[320,480,640,960,1280]`)? Default: ja.
5. Querschnitts-Defaults aus „Key Decisions" Punkt 7 (keine Persistenz,
   TOML-`schema_version`, Nearest ohne Kalibrier-Flag) — bestätigen oder
   ändern?
6. Fehlende Requirements, die ich ergänzt habe (bitte streichen, was nicht
   gewünscht ist): `--dry-run`-Flag, Skip-Zähler für nicht-typbare Zeichen,
   Automation-Pause nach ROI-Wechsel, Terminal-Restore-Guard, ein-Aktion-
   pro-Zyklus mit Cooldowns, Docker-Paketliste im Walkthrough.

## Kontext für einen unabhängigen Agenten (Pflichtlektüre)

1. `plan/20260924_01_automation_tui/prompt.txt` — dieser Auftrag (Automation
   ohne Bildübertragung, source6, Datei-/Tool-Regeln, `deps.md`-Pflicht).
2. `plan/20260924_01_automation_tui/prototype.md` — Startcode (view/ocr/
   input/automation/main-Skizzen; `unwrap`s und ASCII-Lücken nicht
   übernehmen).
3. `plan/20260924_01_automation_tui/plan.md` (diese Datei) + `task.md` —
   was/wie zu tun ist.
4. `plan/20260924_01_automation_tui/deps.md` — Dependency-Registry
   (Org/Projekt-Notation für DeepWiki-Abfragen).
5. `examples/26_onnx/source5/src/{01_view,02_capture,03_detect,04_recognize}.rs`
   — Übernahme-Basis für S3 (Verhalten + Tests bleiben identisch);
   `05_overlay.rs` + `main.rs` als Negativ-Vorlage (macroquad-Pfad entfällt).
6. `examples/26_onnx/source5/Cargo.toml` + `source5/PP-OCRv6_small_*.onnx` +
   `source5/inference.yml` — Pins und Modell-/Dict-Quellen.
7. `examples/26_onnx/source0/tests/{geometry,cli_smoke}.rs` — Test-Stil
   (modellfreie Geometrie-Tests + Xvfb-Smoke-Trennung).
8. `examples/25_dnb/plan/…/{plan,task,deps}.md` — Stil-Vorlage für
   Plan/Tasks/Deps (nicht für Inhalte).
9. Extern (DeepWiki-Muster aus `deps.md`): `psychon/x11rb` (XTEST-/
   GetImage-API), `crossterm-rs/crossterm` (Raw-Mode/Event-API),
   `pykeio/ort` (Session-API); Repo-Kontext: `plops/cl-rust-generator`.
10. Live-Umgebung: `echo $DISPLAY`, `which Xvfb`, `dmesg | tail` bei
    Geräte-Zweifeln; `apt-get install xvfb` für Tests.

## Task-Vorlauf (wird in der Ausführung zu `task.md`)

Seriell, jeder Schritt mit Gates (`fmt`, `clippy -D warnings`, `test` grün)
und eigenem Conventional Commit (`Refs: …/task.md <ID>`): S0 Gerüst+Deps,
S1 Projektion, S2 XTEST-Input, S3 OCR-Übernahme, S4 Regel-Engine, S5 TUI,
S6 Loop, T1 Härtung+E2E (Browser-Szenario im Xvfb), T2 `deps.md`+Walkthrough.
Jeder Block bekommt Implementierung + Host-Tests + Xvfb-Nachweis, danach erst
die TUI-Feinschliff-Tasks.

## Commit-Konvention

Conventional Commits, ein logischer Schritt pro Commit:
`feat|fix|test|docs|chore|refactor(<scope>): <kurz, imperativ, ≤72 Zeichen>`.
Body: was + warum, betroffenes Modul, Validierung
(`cargo fmt --check`, `cargo clippy`, `cargo test`, Xvfb-Ergebnis).
Footer: `Refs: plan/20260924_01_automation_tui/task.md <ID>`.
Modelldateien (`*.onnx`, `inference.yml`) werden per `.gitignore`
ausgeschlossen oder per Pfad referenziert (keine Binaries committen).
Nie ohne grüne Gates committen; keine fremden/untracked Dateien anfassen.
