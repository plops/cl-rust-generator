# Implementierungsplan — 20260925_01_robust_automation

Robuste Automation in `source6/`: Fuzzy-OCR-Matcher (Glyph-Konfusion
`I`/`l`, Dropped-Dots, Cursor-Artefakte) + Fenster-Fokus-Automatik
(`set_input_focus` + `_NET_ACTIVE_WINDOW`, 100-ms-Settle zwischen Klick
und Tippen) + kleine Refactors (kein neues Verhalten, keine neuen Deps).

Auftrag: Wol Pumba (wolpumba@gmail.com) · Linux (Ubuntu 26, Docker, X11)
· Prompt: `plan/20260925_01_robust_automation/prompt.txt` · Prototyp:
`plan/20260925_01_robust_automation/prototype.md` (AI-Vorschläge, 6 Bausteine)
· Probleme aus `source6/collect.sh` (Matcher zu strikt, Fokus manuell) ·
Basis/Ziel: `examples/26_onnx/source6/` (direkt Rust, Stand 2026-09-24:
36 Tests grün, Smokes PASS) · Pfad `examples/25../source6` aus dem Prompt
existiert nicht — gemeint ist `examples/26_onnx/source6` ·
Konventionen: Vorgängerpläne `plan/20260924_01_automation_tui/` und
`plan/20260923_01_keys_source5/`, Stil-Vorlage `examples/25_dnb`

## Goal

Die Automation erkennt Buttons auch dann, wenn das OCR Bildfehler macht
(`Ask Meta Al..` statt `Ask Meta AI...`, Cursor-`|` am Ende), und tippt
auch dann ins richtige Fenster, wenn Firefox vorher nicht fokussiert war
— ohne dass die Maus von Hand ins Fenster gesetzt werden muss. Der Code
bleibt klein, übersichtlich und ohne neue Abhängigkeiten.

## Success Criteria

1. `fuzzy_ocr_match("Ask Meta AI...", "Ask Meta Al..")` ist wahr (sowie die
   anderen Prototyp-Fälle: Cursor-`|`, `Sign ln`, `GoogIe`, Duck.ai mit
   Cursor; `Cancel` vs `Confirm` bleibt falsch).
2. `ClickAndType` läuft über `Sink::click_and_type`: Klick → 100 ms Pause →
   Tippen (die Pause liegt garantiert DAZWISCHEN, nicht danach).
3. `click()` aktiviert vorher das Fenster unter dem Klickpunkt
   (`translate_coordinates` → Kind-Fenster, sonst Root; `set_input_focus` +
   `_NET_ACTIVE_WINDOW`, best-effort ohne Fehlerpfad) und sendet
   Button-Events mit expliziten Koordinaten.
4. Refactors ohne Verhalten: `norm_rgb`-Dedup (02), `load_dict` per
   `trim_matches` (04), `as_u32`-Kettenform (06) — alle bestehenden Tests
   bleiben grün.
5. `rules.metaai.toml` (natürliches Pattern `Ask Meta AI...`, eine
   `click_and_type`-Regel) parst fehlerfrei (Nachweis per Exit-Code).
6. Gates grün: `cargo fmt --check`,
   `cargo clippy --all-targets -- -D warnings`, `cargo test`
   (inkl. neuer Matcher-Tests), Xvfb-Smoke erneut PASS; keine Datei über
   ~300 Zeilen; danach Walkthrough mit Docker-Paketen (unverändert).

## Context And Current Facts

- `source6/collect.sh` (gelesen): zwei Probleme — (a) Substring-Matcher
  (`hit.text.to_lowercase().contains(pattern)`) scheitert an `I`→`l` und
  fehlenden Ellipsen-Punkten (Workaround heute: Pattern auf `Ask Meta A`
  kürzen); (b) Fokus muss von Hand per Maus ins Firefox-Fenster.
- `prototype.md` (gelesen, 6 Bausteine + Verifikationstabelle): (1) Fuzzy-
  Matcher (`norm_ocr_str` + Sliding-Levenshtein, Toleranz nach
  Pattern-Länge), (2) `focus_at` + exakte Button-Koordinaten +
  `click_and_type` mit 100 ms, (3) `norm_rgb`-Dedup, (4) `load_dict` per
  `trim_matches`, (5) `as_u32`-Kettenform, (6) `rules.metaai.toml` mit
  einer Regel.
- Code-Stand (verifiziert): `07_rules.rs` (248 Zeilen, Matcher an
  `evaluate` Zeile ~103, `PendingFire`-Index-Schleife),
  `06_config.rs` (299, Pattern wird in `parse_rule` lowercased gespeichert),
  `05_input.rs` (259, Button-Events mit `(0,0)`, `CLICK_TYPE_DELAY_MS = 50`
  wird in `main.rs` NACH dem Tippen geschlafen — der im Prototyp
  diagnostizierte 0-ms-Bug), `02_capture.rs` (Normalisierung doppelt:
  Zeilen ~85 und ~115), `04_recognize.rs` (`load_dict` mit manuellem
  Quote-Stripping), `main.rs` (`X11Input as Sink` nutzt den Misplaced-Delay).
- Extern verifiziert: x11rb-Registry (`translate_coordinates`,
  `set_input_focus(conn, revert_to, focus: Into<Window>, time: Into<Timestamp>)`,
  `intern_atom`, `send_event`, `CLIENT_MESSAGE_EVENT = 33`,
  `ClientMessageData: From<[u32; 5]>`, `EventMask::{SUBSTRUCTURE_REDIRECT,
  SUBSTRUCTURE_NOTIFY}`) + DeepWiki `psychon/x11rb` (Focus-Praxis:
  `set_input_focus` + `_NET_ACTIVE_WINDOW`-ClientMessage ans Root-Fenster,
  Datenlayout nach EWMH `[source=1, timestamp, …]`).
- Umgebung: Xvfb/xterm/xdotool/Chrome vorhanden (eingerichtet 2026-09-24);
  X11-Tests laufen per Ignored-Test + `smoke_xvfb.sh`.

## Constraints And Non-goals

- Keine neuen Deps (Matcher + Fokus sind Handcode auf `x11rb`); direkt
  Rust; klein/lesbar; Datei-Regeln aus dem Prompt (nummeriert, ≤~300,
  Tests bei Typen, `main.rs` nur Verdrahtung, Aufteilung ohne Verhalten).
- `focus_at` ist best-effort (Fehler werden ignoriert wie im Prototyp) —
  kein harter Fehlerpfad, damit ein eigenwilliger WM nie die Automation
  stoppt; Begründung steht in den Risiken.
- Non-goals: kein Regex/Fuzzy-Edit-Distanz als User-Feature (nur intern),
  kein Wayland, keine Fokus-Garantie per WM-Protokoll-Verhandlung, keine
  Tastatur-Layout-Erweiterung (Skip-Zähler bleibt), kein Bild-Streaming.

## Key Decisions

1. **Matcher als neues Modul `07_match.rs`; `07_rules`→`08_rules`,
   `08_tui`→`09_tui`, `09_canvas`→`10_canvas`.** `norm_ocr_str` +
   `levenshtein_substring` + `fuzzy_ocr_match` + Tests ≈ 130 Zeilen —
   in `07_rules` (248) eingebaut wären es ~340 (Regelbruch). Die
   Umbenennung ist mechanisch (`mod`-Pfade in `main.rs`, sonst nichts).
   Verworfene Alternative: Matcher in `07_rules` lassen und Datei teilen —
   reißt Engine und Matcher auseinander, die zusammengehören (Test-Regel:
   Typ + Tests in einer Datei → Matcher IST die zusammengehörige Einheit).
2. **Pattern roh speichern, normalisieren beim Match.** `parse_rule`
   lowercased heute (dann wäre `I`→`l`-Faltung doppelt/verwaschen);
   `fuzzy_ocr_match` faltet beide Seiten selbst. Bestehende Tests nutzen
   Kleinschreibung-Patterns — bleiben grün, da Norm kleinschreibt.
3. **`click_and_type` als Default-Methode im `Sink`-Trait
   (100 ms, Konstante `FOCUS_SETTLE_MS` in `05_input`).**
   Engine nutzt sie für `ClickAndType`; der deplatzierte 50-ms-Sleep nach
   dem Tippen in `main.rs` + `CLICK_TYPE_DELAY_MS` entfallen. FakeSink erbt
   die Default-Methode (kein Sleep im Test — `Instant`-frei, sofort grün).
4. **`focus_at` vor jedem Klick, Fehler schlucken.** `translate_coordinates`
   (Kind-Fenster oder Root) → `set_input_focus(PARENT)` → `_NET_ACTIVE_WINDOW`
   (Daten `[1, NOW, 0, 0, 0]`, Maske REDIRECT|NOTIFY) → `flush`. Button-Events
   mit echten `(x, y)` statt `(0, 0)`. Begründung best-effort: Fokus ist
   eine Höflichkeit ans WM, kein Vertrag — ein fehlender WM-Antwort darf
   keinen Klick verhindern (der XTEST-Klick selbst fokussiert meist eh).
5. **Refactors 1:1 aus dem Prototyp** (`norm_rgb`, `load_dict`,
   `as_u32`): keine Verhaltensänderung, bestehende Tests sind das Orakel
   (1:1-Byte-Gate, Dict-Test, TOML-Fehler-Tests).
6. **`rules.metaai.toml` wie im Prototyp** (eine Regel, langes
   Red-Panda-Prompt, Cooldown 5 s). Daneben bleiben `rules.example.toml`
   und `rules.duckai.toml` unverändert — welche Datei gefahren wird,
   entscheidet `--rules`.
7. **Querschnitts-Bullets (Firmware-Herkunft), übertragen:** (a)
   Messgenauigkeit/Kalibrierung → OCR-Toleranz ist jetzt längenproportional
   (`max_edits` 0/1/2/3), Eichung per Prototyp-Fällen als Unit-Tests;
   Referenzspannung/Quarz entfallen (keine ADC-Hardware). (b) 3,3-V-Limits
   → unverändert Clamps/Deckel. (c) TUI-Protokoll → TOML-`schema_version`
   unverändert (Matcher braucht keine Schema-Änderung: Pattern bleibt
   String). (d) Modus-Wechsel → ROI-Invalidierung unverändert (Matcher
   sieht nur frame-konsistente Hits). (e) Persistenz → unverändert keine
   (TOML-Dateien sind die Konfiguration).

## Recommended Approach

Kleinster beweisender Weg: R0 Docs (`plan/task/deps.md`), R1 Matcher-Modul
+ Umbenennung (rein, ohne X11), R2 Fokus + `click_and_type` (XTEST-Test
unter Xvfb beweist Akzeptanz), R3 Refactors (bestehende Tests als Orakel),
R4 `rules.metaai.toml` + Parse-Nachweis, danach T1 Gates + Smokes +
Walkthrough. Pro Schritt `fmt`/`clippy -D warnings`/`test` grün.

## Work Plan

- **R0 — Plan-Docs.** Diese Datei + `task.md` (serielle Schritte R1–R4/T1)
  + `deps.md` (keine neue Dep — Vermerk).
- **R1 — `07_match.rs` + Umbenennung.** `norm_ocr_str` (Faltung
  `I/l/1/|/!/i/[/]`→`l`, `O/o/0`→`o`, Cursor-/Satzzeichen-Trim,
  Lowercase), `levenshtein_substring` (1D-DP), `fuzzy_ocr_match`
  (Substring, sonst Toleranz 0/1/2/3 nach Länge); Pattern-Speicherung roh;
  `evaluate` nutzt `fuzzy_ocr_match`; `07_rules`→`08_rules`,
  `08_tui`→`09_tui`, `09_canvas`→`10_canvas` (+ `main.rs`-Pfade;
  Modulnamen `rules`/`tui`/`canvas` bleiben, nur Dateinamen ändern sich).
  Tests: beide Prototyp-Fuzzy-Tests + bestehende Suite.
- **R2 — Fokus + Klick-Pfad (`05_input`, `08_rules`, `main`).**
  `focus_at`, Button-Events mit `(x, y)`, `FOCUS_SETTLE_MS = 100`,
  `Sink::click_and_type`-Default, Engine nutzt sie; `CLICK_TYPE_DELAY_MS`
  + Post-Type-Sleep in `main.rs` entfernen. Test: Ignored-XTEST-Test
  (läuft über `focus_at`, assertet `Ok` + `skipped == 0`).
- **R3 — Refactors ohne Verhalten.** `norm_rgb` in `02_capture`,
  `load_dict`-Trim in `04_recognize`, `as_u32`-Kette in `06_config`.
  Orakel: bestehende Tests (keine neuen nötig; grün = bewiesen).
- **R4 — `rules.metaai.toml`.** Datei wie im Prototyp; Nachweis:
  Binary mit `--rules` parsen (Exit 1 ohne Display = TOML ok, Exit 2 =
  Fehler).
- **T1 — Gates + Smokes + Walkthrough.** `fmt`/`clippy`/`test` final,
  `smoke_xvfb.sh` + `test_duckai.sh` (unveränderte Flächen, Regression),
  `walkthrough.md` (Abweichungen, Messungen, Docker-Pakete — unverändert).

## Validation Plan

- Pro Schritt: `cargo fmt --check`, `cargo clippy --all-targets -- -D
  warnings`, `cargo test` grün. Höchstrisiko-Gate (R1): beide
  Prototyp-Fuzzy-Tests WORTWÖRTLICH übernehmen (inkl. Negativ-Fall
  `Cancel`/`Confirm`) — sie sind das Orakel für die Nutzer-Probleme.
- R2-Gate: Ignored-Test unter Xvfb grün (Server akzeptiert Fokus-Sequenz +
  Klick + Tippen); zusätzlich `smoke_xvfb.sh` (Xvfb + xterm vorhanden).
- R4-Gate: Exit-Code-Trick (1 = parst, 2 = TOML kaputt) ohne X11.
- Manuell (mit Bildschirm, optional): Firefox + `rules.metaai.toml`,
  `a` drücken, Platzhalter-Match + Fokus-Wechsel beobachten.

## Risks / Rollback

- **Levenshtein-Overmatch** (falsche Buttons): Toleranz ist längenproportional
  und klein (≤3 nur bei langen Patterns); Negativ-Test + Cooldowns begrenzen
  Schaden; Rollback: `fuzzy_ocr_match` → Substring (eine Zeile in `evaluate`).
- **WM ignoriert `_NET_ACTIVE_WINDOW`** (z. B. strikte Fokus-Policies):
  best-effort → kein Fehler, alter Pfad (reiner XTEST-Klick) bleibt wirksam;
  Rollback: `focus_at`-Aufruf entfernen (eine Zeile in `click`).
- **Umbenennung bricht Pfade:** mechanisch (`mod`-Zeilen + `crate::`-Pfade);
  Compiler findet jede Stelle; Tests beweisen Gleichheit.
- **`g`-Faltung?** Prototyp faltet `g` NICHT (nur `I/l/1`- und `O/o/0`-Klassen)
  — so übernehmen, keine eigenen Klassen erfinden.
- Pro Schritt ein Commit (Conventional, `Refs: task.md <ID>`); Revert je
  Schritt möglich. Keine Commits ohne explizite Aufforderung (Repo-Regel).

## Open Questions

1. Fokus-Erfolg sichtbar machen (z. B. Zähler im Dashboard wie `Skip`)?
   Default: nein (silent best-effort, Verhalten zuerst beweisen).
2. Toleranz pro Regel per TOML (`max_edits` optional)? Default: nein
   (globale längenproportionale Regel reicht für die dokumentierten Fälle).
3. Fehlende Requirements, ergänzt (bitte streichen was nicht gewünscht):
   100-ms-Konstante als benannte Konstante; Button-Koordinaten explizit;
   `rules.metaai.toml` als dritte Beispiel-Datei.

## Kontext für einen unabhängigen Agenten (Pflichtlektüre)

1. `plan/20260925_01_robust_automation/prompt.txt` — dieser Auftrag.
2. `plan/20260925_01_robust_automation/prototype.md` — die 6 Bausteine +
   Verifikationstabelle (Startpunkt, kein Copy-Paste ohne Prüfung).
3. `source6/collect.sh` — Nutzer-Probleme im Originalton (Matcher,
   Fokus, Klein-/Lesbar-Wunsch).
4. `plan/20260925_01_robust_automation/plan.md` (diese Datei) + `task.md`.
5. `plan/20260925_01_robust_automation/deps.md` — Org/Projekt-Notation.
6. `examples/26_onnx/source6/src/{05_input,06_config,07_rules}.rs` (+ Tests)
   — die drei geänderten Module; `02_capture`/`04_recognize` nur für R3.
7. `examples/26_onnx/source6/rules.{example,duckai}.toml` — TOML-Stil.
8. `examples/26_onnx/source6/scripts/{smoke_xvfb,test_duckai}.sh` —
   Smoke-Harness (Xvfb + CDP-Test).
9. Extern: `psychon/x11rb` (Focus-/XTEST-API; Registry-Signaturen +
   DeepWiki-Praxis), Repo-Kontext `plops/cl-rust-generator`.
10. Live-Umgebung: Xvfb/xterm/Chrome vorhanden; `DISPLAY` für X11-Tests.

## Sources

- [set_input_focus in x11rb::protocol::xproto](https://docs.rs/x11rb/0.14.0/x11rb/protocol/xproto/fn.set_input_focus.html) (docs.rs, x11rb 0.14.0): Signatur mit `revert_to: InputFocus`, `focus: Into<Window>`, `time: Into<Timestamp>` — trägt Key Decision 4 (Fokus-Sequenz); ergänzend lokal in der Cargo-Registry verifiziert (`translate_coordinates`, `intern_atom`, `send_event`, `CLIENT_MESSAGE_EVENT`, `ClientMessageData: From<[u32; 5]>`, `EventMask`-Flags).

## Commit-Konvention

Conventional Commits, ein logischer Schritt pro Commit:
`feat|fix|test|docs|chore|refactor(<scope>): <kurz, imperativ, ≤72 Zeichen>`.
Body: was + warum, betroffenes Modul, Validierung
(`cargo fmt --check`, `cargo clippy`, `cargo test`, Xvfb-Ergebnis).
Footer: `Refs: plan/20260925_01_robust_automation/task.md <ID>`.
Binaries/Modelle nie committen (`.gitignore` beachten). Nie ohne grüne
Gates committen; keine fremden/untracked Dateien anfassen. (Ob überhaupt
committet wird, entscheidet Wol Pumba — kein Commit ohne Aufforderung.)
