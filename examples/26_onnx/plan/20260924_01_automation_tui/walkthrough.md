# Walkthrough — 20260924_01_automation_tui

`source6/`: X11-Live-OCR (PP-OCRv6) + TUI-Dashboard + Button-Klick-/
Text-Automation per XTEST — ohne Bildübertragung. Stand: 2026-09-24,
alle Gates grün, beide Smokes PASS.

## Was implementiert wurde (vs. Plan)

| Plan | Stand |
|---|---|
| S0 Gerüst + Deps + Modelle | ✅ `x11_ocr_automation`-Crate; `crossterm 0.29.0`, `x11rb 0.14.0` + `xtest`, `ort 2.0.0-rc.13`, `toml 0.9.12` (jeweils neueste); Modelle per `fetch_assets.sh` nach `source6/` geladen (SHA-verifiziert) |
| S1 `01_view` + Projektion | ✅ ROI-State aus source5 + `to_screen_rect` (640-Raum → absolute Pixel, Screen-Clamp); Pan-Formel aus `[pan]` via Struct-Felder |
| S2 `05_input` (XTEST) | ✅ Keymap aus Server, `click`/`type_text` mit Delays als Konstanten, Skip-Zähler, alles `Result`; dazu Sink-Adapter (`DrySink`, `X11Input as Sink`) |
| S3 Capture/Detect/Recognize | ✅ Byte-identisch aus source5 übernommen (`cmp`-belegt); dazu `try_capture_roi` für saubere X11-Fehler |
| S4 TOML + Regel-Engine | ✅ `06_config` (händisches `Value`-Parsen ohne serde, `schema_version`-Check) + `07_rules` (Substring, Cooldown, 1 Aktion/Zyklus, Log-Deckel 10, Fehler ins Log) |
| S5 TUI | ✅ `08_tui`: `render` als reine String-Funktion, `map_key`, `FrameDisplay`-Trait; `09_canvas`: räumliches Text-Abbild (`render_spatial`), `TuiGuard` (`Drop`-Restore), `render_frame`-Weiche, CJK-Doppelbreite ohne Extra-Dep |
| S6 Loop | ✅ Capture → Inferenz → Engine → Anzeige; Change-Detect, ROI-Wechsel invalidiert Caches + pausiert Automation 1 Zyklus; `--dry-run`, `--rules`, `--help`, Exit-Codes 0/1/2 |
| T1 Härtung + E2E | ✅ `smoke_xvfb.sh` PASS (XTEST-Test, OCR-Nachweis, Fehlerpfade); `test_duckai.sh` PASS (Anonymitäts-Hinweis + Witz) |
| T2 Abschluss | ✅ Gates grün, `deps.md` final, dieser Walkthrough |

Neue Programme/Skripte in `source6/`: `scripts/fetch_assets.sh`,
`scripts/test_duckai.py` + `test_duckai.sh`, `scripts/smoke_xvfb.sh`,
`rules.example.toml`, `scripts/README.md`.

## Abweichungen (Plan, Prototyp)

1. **Modulschnitt:** `06_rules.rs` (502 Zeilen) → `06_config.rs` (299) +
   `07_rules.rs` (248) getrennt; TUI → `08_tui.rs`. Regel aus dem Prompt,
   keine Verhaltensänderung.
2. **`Sink` fallibel + objekt-sicher:** Fehler landen im Engine-Log statt
   still zu versagen; `Box<dyn Sink>` statt Enum (weniger Code in `main`).
3. **`--headless-frames N` (neu):** Batch-Klartext-Modus, weil das TUI ein
   Pty braucht und diese Umgebung Pty-Kinder in Hintergrund-Sessions per
   SIGTTOU suspendiert (zwei hängende Smokes à 30 min, dann umgestellt).
   Das TUI-Render selbst decken Unit-Tests ab.
4. **Prototyp-Korrekturen:** `unwrap`/`expect` → `Result` + Exit-Codes;
   `Time::CURRENT_TIME` existiert in x11rb 0.14 so nicht (`_EVENT`-Konstanten
   + `0` als CurrentTime); `get_keyboard_mapping` als Free-Function;
   `str::parse::<toml::Value>` kann keine Dokumente (`Table` nötig);
   `evaluate` ohne Kontext trifft bei iframes den falschen Frame (nur im
   Python-CDP-Test relevant, dort per DOM-Domain gelöst).
5. **`center()` aus S1 gestrichen:** Klick-Mitte rechnet die Engine
   (mit Clamp); kein toter Code für Clippy.
6. **`main.rs` (~300) / `main`-Verdrahtung:** eine Zuständigkeit
   (Verdrahtung: `App`-Bundle, `Box<dyn Sink>`, Display-Weiche); Restzähler
   über „ca. 300" bewusst akzeptiert statt weiter zu kürzen. `View::default`
   entfiel zugunsten von `View::from_pan` (TOML-Werte direkt), `Sink` ist
   objekt-sicher (`InputError` statt assoziiertem Typ).
7. **TUI-Umbau auf Nutzerwunsch (räumliches Abbild):** Die Box-Tabelle
   produzierte bei variabler Zeilenzahl + Terminal-Umbruch Müll
   (Fragment-Überlagerung). Jetzt malt `09_canvas::render_spatial` jeden
   Text skaliert an seine Fensterposition (Kopf: 4 Zeilen, Rest Leinwand,
   keine Überlappung, Clipping, kein Scrollen); `TuiDisplay` löscht voll
   (`Clear::All`) statt ab Cursor. Tabelle bleibt für `--headless-frames`
   (grepbar). CJK/Mathe-Zeichen bleiben erhalten (Unifont) und zählen als
   zwei Zellen (hand-gerollte East-Asian-Width, kein Extra-Crate).

## Messungen / Nachweise

- `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`,
  `cargo test`: **36 passed, 0 failed, 1 ignored** (XTEST-Test nur unter Xvfb;
  darunter 5 Canvas-Tests: Position, Clipping, Überlappung, Doppelbreite).
- Xvfb-Smoke (`smoke_xvfb.sh`, Exit 0): XTEST-Test PASS; Batch-OCR auf
  xterm-Text: `HELT0 OCR WORLD 123`, `SECOND LINE ABC XVZ` (kleine
  OCR-Fehler wie in source5 bekannt), **102 ms** (Det 52.7 + Rec 49.3);
  Fehlerpfade `--help`=0, TOML=2, Display=1.
- Duck.ai-Test (`test_duckai.sh`, Exit 0): Frage getippt, „Ask" geklickt,
  Antwort mit „Anonymized by DuckDuckGo. Zero data retention …" + Witz
  („Why do programmers prefer dark mode? Because light attracts bugs.").
- TOML-Beispiel per `tomllib` validiert (2 Regeln, Schema 1).

## Docker-Pakete (fürs Image)

`xvfb xterm xdotool scrot` (Tests), `libnss3 libatk1.0-0t64
libatk-bridge2.0-0t64 libcups2t64 libdrm2 libxkbcommon0 libxcomposite1
libxdamage1 libxfixes3 libxrandr2 libgbm1 libpango-1.0-0 libcairo2
libasound2t64 libnspr4` (Chrome-Laufzeit), `ca-certificates`
(Modell-Download), Chrome-for-Testing nach `/opt/chrome-test`
(portabel, kein Snap). `fonts-unifont` nur für den source5-Viewer nötig.

## Learnings

- Pty-Programme hängen in Hintergrund-Sessions (SIGTTOU) — CI-Pfade immer
  pty-frei auslegen (`--headless-frames` statt `script(1)`).
- `document.body.innerText` lügt bei iframes (`about:srcdoc`); DOM-Domain
  (`getDocument`/`getOuterHTML`) ist deterministisch.
- duck.ai-UI ist zustandsabhängig (Text- vs. Icon-Submit-Button, Platzhalter
  statt Button) — Finder mit Stufen (Text → aria-label → Positions-Fallback)
  plus frisches Profil je Lauf machen den Test stabil.
- Clippy-`dead_code` erzwingt ehrliche Verdrahtung: ungenutzte Prototyp-APIs
  (`center`, `capture_roi`-Wrapper) mussten weg oder angeschlossen werden.

## Mögliche Erweiterungen

- Live-XTEST-Nachweis mit Fenster-Orakel (Klick-Callback im Zielfenster
  statt nur Request-Akzeptanz) — braucht ein instrumentiertes Testfenster.
- Tastatur-Layouts jenseits US-ASCII (xkbcommon) — aktuell Skip-Zähler.
- Regel-Datei per `--watch`/Reload zur Laufzeit; TUI-Interaktion (Regel per
  Tastendruck anlegen); `--save-frame` für Debug-Screenshots.
- Dauerlauf-Smoke (Pan/Zoom-Spam, Drift) auf echtem Display — hier wegen
  fehlendem Bildschirm nur per Xvfb-Statik abgedeckt.
