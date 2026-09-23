# Implementierungsplan — 20260923_01_keys_source5

ROI-Pan (Pfeiltasten) + Zoom (Tasten 1/2) für den source5-Live-OCR-Viewer
(x11rb-Capture → PP-OCRv6 via ort → macroquad-Anzeige). Kleiner,
übersichtlicher, effizienter Code, keine neuen Abhängigkeiten.

Auftrag: Wol Pumba (wolpumba@gmail.com) · Linux (Ubuntu 26, Docker, X11)
· Prompt: `plan/20260923_01_keys_source5/prompt.txt`
· Code: `examples/26_onnx/source5/` (bestehend, `src/main.rs`, 463 Zeilen)
· Konventionen: Vorgängerplan `plan/20260920_01_capture/{plan,task,deps}.md`,
Stil-Vorlage `examples/25_dnb`, Code-Patterns `examples/26_onnx/source0/`

## Goal

Der source5-Viewer liest heute starr die Region (0,0,640×640) oben links.
Ziel: Der Anwender verschiebt die Capture-Region mit den Pfeiltasten frei auf
dem Bildschirm und zoomt mit `1` (hinein = kleinere ROI, Details größer) und
`2` (heraus = größere ROI, mehr Übersicht). Bei 1:1 (ROI = 640×640, Fenster
640×640) läuft kein aufwändiger Interpolationspfad: weder CPU-Resize noch
GPU-Skalierung mit Filter. Kein neues Crate, keine neue System-Abhängigkeit.

## Success Criteria

1. Pfeiltasten verschieben die ROI live; die ROI bleibt immer vollständig
   innerhalb der Bildschirmgeometrie (Clamp, kein Panic, kein X11-Fehler).
2. `1` verkleinert die ROI eine Stufe (hinein), `2` vergrößert sie eine Stufe
   (heraus); Stufen und Startwert sind dokumentiert, Zoom über die
   Bildschirmgrenzen hinaus ist unmöglich.
3. Bei 1:1 ist der Capture→Inferenz→Anzeige-Pfad bit-identisch zum heutigen
   Verhalten und ruft keinen Resize-/Interpolationscode auf (per Test
   nachgewiesen: 1:1-Frame ohne Resize-Funktion, Byte-Vergleich).
4. OCR (Detektion + Erkennung), Change-Detector (kein Re-Inferenz bei
   statischem Bild) und Druck-Deduplizierung funktionieren nach ROI-Wechsel
   korrekt weiter (keine veralteten Boxen/Texte aus der alten Region).
5. HUD zeigt ROI-Position/Größe und Zoom-Stufe plus Tasten-Hilfe; `Escape`
   beendet wie bisher.
6. Datei-Regeln eingehalten: nummerierte Module in Datenfluss-Reihenfolge,
   keine Datei deutlich über ~300 Zeilen, `main.rs`/`lib.rs` nur
   Modul-Deklaration + Verdrahtung, Aufteilung ohne Verhaltensänderung
   (vorher/nachher: alle Tests plus Smoke grün).
7. Gates grün: `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`,
   `cargo test` (davon ein always-green Kern ohne X11/Modell/Font), Xvfb-Smoke
   mit Tasten-Simulation; `deps.md` (Org/Projekt-Notation), `task.md`
   (serielle Schritte), danach Walkthrough unter
   `plan/20260923_01_keys_source5/walkthrough.md`.

## Context And Current Facts

- `source5/src/main.rs` (463 Zeilen, einzige Quelldatei): `SIZE = 640`,
  `PLANE = 640²`; `OcrEngine` (det-+rec-Sessions, wiederverwendete
  `det_input`-/`rec_input`-/`visited`-/`bfs_queue`-Buffer); `prepare_inputs`
  (BGRA→planar-RGB-normalisiert + RGBA-Textur in einer Schleife);
  `detect` (DBNet, `postprocess_dbnet` mit BFS-Komponenten, Schwellen
  `DET_THRESH 0.3` / `BOX_THRESH 0.6` / `UNCLIP_RATIO 1.5`); `recognize`
  (`preprocess_crop` Nearest-Sampling auf `REC_H = 48`, Breite auf 32er
  gerundet, clamp 32..960; `ctc_decode` mit `MAX_REC_LINES = 64`);
  Main-Loop (`#[macroquad::main]`, Fenster 640×640): `xproto::get_image`
  ZPixmap von Root mit (0,0,640,640) → Vollbild-`memcmp` als Change-Detector
  (`prev_screen_bytes != reply.data`) → bei Änderung Inferenz + deduplizierter
  Druck (`current_lines != prev_printed_lines`) → `cached_boxes` rendern
  (Box + Unifont-Label, HUD `PAUSED (STATIC)`/`ACTIVE` mit Det-/Rec-ms, FPS).
- Tastatur-API (lokal verifiziert, macroquad 0.4.16 / miniquad 0.4.11 im
  Cargo-Cache): `is_key_down(KeyCode)` (gehalten, für weiches Pannen),
  `is_key_pressed(KeyCode)` (einmal pro Druck, für Zoom-Stufen),
  `KeyCode::{Left,Right,Up,Down,Key1,Key2,Escape}` existiert.
  Display-API: `draw_texture` (1:1, keine `dest_size`, kein Resampling-Parameter),
  `draw_texture_ex` mit `dest_size` für skalierte Anzeige,
  `Texture2D::set_filter(FilterMode::Nearest)` (miniquad-`FilterMode::{Nearest,Linear}`).
- Capture-API (lokal verifiziert, x11rb 0.14.0): `xproto::get_image(conn,
  Z_PIXMAP, drawable, x: i16, y: i16, w: u16, h: u16, …)` — beliebige
  ROI-Offsets sind nativ möglich; Bildschirmmaße stehen im
  `x11rb`-Setup (`width_in_pixels`/`height_in_pixels`, Nutzung z. B. in
  `x11rb-0.14.0/src/cursor/mod.rs` belegt).
- Wiederverwendbare Patterns aus `source0` (gelesen): `02_capture.rs`
  `clamp_region` (negativ→schieben+kürzen, Überlauf→abschneiden, leer→Fehler,
  mit Unit-Tests); `05_view.rs` Nearest-Blit + `zoomed_size`-Tests als Vorlage
  für reine, display-freie Geometrie-Helfer.
- Offene Befunde (entscheidungsrelevant): Im Repo liegen **keine**
  Modell-/Dict-/Font-Dateien (`PP-OCRv6_small_det.onnx`,
  `PP-OCRv6_small_rec.onnx`, `inference.yml`, `/usr/share/fonts/unifont/…`
  fehlen; `source5/` enthält nur `Cargo.toml`/`collect.sh`/`src/main.rs`);
  `main.rs` kompiliert so aktuell nicht. `Xvfb` ist im Container nicht
  installiert, `DISPLAY` leer. `source5/Cargo.lock` pinnt `macroquad 0.4.16`,
  `ort 2.0.0-rc.13`, `x11rb 0.14.0` (Repo-`.gitignore` ignoriert `*.lock`,
  Befund aus Vorgänger-Walkthrough beachten). Pfad `examples/25../source0`
  existiert nicht — gemeint ist `examples/26_onnx/source5` (analog zur
  Vorgänger-Klärung `25../source0` → `26_onnx/source0`).
  Walkthrough-Pfad `plan/20260912_02_full/walkthrough.md` ist ein Rest aus
  einem anderen Vorhaben — verbindlich ist
  `plan/20260923_01_keys_source5/walkthrough.md`.

## Constraints And Non-goals

- Minimal/übersichtlich/effizient: **kein neues Crate**, keine neue
  System-Abhängigkeit für das Feature selbst (Tasten, Clamp, Nearest-Resize
  und HUD sind Handcode im Kilobyte-Bereich). Test-/Werkzeugpakete
  (`xvfb`, ggf. `unifont`, `ca-certificates` für Modell-Download) nur als
  Docker-/CI-Notiz, nicht als Laufzeit-Dep.
- Datei-Regeln aus dem Prompt (verbindlich): nummerierte Dateien
  `NN_name.rs`, aufsteigende Nummern = Init-/Datenfluss-Reihenfolge,
  Zusammengehöriges (Typ + seine Tests) nicht auseinanderreißen,
  `mod.rs`/`lib.rs`/`main.rs` nur Deklaration + oberste Verdrahtung.
- Tooling: `cargo fmt`, `cargo clippy --all-targets -- -D warnings`,
  `cargo upgrade` nur bei Deps-Einführung (hier: keine Einführung geplant →
  kein Upgrade-Risiko), neue Deps — falls doch nötig — sofort in `deps.md`
  in `<organization>/<projekt>`-Notation.
- Non-goals: kein Maus-Pan, kein stufenloses Zoom-Rad, keine
  persistente Konfiguration (ROI startet immer bei Default; siehe
  Querschnitts-Punkt 5), kein Wayland-Pfad, kein Tracking über Frames,
  keine Änderung an Det-/Rec-Schwellen oder am Deduplizierungsformat.

## Key Decisions

1. **Keine neue Abhängigkeit.** Tasten (`is_key_down`/`is_key_pressed`),
   ROI-Clamp (eigene ~30-Zeilen-Funktion nach `source0`-Vorbild) und
   Nearest-Resize (eigene Schleife) decken alles ab. Verworfene Alternativen:
   `winit`/`pixels`-Stack aus source0 (Hunderte transitive Crates, eigener
   Event-Loop — Overkill neben macroquad), `image`-Crate für Resize
   (neue Dep + Filter-Overhead für einen Ein-Zweck-Nearest-Pfad).
2. **ROI-Modell: quadratische ROI mit Stufen.** `View { x: i32, y: i32,
   size: u32 }`, Stufen `[320, 480, 640, 960, 1280]`, Start `640×640 @ (0,0)`
   (= heutiges Verhalten). `1` = eine Stufe kleiner (hinein), `2` = eine Stufe
   größer (heraus). Quadratisch, weil das Det-Modell fix 640×640 frisst und
   jede Rechteck-ROI ohnehin ein Seitenverhältnis-Problem (Stretch vs.
   Letterbox vs. Crop) einführen würde — Quadrat + Nearest hält den
   1:1-Fast-Path trivial korrekt. Verworfene Alternative: freie
   Rechteck-ROI — mehr Tasten, mehr Geometrie-Fehler, kein OCR-Mehrwert.
3. **Pan-Schritt: `max(8, size / 16)` px pro Frame bei gehaltener Taste**
   (`is_key_down`), Clamp auf `[0, screen_w - size] × [0, screen_h - size]`.
   Verworfene Alternative: `is_key_pressed` (ein Schritt pro Anschlag) —
   fühlt sich bei 640 px über 1920 px Bildschirm zäh an; gehalten+geclampt
   ist der übliche Viewer-Komfort. Exakter Schritt ist eine Konstante mit
   Unit-Test, kein Config-Flag (Konfigurationslosigkeit, s. Punkt 5).
4. **1:1-Fast-Path ohne Interpolation (hart garantiert).** Inferenz:
   `roi_size == 640` → heutige `prepare_inputs`-Schleife direkt auf den
   Capture-Bytes (reiner Copy+Normalisierungs-Loop, kein Resize-Code).
   Nur bei `roi_size != 640` läuft ein separater `resize_nearest`-Schritt
   (BGRA→640×640-planar). Anzeige: Textur in ROI-Größe, `tex.set_filter(
   FilterMode::Nearest)` einmalig; 1:1 → `draw_texture` (kein `dest_size`,
   keine Skalierungsmathematik), sonst `draw_texture_ex` mit
   `dest_size = Fenstergröße`. Der 1:1-Nachweis ist ein Unit-Test
   (Resize-Funktion wird nicht aufgerufen / Byte-Vergleich) plus ein
   Review-Gate (kein `FilterMode::Linear`, kein CPU-Filter im 1:1-Zweig).
5. **ROI-Wechsel invalidiert Caches.** Jede Änderung von `x`/`y`/`size`
   setzt `prev_screen_bytes`, `cached_boxes` und `prev_printed_lines` zurück:
   sonst vergleicht der `memcmp`-Detector Puffer verschiedener Größen und
   der Druck zeigt Texte der alten Region. Box-Koordinaten liegen immer im
   640-Raum der Detektion; Anzeige-Koordinaten = Box × (`display / 640`) —
   unabhängig von der ROI-Größe, da die Textur bereits aufs Fenster skaliert
   wird (bei 1:1 Identität; `display/size` wäre doppelt skaliert).
6. **Fenster: 640×640 fix, ROI-Bild per Nearest eingepasst.** `window_conf`
   bleibt 640×640 (kein Window-Recreate zur Laufzeit — macroquad-Fenster sind
   startup-fixiert); kleinere ROIs werden 1:1 mit schwarzem Rand zentriert
   oder per Nearest hochskaliert (Detailentscheidung: Nearest-Hochskalierung
   auf Fenstergröße, weil „hineinzoomen = Details größer" sonst unsichtbar
   bliebe; `draw_texture_ex`/`Nearest`, kein CPU-Interpolationsaufwand).
   Größere ROIs werden per Nearest herunter skaliert. 1:1 → `draw_texture`.
7. **Aufteilung von `main.rs` (463 Zeilen → 5 Module + Verdrahtung).**
   Datenfluss `view → capture → detect → recognize → overlay`; Tests bleiben
   bei ihren Typen:
   `01_view.rs` (ROI-State: Stufen, Pan/Zoom-Update, Clamp, Anzeigen-Mapping),
   `02_capture.rs` (x11rb-Regions-Capture + BGRA→Planar/RGBA + `resize_nearest`
   nur für `size != 640`), `03_detect.rs` (Session + DBNet-Postprocessing),
   `04_recognize.rs` (Crop + CTC + Dict), `05_overlay.rs` (Box-/Label-Render,
   HUD, Druck-Dedup-Format), `lib.rs`/`main.rs` nur Deklaration + Loop.
   Bestehende Det-/Rec-Konstanten und Schwellen bleiben unverändert.

## Recommended Approach

Kleinster Weg, der den Ansatz beweist: zuerst die reinsten, display-freien
Bausteine (`01_view.rs`: ROI-Arithmetik mit Tests; `resize_nearest` mit
1:1-Byte-Test), dann Capture-Umbau auf variable ROI (GetImage mit `x`/`y`,
Screen-Clamp, Cache-Invalidierung), dann Anzeige-Pfad (Nearest-Filter,
`draw_texture` bei 1:1, sonst `draw_texture_ex`), dann Tasten-Verdrahtung im
Loop plus HUD. Modell-/Font-Beschaffung (Befund: Dateien fehlen) wird als
Schritt 0 vorgezogen und blockiert alle Inferenz-Nachweise — ohne sie bleibt
nur der always-green Geometrie-Kern testbar. Durchführung als serielle Tasks
mit Gates (Details → `task.md` in der Ausführung): pro Schritt
`cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`,
`cargo test`, erst bei Grün committen (Conventional Commits mit
`Refs: plan/20260923_01_keys_source5/task.md <ID>`).

## Work Plan

- **S0 — Bestandsaufnahme + Modell-/Font-Beschaffung (Gate für alles).**
  Herkunft der drei `include_bytes!`/`include_str!`-Dateien + Unifont klären
  (Download-URLs, Lizenzen), Pfade/Prüfsummen dokumentieren, `cargo build`
  wieder grün. Ohne S0 kein Inferenz-Test möglich.
- **S1 — `01_view.rs` (ROI-State, rein, ohne X11).** `View`-Struct, Stufen,
  Pan/Zoom-Update, Screen-Clamp, ROI→Anzeige-Mapping; Unit-Tests (Clamp-Matrix,
  Stufen-Up/Down inkl. Grenzen, 1:1-Mapping = Identität).
- **S2 — `02_capture.rs` + Fast-Path-Trennung.** GetImage mit ROI-Offsets,
  Screen-Größe aus x11rb-Setup, `resize_nearest` nur für `size != 640`,
  1:1-Nachweis-Test (kein Resize-Aufruf, Byte-Vergleich gegen Referenz);
  Cache-Invalidierung bei ROI-Wechsel.
- **S3 — `03_detect.rs` / `04_recognize.rs` (Extraktion ohne Verhalten).**
  Code aus `main.rs` verschieben, Konstanten unverändert; bestehende
  Schwellen per synthetischem Tensor-Test absichern (wie bisher, kein Modell).
- **S4 — `05_overlay.rs` + Tasten + HUD.** Box-/Label-Render mit
  ROI-Skalierung, `set_filter(Nearest)`, `draw_texture` bei 1:1, Tasten
  (Pfeile/`1`/`2`/`Escape`), HUD (ROI, Stufe, Hilfe); Xvfb-Smoke mit
  synthetischen Tasten-Events.
- **T1 — Härtung + E2E.** Dauerlauf-Smoke (kein Drift bei Pan/Zoom-Spam),
  Fehlerpfade (X11 weg, Modell fehlt → Meldung + Exit ≠ 0), `deps.md` final,
  `cargo upgrade`-Check (nur wenn Dep eingeführt wurde).
- **T2 — Abschluss.** `fmt`/`clippy`/`test` final grün, `task.md`-Abhaken,
  `plan/20260923_01_keys_source5/walkthrough.md` (implementiert vs. Plan,
  1:1-Nachweis-Messung, Xvfb-Logs, Docker-Pakete, Learnings, Erweiterungen).

## Validation Plan

- Pro Schritt: `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`,
  `cargo test` grün → committen. Geometrie-/Resize-Tests laufen ohne
  X11/GPU/Netz/Modell (always-green-Basis).
- 1:1-Gate (höchstes Risiko, Kern des Auftrags): Unit-Test stellt sicher, dass
  bei 640×640-ROI kein Resize-Code läuft (z. B. Zähler-Hook oder
  Byte-Vergleich Fast-Path vs. generischer Pfad) + Review-Gate (kein `Linear`,
  kein Filter im 1:1-Zweig) + Xvfb-Messung (Frame-ms vorher/nachher,
  kein Regress).
- E2E unter Xvfb (Paket `xvfb` per apt, `DISPLAY` setzen): Testbild anzeigen,
  Viewer starten, synthetische Pfeil-/`1`-/`2`-Events senden, Screenshots der
  Anzeige sichern (ROI folgt, Zoom-Stufen wechseln, HUD lesbar), OCR-Text bei
  bekanntem Testbild plausibel.
- Manuell (mit Bildschirm): 60-Sekunden-Session — Pan über den ganzen
  Bildschirm ohne Freeze/Artefakte, Zoom rein/raus, `Escape`-Exit.

## Risks / Rollback

- **Modelle/Font fehlen (akut):** `main.rs` baut aktuell nicht; S0 klärt
  Quelle + Lizenz, sonst ist das Feature nicht testbar. Rollback: kein Code
  nötig — S1/S2-Geometrie ist modellfrei wertvoll.
- **macroquad-Fenster startup-fixiert:** Größenwechsel zur Laufzeit geht nur
  via `dest_size`-Skalierung (entschieden) — kein Risiko, aber dokumentierte
  Grenze (kein natives Resize).
- **Key-Repeat-Plattformunterschiede:** `is_key_down`-Pan ist
  frame-abhängig; Schritt als Konstante + FPS-unabhängige Dämpfung
  (`size/16`) hält es auf 30–60 FPS stabil. Fallback: Schritt halbieren.
- **`ort` RC-Drift (2.0.0-rc.13):** keine API-Änderung geplant (kein Upgrade
  ohne neue Dep); falls doch: nur Pin in `deps.md`, kein Refactor.
- **GetImage-Performance bei 1280-ROI:** X11-Roundtrip + Nearest-Resize sind
  linear; HUD zeigt Det-/Rec-ms, bei Bedarf Stufe 1280 streichen (eine Zeile).
- Rollback pro Schritt: jeder Task ist ein eigener Commit — `git revert`
  des jeweiligen `feat`-Commits stellt den vorherigen Stand her.

## Open Questions

1. Zoom-Richtung `1` = hinein / `2` = heraus — so angenommen (Prompt nennt nur
   „hinein und heraus"). Falls vertauscht gewünscht, ein Tastentausch in S4.
2. Pan-Schritt `max(8, size/16)` und Stufen `[320, 480, 640, 960, 1280]` sind
   Annahmen (reversibel, Konstanten mit Tests). Andere Stufen/Schritte auf
   Zuruf.
3. Querschnitts-Bullets aus dem Prompt (Firmware-Herkunft), übertragen wie im
   Vorgängerplan — bitte bestätigen: (a) Messgenauigkeit → OCR-Genauigkeit bei
   skalierten ROIs: Nearest als Default (schnell, kantentreu), kein
   Kalibrier-Flag; Eichung per Referenzbild im E2E. (b) 3,3-V-Limits → harte
   ROI-/Größen-Clamps (kein Überlauf, kein OOM durch Riesen-ROI).
   (c) TUI-Protokoll-Versionierung → entfällt (kein TUI); einziges
   versioniertes Format ist das stdout-Druckformat (unverändert).
   (d) Modus-Wechsel während Messung → ROI-Wechsel benutzt eine
   frame-konsistente ROI-Kopie (kein Tearing zwischen Capture und Inferenz).
   (e) Persistente Konfiguration → keine (immer Default-Start 640 @ 0,0).
4. Modell-/Font-Beschaffung (URLs, Lizenzen, Prüfsummen) — S0 klärt; ohne
   Antwort des Auftraggebers Default: bisherige Dateinamen + dokumentierter
   Download ins `source5/`-Verzeichnis (git-ignoriert, `*.lock`-Analogie
   beachten).
5. Fehlende Requirements, die ich ergänzt habe (bitte bestätigen oder
   streichen): Tasten-Hilfe im HUD, Cache-Invalidierung bei ROI-Wechsel,
   frame-konsistente ROI-Kopie, 1:1-Byte-Test als Gate, Docker-Paketliste
   (`xvfb`, `unifont`, ggf. `ca-certificates`) im Walkthrough.

## Task-Vorlauf (wird in der Ausführung zu `task.md`)

Seriell, jeder Schritt mit Gates (`fmt`, `clippy -D warnings`, `test` grün)
und eigenem Conventional Commit (`Refs: …/task.md <ID>`): S0 Beschaffung,
S1 `01_view`, S2 `02_capture`+Fast-Path, S3 `03_detect`/`04_recognize`,
S4 `05_overlay`+Tasten+HUD, T1 Härtung+E2E+Xvfb, T2 `deps.md`+Walkthrough.
Jeder Modus-Block (Capture/Inferenz/Overlay) bekommt Implementierung +
Host-Tests + Xvfb-Nachweis, danach erst HUD-/Tasten-Feinschliff.

## Commit-Konvention

Conventional Commits, ein logischer Schritt pro Commit:
`feat|fix|test|docs|chore|refactor(<scope>): <kurz, imperativ, ≤72 Zeichen>`.
Body: was + warum, betroffenes Modul, Validierung
(`cargo fmt --check`, `cargo clippy`, `cargo test`, Xvfb-Ergebnis).
Footer: `Refs: plan/20260923_01_keys_source5/task.md <ID>`.
`Cargo.lock` folgt der Repo-Regel (`*.lock` ignoriert — Vorgänger-Befund).
Nie ohne grüne Gates committen; keine fremden/untracked Dateien anfassen.
