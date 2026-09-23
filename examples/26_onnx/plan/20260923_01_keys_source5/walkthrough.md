# Walkthrough — 20260923_01_keys_source5

ROI-Pan (Pfeiltasten) + Zoom (`1`/`2`) für den source5-Live-OCR-Viewer.
Code: `examples/26_onnx/source5/`, Plan: `plan.md`, Tasks: `task.md`,
Deps: `deps.md`. Stand: 2026-09-23, alle Gates grün, alles committet
(7 Commits, s. unten).

## Was implementiert wurde (vs. Plan)

- `01_view.rs` (neu): `View { x, y, size }`, Stufen `[320, 480, 640, 960, 1280]`,
  Start `640×640 @ (0,0)` (= altes Verhalten), Pan-Schritt `max(8, size/16)`,
  Screen-Clamp, `is_native()`, `display_scale()` — 8 Tests.
- `02_capture.rs` (neu): `screen_size`/`capture_roi` (x11rb, ROI-Offsets),
  `bgra_to_rgba`, `prepare_native` (fusionierter 1:1-Single-Pass aus `main.rs`),
  `resize_nearest_planar` (nur `size != 640`), `convert_path`,
  `view_changed` — 1:1-Gate per Test (`prepare_native == Nearest` bit-identisch).
- `03_detect.rs` / `04_recognize.rs` (extrahiert, bit-identisch): `Detector`
  (DBNet), `Recognizer` (Crop mit `stride`-Skalierung — bei 640 exakt wie vorher,
  `x * 1.0 == x` —, Dict, CTC), `TextBox` — 7 Tests (Synthetik-Block, Dict, CTC).
- `05_overlay.rs` (neu): `load_font_bytes` (Suchliste), `scaled_rect`,
  `draw_boxes`, `draw_hud` (ROI/Stufe + Tasten-Hilfe) — 3 Tests.
- `main.rs` (nur Verdrahtung, 207 Zeilen): Tasten-Loop, Cache-Invalidierung bei
  ROI-Wechsel, `draw_texture` bei 1:1 / `draw_texture_ex`+Nearest sonst,
  Textur-Recreate bei Größenwechsel mit `set_filter(Nearest)`.
- `scripts/smoke_xvfb.sh` + `scripts/README.md` (per User-Wunsch im Repo):
  Xvfb-Rauchtest mit Tasten-Simulation, Screenshots und OCR-Log.
- Alle Dateien ≤216 Zeilen, keine Staging-`allow(dead_code)` übrig
  (alle Module voll verdrahtet, Clippy wäre sonst rot).

## Stellen, die Tests/Realität erzwungen haben

1. **Modelle/Font fehlten**: `main.rs` baute nicht (`include_bytes!`-Ziele
   fehlten, Font-Pfad `/usr/share/fonts/unifont/…` existiert nicht — APT
   installiert nach `opentype/`). Fix S0: offizielle HF-Modelle
   (`PaddlePaddle/PP-OCRv6_small_det_onnx`, `…_rec_onnx`, Apache-2.0) +
   `inference.yml` (hat `character_dict:`, passt zu `load_dict`) geladen;
   Font per Suchliste mit klarer Fehlermeldung.
2. **Clippy 1.98**: `chunks_exact(4)` → `as_chunks::<4>()` (Muster aus source0).
3. **`goto_step`-Bug**: Stufen-Reduktion für kleine Screens muss VOR dem
   Gleichheits-Check passieren, sonst meldet `zoom_out` Erfolg ohne Wechsel
   (eigener Test hat es gefangen).
4. **Xvfb ohne Window-Manager**: `windowactivate` scheitert, Tasten müssen per
   `xdotool … --window` direkt ans Fenster; `scrot` überschreibt nie
   (weicht auf `_NNN` aus → `rm -f` im Skript); `Escape` wird gehalten
   (Einzel-Taps können bei langsamen Debug-Frames in Frame-Lücken fallen).
5. **Feedback-Schleife im Test-Setup**: Fenster bei (0,0) liegt in der eigenen
   ROI und OCRt sich selbst (Dauer-Inferenz, HUD-Überlagerung). Artefakt des
   WM-losen Setups, kein Produktfehler — Erstframe mit xterm-Text war sauber
   (`HEI0 0CR WORLD 123`, `SECOND LINE ABC XVZ`).

## Abweichungen vom Plan (bewusst)

- Font-Fix (Suchliste) bereits in S0 statt S4 — sonst kein grüner Build als Basis.
- `cargo upgrade` entfallen: keine Dep eingeführt (nur Handcode), `Cargo.toml`
  unverändert; Modelle sind HF-Artefakte, keine Cargo-Deps.
- `Cargo.lock` folgt der Repo-Regel (`*.lock` ignoriert).
- T1 ohne Code-Änderung: Fehlerpfade (kein X11 → `XOpenDisplay() failed!` +
  Exit ≠ 0; Font fehlt → Hinweis-Panic) genügen; Dauerlauf (~15 min über alle
  Smokes, Det ~47–50 ms stabil) ohne Drift.

## Nachweis-Logs (Xvfb :99, 1280x1024, Debug-Build)

- `fmt --check`, `clippy --all-targets -D warnings`, `cargo test`
  (23 pass) final grün.
- `scripts/smoke_xvfb.sh`: Exit 0 (`Escape`), 6 OCR-Treffer auf Testtext,
  Screenshots: vorher ROI 640 (Start), nachher `ROI 480x480@200,120`
  (5× Rechts + 3× Runter à 40 px, `1` → 640→480) — Pan + Zoom-in per echter
  Tastatur-Events bewiesen; Zoom-out + `Escape` per gehaltenen Tasten.
- Ohne `DISPLAY`: Panic-Meldung + Exit ≠ 0 (T1-Gate).

## Learnings

- macroquad-Eingaben: `is_key_pressed` ist eingerastet (Release löscht den
  Latch nicht — in `lib.rs` verifiziert), Pfeiltasten gehören auf
  `is_key_down` (Halten), Stufen-Tasten auf `is_key_pressed`.
- `Texture2D::set_filter(FilterMode::Nearest)` + `draw_texture` (ohne
  `dest_size`) = garantiert interpolationsfrei bei 1:1; der Pfad ist per
  `convert_path` + Byte-Test abgesichert, nicht nur per Review.
- `xproto::get_image` nimmt beliebige `i16`-Offsets — Pan brauchte exakt
  null neue Deps; Screen-Maße stehen im x11rb-Setup.
- IEEE-Trick für verhaltensneutrale Parametrisierung: `x * 1.0 == x` exakt,
  daher stride-skalierter Crop bei 640 bit-identisch (kein Zweig nötig).

## Mögliche Erweiterungen

- ROI per Maus ziehen, Zoom per Rad (stufenlos), persistente ROI
  (Start-Flags `--x/--y/--zoom`), Fenstergröße folgt ROI (statt fix 640),
  `PAUSED`-Zustand bei statischem Bild auch die Capture-Rate drosseln,
  Release-Benchmark für 1:1-vs-skaliert (erwartet: kein Regress bei 1:1).

## Docker-Pakete (fürs Image vormerken)

Laufzeit/Build: `fonts-unifont` (Pflicht — sonst Panic),
`ca-certificates` (ORT-Bibliotheks-Download beim Bauen).
Tests: `xvfb`, `xdotool`, `xterm`, `x11-apps` (xwd), `scrot`, `netpbm`
(nur für Nachweis-Crops; kein Produktbedarf).

## Commits

`chore(source5)` S0-Beschaffung · `feat(source5)` S1-View · `feat(source5)`
S2-Capture · `refactor(source5)` S3-Split · `feat(source5)` S4-Tasten
(inkl. scripts/) · `docs(plan)` T2-Walkthrough (dieser Commit).
Diese Sektion: Folge-Commit.

# Kosten

┌───────────────────────────────────────────────────────────────────┐
│  MUSE CODE 1.3.0                                        COMPLETED │
│                                                                   │
│  MODEL          muse-spark-1.3-contributor · high                 │
│                 meta · native-basic                               │
│                                                                   │
│  WORKSPACE      /workspace/src/cl-rust-generator/examples/26_onnx │
│                 trusted · not found                               │
│  ACCESS         Unrestricted                                      │
│                 sandbox disabled (--yolo)                         │
│                 Meta account                                      │
│  ACCOUNT        Wol Pumba (wolpumba@gmail.com)                    │
│                                                                   │
│  USAGE          17,541,196 tokens · 122 turns · 0 subagents       │
│  CONTEXT        77% left · 228K used / 1008K · normal             │
│                                                                   │
│  SESSION        01a0cee1-f6ca-7741-95f0-4455b8701ee8              │
│  ACTIVITY       no tasks                                          │
│                 0 terminals · inbox clear                         │
│                                                                   │
│  BILLING        Subscription · Muse Code Everyday Usage           │
└───────────────────────────────────────────────────────────────────┘

┌───────────────────────────────────────────────────────┐
│  Session usage                                        │
│                                                       │
│    Input      17,456,457                              │
│    Cached     17,029,338                              │
│    Output         84,739                              │
│    Total      17,541,196                              │
│                                                       │
│    Turns              122                             │
│    Subagents         none                             │
│                                                       │
│  Subscription · Muse Code Everyday Usage              │
│    Current        6% used · Resets at 8:29 PM         │
│    Weekly         4% used · Resets Sep 28 at 12:00 AM │
└───────────────────────────────────────────────────────┘
