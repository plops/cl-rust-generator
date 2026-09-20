# Implementierungsplan — 20260920_01_capture

X11-Live-Capture → YOLOv8 (ort) → Overlay-Anzeige (pixels) — Linux-Detektor

Auftrag: Wol Pumba (wolpumba@gmail.com) · Linux (Ubuntu 26, Docker, X11) · Prompt: `plan/20260920_01_capture/prompt.txt` · Tasks: `plan/20260920_01_capture/task.md` · Deps: `plan/20260920_01_capture/deps.md` · Code: `examples/26_onnx/source0/` (neu anzulegen, analog `examples/25_dnb/source0/`)

Stack (docs.rs-Stand 2026-09-20, bei Einführung jeweils neueste nehmen): `ort 2.0.0-rc.13`, `xcap 0.9.8`, `pixels 0.17.2`, `winit 0.30` (via pixels-Beispiel), `clap 4.6.7` (derive). System: Xvfb für Tests (Prompt-Vorgabe; im Container aktuell **nicht** installiert — `which Xvfb` leer, DISPLAY unset).

## 1. Was ist gefragt (Checkliste = Erfolgskriterien)

1. Linux-Programm: Screenshot einer X11-Region → YOLOv8-Inferenz via `ort` → gecapturetes Bild + Boxen in neuem Fenster.
2. CLI: Capture-Position/Größe (`--x --y --w --h`), Anzeige-Größe/Zoom (`--zoom`, `--win-w/--win-h`), Framerate (`--fps`). Vorschlag: zusätzlich `--monitor`, `--model`, `--conf`, `--nms`, `--headless`, `--save-frame` (Details Kap. 3).
3. Bibliotheken: `pykeio/ort`, `parasyte/pixels`, `nashaofu/xcap` (Tbl. Kap. 5, Registry `deps.md`).
4. Referenz: `pykeio/ort` YOLOv8-Beispiel (Preprocess 640×640, `inputs!`, `output0`-Decode, Conf 0,5, NMS-IoU 0,7) — als Inferenz-Vorlage, **nicht** als Display-Pfad (Beispiel nutzt `show_image`/`raqote`, wir nutzen `pixels`).
5. Tests: Xvfb für Capture-/Smoke-Tests; neue Unit-/Integration-Tests, ausführen, grün.
6. Datei-Regeln: `NN_name.rs` in Datenfluss-Reihenfolge, ≤~300 Zeilen, `main.rs`/`lib.rs` nur Verdrahtung, Aufteilung ohne Verhaltensänderung (vorher/nachher grün).
7. Tooling: `cargo fmt`, `cargo clippy -D warnings`, `cargo upgrade` bei Deps-Einführung, neueste Versionen auch bei Kompatibilitätswarnung, neue Deps in `deps.md` (Org/Projekt-Notation).
8. Deliverables: dieser Plan, `task.md` (serielle Tasks mit Gates), nach Abschluss `plan/20260920_01_capture/walkthrough.md` + Docker-Paketliste.
9. Usage-Examples vorab: Kap. 6 (aus inspizierten Primärquellen, s. `## Sources`).

## 2. Befund zum Prompt (Lücken und Copy-Paste-Reste — verbindlich entschieden)

- Pfad `examples/25../source0` existiert nicht. Gemeint ist `examples/26_onnx/source0` (Konvention wie `examples/25_dnb/source0`). Dort arbeiten.
- Walkthrough-Pfad `plan/20260912_02_full/walkthrough.md` ist ein Rest aus einem anderen Vorhaben. Verbindlich: `plan/20260920_01_capture/walkthrough.md`.
- Datei-Regel nennt `firmware/src/`, `common/src/`, `text.rs`/`frame.rs`/`router.rs` — nichts davon existiert hier (Firmware-Template). Angewendet wird die Regel auf `source0/src/NN_*.rs` + `tests/`; `main.rs`/`lib.rs` nur Modul-Deklaration + Verdrahtung.
- „Jeder Modus bekommt eigene Tasks …, danach erst die TUI-Tasks": Es gibt hier keine Modi und kein TUI. Stufen = Capture/Inferenz/View (Tasks S1–S3); TUI-Scope entfällt ersatzlos.
- Die fünf Querschnitts-Bullets (Messgenauigkeit, 3,3-V-Limits, TUI-Protokoll, Modus-Wechsel, persistente Konfig) stammen aus einem Firmware-Prompt und sind wörtlich nicht anwendbar. Verbindliche Übertragung in Kap. 3 (Vorschlag, von Wol Pumba zu bestätigen — Defaults eingearbeitet).

## 3. Querschnitts-Punkte und fehlende Requirements (Vorschläge mit Defaults)

1. **Messgenauigkeit/Kalibrierung** → Box-Genauigkeit: Letterbox-Skalierung 640×640 mit sauberer Rücktransformation auf Region-Koordinaten (kein Stretch-Default, sonst verzerrte Boxen); `--conf` (Default 0,5) und `--nms` (Default 0,7) aus dem Referenzbeispiel als Flags; synthetischer Geometrie-Test ohne Modell.
2. **Schutzbeschaltung/Limits (§4, 3,3 V)** → Ressourcen-Limits: Region-Clamp auf Monitor-Geometrie (kein Panic bei Überlauf), `--fps`-Clamp (z. B. 1–30), Fehler im Capture/Inferenz-Loop → Meldung + Exit-Code ≠ 0 statt Panic.
3. **TUI-Protokoll-Versionierung** → Keine TUI, kein Protokoll. Einzig versionierte Größe: `--model`-Format (ONNX/YOLOv8-Output `output0`); bei inkompatiblem Output-Layer klare Fehlermeldung statt falscher Boxen.
4. **Fehler bei Modus-Wechsel während Messung** → Kein Modus-Wechsel. Analog: Monitor-/Regions-Wechsel zur Laufzeit ist nicht vorgesehen (nur per Neustart/Flag); Gerätefehler (X11 weg, Modell fehlt) beenden sauber mit Meldung.
5. **Persistente Konfiguration** → Vorschlag: **keine** Config-Datei, keine Persistenz; alles über CLI-Flags mit eingebauten Defaults (Firmware-Flash-Analogie entfällt).
6. **Modellquelle** (im Prompt fehlend): `--model <pfad-oder-url>`, Default YOLOv8m-URL aus dem Referenzbeispiel (`https://cdn.pyke.io/0/pyke:ort-rs/example-models@0.0.0/yolov8m.onnx`); Dateicache statt Download pro Start; 80 COCO-Labels aus dem Beispiel übernehmen.
7. **Farbraum** (im Prompt fehlend): xcap liefert BGRA/RGBA-artige Bytes (exakte Typen in S0 per `cargo doc` pinnen) → Konvertierung nach RGB-Normalisiert-f32 für `ort` und nach RGBA für `pixels`; als eigene getestete Funktion, kein Inline-Gewirr.
8. **Overlay ohne Extra-Dep**: Boxen/Labels werden mit einem kleinen eigenen CPU-Rasterizer ins `pixels`-Frame gezeichnet (kein `raqote`/`show_image` einführen — das Referenz-Display wird bewusst ersetzt).
9. **Headless-Betrieb** (für CI ohne GPU): `--headless` (kein Fenster, nur Inferenz + Log) und `--save-frame out.png` (ein Frame mit Boxen als Datei-Nachweis); Xvfb-Smoke nutzt genau diesen Pfad.
10. **Timing**: `--fps` als Pacer (Capture+Inferenz pro Tick, kein paralleles Überholen); pro Frame Capture-/Inferenz-ms auf stderr/log für Tuning-Nachweis.
11. **Scope-Grenze**: X11 primär; Wayland-Fallback nur dokumentieren (xcap kann beides, Prompt verlangt X11). Kein Tracking über Frames, keine Video-Aufnahme.

## 4. Architektur (Zielbild, `source0/`)

```
source0/
├── Cargo.toml                   bin x11_yolo_viewer; ort, xcap, pixels, winit, clap(derive), image, ndarray, anyhow
├── src/
│   ├── main.rs                  NUR Args + Verdrahtung (init, loop, join)
│   ├── lib.rs                   Modul-Deklarationen + Re-Exporte (für Tests)
│   ├── 01_cli.rs                Flags: x/y/w/h, monitor, zoom/win-w/win-h, fps, model, conf, nms, headless, save-frame
│   ├── 02_capture.rs            xcap: Monitor wählen, Region clampen + capturen, BGRA→RGB-Konvertierung
│   ├── 03_infer.rs              ort: Session, Preprocess 640 (letterbox), output0-Decode, Conf-Filter, NMS, Labels
│   ├── 04_draw.rs               Overlay: Boxen (Region-Koordinaten) + Labels ins RGBA-Frame rastern
│   └── 05_view.rs               pixels+winit: Fenster, Zoom-Skalierung, fps-Pacer, headless/save-frame-Pfad
└── tests/
    ├── geometry.rs              Letterbox-Rücktransformation, Region-Clamp, NMS (ohne Modell, ohne X11)
    └── cli_smoke.rs             --help, ungültige Region (Exit ≠ 0), --save-frame unter Xvfb (mit Modell)
```

- Datenfluss: `cli → capture → infer → draw → view`. Nummern folgen Init-/Datenfluss-Reihenfolge.
- Inferenz läuft synchron im Frame-Tick (kein zweiter Inferenz-Thread in dieser Phase — hält S4 einfach und testbar).
- `rand`/Video-/Tracking-Deps werden NICHT eingeführt.

## 5. Abhängigkeiten (Details und DeepWiki-Registry in `deps.md`)

| Crate/System | Org/Projekt | Zweck |
|---|---|---|
| ort | pykeio/ort | YOLOv8-Session, `inputs!`, `TensorRef` (s. Kap. 6) |
| xcap | nashaofu/xcap | X11-Regions-Screenshot (API-Signatur in S0 per `cargo doc` pinnen) |
| pixels | parasyte/pixels | GPU-Framebuffer fürs Anzeige-Fenster (s. Kap. 6) |
| winit | rust-windowing/winit | Fenster/Event-Loop (via pixels-Beispiel; Direkt-Dep fürs Fenster) |
| clap | clap-rs/clap | CLI mit derive |
| image, ndarray | image-rs/image, rust-ndarray/ndarray | Preprocess (aus Referenzbeispiel) |
| anyhow | dtolnay/anyhow | Fehler-Propagierung CLI/Pipeline |
| Xvfb (+ `ca-certificates` für Modell-Download) | system (apt) | Test-Display, Modell-Fetch im S4-Smoke |

`cargo upgrade`-Regel: bei Einführung neueste nehmen (auch bei Warnung); danach nur via Task T2.

## 6. Usage-Examples (aus inspizierten Primärquellen)

YOLOv8-Inferenzpfad (Referenzbeispiel, Vorlage für `03_infer.rs`):

```rust
use ndarray::{Array, Axis, s};
use ort::{inputs, session::Session, value::TensorRef};
// Preprocess: Bild auf 640x640, RGB/255 nach (1,3,640,640)
let mut input = Array::zeros((1, 3, 640, 640));
// ... Pixel füllen ...
let mut model = Session::builder(&env)?.commit_from_url(YOLOV8M_URL)?;
let outputs = model.run(inputs!["images" => TensorRef::from_array_view(&input)?])?;
let output = outputs["output0"].try_extract_array::<f32>()?.t().into_owned();
let output = output.slice(s![.., .., 0]);
for row in output.axis_iter(Axis(0)) {
    // row[0..4] = xc,yc,w,h; row[4..] = 80 Klassen-Scores; max + Conf-Filter 0.5; danach NMS (IoU 0.7)
}
```

Modell + Labels (Referenzbeispiel): `YOLOV8M_URL = "https://cdn.pyke.io/0/pyke:ort-rs/example-models@0.0.0/yolov8m.onnx"`, 80 COCO-Labels (`person` … `toothbrush`).

pixels+winit-Anzeige (Minimal-Beispiel, Vorlage für `05_view.rs`):

```rust
use pixels::{Pixels, SurfaceTexture};
use winit::dpi::LogicalSize;
let event_loop = EventLoop::new().unwrap();
let window = Arc::new(event_loop.create_window(
    Window::default_attributes().with_title("x11-yolo").with_inner_size(size)).unwrap());
let surface = SurfaceTexture::new(w, h, &window);
let mut pixels = Pixels::new(W, H, surface)?;
// pro Frame: in pixels.frame_mut() schreiben, pixels.render(); Resize via pixels.resize_surface(w, h)
```

CLI (clap 4, derive — Vorlage für `01_cli.rs`): `#[derive(Parser)] struct Args { #[arg(long, default_value_t = 0)] x: i32, … }` mit `--x/--y/--w/--h`, `--zoom`, `--fps`, `--model`, `--conf`, `--nms`, `--headless`, `--save-frame`.

xcap (teilverifiziert — Signatur in S0 pinnen): verifiziert ist nur: Screenshot-Library, Linux X11 + Wayland, `image`-kompatible Ausgabedaten. Ob Region-Capture `capture_region` o. ä. heißt und welchen Bildtyp sie liefert, klärt S0 per `cargo doc -p xcap` (docs.rs baut 0.9.8 aktuell nicht). Erst danach `02_capture.rs` schreiben.

Referenzen: `pykeio/ort` (YOLOv8-Beispiel + Session-Dok), `parasyte/pixels` (Minimal-winit-Beispiel + Framebuffer-Dok), `nashaofu/xcap` (Repo-/Crate-Beschreibung), `clap-rs/clap` (derive-Dok).

## 7. Risiken

- `ort` ist auf docs.rs latest ein Release Candidate (`2.0.0-rc.13`) — API-Drift gegenüber 1.x möglich; S0 pinnt die tatsächlich installierte Version und passt den Session-Aufbau (`ort::init()` vs. Env-Helfer aus dem Beispiel) an.
- xcap-API unverifiziert (s. Kap. 6) — S0 ist bewusst ein Spike mit Gate `cargo doc`, kein Raten.
- Kein GPU/X-Server im Container erwartet → `pixels`-Fenster braucht E2E einen Display-Kontext; Fallback `--headless`/`--save-frame` ist der CI-Nachweis, kein Workaround zweiter Klasse.
- Modell-Download (double-digit MB) braucht Netz + CA-Zertifikate; offline läuft nur `geometry.rs` (ohne Modell) grün.
- Box-Verzerrung bei Stretch-Skalierung → Letterbox ist Pflicht, kein Tuning-Detail.
- `cargo upgrade` nach Einführung kann ort/pixels-APIs brechen → nur in T2 mit vollem Testlauf.

## 8. Kontext für einen unabhängigen Agenten (Pflichtlektüre)

1. `plan/20260920_01_capture/prompt.txt` — dieser Auftrag (Feature, Libs, Datei-/Tool-Regeln).
2. `plan/20260920_01_capture/plan.md` (diese Datei) + `task.md` — was/wie zu tun ist.
3. `plan/20260920_01_capture/deps.md` — Dependency-Registry (Org/Projekt-Notation für DeepWiki-Abfragen).
4. `examples/26_onnx/source0/src/{main,lib,01_cli,02_capture,03_infer,04_draw,05_view}.rs` — Implementierung (wird in S0–S4 aufgebaut).
5. `examples/26_onnx/source0/tests/{geometry,cli_smoke}.rs` — Vertrags-Tests.
6. ort-YOLOv8-Referenz (`pykeio/ort`, `examples/yolov8/yolov8.rs`) — Inferenz-Vorlage (Preprocess, `output0`-Decode, NMS, Labels, Modell-URL).
7. pixels-Minimal-Beispiel (`parasyte/pixels`, `examples/minimal-winit`) — Fenster-/Framebuffer-Vorlage.
8. xcap-Crate-Dok (`nashaofu/xcap`, 0.9.8) + lokal `cargo doc -p xcap` — Capture-API (in S0 zu pinnen).
9. `examples/25_dnb/plan/20260920_01_audio/{plan,task,deps}.md` — Stil-Vorlage für Plan/Tasks/Deps (nicht für Inhalte).
10. Live-Umgebung: `echo $DISPLAY`, `which Xvfb`, `dmesg | tail` bei Geräte-Zweifeln.

## 9. Validierung (Gates; Details pro Task in `task.md`)

- Pro Task: `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` grün, erst dann committen.
- Modell-/Display-freie Gates zuerst (`geometry.rs` ohne X11/GPU/Netz) — das ist die always-green-Basis.
- E2E-Spätgate (S4): Xvfb + Testbild + Modell-Download → `--save-frame` schreibt annotiertes PNG; Boxen plausibel (Geometrie-Test deckt Exaktheit ab).
- Höchst-Risiko-Validierung: S0-xcap-Spike (`cargo doc`-Signatur + erster echter Regions-Screenshot unter Xvfb).

## 10. Commit-Konvention (für alle Tasks)

Format: Conventional Commits, ein logischer Schritt pro Commit: `feat|fix|test|docs|chore|refactor(<scope>): <kurz, imperativ, ≤72 Zeichen>`. Body: was + warum, betroffenes Modul, Validierung (`cargo fmt --check`, `cargo clippy`, `cargo test`, Xvfb-/Render-Ergebnis). Footer: `Refs: plan/20260920_01_capture/task.md <ID>`. Beispiele: `chore(scaffold): numbered modules cli capture infer draw view`, `feat(infer): yolov8 decode with nms`, `test(geometry): letterbox back-projection asserts`, `docs(plan): walkthrough for x11 yolo viewer`. Nie committen ohne grüne Gates des Tasks; keine fremden/untracked Dateien anfassen; `Cargo.lock` (Binary) wird mitcommittet.

## Offene Fragen (mit Defaults — bitte bestätigen oder ändern)

1. Modell-Default YOLOv8m per `--model`-Flag (CDN-URL) ok, oder festes lokales Modell ohne Download? Default: Flag + CDN + Dateicache.
2. Zoom = ganzzahliger Fenster-Skalierungsfaktor des Capture-Bildes (Default 1)? Default: ja (`--zoom`, zusätzlich `--win-w/--win-h` als Alternative).
3. Wayland explizit außer Scope (nur X11 + Xvfb-Nachweis)? Default: ja.

## Sources

- https://raw.githubusercontent.com/pykeio/ort/main/examples/yolov8/yolov8.rs
- https://docs.rs/ort
- https://docs.rs/xcap
- https://docs.rs/pixels
- https://raw.githubusercontent.com/parasyte/pixels/main/examples/minimal-winit/src/main.rs
- https://docs.rs/clap
