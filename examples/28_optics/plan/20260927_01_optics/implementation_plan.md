# Implementierungsplan — 20260927_01_optics

Differenzierbare 3D-Optik-Simulation in `examples/28_optics/source6/`:
TOML-Config (`[source]` + `[[surfaces]]`) → Ray-Bündel durch sphärische
Flächen (Dual-Zahlen-AD) → RMS-Spot-Loss → Gradientenabstieg über
`optimize`-Variablen → `system.json` für Three.js (`LatheGeometry`-Profile
+ `LineSegments`-Strahlen) und `ratatui`-TUI (Inspektor, Loss-Graph, Status).

Auftrag: Wol Pumba (wolpumba@gmail.com) · Linux (Ubuntu 26, Docker) ·
Prompt: `plan/20260927_01_optics/prompt.txt` · Zusatz: `integration_tests.md`
(Benchmark-Designs Landscape/Cooke/Double-Gauss, Distanzen/Materialien/
Wellenlängen als Optimierungsziele) · Stil: direkt nativer Rust-Code
(kein Lisp-Generator-Input), modular, ≤~300 Zeilen/Datei, minimale Deps,
Vorgängerpläne `examples/26_onnx/plan/2026092*/`.
Arbeitsordner: `examples/28_optics/source6/` (der Prompt nennt
`examples/25../source6`, das existiert nicht — `25_dnb/../source6` wäre
`examples/source6`; gemeint ist offenkundig `source6` im eigenen
Beispielordner, analog zu `26_onnx/source6`).

## Goal

Ein kompilierbares, getestetes Rust-Binary `optics` mit Library-Kern:
`Dual`-AD (Wert + Ableitung), Strahl-Kugel-Schnitt (plus Planflächen
`R = 0`) + Snellius-Vektorform, `trace_system`-Schleife über das
Flächen-Array, exakte Gradienten per Forward-Mode (eine Seed-Trace pro
Optimierungsvariable: Radius, Dicke, Material), Cauchy-Dispersion über
Wellenlängenlisten, EFL/Back-Focus-Analyse, JSON-Export mit
Schema-`version` und TUI-Skelett mit Headless-Tests über `TestBackend`.
Drei Benchmark-Assets (Landscape, Cooke, Double-Gauss) mit gemessenen
paraxialen Goldens als Regressionstests.

## Success Criteria

1. `cargo fmt --check` und `cargo clippy --all-targets -- -D warnings`
   fehlerfrei; keine Quelldatei über ~300 Zeilen.
2. `cargo test` grün: Unit-Tests für Dual-Arithmetik (Summen-/Produkt-/
   Quotientenregel, `sqrt`), Kugel-Schnitt (senkrecht/schief daneben),
   Snellius (Lotfall unverändert, TIR erkannt), TOML-Parsing des
   Prompt-Beispiels, `trace_system`-Durchlauf, Loss + Gradient gegen
   finite Differenzen, JSON-Roundtrip, TUI-`TestBackend`-Render.
3. CLI läuft headless: `trace`/`optimize`/`export` an der Beispiel-TOML,
   Exit 0, `system.json` enthält Flächen-Profile + Strahl-Segmente.
4. TUI rendert Inspektor/Loss/Status (Test via `TestBackend`, kein Display
   nötig); echtes Terminal nur in `run_tui` mit sauberem Restore.
5. `walkthrough.md` mit Entscheidungen, Messwerten, Learnings und
   Dockerfile-Paketen.

## Context And Current Facts

- Beispielordner ist leer bis auf `plan/` (verifiziert): kein Cargo-Gerüst,
  keine Vorgänger-`sourceN` — grüne Wiese.
- Prompt-Mathematik (gelesen): Dual `f+εf'`, Strahl aus `Point3<Dual>` +
  `Vector3<Dual>`, Kugelgleichung `‖o+td−c‖²=R²` mit `c=(0,0,z₀+R)`,
  Snellius-Vektorform mit `μ=n₁/n₂`, Loss `Σ(xᵢ²+yᵢ²)`.
- `toml::from_str` + `#[derive(Deserialize)]` mappt `[[surfaces]]` direkt
  auf `Vec<Surface>` (DeepWiki `toml-rs/toml`, verifiziert).
- `TestBackend::new(w,h)` + `Terminal::new` + `terminal.draw` +
  `assert_buffer_lines` testet TUI headless ohne Display (DeepWiki
  `ratatui/ratatui`, verifiziert).
- Toolchain (verifiziert): rustc/cargo 1.98.1, clippy, `Xvfb`/`xvfb-run`,
  `uv`, `cargo-upgrade` vorhanden.
- `ratatui 0.30.2`/`crossterm 0.29.0`/`serde 1.0.229`/
  `serde_json 1.0.151`/`toml 1.1.6` per `cargo add` latest (Stand 27.9.).

## Constraints And Non-goals

- Direkt Rust, keine Generator-Artefakte; nummerierte Module im Datenfluss
  (`01_dual` … `08_tui`, `lib.rs`/`main.rs` nur Deklaration/Verdrahtung).
- Kein `unsafe`; keine schwere GUI; Mathe-Kern strikt getrennt von TUI/JSON.
- Single-Seed-Forward-Mode wie im Prompt (`d: f64`, ein Seed pro Trace) —
  kein Multi-Dual/Jacobian in einem Durchlauf (Non-goal, siehe Erweiterung).
- Non-goals: asphärische/Freiform-Flächen, Sellmeier/Coatings (nur Cauchy),
  globaler Optimierer (nur Gradientenabstieg), Web-Viewer selbst (nur JSON),
  physikalische Bounds im Abstieg (kein Clamp auf n/d, dokumentiert).

## Key Decisions

1. **Hand-`Vec3` statt `nalgebra`.** Die Skizze nennt `nalgebra`, aber
   `Vector3<Dual>` verlangt `Scalar`-Trait-Boilerplate für `Dual` und zieht
   einen schweren Baum nach sich. ~60 Zeilen `Vec3<Dual>` (`dot`, `norm`,
   `normalize`, Skalar-Ops) erfüllen den verbindlichen
   Minimal-Dep-Wunsch; Präzedenz: `26_onnx`-Face-Plan lehnte `nalgebra`
   für ~100 Zeilen Handcode ab. Verworfene Alternative: `nalgebra` 1:1 zur
   Skizze — schwerer ohne Verhaltensgewinn.
2. **Ein Seed pro Optimierungsvariable.** `Dual.d` ist skalar (Prompt),
   also pro Gradientenkomponente eine Trace mit genau dieser Variable als
   `variable()` und Rest `constant()`; `dLoss/dp = Σ2(x·dx/dp+y·dy/dp)`
   aus den `d`-Feldern der Bildpunkte. Exakt, ohne finite Differenzen.
3. **Geometrie-Konventionen (Prompt-Lücken, festgelegt):** Flächen-Scheitel
   kumulativ aus `thickness` (`z₀[0]=0`, `z₀[k+1]=z₀[k]+thickness[k]`);
   letztes `thickness` = Abstand zur Bildebene; Strahlen kollimiert +z aus
   `z₀[0]−10`, Gitter `⌈√n⌉²` über `±grid_radius`, auf `n` gekürzt;
   Brechung `n₁` = Vorgänger-`material`, `n₂` = eigenes `material`;
   TIR → Strahl endet markiert statt NaN.
4. **Optionaler `diameter` pro Fläche** (Default `2·grid_radius`): ohne
   Apertur keine Linsenprofile; Sag `z(r)=z₀+R−sgn(R)·√(R²−r²)`,
   Apertur auf `0.9·|R|` geklemmt.
5. **JSON mit `version`-Feld + Segment-Paaren:** `segments` als
   `[[p0,p1],…]` ist direkt `THREE.LineSegments`-tauglich; Profile als
   `[r,z]`-Polylinien für `LatheGeometry`; `version: 1` versioniert das
   Protokoll (Prompt: TUI-Protokoll-Versionierung, hier aufs JSON
   übertragen).
6. **TUI als Skelett mit `TestBackend`-Tests:** `render(frame, state)`
   (Inspektor/Chart/Status) ist ohne Display testbar; `run_tui` mit
   Crossterm nur fürs echte Terminal. Kein `xvfb` nötig.
7. **Planflächen + optionales Material + Quell-Varianten** (aus
   `integration_tests.md`): `radius = 0` heißt Ebene; fehlendes `material`
   defaultet auf Luft (Patent-Tabellen listen nur Glas); Quelle versteht
   `grid_radius` oder `aperture_diameter/2` (Präzedenz in dieser
   Reihenfolge) plus optionale `wavelengths` (Default d-Linie).
8. **`material` ist Dual + Cauchy-dispersiv:** `n(l) = n_d + B/l²`
   (`cauchy_b`, Default 0); Trace läuft pro Wellenlänge, Loss summiert
   polychromatisch. Damit sind Distanzen (`thickness`), Materialien und
   Wellenlängen optimier-/testbar.
9. **EFL/BFL aus Marginalstrahl + gemessene Goldens:** `EFL = h/|u'|`,
   Back-Focus als Achsenschnitt; Tracer verifiziert an Gullstrand
   (100.33) und Handrechnung Landscape (BFL 107.34). Die
   Doku-Nominale (EFL 100) treffen die eigenen Prescriptions nicht —
   Tests pinnen gemessene Werte mit Provenienz statt falscher Nominale.
10. **Blenden-Vignettierung explizit:** `stop = true` blockiert Strahlen
    jenseits des Pupillenradius (eigenes `RayEnd`), statt sie still zu
    behalten; Assets annotieren die Doku-Blenden damit.

## Firmware-Ask-Übersetzung (Prompt § „berücksichtige")

- Messgenauigkeit/Kalibrierung → `f64`-Präzision, Toleranzen in Tests
  (`1e-9` Dual, `1e-6` Gradient-vs-Differenzen); keine HW-Kalibrierung.
- Schutzbeschaltung 3,3-V-Limits → Plausibilitäts-Guards: TIR-Abbruch,
  Apertur-Clamp, Strahl-Segment-Cap; keine analogen Pins vorhanden (N/A).
- TUI-Protokoll-Versionierung → JSON-`version` + dokumentiertes Schema.
- Modus-Wechsel bei laufender Messung → Subkommandos sind zustandslos
  (reiner Kern, kein Shared State); TUI-Quit stellt Terminal wieder her.
- Persistente Konfiguration → optimierte Radien können als TOML
  zurückgeschrieben werden (`export --write-back`); Firmware-EEPROM N/A.

## Fehlende Requirements (ergänzt)

- Strahl-Bündel-Definition (Gitter, Start-z, Richtung) — siehe E3.
- Bildebenen-Lage (letztes `thickness`) und `n₁/n₂`-Abbildung — siehe E3.
- `diameter` für Profile — siehe E4. TIR-Verhalten — siehe E3.
- Lernrate/Iterationen (`[optimize]`) für den Abstieg — CLI-Flags.

## Relevante Dateien (mit Begründung)

- `source6/Cargo.toml` — Deps: `serde`/`toml`/`serde_json`,
  `ratatui`/`crossterm`; Edition 2024.
- `source6/assets/sample.toml` — Prompt-Beispiel-TOML + `[optimize]`;
  Eingabe für alle CLI-Smokes und Integrationstests.
- `source6/src/01_dual.rs` — `Dual{v,d}`, alle Ops (`Add/Sub/Mul/Div/Neg`
  für `Dual`+`&Dual`, `f64`-Mix), `sqrt/sin/cos/powf/exp`; AD-Doku.
- `source6/src/02_vec.rs` — `Vec3`/`Point3` über `Dual`, `dot/norm/
  normalize`; Brücke Dual→Geometrie.
- `source6/src/03_ray.rs` — `Ray`, Kugel-Schnitt (quadratisch, nächste
  positive Wurzel), Snellius-Vektorform + TIR-`Option`.
- `source6/src/04_system.rs` — `OpticalSetup`/`Surface`-Serde (optionales
  `material`, `stop`, `cauchy_b`, `diameter`; Quelle mit
  `grid_radius`/`aperture_diameter`/`wavelengths`), Scheitel-Layout,
  Bündel-Generator, Cauchy-Index.
- `source6/src/05_trace.rs` — `trace_system` (Bündel sequenziell durch
  Flächen + Bildebene, pro Wellenlänge), `trace_ray`, `RayEnd`,
  `IntersectionResult`/Segmentpfade, `efl`/`back_focal_z`.
- `source6/src/06_optimize.rs` — `Var`/`VarKey` (`radius`, `thickness`,
  `material`), Spot-Loss, Seed-Gradienten, `descend` (Lernrate ×
  Iterationen), TOML-Write-back.
- `source6/src/07_export.rs` — `SystemJson` (`version`, Profile, Segmente,
  Loss/Variablen), `to_json`.
- `source6/src/08_tui.rs` — `AppState`, `render` (Inspektor/Chart/Status),
  `run_tui`; `TestBackend`-Tests.
- `source6/src/lib.rs` — nur `mod` + Re-Exporte. `source6/src/main.rs` —
  nur CLI-Parse + Dispatch (`trace`/`efl`/`optimize`/`export`/`tui`).
- `source6/assets/{sample,landscape,cooke,double_gauss}.toml` — Prompt-
  Beispiel + drei Benchmark-Transkriptionen aus `integration_tests.md`.
- `source6/tests/pipeline.rs` — Integration: TOML→Trace→Optimize→JSON.
- `source6/tests/designs.rs` — Benchmark-Goldens (EFL/BFL/Strahlschicksale)
  + Distanz-/Material-Abstieg + Dispersions-Nachweis.
- `plan/20260927_01_optics/{implementation_plan,task,deps,walkthrough}.md`
  — Deliverables aus dem Prompt.

## Usage-Beispiele (DeepWiki, verifiziert)

- TOML: `let cfg: OpticalSetup = toml::from_str(&s)?;` mit
  `surfaces: Vec<Surface>` für `[[surfaces]]` (Quelle: `toml-rs/toml`).
- TUI-Test: `let mut t = Terminal::new(TestBackend::new(80,24))?;`
  `t.draw(|f| render(f, &state))?;`
  `t.backend().assert_buffer_lines([...]);` (Quelle: `ratatui/ratatui`).

## Commit-Strategie (Conventional Commits)

1. `docs(plan): optics differentiable tracer plan and tasks` — diese drei
   Docs, kein Code.
2. `feat(source6): scaffold optics crate with sample config` — Cargo.toml,
   `assets/sample.toml`, `lib.rs`/`main.rs`-Gerüst.
3. `feat(source6): dual numbers and vec3 with unit tests` — `01_`, `02_`.
4. `feat(source6): spherical intersection and snell refraction` — `03_`.
5. `feat(source6): toml system and sequential trace loop` — `04_`, `05_`
   (Planflächen, optionales Material, Quell-Varianten, `efl`/BFL).
6. `feat(source6): spot loss with seeded gradient descent` — `06_`
   (`radius`/`thickness`/`material`, Cauchy pro Wellenlänge).
7. `feat(source6): three.js json export with versioned schema` — `07_`.
8. `feat(source6): ratatui telemetry skeleton with headless tests` —
   `08_`, TUI-Dispatch, `tests/pipeline.rs`.
9. `test(source6): benchmark designs with measured goldens` — Assets +
   `tests/designs.rs` (Landscape/Cooke/Double-Gauss, Distanz/Material/
   Wellenlängen), Fixups als `fix(source6): …` bei Bedarf.
10. `docs(plan): optics walkthrough` — Walkthrough nach grünen Gates.
