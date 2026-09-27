# task.md — 20260927_01_optics: seriell abarbeitbare Schritte

Differenzierbarer Optik-Tracer in `examples/28_optics/source6/` (Plan:
`plan/20260927_01_optics/implementation_plan.md`). Jeder Schritt endet mit
Gates (`cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`,
`cargo test` grün). Erst bei grünen Gates weiter. Datei-Regeln aus dem
Prompt gelten ab der ersten Datei (nummeriert, ≤~300 Zeilen, eine
Zuständigkeit). Kein HIL (keine Firmware) — statt HIL-Nachweis je Modus ein
Headless-CLI-Smoke.

## R0 — Plan-Docs (Basis)

- `plan/20260927_01_optics/{implementation_plan,task,deps}.md` liegen vor
  und sind konsistent (Modulnummern `01_`–`08_`, Dep-Versionen, Geometrie-
  Konventionen, Commit-Liste).
- Kein Code, keine Gates nötig.
- Commit: `docs(plan): optics differentiable tracer plan and tasks`.

## R1 — Gerüst + Config

- Implementierung: `source6/Cargo.toml` (serde+derive, toml, serde_json,
  ratatui, crossterm; Edition 2024), `source6/assets/sample.toml`
  (Prompt-Beispiel + `[optimize]`), `src/lib.rs` (nur `mod`/Re-Export),
  `src/main.rs` (nur CLI-Dispatch `trace`/`optimize`/`export`/`tui`).
- Host-Tests: `cargo build` grün; `--help` Exit 0.
- Validierung: Gates grün.
- Commit: `feat(source6): scaffold optics crate with sample config`.

## R2 — Dual + Vec3

- Implementierung: `src/01_dual.rs` (`Dual{v,d}`, `new`/`constant`/
  `variable`, `Add/Sub/Mul/Div/Neg` für `Dual`+`&Dual` und `f64`-Mix,
  `sqrt/sin/cos/powf/exp`, AD-Fluss-Doku), `src/02_vec.rs`
  (`Vec3`/`Point3` über `Dual`, `dot`/`norm`/`normalize`, Skalar-Ops).
- Host-Tests: Dual-Regeln (Summe/Produkt/Quotient/`sqrt` bei `1e-9`),
  `sin/cos`-Ableitungen; Vec3-`dot`/Norm-Beispiele; Seed-Doku-Beispiel.
- Validierung: Gates grün; beide Dateien ≤~300 Zeilen.
- Commit: `feat(source6): dual numbers and vec3 with unit tests`.

## R3 — Schnitt + Brechung

- Implementierung: `src/03_ray.rs` (`Ray{origin,direction}`,
  Kugel-Schnitt `‖o+td−c‖²=R²` mit nächster positiver Wurzel,
  Normale am Treffer, Snellius-Vektorform als `Option` (TIR → `None`)).
- Host-Tests: senkrechter Treffer (Distanz exakt), Vorzeichen beider
  Radien, Strahl-daneben (`None`), Lotfall-Brechung = Einfall, TIR-Fall.
- Headless-Smoke: entfällt (reine Mathe, Unit-Ebene).
- Validierung: Gates grün; Datei ≤~300 Zeilen.
- Commit: `feat(source6): spherical intersection and snell refraction`.

## R4 — System + Trace

- Implementierung: `src/04_system.rs` (`OpticalSetup`/`Surface`-Serde mit
  optionalem `material`, `stop`, `cauchy_b`; Quelle mit
  `grid_radius`/`aperture_diameter`/`wavelengths`; Scheitel aus
  `thickness`, Bündel-Gitter, Cauchy-Index), `src/05_trace.rs`
  (`trace_system` pro Wellenlänge, `trace_ray`, `RayEnd`,
  `IntersectionResult`, Bildebenen-Schnitt, `efl`/`back_focal_z`).
- Host-Tests: Prompt-TOML + Doku-TOML-Varianten parsen (fehlendes
  Material → Luft, Apertur-Quelle); Scheitel-Layout `[0,5]`, Bild-`z=45`;
  Trace liefert Pfade je Welle; Gullstrand-EFL 100.33 ±0.05.
- Headless-Smoke: `cargo run -- trace --config assets/sample.toml` Exit 0.
- Validierung: Gates grün; beide Dateien ≤~300 Zeilen.
- Commit: `feat(source6): toml system and sequential trace loop`.

## R5 — Loss + Abstieg

- Implementierung: `src/06_optimize.rs` (`Var`/`VarKey` mit `radius`,
  `thickness`, `material`; Spot-Loss `Σ(x²+y²)` polychromatisch,
  Seed-Gradient je `optimize`-Variable, `descend` mit Lernrate ×
  Iterationen + Loss-Historie, TOML-Write-back).
- Host-Tests: Loss am Beispiel > 0 und endlich; Gradienten (Radius,
  Dicke, Material) vs. finite Differenzen; `descend` senkt Loss über
  20 Iterationen (einzeln + kombiniert).
- Headless-Smoke: `cargo run -- optimize --config assets/sample.toml
  --iters 5` druckt fallende Loss-Folge.
- Validierung: Gates grün; Datei ≤~300 Zeilen.
- Commit: `feat(source6): spot loss with seeded gradient descent`.

## R6 — JSON-Export

- Implementierung: `src/07_export.rs` (`SystemJson{version,loss,variables,
  surfaces[{name,profile:[[r,z],…]}],segments[[p0,p1],…]}`, `to_json`,
  Sag-Profil mit Apertur-Clamp).
- Host-Tests: Export-JSON parst, `version==1`, Profil liegt auf Kugel
  (Residuum `1e-9`), Segment-Enden stetig, Pfad aus `R4`-Trace.
- Headless-Smoke: `cargo run -- export --config assets/sample.toml
  --out /tmp/system.json` + `python3 -m json.tool` validiert.
- Validierung: Gates grün; Datei ≤~300 Zeilen.
- Commit: `feat(source6): three.js json export with versioned schema`.

## R7 — TUI + Pipeline

- Implementierung: `src/08_tui.rs` (`AppState`, `render` mit Inspektor/
  `Chart`/Status, `run_tui` mit Terminal-Restore), `main.rs`-Dispatch
  (`trace`/`efl`/`optimize`/`export`/`tui`), `tests/pipeline.rs`
  (TOML→Trace→Optimize→JSON Ende-zu-Ende).
- Host-Tests: `TestBackend(80,24)` rendert Titel + Variablen + Loss
  (Buffer-Substrings); Pipeline-Integration grün.
- Headless-Smoke: `cargo run -- --help` Exit 0 (echtes TUI braucht
  PTY — TestBackend-Nachweis genügt, ohne PTY saubere Fehlermeldung).
- Validierung: alle Gates grün.
- Commit: `feat(source6): ratatui telemetry skeleton with headless tests`.

## R8 — Benchmark-Designs (+ Distanz/Material/Welle)

- Implementierung: `assets/{landscape,cooke,double_gauss}.toml`
  (Transkriptionen aus `integration_tests.md` + `stop`-Flags),
  `tests/designs.rs` (EFL/BFL-Goldens, Y=5-Fall, Vignettierung,
  Dicken-/Material-Abstieg, Dispersions-Nachweis F vs. C).
- Host-Tests: gemessene Goldens (Landscape EFL 102.86/BFL 107.34 ab
  letzter Fläche, Cooke EFL 89.11, Double-Gauss EFL 111.40;
  Provenienz: Doku-Nominale treffen Prescriptions nicht, Tracer via
  Gullstrand + Handrechnung verifiziert).
- Headless-Smoke: `cargo run -- efl --config assets/cooke.toml` zeigt
  EFL/BFL/Defokus; `export` + `python3 -m json.tool` validiert.
- Validierung: alle Gates grün.
- Commit: `test(source6): benchmark designs with measured goldens`.

## R9 — Gates + Walkthrough

- Alle Gates final grün (`fmt --check`, `clippy -D warnings`,
  `cargo test`, alle CLI-Smokes, `cargo upgrade --dry-run` ohne
  übersprungene Majors zu prüfen).
- `plan/20260927_01_optics/walkthrough.md` (System, Entscheidungen,
  Testergebnisse, Learnings, neue Systempakete fürs Dockerfile — hier:
  keine, alles `cargo`/vorhanden).
- Commit: `docs(plan): optics walkthrough`.
