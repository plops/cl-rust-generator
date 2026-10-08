# Aufgabenliste: Besseres Wasser (01_better_water)

## Phase 0 – Planung & Baseline

- [x] 0.1 Baseline-Gates (fmt, beide clippy, 26 Tests) + GPU-Umgebung (libclang-Workaround)
- [x] 0.2 Baseline-Sweep N∈{2048, 16384, 65536, 262144} (500 Schritte, PASS + MIUPS)
- [x] 0.3 DeepWiki-Referenzen (Akinci-Kohäsion, macroquad-Postprocessing)
- [x] 0.4 `plan.md` + `task.md` schreiben

## Phase 1 – Zustandsgleichung

- [x] 1.1 `pressure()` mit Tension-Clamp + NaN-Wache in `03_sph_math.rs` (Unit-Tests updaten)
- [x] 1.2 Kohäsions-Regressionstest (2 Partikel, schrumpfender Abstand) – erst Rot auf altem Code zeigen
- [x] 1.3 Defaults: dt 0,0004, substeps 6, wall_damping 0,2 (+ Tests/Hilfe updaten)
- [x] 1.4 Gates (fmt, beide clippy, test) + Tension-Sensitivität 0,05/0,1/0,2 per Headless

## Phase 2 – Visualisierung

- [x] 2.1 `07a_water_style.rs`: Wasserpalette, Dichte-Rampe, Gischt, Konstanten (+ Tests)
- [x] 2.2 `07_renderer.rs`: Kreise, Trails (T), Gischt, Beckenrahmen, Hindernis-Glanz
- [x] 2.3 `08_app.rs`: `particle_r`-Verdrahtung, T-Taste; `lib.rs`: Modul 07a
- [x] 2.4 Gates + Zeilenlimits prüfen (07/07a < 300)

## Phase 3 – Validierung & Bericht

- [x] 3.1 Headless-PASS 500 Schritte + Nachher-Sweep alle N (Tabelle, MIUPS, ρ-Statistik)
- [x] 3.2 GUI-Smoke `--frames 120` auf DISPLAY=:0 (Timing, Trails-Toggle)
- [x] 3.3 `walkthrough.md` (DE, Tabelle, Mermaid, Folgearbeit) + `plan_effort.md` + `deps.md`-Check
- [x] 3.4 Finale Gates; Commits bleiben per Policy ausstehend (im Bericht vermerkt)
