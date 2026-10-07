# Aufgabenliste: h konfigurierbar + 60-FPS-Maximum (03_hscale)

## Phase 0 – Planung

- [x] 0.1 Plan (`plan.md` + `task.md`) schreiben

## Phase 1 – Implementierung

- [x] 1.1 `--h`-Flag in `02_params.rs` (Parse, Validierung, Hilfe, Unit-Test)
- [x] 1.2 In-App-Timing in `08_app.rs` (Ø ms/Frame, Physik-Anteil, FPS)
- [x] 1.3 Gates (fmt, beide clippy, test) + Headless-PASS mit `--h 0.04`

## Phase 2 – Blockgröße

- [x] 2.1 128/256/512 bei N=65k benchen, Winner fixieren + begründen

## Phase 3 – h×N-Sweep (Stabilität + Durchsatz)

- [x] 3.1 Sweep N×h (300 Schritte, PASS-Gate), ggf. dt-Achse bei kleinem h
- [x] 3.2 Finalisten mit 500 Schritten bestätigen

## Phase 4 – 60-FPS-Entscheid

- [x] 4.1 GUI-FPS der Kandidaten messen (In-App + Wanduhr)
- [x] 4.2 Max-N bestimmen, ggf. `--auto-h` einbauen (nur wenn robust)
- [x] 4.3 `walkthrough.md` (Sweep-Tabellen, Empfehlung) + `plan_effort.md`
- [x] 4.4 Conventional Commits, finale Gates
