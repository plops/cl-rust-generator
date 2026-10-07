# Aufgabenliste: GPU-Performance-Optimierung (35_sph, 2026-10-07)

Reihenfolge: erst risikoarme Host-Änderungen (P2/P4), dann Sortierung (P1),
dann Scan (P3). Nach jedem Schritt: Gates + Headless-PASS.

## Phase 0 – Planung

- [x] 0.1 Baseline messen (N-Sweep, 200 Schritte) + Gates prüfen
- [x] 0.2 cuda-oxide-API verifizieren (SharedArray, sync_threads,
      record_event/elapsed_ms, copy_to_host-Sync)
- [x] 0.3 `plan.md` + `task.md` in `plan/20261007_02_speed/` schreiben

## Phase 1 – P2 Sync-frei + P4 Metriken (Host only)

- [x] 1.1 `Backend`-Trait: `run_steps(steps) -> Option<f32>`-Default ergänzen
- [x] 1.2 `GpuBackend`: `synchronize()` aus `step()` entfernen, Sync nur in
      `sync_host()`/Messpunkten; `run_steps` mit CUDA-Events implementieren
- [x] 1.3 `09_headless.rs`: `run_steps` nutzen, MIUPS + Kernel-ms ausgeben
      (Human + `--bench`-CSV)
- [x] 1.4 Gates (fmt, beide clippy, test) + Headless-PASS (500 Schritte)

## Phase 2 – P1 Sortierte Puffer (Ping-Pong + Permutation)

- [x] 2.1 `src/05a_sort_kernels.rs`: `sort_device`-Modul mit `k_hash`,
      `k_scan`, `k_reorder`, neuem `k_permute` anlegen
- [x] 2.2 `src/05_gpu_kernels.rs`: `physics_device`-Modul mit sortierten
      `k_density`/`k_force` (ohne `order`) und angepasstem `k_integrate`
- [x] 2.3 `src/06a_gpu_backend.rs`: Ping-Pong-Buffer (`pos_alt`/`vel_alt`),
      beide Module laden, Launch-Sequenz mit Permute + Swap
- [x] 2.4 `src/06_backend.rs`: nur Trait + `CpuBackend`, GPU-Re-Export
- [x] 2.5 `src/lib.rs`: neue Module verdrahten (`#[path]`, `gpu`-gated)
- [x] 2.6 Gates + Headless-PASS + Zwischen-Benchmark

## Phase 3 – P3 Paralleler Scan

- [x] 3.1 `k_scan` als Chunk-Scan (256 Threads, SharedArray, `sync_threads`,
      `while`-Schleifen, Raw-Pointer-Zugriffe) implementieren
- [x] 3.2 Gates + Headless-PASS + Benchmark (Skalierung prüfen)

## Phase 4 – Validierung & Abschluss

- [x] 4.1 Voller N-Sweep (2.048 / 16.384 / 65.536 / 262.144), Vorher-Nachher
- [x] 4.2 GUI-Smoke (`DISPLAY=:0`, 120 Frames)
- [x] 4.3 `deps.md` bei Dep-Änderungen aktualisieren (sonst: keine Änderung)
- [x] 4.4 `walkthrough.md` (deutsch, Vergleichstabelle, Mermaid) schreiben
- [x] 4.5 `plan_effort.md` schreiben
- [x] 4.6 Conventional Commits pro Phase, finale Commit-Hygiene
