# Plan: GPU-Performance-Optimierung SPH-Solver (P1–P4)

Datum: 2026-10-07. GPU: NVIDIA RTX A4000 (Ampere GA104, sm_86, 16 GB).
Stand: Baseline gemessen (200 Schritte, `cargo oxide run --features gpu`,
Release-Profil), alle CPU-Gates grün.

## 1. Baseline (Vorher, 2026-10-07)

| N | ms/Schritt | Partikel/s | MIUPS¹ |
|---|---|---|---|
| 2.048 | 0,170 | 12,06 Mio | 12,1 |
| 16.384 | 0,542 | 30,23 Mio | 30,2 |
| 65.536 | 2,780 | 23,57 Mio | 23,6 |
| 262.144 | 36,26 | 7,23 Mio | 7,2 |

¹ MIUPS = N·Schritte / (Physik-s·10⁶); bisher nicht ausgegeben, hier
nachträglich aus Partikel/s abgeleitet. Alter Walkthrough-Wert bei
N=16.384 (500 Schritte): 0,74 ms / 22,0 MIUPS — Abweichung durch
Schrittzahl/Warmup und Release-vs-Debug; die Nachher-Messung nutzt
identische Schrittzahlen pro N.

CPU-Gates: `cargo test` (25 Tests) grün, `cargo fmt --check` grün,
`cargo clippy --all-targets -- -D warnings` grün.

## 2. Flaschenhals-Analyse (aus Review + Code-Lektüre)

- **P1 — Indirektion statt sortierter Buffer:** `k_density`/`k_force`
  lesen Nachbarn über `pos[order[k]]` (doppelter Gather). Threads eines
  Warps bearbeiten räumlich verstreute Partikel → Warp-Divergenz in den
  3×3-Zellschleifen, unkoaleszierte Loads, L1-Thrashing. Dominanter
  Kostenfaktor bei N ≥ 16k.
- **P2 — Sync pro Sub-Step:** `GpuBackend::step()` endet mit
  `stream.synchronize()`. Bei 3 Sub-Steps/Frame und 500 Headless-Schritten
  zahlt die CPU hundertfach Treiber-Latenz; der Stream könnte alles
  FIFO-geordnet ohne Host-Eingriff abarbeiten. `copy_to_host` synchronisiert
  ohnehin (Quelle: `cuda-core/src/simt/device_buffer.rs`), daher ist
  `sync_host()` der natürliche einzige Sync-Punkt.
- **P3 — Serieller Scan:** `k_scan` parkt 255 von 256 Threads; ein Thread
  summiert seriell über alle Zellen (aktuell 1.000). Bei feineren Grids
  oder 3D (bis 262k Zellen) wird das zum Blocker.
- **P4 — Messmethodik:** `09_headless.rs` misst nur CPU-Wallclock
  (`Instant`), kein MIUPS, keine CUDA-Events. Kernel-Zeit auf den SMs ist
  nicht von Host/Treiber-Latenz isoliert.

## 3. Soll-Architektur

### 3.1 Ping-Pong-sortierte Partikelpuffer (P1)

Statt `order`-Indirektion in den heißen Schleifen werden `pos`/`vel` pro
Schritt physikalisch in Zellordnung permutiert (Verfahren nach NVIDIA
Particles-Referenz, `reorderDataAndFindCellStart`-Schema):

- Neue Device-Buffer `pos_alt`, `vel_alt` (je N×[f32;2]); `pos`/`vel`
  heißen logisch `pos_cur`/`vel_cur`.
- Neuer Kernel `k_permute`: `pos_nxt[s] = pos_cur[order[s]]` (ein Gather
  pro Schritt, koaleszierende Writes).
- `k_density`/`k_force` arbeiten direkt auf sortierten Buffern:
  Thread `i` = sortiertes Partikel `i`, Nachbarn via
  `pos_sorted[k]`, `k ∈ [cell_start[c], cell_start[c+1])` — keine
  `order`-Indirektion mehr, Warps bleiben zellokal, Loads koaleszieren.
- `k_integrate` aktualisiert die sortierten Buffer in-place (koaleszierend,
  weiter `DisjointSlice`, keine Scatter-Notwendigkeit).
- Danach `swap(cur, nxt)` auf dem Host (reiner Handle-Tausch, kein Copy).
  `dens`/`pres`/`acc` leben immer in der jeweils neuen Sortierordnung und
  müssen nicht doppelt gepuffert werden.
- Host-Download liest `pos_cur` nach dem Swap. Validierung ist
  ordnungsunabhängig (NaN/Tunneln/Dichte-Bänder); Jet-Recycling wirkt auf
  sortierte Slots (Headless nutzt neutral, daher kein Effekt).

### 3.2 Sync-freies Stepping (P2)

- `synchronize()` aus `step()` entfernen. Kernel, `zero_async` und Copies
  laufen FIFO-geordnet auf demselben Stream.
- Sync nur noch in `sync_host()` (implizit via `copy_to_host`) und an
  expliziten Messpunkten (CUDA-Events).
- Neuer Trait-Default `run_steps(steps) -> Option<f32>`: CPU loopt wie
  bisher und gibt `None` zurück; GPU loopt ohne Sync und gibt reine
  Kernel-ms via Events zurück.

### 3.3 Kooperativer Block-Scan (P3)

Ein-Block-Chunk-Scan mit 256 Threads + Shared Memory (`SharedArray<u32,
256>`, `thread::sync_threads()`, `thread::threadIdx_x()` — alle in
cuda-oxide verifiziert vorhanden):

1. Jeder Thread summiert sein Chunk (`ceil(ncell/256)` Zellen, atomare
   Loads wie bisher) → Shared Memory.
2. Thread 0 bildet exklusiven Präfix über die 256 Chunk-Summen (256 Adds,
   vernachlässigbar) + Total; `sync_threads()` davor/dahinter (Barrieren
   außerhalb von Conditionals).
3. Jeder Thread schreibt exklusiven Scan seines Chunks nach `cell_start`
   und initialisiert den `counts`-Scatter-Cursor; Thread 0 schreibt
   `cell_start[ncell] = total`.
4. Skaliert auf beliebiges `ncell` (auch 262k in 3D: 256× parallel statt
   seriell), braucht nur 1 KiB Shared Memory, kein Pow2-Padding, keine
   Bank-Konflikte (ein Wort pro Thread).

Zugriff auf Shared Memory ausschließlich über Raw-Pointer
(`SharedArray::as_raw_mut_ptr(&raw mut …)` + `add/read/write`), damit der
`static_mut_refs`-Lint (Rust 2024, `-D warnings`) nicht anschlägt.
Device-Schleifen bleiben `while` (wie Bestand, unroll-freundlich).

### 3.4 Metriken (P4)

- `09_headless.rs`: MIUPS in Human- und `--bench`-CSV-Ausgabe.
- GPU-Kernelzeit via `CudaStream::record_event(Some(CU_EVENT_DEFAULT))` +
  `CudaEvent::elapsed_ms` (API aus `cuda-core/src/simt/{stream,event}.rs`,
  Timing-Flag via `cuda_core::sys::CUevent_flags_enum_CU_EVENT_DEFAULT`
  wie im `pinned_overlap`-Beispiel). Ausgabe `kernel_ms/Schritt` neben
  Wallclock; Headless misst weiter zusätzlich Wallclock zum Vergleich.

## 4. Datei-Layout (≤ ~300 Zeilen/Datei)

| Datei | Modul | Inhalt |
|---|---|---|
| `src/05a_sort_kernels.rs` (neu) | `sort_kernels` | `#[cuda_module] sort_device`: `k_hash`, `k_scan` (parallel), `k_reorder`, `k_permute` |
| `src/05_gpu_kernels.rs` | `gpu_kernels` | `#[cuda_module] physics_device`: `k_density`, `k_force`, `k_integrate` (sortiert, ohne `order`) |
| `src/06_backend.rs` | `backend` | `Backend`-Trait (+ `run_steps`-Default), `CpuBackend` |
| `src/06a_gpu_backend.rs` (neu) | `gpu_backend` | `GpuBackend` (Ping-Pong, sync-frei, Event-Timing); Re-Export via `backend::GpuBackend` |
| `src/09_headless.rs` | `headless` | `run_steps` nutzen, MIUPS + Kernel-ms ausgeben |
| `src/lib.rs` | — | Neue `#[path]`-Module, `gpu`-gated wie bisher |

Zwei `#[cuda_module]` in einem Crate: je eines pro Sortier-/Physik-Datei,
Backend lädt beide (`sort_device::load`, `physics_device::load`).
Kernel-Namen bleiben (`k_hash` … `k_integrate`) plus `k_permute`.

## 5. Risiken & Gegenmaßnahmen

| Risiko | Maßnahme |
|---|---|
| Zwei PTX-Module in einem Crate laden nicht | Früh testen (`k_permute`-Build nach Split); Fallback: ein Modul mit `include!` |
| `SharedArray`/`sync_threads`-Codegen defekt | Minimaler Scan zuerst allein testen; Fallback: serieller Scan hinter `cfg` |
| `static_mut_refs`-Lint unter `-D warnings` | Nur `&raw mut` + Raw-Pointer, kein `TILE[i]`-Indexing |
| Sortierung bricht Parität/Stabilität | Headless-Validierung (500 Schritte) + Dichte-Bänder nach jedem Schritt; Jet nur GUI-relevant |
| Event-Timing-Overhead verfälscht kurze Läufe | Events nur um die Schritt-Schleife, nicht pro Schritt |
| Datei-Limits | Split wie oben; `02_params.rs`-Bestand (341) unangetastet lassen |

DeepWiki-MCP wurde nicht eigens befragt: die cuda-oxide-Quellen
(`SharedArray`, `sync_threads`, `record_event`/`elapsed_ms`) liegen im
Container vor und sind aktueller als jede Wiki-Zusammenfassung; das
Particles-Permutationsschema ist Standard und oben festgehalten.

## 6. Validierungsplan

1. `cargo fmt --check`
2. `cargo clippy --all-targets -- -D warnings`
3. `cargo clippy --all-targets --features gpu -- -D warnings`
4. `cargo test` (25 Tests)
5. `cargo oxide run --features gpu -- --headless --steps 500` → PASS
6. Sweep N ∈ {2.048, 16.384, 65.536, 262.144} (Vorher-Werte s. §1)
7. GUI-Smoke `DISPLAY=:0 … --frames 120`
