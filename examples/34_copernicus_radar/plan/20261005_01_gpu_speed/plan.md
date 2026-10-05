# Plan: GPU-Beschleunigung der SAR-Fokussierung (`20261005_01_gpu_speed`)

## Ziel

Die GPU-Implementierung der Range-Doppler-Fokussierung ist aktuell **langsamer**
als die CPU-Implementierung (Vollrahmen S6 VV: GPU-Fokus ~92 s, CPU-Fokus ~66 s).
Aufgabe: Die GPU-Pipeline so umbauen, dass sie die CPU deutlich schlägt
(Zielbereich: Fokus in wenigen Sekunden), Ergebnisse validieren und Benchmarks
messen. Details zum Befund stehen in `review.md` neben diesem Plan.

## Kontext-Dateien (Lesereihenfolge für einen unabhängigen Agenten)

Der Agent soll sich in dieser Reihenfolge einarbeiten. Jede Datei ist mit dem
Pfad relativ zu `examples/34_copernicus_radar/` angegeben.

### 1. Aufgabenstellung und Befund

| Datei | Beschreibung |
|---|---|
| `plan/20261005_01_gpu_speed/prompt.txt` | Die Aufgabenstellung: GPU schneller als CPU machen, validieren, Benchmarks, Tests grün halten, `plan.md`/`walkthrough.md`/`plan_effort.md` liefern. |
| `plan/20261005_01_gpu_speed/review.md` | Ursachenanalyse der drei Flaschenhälse (Filter auf CPU + PCIe, `RdaGpuProcessor::new` je Chunk, Allokations-Schleife) und Lösungsarchitektur (On-the-fly-Filter, persistenter Kontext, Streaming, Overlap-Physik). |
| `plan/20261004_02_gpu_decode_sar/walkthrough.md` | Didaktische Gesamtdoku des Vorgängerprojekts: Module, Datenfluss, 13 Funde, Benchmark-Tabellen als Vergleichsbasis. |
| `plan/20261004_02_gpu_decode_sar/plan_effort.md` | Aufwand des Vorgängerprojekts (Phasen, Zeiten, Session-Usage) als Vorlage für `plan_effort.md`. |

### 2. GPU-Pfad (Änderungsschwerpunkt)

| Datei | Beschreibung |
|---|---|
| `sar_focus/src/10_gpu.rs` | `RdaGpuProcessor`: baut aktuell je Chunk 2D-Filter auf der CPU, lädt 2×1,32 GB Filter hoch, allokiert 4 Puffer + 4 cuFFT-Pläne. Kern der Optimierung. |
| `sar_focus/src/09_kernel.rs` | CUDA-Kernel via `cuda-oxide` (`cmul_2d`, `rotate_rows`, `shift_cols`, `tdbp`) und `GpuContext` (Context, Stream, Modul). Hier kommen Fusions-Kernel hinzu. |
| `sar_focus/src/08_cufft.rs` | Minimales cuFFT-FFI (`plan_rows`, `plan_strided`, `exec_inplace`, `set_stream`). Pläne gehören einmalig erstellt, nicht je Chunk. |
| `sar_focus/src/main.rs` (`focus_gpu_chunked`, `compare_cpu_gpu`) | Chunk-Schleife erzeugt aktuell je Chunk einen neuen `RdaGpuProcessor`. Muss auf einen persistenten Prozessor umgebaut werden. |

### 3. CPU-Referenz (Mathematik-Orakel, unverändert lassen)

| Datei | Beschreibung |
|---|---|
| `sar_focus/src/06_rda.rs` | CPU-RDA: `rcmc_filter`, `azimuth_filter`, `d_factor`, `az_freqs`, `range_freqs_unshifted`, `RdaProcessor::focus`. Jede GPU-Formel muss bitnah dasselbe rechnen. |
| `sar_focus/src/05_range.rs` | `RangeCompressor` (Matched-Filter, Zeilen-FFTs), `fftshift`/`ifftshift`, `dft_freqs`. |
| `sar_focus/src/04_chirp.rs` | Chirp-Replika, `num_tx_samples`, `embed_start` (für `correlation_shift_samples`). |
| `sar_focus/src/01_types.rs` | `Complex32` (GPU-kompatibel), `Vec3d`, Konstanten (`SPEED_OF_LIGHT`, `TX_WAVELENGTH_M`), `Error`. |

### 4. Datenfluss und Geometrie (Verständnis, selten ändern)

| Datei | Beschreibung |
|---|---|
| `sar_focus/src/12_ingest.rs` | `.dat` → Rohmatrix: Echo-Auswahl, Orbit-Interpolation, Ausrichtung, `smooth_fft_len`, Dekodierung. |
| `sar_focus/src/02_meta.rs` | Echo-Metadaten aus Paket-Headern (PRI, SWST, Chirp-Bits, `slant_range_vec`). |
| `sar_focus/src/03_ephem.rs` | Orbit-Ephemeriden: `effective_velocity`, `inertial_vel`, `fdc_range_grid`. |
| `sar_focus/src/07_tdbp.rs`, `sar_focus/src/13_tdbp_geo.rs` | TDBP-CPU-Referenz und Geometrie-Brücke (nur relevant, falls TDBP-GPU mit angefasst wird — sonst nicht ändern). |
| `sar_focus/src/11_look.rs` | Quicklook, Kennzahlen, Peak-Suche (für Validierung der Bildqualität). |

### 5. Build, Tests, Daten

| Datei | Beschreibung |
|---|---|
| `sar_focus/Cargo.toml`, `sar_focus/rust-toolchain.toml`, `sar_focus/build.rs` | Abhängigkeiten (`cuda-oxide` als Git-Rev — **gepinnt lassen**), Nightly-Toolchain, cuFFT-Link. |
| `sar_focus/tests/gpu_compare.rs` | Pflicht-Regression: GPU-gegen-CPU mit Schranke 1e-3 (cuFFT, RDA, TDBP). Muss nach dem Umbau grün bleiben. |
| `sar_focus/tests/common.rs`, `sar_focus/tests/rda_point.rs` | Punktziel-Simulation (S6-Skala) und Physik-Orakel (Peak-Lage, FWHM, RCMC-Wirkung). |
| `tests/real_data.rs` (Decoder-Crate) | Regressionstests auf Echtdaten (Paketzensus, Echo-/Rausch-Dekodierung), skip ohne Datensatz. Muster für neue GPU-Regressionstests. |
| `data/vv/*.dat` | Echter S1C-Stripmap-S6-Datensatz (VV, ~600 MB). **Niemals committen.** Quelle für Benchmarks und Validierung. |

## Arbeitsschritte

1. **Baseline messen:** `focus` auf einem 2048-Echo-Ausschnitt je mit `--cpu`
   und GPU (Release-Build), Fokus-Zeiten aus der `Zeit:`-Zeile notieren.
   Optional: Vollrahmen-Baseline, falls die Zeit reicht.
2. **Persistenter GPU-Kontext:** `RdaGpuProcessor` einmal je Geometrie erzeugen,
   in `focus_gpu_chunked` vor die Schleife ziehen und wiederverwenden (Pläne
   und Puffer leben über alle Chunks). Falls sinnvoll: je Geometrie nur 2 statt
   4 cuFFT-Pläne (Richtung ist Exec-Parameter).
3. **On-the-fly-Filter:** `filt_rr`/`filt_az` (2D, je 1,32 GB) ersatzlos
   streichen. Neue Fusions-Kernel berechnen pro Zelle `(a, r)` die Phase aus
   1D-Vektoren (`slant`, `veff`, `fdc`, Range-Filter, `fa`/`fr` aus Indexformeln)
   in Registern und multiplizieren in einem Schritt. Nur noch < 1 MB
   1D-Daten hochladen statt 2,64 GB Filter.
4. **Host-Nacharbeit auf die GPU:** Die Single-Thread-Skalierung `1/(naz·nr)`
   nach dem Download in einen GPU-Kernel verlegen; per `copy_to_host` direkt
   in den Zielpuffer laden (kein Zwischen-`Vec` + `copy_from_slice`).
5. **Validierung:** `gpu_compare`-Tests (Schranke 1e-3), Punktziel-Tests,
   `--compare`-Lauf auf Echtdaten, Bildvergleich (Peak/Kennzahlen) alt-gegen-neu.
   Optional Overlap 2048-gegen-1024 nur mit Qualitätsnachweis ändern — sonst
   Default lassen und als Option dokumentieren.
6. **Benchmarks:** Gleiche Läufe wie Schritt 1 nach dem Umbau + ein
   Vollrahmen-Lauf; Fokus-Zeiten, PCIe-Volumen und VRAM-Belegung berichten.
   Double-Buffering/Streaming nur angehen, wenn nach Schritt 2–4 noch ein
   relevanter Rest bleibt (Erwartung: PCIe < 1 s, nicht nötig).
7. **Gates:** `cargo test`, `cargo clippy --all-targets -- -D warnings`,
   `cargo fmt --check` in beiden Crates (`copernicus-radar`, `sar_focus`);
   GPU-Tests brauchen `cargo oxide test` bzw. GPU-Laufzeit. Neue Tests müssen
   ohne Datensatz per Skip grün bleiben.

## Nicht-Ziele

- Kein Range-Tiling (RDA braucht volle Zeilen-FFT; 1,32 GB/Chunk passen in VRAM).
- Kein Vollrahmen-am-Stück (44.901 ist cuFFT-feindlich: 27×1663; Geometrie
  braucht Block-`v_eff`). Chunk 8192 bleibt.
- Kein `cargo upgrade` der gepinnten `cuda-oxide`-Revision ohne Not (Break-Risiko).
- Keine TDBP-Umbauten, solange der RDA-Pfad der Engpass ist.

## Commit-Konvention (strikte Regeln)

Jede logische Änderung wird als **Conventional Commit** mit **umfassender
Beschreibung** committet. Format:

```text
<typ>(<scope>): <kurze zusammenfassung im imperativ, klein, ohne punkt>

<körper: was wurde geändert und warum? welche messung/entscheidung steckt
dahinter? welche alternative wurde verworfen und warum?>

<optionaler fuß: refs, breaking changes mit `!` im header>
```

- **Typen:** `feat` (neue Funktion/Kernel), `perf` (Beschleunigung ohne
  Verhaltensänderung), `fix` (Fehlerkorrektur), `test` (nur Tests),
  `docs` (nur Doku/Plan), `refactor` (Umstrukturierung ohne Verhaltensänderung),
  `chore` (Build/Tooling). Beispiel-Historie: `feat(sar-focus): …`,
  `fix(34_copernicus_radar)!: …`, `docs(sar-focus): …`.
- **Scope:** `sar-focus` für die Fokus-Crate, `copernicus-radar` für die
  Decoder-Crate, `gpu-speed` für Plan-/Benchmark-Artefakte dieser Aufgabe.
- **Sprache:** Deutsch, technisch präzise. Erste Zeile ≤ 72 Zeichen.
- **Körper (Pflicht):** Mindestens 3–5 Sätze: Ausgangslage (Messwert),
  Änderung, Wirkung (Messwert), Validierung (welcher Test/Lauf grün).
- **Ein Commit pro Arbeitsschritt** (Baseline-Doku, persistenter Kontext,
  On-the-fly-Filter, Host-Nacharbeit, Benchmarks/Doku) — keine Sammel-Commits.
- **Vor jedem Commit:** `cargo fmt`, betroffene Tests, `cargo clippy
  --all-targets -- -D warnings`. Keine `.dat`/`.zip`/`.cf`/Target-Artefakte
  committen (`git status` prüfen).
