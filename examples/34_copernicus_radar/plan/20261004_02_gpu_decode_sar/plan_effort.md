# Plan-Effort: 20261004_02_gpu_decode_sar

Aufwand pro Phase (Wall-Clock aus Logs, Tests/Commits gezählt; Token nicht
instrumentiert — Spalte entfällt ehrlicherweise).

| Phase | Ergebnis | Tests | Commit |
|---|---|---|---|
| Kontext + DeepWiki-Recherche | Decoder/TDBP/SSFocus-Steckbriefe | — | (in plan.md) |
| plan.md + Regeln | Plan + Conventional-Commits-Regeln | — | diverse |
| Crate-Gerüst (01–05) | Typen/Meta/Ephem/Chirp/Range-CPU | 23 lib | `6cd40bf` |
| CPU RDA + TDBP (06–07) | gestufte RDA, TDBP f64 | +4 (Punktziele) | `d3079da` |
| GPU-Pfad (08–10) | cuFFT-FFI, Kernel, Pipeline, Vergleich | +3 (gpu_compare) | `ced1cab` |
| E2E + Quicklook (11, CLI) | f_DC, Chunking, PNG, Schiffe, XCheck | +8 (look/xcheck/ton) | `ad6ad4a` |
| Doku v1 | walkthrough.md, Artefakte | — | `949de78` |
| Revision 2: ingest + Timing | `12_ingest.rs` (kein 512-Limit), Phasen-Zeit, Peak-RSS | +2 (ingest) | (folgt) |
| Revision 2: Gold-Validierung | sentinel1decoder/NumPy-RDA per uv, RFI-Nachweis | — | (Analyse) |
| Revision 2: Benchmarks | CPU/GPU × 512/2048/8192/44901, Zeit + Host/Device-RAM | — | (Analyse) |
| Revision 2: Doku v2 | didaktischer Walkthrough, AVIF statt PNG, PNG aus Historie | — | (folgt) |
| Revision 3: Gold-Analyse | Streak-Diagnose (Up/Down, RFI), Trajectory-Explosion, Zweifel-Protokoll | — | (Analyse) |
| Revision 4: Chirp-Vorzeichen | Polarity 1 = positiv, TXPRR +8.26e11, Kurtosis 202 vs 20 | +2 (chirp_sign) | (folgt) |
| Revision 4: Clutterlock-Robustheit | Range-komprimiert + Phasen-Mittelung (one-vote-per-cell) | (robust grün) | (folgt) |
| Revision 5: TDBP-Geometrie | `13_tdbp_geo.rs`, Epochen-Derotation, Echozeit-Glättung, Kugel-Bogen | +4 (tdbp_geo) | (folgt) |
| Revision 5: TDBP-Benchmarks | 2048×3400 Grid, CPU↔GPU 8.4e-8, RDA-Vergleich, Az-Defokus offen | — | (Analyse) |
| Revision 5: Doku v3 | 1040-Zeilen-Walkthrough, Module 12+13, 13 Funde, Santos-Analyse | — | (folgt) |

Endstand: **48 Tests grün** (34 lib + 2 chirp_sign + 4 tdbp_geo +
3 gpu_compare + 2 rda_point + 2 tdbp_point + 1 erdrotation), Clippy
`-D warnings` grün, `cargo fmt --check` grün. Decoder-Crate: 41 Tests grün.

Teure Einzelläufe (Release, RTX A4000, 32 CPU-Kerne):

| Lauf | Zeit |
|---|---|
| E2E Vollrahmen GPU (44.901 × 20.160, 7 Chunks, mit `--compare`) | 127,5 s |
| Benchmark Vollrahmen CPU (Fokus / gesamt) | 66,5 s / 95,4 s |
| Benchmark Vollrahmen GPU (Fokus / gesamt) | 92,2 s / 121,2 s |
| TDBP 2048×3400 CPU (7.13e9 Puls·Pixel) | 34,3 s (415 M/s) |
| TDBP 2048×3400 GPU (dto.) | 5,5 s (1307 M/s, 3.2×) |
| E2E-Ausschnitt 2048 Echos | ~5 s |
| `cargo oxide test` (gesamt, inkl. GPU) | ~10 s |

Befund-Dichte: 13 dokumentierte Funde (cuFFT-Konstante, ECEF-Frame,
f_DC-Steuerung, RDA-Wahl, Primfaktor-Padding, RFI-Nachweis, Chirp-Vorzeichen,
Clutterlock-Lektion, Epochen-Derotation, Zeitglättung, Doppel-ntx, RCMC-Unschuld,
Az-Defokus-offen) — je mit Messwert im Walkthrough.



┌───────────────────────────────────────────────────────┐
│  Session usage                                        │
│                                                       │
│    Input      66,722,152                              │
│    Cached     65,485,596                              │
│    Output        341,868                              │
│    Total      67,064,020                              │
│                                                       │
│    Turns              350                             │
│    Subagents         none                             │
│                                                       │
│  Subscription · Muse Code Everyday Usage              │
│    Current        43% used · Resets at 1:06 AM        │
│    Weekly         0% used · Resets Oct 12 at 12:00 AM │
└───────────────────────────────────────────────────────┘

◆ Worked for 1h 27m 39s · 12:08 AM

┌───────────────────────────────────────────────────────┐
│  Session usage                                        │
│                                                       │
│    Input      66,722,152                              │
│    Cached     65,485,596                              │
│    Output        341,868                              │
│    Total      67,064,020                              │
│                                                       │
│    Turns              350                             │
│    Subagents         none                             │
│                                                       │
│  Subscription · Muse Code Everyday Usage              │
│    Current        44% used · Resets at 1:06 AM        │
│    Weekly         1% used · Resets Oct 12 at 12:00 AM │
└───────────────────────────────────────────────────────┘

┌───────────────────────────────────────────────────────────────────────────────┐
│  MUSE CODE 1.4.2 / brook-asterism                                   COMPLETED │
│                                                                               │
│  MODEL          muse-spark-1.3-contributor · max                              │
│                 meta · native-basic                                           │
│                                                                               │
│  WORKSPACE      /workspace/src/cl-rust-generator/examples/34_copernicus_radar │
│                 trusted · not found                                           │
│  ACCESS         Unrestricted                                                  │
│                 sandbox disabled (--yolo)                                     │
│                 Meta account                                                  │
│  ACCOUNT        Wol Pumba (wolpumba@gmail.com)                                │
│                                                                               │
│  USAGE          67,064,020 tokens · 350 turns · 0 subagents                   │
│  CONTEXT        70% left · 300K used / 1008K · normal                         │
│                                                                               │
│  SESSION        01a10912-b7e9-7473-818f-451872c1e9f7                          │
│  ACTIVITY       no tasks                                                      │
│                 0 terminals · inbox clear                                     │
│                                                                               │
│  BILLING        Subscription · Muse Code Everyday Usage                       │
└───────────────────────────────────────────────────────────────────────────────┘


try to fix doc and benchmark

◆ Worked for 34m 17s · 5:19 AM

┌───────────────────────────────────────────────────────┐
│  Session usage                                        │
│                                                       │
│    Input      101,408,100                             │
│    Cached      99,321,726                             │
│    Output         423,577                             │
│    Total      101,831,677                             │
│                                                       │
│    Turns               482                            │
│    Subagents          none                            │
│                                                       │
│  Subscription · Muse Code Everyday Usage              │
│    Current        12% used · Resets at 9:44 AM        │
│    Weekly         5% used · Resets Oct 12 at 12:00 AM │
└───────────────────────────────────────────────────────┘

┌───────────────────────────────────────────────────────────────────────────────┐
│  MUSE CODE 1.4.2 / brook-asterism                                   COMPLETED │
│                                                                               │
│  MODEL          muse-spark-1.3-contributor · max                              │
│                 meta · native-basic                                           │
│                                                                               │
│  WORKSPACE      /workspace/src/cl-rust-generator/examples/34_copernicus_radar │
│                 trusted · not found                                           │
│  ACCESS         Unrestricted                                                  │
│                 sandbox disabled (--yolo)                                     │
│                 Meta account                                                  │
│  ACCOUNT        Wol Pumba (wolpumba@gmail.com)                                │
│                                                                               │
│  USAGE          101,831,677 tokens · 482 turns · 0 subagents                  │
│  CONTEXT        90% left · 103K used / 1008K · normal                         │
│                                                                               │
│  SESSION        01a10912-b7e9-7473-818f-451872c1e9f7                          │
│  ACTIVITY       no tasks                                                      │
│                 0 terminals · inbox clear                                     │
│                                                                               │
│  BILLING        Subscription · Muse Code Everyday Usage                       │
└───────────────────────────────────────────────────────────────────────────────┘
