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
| E2E + Quicklook (11, CLI) | f_DC, Chunking, PNG, Schiffe, XCheck | +8 (look/xcheck/ton) | `86846a2` |
| Doku | walkthrough.md, Artefakte | — | (folgt) |

Endstand: **38 Tests grün** (28 lib + 3 gpu_compare + 2 rda_point +
2 tdbp_point + 3 ssfocus_xcheck), Clippy `-D warnings` grün,
`cargo fmt --check` grün.

Teure Einzelläufe (Release, RTX A4000):

| Lauf | Zeit |
|---|---|
| E2E Vollrahmen (44.901 × 20.160, 7 GPU-Chunks) | 127,5 s |
| E2E-Ausschnitt 2048 Echos | ~5 s |
| `ships`-Tiefensuche Vollrahmen | ~30 s |
| `cargo oxide test` (gesamt, inkl. GPU) | ~10 s |

Befund-Dichte: 6 dokumentierte Funde (cuFFT-Konstante, ECEF-Frame,
f_DC-Steuerung, RDA-Wahl, Primfaktor-Padding, RFI-Nachweis) — je mit
Messwert im Walkthrough.



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
