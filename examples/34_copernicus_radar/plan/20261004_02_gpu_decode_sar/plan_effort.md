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
