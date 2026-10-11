# Effort: B2 CELT-Bitstrom-Parser (`04_celt_parser`)

Session vom 11.10.2026 (Muse Code 1.4.4, `muse-spark-1.3-contributor`,
Session `01a12a52-…-36adb7ba1dc2`). Gezaehlt wird die reine
Modell-Arbeitszeit inkl. aller Tool-Latenzen.

## Kennzahlen

| Groesse | Wert |
|---|---|
| Arbeitszeit | 39m 41s |
| Turns | 516 |
| Subagents | keine |
| Input-Tokens | 103 912 928 (davon 101 580 318 cached) |
| Output-Tokens | 324 769 |
| Total | 104 237 697 |
| Kontext am Ende | 234K / 1008K (77 % frei) |

## Woran die Zeit ging (Schaetzung aus dem Verlauf)

| Phase | Anteil | Was |
|---|---|---|
| Referenz-Survey | ~35 % | `opus-pure`-0.2.2-Quellen lesen (Header-Sequenz, Allokation, Redundanz, Reset-Regeln), Testvektor-Framing verifizieren |
| Parser schreiben | ~20 % | `03c_celt.rs` in einem Guss (~1100 Zeilen, spaeter gedrittelt) |
| Orakel-Debugging | ~30 % | Summen-Identitaet herleiten (Overhead-Begriff), `nbits_total`-Bug finden, Fixture-Offset fixen |
| Split + Hygiene | ~15 % | Drei-Module-Split (< 600 Zeilen), Clippy/Fmt, `task.md`, Tests gruen |

## Ertrag pro Token (Einschaetzung)

- Teuerster Block war der Survey — unvermeidbar, da kein Byte der
  Allokation ohne Original-Code verifizierbar ist (keine Doku, nur Code).
- Billigster Nachweis war der Fuzz-Test (2000 synthetische Budgets,
  exakte Identitaet) — wenige Zeilen, hoechste Aussagekraft pro Zeile.
- Der `nbits_total`-Bug hat ~10 Minuten gekostet und wurde von zwei
  unabhaengigen Orakeln gefangen (Bilanz + Korrelation) — ohne sie waere
  er als „plausible Werte" durchgegangen.

## Artefakte dieser Session

- `source1/src/03c_range.rs` (250 Zeilen, neu)
- `source1/src/03d_alloc.rs` (546 Zeilen, neu)
- `source1/src/03c_celt.rs` (409 Zeilen, neu)
- `source1/tests/celt.rs` (315 Zeilen, neu)
- `source1/src/lib.rs`, `source1/src/03b_split.rs` (kleinere Edits)
- `plan/20261011_03_bitstream/task.md` (B0–B2 ✓, B3 geschaerft)
- `plan/20261011_04_celt_parser/` (dieser Ordner)
