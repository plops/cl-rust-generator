# Effort: Verstehen statt nur Ansehen (`05_verstehen`)

Session vom 13.10.2026 (Muse Code 1.4.7, `muse-spark-1.3`,
Session `01a13436-…-c09d90`). Gezaehlt wird die reine
Modell-Arbeitszeit inkl. aller Tool-Latenzen. Enthalten: Review aller
vier Walkthroughs plus Code, Planung (`prompt.txt`, `plan.md`,
`task.md`), Umsetzung V0–V6 und dieser Effort-Nachtrag.

## Kennzahlen

| Groesse | Wert |
|---|---|
| Arbeitszeit | 14m 37s |
| Turns | 160 |
| Subagents | keine |
| Input-Tokens | 26 881 243 (davon 26 529 977 cached) |
| Output-Tokens | 81 662 |
| Total | 26 962 905 |
| Kontext am Ende | 75 % frei (248K / 1008K benutzt) |

## Woran die Zeit ging (Schaetzung aus dem Verlauf)

| Phase | Anteil | Was |
|---|---|---|
| Review | ~25 % | Vier Walkthroughs + alle Module, Tests, Skripte, `opus-pure`-API (Head/Tags/Trim) lesen; Befunde sichern |
| Planung | ~20 % | `prompt.txt`/`plan.md`/`task.md` schreiben, Kompatibilitaets-Nachtrag einarbeiten, Plan zweimal handoff-faehig liefern |
| V1 Ein-Durchlauf | ~20 % | `PacketSet`, drei `_packets`-Stufen, `Sources`-Move, Granule-Korrektur nach `trim.rs`-Blick |
| V2–V4 Fakten/JSON/Report | ~25 % | `03e_fakten.rs`, JSON-Schema-2, `08b_report.rs`, Gold-Snapshot sichten und abnehmen |
| V5–V6 Hygiene | ~10 % | README/`deps.md`/Smoke, Walkthrough, Gates, Effort |

## Ertrag pro Token (Einschaetzung)

- Teuerster Block war das Review — unvermeidbar, da jede
  Verbesserungs-Behauptung am gelesenen Code haengen muss.
- Billigster Nachweis war der Demo-Lauf nach V4 (SILK 0–3 s, CELT
  3–6 s, Transiente bei 3,0 s): eine Ausgabe, die fast alle Fakten
  auf einmal belegt.
- Der `Trim`-Blick hat einen stillen End-Trim-Bruch verhindert —
  wenige Minuten Lesen gegen einen Fehler, den kein Test rot
  gemeldet haette (nur falsche Samples am Datei-Ende).

## Artefakte dieser Session

- `source1/src/03e_fakten.rs` (neu, Fakten-Modul)
- `source1/src/08b_report.rs` (neu, Report + Regeln)
- `source1/src/03_analyze.rs` (`PacketSet`, ein Durchlauf, `split_fallbacks`)
- `source1/src/04_decode.rs` (`ChannelStats`, `decode_packets`)
- `source1/src/05b_bitstream.rs` (`PacketCelt`, einmaliges TOC-Parsing, `Sources`-Move)
- `source1/src/05_spectrum.rs` (`centroid_db` + Test)
- `source1/src/08_cli.rs` (JSON-Schema-2, Report-Standard, `--quiet`)
- `source1/src/01_types.rs`, `06_render.rs`, `09_show.rs`, `lib.rs` (Felder/Signaturen)
- `source1/tests/report.rs` + `tests/data/report_gold.txt` (neu)
- `source1/tests/cli.rs`, `tests/render.rs`, `scripts/smoke.sh`, `README.md`, `deps.md`
- `plan/20261013_05_verstehen/` (`prompt.txt`, `plan.md`, `task.md`, `walkthrough.md`, dieser Effort)
