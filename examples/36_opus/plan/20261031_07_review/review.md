# Review: `source1` (Stand nach `06_silk`, Commit `ab7e07b`)

Geprueft am 31.10.2026: alle sechs Plan-Ordner (`01_init` … `06_silk`),
saemtliche Module in `source1/src`, Tests, Skripte und die
Referenz-Quellen `opus-pure 0.2.2` (`decoder.rs`, `celt/bands.rs`,
`celt/mod.rs`, `silk/dec_api.rs`, `silk/init_decoder.rs`) sowie libopus
per DeepWiki (`xiph/opus`, `opus_decode_frame`). Ist-Stand vor dem
Review: 113 Tests gruen (`cargo test --all-features`, 8 s), 5441+ Zeilen,
2 Crates.

Kurzfassung: Die Architektur (ein Ogg-Durchlauf, nummerierte Module,
Orakel-Kultur, ehrliches `NaN`) ist gut und bleibt. Der Kern hat aber
eine **systematische Luecke**: Beide Walks hoeren vor dem Ende des
Bitstroms auf (CELT vor den Banddaten, SILK ohne LBRR/Stereo/
Multi-Frame-Flags). Damit fehlt das staerkste verfuegbare Orakel — der
Range-Coder-Endzustand `final_range` —, und mehrere Pfade sind
nachweislich falsch oder grau.

## A. Bitstrom-Kern (hohe Prioritaet)

| # | Befund | Beleg | Wirkung |
|---|---|---|---|
| A1 | CELT-Walk endet nach Fine-Energie; PVQ-Banddaten, Anti-Collapse und `unquant_energy_finalise` werden nie gelesen (`03c_celt.rs`, Kopf-Kommentar „PVQ band data itself is skipped") | Code | Kein Endzustand → kein exaktes Orakel; Energien ohne Finalise; Dichte-Ansicht nur geschaetzt |
| A2 | `OpusDecoder::final_range()` existiert in der schon vorhandenen Dep, wird aber nirgends benutzt | `grep final_range source1` = 0 Treffer; `opus-pure/src/decoder.rs:646` | Alle bisherigen Orakel sind indirekt (Bilanz, Korrelation, Pitch); Desyncs nach dem Header bleiben unsichtbar |
| A3 | Dichte = Allokation + „Ideal-Drain" (`drain_shares`), nicht die gelesenen Bits | `03d_alloc.rs` | „Informationsdichte" ist ein Modell, keine Messung |
| A4 | SILK-Flags (VAD/LBRR) nur aus dem **ersten** Payload gelesen (`walk_packet`, `rcs[0]`), die uebrigen Payloads eines Code-1/2/3-Pakets laufen ohne Flags | `03f_silk.rs` `walk_packet`; Referenz `dec_api.rs:81ff` liest Flags je Opus-Frame | 120-ms-SILK (Code 3) liefert Werte, aber aus verschobenen Bits |
| A5 | Hybrid: Folge-Frames eines Pakets `CODE_CONDITIONALLY` (`g > 0`); Referenz startet **jeden** Opus-Frame unabhaengig (`new_packet = true`) | `05c_hybrid.rs` | Hybrid 40/60 ms: falsche Gain-/Pitch-Kodierung ab Frame 2 |
| A6 | LBRR (In-Band-FEC) → `unsupported` (grau) | `03f_silk.rs` `packet_flags` | FEC-Stream `silk_fec`: nur 167 von 401 Paketen mit SILK-Daten |
| A7 | Stereo-SILK/-Hybrid → `unsupported` | `walk_packet` (`channels != 1`) | `silk_stereo_wb`, `hybrid_stereo`: 0 von 401 Paketen mit Daten |
| A8 | Hybrid-Redundanz: Coder-`storage` wird nicht um die Redundanz-Bytes gekuerzt (Kommentar behauptet das Gegenteil der Referenz) | `05c_hybrid.rs` Doku zu `parse_celt_frame_rc`; Referenz `decoder.rs:1550` und libopus (DeepWiki) kuerzen | Roh-Bits (Fine-Energie, Vorzeichen) aus falschen Bytes bei jedem Moduswechsel |
| A9 | SILK-Redundanz-CELT-Frame (implizit, ≥ 17 Rest-Bits) und Hybrid→SILK-Ausblend-Frame werden nicht gewalkt | `bitstream.rs` SILK-Arm | CELT-Zustand nach Wechseln falsch; Redundanz unsichtbar |
| A10 | Reset-Regeln: `source1` setzt SILK **und** CELT bei jedem Moduswechsel zurueck; Referenz: SILK nur nach CELT-only, CELT nicht nach SILK→CELT-Redundanz | `05b_bitstream.rs` Schleife vs. `decoder.rs:521, 1356, 1581` | Zustandsfehler nach jedem Wechsel (Energie-Praediktion, NLSF-Interpolation) |
| A11 | Bit-Zaehlung in ganzen Bits (`tell()`), nicht 1/8 (`tell_frac`) | `03f_silk.rs` `SilkBits` | Bilanzen nur auf ±1 Bit je Posten |

## B. Modell und Darstellung (mittlere Prioritaet)

| # | Befund | Beleg |
|---|---|---|
| B1 | `Bitstream` traegt 13 parallele `Vec`s (+ Tupel-Typ `PacketBands`); `Sources::build` muss jeden einzeln kuerzen — jedes neue Feld ist eine Fehlerquelle | `05b_bitstream.rs` |
| B2 | `CliConfig` liegt in `01_types.rs`, haengt aber von `bitstream::Source` ab (Schicht 01 → 05) | `01_types.rs:237` |
| B3 | Paket→Hop-Mapping, Modus-Dispatch und Spaltenbau in einer 500-Zeilen-Funktion; Hybrid separat in `05c` mit dupliziertem CELT-Akkumulator | `05b`/`05c` |
| B4 | JSON-Writer mit Hand-Komma-Logik in jedem Abschnitt (fehleranfaellig, schwer erweiterbar) | `08c_json.rs` |
| B5 | Keine Sicht auf das *Bit-Budget* eines Pakets (wohin gehen die Bits: SILK-Seiteninfo, Pulse, CELT-Energie, PVQ, Redundanz, Rest) — dabei ist das die Kernfrage „was hat der Encoder entschieden" | Fachlich |

## C. Tests und Werkzeuge

| # | Befund | Beleg |
|---|---|---|
| C1 | Real-Daten-Matrix fehlt: kein Stereo-SILK, kein FEC, keine 10/40/60/120 ms, kein NB/MB, kein DTX, keine Moduswechsel mit Redundanz | `tests/data` = 3 Dateien |
| C2 | HIL (`xvfb`, `interact.sh`) lief in `05`/`06` nicht (fehlendes X11) | Walkthroughs 05/06 §1.2 |
| C3 | Grosse Real-Dateien (`silk.opus`, `silk_silk.opus`, YouTube) nur manuell geprueft | Walkthrough 06 |

## D. Was gut ist und bleibt

- Ein Ogg-Durchlauf (`PacketSet`), `Trim`-treue Decodierung, Pre-Skip-
  korrektes Hop-Mapping.
- Exakte Allokations-Identitaet (Fuzz) und Tabellen-Ports.
- Ehrliche Darstellung: `NaN`/Grau statt vorgetaeuschter Werte.
- Report/Fakten als reine Funktionen, Gold-Snapshot nach Sichtung.
- Viewer-Math (`View`, Hover) testbar getrennt von `minifb`.

## E. Messung nach dem Umbau (Gegenprobe, `source2`)

Nach Umsetzung von A1–A11 (siehe `plan.md`) laufen alle Pfade bis zum
letzten Bit. Ergebnis `examples/oracle.rs` (opus-pure) und
`scripts/libopus_oracle.py` (libopus) — beide identisch:

| Datensatz | Pakete | `final_range` gleich |
|---|---|---|
| `silk_silk.opus` (SILK WB) | 61 641 | 61 641 |
| `silk.opus` (Hybrid FB) | 61 641 | 61 641 |
| `silk.webm`-Audio (CELT stereo) | 61 641 | 61 641 |
| `rpQs3m5jtIM.webm`-Audio (CELT stereo) | 38 655 | 38 655 |
| 25 Fixtures (3 alt + 22 aus `mkfixtures.sh`: Stereo, FEC, 2,5–120 ms, NB–FB, Wechsel mit Redundanz, DTX) | 11 707 | 11 707 (libopus 1.6.1, inkl. PLC = 0) |
| 12 RFC-6716-Testvektoren (Encoder-`rng` im Vektor) | 20 075 | 20 075 |

Damit ist jeder Befund aus A nicht nur behoben, sondern durch ein
unabhaengiges Orakel belegt.
