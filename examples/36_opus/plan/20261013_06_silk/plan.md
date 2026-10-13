# Plan: SILK sehen — Vocal-Modell visualisieren (`06_silk`, 2026-10-13)

## Goal

`opus-vis` zeigt heute nur CELT-Inhalte: Bei SILK- und Hybrid-Dateien
bleiben Ansicht 2–4 (Bitstrom, Divergenz, Dichte) grau, obwohl gerade
Sprache den interessantesten Encoder (SILK: LPC-Vocal-Modell mit Pitch,
LSF-Huelle, Gains) enthaelt. Nach dem Umbau parst ein neuer
SILK-Header-Walk pro Frame Signaltyp, Subframe-Gains, LSF-Indizes,
Pitch/LTP-Parameter und Bitanteile; Ansicht 2 zeigt die LPC-Huelle,
eine neue Pitch-/Voicing-Ebene den Stimmverlauf, Ansicht 4 die
Bitanteile je SILK-Komponente. Als Dividende liefert der SILK-Praefix
den CELT-Start im gemeinsamen Range-Coder und schaltet damit auch
Hybrid-CELT frei. Keine neue Dependency; Fixture ist `silk_silk.opus`
(61641 Pakete, alle Config 9 = SILK WB 20 ms mono, aus `silk.webm`
kodiert, Rezept in prompt.txt).

## Code Review (Befunde, geprueft 2026-10-13)

Stand: 89 Tests gruen, 2 Crates (`opus-pure 0.2.2`), Module < 600
Zeilen, `forbid(unsafe_code)`. Geprueft: `05b_bitstream.rs`
(`available=false` → Boden/Dichte 0/NaN bei Nicht-CELT),
`03c_celt.rs` (Header-Walk-Muster: Seiteninfo lesen, PVQ-Daten
ueberspringen), `02_toc.rs` (Configs 0–11 SILK, 12–15 Hybrid),
`06_render.rs`/`09_show.rs` (4 Quellen, Hover-Zeile 3 CELT-only),
`08_cli.rs`/`08b_report.rs` (Frames mit `celt: null` bei SILK/Hybrid).

1. **Ansichten 2–4 sind CELT-only per Konstruktion.** `per_packet`
   ist `None` fuer jeden Nicht-CELT-Hop; verifiziert an `silk.opus`
   (hybrid_frac 1,0) und `silk_silk.opus` (silk_frac 1,0): beide
   rendern grau. Kein Darstellungsfehler, sondern fehlender Parser.
2. **Kein SILK-Wissen im Code.** `grep -i silk` findet nur
   Modus-Labels und die TOC-Tabelle; der Hybrid-Ausschluss aus
   `04_celt_parser` (gemeinsamer Range-Coder, CELT-Start ohne
   SILK-Decode unrekonstruierbar) gilt weiter — und zeigt zugleich
   den Loesungsweg: Wer den SILK-Praefix liest, kennt den CELT-Start.
3. **Referenz lesbar, aber nicht aufrufbar.** `opus-pure 0.2.2`
   (bereits Dependency) enthaelt den vollen SILK-Decoder
   (`src/silk/decode_indices.rs`: Reihenfolge Signaltyp/Gains →
   NLSF-Stufe-1/2 + Interpolation → voiced-LTP (Lag absolut/delta,
   Kontur, Gains, Skalierung) → Seed; danach erst Excitation).
   Das Modul `silk` ist jedoch nicht `pub` (`lib.rs` exportiert nur
   Decoder/Encoder/Reader auf Dateiebene) — direkter Aufruf geht
   nicht, ein eigener Header-Walk nach CELT-Vorbild (`range` +
   portierte Tabellen wie `03d_alloc.rs`) ist der passende Nachbau.
4. **Anschlussstellen sind duenn.** `Bitstream`/`Sources::build`
   (Hop-Mapping, Pooling, Colormaps), `hover_lines` (Zeile 3),
   JSON-Frames (`celt: null` → `silk`-Objekt daneben),
   Report-Regeln (SILK-Regel existiert als Heuristik, ohne
   Voicing-/Pitch-Zahlen) — alles erweiterbar ohne Pipeline-Umbau.
5. **Fixture-Luecke.** `tests/data` hat nur `ref.opus` (rein hybrid,
   0 CELT-Frames) und Demo-Daten; kein reines SILK-Sprachfixture.
   `silk_silk.opus` (20:32, 3,3 MB) ist zu gross zum Committen —
   ein ~10-s-Ausschnitt (~30 KB) daraus wird Test-Fixture.

## Success Criteria

- `silk_silk.opus` zeigt in Ansicht 2 die LPC-Huelle (aus
  LSF-Indizes), eine Voicing-/Pitch-Ebene (stimmhaft/stimmlos/inaktiv
  + Pitch in Hz) und in Ansicht 4 die Bitanteile je Komponente
  (Gains/LSF/LTP/Seed/Excitation-Rest); Hover-Zeile 3 meldet
  SILK-Werte statt „KEIN CELT"; wo kein SILK verfuegbar ist, bleibt
  die ehrliche Grau-Darstellung (keine 0-dB-Vortaeuschung).
- JSON je Frame mit `silk`-Objekt (Signaltyp, Gains in dB, Pitch-Hz,
  LTP-Gains, NLSF-Interp, Bitanteile), Report mit Voicing-Anteilen,
  Pitch-Statistik und Gain-Dynamik; Schema 2 bleibt prinzipientreu
  (Einheiten im Namen), Alt-Felder duerfen fallen.
- Hybrid-Dateien (`silk.opus`) zeigen zusaetzlich CELT-Baender im
  oberen Band (Start aus dem SILK-Praefix, `start=17`), verifiziert
  per Allokations-Identitaet auf Hybrid-Frames.
- Gates gruen: `cargo test --all-features`, `fmt --check`,
  `clippy -D warnings`, `scripts/smoke.sh`; keine neue Crate
  (`cargo tree`-Beleg); jede Datei < 600 Zeilen.

## Approach

Gleiche Schichtung wie beim CELT-Parser, keine neue Abstraktion:
Neues Modul `03f_silk.rs` (Header-Walk bis einschliesslich Seed,
Excitation-Bits nur mitzaehlen — Spiegel des PVQ-Skips in
`03c_celt.rs`) plus Tabellen-Port `03f_silk_tab.rs` nur falls
`03f_silk.rs` die 600er-Grenze reisst. Eigener `RangeDec`
(`03c_range.rs`) wird wiederverwendet; SILK-Zustand (`SilkState`:
prev-Lag/Signaltyp fuer konditionelle Kodierung) wird bei
Moduswechsel zurueckgesetzt wie `CeltState`. Darstellung stuft nach
Aufwand: S2a (Indizes, Gains, Pitch — nur ICDF-Tabellen noetig)
vor S2b (LPC-Huelle — braucht zusaetzlich NLSF-Codebuecher +
`nlsf_decode`-Port + LPC-Frequenzgang auf die 256 Log-Bins).
Faellt S2b zu gross aus (> 600 Zeilen inkl. Tabellen), wird Ansicht 2
fuer SILK als Gain-/Pitch-Wasserfall statt Huelle gebaut (Fallback,
kein Projektstopp). Bitanteile kommen aus `tell()`-Deltas je
Walk-Abschnitt (exakt summierend wie Orakel 1 in 04).

## Steps

1. **S0 Fixtures sichern.** 10-s-Ausschnitt aus `silk_silk.opus`
   (Sprache, keine Stille) nach `source1/tests/data/silk_sample.opus`;
   Voll-Datei bleibt ausserhalb des Repos (Pfad + Rezept in
   `prompt.txt`/`deps.md`-Notiz). TOC-Zaehhlung (alle Config 9) als
   Smoke-Assert.
2. **S1 Header-Walk `03f_silk.rs`.** `parse_silk_frame` (Lesereihenfolge
   wie Referenz: Typ/Quant-Offset, Gains, NLSF-1/2 + Interp, voiced-LTP,
   Seed), `SilkState` mit Reset bei Moduswechsel, `SilkFrame`
   (Signaltyp, Gains-dB, Lag/Kontur/LTP-Gains, NLSF-Indizes, Bitanteile
   je Abschnitt, Excitation-Rest).
3. **S2a Anzeigen ohne Huelle.** `Bitstream` += `silk`-Spalten
   (voicing, pitch_hz, gain_db je Hop), Pitch-/Voicing-Ebene im
   Renderer, Hover-Zeile 3 und JSON-`silk`-Objekt, Report-Kennzahlen.
4. **S2b LPC-Huelle.** NLSF-Codebuecher + Decode-Port, LPC-Frequenzgang
   auf Log-Bins, Ansicht 2 = Huelle (SILK) / Baender (CELT),
   Ansicht 3 = STFT − Huelle. Bei Groessen-Not: Gain-/Pitch-Fallback.
5. **S3 Hybrid-Dividende.** SILK-Praefix → CELT-Startoffset im
   selben `RangeDec` weiterreichen (`start=17`, Endband aus BW);
   `silk.opus` zeigt Hybrid-CELT-Baender; Allokations-Identitaet gilt
   pro Hybrid-Frame (SILK-Bits + CELT-Bits + Overhead = Input).
6. **S4 Feinschliff + Walkthrough.** README (SILK-Ansichten, Fixture,
   Rezept), `deps.md` (weiter 2 Crates), Smoke (SILK-Assert),
   `walkthrough.md` (Deutsch, Mermaid, Orakel-Tabelle, Pakete:
   keine Aenderung), Commits je S-Schritt.

## Validation Plan

- Je Schritt: `cargo test --all-features`, `cargo fmt --check`,
  `cargo clippy --all-features -- -D warnings` gruen vor dem naechsten.
- Orakel wie in 04 (Unabhaengigkeit vor Plausibilitaet): Bit-Identitaet
  exakt (Header + Rest + Overhead = Input, Fuzz + echte Frames),
  Pitch-Gegenprobe per Autokorrelation auf PCM (Schwelle, z. B. r > 0,7
  wie CELT-Korrelation), Determinismus (2× parsen = identisch),
  `tell()==1`-Analogon als Regressions-Test, Gold-Snapshot des
  Fixture-Reports erst nach Sichtung jeder Zahl.
- End-zu-End: `scripts/smoke.sh` (inkl. SILK-Assert), manueller Lauf
  auf `silk_silk.opus` (Huelle/Pitch sichtbar) und `silk.opus`
  (Hybrid-CELT oben); Report gegen `opusinfo`-Werte wo verfuegbar.

## Risks / Open Questions

- Groesstes Risiko: S2b-Tabellengroesse (NLSF-Codebuecher) gegen
  600er-Regel — gemindert durch S2a-vor-S2b und den dokumentierten
  Fallback (Gain-/Pitch-Wasserfall statt Huelle).
- Konditionelle Kodierung (Delta-Lag, Delta-Gains, LBRR/VAD-Flags)
  braucht Frame-Kontext ueber Paketgrenzen: wie `CeltState` strikt
  sequentiell parsen, Reset nur bei Moduswechsel.
- Stereo (`silk_stereo_decode_pred`, Mid-only-Flag existiert in der
  Referenz) kommt erst nach Mono-Gruen: Fixture ist mono.
- Offene Frage: keine — Dateiname (`silk_silk.opus`), Fixture-Laenge
  (~10 s) und Fallback sind reversible Defaults und im Plan vermerkt.

## Sources

- RFC 6716 (Opus-Standard, SILK-Layer §4.2):
  [RFC-Info](https://www.rfc-editor.org/info/rfc6716/)
- Lese-Reihenfolge der SILK-Seiteninfo: lokale Referenz
  `opus-pure 0.2.2`, `src/silk/decode_indices.rs` (`silk_decode_indices`,
  bis Seed vor Excitation) — bereits Dependency, kein neuer Import.
- Encoder-Optionen der Fixture: lokale Ausgabe
  `ffmpeg -h encoder=libopus` (`application voip`, `frame_duration`).
