# Tasks: SILK sehen — Vocal-Modell visualisieren (`06_silk`)

Seriell abarbeiten; jeder Schritt endet gruen (`cargo test
--all-features` + `cargo fmt --check` + `cargo clippy -D warnings`).
Keine neue Dependency (auch nicht dev); jede Datei < 600 Zeilen;
`forbid(unsafe_code)` bleibt. Commits je S-Schritt (Conventional
Commits wie bisher). Ausgabe-Dateien und Verhalten muessen nicht
backward compatible sein: Alt-Tests/Goldens bei bewussten Aenderungen
anpassen oder ersetzen statt Alt-Formate zu konservieren.

## S0 — Fixtures sichern

Erledigt (Commit 9672059).

- [x] 10-s-Sprach-Ausschnitt aus `silk_silk.opus` (keine Stille,
      hinter `silk.webm`-Minute 1 gesucht) nach
      `source1/tests/data/silk_sample.opus` schneiden
      (Ziel ~30 KB, per TOC-Zaehhlung alle Config 9 bestaetigen).
- [x] Ist-Stand protokollieren: `cargo test --all-features` (89),
      `scripts/smoke.sh`, `cargo tree` (nur `opus-pure`); Voll-Datei
      (`silk_silk.opus`, 61641× Config 9) und Rezept aus `prompt.txt`
      in `deps.md`-Notiz festhalten (Datei selbst bleibt ungecommittet).

Verifikation: Fixture liegt bei, TOC-Assert gruen, Gates protokolliert.

## S1 — Header-Walk `03f_silk.rs` (Implementierung + Tests)

Erledigt: Module `silk` (Walk, 536) + `silk_dq` (Math, 171) +
`silk_pulse` (Excitation, 126) + `silk_tab` (57 Tabellen, 529);
Orakel: 130-Hz-Anker (129,0 Hz), Autokorrelation (Median 0,996,
167/185 ±10 %), Determinismus, NLSF-Invarianten, Bilanz-Regel.
98 Tests gruen (89 + 9).

- [x] `parse_silk_frame` (Lesereihenfolge wie `silk_decode_indices`:
      Typ/Quant-Offset, Gains absolut/delta, NLSF-Stufe-1/2 + Interp,
      voiced-LTP (Lag absolut/delta, Kontur, Gains, Skalierung), Seed;
      Excitation nur mitzaehlen) + `SilkState` (prev-Lag/Signaltyp,
      Reset bei Moduswechsel wie `CeltState`).
- [x] Tabellen-Port aus `opus-pure`-SILK-Tabellen (ICDFs zuerst);
      bei > 600 Zeilen in `03f_silk_tab.rs` auslagern (Verantwortungs-Split).
- [x] Abgeleitete Groessen: Gains in dB (`gain_quant`-Umrechnung),
      Pitch-Hz aus Lag + Kontur, Bitanteile je Abschnitt aus
      `tell()`-Deltas.
- [x] Orakel-Tests: Bit-Identitaet exakt (Header + Rest + Overhead =
      Input; LCG-Fuzz + echte Fixture-Frames), Determinismus
      (2× parsen identisch), `tell()==1`-Analogon als Regression,
      Pitch-Gegenprobe per Autokorrelation auf PCM (Schwelle wie
      CELT-Korrelation r > 0,7).

Verifikation: `cargo test silk` gruen; Bestand unveraendert gruen.

## S2a — Anzeigen ohne Huelle (Integration + Tests)

Erledigt: `Bitstream`/Hops tragen SILK (Summary je Paket, Kennzahlen je
Hop), F0-/Voicing-Lane im Renderer, Hover-Zeile 3 SILK-bewusst, JSON
`silk`-Objekt, Report `stimme:`-Zeile + 2 Regeln, `08c_json.rs` als
JSON-Split (600er-Regel). 105 Tests gruen (89 + 16).

- [x] `Bitstream` += SILK-Spalten (voicing, pitch_hz, gain_db je Hop,
      hop-aligned wie CELT, Pre-Skip beachtet); `Sources::build`
      ohne Vollkopien (V1-Prinzip aus 05_verstehen wahren).
- [x] Pitch-/Voicing-Ebene im Renderer (Ebene, nicht vierte
      Wasserfall-Farbe: Farblegende erweitern); Hover-Zeile 3 meldet
      SILK-Werte (`STIMMHAFT F0=..HZ GAIN=..DB`), sonst weiter
      ehrliches Grau/`DIV=---`.
- [x] JSON-Frames mit `silk`-Objekt (`signal_type`, `gains_db`,
      `pitch_hz`, `ltp_gains`, `nlsf_interp`, `bits` je Komponente;
      Einheiten im Namen); Report-Kennzahlen (Voicing-Anteile,
      Pitch-Ø/Spanne, Gain-Dynamik) + je neue Regel ein Test.
- [x] Unit-Tests (Hop-Mapping, Pooling, Hover-Format) + Gold-Snapshot
      des Fixture-Reports erst nach Sichtung jeder Zahl.

Verifikation: Fixture rendert Pitch/Voicing sichtbar; alle Tests gruen.

## S2b — LPC-Huelle (Implementierung + Tests)

Erledigt ohne Fallback: Float-Huelle (LSF→LPC→Frequenzgang, gamma=0.99
als Decoder-bwexpander-Analogon), NLSF-Interpolation wortwoertlich nach
Referenz (Verlauf aus ROH-Vektor), Ansicht 2 = Huelle / Ansicht 3 =
STFT-Huelle, Pegel bleibt Peak-0 (Gain-Referenz streut zu stark).
Orakel: 1-kHz- + Zwei-Formanten-Position, Vokal 19/19, Hop-Spezifitaet
(0,52 vs. 0,31). 109 Tests gruen.

- [x] NLSF-Codebuecher + Decode-Port, LPC-Frequenzgang auf die
      256 Log-Bins; Ansicht 2 = Huelle (SILK) / Baender (CELT),
      Ansicht 3 = STFT − Huelle (NaN wo unverfuegbar, wie bisher).
- [x] Huellen-Korrelation gegen PCM (Schwelle wie CELT-Orakel) als
      Sync-Beweis; Sinus-Anker ±2 dB wie in 03_bitstream.
- [x] Fallback-Regel: Sprengt S2b inkl. Tabellen die 600er-Grenze
      auch nach Split, wird Ansicht 2 als Gain-/Pitch-Wasserfall
      gebaut (hier entscheiden, nicht aufschieben).

Verifikation: Huellen-Korrelation gruen; `silk_silk.opus`-Sichtung plausibel.

## S3 — Hybrid-Dividende (Implementierung + Tests)

Erledigt (06_silk S3): ein Coder je Hybrid-Frame (SILK-Praefix →
CELT ab Start 17, neues Modul `05c_hybrid`), Ansicht 2 kombiniert
Huelle+CELT, Dichte traegt SILK-Bits, Hover zeigt SILK-Anteil.
Orakel-Fund unterwegs: SILK-Puls-Reihenfolge war block-verzahnt
statt Shells→LSBs→Signs (Referenz); Fix veraendert Puls-Bits
(S1-Bilanz auf 0–5 Schwaenze geeicht). Tracker-vs-Output bleibt
dokumentierte Schaetzung (PVQ-Skip): Cutoff-Stille-Orakel
(`coded_from`, Referenz-PCM < −70 dB) + Split + Determinismus.
113 Tests gruen, Clippy 0.

- [x] SILK-Praefix liefert CELT-Startoffset: denselben `RangeDec`
      an `parse_celt_frame` weiterreichen (`start=17`, Endband aus BW).
- [x] `silk.opus` (100 % Hybrid, Config 15) zeigt CELT-Baender im
      oberen Band; Allokations-Identitaet pro Hybrid-Frame
      (SILK-Anteil + Cutoff-Stille) als Orakel-Test.
- [x] Hover/JSON/Report kennen gemischte Frames (SILK unten,
      CELT oben); bestehende CELT-Orakel bleiben gruen.

Verifikation: Hybrid-CELT sichtbar + Identitaet exakt; Bestand gruen.

## S4 — Feinschliff

- [ ] README (SILK-Ansichten, Fixture + Kodier-Rezept), `deps.md`
      bestaetigen (keine neue Dep, `cargo tree`-Beleg), Smoke um
      SILK-Assert erweitern (Fixture-Modusanteile, Report-Abschnitt).
- [ ] Hover-Kopfzeile nur aendern, wenn Breiten-Test haelt
      (Lehre aus 05_verstehen V5).

Verifikation: `scripts/smoke.sh` (inkl. HIL-Skip-Regel) gruen.

## S5 — Hygiene + Walkthrough

- [ ] `cargo fmt`, `clippy -D warnings`, `cargo test --all-features`,
      `cargo upgrade --dry-run`, `scripts/smoke.sh`.
- [ ] `plan/20261013_06_silk/walkthrough.md` schreiben (Deutsch,
      Mermaid, Glossar-Ergaenzung, Orakel-Tabelle zu plan.md
      §Code Review, Pakete-Tabelle: keine Aenderung).
- [ ] Commits je S-Schritt pruefen (`git log --oneline`).

Verifikation: alle Gates gruen, Walkthrough liegt bei.
