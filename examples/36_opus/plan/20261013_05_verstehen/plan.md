# Plan: Verstehen statt nur Ansehen (`05_verstehen`, 2026-10-13)

## Goal

`opus-vis` macht aus einer `.opus`-Datei weiterhin PNG, JSON und das
X11-Fenster, erklaert die Datei aber zusaetzlich in Worten und Zahlen:
Was steht im Kopf und in den Tags, was hat der Encoder wann und warum
ungefaehr getan, und was faellt am decodierten Signal auf. Keine neue
Dependency; alles Neue ist `std`-only und nutzt Daten, die die Pipeline
schon liest oder decodiert, aber heute wegwirft. Ausgabe-Dateien und
Verhalten muessen nicht backward compatible sein: JSON-Schema,
CLI-Verhalten und PNG-Inhalt werden frei und sauber neu entworfen,
alte Tests und Goldens werden angepasst statt alte Formate
weiterzuschleppen.

## Code Review (Befunde, geprueft 2026-10-13)

Stand: 5441 Zeilen, 71 Tests (`--all-features`: 47 Unit + 24
Integration) gruen, `fmt --check` und `clippy -D warnings` sauber,
`cargo tree` ohne `show` = nur `opus-pure 0.2.2`. Staerken bleiben:
zwei Deps, `forbid(unsafe_code)`, Orakel-Tests fuer den CELT-Parser,
Module < 600 Zeilen, ehrliche `NaN`-Divergenz.

1. **Dreifacher Ogg-Durchlauf.** `cli::run` (`08_cli.rs`) liest die
   Datei komplett ein und demuxt dreimal getrennt:
   `analyze::analyze_reader`, `decode::decode_mono` (eigener
   `OggOpusReader`) und `bitstream::analyze_bytes` (dritter Reader,
   TOC je Paket sogar zweimal). Bei der 12-Minuten-Datei kostet das
   3x Demux plus doppelte Paket-`Vec`s; `analyze_path` (`03_analyze.rs`)
   ist toter Code (kein Aufrufer).
2. **Metadaten werden weggeworfen.** `opus-pure` liefert `OpusHead`
   (Pre-Skip, Eingangs-Samplerate, Output-Gain, Mapping-Familie,
   Stream-Zahlen) und `OggOpusReader::tags()` (Vendor + Kommentare wie
   Titel/`ENCODER`), gelesen wird nur `channel_count`. Genau die
   „interessanten Informationen ueber die Datei" fehlen damit.
3. **JSON ist aermer als das interne Modell.** `FrameMeta`
   (`01_types.rs`) kennt `packing` und `pad_bytes`, `json()` in
   `08_cli.rs` gibt sie nicht aus; Config-Nummer, CELT-Energien/Bits,
   Transient-Flags und jede Kopf-/Tag-Info fehlen. Die maschinenlesbare
   Lern-Ausgabe hinkt dem Hover hinterher.
4. **Decodiertes PCM wird nur gemalt, nie vermessen.** Kein Peak/RMS,
   kein Clipping, keine Stille-/DTX-Abschnitte, keine Stereo-Korrelation
   — obwohl `decode_mono` das Signal schon vollstaendig liefert.
5. **Encoder-Verhalten nur als Bild, nie aggregiert.** Moduswechsel,
   Bandbreiten-Histogramm, Bitraten-Perzentile, VBR-Streung und
   Transient-Zeitpunkte muss der Nutzer aus Pixeln raten; es gibt
   keinen Text-Report und keine Erklaer-Saetze.
6. **Kleinere Punkte.** `Sources::build` (`05b_bitstream.rs`) kopiert
   alle Spalten (`to_vec`); `analyze_reader` schluckt Split-Fehler per
   `unwrap_or(0)` (Pad still falsch statt sichtbar); `03d_alloc.rs`
   liegt mit 592 Zeilen an der 600er-Grenze; PNG bleibt stored-deflate
   (bekannt, ~5 MB bei 12 min, hier bewusst nicht angefasst).

## Success Criteria

- Jeder normale Lauf druckt den deutschen Text-Report standardmaessig
  (nach stdout, bei PNG auf stdout stattdessen nach stderr;
  `--quiet` schaltet ihn ab): Datei/Kopf/Tags, Dauer, Kanaele,
  Pre-Skip, Encoder-Vendor, Bitraten-Statistik (Ø, Min, Max, p50/p95),
  Modus- und Bandbreiten-Anteile, Moduswechsel-Zeitachse (Run-Length),
  Stille-/DTX-Abschnitte, Transienten, PCM-Fakten (Peak, RMS, Clipping,
  Stereo-Korrelation) und 3–5 Erklaer-Saetzen (z. B. hoher SILK-Anteil
  plus schmale Bandbreite spricht fuer Sprache).
- Das JSON wird sauber neu entworfen (kein Alt-Schema als Ballast):
  `head`, `tags`, `summary`, `fakten` (Encoder- und PCM-Auswertung)
  und je Frame vollstaendig `config`, `packing`, `pad` plus — wo CELT
  verfuegbar — Band-Energien/Bits in kompakter Form. Feldnamen tragen
  ihre Einheit (`_ms`, `_kbps`, `_db`), alte Kuerzel entfallen.
- Nur noch ein Ogg-Durchlauf in `run()`; `analyze_path` ist geloest
  (genutzt oder geloescht); Speicher fuer Spalten wird nicht mehr
  vollstaendig dupliziert.
- Gates unveraendert gruen: `cargo test --all-features` (Bestand plus
  neue Tests), `fmt --check`, `clippy -D warnings`, `scripts/smoke.sh`;
  `cargo tree` zeigt weiter keine neue Crate.

## Approach

Schichtung bleibt, Datenfluss wird ehrlich: Ein neuer Einlese-Schritt
(`03_analyze.rs`) liefert in einem Durchlauf Pakete, `OpusHead` und
Tags; Analyse, Decode und Bitstrom konsumieren dieselbe Paketliste
statt je eines eigenen Readers. Darauf sitzen zwei duenne, reine
Auswerte-Schichten ohne neue Deps: `03e_fakten.rs` (Datei-, Encoder-
und PCM-Fakten als Structs, voll testbar) und `08b_report.rs`
(Report-Text + neu entworfenes JSON aus diesen Fakten). Der
CELT-Parser bleibt unangetastet; Render, PNG-Kopf und Viewer duerfen
angepasst werden, wo es der Verstaendlichkeit dient (z. B. Vendor-
und Ansichts-Zeile im Kopf). Erklaer-Saetze sind eine kleine,
deterministische Regel-Tabelle in `08b_report.rs` (Schwellen
mit Test je Regel), kein Freitext-Generator. Deutsche Texte und
der eigene 5x7-Font bleiben.

Nicht-Ziele: kein SILK-Parser (Hybrid/SILK bleibt markiert statt
geparst, Begruendung aus `04_celt_parser` gilt weiter), keine
PVQ-/Banddaten, keine Wiedergabe, keine Web-UI, kein Multistream-
Decode, kein PNG-Deflate-Umbau, keine neue Crate (auch nicht dev).

## Steps

1. **V0 Basis sichern.** Ist-Stand festhalten (71 Tests, Smoke,
   `opusinfo`-Werte der Fixtures als Zahlen-Orakel), dann umbauen.
   Byte-Goldens des Altstands werden nicht konserviert.
2. **V1 Ein Durchlauf.** Paketliste + Head + Tags einmal lesen;
   `analyze_reader`, `decode_mono`, `analyze_bytes` darauf umstellen;
   `analyze_path`-Frage klaeren; `Sources::build`-Kopien auf das
   Minimum kuerzen. Semantik (Frames, Pegel, Allokation) bleibt per
   Orakel-Tests gleich, Ausgabeform darf sich schon hier aendern.
3. **V2 Fakten-Modul.** `03e_fakten.rs`: Head/Tags uebernehmen;
   Encoder-Fakten (Moduswechsel-Runs, BW-Histogramm, Bitraten-
   Perzentile, VBR-Index, Transienten, Stille aus Paketgroessen);
   PCM-Fakten (Peak/RMS je Kanal vor dem Mono-Mix, Clipping-Zaehler,
   Stille-Abschnitte, Stereo-Korrelation, Spektral-Schwerpunkt aus
   vorhandenen STFT-Spalten). Reine Funktionen, Unit-Tests je Fakt.
4. **V3 JSON neu entwerfen.** `08_cli::json` von Grund auf neu
   (`head`, `tags`, `summary`, `fakten`, vollstaendige Frames mit
   `config`/`packing`/`pad` und optionaler CELT-Kurzform);
   String-Escaping fuer Tag-Werte korrekt einfuehren und testen;
   `tests/cli.rs`-Schema-Tests neu schreiben (keine Alt-Feld-Tests).
5. **V4 Text-Report als Standard.** `08b_report.rs`; Report laeuft
   bei jedem Lauf mit (`--quiet` schaltet ab, Hilfe-Text inklusive):
   Abschnitte wie in den Erfolgskriterien, Zahlen aus V2,
   Erklaer-Regeln mit Schwellen-Tests; Report auch bei stdin-Eingabe;
   Snapshot-Test auf der Demo-Fixture.
6. **V5 Feinschliff Doku.** Kopfzeile im PNG optional um Vendor kuerzen
   (nur wenn Platzregel im Test haelt, sonst weglassen), Hover zeigt
   zusaetzlich Pre-Skip-versetzte echte Zeit an; README und `deps.md`
   (unveraendert: keine neue Dep) fortschreiben.
7. **V6 Hygiene + Walkthrough.** fmt/clippy/Tests/Smoke gruen,
   `walkthrough.md` in `plan/20261013_05_verstehen/` (Deutsch,
   Mermaid, Befund→Fix-Tabelle), Commits je V-Schritt.

## Validation Plan

- Je Schritt: `cargo test --all-features`, `cargo fmt --check`,
  `cargo clippy --all-features -- -D warnings` gruen, bevor der
  naechste beginnt. Alt-Tests und -Goldens werden bei bewussten
  Format-/Verhaltens-Aenderungen angepasst oder ersetzt, nicht als
  Kompatibilitaets-Beweis mitgeschleppt.
- Semantik gegen unabhaengige Orakel: Frames/Pegel/Allokation weiter
  exakt (Allokations-Identitaet, Korrelation), Perzentile und
  Histogramm an Hand-Fixtures, PCM-Pegel an der Sinus-Fixture
  (bekannter Pegel aus `tests/spectrum.rs`), neuer Report- und
  JSON-Snapshot exakt nach bewusster Sichtung.
- End-zu-End: `scripts/smoke.sh` (inkl. HIL) plus manueller Lauf auf
  der YouTube-Datei (`rpQs3m5jtIM.webm`-Extrakt) und `ref.opus`;
  Report-Ausgabe lesen und gegen `opusinfo`-Werte (Dauer, Kanaele,
  Vendor) gegenpruefen, falls `opus-tools` vorhanden ist.
- Hoechstes Risiko: V1 (Umbau traegt alles) — deshalb Orakel-Tests
  aus V0 als Tor statt Byte-Gleichheit; zweites Risiko Tag-Strings
  in JSON (Escaping-Test mit Anfuehrungszeichen und Nicht-ASCII).

## Risks / Open Questions

- `opus-pure`-Tags sind Roh-`NAME=value`-Strings ohne Normalisierung —
  Report zeigt sie woertlich (Annahme, im Report kenntlich gemacht).
- Erklaer-Saetze sind Heuristiken mit Schwellen; sie werden als solche
  formuliert („spricht fuer"), nicht als Befund verkauft.
- Keine offene Default-Frage mehr: Der Report ist Standard (mit
  `--quiet` abschaltbar), weil Rueckwaertskompatibilitaet ausdruecklich
  nicht gefordert ist; Smoke und Skripte werden entsprechend angepasst.
