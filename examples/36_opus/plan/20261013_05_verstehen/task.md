# Tasks: Verstehen statt nur Ansehen — `05_verstehen`

Seriell abarbeiten; jeder Schritt endet gruen (`cargo test
--all-features` + `cargo fmt --check` + `cargo clippy -D warnings`).
Keine neue Dependency (auch nicht dev); jede Datei < 600 Zeilen;
`forbid(unsafe_code)` bleibt. Commits je V-Schritt (Conventional
Commits wie bisher). Ausgabe-Dateien und Verhalten muessen nicht
backward compatible sein: Alt-Tests/Goldens bei bewussten Aenderungen
anpassen oder ersetzen statt Alt-Formate zu konservieren.

## V0 — Basis sichern

- [ ] Ist-Stand protokollieren: `cargo test --all-features` (71),
      `scripts/smoke.sh`, `cargo tree` (nur `opus-pure`).
- [ ] `opusinfo` auf Demo + `tests/data/ref.opus` laufen lassen (falls
      `opus-tools` da), Werte als Zahlen-Orakel fuer V4 notieren.
      Keine Byte-Goldens des Altstands konservieren.

Verifikation: Ist-Stand protokolliert, alle Gates gruen.

## V1 — Ein Ogg-Durchlauf (Refaktor, Ausgabeform frei)

- [ ] Paket-Einlesung in `03_analyze.rs` buendeln: einmal
      `OggOpusReader`, liefert `Vec<Vec<u8>>`-Pakete + `OpusHead` +
      `OpusTags` (API: `reader.head()`, `reader.tags()`).
- [ ] `decode_mono` und `bitstream::analyze_bytes` auf Paketliste
      umstellen (Signaturen anpassen, Aufrufer in `08_cli::run`
      und Tests fortschreiben); doppeltes TOC-Parsing in
      `05b_bitstream.rs` (Bounds + Loop) auf eines reduzieren.
- [ ] `analyze_path`: nutzen oder loeschen (kein toter Code).
- [ ] `Sources::build`: Spalten nicht mehr voll kopieren, wo ein
      Borrow/Trim reicht; `unwrap_or(0)`-Pad-Fallback in
      `analyze_reader` durch sichtbaren Zaehler/Fehlerpfad ersetzen.
- [ ] `03d_alloc.rs` (592 Zeilen) bei Beruehrung unter 600 halten,
      sonst vorher verantwortungsrein splitten.

Verifikation: Orakel-Tests (Allokations-Identitaet, Korrelation,
Pegel) weiter exakt gruen; betroffene Ausgabe-Tests bewusst angepasst;
alle Tests gruen.

## V2 — Fakten-Modul `03e_fakten.rs` (Implementierung + Tests)

- [ ] Typen: `HeadFakten` (Kanaele, Pre-Skip in ms, Eingangs-Rate,
      Gain in dB, Mapping-Familie), `TagFakten` (Vendor, Kommentare
      roh), `EncoderFakten`, `PcmFakten`.
- [ ] Encoder-Fakten aus `Analysis`: Moduswechsel als Run-Length-
      Liste (Start, Dauer, Modus), Bandbreiten-Histogramm (Zeit-%),
      Bitraten-Min/Max/p50/p95, VBR-Index (StdAbw/Ø), groesste
      Paket-Spruenge, Transient-Zeitpunkte (aus `Sources`),
      Stille-Kandidaten (Pakete <= 2 Bytes / DTX-artig).
- [ ] PCM-Fakten: Decode gibt Kanal-Statistik vor dem Mono-Mix aus
      `04_decode.rs` zurueck (Peak, RMS je Kanal, Clipping-Samples
      >= 0.999), Stille-Abschnitte (RMS < -60 dB, >= 200 ms),
      Stereo-Korrelation, Spektral-Schwerpunkt (aus STFT-Spalten).
- [ ] Unit-Tests je Fakt mit Hand-Fixtures (Perzentile, Runs,
      Histogramm, Korrelation an Sinus/Schweigen-Fixtures).

Verifikation: `cargo test fakten` gruen; Bestand unveraendert gruen.

## V3 — JSON neu entwerfen (kein Alt-Schema)

- [ ] `json()` in `08_cli.rs` von Grund auf neu: `head`, `tags`,
      `summary`, `fakten`; je Frame vollstaendig `config`, `packing`,
      `pad`; CELT-Kurzform nur wo verfuegbar (Band-Energien gerundet,
      Bits je Band). Feldnamen mit Einheit (`_ms`, `_kbps`, `_db`),
      alte Kuerzel (`bw`, `ch`, `i`) entfallen.
- [ ] JSON-String-Escaping fuer Tag-Werte (Anfuehrungszeichen,
      Steuerzeichen, Nicht-ASCII) einfuehren + Test.
- [ ] `tests/cli.rs`: Schema-Tests neu schreiben; Alt-Feld-Tests
      loeschen statt Kompatibilitaet zu behaupten.

Verifikation: `cargo test cli` gruen; `python3 -m json.tool` auf
Demo-JSON ok.

## V4 — Text-Report als Standard (`08b_report.rs`)

- [ ] Report-Abschnitte (deutsch): Datei (Groesse, Dauer, Kanaele,
      Vendor/Tags), Encoder (Modus-/BW-Anteile, Wechsel-Zeitachse
      kompakt, Bitraten-Statistik, VBR-Charakter, Transienten,
      Stille/DTX), Signal (Peak/RMS/Clipping/Korrelation),
      Einordnung (3–5 Erklaer-Saetze).
- [ ] Erklaer-Regeln als Schwellen-Tabelle, je Regel ein Test
      (z. B. SILK-Zeit > 70 % + BW <= WB => „spricht fuer Sprache";
      CELT > 80 % + FB => „spricht fuer Musik in Vollband").
- [ ] Report bei jedem Lauf: nach stdout, bei PNG auf stdout nach
      stderr; `--quiet` in `08_cli::parse` + `HELP` schaltet ihn ab;
      stdin-Eingabe ok. Smoke-Skripte an das neue Standard-Verhalten
      anpassen.
- [ ] Snapshot-Test: Report der Demo-Fixture als neuer Goldtext in
      `tests/` (nach Sichtung); Gegenprobe der Kopf-Werte gegen
      V0-`opusinfo`.

Verifikation: `cargo test report` gruen; manueller Lauf auf
Demo + YouTube-Extrakt gelesen und plausibel.

## V5 — Feinschliff

- [ ] Hover (`09_show.rs`): echte Zeit inkl. Pre-Skip-Hinweis, wenn
      versetzt; Kopfzeile nur aendern, wenn Breiten-Test haelt.
- [ ] README: Report-Standard + neues JSON-Schema; `deps.md`
      bestaetigen (keine neue Dep, `cargo tree`-Beleg).

Verifikation: `interact.sh`-HIL weiter gruen (8/8 Asserts).

## V6 — Hygiene + Walkthrough

- [ ] `cargo fmt`, `clippy -D warnings`, `cargo test --all-features`,
      `cargo upgrade --dry-run`, `scripts/smoke.sh` (inkl. HIL).
- [ ] `plan/20261013_05_verstehen/walkthrough.md` schreiben
      (Deutsch, Mermaid, Glossar-Ergaenzung, Befund→Fix-Tabelle
      zu plan.md §Code Review, Pakete-Tabelle: keine Aenderung).
- [ ] Commits je V-Schritt pruefen (`git log --oneline`).

Verifikation: alle Gates gruen, Walkthrough liegt bei.
