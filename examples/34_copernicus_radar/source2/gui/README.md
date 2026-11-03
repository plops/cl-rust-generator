# SAR GUI Prototyp (`sar_gui_server`)

Ein lokaler Test- und Demonstrations-Prototyp für die `source2`-Kette:
Du wählst ein Punkt-Asset auf einer Karte (oder tippst Koordinaten),
der Server lokalisiert es im SAR-Datensatz (`locate_asset`), fokussiert
den besten Burst als Hintergrund-Job (`focus_tops_burst_cpu`), zeigt den
Radar-Chip mit Statistik (`chip_stats`) und vergleicht bei zwei Szenen
Master gegen Slave mit Ampel (`bewerte_zeitreihe`) und Wochenreport
(`wochenbericht`).

Gedacht für technische Tester und den ersten Pilotkunden — kein
Produktivsystem, keine Nutzerverwaltung, kein Upload. Alles läuft auf
`127.0.0.1`, alle Jobs und Caches verfallen bei Neustart.

## Voraussetzungen

- Rust-Toolchain mit `cargo` (getestet mit der Workspace-Toolchain).
- Die Taiwan-Testdaten liegen im Repo:
  `../data/insar/S1C_IW_RAW__0SDV_20260928…/` (Master) und
  `…/S1C_IW_RAW__0SDV_20261010…/` (Slave) sowie DEM-Kacheln unter
  `../data/dem/`. Fehlt ein Datensatz, meldet `/api/status`
  `"state": "fehler"` statt `"bereit"`.
- Für die Karte braucht der Browser Netz (Leaflet/OSM-Tiles). Ohne Netz
  bleibt die Karte leer — Koordinateneingabe und alle API-Funktionen
  laufen trotzdem.

## Start

Aus dem Verzeichnis `source2/` starten (Default: Taiwan-Master, Port 8787):

```sh
cargo run --bin sar_gui_server
```

Mit zwei Szenen, damit der Vergleich aktiv ist:

```sh
cargo run --bin sar_gui_server -- --master ../data/insar/S1C_IW_RAW__0SDV_20260928T215149_20260928T215222_009653_01336F_D2EE.SAFE/s1c-iw-raw-s-vv-20260928t215149-20260928t215222-009653-01336f.dat --slave ../data/insar/S1C_IW_RAW__0SDV_20261010T215149_20261010T215222_009828_013950_CEB1.SAFE/s1c-iw-raw-s-vv-20261010t215149-20261010t215222-009828-013950.dat --port 8787
```

Dann im Browser öffnen: `http://127.0.0.1:8787/`

Optionen: `--dataset <pfad>` (Alias für `--master`), `--master`,
`--slave`, `--port` (Default `8787`), `--host` (nur Loopback-Adressen
wie `127.0.0.1`, `::1`, `localhost` werden akzeptiert).

## Walkthrough: die Taiwan-Demo in 6 Schritten

1. **Ankommen.** Nach dem Öffnen zeigt die Statuszeile `bereit` — der
   Datensatz ist geladen, noch ist nichts verarbeitet. (Stünde dort
   `lädt` oder `fehler`, warte bzw. prüfe den Datensatzpfad.)
2. **Demo laden.** Klicke „Kaohsiung-Beispiel laden“. Die Felder füllen
   sich mit `lat 22.62`, `lon 120.31` (Höhe leer = DEM-Vorschlag wird
   genommen), und der Browser schickt `POST /api/locate`.
3. **Burst wählen.** Die Ergebnis-Tabelle listet die Kandidaten, bester
   zuerst. Erwartung: **IW-Swath 11 (= IW2), Burst 9**, dazu
   `az ≈ 267.3`, `rg ≈ 15305`. Klicke beim besten Treffer
   „Fokus starten“ — ohne diesen Klick wird bewusst kein Chip
   versprochen.
4. **Warten.** Der Status wechselt auf „Fokus läuft“, das Frontend fragt
   `GET /api/jobs/{id}` im Sekundentakt ab (`pending` → `running` →
   `done`). Das dauert eine knappe Minute; einfach abwarten.
5. **Ergebnis lesen.** Bei `done` erscheint der Chip (Radar-Ausschnitt
   in Slant-Range, rotes Fadenkreuz = fokussierte Mitte) mit
   `centerRow ≈ 1499.9`, `centerCol ≈ 15305.4` sowie der Statistik
   (Median ≈ 62.3 dB, p90 ≈ 67.6 dB). Die Chip-Halbbreiten
   (`halfAz`/`halfRg`, Default ±16 × ±20) lassen sich anpassen und neu
   fokussieren.
6. **Vergleichen (nur mit Slave).** Master- und Slave-Job je einmal
   fokussieren, dann zeigt der Vergleichsbereich die Drift (Slave minus
   Master, Erwartung ≈ −0.2 dB) mit **Ampel Grün** bei den
   kalibrierten Schwellen Warn 3 dB / Alarm 6 dB (editierbar, Stand
   2026-11-03, als Prototyp gekennzeichnet). Der Wochenreport erscheint als schreibgeschützter
   Text; bei weniger als zwei Messungen steht ein Hinweis darin statt
   einer Ampel-Behauptung.
7. **Polygon (Phase B).** „Polygon-Modus“ einschalten, drei Ecken als
   Kleinpolygon um Kaohsiung anklicken (z. B. `22.62/120.31`,
   `22.64/120.31`, `22.63/120.33`), Schrittweite `0.01` lassen und
   „Polygon auswerten“. Erwartung: mehrere Samples, aggregiert
   rund um **Burst 9** (größte Gruppe zuerst, mit bestem Sample).
   „Zurücksetzen“ löscht Ecken und Ergebnis; fokussiert wird dabei
   bewusst nichts.

## Wenn etwas schiefgeht

| Symptom | Bedeutung / Abhilfe |
|---|---|
| Status `fehler` nach Start | Datensatzpfad prüfen (`--master`/`--dataset`); die `.dat`-Datei muss existieren. |
| „Punkt liegt außerhalb der Szene“ | Koordinaten liegen nicht im geladenen Datensatz — Demo-Koordinaten oder Taiwan-Datensatz verwenden. |
| `lat 91` o. Ä. wird abgelehnt (400) | Korrekt so: Breite muss in [-90, 90], Länge in [-180, 180] liegen, Höhe endlich sein. |
| „Fokus läuft noch: bitte warten“ | Job ist noch in `pending`/`running` — abwarten, nicht doppelt starten (gleicher Burst wird aus dem Cache bedient). |
| Unbekannte Job-ID (404) | Tippfehler oder Server wurde neu gestartet — Jobs verfallen bei Neustart, einfach neu fokussieren. |
| Karte leer, „kann gerade nicht geladen werden“ | Kein Netz für Leaflet/OSM-Tiles — kein Systemfehler; Koordinaten eintippen geht trotzdem. |
| Vergleichsbereich ausgegraut | Nur ein Datensatz geladen — mit `--slave` neu starten. |

## API-Referenz

- `GET /` — Frontend (einzelne eingebettete HTML-Datei, kein Build-Schritt).
- `GET /api/status` — `{"state","dataset","slaveAvailable","slaveDataset","ephemerisAvailable","demTilesAvailable"}`.
- `POST /api/locate` — `{"lat","lon"[,"h"][,"datasetId"]}` → `{"datasetId","hits":[…]}` (bester zuerst). Ohne `h` DEM-Vorschlag, sonst 0 m.
- `POST /api/polygon` — `{"corners":[[lat,lon],…≥3][,"stepDeg"=0.01][,"h"][,"datasetId"]}` → `{"datasetId","sampleCount","stepDeg","groups":[{swathNumber,burstIndex,count,best}]}` (größte Gruppe zuerst; nur Lokalisierung, kein Fokus).
- `POST /api/focus` — `{"datasetId","swathNumber","burstIndex","az","rg","tZd"[,"halfAz","halfRg","asset"]}` → `202 {"jobId"}`. Chip-Fenster-Default ±16 × ±20.
- `GET /api/jobs/{id}` — `pending`/`running`/`done`/`error`; bei `done` Chip-Statistik, Chip-Mitte (`centerRow`/`centerCol`), `halfAz`/`halfRg`, `pngUrl`, TOPS-Parameter (`alpha`, `ktHzS`).
- `GET /api/chip/{id}` — Chip-PNG (Graustufen, rotes Fadenkreuz auf der fokussierten Mitte).
- `GET /api/compare?master={job}&slave={job}[&warn=3][&alarm=6]` — Drift (Slave-Median minus Master-Median) + Ampel nach `bewerte_zeitreihe`. Schwellen 2026-11-03 kalibriert (Endpunkt weiter Prototyp: `"prototyp": true`).
- `GET /api/report?asset=&warn=&alarm=` — Wochenreport als Markdown-Text (`wochenbericht`) aus den abgeschlossenen Fokus-Jobs dieses Assets; Hinweis im Text bei weniger als zwei Einträgen.
- Alle Fehler: JSON `{"error": "<deutsche Meldung>"}` mit 400 (Eingabe), 404 (Job/Datensatz unbekannt), 500 (System).

## Grenzen (bewusst so)

- Nur Loopback, keine Authentifizierung — kein öffentliches Deployment.
- Keine Persistenz: Jobs, Burst-Cache und Report-Einträge verfallen bei Neustart (steht auch im GUI).
- Karte (Leaflet/OSM) braucht Netz; offline bleibt sie leer, alle API-Funktionen laufen weiter.
- Chip-Ausschnitte liegen in Slant-Range-Geometrie (Azimut × Schrägentfernung) — kein Karten-Overlay.
- Vergleichs-Ampel (Warnung 3 dB / Alarm 6 dB) ist 2026-11-03 an 24 beschrifteten Master/Slave-Chip-Paaren + synthetischen Positiven kalibriert (Fehlalarm 3/24 an Warn-, 0/24 an Alarmschwelle; Details im Doku-Kommentar von `bewerte_zeitreihe`).
- Code: alles in `src/bin/sar_gui_server.rs` (Frontend als String-Konstante eingebettet, siehe Kopfkommentar dort). Es wird bewusst nichts aus `12_geoloc.rs`/`05_ingest.rs`/`06_rda.rs` nachgebaut — der Server wrappt nur.
