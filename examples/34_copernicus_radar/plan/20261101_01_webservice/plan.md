# Webservice-Plan (`source2` als Website): Informationssammlung

Stand: 2026-11-01. Ziel: `source2`-Funktionalität als Website anbieten —
Asset-Monitor (Dämme, Hänge, Korridore) per **Reverse-Geokodierung**
(Nutzer wählt Asset auf der Karte, System mappt in Radar-Koordinaten).
Kein Voll-Remapping der Szenen im MVP.

## 1. Datenzugänge

### 1.1 Sentinel-1 RAW-Produkte — Copernicus Data Space Ecosystem (CDSE)

- Account: [dataspace.copernicus.eu](https://dataspace.copernicus.eu) (kostenlos).
- Katalog + Download: [OData-API](https://documentation.dataspace.copernicus.eu/APIs/OData.html)
  (Vollprodukte, kein Subsetting), S3-Zugriff via `eodata.dataspace.copernicus.eu`
  (Bucket `eodata`, Zugangsschlüssel in den Account-Einstellungen erzeugen).
- Alternativen: STAC-API, openEO (inkl. serverseitigem Subsetting/Processing).
- Hinweis: RAW-Produkte sind groß (~1,2 GB/Szene SM). Download-Strategie
  (On-Demand vs. Cache) noch zu entscheiden.
- TODO: Account anlegen, S3-Keys erzeugen, OData-Query für `S1*_RAW__0*`
  testen, Quotas/Kosten für Vielabruf klären.

### 1.2 Präzise Orbits — POD-Hub (AUX_POEORB)

- Der CPOD-Service publiziert Predicted / Restituted / **Precise** Orbits
  (`AUX_POEORB`); präzise Produkte ca. 20 Tage nach Aufnahme, Restituted
  deutlich früher, Predicted schon vor der Aufnahme.
- Alter GNSS-Hub (`scihub.copernicus.eu/gnss`) ist abgekündigt; Orbitprodukte
  liegen jetzt im CDSE (OData/S3, siehe 1.1).
- Produkt-Handbuch (Formate, Latenzen): [CPOD Product Handbook (PDF)](https://sentiwiki.copernicus.eu/__attachments__/1675181/GMV-CPOD3-PH-0001-v1.12-Copernicus-POD-Product-Handbook.pdf?inst-v=a3bbaca0-46f2-4817-9be5-e43ec531ff62).
- Konsequenz: Für Asset-Zeitreihen sind Precise-Orbits ok (ein paar Tage bis
  Wochen Verzug) — für „Warnung in Stunden" nicht. Latenz-Budget dokumentieren.
- TODO: OData-Query für `AUX_POEORB` zum Aufnahmezeitpunkt testen, EOF-Parsing
  (XML) in `03_ephem.rs`-Nähe bauen, Fallback-Kette predicted → restituted → precise.

### 1.3 Höhenmodell — Copernicus DEM GLO-30 (nur Punktabfragen im MVP)

- Für Reverse-Geokodierung reicht **eine Höhenabfrage pro Asset-Punkt**
  (bilinear), kein Szenen-Remapping.
- Bezug: CDSE per STAC + authentifiziertem S3 (Collection `COP-DEM_GLO-30-DGED`,
  COGs, 30 m); Mirrors ohne Account: [AWS Open Data](https://registry.opendata.aws/copernicusdem/),
  Microsoft Planetary Computer. Downloader-Skript als Referenz:
  [Copernicus-DEM-Downloader](https://github.com/MMotaFerreira/Copernicus-DEM-Downloader).
- Lizenz frei/offen (README liegt jedem Zip bei) — vor Produktstart prüfen.
- TODO: Kachel-Cache pro Interessengebiet, Fallback Ellipsoid-Höhe +
  Unsicherheitsradius im Flachland.

### 1.4 Validierung statt Konkurrenz — EGMS & EMS

- [EGMS Explorer](https://egms.land.copernicus.eu) (PSI-Senkungskarten Europa,
  Produkte Basic/Calibrated/Ortho, jährliche Updates) + [API-Notebook](https://github.com/copernicus-land/egms-api).
  Nutzung für uns: Ground Truth zum Validieren eigener Zeitreihen (Europa).
  Werkzeuge: [EGMStream](http://Www.Mdpi.Com/2076-3263/15/4/154) (AoI-Download),
  [EGMSpy](https://eartharxiv.org/repository/view/12964/) (Klassifikation).
- [EMS Rapid Mapping](https://emergency.copernicus.eu/mapping/) nur per
  Aktivierung durch autorisierte Stellen (Bevölkerungsschutz) — kein
  Dauer-Monitoring, keine Asset-Alarme. Bestätigt die Produktlücke
  (Entscheidungsschicht statt Karten).
- TODO: EGMS-Kachel über Taiwan-Validierungsgebiet? (Nein — EGMS nur Europa;
  Validierung dort über eigene Paare + Literatur.)

## 2. Technik-Entscheidungen (offen)

- GPU-Hosting (eigene RTX vs. Cloud), Jobqueue, TB-Speicher, COG-Tiling.
- Latenz-Budget je Alarmtyp (Wochen-Ampel vs. Event-Reaktion) + Disclaimer.
- Formate: Orbit-EOF-Parser, DEM-Cache, Asset-DB (Polygon → Burst/Pixel-Mapping).
- Kostenkontrolle: GPU-Minuten, S3-Traffic, Quotas.

## 3. Nächste Schritte

1. ~~CDSE-Account + S3-Keys~~ — erledigt 2026-11-01 (Keys laufen 2027-01-15,
   danach im Keys-Manager erneuern/widerrufen).
2. ~~Orbit + DEM laden~~ — erledigt 2026-11-01, siehe §4.
3. ~~Reverse-Geokodierer prototypieren~~ — erledigt 2026-11-01, siehe §5.
   Offen daraus: fokussierte Range-Achse (ntx-Wrap-Rand) abgleichen,
   Layover präziser (Hangneigung statt Einfallswinkel-Faustregel).
4. EGMS-Vergleichsgebiet in Europa festlegen (Validierung).
5. MVP-Scope: Asset-Ampel + Wochenreport, ein Pilotkunde (Damm/Korridor).

## 4. Download-Befunde 2026-11-01 (S3-Zugang verifiziert)

Technik: S3-Endpunkt `eodata.dataspace.copernicus.eu`, Bucket `eodata`,
SigV4-**Region `default`** (nicht `us-east-1`!), Pfad-Stil. STAC-Katalog
(`catalogue.dataspace.copernicus.eu/stac`) ist ohne Login suchbar.

Geholt:

- `data/aux_orbits/`: je ein S1C-`AUX_POEORB` (je ~4,6 MB) für Master
  (`V20260927T225942–20260929T005942`, publiziert 2026-10-18) und Slave
  (`V20261009T225942–20261011T005942`, publiziert 2026-10-30) des
  Taiwan-Paars. Pfadschema: `Sentinel-1/AUX/AUX_POEORB/<JJJJ>/<MM>/<TT>/`
  (Tagesordner = Gültigkeitsbeginn). Nur S1C/S1D im Archiv (kein S1A/S1B).
  Bestätigt: ~20 Tage Latenz bis Precise — siehe §1.2.
- `data/dem/`: 6 SRTM-1-Kacheln (`.hgt`, je 3601×3601 int16) N21–N23 ×
  E120–E121 aus `auxdata/SRTMGL1/dem/`. Stichprobe ok: Kaohsiung 21 m,
  Zentralgebirge 2837 m, Yushan-Nähe 3809 m.

Einschränkung: `auxdata/CopDEM_COG/copernicus-dem-30m/` (GLO-30, STAC-Collection
`cop-dem-glo-30-dged-cog`) liefert mit diesen Keys 403 — lesbar ist der
Katalog, ladbar ist SRTM. Für Reverse-Geokodierung (Punktabfragen) reicht
SRTM-30-m völlig; GLO-30 bleibt optional (eigene Berechtigung oder
STAC- items mit OAuth-Token laden).

## 5. Reverse-Geokodierer 2026-11-01 (source2/src/12_geoloc.rs)

Gebaut und getestet (`cargo oxide test`, volle Suite 12/12 + 2/2, clippy/fmt
grün; `cargo-oxide` dafür per `cargo install --git NVlabs/cuda-oxide` geholt):

- `parse_poeorb_eof`: AUX_POEORB-EOF → `EphemPoint` (GPS-Zeitachse inkl.
  18 Schaltsekunden). Achtung Format: `<X unit="m">` trägt Attribute.
- `HgtTile`: SRTM-1-`.hgt`-Punktabfrage (bilinar, NODATA→0).
- `locate_asset(ephem, dataset, lat, lon, h) -> Vec<AssetHit>`:
  (Sub-Swath, Burst, Zeile im fokussierten Burst, Range-Index, Einfallswinkel,
  Layover-Faustregel < 25°). Projektions-Newton wiederverwendet
  (`insar::project_target_to_slave`).
- **Wichtigste Erkenntnis:** Bei TOPS ist die Treffer-Achse die fokussierte
  `n_az_fft`-Achse um die Burst-Mitte (bis 4096 Zeilen), nicht die Rohzeile —
  Zero-Doppler-Zeiten liegen bis ±1,4 s außerhalb des Pulsfensters.
  Verifiziert an Kaohsiung (22,62°N, 120,31°E): Treffer IW2/Burst 9,
  Einfallswinkel 40,6°, Rundreise ECEF↔geodätisch konsistent, Bord- vs.
  Präzisionsorbit gleicher Burst.

Nächster Baustein (Stand 2026-11-02): `ChipStats` + `chip_stats` in
`12_geoloc.rs` sind gebaut und getestet — erster Zeitreihen-Punkt Kaohsiung:
Master (IW2/Burst 9, az=267,3, rg=15305,4) Median 4,8 dB / p90 9,1 dB,
Slave (IW2/Burst 9, az=268,0, rg=15311,3) Median 5,1 dB / p90 9,3 dB —
Drift 0,4 dB über 12 Tage → Ampel Grün (Schwellen 3/6 dB, Prototyp).
Polygon-Assets (`locate_polygon`: Ecken + Raster + Punkt-im-Polygon, an
Realdaten geprüft) und Ampel-Bewertung (`bewerte_zeitreihe`) sind gebaut.
Offen: Schwellen an beschrifteten Zeitreihen kalibrieren, Wochenreport.
Erledigt: Range-Achse abgeglichen — `mosaic_column` (Treffer-`rg` =
Mosaik-Spalte, gültig in `[0, n_rg − n_chirp/4)`; an Kaohsiung-Realdaten
geprüft; Doku-Fehler `n_rg − n_chirp` in `FocusedBurst` korrigiert).
Volle Suite 13/13 + 7/7, clippy/fmt grün.
