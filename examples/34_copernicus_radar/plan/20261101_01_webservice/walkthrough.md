# Walkthrough: Vom Labor-Tool zur Asset-Website — Reverse-Geokodierung für `source2`

## 1. Worum es geht: Eine Ampel für einen Damm

Stell dir eine Betreiberin eines Tailings-Damms vor (das ist der Damm,
der die giftigen Schlämme eines Bergwerks staut — bei Brumadinho 2019
hat ein Bruch 270 Menschen getötet). Sie will keine Radarbilder
interpretieren. Sie will eine Ampel: *Bewegt sich mein Dammabschnitt
ungewöhnlich? Ja/Nein, und seit wann?*

`source2` kann heute das Bild dazu rechnen, aber als Kommandozeilen-Tool
im Labor: `.dat`-Datei rein, fokussiertes Bild plus Mosaik raus. Alles in
**Slant-Range** — also in der Geometrie des Radars, nicht der
Landkarte. Ein Pixel heißt dann (Zeile 431, Spalte 1892). Damit kann
außerhalb des Labors niemand etwas anfangen.

Dieser Walkthrough erzählt, wie aus dem Tool ein Webservice wird —
ohne den schweren Weg (jede Szene auf die Karte zu entzerren). Der
Trick steht in Kapitel 3, die Geometrie in Kapitel 5, und die Stelle,
an der eine naheliegende Annahme über TOPS-Bursts falsch war, in
Kapitel 6. Wer nur das Ergebnis will: Kapitel 7.

## 2. Was Copernicus schon liefert — und wo die Lücke ist

Zuerst die unbequeme Frage: Gibt es das nicht längst gratis?

- **EMS** (Emergency Management) zeichnet bei Flut und Erdbeben
  Schnellkarten — aber nur, wenn eine Behörde den Dienst aktiviert,
  und nur für das eine Ereignis.
- **EGMS** (Ground Motion) liefert Senkungskarten für ganz Europa —
  aber als Pixel zum Selberdeuten, und nur für Europa.

Beide liefern *Karten*. Niemand liefert die *Entscheidung*: „Dein
Dammabschnitt bewegt sich seit acht Wochen anomal — fahr hin." Genau
diese Schicht war früher Handarbeit eines SAR-Interpreten am Bildschirm.
Heute kann Software sie übernehmen: Zeitreihen stapeln, Atmosphäre von
echter Bewegung trennen, Anomalien melden. Copernicus gibt die Pixel
umsonst her; das Urteil ist das Produkt. Deshalb: **nicht mit EMS/EGMS
um Karten konkurrieren, sondern darauf die Entscheidungs-Schicht bauen.**

## 3. Die Kernidee: Das Asset ins Bild, nicht das Bild auf die Karte

Klassische Geokodierung nimmt das ganze Radarbild (Millionen Pixel) und
verzerrt es auf die Landkarte. Teuer, fehleranfällig im Gebirge, und am
Ende liegt eine Gigabyte-Datei herum.

Wir drehen die Richtung um. Die Satelliten fliegen alle 12 Tage
praktisch dieselbe Bahn — aufeinanderfolgende Bilder liegen nach dem
Ausrichten fast pixelgenau übereinander. Der Nutzer klickt also *ein
Asset auf der Karte* an (Dammabschnitt, Hang, Deponie), und wir rechnen
nur diesen einen Punkt in Radar-Koordinaten um:

```text
Landkarte                    Radar (Slant-Range)
┌──────────────┐   klick    ┌──────────────────────┐
│      ✕ Damm  │ ─────────► │ . . . . . . . . . .  │
│              │  reverse   │ . . . . . ✕ . . . .  │  ← (Burst, Zeile, Spalte)
└──────────────┘  geocode   └──────────────────────┘
      ▲                              │
      └───── Pin + Ampel ◄───────────┘  Chip + Zeitreihe
```

Pro Asset ein paar Dutzend Zahlen statt Gigabyte-Bilder. Die Website
zeigt Karte, Pin und Ampel; im Hintergrund rechnet alles in
Radarkoordinaten. Volle Karten-Entzerrung wird erst nötig, wenn ein
Kunde das Gesamtbild als Kartenebene will — also genau dann, wenn
dafür bezahlt wird.

## 4. Die Zutaten: Präzise Bahn, eine Höhe, und drei S3-Überraschungen

### 4.1 Wo der Satellit genau war: POD-Orbits

Jedes Level-0-Paket trägt die Bord-Navigationslösung des Satelliten mit
sich (GPS, ca. 1 Hz, Genauigkeit Meter-Bereich). Für eine Ampel reicht
das. Für Millimeter-Zeitreihen über Monate wird es zu Rauschen — dafür
gibt es die nachträglich am Boden bestimmten **präzisen Orbits**:
Dateien des Typs `AUX_POEORB` aus dem Copernicus Data Space. Eine
Datei ≈ 4,6 MB, 9.361 Stützstellen im 10-s-Raster.

Zwei Dinge lernt man beim ersten Download:

- **Sie kommen spät.** Das Orbitfile für unsere Slave-Aufnahme vom
  10. Oktober wurde erst am 30. Oktober veröffentlicht (~20 Tage).
  Für Wochen-Ampeln ok, für „Warnung in Stunden" nicht. Das gehört
  ehrlich ins Produkt.
- **Der S3-Endpunkt will Region `default`.** Mit der üblichen
  `us-east-1`-Signatur antwortet `eodata.dataspace.copernicus.eu` mit
  403 — die Schlüssel sind trotzdem gültig (der Fehler heißt
  `SignatureDoesNotMatch`, nicht „falscher Schlüssel"; wer den
  Fehlertext liest, spart sich eine Stunde).

Dazu kam: Das Höhenmodell GLO-30 ist mit den Schlüssel zwar im Katalog
*sichtbar*, aber nicht *ladbar* (403), und der in mancher Doku genannte
AWS-Spiegel existiert nicht mehr (`NoSuchBucket`). Ausweg: SRTM-Höhen
(30 m) liegen offen im selben Speicher. Für Punktabfragen völlig
ausreichend — wir brauchen für ein Asset ohnehin nur *eine* Höhe, kein
Höhenbild.

### 4.2 Eine Höhe pro Punkt

Die SRTM-Kachel ist eine simple Datei: 3601×3601 Höhenwerte, zusammen
25 MB pro Grad-Quadrat. Plausibilitätscheck nach dem Download:
Kaohsiung (Hafenstadt) 21 m, Zentralgebirge 2.837 m, Nähe Yushan
3.809 m — passt (der Yushan ist mit 3.952 m Taiwans höchster Berg).

## 5. Geometrie in drei Schritten

Gegeben: ein Klick bei 22,62° N, 120,31° O (Kaohsiung), Höhe 21 m aus
der SRTM-Kachel. Gesucht: In welchem Burst, an welcher Stelle?

**Schritt 1 — Auf die Erdkugel.** Aus Breite/Länge/Höhe wird per
WGS-84-Formel ein Punkt im Raum (ECEF-Koordinaten, Einheit Meter).
Reine Schulmathematik, in beide Richtungen getestet.

**Schritt 2 — Wann war der Satellit genau gegenüber?** Ein Radarpunkt
wird in dem Moment abgebildet, in dem die Verbindungslinie Satellit→Punkt
senkrecht zur Flugrichtung steht (*Zero-Doppler-Moment*). Diese Zeit
findet ein Newton-Verfahren: Starte mit einer Schätzung, messe den
Fehler („wie schräg steht die Linie noch?"), korrigiere, wiederhole.
Wenige Runden genügen. Das Schöne: Dieses Verfahren existierte im Code
schon — die InSAR-Koregistrierung nutzt es, um Master-Punkte in der
Slave-Aufnahme wiederzufinden. Wir verwenden es nur mit einem anderen
Startpunkt wieder.

**Schritt 3 — Von Zeit und Entfernung zu Pixel.** Aus dem
Zero-Doppler-Moment folgt die Zeile, aus der Schrägentfernung
(bei Kaohsiung 887,8 km) die Spalte. Klingt trivial — und genau
hier lauerte der Fehler.

## 6. Der TOPS-Aha-Moment: Die Zeit lügt

Erster Test: Kaohsiung wird *nicht gefunden*. Die Rechnung liefert eine
saubere Zero-Doppler-Zeit — aber sie fällt in eine **Lücke zwischen
zwei Bursts**. Zwischen Burst 8 (endet um t) und Burst 9 (beginnt
1,7 s später) schien der Punkt in keinem Bild zu liegen. Unmöglich —
er liegt mitten in der Szene.

Die Auflösung ist die TOPS-Mechanik: Bei IW/EW schwenkt die Antenne
während eines Bursts aktiv mit. Dadurch bildet ein Burst Bodenpunkte ab,
deren Zero-Doppler-Zeiten **bis zu ±1,4 s außerhalb seines eigenen
Pulsfensters** liegen. Die fokussierte Bildachse eines Bursts ist
entsprechend länger als seine Rohdaten: nicht 1.548 Zeilen, sondern bis
zu 4.096 (im Code `n_az_fft`). Wer — wie unser erster Versuch — mit dem
Rohfenster prüft, verwirft echte Treffer.

Nach der Korrektur (Trefferachse = fokussierte Achse um die Burst-Mitte)
sitzt Kaohsiung in **Sub-Swath IW2, Burst 9**. Zweite Falle gleich
danach: Der Einfallswinkel kam als 139° heraus — das Komplement.
Gemeint ist der Winkel zur Vertikalen in Blickrichtung *zum*
Satelliten: 180° − 139,4° = **40,6°**, exakt der Sollbereich für IW2
(29–46°).

## 6b. Die Stauchung: Warum 267 trotzdem falsch war

Der Chip bei Zeile 267 sah plausibel aus (5 dB, stabile Drift) — und
war trotzdem 650 Zeilen daneben. Der Hinweis steht in der Doku von
`deburst_swath`: Eine fokussierte Zeile entspricht nicht einer PRI,
sondern `dt = PRI/α` wahrer Zeit, mit dem Stauchungsfaktor
α = |k_a/(k_a − k_t)|. Das Deramping beim Fokussieren staucht die
Achse; wer mit PRI rechnet, schätzt den Abstand zur Burst-Mitte zu
groß. Gemessen an IW2/Burst 9: kt = +4629 Hz/s, α = 0,310 — die wahre
Zeile ist **1496,5** statt 267,0, also rund 5 km weiter.

Die Lehre in einem Satz: Die Ortung liefert Burst-Index und
Zero-Doppler-Zeit exakt (dafür braucht sie kein kt); die Zeile im
fokussierten Bild erst der Fokus-Schritt (dafür misst er kt). Unser
`AssetHit.az` ist deshalb ehrlich dokumentiert als α=1-Näherung für
die Burst-Auswahl, und `focused_line` rechnet nach dem Fokussieren
exakt um. Die Chips liegen jetzt an wahrer Stelle (beide ≈ 62 dB,
Drift 0,3 dB → Grün), und ein Konsistenztest vergleicht unser
nachgebautes Mosaik-Raster bit-exakt mit dem echten `deburst_swath` —
damit eine künftige Mosaik-Änderung nicht unbemerkt alles verschiebt.

## 7. Was jetzt da ist (Behauptung → Beweis)

Gebaut wurde `source2/src/12_geoloc.rs` mit drei Bausteinen:

- **EOF-Parser**: liest präzise Orbitfiles ohne XML-Bibliothek
  (Tag-Scan; Achtung, `<X unit="m">` trägt Attribute — erster Bug).
- **SRTM-Abfrage**: Höhe eines Punkts aus einer `.hgt`-Kachel (bilinar).
- **`locate_asset`**: Kartenpunkt → (Sub-Swath, Burst, Zeile, Spalte,
  Einfallswinkel, grobe Layover-Warnung bei < 25°).

Geprüft an echten Daten (Master-Aufnahme Süd-Taiwan). Die Beweise:

1. **Rundreise schließt sich:** Treffer (Zeile, Spalte) → zurück auf die
   Erdkugel → geodätisch: landet wieder bei Kaohsiung (Testschranke
   0,002° ≈ 200 m).
2. **Orbit-Quellen sind austauschbar:** Bord-Ephemeriden und präzises
   Orbitfile liefern denselben Burst und nahe Pixel. Dieser Test fängt
   Epochen- und Schaltsekunden-Fehler in der Zeitumrechnung ab (GPS-Zeit
   = Unix-Zeit − 315.964.782 s — einmal falsch, und alles ist um
   Kilometer daneben).
3. **Nichts kaputt gemacht:** Gesamte `source2`-Suite 12/12
   Integrationstests + 2/2 Einheitentests grün, `clippy -D warnings`
   und `fmt --check` sauber. (Zum Bauen braucht es `cargo-oxide`;
   ohne das Werkzeug linkt in diesem Container kein einziges Binary.)

## 8. Was noch fehlt — ehrlich

- **Zeitreihe + Ampel:** Der Trefferpunkt ist erst die halbe Miete. Der
  erste Schritt ist getan: `chip_stats` schneidet je Aufnahme einen
  Ausschnitt (±16×±20 Pixel) um den Treffer aus und fasst die Leistung
  zusammen (Kaohsiung: beide Chips ≈ 62 dB an wahrer Zeile ≈ 1496 —
  Drift 0,3 dB über 12 Tage → Ampel Grün bei Schwellen 3/6 dB).
  Noch offen: Schwellen an beschrifteten Zeitreihen kalibrieren und
  der Wochenreport.
- **Polygone statt Punkte:** Erledigt — ein Damm ist eine Linie, also
  tastet `locate_polygon` Ecken plus Raster ab (Punkt-im-Polygon-Test
  inklusive) und ortet jeden Punkt. An Kaohsiung-Dreiecken in beiden
  Aufnahmen geprüft.
- **Range-Achse abgeglichen:** Erledigt — das Mosaik übernimmt die
  Spalten linksbündig (`[0, n_rg − n_chirp/4)`), also ist die
  Mosaik-Spalte identisch zum Treffer-`rg`; `mosaic_column` prüft nur
  noch die Gültigkeit (an Realdaten: Kaohsiung bei 15305,4 tief im
  gültigen Bereich).
- **Layover richtig:** Heute Faustregel über den Einfallswinkel.
  Korrekt braucht es die Hangneigung aus dem Höhenmodell entlang der
  Blickrichtung.
- **Range-Achse abgleichen:** Die fokussierte Range-Richtung schneidet
  einen Wrap-Rand ab; Treffer-Spalte und Bild-Spalte sind noch nicht
  deckungsgleich dokumentiert.

Der Weg bleibt der aus Kapitel 3: Detektion und Ampel in
Radarkoordinaten bauen. Die Karte kommt erst, wenn jemand dafür zahlt.
