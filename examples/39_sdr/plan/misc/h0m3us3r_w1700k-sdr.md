## Antwort

Basierend auf dem verfügbaren Kontext lassen sich drei zentrale algorithmische Kernstücke identifizieren: (1) die Slicing/Packing-Logik für die 10GbE-Übertragung, (2) der Reorder-/Scheduling-Algorithmus im `Publisher`, und (3) die "Flatten"-Korrektur (inverser Kaiser-FIR-Filter per Overlap-Save-FFT).

---

### 1. Slicing & Packing (Firmware-seitig, 28-Bit-Kompression)

Um die volle Abtastrate von 320 Msps über die 10GbE-Verbindung zu bekommen, müssen WM- und WA-Prozessor zusammenarbeiten, um die Hälfte der 8-KiB-Slices verlustfrei auf 7 KiB zu packen: das 8. Sample eines Achter-Blocks wird in die ungenutzten Bits der vorherigen sieben Samples codiert [1](#0-0) . Dies ist notwendig, weil die Rohdaten (14-Bit I/Q) sonst die Bandbreite der Verbindung übersteigen würden. Das Entpacken auf Empfängerseite erfolgt im `Publisher` mittels Bitmasken-Extraktion aus 32-Bit-Wörtern: [2](#0-1) 

Dieser Algorithmus macht die Software zu dem, was sie ist, weil er die physikalische Limitierung (Link-Bandbreite vs. Abtastrate) überhaupt erst lösbar macht — ohne ihn wäre 320 Msps auf 10GbE gar nicht möglich [3](#0-2) .

### 2. Reorder- & Scheduling-Algorithmus (`Publisher::schedule`)

Der komplexeste Algorithmus im Host-Code ist wohl der Scheduler, der die eintreffenden (möglicherweise außer Reihenfolge, verspätet oder verlorenen) UDP-Completions in eine geordnete, lückenlos getaggte Shared-Memory-Ring-Struktur überführt [4](#0-3) .

Kernideen:
- Ein Slot gilt als "resolved", sobald seine Completion da ist **oder** `kHorizon` (1024) neuere Slots eingetroffen sind, ohne dass dieser Slot kam — dann gilt er als verloren [5](#0-4) .
- Jobs werden in Chunks (`kChunk=16`) an einen Worker-Pool verteilt und später per `done_`-Map wieder in Sequenzreihenfolge zusammengeführt, bevor sie via `publish_position` (Seqlock) sichtbar werden [6](#0-5) .
- Overrun-Erkennung: ein verlorener Slot, dessen Platz im Empfangsring bereits durch eine spätere Completion überschrieben wurde, zählt separat als `overrun` statt als reiner Netzwerkverlust [7](#0-6) .

Dieser Algorithmus ist entscheidend, weil er die Echtzeit-Garantie des gesamten Systems trägt: Ohne ihn gäbe es keine konsistente, zeitlich geordnete IQ-Stream-Ausgabe im Shared Memory, auf der `w1700k-spectrum` und `sdr-recv` aufbauen [8](#0-7) .

### 3. "Flatten"-Korrektur (Overlap-Save-FFT mit inversem Kaiser-FIR)

Die Dezimation jeder Abtastrate dämpft die Bandränder (bis zu -22 dB bei 99% der Nyquist-Frequenz); das wird durch einen 129-Tap linearphasigen FIR-Filter (Kaiser-Fenster) korrigiert, dessen Frequenzgang invers zur gemessenen Rolloff-Kurve ist, angewendet per Overlap-Save-FFT über Slice-Grenzen hinweg [9](#0-8) .

Implementiert wird das im `Publisher::process`, wo für jeden Job die Samples inkl. `kDelay`-Padding vor/nach dem Chunk gesammelt werden (Events werden übersprungen, verlorene/noch nicht angekommene Slices als Nullen behandelt), bevor `w.flatten.apply(...)` aufgerufen wird [10](#0-9) . Das Ergebnis wird anschließend gerundet, auf int16 geclippt (mit Sättigungszählung) und mit dem Tag `W1700K_STREAM_TAG_FLAT` markiert [11](#0-10) .

Dieser Algorithmus ist das, was das Produkt visuell/qualitativ auszeichnet: Er sorgt dafür, dass das Spektrum über die gesamte Bandbreite (z.B. das komplette 2,4-GHz-Band) flach und ohne Rolloff-Verzerrung dargestellt wird, was explizit als Kernmerkmal im README hervorgehoben wird [12](#0-11) .

---

## Notes

- Eine weitere Kandidatin wäre die MT7996-Firmware-Initialisierung ("Register für Register gegen einen Trace von MediaTeks eigener Firmware decodiert") [13](#0-12) , aber dafür liegen mir keine konkreten Code-Snippets im Index vor — falls du dazu Details brauchst, empfehle ich eine Devin-Session mit vollem Repo-Zugriff auf `firmware/`.
- Der `airoha_bmt_transaction.c`-Treffer zu NAND-Block-Management ist thematisch nicht mit dem SDR-Signalpfad verbunden und daher hier nicht berücksichtigt [14](#0-13) .
- Die adaptive Bitraten-/FPS-Steuerung im WebSocket-Server (`try_send`, `acknowledge`, `adapt_resolution`) ist ebenfalls algorithmisch interessant (TCP-ähnliche Fenstersteuerung), aber eher eine Netzwerk-Optimierung als ein Kern-Signalverarbeitungsalgorithmus [15](#0-14) .

Wiki pages you might want to explore:
- [End-to-End Signal Path (h0m3us3r/w1700k-sdr)](/wiki/h0m3us3r/w1700k-sdr#1.1)
- [Publisher and Shared Memory Ring (h0m3us3r/w1700k-sdr)](/wiki/h0m3us3r/w1700k-sdr#4.2)
