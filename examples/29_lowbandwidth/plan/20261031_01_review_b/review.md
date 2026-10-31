# Review: `source10_instrumentation` (Stand 2026-10-31)

Gegenstand: der aktuell beste Stand des Low-Bandwidth-Remote-Desktops
(`source10_instrumentation`, Protokoll v3, Leaky-Bucket-Pacing aus
`plan/20261017_01_bunching`). Gelesen wurden alle Pläne/Walkthroughs in
`plan/` sowie der komplette Code (`common/`, `server/`, `client/`, `log/`,
~9,8 k Zeilen inkl. Tests). Alle Zahlen unten sind **nachgemessen** in
diesem Container (AMD Threadripper PRO 7955WX, 32 Threads, **kein
CUDA-Treiber** — `libcuda.so` fehlt, `nvidia-smi` fehlt).

## 0. Messgrundlage

| Messung | Ergebnis |
|---|---|
| `cargo test --workspace` (Baseline) | 105 passed, 0 failed (nach `apt install libxcb-randr0-dev …`) |
| `lbw-logstat 20261018_0839.lbwlog` (Server, 321 s, **nach** Pacing) | s. §2 |
| `lbw-logstat 20261017_0614.lbwlog` (Referenz-Log des Bunching-Plans) | **Fehler**: `UnexpectedEnd { additional: 4 }` |
| Modelltest `LBW_CPU=1 … --test models -- --ignored` | det **107 ms** warm (CPU, 8 Threads), rec 0,7 ms warm (Cache) |

Die letzte Zeile ist wichtig für die neue Implementierung: Auf dieser CPU
kostet DBNet für 1280×736 nur ~1,5× der GPU-Zeit aus dem Log (det p50
69 ms). Eine **CPU-only**-Variante ist also ohne Funktionsverlust möglich
und hier — anders als CUDA — vollständig testbar.

## 1. Was gut ist (bleibt erhalten)

- **Architektur-Kern:** OCR-Text als Vektor (40–100 B/Zeile) statt Pixel,
  Maskierung der Textboxen, genau eine AV1-Still-Box pro Frame, v3-Delta
  mit stabilen IDs. Das ist der Grund, warum der Desktop über 6 kB/s
  überhaupt benutzbar ist.
- **Instrumentierung:** `.lbwlog` mit Mono+Wall-Stempel, `Frame`-Records
  auch bei Stille, Gap-Events, Replay durch echten Decoder. Ohne diese
  Daten wäre dieses Review nicht möglich.
- **Robustheit:** Reader-Thread als Abriss-Signal, entkoppelte Injektion,
  Stale-Input-Flush beim Reconnect, Recorder schaltet bei Plattenfehler
  still ab.
- **Code-Hygiene:** nummerierte Dateien < 600 Zeilen, `clippy -D warnings`
  sauber, wenige Dependencies.

## 2. Befunde (nach Schwere)

### B1 — Monolithische Kacheln blockieren den Kanal (kritisch)

Das Pacing (Leaky Bucket) begrenzt die *mittlere* Rate, nicht die Größe
einer einzelnen Nachricht. Aus `20261018_0839.lbwlog` (mit Pacing!):

- Kachelgrößen: p50 80 B, p90 10 475 B, **max 42 448 B**.
- Top-Sekunden: `s+59: 42458 B/s srv, 0 B/s cli`, `s+112: 39544 B/s`, …
- `pace` max **6 104 ms**, p99 964 ms.

Eine 42-kB-Kachel belegt einen 6-kB/s-Kanal **7 s am Stück**. Alles, was
danach in denselben TCP-Strom geschrieben wird (Text-Echo!), steht
dahinter (*Head-of-Line-Blocking*). Der Pacer wartet zwar *vor* dem
Schreiben, aber sobald geschrieben ist, gehört der Kanal der Kachel.

### B2 — Die Session-Schleife steht während des Pacings (kritisch)

`wait_paced` schläft **in der Capture/OCR-Schleife**. Während eine große
Kachel auf Kanalzeit wartet (bis 6 s), gibt es keinen Grab, keine OCR,
keine Text-Deltas. Die im Bunching-Plan versprochene „Vorfahrtspur für
Texte" existiert nur zwischen zwei Frames, nicht während des Wartens.
Folge im 0839-Log: Text-Echo-Latenzen im Sekundenbereich (s. B4 für die
Einschränkung der Messung).

### B3 — Encode, obwohl der Kanal belegt ist (mittel)

Jede Änderung wird sofort AV1-kodiert (encode p99 310 ms, max 588 ms) —
auch wenn die Kachel danach Sekunden warten muss. Bis sie gesendet wird,
ist sie veraltet; gleichzeitig blockiert der Encode die OCR. Richtig wäre:
erst kodieren, wenn der Kanal frei ist, und dann den *aktuellen* Stand
(Differenz zum zuletzt **gesendeten** Bild, nicht zum letzten Frame).

### B4 — Latenz-Metrik in `logstat` ist ein Artefakt (hoch, Analyse)

`summarize_file` matcht Eingaben per FIFO **1:1** gegen Echos. Eine
einzige Kachel beantwortet aber typischerweise viele Eingaben (ein Wort
tippen = 5 Tasten, ein Echo). Die Warteschlange unbeantworteter Eingaben
wächst daher monoton; jede spätere Messung paart sich mit uralten
Eingaben. Ergebnis im 0839-Log: **Input→Kachel p50 47 515 ms** — physikalisch
unsinnig bei einem Kanal, der 30 von 126 s über Budget war. Zusätzlich
zählen `Key{down:false}` und `Button{down:false}` als eigene Eingaben.
Korrekt ist „Erst-Echo": Jede wartende Eingabe wird vom *nächsten* Echo
beantwortet (viele-zu-eins), nur `down`-Ereignisse zählen, mit Timeout
für „keine sichtbare Reaktion".

### B5 — Referenz-Log nicht mehr lesbar (mittel)

Der Bunching-Commit hat `FrameMs.pace` angefügt und `LOG_VERSION` auf 3
gesetzt — ohne Lesepfad für Version 2. `20261017_0614.lbwlog`, die
Messung, auf der der ganze Bunching-Plan beruht, ist damit nicht mehr
auswertbar (`UnexpectedEnd`). Für eine datengetriebene Entwicklung ist
das ein Verlust: Vorher/Nachher-Vergleiche sind unmöglich. Ein
versionsabhängiger Decoder kostet ~20 Zeilen.

### B6 — Maus-Spam ungedrosselt (niedrig)

883 `MouseMove` (8,4 kB) im 0839-Log, Peak 278/min; der Client schickt
jede Pixelbewegung pro UI-Frame. Für den Downstream irrelevant, aber im
geteilten SSH-Tunnel konkurrieren die Bytes mit Tasten. Zusammenfassen
aufeinanderfolgender Moves (nur letzte Position, Reihenfolge zu Klicks
bleibt) ist 10 Zeilen.

### B7 — CUDA ist hier nicht testbar (Umgebung)

`ort` mit Feature `cuda`, `Provider::Auto` mit CPU-Fallback zur Laufzeit.
Ohne Treiber im Container ist der CUDA-Pfad nicht verifizierbar; die
Messung oben zeigt, dass CPU ausreicht. Vorgabe für die neue Version:
**CPU-only** (Feature `cuda` raus, `Provider` raus, `--cpu` raus).

### B8 — Kleinkram (niedrig)

- Wiederholte Kleinst-Kacheln: `tile@1264,117 87B` 9× identisch
  (blinkendes Element am rechten Rand) — 1,4 % Tile-Dedup-Verlust,
  lohnt aktuell keinen Cache.
- Text-Dedup 16 % (Scroll = Remove + Add); gemessen am Gesamtvolumen
  (Texte 54 kB vs. Kacheln 619 kB) zweitrangig.
- `queue_peak` rechnet sekundengenau (s. Plan-Review 20261010) — für
  echte Zustell-Verzögerung je Nachricht fehlt eine Kanal-Simulation.

## 3. Bewertung

| Aspekt | Note | Begründung |
|---|---|---|
| Datenrate im Mittel | gut | Pacing hält den Schnitt (0839: Queue 42 kB statt 2 MB im 0614) |
| Eingabe-/Text-Latenz unter Last | **mangelhaft** | B1 + B2: Sekunden-Blockaden durch Einzelkacheln und stehende Schleife |
| Messwerkzeug | befriedigend | Format top, Latenz-Metrik falsch (B4), Altlog-Verlust (B5) |
| Code | gut | kompakt, sauber; Pacing-Logik an falscher Stelle (Schleife statt Sender) |

## 4. Empfehlungen (→ `plan.md`)

1. **Sender-Thread mit zwei Spuren**: Text sofort, Kacheln gepacet; die
   Capture/OCR-Schleife blockiert nie.
2. **Kachel-Obergrenze** (`--max-tile`, Default 2500 B ≈ 0,4 s Kanal):
   Dirty-Box in horizontale Streifen zerlegen, zu große Streifen
   rekursiv halbieren — keine Nachricht belegt den Kanal länger als
   ~0,5 s.
3. **Encode nur bei freiem Kanal**, Diff gegen das zuletzt *gesendete*
   Bild (Schattenbild) — keine veralteten Kacheln, weniger CPU.
4. **Maus zusammenfassen** im Client.
5. **logstat**: Erst-Echo-Metrik, Kanal-Simulation je Nachricht
   (Text-Zustellung p50/p90/max), Version-2-Logs wieder lesbar.
6. **Prüfstand mit echten Daten**: Bildfolge + Texte aus
   `20261018_0839.lbwlog` rekonstruieren, durch die neue Session über
   einen 6-kB/s-Drosselleser schicken, Kennzahlen messen.
7. **CPU-only**.
