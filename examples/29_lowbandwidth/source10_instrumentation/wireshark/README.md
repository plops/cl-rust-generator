# LBW-Dissector für Wireshark (C-Plugin, kein Lua)

Warum C statt Lua: Der Dissector läuft pro TCP-Segment — bei Kachel-Sessions
mit tausenden Nachrichten zählt jede Millisekunde, und das Plugin lädt ohne
Lua-Laufzeit in Wireshark wie tshark. Preis: Bauen gegen die installierten
Wireshark-Header (ABI-Prüfung — Plugin und Wireshark müssen dieselbe
Major.Minor-Version haben).

## Bauen und installieren

```sh
sudo apt install tshark libwireshark-dev cmake
cd source10_instrumentation/wireshark
cmake -S . -B build && cmake --build build       # → build/lbw.so
V="$(pkg-config --modversion wireshark | cut -d. -f1,2)"
mkdir -p ~/.local/lib/wireshark/plugins/"$V"/epan
cp build/lbw.so ~/.local/lib/wireshark/plugins/"$V"/epan/
```

Hinweis: Als root ignoriert (t)shark User-Plugins (Security) — dort nach
`/usr/lib/x86_64-linux-gnu/wireshark/plugins/"$V"/epan/` installieren oder
als normaler User arbeiten. `./check.sh` prüft alles automatisch (baut,
zerlegt `sample.pcap`, behauptet alle Varianten per tshark-Feld).

Danach zerlegt Wireshark/tshark alles auf TCP-Port 7878 automatisch
(`lbw.msg`-Spalte, Filter `lbw`, Felder s. Quellkopf). Anderer Port:
Einstellungen → Protocols → LBW → Server-Port, oder Analyse → „Decode As“
→ TCP-Port → `lbw`. Richtung: Quellport == Server-Port → Server→Client
(Fallback: niedrigerer Port ist der Server).

## Was er zeigt

Alle 10 v3-Varianten beider Richtungen (Hello, ClearText, AddText mit
ID/Box/Farben/Text, Tile mit Position/Größe/AV1-Bytes, RemoveText,
MouseMove, Button, Text, Key) inkl. TCP-Reassembly (Nachrichten über
Segmentgrenzen). Unbekannte Varianten/Restbytes erscheinen als `lbw.raw`
(Protokoll-Erweiterungen bleiben sichtbar), Fragmente als Expert-Info.

## Dateien

- `packet-lbw.c` — der Dissector (~350 Zeilen, bounds-geprüft).
- `CMakeLists.txt` — Out-of-Tree-Build (fragt die Wireshark-Version ab).
- `gen_sample.py` — erzeugt `sample.pcap` (stdlib-only, deterministisch):
  alle Varianten + Kachel über zwei Segmente gesplittet. Body-Bytes sind
  per `encode_msg` verifizierte Vektoren — bei Protokolländerung hier
  und im Dissector nachziehen.
- `sample.pcap` — committedes Beispiel (13 Pakete, ~1 KB).
- `check.sh` — baut und behauptet alle 10 Varianten per tshark
  (Exit 2 ohne Toolchain, analog zu den Modell-Checks der Smokes).
