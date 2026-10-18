# LBW-v4-Dissector für Wireshark (C-Plugin, kein Lua)

Warum C statt Lua: Der Dissector läuft pro TCP-Segment — bei Kachel-Sessions
mit tausenden Nachrichten zählt jede Millisekunde, und das Plugin lädt ohne
Lua-Laufzeit in Wireshark wie tshark. Preis: Bauen gegen die installierten
Wireshark-Header (ABI-Prüfung — Plugin und Wireshark müssen dieselbe
Major.Minor-Version haben).

## Bauen und installieren

```sh
sudo apt install tshark libwireshark-dev cmake
cd source11_compact/wireshark
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

## Wo mitschneiden (keine Entschlüsselung nötig)

Die LBW-Daten laufen an beiden Tunnel-Enden **unverschlüsselt über
localhost** (SSH verschlüsselt nur die Strecke dazwischen):

- Server-Host: Interface `lo`, Port 7878 filtern.
- Client-Host: russh-Strom läuft prozessintern — für Live-Analyse den
  Client mit `--tap 127.0.0.1:<port>` starten (Debug-Spiegel, Default aus).
- SSH selbst wird nicht entschlüsselt (weder OpenSSH noch russh geben
  Session-Keys heraus).

Vom Auth-Token (`Challenge`/`Auth`) zeigt der Dissector nur Längen
(`lbw.auth.nonce_len`, `lbw.auth.mac_len`), nie Geheimnisse.

## Was er zeigt

Alle 14 v4-Varianten beider Richtungen (Challenge, Hello, ClearText,
NewText mit ID/Box/Farben/Text, MoveText mit ID/Box, RemoveText,
Tile mit Position/Größe/AV1-Bytes, Auth, Hello, MouseMove, Button, Text,
Key) inkl. TCP-Reassembly (Nachrichten über Segmentgrenzen). Unbekannte
Varianten/Restbytes erscheinen als `lbw.raw` (Protokoll-Erweiterungen
bleiben sichtbar), Fragmente als Expert-Info.

## Dateien

- `packet-lbw.c` — der Dissector (~500 Zeilen, bounds-geprüft).
- `CMakeLists.txt` — Out-of-Tree-Build (fragt die Wireshark-Version ab).
- `gen_sample.py` — erzeugt `sample.pcap` (stdlib-only, deterministisch):
  alle Varianten + Kachel über zwei Segmente gesplittet. Body-Bytes sind
  per `encode_msg` verifizierte Vektoren
  (`../common/examples/vecdump.rs`) — bei Protokolländerung hier und im
  Dissector nachziehen.
- `sample.pcap` — committedes Beispiel (16 Pakete).
- `check.sh` — baut und behauptet alle 14 Varianten per tshark
  (Exit 2 ohne Toolchain, analog zu den Modell-Checks der Smokes).
