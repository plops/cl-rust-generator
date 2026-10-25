## Antwort

Die drei komplexesten/algorithmisch anspruchsvollsten Teile von eSpDR sind: (1) die PLL-Abstimmung mit Kondensator-Scan in `tune_pll()`, (2) die Sentinel-basierte Bankgrenzen-Erkennung für die kontinuierliche SRAM-Erfassung, und (3) der adaptive Congestion-Control-Algorithmus für den Live-Spektrum-Stream im Webserver.

---

### 1. PLL-Frequenzplanung + Kondensator-Scan (`tune_pll`)

Dieser Algorithmus macht die eigentliche Funkabstimmung überhaupt erst möglich. Zuerst berechnet `esp32s3_plan_lo()` aus der gewünschten LO-Frequenz das 24-Bit-Sigma-Delta-Wort, wobei je nach Modus (normal oder 5/6-Konversion) ein anderer Referenzschritt verwendet wird [1](#0-0) .

Danach übernimmt `tune_pll()` in der Firmware: Es programmiert das SDM-Wort, wartet auf die PLL-eigene Kalibrierung und scannt anschließend **alle 512 VCO-Kondensator-Codes**, um den längsten zusammenhängenden "locked"-Bereich zu finden und dessen Mitte zu wählen [2](#0-1) . Das ist nötig, weil es keine feste Frequenz-zu-Kondensator-Tabelle gibt — die Firmware muss den funktionierenden Bereich jedes Mal empirisch per Scan finden, wie auch in der Doku beschrieben [3](#0-2) . Die 5/6-Konversion erweitert diesen Mechanismus noch um eine zweite PLL-Koordinatenebene, siehe `docs/LO-EXTENSION.md` [4](#0-3) .

Dieser Algorithmus ist zentral, weil ohne verlässliches PLL-Lock keine Abstimmung auf eine Frequenz möglich wäre — er bildet die Grundlage der gesamten "SDR"-Funktionalität.

### 2. Sentinel-basierte Bankgrenzen-Erkennung

Beim 80-Msps-Capture läuft der Dump-Engine kontinuierlich in SRAM-Bänke, während die CPU-Kerne parallel Daten versenden. Das Problem: der exakte Zeitpunkt, wann ein Bank-Wechsel tatsächlich stattgefunden hat, ist durch eine Pipeline-Verzögerung unklar. Die Lösung ist ein cleverer Trick: Die Firmware pflanzt "Sentinel"-Wörter an den erwarteten Start-/End-Positionen, bevor die nächste Bank übergeben wird, und sucht danach, wo echte Samples diese Sentinels überschrieben haben — das liefert die exakte Paarzahl und Grenze jeder Übertragungseinheit [5](#0-4) .

Dieser Mechanismus ist entscheidend, weil er bei 320 MB/s kontinuierlichen Datenstrom ohne Verlust ermöglicht — die vier SRAM-Bänke rotieren so, dass eine Bank weiter aufzeichnet, während eine andere ausgelesen wird [6](#0-5) . Ohne diesen Algorithmus gäbe es keine verlustfreie, lückenlose Sample-Erfassung in Echtzeit.

### 3. Adaptive Congestion-Window für den Live-Spektrum-Stream

Im Host-Tool `iqstream serve` steuert ein TCP-ähnlicher Congestion-Control-Algorithmus, wie viele Bytes an den Webbrowser "in flight" sein dürfen. Die Kernlogik in `acknowledge()` misst die Round-Trip-Time (RTT), vergleicht sie mit der minimalen beobachteten RTT, und passt das Fenster an: wächst additiv/quadratisch bei niedriger Warteschlangenverzögerung, schrumpft multiplikativ (um 30%) bei Stau [7](#0-6) .

Ergänzt wird das durch `try_send()`, das das Fenster tatsächlich durchsetzt und periodisches "Probing" (alle 10s ein einzelner Frame) nutzt, um die Leerlauf-RTT neu zu kalibrieren [8](#0-7) . Zusätzlich passt `adapt_resolution()` die Auflösung (Bins pro Frame) automatisch an die tatsächlich ankommende Framerate an [9](#0-8) .

Dieser Algorithmus ist das, was die Web-Oberfläche auch bei langsamen/instabilen Verbindungen flüssig hält, ohne dass sich ein Rückstau aufbaut — essenziell für die "live spectrum and waterfall"-Funktion des Projekts [10](#0-9) .

## Notes

- Es gibt weitere algorithmisch interessante Teile (z.B. der SIMD-Packing-Kernel in `transmit.S`, der 16 IQ-Paare in 40 Bytes packt, und die FPGA-seitige Deskew/Reorder-Logik), die aber im Index nicht im Volltext vorliegen — falls du genauere Details dazu brauchst, empfehle ich eine Devin-Session zu starten, um den vollständigen Code einzusehen [11](#0-10) .
- Die drei ausgewählten Algorithmen stammen aus unterschiedlichen Teilsystemen (ESP32-Firmware vs. Host-Webserver), sind aber durch die gemeinsame Pipeline (Funkempfang → Capture → Stream → Browser) sachlich verbunden.

Wiki pages you might want to explore:
- [Glossary (h0m3us3r/eSpDR)](/wiki/h0m3us3r/eSpDR#8)
