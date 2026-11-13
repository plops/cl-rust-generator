was sind die drei wichtigsten/kompliziertesten algorithmen in diesem repo?
wie machen sie die software zu dem was sie ist?

## Kurze Antwort

Dieses Repository ist eine Workshop-Firmware für einen Nanosatelliten (AeroRust/nanosat-workshop) und enthält keine klassischen "komplizierten" Algorithmen (z.B. Sortieralgorithmen, Pathfinding, ML). Die anspruchsvollsten/zentralsten Logikblöcke sind eher Signalverarbeitung/Sensorfusion und asynchrone State Machines für Peripherie-Kommunikation. Die drei wichtigsten sind:

1. **Median-Filterung von ADC-Messwerten** (`calculate_median`)
2. **BNO055 IMU-Kalibrierung & Quaternion-basierte Lagebestimmung**
3. **BMP388 Höhenmessung mit Kalibrierung auf Meereshöhe**

---

## Details

### 1. Median-Filter für Sensordaten (`protocol` crate)

Die Funktion `calculate_median` sortiert einen `heapless::Vec<u16, N>` und berechnet den Median (mit Spezialbehandlung für leere/einzelne Listen sowie gerade/ungerade Längen), ohne Heap-Allokation — wichtig für `no_std`-Embedded-Umgebungen [1](#0-0) . Dies dient dazu, Rauschen aus ADC-Messungen (z.B. Batteriespannung) herauszufiltern, bevor Werte weiterverarbeitet werden [2](#0-1) . Das macht die Software robuster gegenüber verrauschten Analogwerten, was für zuverlässige Telemetrie essenziell ist.

### 2. BNO055 IMU-Kalibrierung und Lagebestimmung (`onboard-computer/pico-w`)

Der `run_imu`-Task muss den BNO055-Sensor erst vollständig kalibrieren, bevor sinnvolle Daten geliefert werden — ein iterativer Polling-Loop, der wiederholt `is_fully_calibrated()` und `get_calibration_status()` abfragt, bis das Gerät bereit ist [3](#0-2) . Das Kalibrierungsprofil wird anschließend persistent im Flash gespeichert, um wiederholtes Kalibrieren beim Neustart zu vermeiden [4](#0-3) . Danach liest die Haupt-Loop fortlaufend Quaternion-, Gyroskop-, Beschleunigungs- und Schwerkraftdaten aus und verpackt sie in `protocol::SendMessage::ImuData` [5](#0-4) . Quaternionen werden statt Euler-Winkeln verwendet, um einen bekannten BNO055-Bug zu umgehen — eine bewusste algorithmische Entscheidung zur Lagebestimmung des Satelliten [6](#0-5) .

### 3. BMP388 Höhen-Kalibrierung (Barometer)

Beim ersten Durchlauf wird die aktuelle Höhe als "Meereshöhe"-Referenz kalibriert (`calibrated_absolute_difference`), danach werden relative Höhenwerte gegen diese Referenz berechnet [7](#0-6) . Die resultierenden Druck-/Temperatur-/Höhendaten werden anschließend per WiFi (sofern verbunden) oder per Debug-UART weitergeleitet [8](#0-7) .

### Ehrenwerte Erwähnung: WiFi/UDP-Netzwerkstack (CYW43)

Technisch anspruchsvoll, aber eher "Integrations-" als "Algorithmus"-Arbeit: Der `run_radio`-Task baut die CYW43-WiFi-Verbindung auf, verwaltet DHCP, und verwendet `embassy_futures::select::select` um parallel Senden und Empfangen über UDP-Sockets zu handhaben [9](#0-8) .

## Notes

- Dies ist eine Lern-/Workshop-Firmware (sichtbar an den `docs/Exercises.md`-Übungen [10](#0-9) ), keine Produktionssoftware mit komplexen Algorithmen wie Verschlüsselung, Pfadplanung oder Kalman-Filtern — zumindest nicht sichtbar im indizierten Code.
- Es gibt auch eine `power-system`-Komponente (ESP32-C3) mit UART-Kommunikation [11](#0-10) , die aber eher einfache I/O-Logik als "komplizierte Algorithmen" enthält.
- Falls du nach einem bestimmten Algorithmus suchst, der hier nicht auftaucht (z.B. ein Kalman-Filter für Sensorfusion oder Orbit-Berechnungen), kann es sein, dass der entsprechende Code aufgrund von Indexgrößenbeschränkungen nicht vollständig erfasst wurde — in diesem Fall lohnt sich eine vollständige Devin-Session, um den kompletten Code zu durchsuchen.

Wiki pages you might want to explore:
- [Application Initialization (AeroRust/nanosat-workshop)](/wiki/AeroRust/nanosat-workshop#3.1)
- [Raspberry Pi Pico W Target (AeroRust/nanosat-workshop)](/wiki/AeroRust/nanosat-workshop#4.3)
- [Telemetry Protocol Crate (AeroRust/nanosat-workshop)](/wiki/AeroRust/nanosat-workshop#4.4)
