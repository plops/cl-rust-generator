+ Systemkonzept: STM32G474 "All-in-One" Advanced Instrument

## 1. Übersicht und Zielsetzung
Entwicklung eines hochintegrierten, modularen Messgeräts auf Basis des **STM32G474CEU6** (170 MHz, 128 KB RAM, 512 KB Flash). Das System agiert als Multi-Instrument (Oszilloskop, Vektor-Netzwerkanalysator, AWG, Frequenzzähler, Kapazitätsmessgerät) und lagert rechen- sowie zeitkritische Operationen komplett in die analoge und digitale Hardware-Peripherie (HRTIM, ADCs, DACs, COMPs, OPAMPs, DMA) des Mikrocontrollers aus. 

Die Steuerung und Visualisierung erfolgt über eine PC-basierte TUI (Terminal User Interface), geschrieben in Rust. Die Firmware auf dem Mikrocontroller wird auf Basis des asynchronen Rust-Frameworks **Embassy** (`no_std`) umgesetzt.

---

## 2. Speicher- und Firmware-Architektur (Monolithische State-Machine)
Aufgrund des begrenzten Arbeitsspeichers (128 KB) bei gleichzeitig hohem RAM-Bedarf der einzelnen Modi (z. B. für ADC-Ringpuffer), wird das System streng **exklusiv modular** betrieben.

* **Monolithische Firmware:** Es wird *eine* Gesamt-Firmware kompiliert. Ein übergeordneter Control-Task lauscht auf USB-Befehle der TUI.
* **Dynamisches RAM-Sharing:** Wenn die TUI den Modus wechselt (z. B. von Oszilloskop zu VNA), beendet Embassy per Cancellation (Drop) den aktuellen Mess-Task. Dadurch wird die Hardware sauber de-initialisiert und der große dynamische Puffer im RAM freigegeben. Der neu gestartete Task erhält exklusiven Zugriff auf diesen Speicherbereich.
* **Speicheraufteilung:**
  * **CCM-SRAM (32 KB):** Reserviert für den kritischen, blockierungsfreien Rust-Code, den Stack, den Embassy-Executor und Hardware-Interrupt-Routinen (0-Wait-State Ausführung).
  * **SRAM1 (80 KB):** Dient als massiver Akkumulations- und DMA-Puffer für den *jeweils aktiven* Modus (z. B. Platz für bis zu 40.000 16-Bit-Samples am Stück).

---

## 3. Die Funktionsmodule (Hardware-Mapping)

### 3.1 Modus A: Real-Time Oszilloskop (RTS) & Equivalent Time Sampling (ETS)
* **Real-Time Sampling (Interleaved):**
  * Mehrere ADCs (ADC 1 bis 4) tasten denselben Pin zeitversetzt ab, um die effektive Samplerate von 4 MSPS (pro ADC) auf bis zu 16 MSPS zu vervierfachen.
  * *Datenfluss:* ADC 1..4 $\to$ DMA (Circular) $\to$ SRAM1 $\to$ Embassy Async Task $\to$ USB Block-Transfer.
* **Equivalent Time Sampling (ETS) für Frequenzen bis in den GHz-Bereich:**
  * *Trigger:* Das Signal wird auf einen internen Komparator (z. B. COMP1) geroutet. Die Schwellspannung (Trigger-Level) wird hardwarenah durch einen internen DAC (z. B. DAC3) vorgegeben.
  * *Time-Base:* Die erkannte Flanke des COMP1 triggert den HRTIM. Der HRTIM verzögert um $\Delta t$ (in 184-ps-Schritten) und löst die ADC-Wandlung aus.
  * *Akkumulation:* Nach jedem Sample passt der HRTIM-Burst-DMA den Delay-Wert autonom an. Die zeitlich verschobenen Messpunkte periodischer Signale werden im SRAM zu einem hochauflösenden Kurvenabbild (z. B. effektiv 1 GSPS) zusammengesetzt.

### 3.2 Modus B: Vektor-Netzwerkanalysator (VNA)
* **Signalerzeugung:** Der HRTIM taktet den internen DAC1 (gepuffert durch internen OPAMP1 für Stromstabilität). Eine Sinuswelle definierter Frequenz wird in das Testobjekt (DUT) injiziert.
* **Signalerfassung & Sweep:** Der HRTIM generiert exakt phasenverschobene ADC-Trigger ($\Delta t$ steppt in Hardware von $0^\circ$ bis $360^\circ$ der erzeugten Periode).
* **Verarbeitung:** Da Phase und Frequenz starr per Hardware gekoppelt sind, sammelt die Rust-Firmware die Datenpunkte im RAM. Aus der Amplitudendifferenz (DUT-Eingang vs. Ausgang) und dem Phasenversatz ($\Delta t$ des Kurvenmaximums) berechnet der Chip Dämpfung und Phasenlage.

### 3.3 Modus C: Arbitrary Waveform Generator (AWG)
* **Funktion:** Ausgabe beliebiger, von der PC-TUI hochgeladener Wellenformen.
* **Hardware:** Der SRAM1 hält die Lookup-Table (LUT) der Ziel-Wellenform $\to$ DMA transferiert zyklisch $\to$ DAC (z. B. DAC1).
* **Analog-Frontend:** Der DAC wird intern auf einen OPAMP im High-Speed-Modus (Slew Rate 45 V/µs) geroutet. Dies ermöglicht es, das Signal am externen Pin stabil zu treiben, ohne dass die Referenzspannung bei geringer Last einbricht.

### 3.4 Modus D: Kapazitätsmessgerät (Multi-Touch)
* **Funktion:** Messung kleinster Kapazitätsänderungen an mehreren Pins im Time-Division-Multiplexing (TDM).
* **Hardware:** Ein I/O-Pin wird kurz als Push-Pull-Ausgang auf High ($3{,}3\text{ V}$) gezogen, um die externe Kapazität aufzuladen. Danach schaltet der Pin in den analogen Eingangsmodus um.
* **Zeitmessung:** Die Entladekurve (über einen Pull-Down-Widerstand) wird von einem Komparator (COMPx) überwacht. Ein 32-Bit-Timer (TIM2) misst per Input Capture die exakte Zeit, bis die Spannung die Komparatorschwelle unterschreitet.
* **Multiplexing:** Die Embassy-Tasks schalten asynchron reihum zwischen 3 oder mehr Pins um.

### 3.5 Modus E: Frequenzzähler (Advanced Filtering)
* **Funktion:** Hochpräzise Zählung von Frequenzen mit flexibler Signalkonditionierung komplett in Hardware.
* **Signalpfad & Trigger-Level:** Das Messsignal durchläuft intern einen Komparator (z. B. COMP2). Das Trigger-Level (Referenzspannung am invertierenden Eingang) wird durch einen DAC (z. B. DAC3) generiert und lässt sich über die TUI stufenlos von $0\text{ V}$ bis $3{,}3\text{ V}$ einstellen.
* **Hardware-Filtering:**
  1. *Analog:* Die Komparator-Hysterese (Low, Medium, High) filtert Signalprellen bei langsamen Flanken.
  2. *Digital:* Der COMP-Ausgang triggert den Zähleingang eines 32-Bit-Timers (TIM2). Dessen digitaler Eingangsfilter wird genutzt, um hochfrequente Störimpulse (Glitches) zu unterdrücken. Beide Filterstärken sind per TUI konfigurierbar.
* **Messprinzip:** Ein Basis-Timer (z. B. TIM6) generiert eine hochpräzise Torzeit (Gate, z. B. exakt $1{,}000\text{ s}$). Der Zähler-Timer ermittelt in diesem Fenster die Anzahl der gefilterten Flanken (= Frequenz in Hz).

---

## 4. Analoges Frontend (Barebone-Design)
Um die Hardware so simpel wie möglich zu halten, wird auf komplexe, aktive analoge Vorschaltungen verzichtet:
* **Spannungsbereich:** Das Gerät misst Signale primär direkt im Bereich von $0\text{ V}$ bis $3{,}3\text{ V}$ (bezogen auf $V_{DDA}$).
* **Schutzbeschaltung:** Reine Analog-Pins (`TT_a`) sind nur bis $3{,}6\text{ V}$ (absolutes Max. $4{,}0\text{ V}$) tolerant. Übersteuern führt zur Beschädigung. Für Mess-Eingänge (Oszilloskop, VNA) werden lediglich rudimentäre Serienwiderstände (z. B. $1\text{ k}\Omega$) empfohlen, um im Fehlerfall den Strom in die internen Klemmdioden des STM32 zu begrenzen. 
* Digitale Signale (z. B. für den Frequenzzähler) können auf Pins mit `FT`-Struktur (5-V-tolerant) gelegt werden.

---

## 5. Kommunikationsschnittstelle (Host <-> Device)
* **Verbindung:** USB 2.0 Full-Speed (12 Mbps) über die native USB-Schnittstelle des STM32G474 (via `embassy-usb`).
* **Transport:** USB CDC (Virtueller COM-Port) für breite OS-Kompatibilität und einfaches Debugging.
* **Datenübertragung (Snapshot-Prinzip):** Da Full-Speed-USB für Live-Streaming von Megasamples zu langsam ist, arbeitet das System *blockbasiert*. Ist ein Puffer (z. B. ein kompletter ETS-Sweep) gefüllt, wird dieser asynchron via USB an den PC gesendet, bevor der nächste Trigger freigegeben wird.
* **Protokoll & Serialisierung (Postcard + COBS):**
  * Es wird ein geteiltes Rust-Crate (`common`) erstellt, das die Nachrichtenformate (Enums/Structs) für Befehle und Messdaten enthält.
  * **Postcard:** Wird genutzt, um die Datenstrukturen rechen- und speichereffizient in Binärdaten zu serialisieren (perfekt für `no_std`).
  * **COBS (Consistent Overhead Byte Stuffing):** Das serialisierte Paket wird in COBS gepackt. Das garantiert, dass das `0x00`-Byte ausschließlich als Frame-Trennzeichen dient, was die Paket-Synchronisation zwischen PC und MCU massiv vereinfacht und absolut robust macht.
