# STM32G474CEU6 – Technisches Datenblatt & Referenzhandbuch

Dieses Dokument enthält die aufbereiteten technischen Spezifikationen und Datenblattangaben für den Mikrocontroller **STM32G474CEU6**. Irrelevante Informationen zu anderen Gehäuseformen (z. B. LQFP100/128, BGA, WLCSP) oder nicht vorhandenen Features (wie FSMC) wurden entfernt.

---

## 1. Übersicht und Spezifikationen (STM32G474CEU6)

### 1.1 Schlüsselmerkmale der Variante CEU6
* **Teilenummer:** STM32G474CEU6
  * **Familie:** STM32 Arm Cortex-M4 mit FPU
  * **Sub-Familie:** STM32G474 (High-Resolution Timer, Rich Analog, Math Acc)
  * **Pin-Anzahl:** `C` = 48 Pins
  * **Flash-Speicher:** `E` = 512 Kbyte
  * **Gehäuse:** `U` = UFQFPN48 ($7 \times 7\text{ mm}$, Pitch 0,5 mm)
  * **Temperaturbereich:** `6` = Industrie, $-40\text{ bis }+85\text{ }^\circ\text{C}$ ($+105\text{ }^\circ\text{C}$ Sperrschichttemperatur $T_J$)
* **CPU-Kern:**
  * 32-Bit Arm® Cortex®-M4 CPU mit FPU (Fließkommaeinheit für einfache Genauigkeit)
  * DSP-Befehlssatz
  * Adaptive Real-Time Accelerator (ART Accelerator™) für 0-Wait-State-Ausführung aus dem Flash
  * Taktfrequenz bis zu **170 MHz** (213 DMIPS)
  * Memory Protection Unit (MPU)
* **Speicher:**
  * **512 Kbyte Flash:** Dual-Bank mit Read-While-Write (RWW), ECC-Unterstützung (Fehlerkorrektur), PCROP (Proprietary Code Readout Protection), konfigurierbarer Sicherheitsbereich, 1 Kbyte OTP.
  * **128 Kbyte SRAM gesamt:**
    * 80 Kbyte SRAM1 auf Systembus (die ersten 32 Kbyte mit Hardware-Paritätsprüfung)
    * 16 Kbyte SRAM2 (Inhaltsübernahme im Standby möglich)
    * 32 Kbyte CCM SRAM (Core Coupled Memory) über I/D-Bus oder Systembus (mit Hardware-Paritätsprüfung, 1-KB-Schreibschutz)
* **Mathematik-Beschleuniger:**
  * **CORDIC:** Hardware-Beschleunigung für trigonometrische, hyperbolische und mathematische Funktionen (Sin, Cos, Sinh, Cosh, Atan, Atan2, Atanh, Modul, Quadratwurzel, $\ln$).
  * **FMAC:** Filter Mathematical Accelerator (FIR- und IIR-Filterbeschleunigung, $16 \times 16\text{-Bit}$ MAC).
* **Peripherieübersicht im 48-Pin Gehäuse (STM32G474Cx):**
  * **FSMC:** Nicht vorhanden.
  * **Quad-SPI:** 1 Interface (SDR/DDR, Memory-Mapped Mode).
  * **I/Os:** Bis zu 42 Fast-I/Os (5-V-tolerant), 3 externe Wakeup-Pins.
  * **DMA:** 2 Controller à 8 Kanäle (16 Kanäle gesamt) mit DMAMUX.
  * **ADCs:** 5x 12-Bit-SAR-ADCs (bis zu 4 Msps, bis zu 16-Bit mit Oversampling, **21 Kanäle** im UFQFPN48 verfügbar).
  * **DACs:** 4x DAC (insgesamt 7 Kanäle: 3 externe, 4 interne Kanäle, bis zu 15 Msps).
  * **Komparatoren:** 7 ultraschnelle Rail-to-Rail-Analogkomparatoren.
  * **Operationsverstärker:** 6 OPAMPs mit interner/externer PGA-Beschaltung (Gain bis 64).
  * **Referenzspannungs-Puffer:** VREFBUF integriert ($2{,}048\text{ V}$, $2{,}5\text{ V}$, $2{,}9\text{ V}$).
  * **Timer (gesamt 17):**
    * 1x HRTIM (High-Resolution Timer, $184\text{ ps}$ Auflösung, 6 Zähler, 12 PWM-Ausgänge)
    * 3x 16-Bit Advanced Motor Control Timer (TIM1, TIM8, TIM20) mit komplementären Ausgängen, Totzeitgenerator und Not-Aus (Break)
    * 2x 32-Bit General Purpose Timer (TIM2, TIM5)
    * 5x 16-Bit General Purpose Timer (TIM3, TIM4, TIM15, TIM16, TIM17)
    * 2x 16-Bit Basic Timer (TIM6, TIM7)
    * 1x Low-Power-Timer (LPTIM1)
    * 2x Watchdog-Timer (IWDG, WWDG)
    * 1x SysTick-Timer (24-Bit Downcounter)
    * Verfügbare PWM-Kanäle gesamt: 41 (davon 30 ohne Komplementärkanäle)
  * **Kommunikationsschnittstellen:**
    * 3x FDCAN (CAN 2.0A/B und CAN-FD) mit gemeinsamem 3-Kbyte-Message-RAM
    * 4x $I^2C$ (Fast Mode Plus 1 Mbit/s, SMBus/PMBus)
    * 3x USART (USART1, USART2, USART3; Smartcard, IrDA, LIN)
    * 1x LPUART (Low-Power UART, betriebsfähig im Stop-Modus)
    * 3x SPI (SPI1, SPI2, SPI3, davon 2x mit halbdirektionaler $I^2S$-Audiofunktion)
    * 1x SAI (Serial Audio Interface)
    * 1x USB 2.0 Full-Speed Device (kristallloser Betrieb über HSI48 / CRS)
    * 1x UCPD (USB Type-C und Power Delivery Rev. 3.0 Schnittstelle)
    * 1x Infrarot-Transmitter (IRTIM)
  * **Kryptografie & Sicherheit:** Echter Zufallszahlengenerator (RNG), 96-Bit Unique ID, CRC-Einheit.
  * **Betriebsbedingungen:**
    * $V_{DD} = 1{,}71\text{ V bis }3{,}6\text{ V}$
    * $V_{DDA} = 1{,}62\text{ V bis }3{,}6\text{ V}$
    * $V_{BAT} = 1{,}55\text{ V bis }3{,}6\text{ V}$

---

## 2. Funktionsübersicht

### 2.1 Systemarchitektur & Busmatrix
Das System basiert auf einer 32-Bit Multi-AHB-Busmatrix, die 5 Master (Cortex-M4 I-Bus, D-Bus, S-Bus, DMA1, DMA2) mit Slaves (Flash, SRAM1, SRAM2, CCM SRAM, Peripherie an AHB1/AHB2, APB1/APB2, QUADSPI) verbindet.

```
[ Cortex-M4 Core ] --(I-Code, D-Code, System-Bus)---+
[ DMA1 (8 Ch)    ] ----------------------------------+---> [ Multi-AHB Busmatrix ]
[ DMA2 (8 Ch)    ] ----------------------------------+            |
                                                                  +--> Flash (512 KB) & ART Accelerator
                                                                  +--> SRAM1 (80 KB)
                                                                  +--> SRAM2 (16 KB)
                                                                  +--> CCM SRAM (32 KB)
                                                                  +--> AHB1/AHB2 Peripherie
                                                                  +--> APB1/APB2 Brücken
                                                                  +--> QUADSPI Interface
```

### 2.2 Speicheraufteilung des SRAM
* **SRAM1:** 80 Kbyte bei `0x2000 0000`. Zugriff durch CPU via Systembus (oder I/D-Bus bei Boot aus SRAM). Erste 32 Kbyte mit Paritätsprüfung.
* **SRAM2:** 16 Kbyte bei `0x2001 4000`. Kann in Standby-Modi gehalten werden.
* **CCM SRAM:** 32 Kbyte bei `0x1000 0000` (I/D-Bus-Zugriff für 0 Wait States bei max. CPU-Takt) und gespiegelt (aliased) bei `0x2001 8000` für Zugriff durch alle Master (CPU, DMA1, DMA2). Schreibschutz in 1-KB-Schritten möglich.

### 2.3 Hardware-Beschleuniger
* **CORDIC:** Berechnet mathematische Funktionen in Hardware mittels 24-Bit-Rotations-Engine. Unterstützt Kreis- und Hyperbelfunktionen in Vektor- und Rotationsmodi. Konvergenzrate 4 Bits pro Takt. Eingänge/Ausgänge in 16-Bit und 32-Bit Festkomma. DMA-Anbindung vorhanden.
* **FMAC:** Ausführung von Vektor-Operationen ($16 \times 16\text{-Bit}$ Multiplikation mit $24+2\text{-Bit}$ Akkumulator). Unterstützt bis zu 3 Ringpufferbereiche (2 Eingänge, 1 Ausgang). Geeignet für FIR- und IIR-Filter.

### 2.4 Spannungsversorgung und Regler
* **Spannungsebenen:**
  * $V_{DD}$ ($1{,}71\text{ V bis }3{,}6\text{ V}$): Externe Versorgung für I/Os, interne Spannungsregler und Basissystem.
  * $V_{DDA}$ ($1{,}62\text{ V bis }3{,}6\text{ V}$): Unabhängige analoge Versorgung für ADC, DAC, OPAMP, COMP und VREFBUF.
  * $V_{BAT}$ ($1{,}55\text{ V bis }3{,}6\text{ V}$): Versorgung für RTC, LSE ($32{,}768\text{ kHz}$) und Backup-Register bei Ausfall von $V_{DD}$.
  * $V_{REF+}$: Referenzspannung für ADC/DAC. Kann extern angelegt oder vom internen Puffer (VREFBUF) generiert werden.
* **Spannungsregler-Modi:**
  * **Range 1 Boost Mode:** CPU-Frequenz bis 170 MHz.
  * **Range 1 Normal Mode:** CPU-Frequenz bis 150 MHz.
  * **Range 2 Mode:** CPU-Frequenz bis 26 MHz (reduzierter Stromverbrauch).
  * **Low-Power-Regler (LPR):** Im Low-Power-Run-, Low-Power-Sleep- und Stop-Modus aktiv.
* **Low-Power-Modi:**
  * **Sleep / Low-Power Sleep:** CPU gestoppt, Peripherie läuft weiter.
  * **Stop 0 / Stop 1:** Alle Takte in der $V_{CORE}$-Domäne gestoppt, SRAM- und Registerinhalt bleiben erhalten. Aufwecken über EXTI-fähige Peripherie.
  * **Standby:** $V_{CORE}$ abgeschaltet. SRAM2 kann optional gehalten werden. Aufwecken via Reset, WKUP-Pins, RTC-Events oder IWDG.
  * **Shutdown:** Minimaler Verbrauch. Regler komplett aus. Nur WKUP-Pins, RTC (LSE) und NRST aktiv.

### 2.5 Taktsystem
* **HSE:** $4\text{ bis }48\text{ MHz}$ Quarz-/Keramikresonator oder externer Takt.
* **LSE:** $32{,}768\text{ kHz}$ Quarz oder externer Takt für RTC.
* **HSI16:** Interner $16\text{ MHz}$ RC-Oszillator ($\pm 1\,\%$ Genauigkeit), werkskalibriert.
* **HSI48:** Interner $48\text{ MHz}$ Oszillator mit autom. Trimmsystem (CRS) synchronisierbar mit USB-SOF.
* **LSI:** Interner $32\text{ kHz}$ RC-Oszillator ($\pm 5\,\%$) für Watchdog und RTC.
* **PLL:** Haupteingang $2{,}66\text{ bis }16\text{ MHz}$, VCO-Ausgang bis $344\text{ MHz}$, Systemtakt (PLL_R) bis $170\text{ MHz}$.

### 2.6 Analogperipherie
* **ADCs (12-Bit):** 5 eigenständige ADCs, Abtastraten bis 4 Msps (bei 12-Bit) bzw. 6,66 Msps (bei 6-Bit). Single-Ended und differentieller Modus. Minimalzeit Abtastung: $41{,}67\text{ ns}$.
* **DACs (12-Bit):** 4 Wandlerkerne. 3 Kanäle extern herausgeführt (gepuffert bis 1 Msps), 4 Kanäle intern (ungepuffert bis 15 Msps).
* **VREFBUF:** Ausgangsspannung umschaltbar auf $2{,}048\text{ V}$, $2{,}5\text{ V}$ oder $2{,}9\text{ V}$.
* **OPAMPs:** 6 Rail-to-Rail-Operationsverstärker, Bandbreite $13\text{ MHz}$, programmierbare Verstärkung (PGA) in invertierender ($-1$ bis $-63$) und nicht-invertierender Konfiguration ($2$ bis $64$).
* **Komparatoren:** 7 ultraschnelle Schienen-zu-Schienen-Komparatoren mit programmierbarer Hysterese und Referenzauswahl.

### 2.7 High-Resolution Timer (HRTIM)
* Modul mit 7 Sub-Timern (1 Master, 6 Slaves) und 12 PWM-Ausgängen.
* Digitale Delay-Line generiert Taktung mit **$184\text{ ps}$ Zeitauflösung** (DLL geregelt, unabhängig von $V_{DD}$ und Temperatur).
* Geeignet für LLC-Konverter, phasengeschobene Vollbrücken (Phase-Shifted Full Bridge), Buck-, Boost- und Resonanzwandler.
* Direkte Schnittstelle zu Komparator-Ausgängen für Cycle-by-Cycle-Strombegrenzung oder Zero-Voltage-Switching (ZVS).

---

## 3. Pinbelegung & Definitionen: UFQFPN48

### 3.1 Gehäuseübersicht UFQFPN48
* **Abmessungen:** $7 \times 7\text{ mm}$, 48 Anschlüsse + zentriertes Exposed Pad (Thermal Pad).
* **Thermal Pad:** Das Exposed Pad auf der Unterseite ist elektrisch intern mit $V_{SS}$ verbunden und **muss** auf der Leiterplatte flächig mit GND verlötet werden.

```
                  UFQFPN48 (Draufsicht / Top View)
                 
                      48 47 46 45 44 43 42 41 40 39 38 37
                     +-----------------------------------+
             VBAT -- | 1                               36| -- PA13
             PC13 -- | 2                               35| -- VDD
  PC14-OSC32_IN -- | 3                               34| -- PA12
 PC15-OSC32_OUT -- | 4                               33| -- PA11
      PF0-OSC_IN -- | 5                               32| -- PA10
     PF1-OSC_OUT -- | 6          [EXPOSED PAD]        31| -- PA9
        PG10-NRST -- | 7              (VSS)           30| -- PA8
              PA0 -- | 8                               29| -- PC6
              PA1 -- | 9                               28| -- PB15
              PA2 -- | 10                              27| -- PB14
              PA3 -- | 11                              26| -- PB13
              PA4 -- | 12                              25| -- PB12
                     +-----------------------------------+
                       13 14 15 16 17 18 19 20 21 22 23 24
                       
   Pin 13: PA5   | Pin 16: PC4  | Pin 19: PB2    | Pin 22: PB10
   Pin 14: PA6   | Pin 17: PB0  | Pin 20: VREF+  | Pin 23: VDD
   Pin 15: PA7   | Pin 18: PB1  | Pin 21: VDDA   | Pin 24: PB11
   
   Pin 37: PA14  | Pin 40: PC10 | Pin 43: PB5    | Pin 46: PB8-BOOT0
   Pin 38: PA15  | Pin 41: PC11 | Pin 44: PB6    | Pin 47: PB9
   Pin 39: PB3   | Pin 42: PB4  | Pin 45: PB7    | Pin 48: VDD
```

### 3.2 Tabelle der Pin-Definitionen (UFQFPN48)

* **I/O-Struktur-Legende:**
  * `FT`: 5-V-tolerant
  * `TT`: 3,6-V-tolerant
  * `_a`: Mit Analog-Switch-Funktion (versorgt durch $V_{DDA}$)
  * `_c`: USB Type-C PD fähig
  * `_d`: USB Type-C PD Dead Battery Funktion
  * `_f`: $I^2C$ Fast-mode Plus (Fm+) fähig
  * `_u`: USB-Signal-fähig

| Pin-Nr. | Pin-Name | Typ | I/O-Struktur | Hinweise | Zusätzliche / Analoge Funktionen |
| :---: | :--- | :---: | :---: | :---: | :--- |
| **1** | VBAT | S | - | - | Backup-Spannungsversorgung ($1{,}55\text{ bis }3{,}6\text{ V}$) |
| **2** | PC13 | I/O | FT | (1) | WKUP2, RTC_TAMP1, RTC_TS, RTC_OUT1 |
| **3** | PC14-OSC32_IN | I/O | FT | (1) | OSC32_IN |
| **4** | PC15-OSC32_OUT | I/O | FT | (1) | OSC32_OUT |
| **5** | PF0-OSC_IN | I/O | FT_fa | - | ADC1_IN10, OSC_IN |
| **6** | PF1-OSC_OUT | I/O | FT_a | - | ADC2_IN10, COMP3_INM, OSC_OUT |
| **7** | PG10-NRST | I/O | NRST | (2) | Reset-Eingang (bidirektional) |
| **8** | PA0 | I/O | TT_a | - | ADC12_IN1, COMP1_INM, COMP3_INP, RTC_TAMP2, WKUP1 |
| **9** | PA1 | I/O | TT_a | - | ADC12_IN2, COMP1_INP, OPAMP1_VINP, OPAMP3_VINP, OPAMP6_VINM |
| **10** | PA2 | I/O | FT_a | - | ADC1_IN3, COMP2_INM, OPAMP1_VOUT, WKUP4, LSCO |
| **11** | PA3 | I/O | TT_a | - | ADC1_IN4, COMP2_INP, OPAMP1_VINM/VINP, OPAMP5_VINM |
| **12** | PA4 | I/O | TT_a | - | ADC2_IN17, DAC1_OUT1, COMP1_INM |
| **13** | PA5 | I/O | TT_a | - | ADC2_IN13, DAC1_OUT2, COMP2_INM, OPAMP2_VINM |
| **14** | PA6 | I/O | TT_a | - | ADC2_IN3, DAC2_OUT1, OPAMP2_VOUT |
| **15** | PA7 | I/O | TT_a | - | ADC2_IN4, COMP2_INP, OPAMP1_VINP, OPAMP2_VINP |
| **16** | PC4 | I/O | FT_fa | - | ADC2_IN5 |
| **17** | PB0 | I/O | TT_a | - | ADC3_IN12, ADC1_IN15, COMP4_INP, OPAMP2_VINP, OPAMP3_VINP |
| **18** | PB1 | I/O | TT_a | - | ADC3_IN1, ADC1_IN12, COMP1_INP, OPAMP3_VOUT, OPAMP6_VINM |
| **19** | PB2 | I/O | TT_a | - | ADC2_IN12, COMP4_INM, OPAMP3_VINM |
| **20** | VREF+ | S | - | - | Referenzspannungsausgang/Eingang für ADC/DAC (VREFBUF) |
| **21** | VDDA | S | - | - | Analoge Stromversorgung ($1{,}62\text{ bis }3{,}6\text{ V}$) |
| **22** | PB10 | I/O | TT_a | - | COMP5_INM, OPAMP3_VINM, OPAMP4_VINM |
| **23** | VDD | S | - | - | Digitale Stromversorgung |
| **24** | PB11 | I/O | TT_a | - | ADC12_IN14, COMP6_INP, OPAMP4_VINP, OPAMP6_VOUT |
| **25** | PB12 | I/O | TT_a | - | ADC4_IN3, ADC1_IN11, COMP7_INM, OPAMP4_VOUT, OPAMP6_VINP |
| **26** | PB13 | I/O | TT_a | - | ADC3_IN5, COMP5_INP, OPAMP3_VINP, OPAMP4_VINP, OPAMP6_VINP |
| **27** | PB14 | I/O | TT_a | - | ADC4_IN4, ADC1_IN5, COMP7_INP, OPAMP2_VINP, OPAMP5_VINP |
| **28** | PB15 | I/O | TT_a | - | ADC4_IN5, ADC2_IN15, COMP6_INM, OPAMP5_VINM |
| **29** | PC6 | I/O | FT_f | - | - |
| **30** | PA8 | I/O | FT_a | - | ADC5_IN1, OPAMP5_VOUT |
| **31** | PA9 | I/O | FT_fda | (3) | ADC5_IN2, UCPD1_DBCC1 |
| **32** | PA10 | I/O | FT_fda | (3) | UCPD1_DBCC2, PVD_IN |
| **33** | PA11 | I/O | FT_u | - | USB_DM |
| **34** | PA12 | I/O | FT_u | - | USB_DP |
| **35** | VDD | S | - | - | Digitale Stromversorgung |
| **36** | PA13 | I/O | FT_f | (4) | SWDIO / JTMS |
| **37** | PA14 | I/O | FT_f | (4) | SWCLK / JTCK |
| **38** | PA15 | I/O | FT_f | (4) | JTDI |
| **39** | PB3 | I/O | FT | (4) | JTDO / TRACESWO |
| **40** | PC10 | I/O | FT | - | - |
| **41** | PC11 | I/O | FT_f | - | - |
| **42** | PB4 | I/O | FT_c | (3)(4) | JTRST, UCPD1_CC2 |
| **43** | PB5 | I/O | FT_f | - | - |
| **44** | PB6 | I/O | FT_c | (3) | UCPD1_CC1 |
| **45** | PB7 | I/O | FT_f | - | - |
| **46** | PB8-BOOT0 | I/O | FT_f | (5) | BOOT0 |
| **47** | PB9 | I/O | FT_f | - | - |
| **48** | VDD | S | - | - | Digitale Stromversorgung |
| **Pad** | VSS | S | - | - | Masse (Muss verlötet werden) |

#### Spezielle Hinweise zu Pins:
1. **PC13, PC14, PC15:** Werden über einen internen Leistungsschalter gespeist (max. Strombelastbarkeit $3\text{ mA}$). Nicht als Stromquelle verwenden (z. B. für LEDs). Maximale Ausgangsfrequenz auf $2\text{ MHz}$ (bei $30\text{ pF}$) begrenzt.
2. **PG10-NRST:** Kann über Option-Bytes als universeller Eingang/Ausgang (PG10) konfiguriert werden.
3. **PB4 & PB6 / PA9 & PA10 (UCPD Dead Battery):** PB4 und PB6 enthalten integrierte Pull-Down-Widerstände ($5{,}1\text{ k}\Omega$) für USB Type-C CC-Leitungen. Diese werden aktiviert, wenn PA10 bzw. PA9 auf High gezogen sind. Die Funktion kann über das Bit `UCPD1_DBDIS=1` im Register `PWR_CR3` deaktiviert werden.
4. **JTAG/SWD-Pins:** Nach Reset sind PA13, PA14, PA15, PB3 und PB4 als Debug-Pins konfiguriert mit aktivierten internen Pull-Ups (PA13, PA15, PB4) bzw. Pull-Down (PA14).
5. **PB8-BOOT0:** Es wird empfohlen, diesen Pin nach dem Startup nicht im analogen Modus zu belassen, falls er unbeschaltet bleibt, um Querströme zu minimieren.

---

## 4. Alternate Functions (AF-Tabelle für STM32G474CEU6 Pins)

| Pin | AF0 | AF1 | AF2 | AF3 | AF4 | AF5 | AF6 | AF7 | AF8 | AF9 | AF10 | AF11 | AF12 | AF13 | AF14 | AF15 |
| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
| **PA0** | - | TIM2_CH1 | TIM5_CH1 | - | - | - | - | USART2_CTS | COMP1_OUT | TIM8_BKIN | TIM8_ETR | - | - | - | TIM2_ETR | EVENTOUT |
| **PA1** | RTC_REFIN | TIM2_CH2 | TIM5_CH2 | - | - | - | - | USART2_RTS_DE | - | TIM15_CH1N | - | - | - | - | - | EVENTOUT |
| **PA2** | - | TIM2_CH3 | TIM5_CH3 | - | - | - | - | USART2_TX | COMP2_OUT | TIM15_CH1 | QUADSPI1_BK1_NCS | - | LPUART1_TX | - | UCPD1_FRSTX | EVENTOUT |
| **PA3** | - | TIM2_CH4 | TIM5_CH4 | SAI1_CK1 | - | - | - | USART2_RX | - | TIM15_CH2 | QUADSPI1_CLK | - | LPUART1_RX | SAI1_MCLK_A | - | EVENTOUT |
| **PA4** | - | - | TIM3_CH2 | - | - | SPI1_NSS | SPI3_NSS/I2S3_WS | USART2_CK | - | - | - | - | - | SAI1_FS_B | - | EVENTOUT |
| **PA5** | - | TIM2_CH1 | TIM2_ETR | - | - | SPI1_SCK | - | - | - | - | - | - | - | - | UCPD1_FRSTX | EVENTOUT |
| **PA6** | - | TIM16_CH1 | TIM3_CH1 | - | TIM8_BKIN | SPI1_MISO | TIM1_BKIN | - | COMP1_OUT | - | QUADSPI1_BK1_IO3 | - | LPUART1_CTS | - | - | EVENTOUT |
| **PA7** | - | TIM17_CH1 | TIM3_CH2 | - | TIM8_CH1N | SPI1_MOSI | TIM1_CH1N | - | COMP2_OUT | - | QUADSPI1_BK1_IO2 | - | - | - | UCPD1_FRSTX | EVENTOUT |
| **PA8** | MCO | - | I2C3_SCL | - | I2C2_SDA | I2S2_MCK | TIM1_CH1 | USART1_CK | COMP7_OUT | - | TIM4_ETR | FDCAN3_RX | SAI1_CK2 | HRTIM1_CHA1 | SAI1_SCK_A | EVENTOUT |
| **PA9** | - | - | I2C3_SMBA | - | I2C2_SCL | I2S3_MCK | TIM1_CH2 | USART1_TX | COMP5_OUT | TIM15_BKIN | TIM2_CH3 | - | - | HRTIM1_CHA2 | SAI1_FS_A | EVENTOUT |
| **PA10**| - | TIM17_BKIN | - | USB_CRS_SYNC | I2C2_SMBA | SPI2_MISO | TIM1_CH3 | USART1_RX | COMP6_OUT | - | TIM2_CH4 | TIM8_BKIN | SAI1_D1 | HRTIM1_CHB1 | SAI1_SD_A | EVENTOUT |
| **PA11**| - | - | - | - | - | SPI2_MOSI/I2S2_SD | TIM1_CH1N | USART1_CTS | COMP1_OUT | FDCAN1_RX | TIM4_CH1 | TIM1_CH4 | TIM1_BKIN2 | HRTIM1_CHB2 | - | EVENTOUT |
| **PA12**| - | TIM16_CH1 | - | - | - | I2SCKIN | TIM1_CH2N | USART1_RTS_DE | COMP2_OUT | FDCAN1_TX | TIM4_CH2 | TIM1_ETR | - | HRTIM1_FLT1 | - | EVENTOUT |
| **PA13**| JTMS-SWDIO | TIM16_CH1N | - | I2C4_SCL | I2C1_SCL | IR_OUT | - | USART3_CTS | - | - | TIM4_CH3 | - | - | SAI1_SD_B | - | EVENTOUT |
| **PA14**| JTCK-SWCLK | LPTIM1_OUT | - | I2C4_SMBA | I2C1_SDA | TIM8_CH2 | TIM1_BKIN | USART2_TX | - | - | - | - | - | SAI1_FS_B | - | EVENTOUT |
| **PA15**| JTDI | TIM2_CH1 | TIM8_CH1 | - | I2C1_SCL | SPI1_NSS | SPI3_NSS/I2S3_WS | USART2_RX | UART4_RTS_DE | TIM1_BKIN | - | FDCAN3_TX | - | HRTIM1_FLT2 | TIM2_ETR | EVENTOUT |
| **PB0** | - | - | TIM3_CH3 | - | TIM8_CH2N | - | TIM1_CH2N | - | - | - | QUADSPI1_BK1_IO1 | - | - | HRTIM1_FLT5 | UCPD1_FRSTX | EVENTOUT |
| **PB1** | - | - | TIM3_CH4 | - | TIM8_CH3N | - | TIM1_CH3N | - | COMP4_OUT | - | QUADSPI1_BK1_IO0 | - | LPUART1_RTS_DE | HRTIM1_SCOUT | - | EVENTOUT |
| **PB2** | RTC_OUT2 | LPTIM1_OUT | TIM5_CH1 | TIM20_CH1 | I2C3_SMBA | - | - | - | - | - | QUADSPI1_BK2_IO1 | - | - | HRTIM1_SCIN | - | EVENTOUT |
| **PB3** | TRACESWO | TIM2_CH2 | TIM4_ETR | USB_CRS_SYNC | TIM8_CH1N | SPI1_SCK | SPI3_SCK/I2S3_CK | USART2_TX | - | - | TIM3_ETR | FDCAN3_RX | HRTIM1_SCOUT | HRTIM1_EEV9 | SAI1_SCK_B | EVENTOUT |
| **PB4** | JTRST | TIM16_CH1 | TIM3_CH1 | - | TIM8_CH2N | SPI1_MISO | SPI3_MISO | USART2_RX | UART5_RTS_DE | - | TIM17_BKIN | FDCAN3_TX | - | HRTIM1_EEV7 | SAI1_MCLK_B | EVENTOUT |
| **PB5** | - | TIM16_BKIN | TIM3_CH2 | TIM8_CH3N | I2C1_SMBA | SPI1_MOSI | SPI3_MOSI/I2S3_SD | USART2_CK | I2C3_SDA | FDCAN2_RX | TIM17_CH1 | LPTIM1_IN1 | SAI1_SD_B | HRTIM1_EEV6 | UART5_CTS | EVENTOUT |
| **PB6** | - | TIM16_CH1N | TIM4_CH1 | - | - | TIM8_CH1 | TIM8_ETR | USART1_TX | COMP4_OUT | FDCAN2_TX | TIM8_BKIN2 | LPTIM1_ETR | HRTIM1_SCIN | HRTIM1_EEV4 | SAI1_FS_B | EVENTOUT |
| **PB7** | - | TIM17_CH1N | TIM4_CH2 | I2C4_SDA | I2C1_SDA | TIM8_BKIN | - | USART1_RX | COMP3_OUT | - | TIM3_CH4 | LPTIM1_IN2 | FMC_NL | HRTIM1_EEV3 | UART4_CTS | EVENTOUT |
| **PB8** | - | TIM16_CH1 | TIM4_CH3 | SAI1_CK1 | I2C1_SCL | - | - | USART3_RX | COMP1_OUT | FDCAN1_RX | TIM8_CH2 | - | TIM1_BKIN | HRTIM1_EEV8 | SAI1_MCLK_A | EVENTOUT |
| **PB9** | - | TIM17_CH1 | TIM4_CH4 | SAI1_D2 | I2C1_SDA | - | IR_OUT | USART3_TX | COMP2_OUT | FDCAN1_TX | TIM8_CH3 | - | TIM1_CH3N | HRTIM1_EEV5 | SAI1_FS_A | EVENTOUT |
| **PB10**| - | TIM2_CH3 | - | - | - | - | - | USART3_TX | LPUART1_RX | - | QUADSPI1_CLK | - | TIM1_BKIN | HRTIM1_FLT3 | SAI1_SCK_A | EVENTOUT |
| **PB11**| - | TIM2_CH4 | - | - | - | - | - | USART3_RX | LPUART1_TX | - | QUADSPI1_BK1_NCS | - | - | HRTIM1_FLT4 | - | EVENTOUT |
| **PB12**| - | - | TIM5_ETR | - | I2C2_SMBA | SPI2_NSS/I2S2_WS | TIM1_BKIN | USART3_CK | LPUART1_RTS_DE | FDCAN2_RX | - | - | - | HRTIM1_CHC1 | - | EVENTOUT |
| **PB13**| - | - | - | - | - | SPI2_SCK/I2S2_CK | TIM1_CH1N | USART3_CTS | LPUART1_CTS | FDCAN2_TX | - | - | - | HRTIM1_CHC2 | - | EVENTOUT |
| **PB14**| - | TIM15_CH1 | - | - | - | SPI2_MISO | TIM1_CH2N | USART3_RTS_DE | COMP4_OUT | - | - | - | - | HRTIM1_CHD1 | - | EVENTOUT |
| **PB15**| RTC_REFIN | TIM15_CH2 | TIM15_CH1N | COMP3_OUT | TIM1_CH3N | SPI2_MOSI/I2S2_SD | - | - | - | - | - | - | - | HRTIM1_CHD2 | - | EVENTOUT |
| **PC4** | - | - | TIM1_ETR | - | I2C2_SCL | - | - | USART1_TX | - | - | QUADSPI1_BK2_IO3 | - | - | - | - | EVENTOUT |
| **PC6** | - | - | TIM3_CH1 | HRTIM1_EEV10 | TIM8_CH1 | - | I2S2_MCK | COMP6_OUT | I2C4_SCL | - | - | - | - | HRTIM1_CHF1 | - | EVENTOUT |
| **PC10**| - | - | - | - | TIM8_CH1N | UART4_TX | SPI3_SCK/I2S3_CK | USART3_TX | - | - | - | - | - | HRTIM1_FLT6 | - | EVENTOUT |
| **PC11**| - | - | - | HRTIM1_EEV2 | TIM8_CH2N | UART4_RX | SPI3_MISO | USART3_RX | I2C3_SDA | - | - | - | - | - | - | EVENTOUT |
| **PC13**| - | - | TIM1_BKIN | - | TIM1_CH1N | - | TIM8_CH4N | - | - | - | - | - | - | - | - | EVENTOUT |
| **PF0** | - | - | - | - | I2C2_SDA | SPI2_NSS/I2S2_WS | TIM1_CH3N | - | - | - | - | - | - | - | - | EVENTOUT |
| **PF1** | - | - | - | - | - | SPI2_SCK/I2S2_CK | - | - | - | - | - | - | - | - | - | EVENTOUT |
| **PG10**| MCO | - | - | - | - | - | - | - | - | - | - | - | - | - | - | EVENTOUT |

---

## 5. Elektrische Kennwerte

### 5.1 Absolute Grenzwerte (Absolute Maximum Ratings)
* $V_{DD} - V_{SS}$: $-0{,}3\text{ V bis }+4{,}0\text{ V}$
* $V_{IN}$ (Eingangsspannung):
  * Pins mit FT-Struktur (außer FT_c): $V_{SS}-0{,}3\text{ V bis }\min(V_{DD}, V_{DDA}) + 4{,}0\text{ V}$ (max. $5{,}5\text{ V}$)
  * Pins mit FT_c-Struktur: $V_{SS}-0{,}3\text{ V bis }+5{,}5\text{ V}$
  * Pins mit TT-Struktur: $V_{SS}-0{,}3\text{ V bis }+4{,}0\text{ V}$
* Spannungsdifferenzen:
  * $|\Delta V_{DDx}| \le 50\text{ mV}$
  * $|V_{SSx} - V_{SS}| \le 50\text{ mV}$
  * $V_{REF+} - V_{DDA} \le 0{,}4\text{ V}$
* Maximale Summenströme:
  * $\sum I_{VDD}$ (Gesamtstromquelle aller $V_{DD}$): max. $150\text{ mA}$
  * $\sum I_{VSS}$ (Gesamtsenke aller $V_{SS}$): max. $150\text{ mA}$
  * $I_{VDD(PIN)}$ / $I_{VSS(PIN)}$ pro Versorgungs-/Massepin: max. $100\text{ mA}$
  * Ausgangsstrom pro I/O-Pin ($I_{IO}$): $\pm 20\text{ mA}$
  * Summe aller I/O-Ausgangsströme ($\sum I_{IO}$): max. $100\text{ mA}$
* Injektionsstrom ($I_{INJ}$):
  * Negativer Injektionsstrom ($V_{IN} < V_{SS}$): $-5\text{ mA}$
  * Positiver Injektionsstrom ($V_{IN} > V_{DD}$): $0\text{ mA}$ (nicht zulässig)
  * Summe der Injektionsströme $\sum |I_{INJ}|$: max. $\pm 25\text{ mA}$
* Sperrschichttemperatur ($T_J$): max. $150\text{ }^\circ\text{C}$
* Lagertemperatur ($T_{STG}$): $-65\text{ bis }+150\text{ }^\circ\text{C}$

### 5.2 Allgemeine Betriebsbedingungen
* **Taktfrequenzen:**
  * $f_{HCLK}$ (AHB): bis zu $170\text{ MHz}$
  * $f_{PCLK1}$ (APB1): bis zu $170\text{ MHz}$
  * $f_{PCLK2}$ (APB2): bis zu $170\text{ MHz}$
* **Versorgungsspannungen:**
  * $V_{DD} = 1{,}71\text{ V bis }3{,}6\text{ V}$
  * $V_{DDA}$ (bei Nutzung von ADC/COMP): $1{,}62\text{ V bis }3{,}6\text{ V}$
  * $V_{DDA}$ (bei Nutzung von DAC): $1{,}71\text{ V bis }3{,}6\text{ V}$
  * $V_{DDA}$ (bei Nutzung von OPAMP): $2{,}0\text{ V bis }3{,}6\text{ V}$
  * $V_{DDA}$ (bei Nutzung von VREFBUF): $2{,}4\text{ V bis }3{,}6\text{ V}$
  * $V_{BAT} = 1{,}55\text{ V bis }3{,}6\text{ V}$
* **Umgebungstemperatur ($T_A$ für Suffix 6):** $-40\text{ bis }+85\text{ }^\circ\text{C}$ (bei voller Verlustleistung), bis $+105\text{ }^\circ\text{C}$ (im Low-Power-Betrieb mit reduzierter Verlustleistung).
* **Zulässige Sperrschichttemperatur ($T_J$ für Suffix 6):** $-40\text{ bis }+105\text{ }^\circ\text{C}$.

### 5.3 Reset- und Power-Control-Schwellwerte
* **Brown-Out Reset (BOR):**
  * Stufe 0 (BOR0): Steigend $1{,}62\text{ bis }1{,}70\text{ V}$ (Typ. $1{,}66\text{ V}$), Fallend $1{,}60\text{ bis }1{,}69\text{ V}$ (Typ. $1{,}64\text{ V}$).
  * Stufe 1 (BOR1): Steigend Typ. $2{,}10\text{ V}$, Fallend Typ. $2{,}00\text{ V}$.
  * Stufe 2 (BOR2): Steigend Typ. $2{,}31\text{ V}$, Fallend Typ. $2{,}20\text{ V}$.
  * Stufe 3 (BOR3): Steigend Typ. $2{,}61\text{ V}$, Fallend Typ. $2{,}52\text{ V}$.
  * Stufe 4 (BOR4): Steigend Typ. $2{,}90\text{ V}$, Fallend Typ. $2{,}81\text{ V}$.
* **Reset-Verzögerung ($t_{RSTTEMPO}$):** Typ. $250\text{ }\mu\text{s}$, Max. $400\text{ }\mu\text{s}$ nach Erreichen von BOR0.

### 5.4 Interne Referenzspannung ($V_{REFINT}$) & Kalibrierungswerte
* **Spannung ($V_{REFINT}$):** $1{,}182\text{ V (min)} \dots 1{,}212\text{ V (typ)} \dots 1{,}232\text{ V (max)}$ bei $-40\text{ bis }+130\text{ }^\circ\text{C}$.
* **Temperaturkoeffizient:** Typ. $30\text{ ppm/}^\circ\text{C}$, Max. $50\text{ ppm/}^\circ\text{C}$.
* **Startzeit ($t_{start\_vrefint}$):** Typ. $8\text{ }\mu\text{s}$, Max. $12\text{ }\mu\text{s}$.
* **Abtastzeit ($t_{S\_vrefint}$):** Min. $4\text{ }\mu\text{s}$.
* **Speicheradressen der Werkskalibrierungsdaten (Read-Only im System Memory):**
  * **$V_{REFINT}$-Kalibrierung:** `0x1FFF 75AA - 0x1FFF 75AB` (Rohwert bei $30\text{ }^\circ\text{C} \pm 5\text{ }^\circ\text{C}$, $V_{DDA} = V_{REF+} = 3{,}0\text{ V} \pm 10\text{ mV}$).
  * **Temperatursensor `TS_CAL1`:** `0x1FFF 75A8 - 0x1FFF 75A9` (Rohwert bei $30\text{ }^\circ\text{C} \pm 5\text{ }^\circ\text{C}$, $V_{DDA} = 3{,}0\text{ V}$).
  * **Temperatursensor `TS_CAL2`:** `0x1FFF 75CA - 0x1FFF 75CB` (Rohwert bei $130\text{ }^\circ\text{C} \pm 5\text{ }^\circ\text{C}$, $V_{DDA} = 3{,}0\text{ V}$).

### 5.5 Stromverbrauch (Typische Werte bei $V_{DD} = 3{,}0\text{ V}$, $25\text{ }^\circ\text{C}$)
* **Run-Modus (Code-Ausführung aus Flash, ART aktiv, Single Bank):**
  * 170 MHz (Range 1 Boost): $29{,}5\text{ mA}$ (Typ), $31{,}0\text{ mA}$ (Max bei $25\text{ }^\circ\text{C}$).
  * 150 MHz (Range 1 Normal): $24{,}5\text{ mA}$.
  * 26 MHz (Range 2): $3{,}65\text{ mA}$.
* **Run-Modus (Code-Ausführung aus SRAM1):**
  * 170 MHz: $26{,}0\text{ mA}$.
  * 150 MHz: $21{,}5\text{ mA}$.
* **Low-Power Run (2 MHz, LPR aktiv):** ca. $455\text{ }\mu\text{A}$ (HSE-Bypass) bzw. $920\text{ }\mu\text{A}$ (HSI16).
* **Stop 1 Modus:**
  * RTC aus: Typ. $80{,}5\text{ }\mu\text{A}$ (Max. $640\text{ }\mu\text{A}$ bei $25\text{ }^\circ\text{C}$).
  * RTC aktiv (via LSE Quartz low-drive): Typ. $84{,}5\text{ }\mu\text{A}$.
* **Stop 0 Modus:** Typ. $190\text{ }\mu\text{A}$.
* **Standby-Modus (ohne RTC, $3{,}0\text{ V}$):** Typ. $130\text{ nA}$ (Max. $240\text{ nA}$ bei $25\text{ }^\circ\text{C}$).
* **Standby-Modus (mit RTC über LSE-Quarz):** Typ. $655\text{ nA}$.
* **Zusatzverbrauch SRAM2-Erhalt im Standby:** Typ. $305\text{ nA}$.
* **Shutdown-Modus (ohne RTC):** Typ. $43\text{ nA}$ (Max. $130\text{ nA}$ bei $25\text{ }^\circ\text{C}$).
* **$V_{BAT}$-Modus (RTC über LSE-Quarz, $3{,}0\text{ V}$):** Typ. $525\text{ nA}$.

### 5.6 Aufwachzeiten aus Low-Power-Modi
* **Sleep $\to$ Run:** 11 CPU-Zyklen.
* **Stop 0 $\to$ Run (im Flash, Range 1):** $5{,}8\text{ }\mu\text{s}$ (SRAM1: $2{,}8\text{ }\mu\text{s}$).
* **Stop 1 $\to$ Run (im Flash, Range 1):** $9{,}5\text{ }\mu\text{s}$ (SRAM1: $6{,}6\text{ }\mu\text{s}$).
* **Standby $\to$ Run:** $29{,}7\text{ }\mu\text{s}$.
* **Shutdown $\to$ Run:** $267{,}9\text{ }\mu\text{s}$.

### 5.7 Oszillatoreigenschaften
* **HSE (High-Speed External):**
  * Frequenz: $4\text{ bis }48\text{ MHz}$.
  * Startup-Zeit ($t_{SU(HSE)}$): Typ. $2\text{ ms}$.
  * Transkonduktanz ($G_m$): Min. $1{,}5\text{ mA/V}$.
  * Externe Beschaltung: $C_{L1}, C_{L2}$ typisch $5\text{ bis }20\text{ pF}$.
* **LSE (Low-Speed External, $32{,}768\text{ kHz}$):**
  * Startup-Zeit: Typ. $2\text{ s}$.
  * Konfigurierbare Treiberstärken: Low ($0{,}5\text{ }\mu\text{A/V}$), Med-Low ($0{,}75\text{ }\mu\text{A/V}$), Med-High ($1{,}7\text{ }\mu\text{A/V}$), High ($2{,}7\text{ }\mu\text{A/V}$).
  * Kein externer Serienwiderstand zwischen OSC32_IN und OSC32_OUT zulässig.
* **HSI16 (Interner 16 MHz RC):**
  * Genauigkeit ab Werk ($25\text{ }^\circ\text{C}$): $\pm 0{,}5\,\%$.
  * Drift über Temperatur ($-40\text{ bis }125\text{ }^\circ\text{C}$): $-2\,\%\text{ bis }+1{,}5\,\%$.
  * Startzeit ($t_{su(HSI16)}$): Typ. $0{,}8\text{ }\mu\text{s}$, Max. $1{,}2\text{ }\mu\text{s}$.
  * Stromverbrauch: Typ. $155\text{ }\mu\text{A}$.
* **HSI48 (Interner 48 MHz RC):**
  * Startzeit: Typ. $2{,}5\text{ }\mu\text{s}$, Max. $6\text{ }\mu\text{s}$.
  * Verbrauch: Typ. $340\text{ }\mu\text{A}$.
* **LSI (Interner 32 kHz RC):**
  * Frequenz: $31{,}04\text{ bis }32{,}96\text{ kHz}$ (bei $30\text{ }^\circ\text{C}$).
  * Startzeit: Typ. $80\text{ }\mu\text{s}$, Max. $130\text{ }\mu\text{s}$.

### 5.8 I/O Port Eigenschaften
* **Pegelschwellen (CMOS, $1{,}62\text{ V} < V_{DD} < 3{,}6\text{ V}$):**
  * Standard I/O Eingangs-Low-Pegel ($V_{IL}$): Max. $0{,}3 \times V_{DD}$ (bzw. $0{,}39 \times V_{DD} - 0{,}06\text{ V}$)
  * FT_c Eingangs-Low-Pegel: Max. $0{,}3 \times V_{DD}$ (bzw. $0{,}25 \times V_{DD}$)
  * Standard I/O Eingangs-High-Pegel ($V_{IH}$): Min. $0{,}7 \times V_{DD}$ (bzw. $0{,}49 \times V_{DD} + 0{,}26\text{ V}$)
  * Eingangshysterese ($V_{HYS}$): Typ. $200\text{ mV}$
* **Schwache Pull-Up / Pull-Down-Widerstände ($R_{PU} / R_{PD}$):** Min. $25\text{ k}\Omega$, Typ. $40\text{ k}\Omega$, Max. $55\text{ k}\Omega$.
* **Pin-Kapazität ($C_{IO}$):** Typ. $5\text{ pF}$.
* **Ausgangsspannung ($V_{OL} / V_{OH}$ bei $V_{DD} \ge 2{,}7\text{ V}$):**
  * $V_{OL}$ bei $I_{IO} = 8\text{ mA}$: Max. $0{,}4\text{ V}$
  * $V_{OH}$ bei $I_{IO} = 8\text{ mA}$: Min. $V_{DD} - 0{,}4\text{ V}$
  * $V_{OL}$ bei $I_{IO} = 20\text{ mA}$: Max. $1{,}3\text{ V}$
  * $V_{OH}$ bei $I_{IO} = 20\text{ mA}$: Min. $V_{DD} - 1{,}3\text{ V}$
  * Fast-Mode Plus ($V_{OLFM+}$): Max. $0{,}4\text{ V}$ bei $20\text{ mA}$ ($V_{DD} \ge 2{,}7\text{ V}$) bzw. $10\text{ mA}$ ($V_{DD} \ge 1{,}62\text{ V}$).

### 5.9 ADC-Kennwerte
* **Auflösung:** 12-Bit nativ (Hardware-Oversampling bis 16-Bit einstellbar).
* **Max. Taktfrequenz ($f_{ADC}$):**
  * Range 1, Single ADC: bis $60\text{ MHz}$.
  * Range 1, alle ADCs im Single-Ended Modus ($V_{DDA} \ge 2{,}7\text{ V}$): bis $52\text{ MHz}$.
  * Range 1, alle ADCs im differentiellen Modus ($V_{DDA} \ge 1{,}62\text{ V}$): bis $56\text{ MHz}$.
* **Abtastrate ($f_S$):** Max. $4\text{ Msps}$ (bei 12-Bit Auflösung), bis zu $6{,}66\text{ Msps}$ (bei 6-Bit Auflösung).
* **Minimale Abtastzeit ($t_S$):** $41{,}67\text{ ns}$ (2,5 ADC-Taktzyklen bei $60\text{ MHz}$).
* **Gesamtwandlungszeit ($t_{CONV}$):** $t_S + 12{,}5\text{ Zyklen}$ ($0{,}25\text{ bis }10{,}88\text{ }\mu\text{s}$ bei $60\text{ MHz}$).
* **Linearität & Genauigkeit (Single ADC, Range 1, $V_{DDA}=3\text{ V}$, $25\text{ }^\circ\text{C}$):**
  * Offset-Fehler ($E_O$): Typ. $\pm 1{,}9\text{ bis }2{,}5\text{ LSB}$.
  * Gain-Fehler ($E_G$): Typ. $\pm 4{,}5\text{ bis }4{,}6\text{ LSB}$.
  * DNL ($E_D$): Typ. $\pm 1{,}1\text{ bis }1{,}3\text{ LSB}$.
  * INL ($E_L$): Typ. $\pm 2{,}3\text{ bis }2{,}4\text{ LSB}$.
  * ENOB: $10{,}4\text{ bis }10{,}6\text{ Bits}$ (Single-Ended), $10{,}8\text{ bis }10{,}9\text{ Bits}$ (Differentiell).
  * SNR / SINAD: $65\text{ dB} / 64{,}4\text{ dB}$ (Single-Ended), $69\text{ dB} / 67{,}5\text{ dB}$ (Differentiell).

### 5.10 DAC-Kennwerte
* **1 MSPS Modus (Gepuffert / Ungepuffert):**
  * DNL: Max. $\pm 2\text{ LSB}$.
  * INL: Max. $\pm 4\text{ LSB}$.
  * Offset: Max. $\pm 12\text{ LSB}$ ($V_{REF+} = 3{,}6\text{ V}$).
  * Einschwingzeit ($t_{SETTLING}$ für Vollhub $\pm 1\text{ LSB}$): Typ. $1{,}6\text{ }\mu\text{s}$, Max. $2{,}9\text{ }\mu\text{s}$.
  * Ausgangsspannungsbereich: $0{,}2\text{ V bis }V_{REF+} - 0{,}2\text{ V}$ (gepuffert), $0\text{ V bis }V_{REF+}$ (ungepuffert).
* **15 MSPS Modus:**
  * DNL: Max. $\pm 2\text{ LSB}$.
  * INL: Max. $\pm 5\text{ LSB}$.
  * Einschwingzeit ($10\,\%\text{ bis }90\,\%$, mit Komparator): Typ. $16\text{ ns}$, Max. $22\text{ ns}$ ($V_{DDA} > 2{,}7\text{ V}$).

### 5.11 Operationsverstärker (OPAMP)
* **GBW:** Min. $7\text{ MHz}$, Typ. $13\text{ MHz}$.
* **Slew Rate (SR):**
  * Normal-Modus: Typ. $6{,}5\text{ V/}\mu\text{s}$.
  * High-Speed-Modus: Typ. $45\text{ V/}\mu\text{s}$.
* **Eingangsoffsetspannung ($V_{IOFFSET}$):** Max. $\pm 1{,}5\text{ mV}$ ($25\text{ }^\circ\text{C}$).
* **Einschwingzeit / Wakeup ($t_{WAKEUP}$):** Typ. $3\text{ }\mu\text{s}$, Max. $6\text{ }\mu\text{s}$.
* **Spannungsrauschdichte ($e_N$):** $250\text{ nV/}\sqrt{\text{Hz}}$ bei $1\text{ kHz}$, $90\text{ nV/}\sqrt{\text{Hz}}$ bei $10\text{ kHz}$.

### 5.12 Komparatoren (COMP)
* **Verzögerung ($t_D$):**
  * $V_{DDA} \ge 2{,}7\text{ V}$ ($200\text{ mV}$ Schritt, $100\text{ mV}$ Overdrive, $50\text{ pF}$ Last): Typ. $16{,}7\text{ ns}$, Max. $31\text{ ns}$.
  * $V_{DDA} < 2{,}7\text{ V}$: Max. $35\text{ ns}$.
* **Offsetfehler ($V_{offset}$):** Typ. $-6/+2\text{ mV}$, Max. $\pm 9\text{ mV}$.
* **Hysterese ($V_{hys}$):** Einstellbar über 8 Stufen (`HYST[2:0]`) von $0\text{ mV}$ bis typ. $63\text{ mV}$ (max. $110\text{ mV}$).
* **Stromverbrauch pro Komparator:** Typ. $450\text{ }\mu\text{A}$, Max. $720\text{ }\mu\text{A}$.

### 5.13 VREFBUF (Spannungsreferenzpuffer)
* **Ausgangsspannungsstufen ($V_{REFBUF\_OUT}$ bei Normal Mode, $25\text{ }^\circ\text{C}$):**
  * `VRS = 00`: $2{,}048\text{ V}$ ($2{,}044\text{ V bis }2{,}052\text{ V}$) – erfordert $V_{DDA} \ge 2{,}4\text{ V}$.
  * `VRS = 01`: $2{,}500\text{ V}$ ($2{,}496\text{ V bis }2{,}504\text{ V}$) – erfordert $V_{DDA} \ge 2{,}8\text{ V}$.
  * `VRS = 10`: $2{,}900\text{ V}$ ($2{,}896\text{ V bis }2{,}904\text{ V}$) – erfordert $V_{DDA} \ge 3{,}135\text{ V}$.
* **Lastkapazität ($C_L$):** $0{,}5\text{ bis }1{,}5\text{ }\mu\text{F}$ (Typ. $1{,}0\text{ }\mu\text{F}$) plus $100\text{ nF}$ Keramikkondensator (Low ESR).
* **Startzeit ($t_{START}$ bei $C_L = 1{,}0\text{ }\mu\text{F}$):** Typ. $500\text{ }\mu\text{s}$, Max. $650\text{ }\mu\text{s}$.

### 5.14 Kommunikationsschnittstellen-Timings
* **SPI:**
  * Master Mode ($2{,}7\text{ V} \le V_{DD} \le 3{,}6\text{ V}$, Range 1): bis zu $75\text{ MHz}$.
  * Master Mode ($1{,}71\text{ V} \le V_{DD} \le 3{,}6\text{ V}$, Range 1): bis zu $50\text{ MHz}$.
  * Slave Mode Receiver: bis zu $50\text{ MHz}$.
  * Slave Mode Transmitter: bis zu $41\text{ MHz}$ ($V_{DD} \ge 2{,}7\text{ V}$).
* **$I^2C$:**
  * Standard Mode: bis $100\text{ kbit/s}$ (Min. $I^2CCLK = 2\text{ MHz}$).
  * Fast Mode: bis $400\text{ kbit/s}$ (Min. $I^2CCLK = 8\text{ MHz}$ mit Filter, $9\text{ MHz}$ ohne Filter).
  * Fast Mode Plus: bis $1\text{ Mbit/s}$ (Min. $I^2CCLK = 17\text{ MHz}$ mit Filter, $16\text{ MHz}$ ohne Filter).
  * Analogfilter unterdrückt Spikes von $50\text{ ns (min) bis }90\text{ ns (max)}$.
* **Quad-SPI:**
  * SDR Modus: bis $110\text{ MHz}$ (Range 2, $C_L=20\text{ pF}$) bzw. $50\text{ MHz}$ (Range 1).
  * DDR Modus: bis $70\text{ MHz}$ (Range 2) bzw. $50\text{ MHz}$ (Range 1).
* **USART:**
  * Max. USART-Takt ($f_{CK}$): Master $21\text{ MHz}$, Slave $22\text{ MHz}$.
* **USB:**
  * Betriebsspannung: $3{,}0\text{ V bis }3{,}6\text{ V}$ (Funktion bis $2{,}7\text{ V}$ garantiert, jedoch mit reduzierten elektrischen Parametern).
  * Ausgangsimpedanz des Treibers ($Z_{SDRV}$): $28\text{ bis }44\text{ }\Omega$ (integriert, keine externen Serienwiderstände an D+/D- nötig).
  * Integrierte Pull-Up-Widerstände: $900\text{ bis }1500\text{ }\Omega$ (Idle), $1400\text{ bis }3200\text{ }\Omega$ (Reception).

---

## 6. Gehäusedaten & Mechanische Zeichnung (UFQFPN48)

### 6.1 Mechanische Maße (UFQFPN48, $7 \times 7\text{ mm}$, 0,5 mm Pitch)

| Symbol | Millimeter (Min) | Millimeter (Typ) | Millimeter (Max) |
| :---: | :---: | :---: | :---: |
| **A** (Gesamthöhe) | 0,500 | 0,550 | 0,600 |
| **A1** (Abstand Platine) | 0,000 | 0,020 | 0,050 |
| **b** (Pinbreite) | 0,200 | 0,250 | 0,300 |
| **D** (Gehäusebreite) | 6,900 | 7,000 | 7,100 |
| **D2** (Exposed Pad X) | 5,500 | 5,600 | 5,700 |
| **E** (Gehäuselänge) | 6,900 | 7,000 | 7,100 |
| **E2** (Exposed Pad Y) | 5,500 | 5,600 | 5,700 |
| **e** (Pitch / Raster) | - | 0,500 | - |
| **L** (Kontaktlänge) | 0,300 | 0,400 | 0,500 |
| **T** | - | 0,152 | - |
| **ddd** (Koplanarität) | - | - | 0,080 |

### 6.2 PCB-Footprint-Empfehlungen (UFQFPN48)
* **Gesamtbreite/-länge (über Pads):** $7{,}30\text{ mm} \times 7{,}30\text{ mm}$
* **Exposed Pad Kontaktfläche:** $5{,}60\text{ mm} \times 5{,}60\text{ mm}$
* **Pad-Abmessungen:** $0{,}30\text{ mm} \times 0{,}75\text{ mm}$
* **Pitch:** $0{,}50\text{ mm}$
* **Abstand Padkante zu Exposed Pad:** $0{,}20\text{ mm}$

### 6.3 Thermische Kennwerte für UFQFPN48
* **Wärmewiderstand Sperrschicht-Umgebung ($\Theta_{JA}$):** $26{,}8\text{ }^\circ\text{C/W}$
* **Wärmewiderstand Sperrschicht-Gehäuseboden / Pad ($\Theta_{JC}$):** $2{,}0\text{ }^\circ\text{C/W}$
* **Wärmewiderstand Sperrschicht-Gehäuseoberseite ($\Theta_{JC}$):** $7{,}5\text{ }^\circ\text{C/W}$
* **Wärmewiderstand Sperrschicht-Board ($\Theta_{JB}$):** $11{,}0\text{ }^\circ\text{C/W}$

### 6.4 Gehäusebeschriftung (Top View Device Marking)
Die Lasermarkierung auf dem Gehäuse des STM32G474CEU6 ist wie folgt strukturiert:

```
+--------------------------+
|  STM32G474               |  <-- Produktfamilie / Modell
|  CEU6                    |  <-- C=48 Pin, E=512KB Flash, U=UFQFPN, 6=-40..+85°C
|  [     ] [     ] [     ] |  <-- Interne Fertigungscodes
|              [ Y ][ WW ] |  <-- Date Code (Jahr Y, Kalenderwoche WW)
|  [ST-Logo]   ( )   [ B ] |  <-- Revisionscode (z. B. "B"), Pin-1-Indexbohrung
|    O                     |  <-- Pin-1-Punktmarkierung (Laser)
+--------------------------+
```

---

## 7. Beschaltungsempfehlungen (Hardware Design Guidelines)

1. **Spannungsversorgung und Abblockung:**
   * Jeder $V_{DD}$-Pin muss mit einem $100\text{ nF}$ Keramikkondensator gegen $V_{SS}$ abgeblockt werden.
   * Zusätzlich ist ein zentraler Pufferkondensator von $4{,}7\text{ }\mu\text{F}$ an $V_{DD}$ vorzusehen.
   * $V_{DDA}$ erfordert eine Parallelschaltung aus $10\text{ nF}$ und $1\text{ }\mu\text{F}$ Keramikkondensatoren direkt am Pin gegen $V_{SSA}$.
   * An $V_{REF+}$ muss bei Nutzung des internen Referenzpuffers ein Kondensator von $1{,}0\text{ }\mu\text{F}$ (Low ESR) parallel zu $100\text{ nF}$ gegen $V_{SSA}$ platziert werden.
2. **NRST-Pin:**
   * Der NRST-Pin verfügt über einen internen Pull-Up-Widerstand von typ. $40\text{ k}\Omega$.
   * Es wird empfohlen, extern einen Kondensator von $100\text{ nF}$ gegen Masse vorzusehen, um Störimpulse zu filtern.
3. **Masseanbindung (Thermal Pad):**
   * Das Exposed Pad unter dem UFQFPN48 bildet den primären Masse- und Wärmepfad. Es muss über mehrere Vias (Durchkontaktierungen) direkt mit der internen GND-Lage der Leiterplatte verbunden werden.
