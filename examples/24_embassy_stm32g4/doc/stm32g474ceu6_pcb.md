# WeAct Studio STM32G474CEU6 Core Board – Hardware-Referenz

## 1. Übersicht & Kernspezifikationen

* **Mikrocontroller:** STM32G474CEU6
* **Architektur:** ARM Cortex-M4 mit FPU und DSP-Befehlssatz
* **Maximale Taktfrequenz:** 170 MHz
* **Flash-Speicher (ROM):** 512 KB
* **SRAM:** 128 KB (inklusive 32 KB CCM-SRAM)
* **USB-Schnittstelle:** USB-C (für Stromversorgung, DFU-Bootloader und USB-Kommunikation)
* **Taktquellen:** 
  * HSE (High Speed External Crystal)
  * LSE (Low Speed External Crystal, 32.768 kHz für RTC)

---

## 2. Onboard-Elemente & Bedienelemente

* **Taster:**
  * `NRST`: Hardware-Reset des Mikrocontrollers.
  * `BOOT0`: Boot-Modus-Umschaltung (gedrückt halten beim Reset für System-Bootloader / DFU).
  * `KEY (C13)`: Frei programmierbarer Benutzertaster, verbunden mit Pin `PC13`.
* **LEDs:**
  * `P`: Power-LED (rot), leuchtet bei anliegender Betriebsspannung.
  * `L`: Frei programmierbare Benutzer-LED.
* **Lötbrücken / Konfiguration:**
  * `SB8`, `SB9`: Konfigurationsbrücken für USB-Leitungen / Pull-Ups.

---

## 3. Pinbelegung (Pinout)

Das Board verfügt über zweireihige Stiftleisten an der Ober- und Unterkante sowie eine dedizierte 4-Pin-Debug-Schnittstelle (SWD) auf der rechten Seite.

### 3.1. Debug-Schnittstelle (SWD – rechter Rand)
Von oben nach unten:
1. `GND` – Masse
2. `CLK` – SWCLK (PA14)
3. `DIO` – SWDIO (PA13)
4. `3V3` – 3.3V Referenz/Ausgang

---

### 3.2. Obere Stiftleiste (Draufsicht, von links nach rechts)

Die obere Leiste ist zweireihig angeordnet (äußere Reihe = Boardkante, innere Reihe = Richtung MCU):

| Position (v. l. n. r.) | Äußere Reihe (Boardkante) | Innere Reihe (MCU-Seite) |
|:-----------------------|:--------------------------|:-------------------------|
| **Pin 1**              | `3V3`                     | `3V3`                    |
| **Pin 2**              | `GND`                     | `GND`                    |
| **Pin 3**              | `PB11`                    | `PB10`                   |
| **Pin 4**              | `V+` (5V / VBUS)          | `PB2`                    |
| **Pin 5**              | `PB1`                     | `PB0`                    |
| **Pin 6**              | `PC4`                     | `PA7`                    |
| **Pin 7**              | `PA6`                     | `PA5`                    |
| **Pin 8**              | `PA4`                     | `PA3`                    |
| **Pin 9**              | `PA2`                     | `PA1`                    |
| **Pin 10**             | `PA0`                     | `NRST`                   |
| **Pin 11**             | `PC15`                    | `PC14`                   |
| **Pin 12**             | `PC13` (User Key)         | `VB` (VBAT)              |

---

### 3.3. Untere Stiftleiste (Draufsicht, von links nach rechts)

Die untere Leiste ist zweireihig angeordnet (innere Reihe = Richtung MCU, äußere Reihe = Boardkante):

| Position (v. l. n. r.) | Innere Reihe (MCU-Seite) | Äußere Reihe (Boardkante) |
|:-----------------------|:-------------------------|:--------------------------|
| **Pin 1**              | `VCC`                    | `VCC`                     |
| **Pin 2**              | `GND`                    | `GND`                     |
| **Pin 3**              | `PB13`                   | `PB12`                    |
| **Pin 4**              | `PB15`                   | `PB14`                    |
| **Pin 5**              | `PA8`                    | `PC6`                     |
| **Pin 6**              | `PA10`                   | `PA9`                     |
| **Pin 7**              | `PA12`                   | `PA11`                    |
| **Pin 8**              | `PC10`                   | `PA15`                    |
| **Pin 9**              | `PB3`                    | `PC11`                    |
| **Pin 10**             | `PB5`                    | `PB4`                     |
| **Pin 11**             | `PB7`                    | `PB6`                     |
| **Pin 12**             | `PB9`                    | `PB8`                     |

---

## 4. Wichtige Spannungs- und Versorgungsanschlüsse

* **`VCC` / `V+`:** Direkte Verbindung zur 5V-Versorgung (USB VBUS oder externe Einspeisung).
* **`3V3`:** Ausgang des internen Linearreglers (LDO) bzw. Eingang bei reiner 3.3V-Speisung.
* **`VB` (VBAT):** Pufferbatterieanschluss für RTC (Real-Time Clock) und Backup-Register.
* **`GND`:** Massepotenzial.

---

## 5. Wesentliche MCU-Funktionalitäten (STM32G474)

* **High-Resolution Timer (HRTIM):** Spezialisierter Timer mit einer Auflösung im Sub-Nanosekundenbereich (ideal für Schaltnetzteile, Motorregelung, Wechselrichter).
* **Analog-Frontend:**
  * Bis zu 5 schnelle 12-Bit ADCs mit bis zu 4 MSPS.
  * Mehrere integrierte Operationsverstärker (OpAmps mit programmierbarer Verstärkung).
  * Bis zu 7 ultraschnelle Komparatoren (Rail-to-Rail).
  * Bis zu 7 DAC-Kanäle.
* **Kommunikationsschnittstellen:**
  * CAN-FD
  * I2C (inkl. Fast Mode Plus / SMBus)
  * SPI / I2S
  * USART / UART (inkl. ISO 7816, LIN, IrDA)
  * USB 2.0 Full-Speed Device.


# Information from supplier:

1. Product Informaton【Mandatory】
Product Name: Demo Board
Model: STM32G431/G474
Batch/Serial Number: \
2. MANUFACTURER & EU REPRESENTATIVE INFO【Mandatory】
 Manufacturer: Dongguan Dongcheng Weixing Electronic Studio
Room 206, Cluster 504593TB, Building 6, No.1 Dongke Road, Dongcheng Street,
Dongguan City, Guangdong Province, WeAct_TC@163.com
 EU Authorized Representative: Apex CE Specialists GmbH
Grafenberger Allee 277,40237 Düsseldorf, Info@apex-ce.com
3. Compliance Information【Mandatory】
Complies with general electronic requirements.
4. Safety & Warnings 【Mandatory by Category】
Important Safety Information
Read all instructions before use. Failure to follow these instructions may result in damage to the
product or personal injury.
 Warning: Keep the device and its cord out of reach of children under 3 years old.
 Keep away from children unless supervised by an adult.
 Do not use this product for purposes other than its intended use.
 Inspect the product before each use; do not use if damaged.
 Follow installation and operation guidelines to avoid injury.
 Do not expose to water or moisture.
5. Instructions for Use【Mandatory】
1. Before First Use: Remove all protective films from the device.
2. Charging: Connect the provided USB cable to a certified 5V power adapter.
3. After Use: Turn off the device and store it in a dry place. Keep away from children.
