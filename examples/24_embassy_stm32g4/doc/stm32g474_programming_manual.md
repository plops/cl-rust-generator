Hier ist die strukturierte Zusammenfassung und Konvertierung des Handbuchs **PM0214 (STM32 Cortex-M4 Programming Manual)** in Markdown, zugeschnitten auf den **STM32G474CEU6** (Arm Cortex-M4 mit FPU, MPU, NVIC, SysTick).

Irrelevante Informationen für andere Familien (wie MP1-Microprozessoren) sowie reine Byte-Offset-Tabellen wurden ausgelassen; der Fokus liegt auf Kernarchitektur, Programmiermodell, Exception Handling, Speichersystem, FPU, MPU und Systemperipherie.

---

# STM32 Cortex-M4 (STM32G474) – Programmier- und Architekturhandbuch

## 1. Architektur- und Systemübersicht

Der STM32G474 basiert auf dem **Arm 32-Bit Cortex-M4F Prozessor** (Harvard-Architektur mit 3-stufiger Pipeline) und integriert:
* Eine Hardware-Gleitkommaeinheit (**FPU**) für einfache Genauigkeit (IEEE 754 konform).
* Eine Memory Protection Unit (**MPU**) mit bis zu 8 programmierbaren Regionen.
* Einen Nested Vectored Interrupt Controller (**NVIC**) mit schnellem Exception Handling (Tail-Chaining, Late-Arriving).
* Einen integrierten System-Timer (**SysTick**, 24 Bit abwärtszählend).
* Hardware-Division und Ein-Zyklus-Multiplikation (inklusive DSP-/SIMD-Befehle).

### Kernarchitektur (Blockdiagramm)

```
       +-------------------------------------------------------------+
       | Cortex-M4 Processor Core                                    |
       |                                                             |
       |  +--------------------+         +------------------------+  |
       |  |  Processor Core    |<------->| Floating Point Unit    |  |
       |  |  (3-Stage Harvard) |         | (FPU: FPv4-SP)         |  |
       |  +--------------------+         +------------------------+  |
       |            ^                                                |
       |            |                                                |
       |  +--------------------+         +------------------------+  |
       |  | Nested Vectored    |<------->| Memory Protection      |  |
       |  | Interrupt Ctrl     |         | Unit (MPU, 8 Regionen) |  |
       |  | (NVIC)             |         +------------------------+  |
       |  +--------------------+                     ^               |
       |            ^                                |               |
       +------------|--------------------------------|---------------+
                    |                                |
       +------------v--------------------------------v---------------+
       | Bus-Matrix (I-Code, D-Code, System Bus, PPB)                |
       +-------------------------------------------------------------+
              |                      |                      |
      +-------v------+       +-------v------+       +-------v------+
      | Flash Memory |       | SRAM         |       | Peripherie   |
      +--------------+       +--------------+       +--------------+
```

---

## 2. Programmiermodell

### 2.1 Betriebsmodi und Privilegierungsstufen

* **Betriebsmodi:**
  * **Thread Mode:** Normaler Ausführungsmodus für Anwendungssoftware. Der Prozessor wechselt nach einem Reset in diesen Modus.
  * **Handler Mode:** Modus zur Ausnahmebehandlung (Exceptions/Interrupts). Die Rückkehr zum Thread-Modus erfolgt nach Abschluss des Exception Handlers. Die Ausführung im Handler-Modus ist **immer privilegiert**.
* **Privilegierungsstufen:**
  * **Privileged:** Voller Zugriff auf alle Befehle, Systemregister (NVIC, SCB, SysTick, MPU) und Peripherie.
  * **Unprivileged:** Eingeschränkter Zugriff; kein Zugriff auf SCB, NVIC, SysTick; Zugriff auf Memory/Peripherie kann über MPU gesperrt werden. Software muss den `SVC`-Befehl (Supervisor Call) nutzen, um privilegierte Dienste aufzurufen.

### 2.2 Stack-Konzept (Full-Descending)

Der Cortex-M4 implementiert zwei Stacks mit separaten Stack-Pointern (SP / R13):
* **MSP (Main Stack Pointer):** Standard nach dem Reset. Wird im Handler Mode zwingend verwendet. Empfohlen für Kernel und Interrupt-Routinen.
* **PSP (Process Stack Pointer):** Wird im Thread-Modus über das `CONTROL`-Register (Bit 1) ausgewählt. Empfohlen für Tasks/Threads unter einem RTOS.

> **Wichtig:** Nach Umschalten des Stack-Pointers via `MSR CONTROL, ...` muss unmittelbar ein `ISB`-Befehl folgen, um die Pipeline zu synchronisieren!

### 2.3 Registersatz

1. **Allzweckregister:**
   * `R0` – `R12`: 32-Bit Allzweckdatenregister.
   * `R13` (`SP`): Stack Pointer (physisch getrennt in `MSP` und `PSP`).
   * `R14` (`LR`): Link Register (speichert Rücksprungadressen oder `EXC_RETURN`-Codes).
   * `R15` (`PC`): Program Counter. Bit[0] muss immer 1 sein (zeigt Thumb-Modus an).
2. **Program Status Register (`PSR` / `xPSR`):**
   * **APSR (Application PSR):** Flags `N` (Negative), `Z` (Zero), `C` (Carry), `V` (Overflow), `Q` (Sticky DSP Saturation/Overflow), `GE[3:0]` (Greater than or Equal für SIMD).
   * **IPSR (Interrupt PSR):** Enthält die `ISR_NUMBER` (Ausnahmenummer 0–255) des aktuell aktiven Handlers.
   * **EPSR (Execution PSR):** Enthält das `T`-Bit (Thumb state, muss immer 1 sein), das `IT`-Feld (Status des `IT`-Blocks) und `ICI` (Interruptible-Continuable Instructions für `LDM`/`STM`).
3. **Ausnahmemasken-Register:**
   * **`PRIMASK`:** 1-Bit-Register. Setzen auf `1` sperrt alle konfigurierbaren Interrupts (nur NMI und HardFault bleiben aktiv). Zugriff über `CPSIE i` / `CPSID i`.
   * **`FAULTMASK`:** 1-Bit-Register. Setzen auf `1` sperrt alle Exceptions mit Ausnahme von NMI. Zugriff über `CPSIE f` / `CPSID f`.
   * **`BASEPRI`:** Definiert eine Prioritätsschwelle. Alle Interrupts mit gleicher oder niedrigerer Priorität (höherer Zahlenwert) werden blockiert.
4. **`CONTROL`-Register:**
   * `Bit 0 (nPRIV)`: 0 = Privilegiert, 1 = Unprivilegiert (nur im Thread-Modus).
   * `Bit 1 (SPSEL)`: 0 = MSP aktiv, 1 = PSP aktiv (im Handler-Modus ignoriert/immer MSP).
   * `Bit 2 (FPCA)`: Floating-Point Context Active. Zeigt an, ob FPU-Register gesichert werden müssen.

---

## 3. Speichermodell

Der Cortex-M4 bietet einen linearen, fest zugewiesenen 4-GB-Adressraum (Little-Endian):

| Adressbereich | Region | Speichertyp | Beschreibung |
| :--- | :--- | :--- | :--- |
| `0x00000000 - 0x1FFFFFFF` | Code | Normal | Flash, Boot-ROM. Befehlsausführung optimiert über I-Bus |
| `0x20000000 - 0x3FFFFFFF` | SRAM | Normal | On-Chip SRAM (inkl. Bit-Band-Bereich) |
| `0x40000000 - 0x5FFFFFFF` | Peripherie | Device (XN) | STM32-Peripherieregister (inkl. Bit-Band) |
| `0x60000000 - 0x9FFFFFFF` | Ext. RAM | Normal | Externe Speicher via FMC/QUADSPI |
| `0xA0000000 - 0xDFFFFFFF` | Ext. Device | Device (XN) | Externe Peripheriebausteine |
| `0xE0000000 - 0xE00FFFFF` | PPB | Strongly-Ordered (XN) | Private Peripheral Bus (NVIC, SCB, SysTick, MPU) |
| `0xE0100000 - 0xFFFFFFFF` | System | Device (XN) | Zusätzliche Peripherie |

* **XN (Execute Never):** Verhindert Codeausführung aus diesen Bereichen; ein Sprung dorthin löst einen `MemManage`-Fault aus.

### 3.1 Bit-Banding

Ermöglicht atomare Bit-Operationen (Read-Modify-Write in Hardware):
* **SRAM-Bereich:** 1 MB Bit-Band (`0x20000000–0x200FFFFF`) wird abgebildet auf 32 MB Alias (`0x22000000–0x23FFFFFF`).
* **Peripherie-Bereich:** 1 MB Bit-Band (`0x40000000–0x400FFFFF`) wird abgebildet auf 32 MB Alias (`0x42000000–0x43FFFFFF`).
* **Berechnungsformel:**
  $$\text{bit\_word\_offset} = (\text{byte\_offset} \times 32) + (\text{bit\_number} \times 4)$$
  $$\text{bit\_word\_addr} = \text{bit\_band\_base} + \text{bit\_word\_offset}$$

### 3.2 Synchronisationsprimitive und Barrieren

* **Exklusive Zugriffe:** `LDREX` / `STREX` (auch für Byte/Halfword) ermöglichen Semaphore ohne Sperren aller Interrupts.
  * Wenn ein Task Daten per `LDREX` liest und ein Kontextwechsel/anderer Zugriff erfolgt, schlägt das anschließende `STREX` fehl (Status = 1) $\rightarrow$ Wiederholung erforderlich.
  * `CLREX`: Setzt den lokalen Exklusiv-Monitor zurück (z. B. bei Abbruch).
* **Speicherbarrieren:**
  * **`DMB` (Data Memory Barrier):** Stellt sicher, dass vorangegangene Speicherzugriffe vor nachfolgenden Speicherzugriffen abgeschlossen sind.
  * **`DSB` (Data Synchronization Barrier):** Wartet, bis alle vorherigen Datenzugriffe vollständig abgeschlossen sind, bevor der nächste Befehl ausgeführt wird (z. B. vor Sleep/WFI oder nach MPU-Änderungen).
  * **`ISB` (Instruction Synchronization Barrier):** Leert die Prozessor-Pipeline; sorgt dafür, dass nachfolgende Befehle frisch aus dem Cache/Speicher geholt werden (z. B. nach MPU-Rekonfiguration, Vektortabellenumschaltung oder Ändern des `CONTROL`-Registers).

---

## 4. Exception- und Interrupt-Modell

### 4.1 Eigenschaften der System-Exceptions

| Nummer | IRQ-Nr. | Typ | Priorität | Beschreibung / Besonderheit |
| :---: | :---: | :--- | :---: | :--- |
| 1 | – | **Reset** | -3 (Höchste) | Asynchron; privilegierter Thread-Modus |
| 2 | -14 | **NMI** | -2 | Nicht maskierbar, asynchron |
| 3 | -13 | **HardFault** | -1 | Generisch für nicht abgefangene Fehler |
| 4 | -12 | **MemManage** | Konfigurierbar | MPU-Verletzung, XN-Zugriff (synchron) |
| 5 | -11 | **BusFault** | Konfigurierbar | Busfehler (synchron präzise oder asynchron impräzise) |
| 6 | -10 | **UsageFault** | Konfigurierbar | Undefinierte Befehle, Divide-by-Zero, unaligned Access |
| 11 | -5 | **SVCall** | Konfigurierbar | Systemaufruf via `SVC`-Befehl |
| 14 | -2 | **PendSV** | Konfigurierbar | Software-Interrupt für RTOS-Kontextwechsel |
| 15 | -1 | **SysTick** | Konfigurierbar | System-Tick-Timer-Exception |
| 16+ | 0+ | **Peripherie-IRQ**| Konfigurierbar | Peripherie-Interrupts des STM32G474 |

### 4.2 Prioritätsgruppierung (`AIRCR.PRIGROUP`)

Der Cortex-M4 unterstützt bis zu 16 Prioritätsstufen (oberste 4 Bits implementiert: `[7:4]`). Über das `AIRCR`-Register wird die Aufteilung in **Preemption Priority** (Gruppenpriorität) und **Subpriority** festgelegt:
* Nur die Gruppenpriorität entscheidet über Preemption (Schachtelung).
* Haben zwei Interrupts die gleiche Gruppenpriorität, entscheidet die Subpriorität über die Reihenfolge beim gleichzeitigen Anstehen (keine Schachtelung).
* Bei identischer Gruppen- und Subpriorität entscheidet die kleinere IRQ-Nummer.

### 4.3 Stacking & Kontextrettung

Beim Eintritt in eine Exception sichert die Hardware automatisch Register auf dem aktuellen Stack:

```
Niedrige Adresse
      |   R0
      |   R1
      |   R2
      |   R3
      |   R12
      |   LR (R14)
      |   PC (R15, Rücksprungadresse)
      |   xPSR
      +--------------------------------------
      |   (Optional FPU-Kontext falls aktiv)
      |   S0 - S15
      |   FPSCR
      |   {Aligner}
Hohe Adresse (Pre-IRQ Top of Stack)
```

* Das **Link Register (`LR`)** wird im Handler mit dem **`EXC_RETURN`**-Wert überschrieben:
  * `0xFFFFFFF9`: Rückkehr in Thread Mode, kein FP-Stack, nutzt MSP.
  * `0xFFFFFFFD`: Rückkehr in Thread Mode, kein FP-Stack, nutzt PSP.
  * `0xFFFFFFE9`: Rückkehr in Thread Mode, FP-Stack vorhanden, nutzt MSP.
  * `0xFFFFFFED`: Rückkehr in Thread Mode, FP-Stack vorhanden, nutzt PSP.

---

## 5. Core-Peripherie & System Control Block (SCB)

### 5.1 Nested Vectored Interrupt Controller (NVIC)
* **Registergruppen:**
  * `NVIC_ISERx` / `NVIC_ICERx`: Interrupt Set-Enable / Clear-Enable.
  * `NVIC_ISPRx` / `NVIC_ICPRx`: Interrupt Set-Pending / Clear-Pending.
  * `NVIC_IABRx`: Interrupt Active Bit Flags (zeigt an, ob ISR aktiv ist).
  * `NVIC_IPRx`: Byte-adressierbare Prioritätsregister (jeweils Bits `[7:4]` genutzt).
  * `NVIC_STIR`: Software Trigger Interrupt Register (ermöglicht Auslösen von IRQs per Software).

### 5.2 System Control Block (SCB)
Wesentliche Kontroll- und Statusregister:
* **`VTOR` (Vector Table Offset Register):** Verschiebt die Vektortabelle (z. B. auf Bootloader/Flash-Offsets oder ins SRAM). Bits `[29:9]` definieren die Basisadresse (mindestens auf 128 Worte ausgerichtet).
* **`AIRCR` (Application Interrupt & Reset Control Register):**
  * Zum Schreiben muss das Schlüsselwort `0x5FA` in `VECTKEY` geschrieben werden.
  * Enthält `SYSRESETREQ` (Software-System-Reset) und `PRIGROUP`.
* **`SCR` (System Control Register):**
  * `SLEEPDEEP`: Schaltet zwischen normalem Sleep und Deep Sleep (Stop/Standby im STM32) um.
  * `SLEEPONEXIT`: Automatischer Sleep-Eintritt nach dem Verlassen des letzten Handlers.
  * `SEVONPEND`: Weckt die CPU auch bei deaktivierten Interrupts über WFE auf.
* **`CCR` (Configuration and Control Register):**
  * `DIV_0_TRP`: Löst UsageFault bei Division durch 0 aus.
  * `UNALIGN_TRP`: Löst UsageFault bei unaligned Zugriffen aus.
  * `STKALIGN`: Garantiert 8-Byte-Stack-Ausrichtung bei Exception-Eintritt (Default: 1).
* **`SHCSR` (System Handler Control and State Register):** Aktiviert `MEMFAULTENA`, `BUSFAULTENA`, `USGFAULTENA`. Fehlen diese Enabless, eskalieren diese Fehler direkt zu einem `HardFault`.
* **`CFSR` / `HFSR` (Fault Status Registers):**
  * Detaillierte Ursachenbits für `UsageFault`, `BusFault`, `MemManage` und eskalierte `HardFaults`.
  * **`MMFAR` / `BFAR`:** Halten die genaue Speicheradresse fest, an der die Schutzverletzung / der Busfehler aufgetreten ist (gültig, wenn `MMARVALID` bzw. `BFARVALID` gesetzt ist).

### 5.3 SysTick Timer (STK)
Ein 24-Bit-Abwärtszähler für RTOS oder Verzögerungen:
* `STK_CTRL`: Enable, Clock-Source (AHB oder AHB/8), Interrupt-Enable (`TICKINT`), `COUNTFLAG`.
* `STK_LOAD`: Reload-Wert (Zähler läuft von `RELOAD` bis `0`). Für eine Periode von $N$ Zyklen muss $N-1$ geladen werden.
* `STK_VAL`: Aktueller Zählerstand. Schreiben eines beliebigen Wertes setzt den Zähler und `COUNTFLAG` auf 0 zurück.

### 5.4 Memory Protection Unit (MPU)
* Unterstützt **8 programmierbare Regionen** (Region 0 bis 7) mit flexibler Größe (32 Bytes bis 4 GB, Zweierpotenzen).
* Jede Region größer/gleich 256 Bytes besitzt **8 Subregionen** (steuerbar über das Subregion Disable Bitfeld `SRD`), die gezielt maskiert werden können.
* **Attribute:** Normal / Device / Strongly-ordered, Cache-Richtlinie (Write-Through, Write-Back), Shareability, Execution Never (`XN`).
* **Zugriffsrechte (`AP`):** Kein Zugriff, Nur Privilegiert (RW oder RO), Vollzugriff (RW oder RO).
* **Hintergrundregion (`PRIVDEFENA` in `MPU_CTRL`):** Wenn aktiv, greifen für den privilegierten Modus die Standard-Speicherattribute, falls keine Region zutrifft.
* **Ablauf MPU-Update:**
  1. Interrupts sperren (`__disable_irq()`).
  2. Zu ändernde Region auswählen (`MPU_RNR`) und ggf. deaktivieren.
  3. Basisadresse (`MPU_RBAR`) und Attribute/Größe/Enable (`MPU_RASR`) schreiben.
  4. Daten- und Befehlssynchronisation erzwingen: `DSB` gefolgt von `ISB`.
  5. Interrupts wieder freigeben (`__enable_irq()`).

---

## 6. Floating Point Unit (FPU)

Der STM32G474 enthält eine IEEE-754-kompatible Single-Precision FPU (FPv4-SP).

### 6.1 Aktivierung der FPU
Nach einem Reset ist die FPU standardmäßig hardwaremäßig deaktiviert. Vor der Nutzung eines Floating-Point-Befehls muss sie über das **`CPACR`**-Register (Coprocessor Access Control) aktiviert werden:

```c
// Aktivierung von CP10 und CP11 (Full Access):
SCB->CPACR |= ((3UL << 10*2) | (3UL << 11*2));
__DSB();
__ISB();
```

Wird ein FPU-Befehl ausgeführt, ohne dass die Coprozessoren freigegeben sind, tritt ein **NOCP UsageFault** auf.

### 6.2 FPU-Kontext & Lazy Stacking (`FPCCR`)
* **Lazy State Preservation (`LSPEN = 1`, Default):**
  * Bei einer Exception reserviert der Prozessor Platz auf dem Stack für die FPU-Register (`S0–S15`, `FPSCR`), sichert sie jedoch zunächst **nicht**.
  * Erst wenn im Exception Handler tatsächlich ein FPU-Befehl ausgeführt wird, werden die Register hardwaremäßig weggeschrieben.
  * Vermeidet unnötigen Overhead in ISRs, die keine Fließkommazahlen verwenden (drastische Reduzierung der Interrupt-Latenz).

---

## 7. Power Management (Sleep-Modi)

* **`WFI` (Wait For Interrupt):** Hält die CPU sofort an, bis eine zulässige Exception / ein Interrupt ansteht.
* **`WFE` (Wait For Event):** Versetzt die CPU in den Ruhezustand, sofern das 1-Bit Event-Register `0` ist. Ist das Event-Register `1`, wird es gelöscht und die CPU läuft ohne Verzögerung weiter. Aufwecken über externe Ereignisse (z. B. STM32 EXTI) oder `SEV`-Befehl.
* **Sleep-on-Exit:** Spart Taktzyklen bei reinen Interrupt-Systemen; kehrt nach einer ISR sofort in den Sleep-Modus zurück, ohne in den Thread-Modus zu springen.
