## Vergleich von STM32-Entwicklungsökosystemen in Rust
In der Rust-Embedded-Landschaft existieren zwei grundlegende Herangehensweisen zur Unterstützung von STM32-Mikrocontrollern: das traditionelle, auf gerätespezifischen SVD-Dateien basierende Ökosystem (stm32-rs) und das moderne, datenbankgestützte sowie asynchrone Ökosystem (embassy-rs).
Die folgende Übersicht vergleicht die Kernkomponenten dieser beiden Ansätze hinsichtlich ihrer Architektur, Abstraktionsebene und Zielsetzung.
## Direktvergleich der Komponenten

| Merkmal / Ebene | Traditioneller Ansatz (stm32-rs) | Moderner & Asynchroner Ansatz (embassy-rs) |
|---|---|---|
| Peripheral Access Crate (PAC) | Generiert via svd2rust aus gepatchten Hersteller-SVDs (z. B. stm32g4-Crate). | Generiert via stm32-metapac aus einer zentralisierten YAML-Datenbank (stm32-data). |
| Abstraktionsebene (HAL) | Monolithisch pro Familie (z. B. stm32g4xx-hal). Code wird für jede Chip-Familie neu geschrieben. | Universell über alle Familien hinweg (embassy-stm32). Code wird pro Peripherie-Version geteilt. |
| Programmiermodell | Primär blockierend (blocking). Asynchroner Code erfordert manuelle Interrupt-Handhabung. | Nativ asynchron (async/await) mit optionalen blockierenden Fallbacks. |
| Ökosystem-Traits | Implementiert embedded-hal (v0.2 / v1.0). | Implementiert embedded-hal, embedded-hal-async, embedded-io und embedded-io-async. |

------------------------------
## 1. Die Register-Ebene (Peripheral Access Crates)## stm32-rs/stm32-rs
Das Projekt stm32-rs bildet das klassische Fundament für STM32-Code in Rust. Es korrigiert fehlerhafte und inkonsistente SVD-Dateien (System View Description) des Herstellers STMicroelectronics über automatisierte Patch-Skripte. Aus diesen bereinigten XML-Dateien werden mittels des Werkzeugs svd2rust hardwarenahe Zugriffscrates (PACs) für alle STM32-Gerätefamilien generiert und auf crates.io publiziert.
## embassy-rs/stm32-data
stm32-data bricht mit dem klassischen SVD-Ansatz. Es handelt sich um eine technische Pipeline, die herstellerspezifische Datenquellen (CubeDB XMLs, CMSIS-Packs, C-Header und SVDs) aggregiert und mit manuell gepflegten Registerdefinitionen im YAML-Format kombiniert.

* Zentralisierung: Anstatt jede Chipvariante einzeln zu patchen, abstrahiert das Projekt die Peripherieregister in eine einheitliche Struktur (data/registers/).
* Metapac-Generierung: Ein Parser-Modul (stm32-data-gen) transformiert diese Daten in strukturierte JSON-Metadaten. Daraus entsteht zur Compilezeit das Crate stm32-metapac, welches die klassischen Einzelfamilien-PACs ersetzt.

------------------------------
## 2. Die Hardware-Abstraktionsschicht (HAL)## stm32-rs/stm32g4xx-hal
Die stm32g4xx-hal ist eine dedizierte Hardware-Abstraktionsschicht ausschließlich für die STM32G4-Serie. Sie baut direkt auf dem PAC stm32g4 auf.

* Architektur: Sie übersetzt unsichere Registerzugriffe (Memory Mappings) in typsichere Treiber. Jede STM32-Familie (F4, H7, G4 etc.) besitzt in diesem Modell eine eigene, separat gepflegte HAL-Crate.
* Schnittstellen: Die Treiber implementieren die synchronen Standardschnittstellen des embedded-hal-Ökosystems.

## embassy-rs/embassy (embassy-stm32)
Das Crate embassy-stm32 ist eine familienübergreifende HAL, die alle STM32-Chips in einer einzigen Codebasis unterstützt.

* Wiederverwendbarkeit: Da sich Peripherieblöcke (z. B. ein bestimmter SPI- oder UART-Typ) über verschiedene Chipfamilien hinweg gleichen, nutzt Embassy die generierten Registertypen aus stm32-metapac. Ein Treiber wird nur einmal pro Peripherieversions-Typ geschrieben, nicht pro Mikrocontroller-Familie.
* Kompilierung: Der Zielchip wird über ein Compiler-Feature-Flag ausgewählt. Die HAL wählt automatisch die passende Implementierung für die Hardwarekomponenten des jeweiligen Chips. Sie bietet sowohl blockierende als auch native asynchrone APIs.

------------------------------
## 3. Laufzeitumgebung und Interoperabilität## Asynchroner Betrieb und Zeitsteuerung

* Während klassische HALs wie stm32g4xx-hal für asynchronen Code zusätzliche Abstraktionen (wie futur_s) oder manuelle Zustandsmaschinen benötigen, ist embassy-stm32 für die asynchrone Laufzeitumgebung optimiert.
* Zeitkomponente (embassy-time): embassy-stm32 enthält integrierte Hardware-Timer-Treiber für das Zeitmanagement. Bei der Nutzung von 16-Bit-Timern wird eine Absenkung der Standard-Tickrate von 1 MHz auf beispielsweise 32,768 kHz empfohlen. Dies verhindert Überläufe und verpasste Interrupts, falls kritische Sektionen die Laufzeitumgebung kurzzeitig blockieren.

## Executor-Kompatibilität

* Universeller Einsatz: Kern-Treiber aus embassy-stm32 sind hardwareunabhängig konzipiert und können auf jedem beliebigen Rust-Executor ausgeführt werden.
* Eingeschränkte Features: Spezifische Low-Power-Funktionen und Energiesparmodi (Deep Sleep / Stop) sind eng mit dem embassy-executor verzahnt und stehen auf externen Executoren plattformbedingt nicht zur Verfügung.
