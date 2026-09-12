# HAL

## embassy-rs/stm32-data  

is a technical pipeline and workspace project that aggregates STMicroelectronics vendor data—such as CubeDB XMLs, CMSIS-Packs, C headers, and SVDs—alongside hand-maintained register YAML definitions, transforming them into structured JSON chip metadata and generating the stm32-metapac Rust Peripheral Access Crate (PAC)
README.md1-4
README.md74-91

The project solves the inconsistency and error-prone nature of vendor-supplied SVD files by replacing automated patching systems with a clean, centralized set of register YAMLs (data/registers/) and robust parser modules (stm32-data-gen), enabling uniform driver development across all STM32 microcontroller families

## embassy-stm32 in embassy-rs/embassy

The embassy-stm32 HAL aims to provide a safe, idiomatic hardware abstraction layer for all STM32 families. The HAL implements both blocking and async APIs for many peripherals. Where appropriate, traits from both blocking and asynchronous versions of embedded-hal v0.2 and v1.0 are implemented, as well as serial traits from embedded-io[-async].

    embassy-stm32 on crates.io
    Documentation (Important: use docs.embassy.dev rather than docs.rs to see the specific docs for the chip you’re using!)
    Source
    Examples

embassy-stm32 supports all STM32 chip families

STM32 microcontrollers come in many families and flavors, and supporting all of them is a big undertaking. Embassy takes advantage of the fact that the STM32 peripheral versions are shared across chip families. For example, instead of re-implementing the SPI peripheral for every STM32 chip family, embassy has a single SPI implementation that depends on code-generated register types that are identical for STM32 families with the same version of a given peripheral.

In practice, this works as follows:

    You tell the compiler which chip you’re using with a feature flag
    The stm32-metapac module generates register types for that chip at compile time, based on data from the stm32-data module
    The embassy-stm32 HAL picks the correct implementation each peripheral based on automatically-generated feature flags, and applies any other tweaks which are required for the HAL to work on that chip

Be aware that, while embassy-stm32 strives to consistently support all peripherals across all chips, this approach can lead to slightly different APIs and capabilities being available on different families. Check the documentation for the specific chip you’re using to confirm exactly what’s available.
Embedded-hal

The embassy-stm32 HAL implements the traits from embedded-hal (v0.2 and 1.0) and embedded-hal-async, as well as embedded-io and embedded-io-async.
embassy-time time driver

If a time-driver-* feature is enabled, embassy-stm32 provides a time driver for use with embassy-time. You can pick which hardware timer is used for this internally via the time-driver-tim* features, or let embassy pick with time-driver-any.

embassy-time has a default tick rate of 1MHz, which is fast enough to cause problems with the 16-bit timers currently supported by the embassy-stm32 time driver (specifically, if a critical section delays an IRQ by more than 32ms). To avoid this, it’s recommended to pick a lower tick rate. 32.768kHz is a reasonable default for many purposes.
Interoperability

This crate can run on any executor.

Optionally, some features requiring embassy-time can be activated with the time feature. If you enable it, you must link an embassy-time driver in your project.

The low-power feature integrates specifically with embassy-executor, it can't be used on other executors for now.


# Example

## justinlhudson/embassy-stm32-starter

## Überblick: embassy-stm32-starter

Dieses Projekt ist ein asynchrones Rust-Embedded-Framework-Template auf Basis von [Embassy](https://embassy.dev) für STM32-Nucleo-Boards, mit Multi-Board-Support über Cargo-Features <cite repo="justinlhudson/embassy-stm32-starter" path="README.md" start="1-17" end="1-17" />.

---

### Unterstützte Hardware

| Board | MCU | Flash | RAM | Serial | LED | Button | Storage |
|---|---|---|---|---|---|---|---|
| Nucleo-144 | STM32F413ZH | 1536KB | 320KB | USART3 | PB0 | PC13 | 128KB (S15) |
| Nucleo-64 | STM32F446RE | 512KB | 128KB | USART2 | PA5 | PC13 | 128KB (S6) | [1](#0-0) 

Beide Boards implementieren dasselbe `Board`-Trait mit Konstanten für Flash/RAM-Layout, Watchdog-Timeout und Pin-Belegung [2](#0-1) . Die konkrete Initialisierung erfolgt in [3](#0-2)  bzw. [4](#0-3) . Boarderkennung beim Flashen erfolgt automatisch über die `DBGMCU_IDCODE` [5](#0-4) .

### Genutzte IP / Peripherie

- **USART + DMA** mit Idle-Line-Detection für asynchronen seriellen Empfang [6](#0-5) 
- **Flash-Controller** (Sector Erase, 4-Byte-Word-Program) für persistenten Storage [7](#0-6) 
- **IWDG (Independent Watchdog)** zur Systemüberwachung [8](#0-7) 
- **RTC** für Zeitfunktionen [9](#0-8) 
- **GPIO** (LED, Button) mit Default-Konfigurationen (`Level::Low`, `Speed::Low`, `Pull::Down`) [10](#0-9) 

### Funktionen

1. **HDLC-Framing mit optionalem CRC-16 FCS** für robuste serielle Kommunikation, Protokoll mit Ack/Nak/Ping/Raw/Version-Befehlen [11](#0-10) , implementiert über `consumer_task` und `dispatch()` [12](#0-11) .
2. **Hard-Fault-Auto-Reset**: dumpt Register über `defmt`, wartet auf RTT-Buffer-Drain, dann `SCB::sys_reset()` [13](#0-12) .
3. **Wiederverwendbare Async-Tasks**: `led_blink`, `button_monitor` (debounced), `rtc_clock` [14](#0-13) .
4. **Timing-Helfer** (`Timing`) für Button-Debounce, Watchdog-Pet-Intervall, Heartbeat, RTC-Update [15](#0-14) .
5. **HIL-Tests** (hardware-in-the-loop) über `probe-rs` direkt auf dem Zielchip [16](#0-15) .

### Anwendungsfälle (Beispiel-Binaries)

- **`example`**: kompletter Feature-Tour – Board-Init, Flash-Demo, Watchdog, Stack-Monitoring, HDLC-Comms mit automatischem Ping/Version-Echo [17](#0-16) .
- **`relay`**: HDLC-gesteuertes GPIO-Relay (PA9/Arduino D8) – Fernsteuerung eines Pins per serieller Nachricht, plus lokale Button-Steuerung und App-Policy für Reset bei anhaltenden FCS-Fehlern [18](#0-17) .

---

## Notes
- Die Frage kam von der Wiki-Seite "Timers, GPIO Defaults and Hard Fault Handling" – diese Themen sind in Abschnitt 1–3 der Datei `src/hardware/timers.rs`, `src/hardware/gpio.rs` und `src/hardware/hardfault.rs` konkret behandelt, aber der Rest der Antwort deckt das Gesamtprojekt breiter ab, wie in der Frage verlangt.
- Weitere Details zu Glossar-Begriffen (HDLC, FCS, IWDG, BSP, DMA Idle-Line etc.) sind in der Glossary-Wiki-Seite dokumentiert.

Wiki pages you might want to explore:
- [Timers, GPIO Defaults and Hard Fault Handling (justinlhudson/embassy-stm32-starter)](/wiki/justinlhudson/embassy-stm32-starter#3.3)
- [Applications and Tasks (justinlhudson/embassy-stm32-starter)](/wiki/justinlhudson/embassy-stm32-starter#5)
- [Glossary (justinlhudson/embassy-stm32-starter)](/wiki/justinlhudson/embassy-stm32-starter#7)

