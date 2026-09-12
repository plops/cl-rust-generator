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


## stm32-rs/stm32-rs

The stm32-rs project serves two primary purposes: creating a source of corrected STM32 SVD files with manufacturer errors and inconsistencies fixed, and creating and publishing svd2rust-generated crates covering all STM32 device families. For detailed information about the build system mechanics, see Build System & Architecture. For device-specific configurations, see Device Configuration System.

## stm32-rs/stm32g4xx-hal

stm32g4xx-hal is a hardware abstraction layer (HAL) crate providing safe, idiomatic, and multi-device peripheral APIs on top of the peripheral access cell (PAC) for the STMicroelectronics STM32G4 series of microcontrollers. Its primary role is to sit between raw register-level register blocks (stm32g4) and standard embedded ecosystem traits (embedded-hal), turning unsafe memory mappings into robust, type-checked hardware drivers
