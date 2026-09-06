https://rexai.top/en/tutorials/rust/rust-embassy-embedded-rp2040/

Ditch Arduino: Write Microcontrollers in Rust + Embassy — Blink an LED and Learn Async Bare-Metal Programming

Arduino uses C++, where a few dozen KB of RAM means array overruns and dangling pointers slip past the compiler. This guide gets you running Rust bare-metal (no_std) plus the Embassy async framework on a Raspberry Pi Pico (RP2040): toolchain, Cargo.toml, memory.x, an async blink, and 5 beginner pitfalls.
August 8, 2026 · 4 min · 809 words · Rexai Programming0

Arduino uses C++. It’s fast to write, but on a microcontroller with only a few dozen KB of RAM, the two scariest things are array overruns and dangling pointers — and C++ won’t stop you at compile time. When it blows up, you get a hard reset in the field. Rust’s bare-metal mode (no_std) brings ownership and borrow checking to the microcontroller, and Embassy uses async/await to solve the age-old problem of “how do I write bare-metal multitasking.”

This guide uses a $2 Raspberry Pi Pico (RP2040) to get your first Embassy program running: blinking the on-board LED once per second.
Why Embassy, instead of poking registers by hand

Traditional bare-metal code either busy-waits with blocking delay or pulls in an RTOS. Embassy takes a different approach:

    Cooperative async executor: each task is an async fn that yields on I/O with .await, and the scheduler runs something else. Switching is almost free and no per-task kernel stack is needed.
    Safe hardware abstraction layer: GPIO, UART, I2C, SPI, and USB all have safe Rust APIs, so you don’t have to hand-tremble your way into registers.
    DMA is a natural fit for async: waiting on a peripheral like a serial transceiver is far cleaner with async than with blocking calls.

In one line: lighter than an RTOS, safer than hand-written registers.
Set up the toolchain

# 1. Install the Rust target triple (RP2040 is Cortex-M0+)
rustup target add thumbv6m-none-eabi

# 2. Install probe-rs (flashes firmware and monitors serial, replacing OpenOCD)
cargo install probe-rs --locked

    STM32 and other Cortex-M4/M7 boards use thumbv7em-none-eabihf — don’t install the wrong one.

Connect the Pico over USB, hold the BOOTSEL button while plugging in, and it shows up as a USB drive; probe-rs also needs this state to flash (some boards enter the bootloader automatically).
Project layout

pico-blinky/
├── Cargo.toml
├── .cargo/
│   └── config.toml
├── memory.x
├── build.rs
└── src/
    └── main.rs

Cargo.toml

[package]
name = "pico-blinky"
version = "0.1.0"
edition = "2021"

[dependencies]
embassy-rp = { version = "0.1", features = ["rp2040"] }
embassy-executor = { version = "0.1", features = ["arch-cortex-m", "executor-thread", "integrated-timers"] }
embassy-time = "0.1"
static-cell = "0.2"
cortex-m = { version = "0.7", features = ["inline-asm"] }
cortex-m-rt = "0.7"
defmt = "0.3"
defmt-rtt = "0.4"
panic-probe = { version = "0.3", features = ["print-defmt"] }

[profile.release]
opt-level = "s"
debug = 0
lto = true

    Use the latest versions on crates.io; the features names may shift between versions — just follow the compiler’s hints.

.cargo/config.toml

[build]
target = "thumbv6m-none-eabi"

[target.thumbv6m-none-eabi]
runner = "probe-rs run --chip RP2040"

memory.x

MEMORY
{
  BOOT2 : ORIGIN = 0x10000000, LENGTH = 0x100
  FLASH : ORIGIN = 0x10000100, LENGTH = 2048K - 0x100
  RAM   : ORIGIN = 0x20000000, LENGTH = 264K
}

build.rs

fn main() {
    // Tell the linker to use Embassy's provided linker script
    println!("cargo:rustc-link-arg-bins=-Tlink.x");
}

src/main.rs — the core

#![no_std]
#![no_main]

use defmt::info;
use embassy_executor::Spawner;
use embassy_rp::gpio::{Level, Output};
use embassy_time::Timer;
use {defmt_rtt as _, panic_probe as _};

#[embassy_executor::main]
async fn main(_spawner: Spawner) {
    // Initialize RP2040 peripherals
    let p = embassy_rp::init(Default::default());
    // On-board LED is on GPIO 25
    let mut led = Output::new(p.PIN_25, Level::Low);

    loop {
        info!("led on");
        led.set_high();
        Timer::after_secs(1).await; // async wait — scheduler can run other tasks meanwhile
        info!("led off");
        led.set_low();
        Timer::after_secs(1).await;
    }
}

Note the #[embassy_executor::main] macro — it wraps your async fn main into Embassy’s executor so you don’t write the executor bootstrap yourself. The line Timer::after_secs(1).await is the key: it’s an async wait, so the CPU is free to do other work during that second instead of spinning.
Flash and run

cargo run --release

On the first flash, make sure the Pico is in BOOTSEL mode. On success the on-board LED blinks once per second and the terminal prints:

0.001 INFO led on
1.002 INFO led off
2.002 INFO led on

5 beginner pitfalls

    Wrong target triple: RP2040 is thumbv6m-none-eabi, STM32F4 is thumbv7em-none-eabihf. Get it wrong and compilation fails outright.
    No logs: you must have defmt + defmt-rtt + panic-probe, and the runner in .cargo/config.toml must point at the right chip (--chip RP2040).
    Forgot BOOTSEL: when probe-rs run can’t connect the first time, nine times out of ten you didn’t enter the bootloader.
    A loop without .await starves tasks: Embassy is cooperative — if one task hogs the CPU, the others never get a turn.
    Assuming println!/Vec work: under no_std there is no standard library; for dynamic allocation pull in heapless or embedded-alloc explicitly.

Next steps

    Add WiFi to a Pico W with embassy-rp’s cyw43 crate and report temperature/humidity over the network.
    Turn the Pico into a USB keyboard or serial device with embassy-usb.
    Run multiple async fns concurrently with spawner.spawn(...) to feel cooperative multitasking.

Rust moves “memory safety” from runtime to compile time, and Embassy brings it to microcontrollers that don’t even have an OS — a combination every IoT builder should try at least once.
Frequently Asked Questions
What is no_std, and why do microcontrollers need it?

The standard library (std) relies on an OS for the heap, threads, filesystem, and so on — and a microcontroller has no OS. no_std means you don't link std; you keep only the core language and the bare-metal-friendly crates you explicitly pull in, which shrinks the final firmware to a few tens of KB.
How is Embassy different from a traditional RTOS like FreeRTOS?

Embassy implements cooperative multitasking with Rust's async/await: task switching is nearly free, no per-task kernel stack or preemptive scheduler is needed, so it saves both RAM and power. The trade-offs are a slightly larger binary and the rule that a task that loops without .await will starve the others.
Can I run this on a PC without a dev board?

Yes. Embassy ships std examples you can run with cargo run on a PC to validate the async logic. You only need a Pico board and probe-rs to flash real firmware.
Which pin is the RP2040's on-board LED on?

The Raspberry Pi Pico's on-board LED is on GPIO 25, which is PIN_25 in embassy_rp. For an external LED just use any gpio pin.
