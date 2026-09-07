# Walkthrough — Pico 2 Embassy example (plan 20260906_03_pico)

Date: 2026-09-06/07. Container: Ubuntu 26, Rust 1.98.1, SBCL 2.6.0,
probe-rs 0.32.0. All generated crates use Rust edition 2024.

## T1 Toolchain

- `rustup target add thumbv8m.main-none-eabihf` (RP2350 ARM target).
  `rustup` + the target were already present; `probe-rs` was installed via
  cargo (`probe-rs 0.32.0`). No flip-link: `build.rs` links
  `-Tlink.x` (cortex-m-rt 0.7.6) directly.
- Host crate needs libudev headers for `serialport 4.10.0`
  (`apt install libudev-dev`).
- Generator fix: `sbcl --load genXX.lisp` only works if ASDF finds
  `cl-rust-generator`; use `--eval '(push #P".../cl-rust-generator/"
  asdf:*central-registry*)'` instead of relying on `ql:quickload` fallback.

## T2 API research (embassy-rp 0.10.0, embassy-executor/time 0.5.x)

- Chip feature: `embassy-rp = { version = "0.10.0", features = ["rp235xa", ...] }`.
- UART: `BufferedUart::new(p.UART0, Irqs, p.PIN_0, p.PIN_1, tx_buf, rx_buf, Config::default())`
  with 115200 baud; interrupt struct must bind both `UART0_IRQ` and DMA channels.
- ADC: `embassy_rp::adc::{Adc, Channel, Config, InterruptHandler}`; temp sensor
  via `adc.temperature_sensor()`. `Adc::read` is async.
- PWM: `pwm::Pwm` per slice with `Config::default()` + `set_counter_compare`,
  phase via counter offset; 8 slices × 2 channels.
- PIO capacitive sensing: `pio::Pio` + `pio_asm!` program on PIO0, IRQ0 handler.
- HSTX: `embassy_rp::hstx::Hstx` driven by DMA (`dma::Channel`) from a
  precomputed shift-register bitstream; simplest reliable path for MHz sine.
- Executor: `embassy-executor` thread-mode executor only — enabling
  `executor-thread` on `embassy-rp` too causes `duplicate symbol: __pender`.
- `IMAGE_DEF` (embassy boot header): `.start_block` section must land in the
  `BOOT2` flash slot at `0x10000000`. Linker fix in `build.rs`: emit
  `link-rp235x.x` pinning `.start_block` into `BOOT2` at flash offset 0,
  and `memory.x` starts `FLASH` at `0x10000200` because the RP2350 vector
  table is 276 bytes and cortex-m-rt requires 512-byte alignment
  (`ASSERT` fails at `0x1000011C`-style origins). Verified via `readelf -S`:
  `.start_block` @ `0x10000000`, vectors @ `0x10000200`.

## T3 Protocol crate (`proto`)

- `FRAME { u16 LE len, payload, CRC16-CCITT }`, `no_std`-compatible
  (`core` only + optional `std::io` helper behind default feature).
- Message enum: `SetPwm { ch, freq_hz, amp, phase }`, `SetHstx`,
  `SetAdc { rate_hz, trig_phase }`, `ReadBlock { ch }`, `AdcData`,
  `CapData`, `BlockSample` (128-sample block, u16 LE).
- 8 unit tests pass: frame round-trip, CRC error reject, truncated input,
  max-len reject, command encode/decode, block layout/offset.

## T4 Firmware (`fw_pico2`, from `gen10_firmware.lisp` + `gen11_fwproj.lisp`)

- Tasks: `adc_task` (ch0–2 max speed + temp @10 Hz), `cap_task` (PIO),
  `pwm_task` (all slices, per-channel freq/amp/phase), `hstx_task`
  (DMA bitstream), `uart_task` (command loop), 128-sample block buffer
  readable over UART (`read_block` → `block_sample` frames).
- `gen11_fwproj.lisp` emits `Cargo.toml`, `memory.x`,
  `.cargo/config.toml`, `build.rs` (hence the `.cargo/` dir is generated,
  not hand-written).
- Fixes during bring-up: UART/PIO init arg order, task `#[embassy_executor::task]`
  spawn syntax, `pio_asm!` macro form, len-scope/mutability errors,
  `block_sample` payload offset, `step_phase` wrapping.
- `cargo check` + `cargo build` (debug and `--release` LTO) for
  `thumbv8m.main-none-eabihf` link clean. `cargo fmt --check` clean.
  `cargo clippy` has only benign pedantic warnings (cast precision etc.).

## T5 Host (`host_ctl`, from `gen20_host.lisp`)

- `serialport 4.10.0` client + `ratatui 0.30.2`/`crossterm 0.29.0` TUI:
  panels for PWM / HSTX / ADC / cap / live values.
- Keys: `q` quit, `Tab` focus, `1-4` channel, `-`/`=` freq, `[`/`]` amp,
  `;`/`'` phase, `s` send, `c` cap-select, `v` block channel, `b` request block.
- `--demo` mode with simulated device (no hardware needed).
- `serialport` opens with 50 ms read timeout; timeouts are silently ignored
  by the receive pump. 5 unit tests pass (step helpers, focus names).

## T6 Verify (all observed this session)

- `proto`: `cargo test` 8/8 pass; clippy clean-ish; `cargo fmt --check` clean.
- `host_ctl`: `cargo test` 5/5 pass; `cargo fmt --check` clean.
- `fw_pico2`: `cargo check` + debug/release `cargo build` for
  `thumbv8m.main-none-eabihf` link clean; `cargo fmt --check` clean.
- On-hardware `probe-rs run --chip RP235x` flash + DEFMT validation
  NOT done (no hardware in container) — remains the only unverified step.

## T7 Docs + commit

- `examples/23_embassy_pico/doc/BUILDING.md`: install → build → flash → run.
- This file: investigation results + versions.
- `doc/pins.md` (user-added board pinout) left untouched.
- Committed per `prompt.txt` ("commite die aenderungen"); pre-existing
  unrelated change `examples/02_webgcd/...` left uncommitted.
