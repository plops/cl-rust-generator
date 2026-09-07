# Tasks — Pico 2 Embassy example via lisp-to-rust transpiler (plan 20260906_03_pico)

Goal (from `prompt.txt`): generate an example program with the lisp-to-rust
transpiler: Linux TUI control software + Embassy firmware on Pi Pico 2,
talking UART with a simple framed/checksummed/serialized message protocol.
Firmware: 4 ADC channels (temp @10 Hz, others max speed), GPIO capacitive
measurement via PIO, PWM sine with per-channel freq/amplitude/phase,
HSTX PWM sine via DMA, adjustable ADC rate + PWM-phase trigger, ADC sample
blocks readable over UART. Sources live in
`cl-rust-generator/examples/23_embassy_pico`, generated from `.lisp` files.
Plus: rustup SDK setup notes, build/flash docs, unit tests, `walkthrough.md`.

- [x] T1 Toolchain: rustup targets (`thumbv8m.main-none-eabihf` for RP2350
      ARM, host target present), `probe-rs`, `flip-link`; record versions
      → done 2026-09-07: Rust 1.98.1, probe-rs 0.32.0, no flip-link
      (build.rs links `-Tlink.x` directly); details in walkthrough.md
- [x] T2 API research: pin down `embassy-rp` version with RP2350 support and
      exact APIs for ADC / PWM / PIO / UART / HSTX+DMA; record in walkthrough
      → done: embassy-rp 0.10.0 (`rp235xa`), see walkthrough.md T2
- [x] T3 Protocol crate (`proto`): `FRAME {LEN, PAYLOAD, CRC16}` + message
      enum (set_pwm, set_hstx, set_adc, read_block, adc_data, cap_data…);
      shared by firmware + host; `no_std` compatible; unit tests (frame
      round-trip, crc error, truncated input, max-len reject)
      → done: 8/8 tests pass
- [x] T4 Firmware gen (`gen10_firmware.lisp` → `fw_pico2/`): ADC task
      (ch0-2 fast + temp 10 Hz), PIO capacitive task, PWM sine task
      (all slices, per-channel params), HSTX+DMA sine task, UART cmd loop,
      sample-block buffer; `memory.x`, `.cargo/config.toml`, `build.rs`
      → done: debug+release link for thumbv8m.main-none-eabihf
      (project files from new `gen11_fwproj.lisp`)
- [x] T5 Host gen (`gen20_host.lisp` → `host_ctl/`): serial UART client +
      TUI (ratatui or cursive) with panels for PWM/HSTX/ADC/cap/live values
      → done: ratatui 0.30.2 TUI + `--demo` mode, 5/5 tests pass
- [x] T6 Verify: `cargo test -p proto`, `cargo clippy`, `cargo fmt --check`
      on generated code; `cargo check` firmware for
      `thumbv8m.main-none-eabihf` (needs network for embassy deps)
      → done: proto 8/8, host 5/5, fw debug+release link, fmt clean;
      on-hardware probe-rs flash NOT done (no hardware in container)
- [x] T7 Docs: `BUILDING.md` (install → build → flash → run), fix/extend
      `doc/*.md` where wrong, write `walkthrough.md` here, commit
      → done: doc/BUILDING.md + walkthrough.md, committed

Notes
- Keep each `.lisp` generator runnable via `sbcl --load genXX.lisp` and
  emitting `cargo fmt`-clean Rust (verify with `cargo fmt --check`).
- If T6 firmware check fails (no network/hw), record exact error + version
  pin in walkthrough; host + proto tests must still pass.
