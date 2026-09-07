# Building the Pico 2 example (23_embassy_pico)

All Rust sources are generated from Lisp (`gen00_proto.lisp`,
`gen10_firmware.lisp`, `gen11_fwproj.lisp`, `gen20_host.lisp`) via
`cl-rust-generator`. Edit the `.lisp` files, never the generated
`.rs`/`.toml` files by hand. Generators run with:

```sh
sbcl --non-interactive \
  --eval '(push #P"/workspace/src/cl-rust-generator/" asdf:*central-registry*)' \
  --load examples/23_embassy_pico/gen10_firmware.lisp
```

(If `ql:quickload` cannot find `cl-rust-generator`, the `push` line above
is the fix: it registers the local system with ASDF directly.)

## Toolchain (verified 2026-09-07)

- Rust 1.98.1 (`rustc`/`cargo`), edition 2024 everywhere.
- RP2350 target: `rustup target add thumbv8m.main-none-eabihf`
- `probe-rs 0.32.0` for flashing (`--chip RP235x` is configured as runner).
- Host build needs libudev headers for `serialport`: `apt install libudev-dev`.
- SBCL 2.6.0 for the generators.

## Protocol crate (host + firmware shared)

```sh
cd examples/23_embassy_pico/proto
cargo test          # 8 unit tests (framing, CRC, commands, block layout)
cargo clippy --all-targets
cargo fmt --check
```

## Firmware (`fw_pico2`, Pico 2 / RP2350)

```sh
cd examples/23_embassy_pico/fw_pico2
cargo check --target thumbv8m.main-none-eabihf
cargo build --target thumbv8m.main-none-eabihf            # debug, links clean
cargo build --release --target thumbv8m.main-none-eabihf  # LTO, s-opt
cargo clippy --target thumbv8m.main-none-eabihf
cargo fmt --check
probe-rs run --chip RP235x target/thumbv8m.main-none-eabihf/debug/pico2-fw
```

Linker notes: `build.rs` passes `-Tlink.x` (cortex-m-rt) plus a generated
`link-rp235x.x` that pins embassy's `IMAGE_DEF` (`.start_block`) into the
`BOOT2` slot at flash offset 0. `memory.x` starts `FLASH` at `0x10000200`
because the 276-byte RP2350 vector table needs 512-byte alignment
(cortex-m-rt `ASSERT`, see walkthrough). ELF layout was verified with
`readelf -S`: `.start_block` at `0x10000000`, vector table at `0x10000200`.

UART wiring: GP0 = TX, GP1 = RX, 115200 baud, 8N1.

## Host TUI (`host_ctl`, Linux)

```sh
cd examples/23_embassy_pico/host_ctl
cargo test          # 5 unit tests (step helpers, focus names)
cargo build
./target/debug/pico-host-ctl --port /dev/ttyUSB0 --baud 115200
./target/debug/pico-host-ctl --demo   # no hardware: simulated device
```

Keys: `q` quit, `Tab` focus (PWM/HSTX/ADC), `1-4` PWM channel,
`-/=` frequency, `[/]` amplitude, `;`/`'` phase, `s` send focused,
`c` cycle cap-select, `v` cycle block channel, `b` request 128-sample block.

Note: `serialport` opens the port with a 50 ms read timeout; timeouts are
normal and silently ignored by the receive pump.

## Troubleshooting

- `duplicate symbol: __pender`: never enable `executor-thread` on
  `embassy-rp` when `embassy-executor` already provides the thread-mode
  executor. This project keeps executor features on `embassy-executor` only.
- `vector table misalignment` at link time: keep `FLASH` origin 512-aligned.
- `No such file or directory` on open: check `--port` and dialout group.
- Blank TUI: the terminal must report a nonzero size (`stty size`).
