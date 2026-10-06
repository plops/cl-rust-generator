# Abhängigkeiten (`<organization>/<project>`)

## GPU-Kompilierung & Laufzeit (primär)

- `NVlabs/cuda-oxide` (enthält `NVIDIA/cuda-rust`-Historie; Monorepo mit
  `cuda-device`, `cuda-host`, `cuda-macros`, `cuda-core`, `cargo-oxide`)
  - Optionale Git-Dependencies für `cuda-device`, `cuda-host`, `cuda-core`
    (alle drei aus derselben gepinnten Revision, sonst E0277/E0308),
    aktiviert nur per `gpu`-Feature (`dep:`-Syntax). Ohne das Feature baut
    das Projekt auf CPU-only-Systemen ohne CUDA-Toolkit.
  - Doku via DeepWiki MCP (`NVIDIA/cuda-rust`) erfragt: Kernel-Autorenschaft,
    Atomics, Shared Memory, skalare Argumente, Float-Math, Thread-Intrinsics.

## Rendering, Windowing & Events

- `not-fl3/macroquad` (X11/OpenGL-Rendering, Partikelzeichnung, Maus/Tastatur)

## Mathematik & Transfers

- `bitshifter/glam-rs` (`glam`, `Vec2`, SIMD-optimiert)
- `bitflags/bytemuck` (`bytemuck`, `Pod`/`Zeroable` für GPU-Host-Transfers)

## CLI

- `bottledlactose/lexopt` (`lexopt`, minimalistischer CLI-Parser)

## Build-/Toolchain-Voraussetzungen (keine Cargo-Deps)

- `rust-lang/rustup` (Toolchain `nightly-2026-08-28` + `rust-src`, `rustc-dev`,
  `llvm-tools`, `clippy`, `rustfmt`, `rust-analyzer`)
- `llvm/llvm-project` (über `rustup`-Komponente `llvm-tools` + Systempakete
  `clang-21`, `llvm-21-dev`, `libclang-21-dev` für `cuda-bindings`/bindgen)
- NVIDIA CUDA-Toolkit 13.4 (`/usr/local/cuda`: `nvcc`, `libNVVM`,
  `nvJitLink`, `libdevice`) + Treiber 610.x
