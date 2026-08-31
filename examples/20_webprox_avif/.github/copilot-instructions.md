# Copilot Instructions for cl-rust-generator

## Build, Test, and Lint Commands

- **Build all (debug):**
  ```bash
  cargo build
  ```
- **Build all (release):**
  ```bash
  cargo build --release
  ```
- **Build server only:**
  ```bash
  cargo build --bin cloud-render-srv
  ```
- **Run server (debug):**
  ```bash
  cargo run --bin cloud-render-srv -- serve
  ```
- **Run server (release):**
  ```bash
  ./target/release/cloud-render-srv serve
  ```
- **Run Macroquad client:**
  ```bash
  cargo run --bin macroquad-client
  # or
  ./target/release/macroquad-client
  ```
- **Run Iced client (if present):**
  ```bash
  cd iced-client && cargo run --bin iced-server
  # or
  cd iced-client && cargo run --release --bin iced-server
  ```
- **Test scripts:**
  - `test_scroll_latency.sh` and `test_scroll_latency_dev.sh` in `examples/20_webprox_avif/` automate scroll latency testing and server startup with various encoder settings.

## High-Level Architecture

- **Server (`cloud-render-srv`)**: Automates Chrome, captures screenshots, encodes as AV1 (rav1e), and streams via gRPC.
- **Clients:**
  - **Macroquad client**: Renders AV1/AVIF video using Macroquad and aom-decode.
  - **Iced client** (if present): Modern GUI, native tokio integration, improved threading.
- **Protocol:** gRPC bidirectional streaming (see `proto-def/proto/browser_stream.proto`).
- **Shared utilities:** `core-utils` (e.g., color conversion), `proto-def` (protocol definitions).
- **AVIF Proof-of-Concept:** `avif_poc` for AVIF decoding experiments.

## Key Conventions

- **Keyframe enforcement:** All video frames are encoded as keyframes for client simplicity (see `StreamConfig.force_keyframes`).
- **Workspace structure:** All major components are Rust crates in a Cargo workspace (`examples/20_webprox_avif/Cargo.toml`).
- **Debug logging:** Enable with `RUST_LOG=debug` for both server and client.
- **Convenience scripts:** Use `start_server.sh`, `start_client.sh`, and `cleanup.sh` for common workflows. `cleanup.sh` will kill all Chrome/Chromium processes.
- **Performance tuning:** Adjust encoder speed, quantizer, and thread count via CLI flags to `cloud-render-srv`.
- **Testing:** Use provided shell scripts for scroll latency and encoder configuration experiments.

---

This file summarizes build, test, and architecture details for Copilot and future AI assistants. If you want to adjust or expand coverage, let me know!
