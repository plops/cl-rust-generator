#!/usr/bin/env bash
# setup00_deps.sh -- install every dependency needed by setup01_build.sh
# for 23_embassy_pico (idempotent: skips anything already present).
#
# Installs:
#   Rust toolchain (via rustup) + thumbv8m.main-none-eabihf target (RP2350)
#   probe-rs 0.32.0 (flashing, via cargo)
#   Debian/Ubuntu apt packages: sbcl (generators), libudev-dev + pkg-config
#     (host `serialport` crate), libusb-1.0-0-dev (probe-rs USB, if built)
#
# Usage: ./setup00_deps.sh
set -euo pipefail

export PATH="$HOME/.cargo/bin:$PATH"
SUDO=""
[ "$(id -u)" -eq 0 ] || SUDO="sudo"

if ! command -v rustup >/dev/null; then
    echo "--- installing rustup (stable toolchain)"
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs \
        | sh -s -- -y --profile default --default-toolchain stable
    export PATH="$HOME/.cargo/bin:$PATH"
fi

echo "--- RP2350 target"
rustup target add thumbv8m.main-none-eabihf

if ! command -v probe-rs >/dev/null; then
    echo "--- installing probe-rs 0.32.0 (takes a while)"
    cargo install probe-rs-tools --version 0.32.0 --locked
fi

if command -v apt-get >/dev/null; then
    echo "--- apt packages (sbcl, libudev-dev, pkg-config, libusb-1.0-0-dev)"
    $SUDO apt-get update
    $SUDO apt-get install -y sbcl libudev-dev pkg-config libusb-1.0-0-dev
else
    echo "--- no apt-get; ensure sbcl + libudev headers exist manually"
fi

echo "=== versions ==="
rustc --version
cargo --version
rustup target list --installed | grep -E 'thumbv8m|host' || true
probe-rs --version
sbcl --version
echo "OK: dependencies ready; next run ./setup01_build.sh"
