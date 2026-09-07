#!/usr/bin/env bash
# setup01_build.sh -- regenerate all Rust code for 23_embassy_pico from the
# Lisp generator inputs, then compile every Rust crate in release mode.
#
# Usage: ./setup01_build.sh        (run from anywhere; locates repo root via git)
#
# Step 1: SBCL runs gen00 (proto lib) -> gen10 (firmware main) ->
#         gen11 (firmware Cargo.toml/memory.x/.cargo/build.rs) ->
#         gen20 (host TUI). Never edit the generated files by hand.
# Step 2: cargo build --release for proto + host_ctl (host target) and
#         fw_pico2 (thumbv8m.main-none-eabihf, RP2350).
set -euo pipefail

export PATH="$HOME/.cargo/bin:$PATH"

if ROOT="$(git rev-parse --show-toplevel 2>/dev/null)"; then
    :
else
    # Fallback: this script lives in examples/23_embassy_pico/
    ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
fi
EX="$ROOT/examples/23_embassy_pico"
echo "repo root: $ROOT"

command -v sbcl >/dev/null || { echo "ERROR: sbcl not found" >&2; exit 1; }
command -v cargo >/dev/null || { echo "ERROR: cargo not found" >&2; exit 1; }

echo "=== [1/2] generating Rust code from Lisp ==="
for gen in gen00_proto gen10_firmware gen11_fwproj gen20_host; do
    echo "--- $gen.lisp"
    sbcl --non-interactive \
        --eval "(push #P\"$ROOT/\" asdf:*central-registry*)" \
        --load "$EX/$gen.lisp"
done

echo "=== [2/2] release builds ==="
echo "--- proto (host)"
cargo build --release --manifest-path "$EX/proto/Cargo.toml"
echo "--- host_ctl (host)"
cargo build --release --manifest-path "$EX/host_ctl/Cargo.toml"
echo "--- fw_pico2 (thumbv8m.main-none-eabihf)"
rustup target add thumbv8m.main-none-eabihf 2>/dev/null || true
cargo build --release --manifest-path "$EX/fw_pico2/Cargo.toml" \
    --target thumbv8m.main-none-eabihf

echo "OK: all generated and built in release mode."
