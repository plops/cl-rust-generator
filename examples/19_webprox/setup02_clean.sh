#!/bin/bash
# setup02_clean.sh - Prunes stale artifacts of this project as described in doc/080_house_keeping.md
set -e

# From 1. Prune Stale Artifacts:
# - cargo sweep: deletes build files older than a specified number of days
if command -v cargo-sweep >/dev/null 2>&1; then
    echo "Running cargo sweep --time 30..."
    cargo sweep --time 30
else
    echo "cargo-sweep not found. Install it with: cargo install cargo-sweep"
fi

# - cargo-clean-all: recursively finds and cleans target/ directories
# Note: we only want to clean the current project's target folder here, 
# so we'll just use it on the local directory if available.
if command -v cargo-clean-all >/dev/null 2>&1; then
    echo "Running cargo-clean-all --yes..."
    cargo clean-all --yes
fi

# From 4. Manage Global Cargo Caches:
# - cargo clean gc: natively garbage collect global cargo home
if cargo clean --help | grep -q "gc"; then
    echo "Running cargo clean gc..."
    cargo clean gc
else
    echo "cargo clean gc is not supported by your installed cargo version."
fi

echo "Done."
