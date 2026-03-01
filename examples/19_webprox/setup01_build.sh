#!/bin/bash
# setup01_build.sh - Builds the two main components of the cloud-proxy project.
set -e

echo "Building cloud-proxy-server..."
cargo build --package cloud-proxy-server

echo "Building minimal-tui-client..."
cargo build --package minimal-tui-client

echo "Done."
