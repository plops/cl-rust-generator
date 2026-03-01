#!/bin/bash

# Start the AV1 Remote Browser Client in release mode
echo "Starting AV1 Remote Browser Client (release mode)..."
echo "=================================================="

# Set working directory to the project root
cd "$(dirname "$0")"

# Run the macroquad-client in release mode
cargo run --release --bin macroquad-client

echo "Client stopped."
