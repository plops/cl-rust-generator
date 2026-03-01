#!/bin/bash

# Start the AV1 Remote Browser Client in development mode
echo "Starting AV1 Remote Browser Client (development mode)..."
echo "=================================================="

# Set working directory to the project root
cd "$(dirname "$0")"

# Run the macroquad-client in development mode
cargo run --bin macroquad-client -- "$@"

echo "Client stopped."
