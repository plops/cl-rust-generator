#!/bin/bash

# Start the AV1 Remote Browser Server in release mode
echo "Starting AV1 Remote Browser Server (release mode)..."
echo "=================================================="

# Set working directory to the project root
cd "$(dirname "$0")"

# Run the cloud-render-srv server in release mode
cargo run --release --bin cloud-render-srv

echo "Server stopped."
