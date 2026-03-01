#!/bin/bash

# Start the AV1 Remote Browser Server in development mode
echo "Starting AV1 Remote Browser Server (development mode)..."
echo "=================================================="

# Set working directory to the project root
cd "$(dirname "$0")"

# Run the cloud-render-srv server in development mode
cargo run --bin cloud-render-srv

echo "Server stopped."
