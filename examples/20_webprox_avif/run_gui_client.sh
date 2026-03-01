#!/bin/bash

# Script to run the GUI client without tokio::main conflicts
# This builds and runs the client in GUI mode only

echo "Building GUI client..."
cd macroquad-client
cargo build --bin macroquad-client

echo "Starting GUI client..."
# Set environment to avoid display issues if needed
export DISPLAY=${DISPLAY:-:0}
./target/debug/macroquad-client
