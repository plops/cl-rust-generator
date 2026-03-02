#!/bin/bash

# Start the AV1 Remote Browser Server in release mode
echo "Starting AV1 Remote Browser Server (release mode)..."
echo "=================================================="

# Set working directory to the project root
cd "$(dirname "$0")"

# Parse command line arguments
LOG_LEVEL="info"
while [[ $# -gt 0 ]]; do
    case $1 in
        --log-level)
            LOG_LEVEL="$2"
            shift 2
            ;;
        *)
            echo "Unknown option: $1"
            echo "Usage: $0 [--log-level trace|debug|info|warn|error]"
            exit 1
            ;;
    esac
done

echo "Using log level: $LOG_LEVEL"

# Run the cloud-render-srv server in release mode with log level
cargo run --release --bin cloud-render-srv -- --log-level "$LOG_LEVEL"

echo "Server stopped."
