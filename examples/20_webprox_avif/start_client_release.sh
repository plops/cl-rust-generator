#!/bin/bash

# Start the AV1 Remote Browser Client in release mode
echo "Starting AV1 Remote Browser Client (release mode)..."
echo "=================================================="

# Set working directory to the project root
cd "$(dirname "$0")"

# Parse command line arguments
LOG_LEVEL="info"
Y_OFFSET="0"
VERBOSE_COORDS=""

while [[ $# -gt 0 ]]; do
    case $1 in
        --log-level)
            LOG_LEVEL="$2"
            shift 2
            ;;
        --y-offset)
            Y_OFFSET="$2"
            shift 2
            ;;
        --verbose-coords)
            VERBOSE_COORDS="--verbose-coords"
            shift
            ;;
        *)
            echo "Unknown option: $1"
            echo "Usage: $0 [--log-level trace|debug|info|warn|error] [--y-offset N] [--verbose-coords]"
            exit 1
            ;;
    esac
done

echo "Using log level: $LOG_LEVEL"
echo "Using Y offset: $Y_OFFSET"
if [[ -n "$VERBOSE_COORDS" ]]; then
    echo "Verbose coordinates enabled"
fi

# Run the macroquad-client in release mode with log level and other options
cargo run --release --bin macroquad-client -- --log-level "$LOG_LEVEL" --y-offset "$Y_OFFSET" $VERBOSE_COORDS

echo "Client stopped."
