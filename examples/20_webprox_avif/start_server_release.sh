#!/bin/bash

# Start the AV1 Remote Browser Server in release mode
echo "Starting AV1 Remote Browser Server (release mode)..."
echo "=================================================="

# Set working directory to the project root
cd "$(dirname "$0")"

# Parse command line arguments
LOG_LEVEL="info"
QUANTIZER="150"
MIN_QUANTIZER="80"
SPEED_PRESET="8"
THREADS="2"
TILE_COLS="1"
TILE_ROWS="1"

while [[ $# -gt 0 ]]; do
    case $1 in
        --log-level)
            LOG_LEVEL="$2"
            shift 2
            ;;
        --quantizer)
            QUANTIZER="$2"
            shift 2
            ;;
        --min-quantizer)
            MIN_QUANTIZER="$2"
            shift 2
            ;;
        --speed-preset)
            SPEED_PRESET="$2"
            shift 2
            ;;
        --threads)
            THREADS="$2"
            shift 2
            ;;
        --tile-cols)
            TILE_COLS="$2"
            shift 2
            ;;
        --tile-rows)
            TILE_ROWS="$2"
            shift 2
            ;;
        *)
            echo "Unknown option: $1"
            echo "Usage: $0 [--log-level trace|debug|info|warn|error] [--quantizer 0-255] [--min-quantizer 0-255] [--speed-preset 0-10] [--threads 0-N] [--tile-cols 1-64] [--tile-rows 1-64]"
            echo "  quantizer: 0=highest quality, 255=lowest quality/smallest size"
            echo "  speed-preset: 0=slowest/highest quality, 10=fastest/lowest quality"
            echo "  threads: 0=auto-detect CPUs, or specify number"
            echo "  tile-cols/rows: must be power of 2 (1,2,4,8,16,32,64)"
            exit 1
            ;;
    esac
done

echo "Configuration:"
echo "  Log level: $LOG_LEVEL"
echo "  Quantizer: $QUANTIZER"
echo "  Min quantizer: $MIN_QUANTIZER"
echo "  Speed preset: $SPEED_PRESET"
echo "  Threads: $THREADS"
echo "  Tiles: ${TILE_COLS}x${TILE_ROWS}"

# Run the cloud-render-srv server in release mode with all parameters
cargo run --release --bin cloud-render-srv -- \
    --log-level "$LOG_LEVEL" \
    --quantizer "$QUANTIZER" \
    --min-quantizer "$MIN_QUANTIZER" \
    --speed-preset "$SPEED_PRESET" \
    --threads "$THREADS" \
    --tile-cols "$TILE_COLS" \
    --tile-rows "$TILE_ROWS"

echo "Server stopped."
