#!/bin/bash

# Test script for scroll latency investigation
# This script starts the server with enhanced logging and provides testing instructions

echo "=== Scroll Latency Test Script ==="
echo "This script will start the AV1 server with enhanced scroll logging"
echo ""

# Check if server binary exists
SERVER_BINARY="./target/release/cloud-render-srv"
if [ ! -f "$SERVER_BINARY" ]; then
    echo "Server binary not found at $SERVER_BINARY"
    echo "Building server in release mode..."
    cargo build --release --bin cloud-render-srv
fi

# Create log directory
mkdir -p logs

# Start server with enhanced logging
echo "Starting server with debug logging..."
echo "Logs will be saved to logs/scroll_latency_test.log"
echo ""

# Use fast configuration for better baseline performance
./target/release/cloud-render-srv serve \
    --log-level debug \
    --quantizer 200 \
    --min-quantizer 150 \
    --speed-preset 10 \
    --threads 4 \
    --tile-cols 2 \
    --tile-rows 1 2>&1 | tee logs/scroll_latency_test.log &

SERVER_PID=$!
echo "Server started with PID: $SERVER_PID"
echo ""

# Wait for server to initialize
echo "Waiting for server to initialize (5 seconds)..."
sleep 5

# Check if server is still running
if ! kill -0 $SERVER_PID 2>/dev/null; then
    echo "ERROR: Server failed to start!"
    echo "Check logs/scroll_latency_test.log for details"
    exit 1
fi

echo "Server is running successfully!"
echo ""
echo "=== Testing Instructions ==="
echo "1. Start the client in another terminal:"
echo "   ./start_client_release.sh --log-level debug"
echo ""
echo "2. Open the JavaScript console in your browser to see:"
echo "   - Frame counter increments"
echo "   - Scroll position tracking"
echo "   - Update rate (fps)"
echo "   - Color-coded status (green=active, yellow=slow, red=stalled)"
echo ""
echo "3. Test scroll latency:"
echo "   - Scroll using mouse wheel or arrow keys"
echo "   - Watch the counter in the top-right corner"
echo "   - Observe the time between scroll action and counter increment"
echo ""
echo "4. Check server logs for detailed timing:"
echo "   - Scroll event processing time"
echo "   - Browser scroll execution time"
echo "   - Fixed 100ms delay tracking"
echo "   - Screenshot capture timing"
echo ""
echo "5. Look for these key log messages:"
echo "   '[Server] Scroll event received'"
echo "   '[Server] Browser scroll execution took:'"
echo "   '[Server] Fixed 100ms delay completed:'"
echo "   '[Server] This screenshot appears to be scroll-related'"
echo ""
echo "=== Expected Results ==="
echo "• Total scroll processing should be <150ms (excluding 100ms delay)"
echo "• Browser scroll execution should be <20ms"
echo "• Screenshot capture should be <100ms"
echo "• Frame counter should increment within 200ms of scroll action"
echo ""
echo "=== To Stop Testing ==="
echo "Press Ctrl+C in this terminal to stop the server"
echo "Or run: kill $SERVER_PID"
echo ""

# Function to cleanup on exit
cleanup() {
    echo ""
    echo "Stopping server (PID: $SERVER_PID)..."
    kill $SERVER_PID 2>/dev/null
    echo "Server stopped."
    echo "Logs saved to: logs/scroll_latency_test.log"
    exit 0
}

# Set up signal handlers
trap cleanup SIGINT SIGTERM

# Keep script running and show server status
while true; do
    if kill -0 $SERVER_PID 2>/dev/null; then
        echo -e "\rServer running... (Press Ctrl+C to stop) "
        sleep 2
    else
        echo ""
        echo "Server stopped unexpectedly!"
        echo "Check logs/scroll_latency_test.log for details"
        exit 1
    fi
done
