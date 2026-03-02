#!/bin/bash

# Dev mode scroll latency test - focuses on buffer/encoder configuration
# This script uses debug build for faster iteration and tests different encoder settings

echo "=== Scroll Latency Test (Dev Mode) ==="
echo "Focus: Buffer configuration and rav1e encoder settings"
echo ""

# Check if server binary exists
SERVER_BINARY="./target/debug/cloud-render-srv"
if [ ! -f "$SERVER_BINARY" ]; then
    echo "Server binary not found at $SERVER_BINARY"
    echo "Building server in debug mode..."
    cargo build --bin cloud-render-srv
fi

# Create log directory
mkdir -p logs

echo "=== Testing Different Encoder Configurations ==="
echo ""
echo "Configuration 1: Current Default Settings"
echo "  - Speed preset: 8 (balanced)"
echo "  - Quantizer: 150"
echo "  - Threads: 12"
echo "  - Low latency: true"
echo ""

# Start server with current default settings
echo "Starting server with default settings..."
./target/debug/cloud-render-srv serve \
    --log-level debug \
    --quantizer 150 \
    --min-quantizer 100 \
    --speed-preset 8 \
    --threads 12 \
    --tile-cols 2 \
    --tile-rows 1 2>&1 | tee logs/default_config.log &
SERVER_PID=$!

echo "Server PID: $SERVER_PID"
echo "Waiting for initialization..."
sleep 3

# Check if server is running
if ! kill -0 $SERVER_PID 2>/dev/null; then
    echo "ERROR: Server failed to start!"
    exit 1
fi

echo "✅ Server running with default settings"
echo ""
echo "📋 Test Instructions:"
echo "1. Start client: ./start_client.sh --log-level debug"
echo "2. Scroll and observe latency"
echo "3. Check for these key indicators:"
echo "   - Frame counter increments (should be frequent)"
echo "   - Color status (green=good, yellow=slow, red=stalled)"
echo "   - Server logs showing scroll processing times"
echo "   - Look for: 'Fixed 100ms delay completed' messages"
echo ""
echo "4. Press Enter to test fast configuration..."
read

# Stop server
kill $SERVER_PID 2>/dev/null
wait $SERVER_PID 2>/dev/null

echo ""
echo "Configuration 2: Fast Settings (from benchmarks)"
echo "  - Speed preset: 10 (fastest)"
echo "  - Quantizer: 200 (lower quality, faster)"
echo "  - Threads: 4 (optimal parallelization)"
echo "  - Low latency: true"
echo ""

echo "Starting server with fast settings..."
./target/debug/cloud-render-srv serve \
    --log-level debug \
    --quantizer 200 \
    --min-quantizer 150 \
    --speed-preset 10 \
    --threads 4 \
    --tile-cols 2 \
    --tile-rows 1 2>&1 | tee logs/fast_config.log &
SERVER_PID=$!

echo "Server PID: $SERVER_PID"
echo "Waiting for initialization..."
sleep 3

echo "✅ Server running with fast settings"
echo ""
echo "📋 Compare with previous test:"
echo "1. Scroll again - notice any difference?"
echo "2. Check frame counter frequency"
echo "3. Look for reduced encoding times in logs"
echo "4. Press Enter to test minimal delay settings..."
read

# Stop server
kill $SERVER_PID 2>/dev/null
wait $SERVER_PID 2>/dev/null

echo ""
echo "Configuration 3: Minimal Delay Test"
echo "  - Fast encoder settings"
echo "  - Testing without 100ms delay (need to modify code)"
echo "  - Focus on buffer behavior"
echo ""

echo "🔧 To test minimal delay, we need to modify the code:"
echo "   In main.rs line ~301, change:"
echo "   tokio::time::sleep(Duration::from_millis(100)).await;"
echo "   to:"
echo "   tokio::time::sleep(Duration::from_millis(0)).await;"
echo ""
echo "Would you like me to make this change now? (y/n)"
read -r response

if [[ $response == "y" || $response == "Y" ]]; then
    # Backup original
    cp cloud-render-srv/src/main.rs cloud-render-srv/src/main.rs.backup
    
    # Modify the delay
    sed -i 's/tokio::time::sleep(Duration::from_millis(100))/tokio::time::sleep(Duration::from_millis(0))/' cloud-render-srv/src/main.rs
    
    echo "✅ Modified delay to 0ms, rebuilding..."
    cargo build --bin cloud-render-srv
    
    echo "Starting server with 0ms delay..."
    ./target/debug/cloud-render-srv serve \
        --log-level debug \
        --quantizer 200 \
        --min-quantizer 150 \
        --speed-preset 10 \
        --threads 4 \
        --tile-cols 2 \
        --tile-rows 1 2>&1 | tee logs/zero_delay.log &
    SERVER_PID=$!
    
    echo "Server PID: $SERVER_PID"
    echo "Waiting for initialization..."
    sleep 3
    
    echo "✅ Server running with 0ms delay"
    echo ""
    echo "📋 Test the impact of removing the 100ms delay:"
    echo "1. Scroll and observe dramatic improvement?"
    echo "2. Frame counter should be much more responsive"
    echo "3. Check logs for 'Fixed 0ms delay completed'"
    echo "4. Press Enter to restore original code..."
    read
    
    # Stop server
    kill $SERVER_PID 2>/dev/null
    wait $SERVER_PID 2>/dev/null
    
    # Restore original
    mv cloud-render-srv/src/main.rs.backup cloud-render-srv/src/main.rs
    cargo build --bin cloud-render-srv
    echo "✅ Restored original code"
else
    echo "Skipping 0ms delay test"
fi

echo ""
echo "=== Test Summary ==="
echo "Logs saved to:"
echo "  - logs/default_config.log"
echo "  - logs/fast_config.log"
echo "  - logs/zero_delay.log (if tested)"
echo ""
echo "🔍 Key things to look for in logs:"
echo "1. '[Server] Total scroll processing time:' - should be <150ms"
echo "2. '[Server] Fixed Xms delay completed:' - the 100ms culprit"
echo "3. '[Server] Screenshot captured in Xms' - Chrome performance"
echo "4. '[Server] encoding latency: Xms' - rav1e performance"
echo "5. '[Server] This screenshot appears to be scroll-related'"
echo ""
echo "🎯 Expected findings:"
echo "- 100ms delay is likely the main bottleneck"
echo "- Encoder settings affect frame rate consistency"
echo "- Buffer behavior during rapid scrolling"
echo "- Chrome screenshot performance impact"
echo ""
echo "Ready to analyze the results!"
