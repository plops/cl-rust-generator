#!/bin/bash

# Latency Measurement Script
# Runs 6 different encoding configurations and extracts latency statistics

set -e

echo "Starting Latency Measurement Tests..."
echo "====================================="

# Create results directory
mkdir -p latency_results
cd latency_results

# Function to run test and extract latency stats
run_test() {
    local test_name="$1"
    local server_args="$2"
    local client_args="$3"
    
    echo "Running test: $test_name"
    echo "Server args: $server_args"
    echo "Client args: $client_args"
    
    # Start server in background
    echo "Starting server..."
    cargo run --bin cloud-render-srv -- serve $server_args > server_${test_name}.log 2>&1 &
    local server_pid=$!
    
    # Wait for server to start
    echo "Waiting for server to start..."
    sleep 5
    
    # Start client in background
    echo "Starting client..."
    timeout 60s cargo run --bin macroquad-client -- $client_args > client_${test_name}.log 2>&1 &
    local client_pid=$!
    
    # Wait for test to complete or timeout
    echo "Waiting for test to complete..."
    wait $client_pid 2>/dev/null || true
    
    # Kill server if still running
    kill $server_pid 2>/dev/null || true
    wait $server_pid 2>/dev/null || true
    
    # Extract latency measurements
    echo "Extracting latency measurements..."
    grep "LATENCY:" client_${test_name}.log | sed 's/.*LATENCY: \([0-9]*\)ms.*/\1/' > latency_${test_name}.txt
    
    # Calculate statistics
    if [ -s latency_${test_name}.txt ]; then
        local min=$(sort -n latency_${test_name}.txt | head -1)
        local max=$(sort -n latency_${test_name}.txt | tail -1)
        local avg=$(awk '{sum+=$1} END {print sum/NR}' latency_${test_name}.txt)
        local count=$(wc -l < latency_${test_name}.txt)
        
        echo "Test $test_name completed: $count measurements"
        echo "Min: ${min}ms, Avg: ${avg}ms, Max: ${max}ms"
        echo "$min,$avg,$max,$count" > stats_${test_name}.csv
    else
        echo "No latency measurements found for $test_name"
        echo "0,0,0,0" > stats_${test_name}.csv
    fi
    
    echo "----------------------------------------"
}

# Test 1: RAW RGB
run_test "raw_rgb" "--raw-rgb --max-frames 100 --loop-delay 500" "--log-level info"

# Test 2: Still Picture (Default)
run_test "still_picture" "--max-frames 100 --loop-delay 500" "--log-level info"

# Test 3: Low Latency Video
run_test "low_latency_video" "--disable-still-picture --max-frames 100 --loop-delay 500" "--log-level info"

# Test 4: Max Efficiency Video
run_test "max_efficiency" "--disable-still-picture --disable-low-latency --rdo-lookahead-frames 20 --max-frames 100 --loop-delay 500" "--log-level info"

# Test 5: High Network Payload
run_test "high_network" "--quantizer 50 --max-frames 100 --loop-delay 500" "--log-level info"

# Test 6: High CPU Load
run_test "high_cpu" "--speed-preset 5 --max-frames 100 --loop-delay 500" "--log-level info"

echo "All tests completed!"
echo "Results saved in latency_results/"
