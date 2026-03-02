#!/bin/bash

# Balanced encoding benchmark - current default settings
echo "=== BALANCED ENCODING BENCHMARK ==="
echo "Balanced speed/quality settings (current defaults)"
echo "=========================================="

LOG_FILE="benchmark_balanced.log"

# Balanced settings: current defaults
./start_server_release.sh \
    --log-level debug \
    --quantizer 150 \
    --min-quantizer 80 \
    --speed-preset 8 \
    --threads 2 \
    --tile-cols 1 \
    --tile-rows 1 > "$LOG_FILE" 2>&1 &

SERVER_PID=$!

echo "Server PID: $SERVER_PID"
echo "Waiting 5 seconds for server to start..."
sleep 5

echo "Starting client..."
./start_client_release.sh \
    --log-level debug > "${LOG_FILE%.log}_client.log" 2>&1 &

CLIENT_PID=$!

echo "Client PID: $CLIENT_PID"
echo "Running benchmark for 30 seconds..."
sleep 30

echo "Stopping processes..."
kill $CLIENT_PID 2>/dev/null
kill $SERVER_PID 2>/dev/null
./cleanup.sh >/dev/null 2>&1

echo "Benchmark completed. Log saved to: $LOG_FILE"
echo "Client log saved to: ${LOG_FILE%.log}_client.log"
