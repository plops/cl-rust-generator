#!/bin/bash

# Fast encoding benchmark - high speed, lower quality
echo "=== FAST ENCODING BENCHMARK ==="
echo "High speed, lower quality settings"
echo "================================"

LOG_FILE="benchmark_fast.log"

# Fast settings: high speed preset, higher quantizer
./start_server_release.sh \
    --log-level debug \
    --quantizer 230 \
    --min-quantizer 190 \
    --speed-preset 10 \
    --threads 12 \
    --tile-cols 2 \
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
