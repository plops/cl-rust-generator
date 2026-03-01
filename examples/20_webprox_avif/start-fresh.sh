#!/bin/bash

# Fresh start script for AV1 Remote Browser project
# Cleans up and starts server and client

set -e

# Check if colors are supported
if [ -t 1 ]; then
    RED='\033[0;31m'
    GREEN='\033[0;32m'
    YELLOW='\033[1;33m'
    BLUE='\033[0;34m'
    CYAN='\033[0;36m'
    NC='\033[0m' # No Color
else
    RED=''
    GREEN=''
    YELLOW=''
    BLUE=''
    CYAN=''
    NC=''
fi

printf "${CYAN}🚀 AV1 Remote Browser - Fresh Start${NC}\n\n"

# Step 1: Cleanup
printf "${BLUE}Step 1: Cleaning up existing processes...${NC}\n"
./cleanup.sh
printf "\n"

# Step 2: Start server
printf "${BLUE}Step 2: Starting server...${NC}\n"
printf "${YELLOW}Server will start in background. Check logs for connection details.${NC}\n"

# Start server in background with logging
RUST_LOG=info cargo run --bin cloud-render-srv > server.log 2>&1 &
SERVER_PID=$!

printf "${GREEN}✓ Server started with PID: $SERVER_PID${NC}\n"
printf "${YELLOW}Server logs: server.log${NC}\n"

# Wait a moment for server to start
sleep 3

# Check if server is running
if kill -0 $SERVER_PID 2>/dev/null; then
    printf "${GREEN}✓ Server is running${NC}\n"
else
    printf "${RED}❌ Server failed to start. Check server.log for details.${NC}\n"
    exit 1
fi

# Step 3: Start client
printf "\n${BLUE}Step 3: Starting client...${NC}\n"
printf "${YELLOW}Client will start with debug logging.${NC}\n"

# Start client with logging
RUST_LOG=debug cargo run --bin macroquad-client > client.log 2>&1 &
CLIENT_PID=$!

printf "${GREEN}✓ Client started with PID: $CLIENT_PID${NC}\n"
printf "${YELLOW}Client logs: client.log${NC}\n"

# Wait a moment for client to start
sleep 2

# Check if client is running
if kill -0 $CLIENT_PID 2>/dev/null; then
    printf "${GREEN}✓ Client is running${NC}\n"
else
    printf "${RED}❌ Client failed to start. Check client.log for details.${NC}\n"
    # Kill server if client failed
    kill $SERVER_PID 2>/dev/null || true
    exit 1
fi

printf "\n${CYAN}=== Monitoring ===${NC}\n"
printf "${BLUE}Both server and client are running.${NC}\n"
printf "${YELLOW}To view logs:${NC}\n"
printf "  Server: tail -f server.log\n"
printf "  Client: tail -f client.log\n"
printf "\n${YELLOW}To stop both processes:${NC}\n"
printf "  ./cleanup.sh\n"
printf "\n${YELLOW}To manually stop:${NC}\n"
printf "  kill $SERVER_PID $CLIENT_PID\n"
printf "\n${GREEN}🎉 Fresh start completed!${NC}\n"

# Optional: Monitor logs for a few seconds
printf "\n${CYAN}Showing recent logs (10 seconds)...${NC}\n"
sleep 2

printf "\n${YELLOW}=== Recent Server Logs ===${NC}\n"
tail -10 server.log 2>/dev/null || printf "No server logs yet\n"

printf "\n${YELLOW}=== Recent Client Logs ===${NC}\n"
tail -10 client.log 2>/dev/null || printf "No client logs yet\n"

printf "\n${CYAN}Monitoring will continue in background. Use Ctrl+C to stop monitoring.${NC}\n"

# Keep the script running to monitor
trap 'printf "\n${YELLOW}Stopping monitoring...${NC}\n"; kill $SERVER_PID $CLIENT_PID 2>/dev/null || true; printf "${GREEN}✓ Processes stopped${NC}\n"; exit 0' INT

# Monitor processes
while true; do
    if ! kill -0 $SERVER_PID 2>/dev/null; then
        printf "\n${RED}❌ Server has stopped!${NC}\n"
        break
    fi
    
    if ! kill -0 $CLIENT_PID 2>/dev/null; then
        printf "\n${RED}❌ Client has stopped!${NC}\n"
        break
    fi
    
    sleep 5
done
