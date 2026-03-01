#!/bin/bash

# Cleanup script for AV1 Remote Browser project
# Kills all related processes and cleans up temporary files

set -e

# Check if colors are supported
if [ -t 1 ]; then
    RED='\033[0;31m'
    GREEN='\033[0;32m'
    YELLOW='\033[1;33m'
    BLUE='\033[0;34m'
    NC='\033[0m' # No Color
else
    RED=''
    GREEN=''
    YELLOW=''
    BLUE=''
    NC=''
fi

echo "🧹 Cleaning up AV1 Remote Browser processes..."

# Function to kill processes by name
kill_processes() {
    local process_name="$1"
    local description="$2"
    
    printf "${YELLOW}Killing $description...${NC} "
    
    local pids=$(pgrep -f "$process_name" 2>/dev/null || true)
    
    if [ -n "$pids" ]; then
        echo "$pids" | xargs kill -TERM 2>/dev/null || true
        sleep 2
        
        # Force kill if still running
        pids=$(pgrep -f "$process_name" 2>/dev/null || true)
        if [ -n "$pids" ]; then
            echo "$pids" | xargs kill -KILL 2>/dev/null || true
        fi
        
        local count=$(echo "$pids" | wc -w)
        printf "${GREEN}✓ Killed $count processes${NC}\n"
    else
        printf "${BLUE}No processes found${NC}\n"
    fi
}

# Function to cleanup directories
cleanup_dir() {
    local dir_path="$1"
    local description="$2"
    
    printf "${YELLOW}Cleaning $description...${NC} "
    
    if [ -d "$dir_path" ]; then
        rm -rf "$dir_path"
        printf "${GREEN}✓ Removed${NC}\n"
    else
        printf "${BLUE}Not found${NC}\n"
    fi
}

printf "\n${BLUE}=== Killing Processes ===${NC}\n\n"

# Kill our application processes
kill_processes "macroquad-client" "Macroquad Client"
kill_processes "cloud-render-srv" "Cloud Render Server"

# Kill Chrome/Chromium processes
kill_processes "chrome" "Chrome/Chromium"
kill_processes "chromium" "Chromium"

# Kill any remaining Rust processes from our project
kill_processes "target/debug/cloud-render-srv" "Debug Server"
kill_processes "target/debug/macroquad-client" "Debug Client"

printf "\n${BLUE}=== Cleaning Directories ===${NC}\n\n"

# Clean up temporary directories
cleanup_dir "/tmp/chromiumoxide-runner" "Chrome temporary directory"
cleanup_dir "/tmp/.com.google.Chrome" "Chrome user data"
cleanup_dir "/tmp/.chromium" "Chromium user data"

printf "\n${BLUE}=== Checking for Remaining Processes ===${NC}\n\n"

# Check if any processes are still running
echo "Checking for remaining processes..."
remaining_processes=""

# Check for our specific processes
for proc in "macroquad-client" "cloud-render-srv" "chrome" "chromium"; do
    pids=$(pgrep -f "$proc" 2>/dev/null || true)
    if [ -n "$pids" ]; then
        remaining_processes="$remaining_processes\n$proc: $pids"
    fi
done

if [ -n "$remaining_processes" ]; then
    printf "${RED}⚠️  Some processes are still running:$remaining_processes${NC}\n"
    printf "${YELLOW}You may need to manually kill them or run this script with sudo${NC}\n"
else
    printf "${GREEN}✓ All related processes have been killed${NC}\n"
fi

printf "\n${BLUE}=== Port Status ===${NC}\n\n"

# Check if port 50051 is still in use
port_pid=$(lsof -ti:50051 2>/dev/null || true)
if [ -n "$port_pid" ]; then
    printf "${RED}⚠️  Port 50051 is still in use by PID: $port_pid${NC}\n"
    printf "${YELLOW}Killing process on port 50051...${NC}\n"
    kill -TERM "$port_pid" 2>/dev/null || true
    sleep 1
    kill -KILL "$port_pid" 2>/dev/null || true
else
    printf "${GREEN}✓ Port 50051 is free${NC}\n"
fi

printf "\n${GREEN}🎉 Cleanup completed!${NC}\n"
printf "${BLUE}You can now restart the server and client cleanly.${NC}\n"
