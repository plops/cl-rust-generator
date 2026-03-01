# AV1 Remote Browser - Utility Scripts

This directory contains utility scripts for managing the AV1 Remote Browser project.

## Scripts

### `cleanup.sh`
Cleans up all processes and temporary files related to the project.

**Usage:**
```bash
./cleanup.sh
```

**What it does:**
- Kills all `macroquad-client` processes
- Kills all `cloud-render-srv` processes  
- Kills all Chrome/Chromium processes
- Removes Chrome temporary directories (`/tmp/chromiumoxide-runner`)
- Frees up port 50051
- Provides colored output and status reporting

**Features:**
- Graceful termination (SIGTERM) followed by force kill (SIGKILL)
- Color-coded output (when supported)
- Comprehensive process detection
- Port status checking
- Error handling and reporting

### `start-fresh.sh`
Performs a complete cleanup and starts both server and client with logging.

**Usage:**
```bash
./start-fresh.sh
```

**What it does:**
1. Runs `cleanup.sh` to ensure clean state
2. Starts the server with INFO level logging to `server.log`
3. Starts the client with DEBUG level logging to `client.log`
4. Monitors both processes
5. Shows recent log output
6. Handles graceful shutdown on Ctrl+C

**Features:**
- Automatic process monitoring
- Log file management
- Status reporting
- Graceful shutdown handling
- Real-time log preview

## Common Workflows

### Quick Start
```bash
./start-fresh.sh
```

### Manual Control
```bash
# Clean up everything
./cleanup.sh

# Start server manually
RUST_LOG=info cargo run --bin cloud-render-srv

# In another terminal, start client
RUST_LOG=debug cargo run --bin macroquad-client
```

### Debugging Issues
```bash
# Clean up first
./cleanup.sh

# Start with detailed logging
./start-fresh.sh

# View logs in real-time
tail -f server.log
tail -f client.log
```

### Killing Stuck Processes
```bash
# Force cleanup
./cleanup.sh

# If that doesn't work, try with sudo
sudo ./cleanup.sh

# Manual process killing
pkill -f macroquad-client
pkill -f cloud-render-srv
pkill -f chrome
```

## Log Files

When using `start-fresh.sh`, logs are saved to:
- `server.log` - Server output with INFO level logging
- `client.log` - Client output with DEBUG level logging

### Log Levels
- **ERROR**: Serious errors that prevent operation
- **WARN**: Warning messages for potential issues  
- **INFO**: General operational information
- **DEBUG**: Detailed debugging information

## Troubleshooting

### Port Already in Use
```bash
# Check what's using port 50051
lsof -i:50051

# Kill the process
./cleanup.sh
```

### Chrome Won't Start
```bash
# Clean up Chrome directories
./cleanup.sh

# Manually remove Chrome temp dirs
rm -rf /tmp/chromiumoxide-runner
rm -rf /tmp/.com.google.Chrome
```

### Processes Won't Die
```bash
# Try the cleanup script first
./cleanup.sh

# If still running, check manually
ps aux | grep -E "(macroquad|cloud-render|chrome)"

# Kill manually if needed
sudo pkill -9 -f "process-name"
```

## Environment Variables

These scripts respect the following environment variables:

- `RUST_LOG`: Controls logging level (error, warn, info, debug, trace)
- `NO_COLOR`: Disables colored output when set to any value

Example:
```bash
RUST_LOG=trace ./start-fresh.sh
NO_COLOR=1 ./cleanup.sh
```
