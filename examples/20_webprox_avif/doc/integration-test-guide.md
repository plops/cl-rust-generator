# AVIF Integration Test Guide

## Quick Start

### 1. Start Server
```bash
cd cloud-render-srv
cargo run -- serve
```

### 2. Run Integration Test
```bash
cd macroquad-client
cargo run -- --integration-test --test-image ../test.png --tolerance 0.0
```

## Command Line Options

- `--integration-test`: Enable integration test mode
- `--test-image <PATH>`: Path to test PNG image (required)
- `--server-addr <ADDR>`: Server address (default: `[::1]:50051`)
- `--tolerance <FLOAT>`: Pixel comparison tolerance (default: 0.0)
- `--output-result <PATH>`: Save JSON report to file

## Development Cycle

### Efficient Workflow
1. **Edit code** in server or client
2. **Build**: `cargo build` in both directories
3. **Stop server**: `pkill -f cloud-render-srv`
4. **Start server**: `cargo run -- serve`
5. **Run test**: `cargo run -- --integration-test --test-image ../test.png --tolerance 0.0`

### Quick Script
Create `dev-test.sh`:
```bash
#!/bin/bash
pkill -f cloud-render-srv || true
cd cloud-render-srv && cargo build
cd ../macroquad-client && cargo build
cd ../cloud-render-srv && cargo run -- serve &
SERVER_PID=$!
sleep 2
cd ../macroquad-client
cargo run -- --integration-test --test-image ../test.png --tolerance 0.0
kill $SERVER_PID 2>/dev/null || true
```

## Test Images

Available in root directory:
- `test.png` - 800x600 basic test image
- `screenshot.png` - Full browser screenshot
- `wikipedia_rust.png` - Wikipedia page screenshot

## Troubleshooting

### "Connection refused"
```bash
# Check server
netstat -ln | grep 50051
# Restart server
cd cloud-render-srv && cargo run -- serve
```

### "AVIF decode failed: 'ftyp' box must occur first"
Server sending raw AV1 instead of AVIF container. Check `create_simple_avif()` in `test_handler.rs`.

### "No encoded data received"
Rav1e encoder not producing output. Check encoder configuration and YUV conversion.

### Debug Mode
```bash
RUST_LOG=debug cargo run --bin cloud-render-srv -- serve
RUST_LOG=debug cargo run --bin macroquad-client -- --integration-test --test-image ../test.png
```

## Architecture

**Server**: `cloud-render-srv/src/session/test_handler.rs` - AVIF encoding
**Client**: `macroquad-client/src/integration_test/` - Test orchestration
**Protocol**: Extended gRPC with `RawImageData` and `TestFrameResult`

## Data Flow
```
Client (PNG) → RGB → gRPC → Server → AVIF → gRPC → Client → Decode → Compare
```
