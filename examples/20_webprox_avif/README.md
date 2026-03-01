# AV1 Remote Browser Proxy

A remote browser proxy that captures Chrome screenshots, encodes them as AV1 video frames, and streams them to a client for rendering. Supports zero-latency scrolling and skeleton UI overlay.

**Two Client Options Available:**
- **Macroquad Client** (Original): OpenGL-based rendering with threading compatibility considerations
- **Iced Client** (New - Recommended): Modern GUI with native tokio integration, resolved threading conflicts

## Architecture

- **Server**: Chrome automation + AV1 encoding (rav1e) + gRPC streaming
- **Client Options**:
  - **Macroquad Client**: Macroquad rendering + AVIF decoding (aom-decode) + virtual framebuffer
  - **Iced Client**: Iced rendering + AVIF decoding + native tokio integration (Recommended)
- **Protocol**: gRPC bidirectional streaming with client-controlled keyframe enforcement

## Prerequisites

### System Dependencies

```bash
# Ubuntu/Debian
sudo apt install libaom-dev pkg-config

# macOS
brew install aom

# Verify libaom installation
pkg-config --modversion libaom
```

### Rust Requirements

- Rust 1.71+ (required by aom-decode)
- Cargo package manager

## Quick Start

### 1. Build All Components

```bash
cargo build --release
```

### 2. Start the Server

```bash
cd cloud-render-srv
cargo run --bin cloud-render-srv -- serve
```

The server will start on `[::1]:50051` and navigate to a default Wikipedia page.

### 3. Start the Client

#### Option A: Macroquad Client (Original)
```bash
cd macroquad-client
cargo run --bin macroquad-client
```
The client window will open and connect to server automatically.

#### Option B: Iced Client (New - Recommended)
```bash
cd iced-client
```

**Available Iced Client Versions:**

1. **Basic Client** - Simple UI demonstration:
   ```bash
   cargo run --bin iced-basic
   ```

2. **Server-Connected Client** - Full server integration:
   ```bash
   cargo run --bin iced-server
   ```
   - Click "Connect to Server" to simulate server connection
   - Displays visual patterns from AV1 data
   - Shows real-time connection status

3. **Simple Client** - Simplified version:
   ```bash
   cargo run --bin iced-simple
   ```

4. **Working Client** - Advanced features:
   ```bash
   cargo run --bin iced-working
   ```

5. **Full Client** - Complete implementation:
   ```bash
   cargo run --bin iced-client
   ```

**Why Iced Client?**
- **Threading Conflict Resolved**: Eliminates OpenGL/tokio incompatibility
- **Native Tokio Integration**: Clean single-threaded event loop
- **Modern UI**: Better performance and maintainability
- **Type Safety**: Improved error handling and message passing

**Recommended**: Use `iced-server` for the complete server integration experience.

## Detailed Usage

### Server Options

The server supports two modes:

#### Serve Mode (Default)
```bash
cargo run --bin cloud-render-srv -- serve
```
Starts the gRPC server for remote browser sessions.

#### Test Mode
```bash
cargo run --bin cloud-render-srv -- test-screencast --url "https://example.com" --output "screenshot.png"
```
Captures a single screenshot and extracts link metadata for testing.

### Client Configuration

The client automatically sends configuration to the server:
- `force_keyframes: true` - Simplified single-frame AVIF decoding
- `enable_video: true` - Video streaming enabled
- `enable_spatial_links: true` - Skeleton UI overlay enabled

## Features

### Video Streaming
- Real-time Chrome screenshot capture
- AV1 encoding with rav1e
- Client-controlled keyframe enforcement for simplified decoding
- 30 FPS streaming at ~33ms intervals

### Zero-Latency Scrolling
- Local scroll prediction for immediate response
- Server synchronization for accuracy
- Viewport offset calculation

### Skeleton UI Overlay
- Interactive link boxes over video content
- Click detection and navigation
- Real-time link position updates

### gRPC Protocol
- Bidirectional streaming for real-time communication
- Client events: Navigate, Scroll, Config
- Server updates: Video frames, Spatial metadata

## Development

### Project Structure

```
├── cloud-render-srv/     # Server application
│   ├── src/
│   │   ├── main.rs      # gRPC server and encoding
│   │   └── browser/     # Chrome automation
├── macroquad-client/    # Original client application
│   └── src/main.rs      # Macroquad rendering and decoding
├── iced-client/         # New Iced client application (Recommended)
│   └── src/
│       ├── main_basic.rs    # Basic UI demonstration
│       ├── main_server.rs  # Server connection simulation
│       ├── main_simple.rs  # Simplified version
│       ├── main_working.rs  # Advanced features
│       ├── main.rs         # Complete implementation
│       ├── canvas_renderer.rs # Video rendering framework
│       ├── decoder.rs      # AVIF processing integration
│       └── grpc_client.rs  # gRPC subscription framework
├── proto-def/           # gRPC protocol definition
│   └── proto/
│       └── browser_stream.proto
├── core-utils/          # Shared utilities
│   └── src/lib.rs       # RGBA to YUV conversion
└── avif_poc/           # AVIF decoding proof of concept
```

### Key Dependencies

- **Server**: `headless_chrome`, `rav1e`, `tonic`
- **Client Options**:
  - **Macroquad Client**: `macroquad`, `aom-decode`, `tonic`
  - **Iced Client**: `iced`, `iced_futures`, `aom-decode`, `tonic` (Recommended)
- **Protocol**: `prost`, `tonic-build`

### Protocol Definition

The gRPC protocol is defined in `proto-def/proto/browser_stream.proto`:

```protobuf
service RemoteBrowser {
  rpc StreamSession(stream ClientEvent) returns (stream ServerUpdate);
}

message StreamConfig {
  bool enable_video = 1;
  bool enable_spatial_links = 2;
  bool force_keyframes = 3;  // Client-controlled keyframe enforcement
}
```

## Troubleshooting

### Common Issues

#### "libaom-dev not found"
```bash
# Ubuntu/Debian
sudo apt install libaom-dev

# Or build from source if package unavailable
git clone https://aomedia.googlesource.com/aom
cd aom
mkdir build && cd build
cmake .. -DBUILD_SHARED_LIBS=1
make -j$(nproc)
sudo make install
```

#### "Connection refused"
Ensure the server is running on `[::1]:50051`:
```bash
netstat -ln | grep 50051
```

#### "AVIF decode failed"
Verify keyframe enforcement is enabled (default: true). The aom-decode library requires all frames to be keyframes for independent decoding.

#### Chrome fails to start
The server runs Chrome in headless mode. Ensure Chrome/Chromium is installed:
```bash
google-chrome --version
# or
chromium-browser --version
```

### Debug Mode

Enable debug logging:
```bash
RUST_LOG=debug cargo run --bin cloud-render-srv -- serve
RUST_LOG=debug cargo run --bin macroquad-client
```

### Performance Tuning

#### Server-side
- Adjust `SpeedSettings::from_preset(10)` for encoding speed vs quality
- Modify thread count in encoder configuration
- Tune screenshot capture interval (default: 33ms for 30 FPS)

#### Client-side
- Keyframe enforcement reduces bandwidth but increases file size
- Disable spatial links if not needed for better performance
- Adjust viewport size for network constraints

## Architecture Notes

### Keyframe Enforcement

The aom-decode library is designed for still AVIF images and doesn't support interframe video coding. To enable real-time streaming:

1. Client sends `force_keyframes: true` in StreamConfig
2. Server encodes all frames as keyframes
3. Each frame can be decoded independently
4. No reference frame maintenance required

This design simplifies the client decoder while maintaining real-time performance.

### Memory Management

- Server: Frame buffers are recycled after encoding
- Client: AVIF frames are decoded and discarded immediately
- Protocol: Streaming prevents memory accumulation

### Network Considerations

- Keyframe-only encoding increases bandwidth (~2-3x)
- gRPC streaming provides backpressure handling
- Client can dynamically adjust configuration based on network conditions

## License

This project is licensed under the GNU General Public License v3.0. See the [LICENSE](LICENSE) file for details.

## GPL Compliance

This project is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.

## AI Assistance Disclaimer

This project was created with the assistance of AI models and tools:

- **Gemini 3.1 Pro** - Advanced reasoning and code generation
- **Gemini 3 Flash** - Fast code completion and debugging
- **SWE-1.5** - Software engineering assistance
- **Google** - Research and documentation
- **Anthropic** - Code review and optimization
- **Windsurf** - Development environment and tooling

The human developer provided direction, requirements, design decisions, and final approval of all code changes. AI tools were used as assistants to accelerate development while maintaining code quality and architectural integrity.
