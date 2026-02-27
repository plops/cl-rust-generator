# webprox — Remote Browser Proxy over gRPC

A cloud proxy server that runs headless Chromium, transcodes web pages to Markdown, and streams them over a bidirectional gRPC connection to a minimal TUI client. Designed for extremely low-bandwidth links (10 KB/s target).

```
┌──────────────┐       gRPC/HTTP2        ┌──────────────────┐
│  TUI Client  │◄──────────────────────►  │  Cloud Proxy     │
│  (terminal)  │   Markdown + patches     │  (headless       │
│              │   ← PageUpdate stream    │   Chromium)      │
│  crossterm   │   Interaction stream →   │                  │
└──────────────┘                          └──────────────────┘
```

The server does all the heavy lifting — rendering, readability extraction, link rewriting, DOM diffing, zstd compression — so the client receives only lightweight Markdown text.

## Prerequisites

- **Rust** stable toolchain (1.70+). Install via [rustup](https://rustup.rs/).
- **Protobuf compiler** (`protoc`). Required by `tonic-build` to compile `.proto` files.
- **Chromium** or **Google Chrome** installed and accessible in `$PATH` (the server launches it headless via CDP).

### Installing protoc

```bash
# Debian/Ubuntu
sudo apt install -y protobuf-compiler

# Fedora
sudo dnf install -y protobuf-compiler

# macOS
brew install protobuf

# Arch
sudo pacman -S protobuf
```

### Installing Chromium

```bash
# Debian/Ubuntu
sudo apt install -y chromium-browser

# Fedora
sudo dnf install -y chromium

# macOS
brew install --cask chromium
```

## Building

```bash
# Debug build (faster compile, larger binary)
cargo build

# Release build (optimized for size: opt-level=z, LTO, strip)
cargo build --release
```

Release binaries land in `target/release/`. The release profile is tuned aggressively for small binaries (`panic=abort`, `strip=true`, `lto=true`, `codegen-units=1`).

## Running

### 1. Start the server

```bash
# Debug
cargo run --bin cloud-proxy-server

# Release
./target/release/cloud-proxy-server
```

The server listens on `[::1]:50051` (IPv6 localhost) by default. You'll see:

```
Starting cloud-proxy-server on [::1]:50051
```

### 2. Start the client

In a separate terminal:

```bash
# Debug
cargo run --bin minimal-tui-client

# Release
./target/release/minimal-tui-client

# Connect to a different address
./target/release/minimal-tui-client http://your-server:50051
```

The client defaults to `http://[::1]:50051` if no argument is given.

## Usage

Once the client is running, you're in a vim-like modal TUI:

### Normal mode (default)

| Key | Action |
|-----|--------|
| `g` | Open URL bar — type a URL and press Enter |
| `j` / `↓` | Scroll down |
| `k` / `↑` | Scroll up |
| `i` | Enter text input mode (for form fields) |
| `q` | Quit |
| `Ctrl+C` | Quit (works in any mode) |
| Mouse click | Click a link (if terminal supports mouse) |
| Scroll wheel | Scroll up/down |

### URL bar mode (after pressing `g`)

Type a URL and press Enter. Prefix `https://` is added automatically if omitted. Press Escape to cancel.

### Text input mode (after pressing `i`)

Keystrokes are forwarded to the focused form field in the remote browser. Press Enter to submit, Escape to return to normal mode.

### Links

Links appear as `[Link Text][1]` with numeric IDs. On terminals that support OSC 8 hyperlinks (Ghostty, WezTerm, iTerm2), links are clickable natively. On other terminals, click with the mouse to follow a link, or note the numeric ID.

## Security / TLS

To encrypt the gRPC connection, you can launch the server and client with TLS enabled.

First, generate a certificate authority (CA) and a server certificate signed by it:
```bash
# 1. Create a CA certificate and key
openssl genrsa -out ca-key.pem 4096
openssl req -new -x509 -key ca-key.pem -out ca-cert.pem -days 3650 -subj "/CN=Webprox CA"

# 2. Create a server key and certificate signing request (CSR)
openssl genrsa -out key.pem 4096
openssl req -new -key key.pem -out server.csr -subj "/CN=localhost"

# 3. Sign the server CSR with the CA
cat <<EOF > server.ext
authorityKeyIdentifier=keyid,issuer
basicConstraints=CA:FALSE
keyUsage = digitalSignature, nonRepudiation, keyEncipherment, dataEncipherment
subjectAltName = @alt_names

[alt_names]
DNS.1 = localhost
IP.1 = ::1
IP.2 = 127.0.0.1
EOF

openssl x509 -req -in server.csr -CA ca-cert.pem -CAkey ca-key.pem -CAcreateserial -out cert.pem -days 365 -extfile server.ext
```

Start the server using the generated server certificate and key:
```bash
cargo run --bin cloud-proxy-server -- --tls-cert cert.pem --tls-key key.pem
```
*(Note: When the server is started with TLS, clients must use TLS to connect; plaintext connections are rejected at the transport layer.)*

Start the client pointing to an `https://` endpoint. Provide the CA certificate with `--tls-ca` to trust the server:
```bash
cargo run --bin minimal-tui-client -- --server https://localhost:50051 --tls-ca ca-cert.pem
```

## Architecture

```
webprox/
├── proto-definitions/     # Shared .proto schema + generated Rust types
│   └── proto/browser.proto
├── cloud-proxy-server/    # The proxy server
│   └── src/
│       ├── main.rs        # Entry point, tonic server startup
│       ├── service.rs     # BrowsingService gRPC impl, priority queue egress
│       ├── session.rs     # Session lifecycle, idle reaper (5min timeout)
│       ├── browser.rs     # Chromium pool, CDP network interception, DOM watcher
│       ├── extractor.rs   # readability → markdown → link_map pipeline
│       ├── differ.rs      # VDOM diffing, patch serialization
│       ├── compressor.rs  # zstd compression for patches
│       ├── throttle.rs    # Debouncer, priority queue, bandwidth budget
│       └── input.rs       # CDP keyboard dispatch
└── minimal-tui-client/    # The TUI client
    └── src/
        ├── main.rs        # Event loop, reconnection logic
        ├── connection.rs  # gRPC client, exponential backoff reconnect
        ├── state.rs       # Page state, patch application
        ├── renderer.rs    # Markdown → ANSI rendering, OSC 8 links, hitbox map
        ├── input.rs       # Crossterm input, modal state machine, mouse handling
        └── echo.rs        # Local echo, loading indicators, status bar
```

### How it works

1. Client sends a `NavigateRequest` with a URL.
2. Server navigates headless Chromium to that URL, blocking images/CSS/fonts/media via CDP network interception.
3. Server runs the HTML through `readability-rust` to strip ads and sidebars.
4. Server converts clean HTML to Markdown via `html-to-markdown-rs`, rewriting all links to short numeric IDs (saving bandwidth).
5. Server sends a `PreRenderedPage` (Markdown + link map + title) through the priority queue.
6. Client renders Markdown to the terminal with ANSI colors and OSC 8 hyperlinks.
7. On subsequent DOM mutations, the server diffs the VDOM, compresses patches with zstd, and sends incremental `DomDelta` updates instead of full pages.
8. All egress is routed through a 4-tier priority queue with a 10 KB/s bandwidth budget. Under congestion, low-priority patches are dropped in favor of status messages and content.

### Bandwidth management

The server enforces a 10 KB/s egress budget with a priority system:

| Priority | Type | Behavior under congestion |
|----------|------|--------------------------|
| 0 (highest) | Status messages | Always sent |
| 1 | Full page content | Always sent |
| 2 | Incremental DOM patches | Dropped if >80% budget used |
| 3 (lowest) | Metadata | Dropped if >80% budget used |

A 75ms debouncer coalesces rapid DOM mutation events before triggering re-extraction.

## Configuration

There's no config file — the server binds to `[::1]:50051` and the client connects there by default. To change the address, pass it as the first argument to the client.

To simulate a bandwidth-constrained link for testing:

```bash
# Throttle loopback to 10KB/s using Linux tc
sudo tc qdisc add dev lo root tbf rate 10kbit burst 1540 latency 50ms

# Remove throttle
sudo tc qdisc del dev lo root
```

## Troubleshooting

- **"Browser config error"** — Chromium isn't found. Make sure `chromium-browser` or `google-chrome` is in your `$PATH`.
- **Client shows "Connection failed"** — The server isn't running, or you're connecting to the wrong address. Check that the server started successfully.
- **Blank page after navigation** — Some SPAs require JavaScript execution time. The server waits for navigation to complete, but very heavy JS apps may not render fully.
- **Garbled terminal after crash** — Run `reset` to restore your terminal state.

## License

This project is provided as-is for educational and experimental use.
