# Adding Low Latency Parameters to rav1e server

The goal is to add more command-line parameters to `cloud-render-srv` to control the latency of the `rav1e` server, prioritizing the lowest frame delay by default based on the rav1e Low-Latency Screenshot Encoding Guide. The help output will explicitly explain the effects of each parameter and document different encoding strategies.

## Proposed Changes

### Configuration Command Line Arguments (clap)

#### [MODIFY] main.rs(file:///home/kiel/stage/cl-rust-generator/examples/20_webprox_avif/cloud-render-srv/src/main.rs)

1. **Add `after_help` text to the `Cli` struct** to provide a global guide on encoding strategies in the help output (`-h` / `--help`).
```rust
#[command(
    author, version, about = "AV1 Remote Browser Server",
    after_help = "ENCODING STRATEGIES:
  1. RAW RGB (Lowest Latency, Highest Bandwidth): Use --raw-rgb
  2. Still Picture (Low Latency, Intra-only AVIF): [Default]
  3. Low Latency Video (Low Latency, Inter-prediction): Use --disable-still-picture
  4. Max Efficiency Video (Higher Latency, Best Compression): Use --disable-still-picture --disable-low-latency --rdo-lookahead-frames 20"
)]
```

2. **Add/Modify the following fields to the `Cli` struct** with explicit `help` text.

- `--disable-low-latency`: (bool) 
  - **Help text**: `Disable low_latency mode (enables frame reordering like B-frames). Increases encoding efficiency but adds significant frame delay. Default: low_latency is enabled.`
  - **Default for lowest latency**: Omitted (so `low_latency = true`).

- `--disable-still-picture`: (bool) 
  - **Help text**: `Disable still_picture mode (enables temporal inter-prediction). When enabled (default), forces standalone frames (intra-only) with screen content tools. Disabling this trades independence for better compression.`
  - **Default for lowest latency**: Omitted (so `still_picture = true`).

- `--rdo-lookahead-frames`: (usize)
  - **Help text**: `Number of lookahead frames for Rate-Distortion Optimization (1-40). Lower values reduce initial buffering delay. Default: 1 (minimum delay).`
  - **Default for lowest latency**: `1`

- `--min-keyint`: (u64)
  - **Help text**: `Minimum interval between keyframes. Default: 1.`
  - **Default for lowest latency**: `1`

- `--keyint`: (u64)
  - **Help text**: `Maximum interval between keyframes. Setting to 1 forces all frames to be keyframes. Default: 1.`
  - **Default for lowest latency**: `1`

- `--switch-frame-interval`: (u64)
  - **Help text**: `Interval for S-frames within a GOP (requires low-latency enabled). 0 disables. Default: 0.`
  - **Default for lowest latency**: `0`

- `--reservoir-frame-delay`: (i32)
  - **Help text**: `Rate control buffer size (12-131072). This controls bitrate smoothing span, not output delay. Default: 12 (minimum allowed).`
  - **Default for lowest latency**: `12`

- `--disable-error-resilient`: (bool)
  - **Help text**: `Disable error_resilient mode. When enabled (default), frames are independently decodable without prior context blocking.`
  - **Default for lowest latency**: Omitted (so `error_resilient = true`).

- `--max-frames`: (Option<usize>)
  - **Help text**: `Testing flag: Shutdown the server after sending this many frames.`
  - **Default for lowest latency**: `None`

3. **Document Existing Delay Parameters**: Update help text for existing parameters (if absent) to clarify their latency impact.
- `--loop-delay`: (u64)
  - **Help text**: `Stream loop delay in milliseconds. Controls the base time interval between frame captures. Default: 2500.`

4. **Update `EncoderConfig` in `run_browser_session`**:
```rust
    enc.low_latency = !cli.disable_low_latency;
    enc.still_picture = !cli.disable_still_picture;
    enc.speed_settings.rdo_lookahead_frames = cli.rdo_lookahead_frames;
    enc.min_key_frame_interval = cli.min_keyint;
    enc.max_key_frame_interval = cli.keyint;
    enc.switch_frame_interval = cli.switch_frame_interval;
    enc.reservoir_frame_delay = Some(cli.reservoir_frame_delay);
    enc.error_resilient = !cli.disable_error_resilient;
```

5. **Shutdown Logic**: Implement logic in `run_browser_session` to exit after `packet_count` >= `cli.max_frames.unwrap()`.

## Verification Plan

### Automated Tests
1. Compilation check: `cargo check --bin cloud-render-srv`
2. Validate the help output includes the explicit tooltips and the global encoding strategies summary: `cargo run --bin cloud-render-srv -- --help`

### Automated Latency Measurement Verification
We will use automated coding agents to run both the server and the macroquad-client across **6 different configurations**, extracting the latency prefix from the client log: `LATENCY: <time>ms`.
The client must log this per frame over 100 frames. The server will be bounded using the new `--max-frames 100` parameter.

**The 6 Test Scenarios:**

1. **RAW RGB**
   Server: `cargo run --bin cloud-render-srv serve --raw-rgb --max-frames 100 --loop-delay 500`
2. **Still Picture (Default Minimal Latency)**
   Server: `cargo run --bin cloud-render-srv serve --max-frames 100 --loop-delay 500`
3. **Low Latency Video**
   Server: `cargo run --bin cloud-render-srv serve --disable-still-picture --max-frames 100 --loop-delay 500`
4. **Max Efficiency Video**
   Server: `cargo run --bin cloud-render-srv serve --disable-still-picture --disable-low-latency --rdo-lookahead-frames 20 --max-frames 100 --loop-delay 500`
5. **High Network Payload (Large Frame Size)**
   Server: `cargo run --bin cloud-render-srv serve --quantizer 50 --max-frames 100 --loop-delay 500`
6. **High CPU Load (Slow Encoding Preset)**
   Server: `cargo run --bin cloud-render-srv serve --speed-preset 5 --max-frames 100 --loop-delay 500`

The extracted statistics (Min, Max, Avg latency) will be appended into the tables prepared in `doc/on_latency2.md`.
