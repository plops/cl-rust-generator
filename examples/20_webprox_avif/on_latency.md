# Latency Measurement Validation

This document tracks the measured latency across various encoding strategies using `cloud-render-srv`.
The tests measure end-to-end latency logged by the client upon receiving a frame, calculated using the difference between the frame's `timestamp` extraction block and the client's current time.

## Test Methodology

Measurements are taken by automated coding agents running both the server and client over a sequence of 100 frames (`--max-frames 100`). The loop delay parameter is held constant (`--loop-delay 500` on the server) to maintain a steady stream of capture events. The client's output logs with the prefix `LATENCY: <time>ms` are extracted to formulate these aggregate statistics.

The evaluated configurations explore the trade-offs between bandwidth efficiency, CPU load, and frame latency.

### 1. RAW RGB Profile
**Configuration**: `--raw-rgb`
*Description*: Sends raw uncompressed RGB data bypassing the `rav1e` encoder entirely. Provides baseline latency floor bounded only by network constraints and capture times.

| Metric | Measured Latency (ms) |
| --- | --- |
| Minimum | |
| Average | |
| Maximum | |

### 2. Still Picture profile (Lowest Latency AV1)
**Configuration**: *(Default Settings)*
*Description*: Encodes standalone AVIF/AV1 intra-frames using screen content tools (`still_picture=true`, `low_latency=true`, `rdo_lookahead=1`). Ensures every frame is independently coded with zero temporal buffering.

| Metric | Measured Latency (ms) |
| --- | --- |
| Minimum | |
| Average | |
| Maximum | |

### 3. Low Latency Video Profile
**Configuration**: `--disable-still-picture`
*Description*: Enables temporal inter-prediction while maintaining `low_latency=true` (disables frame-reordering/B-frames) and minimal RDO lookahead. Leverages temporal compression but avoids significant reordering buffering delays.

| Metric | Measured Latency (ms) |
| --- | --- |
| Minimum | |
| Average | |
| Maximum | |

### 4. Max Efficiency Video Profile
**Configuration**: `--disable-still-picture --disable-low-latency --rdo-lookahead-frames 20`
*Description*: Optimizes for maximum compression efficiency. Enables `still_picture=false`, enables frame-reordering (`low_latency=false`), and sets a deep RDO lookahead buffer of 20 frames. Expected to yield the highest latency due to deep GOP buffering before initial output.

| Metric | Measured Latency (ms) |
| --- | --- |
| Minimum | |
| Average | |
| Maximum | |

### 5. High Network Payload (Large Frame Size)
**Configuration**: `--quantizer 50`
*Description*: Uses the low-latency default settings but forces an extremely high visual quality, producing very large frame sizes. This tests the impact of network transmission times on overall latency.

| Metric | Measured Latency (ms) |
| --- | --- |
| Minimum | |
| Average | |
| Maximum | |

### 6. High CPU Load (Slow Encoding Preset)
**Configuration**: `--speed-preset 5`
*Description*: Uses the low-latency default settings but lowers the speed preset from 10 to 5. This forces rav1e to spend more CPU time optimizing the frame, demonstrating the processing time impact on latency.

| Metric | Measured Latency (ms) |
| --- | --- |
| Minimum | |
| Average | |
| Maximum | |
