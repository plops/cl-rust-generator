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
| Minimum | 252 |
| Average | 299.2 |
| Maximum | 352 |

### 2. Still Picture profile (Lowest Latency AV1)
**Configuration**: *(Default Settings)*
*Description*: Encodes standalone AVIF/AV1 intra-frames using screen content tools (`still_picture=true`, `low_latency=true`, `rdo_lookahead=1`). Ensures every frame is independently coded with zero temporal buffering.

| Metric | Measured Latency (ms) |
| --- | --- |
| Minimum | *No data - encoder issue* |
| Average | *No data - encoder issue* |
| Maximum | *No data - encoder issue* |

*Note: AV1 encoder configuration issue prevented frame output. Server captured frames but rav1e encoder produced no packets.*

### 3. Low Latency Video Profile
**Configuration**: `--disable-still-picture`
*Description*: Enables temporal inter-prediction while maintaining `low_latency=true` (disables frame-reordering/B-frames) and minimal RDO lookahead. Leverages temporal compression but avoids significant reordering buffering delays.

| Metric | Measured Latency (ms) |
| --- | --- |
| Minimum | 1622 |
| Average | 1622 |
| Maximum | 1622 |

### 4. Max Efficiency Video Profile
**Configuration**: `--disable-still-picture --disable-low-latency --rdo-lookahead-frames 20`
*Description*: Optimizes for maximum compression efficiency. Enables `still_picture=false`, enables frame-reordering (`low_latency=false`), and sets a deep RDO lookahead buffer of 20 frames. Expected to yield the highest latency due to deep GOP buffering before initial output.

| Metric | Measured Latency (ms) |
| --- | --- |
| Minimum | *No data - encoder issue* |
| Average | *No data - encoder issue* |
| Maximum | *No data - encoder issue* |

### 5. High Network Payload (Large Frame Size)
**Configuration**: `--quantizer 50`
*Description*: Uses the low-latency default settings but forces an extremely high visual quality, producing very large frame sizes. This tests the impact of network transmission times on overall latency.

| Metric | Measured Latency (ms) |
| --- | --- |
| Minimum | *No data - encoder issue* |
| Average | *No data - encoder issue* |
| Maximum | *No data - encoder issue* |

### 6. High CPU Load (Slow Encoding Preset)
**Configuration**: `--speed-preset 5`
*Description*: Uses the low-latency default settings but lowers the speed preset from 10 to 5. This forces rav1e to spend more CPU time optimizing the frame, demonstrating the processing time impact on latency.

| Metric | Measured Latency (ms) |
| --- | --- |
| Minimum | *No data - encoder issue* |
| Average | *No data - encoder issue* |
| Maximum | *No data - encoder issue* |

## Test Results Summary

**Successful Tests:**
- ✅ **RAW RGB**: 252-352ms (baseline latency without encoding)
- ⚠️ **Low Latency Video**: 1622ms (single data point, high latency due to encoder initialization)

**Failed Tests:**
- ❌ **Still Picture, Max Efficiency, High Network, High CPU**: AV1 encoder configuration issue prevented packet output

**Key Findings:**
1. RAW RGB provides the lowest latency (252-352ms) as expected
2. AV1 encoding configurations have issues preventing consistent frame output
3. The rav1e encoder may need additional configuration or more frames to initialize properly
4. Low latency video showed much higher latency (1622ms) but this may be due to encoder initialization overhead

**Recommendations:**
1. Investigate rav1e encoder configuration to fix packet output issues
2. Test with higher frame counts to allow encoder initialization
3. Consider encoder warm-up period before measuring latency
4. Verify timestamp synchronization between server and client
