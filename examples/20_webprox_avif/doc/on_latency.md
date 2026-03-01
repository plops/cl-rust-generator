# Timestamp Analysis and Latency Hypotheses

## Key Timestamp Findings

### Connection Latency
- **Server Start**: 22:51:02.680
- **Client Start**: 22:52:00.245  
- **Connection Established**: 22:52:00.247 (instant - 2ms)
- **First Metadata**: 22:52:04.522 (4.3 seconds after connection)
- **First Video Frame**: 22:52:05.868 (5.6 seconds after connection)

### Frame Processing Latency
**Encoder Bottleneck Analysis**:
- Frame 13: Sent to encoder at 22:52:05.655, received at 22:52:05.867 = **212ms encoding latency**
- Frame 14: Sent to encoder at 22:52:05.941, received at 22:52:06.147 = **206ms encoding latency**  
- Frame 15: Sent to encoder at 22:52:06.215, received at 22:52:06.352 = **137ms encoding latency**
- Frame 16: Sent to encoder at 22:52:06.416, received at 22:52:06.553 = **137ms encoding latency**

**Network Transfer Latency**:
- Server sends frame at 22:52:05.867, client receives at 22:52:05.868 = **1ms**
- Server sends frame at 22:52:06.147, client receives at 22:52:06.147 = **0ms**

**Client Decoding Latency**:
- Receives at 22:52:05.868, decoded at 22:52:05.895 = **27ms**
- Receives at 22:52:06.147, decoded at 22:52:06.164 = **17ms**

## Hypotheses

### 1. **Primary Issue: AV1 Encoder Bottleneck**
- **Evidence**: 137-212ms encoding latency per frame
- **Impact**: This is the dominant latency source, far exceeding the 33ms budget for 30 FPS
- **Root Cause**: Rav1e encoder configuration may be too slow/quality-focused

### 2. **Secondary Issue: Chrome Browser Startup Delay**
- **Evidence**: 4.3 seconds from connection to first metadata
- **Impact**: Significant initial connection delay
- **Root Cause**: Chrome initialization and page load overhead

### 3. **Tertiary Issue: Early Frame Buffering**
- **Evidence**: Frames 0-12 show "No packets received from encoder"
- **Impact**: No visual feedback for first ~400ms after Chrome loads
- **Root Cause**: Encoder needs warm-up period before producing output


