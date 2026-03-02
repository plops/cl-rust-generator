# Scroll Latency Investigation Implementation

## Overview

This implementation adds comprehensive timing measurements and visual feedback to investigate scroll latency in the AV1 remote browser system.

## Changes Made

### 1. Enhanced Calibration HTML (`calibration.html`)

**Added Visual Counter Display:**
- Frame counter that increments on each `requestAnimationFrame`
- Real-time timestamp with millisecond precision
- Current scroll position tracking
- Time since last update
- Update rate (frames per second)
- Color-coded status indicators:
  - Green: Active (<100ms since last update)
  - Yellow: Slow (100-200ms since last update)  
  - Red: Stalled (>200ms since last update)

**JavaScript Features:**
- `UpdateTracker` class for continuous monitoring
- Enhanced scroll tracking with velocity calculation
- MutationObserver for DOM change detection
- Performance timing using `performance.now()`
- Console logging for debugging

### 2. Server Logging Enhancements (`cloud-render-srv/src/main.rs`)

**Scroll Event Timing:**
- Total scroll processing time measurement
- Browser scroll execution timing
- Fixed 100ms delay tracking
- Scroll-to-screenshot correlation tracking

**Screenshot Capture Timing:**
- Scroll-related frame detection
- Time since last scroll event tracking
- Enhanced logging for scroll-related captures

**Key Variables Added:**
- `last_scroll_time`: Tracks when scroll was processed
- Detailed timing measurements for each pipeline stage

### 3. Test Script (`test_scroll_latency.sh`)

**Automated Testing:**
- Builds server if needed
- Starts server with fast configuration and debug logging
- Provides comprehensive testing instructions
- Creates log files for analysis
- Monitors server health

## Usage

### Quick Start
```bash
# Start the enhanced server
./test_scroll_latency.sh

# In another terminal, start the client
./start_client_release.sh --log-level debug
```

### What to Watch For

**In the Browser (calibration page):**
1. **Frame Counter**: Should increment continuously
2. **Update Rate**: Should show ~60fps for smooth animation
3. **Color Status**: Green when responsive, yellow/red when lagging
4. **Scroll Position**: Updates in real-time as you scroll
5. **Console Logs**: Detailed scroll velocity and DOM mutation events

**In Server Logs:**
1. **Scroll Event Processing**: `[Server] Scroll event received`
2. **Browser Execution Time**: `[Server] Browser scroll execution took: Xms`
3. **Fixed Delay**: `[Server] Fixed 100ms delay completed: Xms`
4. **Total Processing**: `[Server] Total scroll processing time: Xms`
5. **Scroll Correlation**: `[Server] This screenshot appears to be scroll-related`

### Expected Baseline Performance

**Good Performance:**
- Browser scroll execution: <20ms
- Screenshot capture: <100ms
- Total scroll processing: <150ms (excluding 100ms delay)
- Frame counter increment: <200ms after scroll action

**Problem Indicators:**
- Red status on counter display
- Update rate dropping below 30fps
- Server logs showing >200ms processing times
- Long delays between scroll and counter increment

## Investigation Focus Areas

### 1. Fixed 100ms Delay
The server has a hardcoded `tokio::time::sleep(Duration::from_millis(100))` after browser scroll execution. This is a prime suspect for scroll latency.

**Questions to Answer:**
- Is this delay necessary for browser rendering stability?
- Can it be reduced or eliminated?
- What happens if we set it to 0ms?

### 2. Screenshot Capture Timing
Chrome DevTools Protocol screenshot capture may have variable performance.

**Metrics to Track:**
- Capture time during scroll vs static scenes
- Impact of scroll position on capture time
- Correlation with DOM complexity

### 3. Encoder Pipeline
The AV1 encoder may buffer frames during rapid scrolling.

**Indicators:**
- Frames backing up in encoder queue
- Variable encoding latency during scroll bursts
- Frame drops during rapid scrolling

## Next Steps for Optimization

1. **Test with 0ms delay**: Modify the fixed delay to measure impact
2. **Screenshot optimization**: Test different capture parameters
3. **Encoder tuning**: Adjust settings for scroll scenarios
4. **Frame dropping**: Consider dropping frames during rapid scroll
5. **Pre-warming**: Cache scroll positions or pre-render

## Data Collection

Run the test script and collect:
- Server logs with timing measurements
- Browser console output
- Visual observations of counter behavior
- Performance metrics under different scroll patterns

This data will help identify the actual bottlenecks and guide optimization efforts.
