# Latency Protocol and Analysis Implementation Plan

## Goal
To investigate and minimize the end-to-end latency from the DOM rendering in Chrome to the final pixel display in the Macroquad client. We will accomplish this by providing modes to completely bypass the AV1 encoder (`--raw-rgb`) and viewport clipping overhead (`--full-page`). Additionally, we will introduce programmatic latency measurements via a timestamp extraction feature.

## Feature 1: Full-Page Screenshots (`--full-page`)
When activated, `chromiumoxide` is instructed to capture the entire available page ignoring the current viewport. 

**Coordinate Mapping for Links:**
`chromiumoxide` extracts link bounding boxes relative to the top-left origin of the document (absolute positioning). For a full-page screenshot, the entire image covers from `0` to `document_height`. So, a link at `(x=50, y=1000)` resides exactly at pixel `(50, 1000)` in the image textually. 
- **Server:** Will send the full image and enforce `viewport_x = 0` and `viewport_y = 0` for the `VideoFrame`.
- **Client:** Receives an image equal to the size of the document. The client draws the image starting at the `local_scroll_y` offset (panning the oversized texture). Because the spatial metadata is also absolute to the document, `(x, y - local_scroll_y)` correctly places the link highlights exactly above the visual text on the full-page texture without any complex coordinate synchronization.

## Feature 2: Raw RGB Transmission (`--raw-rgb`)
To eliminate arbitrary delays inside the AV1 encoder (such as lookahead buffers scaling over multiple frames), we add a raw RGB transmission mode.
- We skip converting from RGBA to YUV420.
- We pack the image into raw RGB bytes (excluding the Alpha channel to save 25% bandwidth).
- The client reconstructs this directly to a texture, removing `av1-decode` completely from the pipeline.

## Feature 3: Timestamp Text Extraction
To measure precise latency without relying on visual OCR algorithms:
- The test HTML `calibration.html` embeds a Unix timestamp enclosed in delimiters (`__TIMESTAMP_START__...__TIMESTAMP_END__`).
- The server extracts this timestamp immediately via Javascript (`page.evaluate`) before capturing the screenshot.
- The extracted string is wrapped inside the gRPC `VideoFrame` payload.
- Upon receiving and decoding the frame, the client parses the timestamp and subtracts it from the current system time to log the exact end-to-end latency.

## Feature 4: Configurable Stream Delay (`--loop-delay`)
Currently, there is a hardcoded sleep (`tokio::time::sleep(Duration::from_millis(2500)).await;`) at the end of the server event loop which throttles FPS to 0.4. There is also a 1-second delay upon navigation.
- We will add a command-line argument `--loop-delay <ms>` (defaulting to `2500` for backwards compatibility with the current tests).
- When benchmarking latency, we can set this to `0` or `16` to push the pipeline to its limits and measure true system latency without artificial bottlenecks.

## Implementation Steps
### 1. Protobuf Definitions
Modify `browser_stream.proto`:
- Add `bool full_page` and `bool raw_rgb` to `StreamConfig`.
- Add `bytes raw_rgb_data`, `uint32 frame_width`, `uint32 frame_height`, and `string timestamp_text` to `VideoFrame`.

### 2. Command Line Parameters
- **`cloud-render-srv`**: Add `--full-page`, `--raw-rgb`, and `--loop-delay` to the `clap` parser.
- **`macroquad-client`**: Add `--full-page` and `--raw-rgb` to the `clap` parser.

### 3. Server Logic (`cloud-render-srv`)
- **Screenshotting**: Update `CdpStream::capture_screenshot` to accept `full_page: bool`. Use `ScreenshotParams::builder().full_page(true).build()`.
- **Delay Control**: Replace the `tokio::time::sleep` in the loop with the value provided from `--loop-delay`. Remove the 1000ms hardcoded delay in `cdp_stream::navigate_to`.
- **Timestamping**: Before taking the screenshot, run `page.evaluate("document.getElementById('timestamp').textContent")`. Extract the start-to-end delimiter payload.
- **Bypassing AV1**: If `raw_rgb` is requested, take the uncompressed RGBA, strip the alpha channel, and put it into `VideoFrame.raw_rgb_data`. Bypass `rav1e` altogether.

### 4. Client Logic (`macroquad-client`)
- Send the `full_page` and `raw_rgb` configurations in the initial `Config` client event.
- In `grpc_client.rs`, when a `VideoFrame` arrives, check `frame.raw_rgb_data`:
  - If heavily populated, skip the AV1 decode stage. Reformulate the RGBA buffer for Macroquad by adding `255` as alpha channel per RGB triplet.
- Check `frame.timestamp_text`. Extract the Unix timestamp, retrieve the client's current Unix timestamp, and calculate `latency = current - start`. Log this using the `info!` macro.

## Verification
1. Run server `cargo run --bin cloud-render-srv --release -- --full-page --raw-rgb --loop-delay 16`
2. Run client `cargo run --bin macroquad-client --release -- --full-page --raw-rgb`
3. Observe `client.log` tracking frame decoding speeds and accurate end-to-end system latency measurements.
