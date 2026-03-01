Based on the provided code and the problem description, there are **two critical root causes** for the visual disconnect between the rendered server image and the interactive link highlights when scrolling:

1. **Client-Side Texture Scaling:** In `macroquad-client/src/render/ui.rs`, the client renders the AV1 texture using `dest_size: Some(vec2(screen_width(), screen_height()))`. If the server's screenshot dimensions (e.g., 1280x720) do not perfectly match the client's window size (defaults to 800x600 in macroquad), the image is stretched or squished. However, the DOM link coordinates (`link.x` and `link.y`) are *absolute* and are drawn **unscaled**. This leads to a severe misalignment where links appear to drift away from their visual anchors on the page.
2. **Server-Side Encoder Delay (Desync):** The `rav1e` AV1 encoder operates asynchronously and buffers frames. When `ctx.receive_packet()` yields a frame, it is an older frame that was fed into the encoder a few cycles ago. However, the server assigns it the *newest* `current_viewport_y`. As a result, the client receives an old image but renders it at the new offset position.

Here are the fixes to resolve these alignment issues and enforce a strict 1:1 pixel mapping.

### 1. Fix the Texture Scaling in Client (`macroquad-client/src/render/ui.rs`)

Change the texture rendering to use the native dimensions of the decoded frame. This ensures that the 1:1 coordinate mapping of the spatial metadata perfectly overlays the rendered video, regardless of the client window size.

```rust
// start of macroquad-client/src/render/ui.rs
<<<<
        draw_texture_ex(
            tex,
            0.0,
            render_offset_y,
            WHITE,
            DrawTextureParams {
                dest_size: Some(vec2(screen_width(), screen_height())),
                ..Default::default()
            },
        );
====
        draw_texture_ex(
            tex,
            0.0,
            render_offset_y,
            WHITE,
            DrawTextureParams {
                // Ensure 1:1 rendering scale to perfectly match the unscaled DOM metadata
                dest_size: Some(vec2(tex.width(), tex.height())),
                ..Default::default()
            },
        );
>>>>
```

### 2. Fix the Encoder Pipeline Desync in Server (`cloud-render-srv/src/main.rs`)

We need to add a `viewport_y_queue` to keep track of the exact scroll position *at the time a frame was captured* and pair it correctly when the encoded packet pops out of the pipeline. We also enforce instant scrolling and read back the clamped boundaries so the server doesn't get stuck over-scrolling past the document bottom.

```rust
// start of cloud-render-srv/src/main.rs
<<<<
    enc.min_key_frame_interval = 1;  // Every frame is a keyframe
    enc.max_key_frame_interval = 1;  // Force all frames to be keyframes
    
    let cfg = Config::new().with_encoder_config(enc).with_threads(2);
====
    enc.min_key_frame_interval = 1;  // Every frame is a keyframe
    enc.max_key_frame_interval = 1;  // Force all frames to be keyframes
    enc.low_latency = true;          // Crucial: disables frame reordering to minimize delay
    
    let cfg = Config::new().with_encoder_config(enc).with_threads(2);
>>>>
```

```rust
// start of cloud-render-srv/src/main.rs
<<<<
    let mut frame_count = 0;
    let mut current_viewport_y = 0i32;
    let mut force_keyframes = true; // Default to simplified mode
    
    loop {
====
    let mut frame_count = 0;
    let mut current_viewport_y = 0i32;
    let mut force_keyframes = true; // Default to simplified mode
    let mut viewport_y_queue = std::collections::VecDeque::new(); // Tracks exact Y for buffered frames
    
    loop {
>>>>
```

```rust
// start of cloud-render-srv/src/main.rs
<<<<
                    proto_def::graphical_proxy::client_event::Event::Scroll(scroll) => {
                        let old_viewport_y = current_viewport_y;
                        current_viewport_y = (current_viewport_y + scroll.delta_y).max(0);
                        info!("[Server] Scroll event: delta_y={}, old_viewport={}, new_viewport={}", 
                            scroll.delta_y, old_viewport_y, current_viewport_y);
                        // Execute scroll in browser
                        let scroll_script = format!("window.scrollTo(0, {});", current_viewport_y);
                        debug!("[Server] Executing scroll script: {}", scroll_script);
                        let _ = page.evaluate(scroll_script.as_str()).await?;
                        tokio::time::sleep(Duration::from_millis(100)).await;
                        info!("[Server] Browser scroll completed, viewport now at: {}", current_viewport_y);
                    }
====
                    proto_def::graphical_proxy::client_event::Event::Scroll(scroll) => {
                        let old_viewport_y = current_viewport_y;
                        current_viewport_y = (current_viewport_y + scroll.delta_y).max(0);
                        info!("[Server] Scroll event: delta_y={}, old_viewport={}, new_viewport={}", 
                            scroll.delta_y, old_viewport_y, current_viewport_y);
                        
                        // Execute instant scroll in browser and return actual clamped scroll position
                        let scroll_script = format!(
                            "(() => {{ window.scrollTo({{top: {}, behavior: 'instant'}}); return window.scrollY; }})()", 
                            current_viewport_y
                        );
                        debug!("[Server] Executing scroll script: {}", scroll_script);
                        if let Ok(res) = page.evaluate(scroll_script.as_str()).await {
                            if let Ok(actual_y) = res.into_value::<f64>() {
                                current_viewport_y = actual_y as i32;
                            }
                        }
                        tokio::time::sleep(Duration::from_millis(100)).await;
                        info!("[Server] Browser scroll completed, viewport now at: {}", current_viewport_y);
                    }
>>>>
```

```rust
// start of cloud-render-srv/src/main.rs
<<<<
        println!("[Server] Sending frame to encoder...");
        ctx.send_frame(frame).map_err(|e| format!("Send frame error: {:?}", e))?;
        println!("[Server] Frame sent to encoder");
        
        info!("[Server] Receiving encoded packets...");
        // Receive encoded packets
        let mut packet_count = 0;
        let mut found_packet = false;
        
        // Try to receive packets with a timeout
        while let Ok(packet) = ctx.receive_packet() {
            found_packet = true;
            println!("[Server] Sending packet {} with {} bytes", packet_count, packet.data.len());
            let update = ServerUpdate {
                update: Some(proto_def::graphical_proxy::server_update::Update::Frame(VideoFrame {
                    av1_data: packet.data,
                    is_keyframe: force_keyframes || packet.frame_type == FrameType::KEY,
                    viewport_x: 0,
                    viewport_y: current_viewport_y,
                })),
            };
====
        println!("[Server] Sending frame to encoder...");
        match ctx.send_frame(frame) {
            Ok(_) => {
                println!("[Server] Frame sent to encoder");
                // Store the current state to guarantee metadata perfectly aligns with the encoded image
                viewport_y_queue.push_back(current_viewport_y);
            },
            Err(e) => {
                warn!("[Server] Send frame error: {:?}", e);
            }
        }
        
        info!("[Server] Receiving encoded packets...");
        // Receive encoded packets
        let mut packet_count = 0;
        let mut found_packet = false;
        
        // Try to receive packets
        while let Ok(packet) = ctx.receive_packet() {
            found_packet = true;
            // Pop the viewport_y belonging exactly to the frame that finished encoding
            let packet_viewport_y = viewport_y_queue.pop_front().unwrap_or(current_viewport_y);
            
            println!("[Server] Sending packet {} with {} bytes", packet_count, packet.data.len());
            let update = ServerUpdate {
                update: Some(proto_def::graphical_proxy::server_update::Update::Frame(VideoFrame {
                    av1_data: packet.data,
                    is_keyframe: force_keyframes || packet.frame_type == FrameType::KEY,
                    viewport_x: 0,
                    viewport_y: packet_viewport_y,
                })),
            };
>>>>
```

### Why this fixes the problem:
By preventing the client from dynamically squishing the texture width/height to the window size, the absolute link coordinates (`link.x`, `link.y`) and pixel data operate on the same spatial plane. Also, by queueing `current_viewport_y` down into the `rav1e` pipeline and recovering it upon `receive_packet()`, we guarantee the client will never offset an older image visually assuming it represents a newer scrolling frame.