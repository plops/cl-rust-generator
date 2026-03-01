# Implementation Plan 09: Client Threading Model Fix & Code Modularization

**Status:** Planned  
**Objective:** Resolve the severe threading collision between `macroquad` and `tokio`, and refactor `macroquad-client/src/main.rs` into a modular, maintainable structure as strictly defined in `05_architecture_and_validation.md`.

---

## 1. Problem Statement

### 1.1 The Threading Clash
Currently, the `macroquad-client` runs the `tokio` asynchronous runtime on the **main thread** using `rt.block_on(...)` and attempts to spawn the GUI (`miniquad::start`) on a **background thread**.
* **Why it fails:** Almost all operating systems (Windows, macOS, Wayland/X11) enforce a strict rule that the UI/Window context *must* be initialized and run on the primary main thread. Spawning the UI on a secondary thread leads to immediate crashes, deadlocks, or undefined behavior. 
* **Additionally:** Macroquad uses its own custom coroutine executor, which is incompatible with Tokio. Mixing them in the same thread execution scope leads to severe panics.

### 1.2 The Monolithic Codebase
`macroquad-client/src/main.rs` currently contains over 400 lines of mixed concerns: CLI parsing, GUI rendering, Tokio gRPC streaming, State management, and Video decoding. This violates the anti-monolith guidelines established in `05_architecture_and_validation.md`.

---

## 2. The New Architecture: Strict Thread Separation

To solve the UI thread constraints, we will inverse the threading model and strictly isolate the executors using thread-safe channels and shared state locks.

1. **Main Thread (Macroquad Custom Executor):** Owns the OS window, handles user input (scroll, click), and renders the textures at 60 FPS.
2. **Background Thread (Tokio Runtime):** Owns the gRPC network connection, receives packets from the server, and runs the heavy AVIF decoding logic (`aom-decode`).

### Communication Bridge
* **Network to UI:** `Arc<Mutex<ClientState>>`. The Tokio thread locks this state to push new frames and metadata. The UI thread locks it briefly per frame to pull the latest image and draw it.
* **UI to Network:** `tokio::sync::mpsc::unbounded_channel()`. The UI thread uses the synchronous `send()` method of an unbounded sender to push user actions (scrolls, clicks) without blocking or awaiting. The Tokio thread `recv().await`s these messages and streams them to the gRPC server.

---

## 3. Modularization Strategy (File Splitting)

The monolithic `main.rs` will be refactored into the exact directory structure outlined in the validation plan.

### Directory Tree Overview
```text
macroquad-client/src/
├── main.rs                  // Entry point, thread spawning, CLI setup
├── state.rs                 // Shared state structs (ClientState, ClientImage)
├── integration_test.rs      // Agent testing mode (untouched logic, relocated)
├── network/
│   ├── mod.rs
│   └── grpc_client.rs       // Tokio gRPC event loop
├── render/
│   ├── mod.rs
│   ├── ui.rs                // UI rendering logic (Skeleton UI, Scrollbars)
│   └── main_loop.rs         // The macroquad #[macroquad::main] loop
└── video/
    ├── mod.rs
    └── decoder.rs           // aom-decode wrapper and fallback visualization
```

### Module Responsibilities

#### `src/main.rs`
Parses CLI arguments. If in integration test mode, it bootstraps the test. Otherwise, it creates the `Arc<Mutex<ClientState>>` and the MPSC channel. It spawns the Tokio background thread, and then hands control over to `macroquad`'s main macro.

#### `src/state.rs`
Contains `ClientState` and `ClientImage`. Completely decoupled from Tokio and Macroquad's heavy imports to allow easy sharing across the crate.

#### `src/network/grpc_client.rs`
Contains `start_grpc_client`. Runs purely inside `tokio::spawn`. It streams `ClientEvent` messages emitted by the UI over the MPSC channel to the server, while simultaneously receiving `ServerUpdate` messages. Upon receiving a frame, it passes it to the `video::decoder`.

#### `src/video/decoder.rs`
Houses the `Av1Decoder` logic using the `aom-decode` crate. It transforms the AVIF bytes into an `ImageBuffer<Rgba<u8>>` and maps it to `ClientImage` before injecting it into the Mutex state.

#### `src/render/main_loop.rs` & `src/render/ui.rs`
`main_loop.rs` replaces the raw `miniquad` EventHandler approach with the idiomatic `macroquad` loop.
`ui.rs` contains the layout, hit-testing, and drawing instructions for the virtual framebuffer and blue skeleton UI links.

---

## 4. Execution Plan (Step-by-Step)

### Phase 1: Create Structure & Extract State
1. Create directories: `src/network`, `src/render`, `src/video`.
2. Move `ClientState`, `ClientImage` and trait implementations to `src/state.rs`.

### Phase 2: Refactor Video & Network
1. Move `Av1Decoder` implementation into `src/video/decoder.rs`.
2. Move `start_grpc_client` into `src/network/grpc_client.rs`. Update it to accept `UnboundedReceiver<ClientEvent>` and process it via `tokio_stream::wrappers::UnboundedReceiverStream`.

### Phase 3: Rewrite Render Loop & Main Entry Point
1. Remove `miniquad::start` and `EventHandler` traits.
2. Implement idiomatic `macroquad`:
```rust
// src/render/main_loop.rs
pub async fn run_render_loop(state: Arc<Mutex<ClientState>>, event_tx: UnboundedSender<ClientEvent>) {
    loop {
        // 1. Process local input & send to event_tx
        // 2. Lock state to fetch latest texture / metadata
        // 3. Draw texture & UI
        next_frame().await;
    }
}
```
3. Wire the threads together in `main.rs`:
```rust
// src/main.rs
#[macroquad::main("AV1 Remote Browser")]
async fn main() {
    let shared_state = Arc::new(Mutex::new(ClientState::default()));
    let (tx_events, rx_events) = tokio::sync::mpsc::unbounded_channel();
    
    let state_clone = shared_state.clone();
    
    // Spawn network completely independent of macroquad's executor
    std::thread::spawn(move || {
        let rt = tokio::runtime::Runtime::new().unwrap();
        rt.block_on(async {
            network::grpc_client::start_grpc_client(state_clone, rx_events).await;
        });
    });

    // Main thread is now correctly owned by Macroquad
    render::main_loop::run_render_loop(shared_state, tx_events).await;
}
```

### Phase 4: Clean Up & Verification
1. Ensure all features (`integration-test`) remain functional.
2. Compile and ensure the window spawns safely on the host OS without deadlocking the gRPC stream.
