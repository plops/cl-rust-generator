# Refactoring Plan: AVIF Remote Browser

## 1. Goal

The purpose of this refactoring pass is to break apart large, monolithic files and functions that have grown organically during prototyping. We will apply established architectural and programming patterns to improve maintainability, testability, and code clarity.

## 2. Identified Problem Areas

During the codebase review, the following files were flagged for being too large or complex:

### `cloud-render-srv/src/main.rs` (34 KB / 793 lines)
- **Issue**: Highly cohesive but functionally dense. 
- **Key Offender**: The `run_browser_session` function spans over 400 lines. It mixes application state management, browser communication, video encoding, and gRPC streaming into a single, massive asynchronous loop.

### `macroquad-client/src/network/grpc_client.rs` (13 KB / 266 lines)
- **Issue**: Monolithic network handler.
- **Key Offender**: The `start_grpc_client` function manages the connection lifecycle, channel state, asynchronous message processing, and frame buffering in a single 250-line stream.

### `macroquad-client/src/render/ui.rs` (7 KB / 167 lines)
- **Issue**: Tightly coupled UI and state.
- **Key Offender**: `draw_ui_and_frame` manages lock acquisition, view offsets, event handling (interpreting clicks to events), and raw drawing.

### `cloud-render-srv/src/session/test_handler.rs` (6 KB)
- **Issue**: Procedural and redundant. Mixes image conversion, encoder setup, and gRPC response dispatch inline inside test handlers.

---

## 3. Proposed Architectural Patterns

We'll use a combination of the **Actor Model**, **State Machine Pattern**, and **Strategy Pattern** to disentangle these modules.

### Pattern A: The Actor Model (Separation of Concerns via Message Passing)
Currently, asynchronous tasks in Rust are sharing state heavily with monolithic loops. We will refactor major async loops into dedicated "Actors" that communicate via structured messages (`tokio::sync::mpsc::channel`).
- **Server Actor**: `SessionActor` handles the lifetime of a connected user. It listens for events from a `BrowserActor` and an `EncoderActor`.
- **Client Actor**: Separate the `NetworkActor` (pure gRPC and reconnection logic) from the `StateActor` (managing incoming decoded frames and application state).

### Pattern B: The Strategy Pattern (For Encoders and Decoders)
- Extract the Rav1e encoding setup from `main.rs` into an implementer of a `VideoEncoder` strategy trait. This allows us to easily swap out mock encoders for testing without changing the main session loop.

### Pattern C: Model-View-Controller (MVC) adaptation for UI
- **Model**: `ClientState`
- **View**: Extract pure drawing logic into `view.rs`. It should take immutable references to state and not handle network sending.
- **Controller**: `input.rs` interprets mouse/scroll input and dispatches messages to the `NetworkActor`.

---

## 4. Execution Plan (File-By-File)

As per conventions, we will group highly cohesive logical modules into individual folders. Inside each folder, files will be ordered by importance/entrypoint using numerical prefixes (`01_...rs`, `02_...rs`, etc.) so the top-level orchestrator is easy to identify and read first.

### Step 4.1: Decoupling `cloud-render-srv/src/main.rs`
1. **Create `session/` module**:
    - `01_manager.rs`: Move the `run_browser_session` loop here. Create a `SessionManager` struct containing the gRPC stream and state, acting as the main entry point for the module.
2. **Create `encoder/` module**:
    - `01_rav1e_enc.rs`: Main entry point for encoder operations. Contains the `EncoderConfig` and `Context` initialization, as well as the frame pumping loop, via a dedicated struct `Rav1eEncoder`.
3. **Slim down `main.rs`**: `main.rs` should only parse CLI arguments, initialize the gRPC server, and bootstrap the `SessionManager`.

### Step 4.2: Breaking down `macroquad-client/src/network/grpc_client.rs`
1. **Rename module to `network/` (or keep)**:
    - `01_connection_manager.rs`: Manage the raw tonic connection and retry logic.
    - `02_stream_handler.rs`: Abstract the event matching (`match response.update`) from `start_grpc_client`. Move rendering-specific side effects into smaller functional blocks: `handle_video_frame`, `handle_spatial_metadata`.

### Step 4.3: Refactoring `macroquad-client/src/render/ui.rs`
1. **Rename module strictly to `view/` or `render/`**:
    - `01_main_loop.rs`: (Existing or newly decoupled top-level loop logic).
    - `02_ui.rs`: Isolate state extraction and pure drawing.
    - `03_input.rs`: Move `handle_input_and_send_events` here. The UI should only render, not handle network sending.

### Step 4.4: Cleaning up `test_handler.rs`
1. **Within `session/` folder**:
    - `02_test_handler.rs`: Delegate the encoding of test frames to the newly created `Rav1eEncoder` object, removing duplicated boilerplate in `test_handler.rs`.

---

## 5. Next Steps
Once this refactoring plan is approved, we will begin execution systematically, starting with the client (`grpc_client.rs` & `ui.rs`), followed by the server (`main.rs` -> `manager.rs` & `encoder.rs`).
