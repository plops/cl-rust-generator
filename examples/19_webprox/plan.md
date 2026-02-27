### IMPLEMENTATION ROADMAP

#### **Phase 1: Project Scaffolding & Core Architecture (Weeks 1-2)**
*Objective: Establish the monorepo, configure the Rust toolchain for binary optimization, and define the gRPC transport layer.*

*   **1.1 Repository & Workspace Setup:**
    *   Initialize a Cargo workspace with two core crates: `cloud-proxy-server` and `minimal-tui-client`.
    *   Establish a shared `proto-definitions` crate.
*   **1.2 Toolchain & Profile Optimization:**
    *   Enforce `opt-level = "z"`, `lto = true`, `codegen-units = 1`, `panic = "abort"`, and `strip = true` in the release profile of `Cargo.toml`.
    *   Audit transitive dependencies using `cargo tree` to prevent version duplication (e.g., ensuring only one version of `indexmap` is compiled).
*   **1.3 Protobuf Schema Implementation:**
    *   Write the `browser.proto` schema, defining the `BrowsingService` bidirectional stream.
    *   Implement the `Interaction` (Navigate, Click, Input) and `PageUpdate` (PreRenderedPage, DomDelta) `oneof` message structures.
    *   Use `prost` and `tonic-build` to generate type-safe Rust structs from the schema.

#### **Phase 2: Cloud Browser & Extraction Engine (Weeks 3-6)**
*Objective: Deploy the headless browser, execute semantic sanitization, output Markdown, and manage sessions.*

*   **2.1 CDP Automation Pipeline:**
    *   Integrate `chromiumoxide` to spawn an asynchronous headless Chromium instance.
    *   Configure network interceptors via CDP to block all `.png`, `.jpg`, `.mp4`, `.css`, and `.woff2` requests, simulating a strict text-only environment at the proxy level.
*   **2.2 Session Lifecycle Management:**
    *   Implement `sessions: Arc<Mutex<HashMap<String, Page>>>` to track active browsing sessions per client connection.
    *   Handle session creation on first `NavigateRequest`, cleanup on stream close or timeout (idle sessions reaped after 5 minutes).
    *   Ensure chromiumoxide browser contexts are properly dropped to prevent resource leaks on the server.
*   **2.3 Readability & Markdown Transcoding:**
    *   Pipe the raw HTML from the Chromium instance into the `readability-rust` engine to score and strip non-semantic DOM trees (ads, sidebars).
    *   Implement `html-to-markdown-rs` utilizing a custom visitor pattern to mutate `<a>` tags.
    *   **Crucial Data Structure:** Implement the `link_map<uint32, string>`. Replace all inline URLs with short numeric IDs (e.g., `[Link Text](1)`) to preserve wire bandwidth.
*   **2.4 Text Input Dispatch:**
    *   Implement the `InputRequest` handler in the server's session loop.
    *   On receiving an `InputRequest`, use CDP `Input.dispatchKeyEvent` to forward keystrokes to the currently focused element in the chromiumoxide page.
    *   Handle focus tracking — maintain a server-side record of the active input element to validate input dispatch targets.

#### **Phase 3: VDOM Diffing & Transport Optimization (Weeks 7-9)**
*Objective: Enable dynamic page updates using minimal delta payloads.*

*   **3.1 VDOM Synchronization:**
    *   Integrate the `mt-dom` crate on the server to maintain an in-memory Virtual DOM of the current page state.
    *   Establish an event listener on the CDP socket. On DOM mutation, compute the `mt-dom` patch (insertions, deletions, text updates).
*   **3.2 Compression & Debouncing:**
    *   Implement a `tokio::sync::mpsc` channel to aggregate rapid CDP events ("event debouncing") over a 50-100ms window to prevent network saturation.
    *   Initialize a persistent-context `zstd` compression dictionary. Compress the `mt-dom` patch before serializing it into the `DomDelta` protobuf bytes field.

#### **Phase 4: Terminal Client Implementation (Weeks 10-11)**
*Objective: Build the zero-bloat Unix TUI for consuming the gRPC stream.*

*   **4.1 TUI Foundation:**
    *   Initialize `tuinix` directly over `libc` to bypass heavy widget frameworks. Enable raw terminal mode and alternate screen buffer (`smcup`).
*   **4.2 Rendering & Interactivity:**
    *   Write a custom Markdown renderer that maps header/list structures to truecolor ANSI sequences.
    *   **Link Resolution:** Implement the OSC 8 escape sequence (`\x1b]8;;{url}\x1b\\{label}\x1b]8;;\x1b\\`) mapper, resolving numeric IDs from the server's `link_map`.
    *   Integrate `crossterm` event handling as a fallback for mouse tracking (X10/SGR protocols) to capture clicks on non-OSC-8 compliant terminals. Send local X/Y coordinates to a local hit-box map to trigger `ClickRequest` events.
*   **4.3 Text Input Capture:**
    *   Detect when the user activates an input field (via click or tab navigation on an interactive node).
    *   Enter a "text input mode" that captures raw keystrokes and streams them as `InputRequest` messages to the server.
    *   Display a local input line/cursor as visual feedback while typing.
*   **4.4 Local Echo & Latency Mitigation:**
    *   On link click, immediately restyle the clicked element locally (e.g., dim text, append "[Loading...]") to provide instant visual feedback while the round-trip completes.
    *   On text input, echo characters locally before server confirmation to mask network latency.
*   **4.5 Reconnection & Error Recovery:**
    *   Detect gRPC stream disconnects via tonic status codes and transport errors.
    *   Implement exponential backoff reconnection (initial 500ms, max 10s, 5 retries).
    *   On successful reconnect, send a fresh `NavigateRequest` for the last known URL to resynchronize state via a full `PreRenderedPage` snapshot.
    *   Display connection status in a persistent status bar (Connected / Reconnecting / Offline).

#### **Phase 5: Bandwidth Throttling & System Tuning (Weeks 12-14)**
*Objective: Guarantee operational stability under strict 10KB/s constraints.*

*   **5.1 Priority Queuing System:**
    *   Implement a 4-tier egress queue on the server stream.
    *   *Algorithm:* If buffer utilization > 80% of the 10KB/s quota, aggressively drop Priority 3 (Incremental DOM patches) and prioritize Priority 1 (Status messages) and Priority 2 (Markdown structure).
*   **5.2 End-to-End Simulation:**
    *   Deploy the server to a cloud staging environment.
    *   Use Linux `tc` (Traffic Control) to artificially throttle the network interface to exactly 10KB/s.
    *   Measure CPU utilization, memory footprint, and client rendering latency. Target sub-500ms interaction feedback loops.
*   **5.3 Real-World Functional Testing:**
    *   Test against notoriously heavy targets: news portals (CNN, BBC), modern SPAs (GitHub, Reddit), and form-heavy sites (login flows, search engines).
    *   Verify text input round-trip works on form fields (search boxes, login forms).
    *   Verify session cleanup under load — spin up 50 concurrent sessions and confirm memory remains stable after teardown.
    *   Measure client binary size — target < 3MB stripped release build on stable Rust.

---

### DEPENDENCY ARCHITECTURE MATRIX

| Subsystem | Primary Crate | Feature Flags (Constraint Enforcement) | Implementation Goal |
| :--- | :--- | :--- | :--- |
| **gRPC Transceiver** | `tonic` | `default-features = false`, `tls`, `prost` | Exclude HTTP/1.1 overhead; strictly HTTP/2 binary framing. |
| **Browser Runner** | `chromiumoxide` | `_default_` | Asynchronous DevTools socket interfacing. |
| **Content Extraction** | `readability-rust` | `_default_` | Mozilla-derived content scoring; strip ads and sidebars. |
| **Markdown Conversion** | `html-to-markdown-rs` | `_default_` | Visitor-pattern HTML→Markdown with custom link rewriting. |
| **DOM Diffing** | `mt-dom` | `_default_` | Isolate generic VDOM diff generation without web-sys bloat. |
| **Compression** | `zstd` | `experimental`, `dictionaries` | Dictionary-based persistent-state patch compression. |
| **UI Rendering** | `tuinix` | `_default_` | Direct `libc` bindings; no `ncurses` dependency. |
| **Input Fallback** | `crossterm` | `event-stream` | Mouse tracking (X10/SGR) for non-OSC-8 terminals. |

---

### CRITICAL RISK MITIGATION

1.  **Risk:** gRPC stream saturation from heavy JavaScript single-page applications (SPAs) causing endless DOM mutations.
    *   **Mitigation:** Implement strict debouncing in the `BrowserBackend::session` loop. Cap DOM diff transmissions to a maximum of 2 frames per second (2Hz).
2.  **Risk:** Uncontrollable binary size bloat due to `tonic` and `chromiumoxide` transitive dependencies.
    *   **Mitigation:** Enforce `panic = "abort"` and `strip = true` in the release profile. Use nightly `build-std` as an optional CI optimization for further reduction, but ensure the project builds on stable Rust by default.
3.  **Risk:** Terminal incompatibility with OSC 8 hyperlinking.
    *   **Mitigation:** The `crossterm` mouse coordinate map (Graceful Degradation matrix) must be initialized automatically if the `COLORTERM` or `TERM_PROGRAM` environment variables do not indicate modern emulator support (e.g., Ghostty, WezTerm).
4.  **Risk:** gRPC stream disconnects on high-latency or unstable 10KB/s links.
    *   **Mitigation:** Client implements exponential backoff reconnection with automatic state resync via fresh `NavigateRequest`. Status bar provides persistent connection state visibility to the user.
5.  **Risk:** Server resource leaks from abandoned or idle sessions.
    *   **Mitigation:** Implement a session reaper that drops chromiumoxide browser contexts after 5 minutes of inactivity. Monitor session count and memory usage via server-side metrics.