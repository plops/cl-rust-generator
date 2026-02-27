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

#### **Phase 2: Cloud Browser & Extraction Engine (Weeks 3-5)**
*Objective: Deploy the headless browser, execute semantic sanitization, and output Markdown.*

*   **2.1 CDP Automation Pipeline:**
    *   Integrate `chromiumoxide` to spawn an asynchronous headless Chromium instance.
    *   Configure network interceptors via CDP to block all `.png`, `.jpg`, `.mp4`, `.css`, and `.woff2` requests, simulating a strict text-only environment at the proxy level.
*   **2.2 Readability & Markdown Transcoding:**
    *   Pipe the raw HTML from the Chromium instance into the `readability-rust` engine to score and strip non-semantic DOM trees (ads, sidebars).
    *   Implement `html-to-markdown-rs` utilizing a custom visitor pattern to mutate `<a>` tags.
    *   **Crucial Data Structure:** Implement the `link_map<uint32, string>`. Replace all inline URLs with short numeric IDs (e.g., `[Link Text](1)`) to preserve wire bandwidth.

#### **Phase 3: VDOM Diffing & Transport Optimization (Weeks 6-8)**
*Objective: Enable dynamic page updates using minimal delta payloads.*

*   **3.1 VDOM Synchronization:**
    *   Integrate the `mt-dom` crate on the server to maintain an in-memory Virtual DOM of the current page state.
    *   Establish an event listener on the CDP socket. On DOM mutation, compute the `mt-dom` patch (insertions, deletions, text updates).
*   **3.2 Compression & Debouncing:**
    *   Implement a `tokio::sync::mpsc` channel to aggregate rapid CDP events ("event debouncing") over a 50-100ms window to prevent network saturation.
    *   Initialize a persistent-context `zstd` compression dictionary. Compress the `mt-dom` patch before serializing it into the `DomDelta` protobuf bytes field.

#### **Phase 4: Terminal Client Implementation (Weeks 9-10)**
*Objective: Build the zero-bloat Unix TUI for consuming the gRPC stream.*

*   **4.1 TUI Foundation:**
    *   Initialize `tuinix` directly over `libc` to bypass heavy widget frameworks. Enable raw terminal mode and alternate screen buffer (`smcup`).
*   **4.2 Rendering & Interactivity:**
    *   Write a custom Markdown renderer that maps header/list structures to truecolor ANSI sequences.
    *   **Link Resolution:** Implement the OSC 8 escape sequence (`\x1b]8;;{url}\x1b\\{label}\x1b]8;;\x1b\\`) mapper, resolving numeric IDs from the server's `link_map`.
    *   Integrate `crossterm` event handling as a fallback for mouse tracking (X10/SGR protocols) to capture clicks on non-OSC-8 compliant terminals. Send local X/Y coordinates to a local hit-box map to trigger `ClickRequest` events.

#### **Phase 5: Bandwidth Throttling & System Tuning (Weeks 11-12)**
*Objective: Guarantee operational stability under strict 10KB/s constraints.*

*   **5.1 Priority Queuing System:**
    *   Implement a 4-tier egress queue on the server stream.
    *   *Algorithm:* If buffer utilization > 80% of the 10KB/s quota, aggressively drop Priority 3 (Incremental DOM patches) and prioritize Priority 1 (Status messages) and Priority 2 (Markdown structure).
*   **5.2 End-to-End Simulation:**
    *   Deploy the server to a cloud staging environment.
    *   Use Linux `tc` (Traffic Control) to artificially throttle the network interface to exactly 10KB/s.
    *   Measure CPU utilization, memory footprint, and client rendering latency. Target sub-500ms interaction feedback loops.

---

### DEPENDENCY ARCHITECTURE MATRIX

| Subsystem | Primary Crate | Feature Flags (Constraint Enforcement) | Implementation Goal |
| :--- | :--- | :--- | :--- |
| **gRPC Transceiver** | `tonic` | `default-features = false`, `tls`, `prost` | Exclude HTTP/1.1 overhead; strictly HTTP/2 binary framing. |
| **Browser Runner** | `chromiumoxide` | `_default_` | Asynchronous DevTools socket interfacing. |
| **DOM Diffing** | `mt-dom` | `_default_` | Isolate generic VDOM diff generation without web-sys bloat. |
| **Compression** | `zstd` | `experimental`, `dictionaries` | Dictionary-based persistent-state patch compression. |
| **UI Rendering** | `tuinix` | `_default_` | Direct `libc` bindings; no `ncurses` or `crossterm` UI overlap. |

---

### CRITICAL RISK MITIGATION

1.  **Risk:** gRPC stream saturation from heavy JavaScript single-page applications (SPAs) causing endless DOM mutations.
    *   **Mitigation:** Implement strict debouncing in the `BrowserBackend::session` loop. Cap DOM diff transmissions to a maximum of 2 frames per second (2Hz).
2.  **Risk:** Uncontrollable binary size bloat due to `tonic` and `chromiumoxide` transitive dependencies.
    *   **Mitigation:** Enforce nightly Rust `build-std` compilation in the CI pipeline to strip and recompile the standard library with `panic=abort`.
3.  **Risk:** Terminal incompatibility with OSC 8 hyperlinking.
    *   **Mitigation:** The `crossterm` mouse coordinate map (Graceful Degradation matrix) must be initialized automatically if the `COLORTERM` or `TERM_PROGRAM` environment variables do not indicate modern emulator support (e.g., Ghostty, WezTerm).