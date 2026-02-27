# **Implementation Plan: Low-Bandwidth Remote Browser Isolation (RBI)**

## **Project Overview**
**Objective:** Build a minimalist, terminal-based web browser where a Rust/Playwright server renders pages, prunes the DOM semantically, and streams highly compressed (`Zstd`) differential updates to a lightweight, Tokio-free Rust terminal client via gRPC. 

**Core Stack:** Rust, gRPC (`grpcio` or custom `h2`), Protobuf (`prost`), Playwright, Ratatui, Zstandard.

---

## **Phase 1: Repository Structure & Base Configuration (Weeks 1-2)**
*Goal: Establish a Cargo Workspace and configure dependencies adhering to the "Minimalist Runtime" philosophy.*

### 1.1 Workspace Setup
Create a Cargo workspace containing three distinct crates:
*   `proto-lib`: Shared library containing the gRPC/Protobuf definitions and generated Rust code.
*   `rbi-server`: The heavyweight remote browser engine and gRPC server.
*   `rbi-client`: The highly optimized, minimal-dependency TUI client.

### 1.2 Dependency Management (Strict "No-Bloat" Rules)
*   **`proto-lib`:** Use `prost` and `prost-build` (avoids the heavier `protobuf-rust`).
*   **gRPC Layer:** Use `grpcio` (C-core wrapper) or a minimal `smol`-based gRPC implementation to avoid pulling in the multithreaded Tokio executor.
*   **`rbi-client`:** Include `ratatui` and `crossterm`, but strictly configure `default-features = false`. Include `zstd-safe` for bare-metal compression.
*   **`rbi-server`:** Include `playwright-rs` (or a bridge to a Node.js Playwright runner if Rust bindings lack specific CDP features), `zstd-safe`, and an optimized allocator like `mimalloc`.

### 1.3 Compiler Optimizations
Configure the workspace `Cargo.toml` for aggressive binary size and performance optimizations:
```toml
[profile.release]
lto = true
codegen-units = 1
opt-level = "z"
strip = true
```

---

## **Phase 2: Protocol Definition & Optimization (Week 2)**
*Goal: Implement and optimize the data transport layer to ensure maximum semantic density.*

### 2.1 Protobuf Implementation
*   Implement the provided `.proto` schema (`BrowserService`, `NavigationRequest`, `PageUpdate`, etc.).
*   Ensure the use of `oneof` for `update` (Snapshot vs. Mutation) and `uint32` for DOM Node IDs to minimize byte-size.

### 2.2 Zstandard Pipeline Configuration
*   Implement compression middleware on both Server and Client.
*   **Optimization:** Train a custom `Zstd` dictionary using a dataset of common structural HTML/text patterns. Distribute this dictionary file to both the client and server to push compression ratios beyond 150:1.

---

## **Phase 3: Server-Side Engine Development (Weeks 3-5)**
*Goal: Build the DOM-pruning, Playwright-driven rendering engine and gRPC server.*

### 3.1 Playwright Initialization & Network Interception
*   Initialize headless browser instances with multi-context support.
*   Configure **Network Interception** (`page.route("**/*")`) to aggressively block all images, CSS, fonts, videos, and analytics scripts at the browser level.

### 3.2 The Semantic Pruning Engine (DOM Downsampling)
*   Write an injection script to linearize the DOM. 
*   **Rules:** Strip all decorative elements (`<div>`, `<span>` wrappers). Retain and assign `uint32` IDs to `<a>`, `<button>`, `<input>`, and text nodes.
*   Generate the initial `Snapshot` gRPC message from this pruned tree.

### 3.3 Differential Streaming (`MutationObserver`)
*   Inject JavaScript via `addInitScript` to attach a `MutationObserver` to `document.documentElement`.
*   Watch for `childList`, `attributes`, and `characterData`.
*   Expose a Rust binding to the browser (`expose_binding`). When mutations occur, format them into the `Mutation` proto message.
*   **Rate Limiting:** Implement a queue/throttle. If mutation throughput exceeds ~8KB/s, pause streaming and wait for the DOM to settle, then send a fresh Snapshot.

### 3.4 Server gRPC Methods
*   Implement the `OpenPage` server-streaming RPC.
*   Implement the `SendAction` RPC. When an action is received (e.g., `NodeID: 42, Click`), map it to the corresponding Playwright element via CDP and execute `element.click()`.

---

## **Phase 4: Client-Side Terminal Interface (Weeks 6-7)**
*Goal: Build a dumb-terminal client that transforms gRPC state into a readable UI.*

### 4.1 State Management (Virtual DOM)
*   Create a local state struct representing the linearized DOM tree.
*   Implement update logic: Initial `Snapshot` replaces the tree; incoming `Mutation` messages append, delete, or modify specific nodes by ID.

### 4.2 Immediate-Mode Rendering (`ratatui`)
*   Implement a linear "Reader View" layout engine.
*   Render text contiguously. Style interactive nodes explicitly (e.g., underline links, wrap buttons in `[ ]`).
*   **Hit-Mapping:** During the render pass loop, calculate the terminal screen `Rect` (X, Y, Width, Height) for every interactive element and store it in a volatile frame state.

### 4.3 Input Handling & Local Echo
*   Capture `MouseEvent` and keyboard events via `crossterm`.
*   On mouse click, intersect the (X, Y) coordinates with the frame's Hit-Map. 
*   If a match is found, fire the `SendAction` gRPC request.
*   **Latency Mitigation (Local Echo):** Instantly change the styling of the clicked element locally (e.g., change text to yellow or append "[Loading...]") so the user isn't paralyzed while the 10KB/s round-trip completes.

---

## **Phase 5: Testing, Simulation & Refinement (Week 8)**
*Goal: Validate the 10KB/s constraint and ensure stability.*

### 5.1 Bandwidth Constraint Simulation
*   Use Linux `tc` (Traffic Control) or `NetEm` to artificially restrict the server interface to a strict 10KB/s limit and introduce 300ms latency.
*   Monitor protocol timeouts and adjust gRPC keep-alive and chunking settings.

### 5.2 Real-World Benchmarking
*   Test against notoriously "heavy" websites (e.g., news portals, modern SPAs).
*   Verify that memory usage remains stable on the server when handling multiple browser contexts.
*   Verify client binary size remains strictly minimal (target: < 3MB).

---

## **Resource Allocation & Milestones**

| Milestone | Deliverable | Est. Timeline |
| :--- | :--- | :--- |
| **M1: Architecture** | Workspace setup, pure-proto payload definitions, gRPC layer tests. | End of Week 2 |
| **M2: Heavy Lifting** | Playwright server running, intercepting network, pruning DOM into JSON/Proto. | End of Week 4 |
| **M3: Streaming** | MutationObserver injected, continuous differential streaming working. | End of Week 5 |
| **M4: The Client** | Ratatui rendering VDOM, hit-map working, clicks successfully reaching server. | End of Week 7 |
| **M5: Optimization** | Zstd dictionary training, strict 10KB/s testing, binary size shrinking. | End of Week 8 |