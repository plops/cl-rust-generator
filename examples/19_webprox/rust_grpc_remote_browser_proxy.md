# **Engineering High-Efficiency Web Proxy Systems: A Rust-Based gRPC Approach to 10KB/s Browsing**

The evolution of the modern web has created a significant disparity between the resources required to render a typical webpage and the connectivity available in bandwidth-constrained environments. As of the current architectural landscape, the average webpage exceeds several megabytes in size, driven by high-resolution assets, expansive JavaScript frameworks, and pervasive tracking telemetry. For users operating under a strict 10KB/s ceiling, traditional browser architectures are fundamentally non-viable. To bridge this gap, a radical decoupling is required: a cloud-based server must act as the primary engine for browser automation and content reduction, while a minimalist terminal client serves as the interface for consuming a heavily distilled, semantically rich data stream. This report examines the technical implementation of such a system, leveraging the Rust programming language, the gRPC framework, and advanced content transformation techniques to deliver a functional browsing experience within extreme throughput constraints.

## **The Architecture of Semantic Transcoding**

The core challenge of browsing at 10KB/s is not merely the reduction of data but the intelligent selection of what data to preserve. Traditional proxies that simply compress images or minify HTML fail to address the fundamental "bloat" of modern DOM structures and client-side scripts. The proposed solution adopts a cloud-proxy-client model. The cloud server hosts a full-featured headless browser, executing all scripts and rendering the page in a high-bandwidth environment. It then transcodes the resulting DOM into a compact, structured format—primarily Markdown—which is subsequently compressed and streamed to the client via gRPC. This approach shifts the computational and bandwidth burden to the cloud, allowing the client to operate with minimal resources.

A critical component of this architecture is the use of the Chrome DevTools Protocol (CDP) to drive browser automation. Unlike high-level testing frameworks that introduce significant overhead, direct CDP interaction allows for fine-grained control over browser events and network interception. By utilizing the chromiumoxide crate, which provides an asynchronous, type-safe interface to CDP, the server can efficiently manage multiple concurrent browsing sessions. This efficiency is paramount for maintaining a low-latency feedback loop between user input on the terminal client and the resulting state change in the cloud-hosted browser.

## **Minimalist Dependency Management in Rust**

Adhering to the requirement for absolute minimum dependencies is a challenging task in the modern Rust ecosystem, where transitive dependencies can quickly bloat the binary and increase the attack surface. The philosophy of "ultra-fine-grained symbols" suggests that every function and type should ideally declare its specific needs to minimize waste.1 However, in practice, the most effective strategy is the rigorous selection of "shallow" crates and the strategic use of feature gates.

When configuring the project, it is essential to scrutinize the dependency tree using tools like cargo tree. The goal is to avoid duplicate crates and minimize the inclusion of unused functionality. For instance, the tokio runtime, while necessary for async operations, should be included only with the specific features required for networking and I/O, rather than the "full" feature set.2 Similarly, the tonic gRPC implementation should be tuned to exclude unneeded transport or codegen features when possible.

### **Dependency Footprint and Versioning Strategy**

| Component | Library Choice | Primary Justification | Footprint Mitigation |
| :---- | :---- | :---- | :---- |
| **Networking** | tonic | Industry-standard gRPC over HTTP/2 2 | Disable default features, use prost for binary serialization. |
| **Automation** | chromiumoxide | Async CDP integration with generated types 4 | Use headless mode and disable image loading in the browser. |
| **TUI** | tuinix | Lightweight Unix TUI with minimal dependencies 6 | Direct libc usage, avoids heavy widget frameworks. |
| **Compression** | zstd | Superior ratio/speed balance for streaming 7 | Use dictionary-based compression for small DOM patches. |
| **Extraction** | readability-rust | Mozilla-derived content scoring algorithm 9 | Strip all non-semantic elements before conversion. |

A common pitfall in Rust dependency management is the accumulation of multiple versions of the same crate due to conflicting requirements from upstream libraries. For example, if one library depends on indexmap version 1.0 and another on version 2.0, Cargo will include both, increasing the binary size.11 The strategy of choosing the lowest possible version for direct dependencies often fails because Cargo prioritizes major version compatibility. Instead, a more robust approach is to push older dependencies forward or hold back newer ones to align versions across the tree.11 Furthermore, utilizing opt-level \= "z" and lto \= true in the Cargo profile can significantly reduce the final binary size by optimizing for space and enabling link-time optimizations that prune unused code.12

## **The gRPC Transport Layer**

gRPC is uniquely suited for low-bandwidth browsing because it utilizes HTTP/2 for transport and Protocol Buffers (protobuf) for serialization. HTTP/2’s binary framing and header compression (HPACK) drastically reduce protocol overhead compared to the textual headers of HTTP/1.1. At 10KB/s, the efficiency of the wire format is a decisive factor in system responsiveness.

The system uses a bidirectional streaming RPC to maintain a persistent connection between the client and the server. This allows the client to send navigation and interaction events (clicks, scrolls, keystrokes) while the server asynchronously pushes content updates. Protobuf's use of integer field numbers instead of string keys in its binary encoding ensures that the payload remains as dense as possible.2

### **Protocol Buffer Service Definition**

The following service definition outlines the primary communication interface. The use of a oneof field in the PageUpdate message allows the server to send either a full page render or a incremental DOM patch, depending on the nature of the change.

Protocol Buffers

syntax \= "proto3";  
package browser;

service BrowsingService {  
  // Bidirectional stream for interactive browsing  
  rpc Session(stream Interaction) returns (stream PageUpdate);  
}

message Interaction {  
  oneof type {  
    NavigateRequest navigate \= 1;  
    ClickRequest click \= 2;  
    InputRequest input \= 3;  
  }  
}

message PageUpdate {  
  oneof content {  
    PreRenderedPage full\_page \= 1;  
    DomDelta patch \= 2;  
  }  
}

message PreRenderedPage {  
  string markdown \= 1;  
  map\<uint32, string\> link\_map \= 2; // Maps numeric IDs to URLs  
}

message DomDelta {  
  bytes compressed\_patch \= 1; // Zstd-compressed mt-dom patches  
}

In this schema, the link\_map is a critical optimization. Rather than sending full URLs within the Markdown text, the server replaces links with short numeric IDs. The client then resolves these IDs locally, significantly reducing the amount of redundant string data sent over the wire. This is particularly effective for pages with numerous long URLs, such as those found on news aggregators or scientific databases.

## **Cloud Browser Automation via Chromiumoxide**

The server's primary responsibility is to maintain a high-fidelity representation of the web while exposing a low-fidelity interface to the client. This is achieved by running a headless instance of Chromium in the cloud. The chromiumoxide crate allows the server to attach to the browser's DevTools socket and receive a stream of events representing DOM changes, network activity, and page lifecycle transitions.4

To minimize resource usage on the server and bandwidth to the web, the browser is configured to block images, media, and fonts. This ensures that the cloud instance only fetches the HTML, CSS, and JavaScript necessary to reconstruct the semantic structure of the page. Once the page has loaded, the server applies a content extraction pipeline.

### **Content Extraction and Markdown Conversion**

The transformation of raw HTML into heavily compressed pre-rendered data involves several stages:

1. **Readability Assessment**: The server uses the readability-rust crate to identify the primary content of the page.9 This algorithm scores elements based on text density and link density, effectively stripping away advertisements, sidebars, and navigation menus that occupy precious screen real estate and bandwidth.15  
2. **Semantic Sanitization**: The resulting "clean" HTML is further processed to remove any remaining non-semantic tags.  
3. **Markdown Rendering**: The html-to-markdown-rs crate converts the sanitized HTML into CommonMark-compliant Markdown.17 Markdown is an ideal format for this application because it is natively textual, highly legible, and preserves the structural hierarchy of the document (headers, lists, and links).19

The performance of html-to-markdown-rs is noteworthy, achieving throughputs between 150 and 280 MB/s, which ensures that the transformation process does not become a bottleneck on the server.17 By using a visitor pattern, the converter can be customized to rewrite URLs and inject the numeric link IDs required by the gRPC protocol.17

## **Streaming Incremental DOM Changes**

A static render is insufficient for modern web applications that update content dynamically. To achieve the requirement for streaming small changes to the DOM, the system employs a virtual DOM (VDOM) diffing strategy. The server maintains a VDOM of the current page state using the mt-dom crate, which provides a generic VDOM implementation capable of producing portable patches.22

When the cloud browser reports a DOM change via CDP, the server updates its VDOM and calculates the diff between the old and new states. The resulting patches describe the minimal set of operations (insertions, deletions, or text updates) needed to synchronize the client's view.24

### **Performance Characteristics of mt-dom Diffing**

| Operation Type | Complexity | Bandwidth Impact | Data Transmitted |
| :---- | :---- | :---- | :---- |
| **Node Insertion** | **![][image1]** | Low to Medium | Full serialized node \+ path |
| **Text Update** | **![][image2]** | Minimal | New text string \+ path |
| **Node Removal** | **![][image2]** | Minimal | Target path ID |
| **Attribute Change** | **![][image2]** | Minimal | Key-value pair \+ path |

The patches are serialized using Protocol Buffers and then compressed using Zstandard (zstd). Zstd is particularly effective here because the server can maintain a persistent compression context, allowing it to "remember" the symbols and patterns of the previous state.7 This dictionary-based approach can reduce the size of incremental patches by up to 80%, ensuring that even complex dynamic updates fit within the 10KB/s budget.27

## **Client-Side TUI and Interaction**

The client is a lightweight Terminal User Interface (TUI) application designed for Unix systems. To minimize dependencies, it is built directly on the tuinix library, which relies only on libc for terminal state management and I/O.6 The client enters raw mode to capture individual keystrokes and mouse events without waiting for a newline, providing the responsiveness expected of a modern browser.

### **Implementation of Clickable Links in the Terminal**

Making links clickable in a text interface is achieved through two complementary methods. The primary method is the use of the OSC 8 escape sequence, which is supported by modern terminal emulators like Ghostty, iTerm2, and WezTerm.28 This sequence allows the client to embed a URL directly into a string of text, which the terminal then renders as a clickable hyperlink.

Rust

// Conceptual OSC 8 Hyperlink Generation  
fn format\_link(label: &str, url: &str) \-\> String {  
    format\!("\\x1b\]8;;{}\\x1b\\\\{}\\x1b\]8;;\\x1b\\\\", url, label)  
}

For terminals that do not support OSC 8, the client implements mouse tracking using crossterm's event handling.31 When mouse capture is enabled, the client receives ![][image3] coordinates for every click. By maintaining a mapping of screen regions to link IDs, the client can determine when a link has been selected and send a ClickRequest back to the server.

### **Terminal Compatibility and Graceful Degradation**

| Terminal Feature | Support Level | Implementation Method |
| :---- | :---- | :---- |
| **OSC 8 Links** | Emerging | ANSI escape sequences 28 |
| **Mouse Tracking** | High | X10 / SGR mouse protocols 31 |
| **24-bit Color** | High | Truecolor ANSI sequences 6 |
| **Alternate Screen** | High | smcup / rmcup escape codes 6 |

The client also handles window resize events dynamically. When the terminal is resized, the client sends a BrowserStatus update to the server containing the new dimensions. The cloud-hosted browser then reflows the page to match the client's aspect ratio, ensuring that the pre-rendered Markdown remains legible and correctly formatted.

## **Integrated Implementation: Server and Client Logic**

The following section provides a detailed implementation guide for the core components of the system. This code emphasizes the use of asynchronous Rust to handle I/O and browser events concurrently.

### **Server Implementation**

The server implements the BrowsingService trait. It uses a Mutex to manage access to the chromiumoxide page object, as gRPC service methods take an immutable reference to self.14

Rust

use tonic::{transport::Server, Request, Response, Status};  
use chromiumoxide::browser::{Browser, BrowserConfig};  
use tokio::sync::mpsc;  
use tokio\_stream::wrappers::ReceiverStream;  
use std::sync::Arc;  
use tokio::sync::Mutex;

pub mod browser\_proto {  
    tonic::include\_proto\!("browser");  
}

pub struct BrowserBackend {  
    browser: Browser,  
    // Store active pages indexed by session ID  
    sessions: Arc\<Mutex\<HashMap\<String, Page\>\>\>,  
}

\#\[tonic::async\_trait\]  
impl BrowsingService for BrowserBackend {  
    type SessionStream \= ReceiverStream\<Result\<PageUpdate, Status\>\>;

    async fn session(  
        &self,  
        request: Request\<Streaming\<Interaction\>\>,  
    ) \-\> Result\<Response\<Self::SessionStream\>, Status\> {  
        let mut in\_stream \= request.into\_inner();  
        let (tx, rx) \= mpsc::channel(128);

        tokio::spawn(async move {  
            while let Some(interaction) \= in\_stream.next().await {  
                match interaction {  
                    Ok(Interaction { type: Some(navigate) }) \=\> {  
                        // 1\. Instruct chromiumoxide to navigate  
                        // 2\. Wait for LoadEvent  
                        // 3\. Extract content with Readability  
                        // 4\. Convert to Markdown  
                        // 5\. Send FullPage update via tx  
                    }  
                    Ok(Interaction { type: Some(click) }) \=\> {  
                        // Dispatch mouse event to CDP  
                    }  
                    \_ \=\> {}  
                }  
            }  
        });

        Ok(Response::new(ReceiverStream::new(rx)))  
    }  
}

The Interaction processing loop is the heart of the server. It must reconcile the high-speed event stream from the browser with the 10KB/s constraint of the gRPC pipe. This is achieved through "event debouncing"—if the browser produces ten DOM changes in a millisecond, the server aggregates them into a single DomDelta patch to avoid saturating the connection.

### **Client Implementation**

The client uses tuinix to manage the terminal and a tonic client to consume the PageUpdate stream.

Rust

use tuinix::terminal::Terminal;  
use browser\_proto::browsing\_service\_client::BrowsingServiceClient;

async fn run\_client() \-\> Result\<(), Box\<dyn std::error::Error\>\> {  
    let mut client \= BrowsingServiceClient::connect("http://\[::1\]:50051").await?;  
    let (tx, mut rx) \= mpsc::channel(32);  
    let stream \= client.session(ReceiverStream::new(rx)).await?.into\_inner();

    let mut terminal \= Terminal::new()?;  
      
    tokio::spawn(async move {  
        while let Some(update) \= stream.next().await {  
            match update {  
                Ok(PageUpdate { content: Some(full\_page) }) \=\> {  
                    // Parse Markdown and render to terminal using ANSI styles  
                    terminal.draw(|frame| {  
                        render\_markdown(frame, \&full\_page.markdown);  
                    });  
                }  
                Ok(PageUpdate { content: Some(patch) }) \=\> {  
                    // Apply mt-dom patch to local VDOM and re-render  
                }  
                \_ \=\> {}  
            }  
        }  
    });

    // Main event loop for keyboard/mouse input  
    loop {  
        if let Some(event) \= terminal.poll\_event(Duration::from\_millis(10))? {  
            // Map input to Interaction and send via tx  
        }  
    }  
}

The render\_markdown function utilizes the structural information provided by the Markdown format to apply appropriate terminal styling. Headers are rendered in bold, and list items are indented. Importantly, the numeric link IDs are embedded as OSC 8 sequences, allowing for a seamless browsing experience despite the textual medium.28

## **Performance Analysis and Optimization**

To ensure the system remains functional at 10KB/s, several lower-level optimizations are applied. These focus on reducing the binary size and optimizing the runtime behavior of the Rust application.

### **Binary Size Reduction**

Rust binaries can be notoriously large due to the inclusion of debug symbols and the standard library's panic handling. For a tool where "absolute minimum dependencies" is a requirement, the following configurations in Cargo.toml are mandatory:

Ini, TOML

\[profile.release\]  
opt-level \= "z"     \# Optimize for size  
lto \= true          \# Enable Link Time Optimization  
codegen-units \= 1   \# Maximize LTO effectiveness  
panic \= "abort"     \# Remove stack unwinding code  
strip \= true        \# Automatically strip symbols from binary

Stripping symbols alone can reduce a 4.5 MiB binary to approximately 415 KiB, a 90% reduction.12 Further reductions can be achieved by using build-std to recompile the standard library with size optimizations, although this currently requires the nightly Rust toolchain.13

### **Bandwidth Budgeting and Congestion Control**

At 10KB/s, the system must prioritize essential data. The server implements a "priority queue" for updates:

* **Priority 1**: Browser status messages (e.g., "Loading...").  
* **Priority 2**: Textual content and structure (Markdown).  
* **Priority 3**: Incremental DOM patches.  
* **Priority 4**: Metadata and auxiliary link mappings.

If the gRPC stream buffer exceeds a certain threshold, the server begins dropping Priority 3 updates, favoring a slightly out-of-date but structurally sound view over a lagging and unresponsive one. This is crucial for maintaining interactivity on high-latency links.

## **Conclusion: The Viability of Constrained Browsing**

The implementation of a gRPC-based cloud proxy for 10KB/s browsing demonstrates the efficacy of semantic content reduction and efficient binary transport. By shifting the complexity of the modern web into the cloud and delivering a distilled, structured stream to a minimalist terminal client, the system overcomes the fundamental limitations of traditional browser architectures. The combination of chromiumoxide for automation, readability-rust for noise reduction, and zstd for persistent-context compression provides a robust foundation for browsing in extreme environments. As web content continues to expand in complexity, the paradigm of semantic transcoding will remain a vital strategy for ensuring global information accessibility regardless of infrastructure constraints.

#### **Works cited**

1. Rust's dependencies are starting to worry me \- Hacker News, accessed February 27, 2026, [https://news.ycombinator.com/item?id=43935067](https://news.ycombinator.com/item?id=43935067)  
2. gRPC Basics for Rust Developers \- DockYard, accessed February 27, 2026, [https://dockyard.com/blog/2025/04/08/grpc-basics-for-rust-developers](https://dockyard.com/blog/2025/04/08/grpc-basics-for-rust-developers)  
3. hyperium/tonic: A native gRPC client & server implementation with async/await support. \- GitHub, accessed February 27, 2026, [https://github.com/hyperium/tonic](https://github.com/hyperium/tonic)  
4. What is the difference between this and the headless\_chrome \[0\] crate? \[0\]: http... | Hacker News, accessed February 27, 2026, [https://news.ycombinator.com/item?id=25418154](https://news.ycombinator.com/item?id=25418154)  
5. Awesome-Web-Scraping/rust.md at main \- GitHub, accessed February 27, 2026, [https://github.com/luminati-io/Awesome-Web-Scraping/blob/main/rust.md](https://github.com/luminati-io/Awesome-Web-Scraping/blob/main/rust.md)  
6. sile/tuinix: A Rust library for building terminal user interface ... \- GitHub, accessed February 27, 2026, [https://github.com/sile/tuinix](https://github.com/sile/tuinix)  
7. Compress Files with zstd in Rust | Compression Algorithms in Programming \- SSOJet, accessed February 27, 2026, [https://ssojet.com/compression/compress-files-with-zstd-in-rust](https://ssojet.com/compression/compress-files-with-zstd-in-rust)  
8. Smaller and faster data compression with Zstandard \- Engineering at Meta \- Facebook, accessed February 27, 2026, [https://engineering.fb.com/2016/08/31/core-infra/smaller-and-faster-data-compression-with-zstandard/](https://engineering.fb.com/2016/08/31/core-infra/smaller-and-faster-data-compression-with-zstandard/)  
9. readability-rust \- crates.io: Rust Package Registry, accessed February 27, 2026, [https://crates.io/crates/readability-rust](https://crates.io/crates/readability-rust)  
10. readability\_rust \- Rust \- Docs.rs, accessed February 27, 2026, [https://docs.rs/readability-rust](https://docs.rs/readability-rust)  
11. Strategies for minimizing dependency tree? : r/rust \- Reddit, accessed February 27, 2026, [https://www.reddit.com/r/rust/comments/191fyrp/strategies\_for\_minimizing\_dependency\_tree/](https://www.reddit.com/r/rust/comments/191fyrp/strategies_for_minimizing_dependency_tree/)  
12. Making Rust binaries smaller by default | Kobzol's blog, accessed February 27, 2026, [https://kobzol.github.io/rust/cargo/2024/01/23/making-rust-binaries-smaller-by-default.html](https://kobzol.github.io/rust/cargo/2024/01/23/making-rust-binaries-smaller-by-default.html)  
13. A very small Rust binary indeed \- Reddit, accessed February 27, 2026, [https://www.reddit.com/r/rust/comments/1augwm0/a\_very\_small\_rust\_binary\_indeed/](https://www.reddit.com/r/rust/comments/1augwm0/a_very_small_rust_binary_indeed/)  
14. How to Setup gRPC Server and Client in Rust | by Doğukan Akkaya \- Medium, accessed February 27, 2026, [https://medium.com/@dogukanakkaya/how-to-create-grpc-server-client-in-rust-4e37692229f0](https://medium.com/@dogukanakkaya/how-to-create-grpc-server-client-in-rust-4e37692229f0)  
15. theiskaa/readabilityrs: A Rust port of Mozilla's standalone readability library \- GitHub, accessed February 27, 2026, [https://github.com/theiskaa/readabilityrs](https://github.com/theiskaa/readabilityrs)  
16. niklak/dom\_smoothie: A Rust crate for extracting readable content from web pages. \- GitHub, accessed February 27, 2026, [https://github.com/niklak/dom\_smoothie](https://github.com/niklak/dom_smoothie)  
17. kreuzberg-dev/html-to-markdown: High performance and ... \- GitHub, accessed February 27, 2026, [https://github.com/kreuzberg-dev/html-to-markdown](https://github.com/kreuzberg-dev/html-to-markdown)  
18. html-to-markdown-rs \- crates.io: Rust Package Registry, accessed February 27, 2026, [https://crates.io/crates/html-to-markdown-rs](https://crates.io/crates/html-to-markdown-rs)  
19. Announcing html-to-markdown V2: Rust engine and CLI with Python, Node and WASM bindings \- Reddit, accessed February 27, 2026, [https://www.reddit.com/r/rust/comments/1o5q7k4/announcing\_htmltomarkdown\_v2\_rust\_engine\_and\_cli/](https://www.reddit.com/r/rust/comments/1o5q7k4/announcing_htmltomarkdown_v2_rust_engine_and_cli/)  
20. I built a Rust-based HTML-to-Markdown converter to save RAG tokens (Self-Hosted / API), accessed February 27, 2026, [https://www.reddit.com/r/LocalLLaMA/comments/1ps482o/i\_built\_a\_rustbased\_htmltomarkdown\_converter\_to/](https://www.reddit.com/r/LocalLLaMA/comments/1ps482o/i_built_a_rustbased_htmltomarkdown_converter_to/)  
21. Safe, Fast, and Scalable: Why gRPC-Rust Should Be Your Next RPC Framework \- Reddit, accessed February 27, 2026, [https://www.reddit.com/r/rust/comments/1qxj0j5/safe\_fast\_and\_scalable\_why\_grpcrust\_should\_be/](https://www.reddit.com/r/rust/comments/1qxj0j5/safe_fast_and_scalable_why_grpcrust_should_be/)  
22. mt\_dom \- Rust \- Docs.rs, accessed February 27, 2026, [https://docs.rs/mt-dom/latest/mt\_dom/](https://docs.rs/mt-dom/latest/mt_dom/)  
23. mt\_dom \- Rust \- Docs.rs, accessed February 27, 2026, [https://docs.rs/mt-dom](https://docs.rs/mt-dom)  
24. Diff-dom-streaming (forked) \- StackBlitz, accessed February 27, 2026, [https://stackblitz.com/edit/diff-dom-streaming-5tdx5w](https://stackblitz.com/edit/diff-dom-streaming-5tdx5w)  
25. patrick-steele-idem/morphdom: Fast and lightweight DOM diffing/patching (no virtual DOM needed) \- GitHub, accessed February 27, 2026, [https://github.com/patrick-steele-idem/morphdom](https://github.com/patrick-steele-idem/morphdom)  
26. DOM diff algorithm with HTML streaming? \- javascript \- Stack Overflow, accessed February 27, 2026, [https://stackoverflow.com/questions/78022306/dom-diff-algorithm-with-html-streaming](https://stackoverflow.com/questions/78022306/dom-diff-algorithm-with-html-streaming)  
27. Compressor in zstd::bulk \- Rust \- Apache Avro, accessed February 27, 2026, [https://avro.apache.org/docs/++version++/api/rust/zstd/bulk/struct.Compressor.html](https://avro.apache.org/docs/++version++/api/rust/zstd/bulk/struct.Compressor.html)  
28. konradko/add-osc-8-hyperlink-rs \- GitHub, accessed February 27, 2026, [https://github.com/konradko/add-osc-8-hyperlink-rs](https://github.com/konradko/add-osc-8-hyperlink-rs)  
29. support for OSC 8 hyperlinks · Issue \#9 · Dicklesworthstone/frankentui \- GitHub, accessed February 27, 2026, [https://github.com/Dicklesworthstone/frankentui/issues/9](https://github.com/Dicklesworthstone/frankentui/issues/9)  
30. osc8 \- Rust \- Docs.rs, accessed February 27, 2026, [https://docs.rs/osc8](https://docs.rs/osc8)  
31. crossterm \- Rust \- Docs.rs, accessed February 27, 2026, [https://docs.rs/crossterm/](https://docs.rs/crossterm/)  
32. crossterm-rs/crossterm: Cross platform terminal library rust \- GitHub, accessed February 27, 2026, [https://github.com/crossterm-rs/crossterm](https://github.com/crossterm-rs/crossterm)  
33. r3bl\_tui — CLI for Rust // Lib.rs, accessed February 27, 2026, [https://lib.rs/crates/r3bl\_tui](https://lib.rs/crates/r3bl_tui)  
34. Preferred library for grpc in rust \- help \- The Rust Programming Language Forum, accessed February 27, 2026, [https://users.rust-lang.org/t/preferred-library-for-grpc-in-rust/37801](https://users.rust-lang.org/t/preferred-library-for-grpc-in-rust/37801)

[image1]: <data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACoAAAAVCAYAAAAw73wjAAACX0lEQVR4Xu2WS6hNURjH/955T66QDMyUpLxGNwMhyWtEoSRKIQo3YnCVSAYMECMTj1LyKBOPSGSCKFFETDwyEYUo8f+fb+/OOv+9z3E5R52BX/26d3/f+s5ae621197Af9qbfvQsHeuJJuikRz3YLEfoYg+2gN0eaIZl9JwHW0RfD+QMozvoaXqRXqe36TbaJ2mX05u+pnM9kTGEXqEP6E/6hY6oaRGz9hGR/0Qv1aaLrKDP6BbEAHI66H16jQ5I4mIhfU57WdzZSV8hBrO1NlVhKuJmhnrC2U/f0vGeyJiJ6KTb4ifoIYuVcYPOoD/oGxRveCndZbECqxGDmOOJBP2wOrlr8ad0g8WcwYgVEacQfa2ppiscRtxIXTSDn+kdTxjqTB28T2LDs9iiJFbGAnog+38youZhNV3hERo8OELLpsK1njB0t2p3K4lNymINZwIxyHnJ9WVE3azsWpN1vpouoqf4HaJonOWcfYh26X6clsUmJrEy9JDo6c/RCaG6C9m1ts7GarrIIESBbDTtoxDb4ztqB5XPqP7WYwy96kFyD1E7BXEGT6hNF9HeUMFATyRoo6vNdovrdfm7pV+JYp1YjqjVIJ9YrpQ9iIJ0D6WsQ+SPeQLVB2y+JxJ0fE33IGIFdf6q/rjlStHyv6CPEUuc05/upd/oZtQ/0F/STR7M0IzrlPA3UY72pQaqF02PGEkP0puI16Zm4QztoqOTdmVoNk5aTG8X/dZXxEA+oPxmtCK6kXSC/hmrEJ3pM6+t0UeMjrglnmhH1qMHXzx/SaNj84/Rl5aOmdmeaAHdvwALa3djCD0E0AAAAABJRU5ErkJggg==>

[image2]: <data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACMAAAAVCAYAAADM+lfpAAAB8UlEQVR4Xu2WPUgdQRSFr0YjUVQigWhhYSHYiDZiI0ENCUGCWlmIjVgIKghKUGxSRcTGQgWLEIQXAoEQIqYRRRDFRqxEQVGsxJ8i8QeLCKLncnfJ7HHfyz5eSBo/+Hhw7jB7mdk3syL3/Bsy4RdYzIUEPIbfYB4XUmUSNnPoUMKBx3M4x2EqtMKvHIJcWAvfw5/BUoAPHPjokg3BT3AGLsIVOAAfOON80uEBfEV5JTwWm2cLngbLAao5UNrgDuwXe4jPE7gOF2CWkyuNcBemUe4yK4mbucMoPIRlXPCohzfwLeUxOE4Zk1QzHWIPeskFB12Ra7hG+TbsoYyJ3IyuxCVc5QKRI9bwiZPle1mTk4URuRldYp2wkwvEM7Fxy05W4WVaS0SkZvTfcSQ2YbxzwGdEbJz7flR5WbmThaHNnHHIZItNpmZQzaVQbCuvJPhgf2X0NxGRmlE2xCZ8xAWHCbExg5Tr0R91m845DOOd2IQNXPDoEqtPcUF+v9SvuUBoMxcchqFbtQc3xbbD5yEchr9gn8Q/1PZhL4fEvNg2a/N/5Ckcg0tiV4AeZJ/hG1jkjAtD75WPHIICsfn0TvLfyx9ijdU44/4q7WJnj35C/Hf0YtXjoYULSVDKQSp0w+8cJsE0B6mgN7x+z7zgQgTqYOwWrytkk9ak5K4AAAAASUVORK5CYII=>

[image3]: <data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAC0AAAAYCAYAAABurXSEAAACkUlEQVR4Xu2WTYiNURjHH2J8Dgv5SNEUFjKiRPksihQxKwsfC9TIECsL5CMskFIkFlLCwvdHRIkys2JDUogoErFgTRP/v+cc98x/zh33UvPOYn71697zPOe977nnPec5r1k3xdEbXoKjNNGJzILHNdgRx2CDBgtgL9yuwRzL4RUNFkQv+ALO0ERKT/gBLtREgWyB1zWYsgS+hj00USBj4E9Yr4nIGXhUg12At3C3BiMv4UYNdgFOw2saJIPNH8NSTQSmw7vwIbwHp8FT8BzcmvSrBPa/Ci/CoUl8tvnE9U1ihBXkqcR+M8l80HM0ASaaL52Bod0Mv5pfs9/8upj7G2vMN1et+XVNSe4EfJO0I3z6HzVIppr/CAeoHIGjw3du0nfwQmgfhI3heyVwdvvB1eb3S8sZ1y6XgrIK/jCvbm2IM83Pjphg3o83/R9a4POkPc78d9cnscgK81y7QfPILrc8Uvij7Fcn8WoYbv4bO5IYl0m5SWPumwbJAPOLFmsCrIR7wvcb8FWSq4OHkzZZYPllFmGe95qZxPiu88Uys2m+cZ9oMMI1tVlifWArPG8+kO/mj5bwmD0Lp4Q2GW8+oE/mL145Yp95oc2XI96D6z3HSfMqlYUljINQDsA78BmcC28HD1n7GR1mPmP8c2VPMbDW/Hh+AO+b/4lNbXqUYLljxcnCzfXZys9QNbDicHPl4EzXJG3WYQ6aR7YyyLxyjNBEhB34WJdp4h+4pYHAIvMBbgvt/vC9lUqosg5e1qCyAd7UYJXwxWunBgM8XDgxk0ObB8pjOORPjxLcM1waYzWhcPfyfXq+Jqpgn/kNc/CIZrXhu8Qj88OJTzjHLvM/WRE8sbiTR2qiE2FFYWHoplB+AU/zd9OlUYemAAAAAElFTkSuQmCC>