**Protocol & Communication:**
- [proto-def/proto/browser_stream.proto](cci:7://file:///home/kiel/stage/cl-rust-generator/examples/20_webprox_avif/proto-def/proto/browser_stream.proto:0:0-0:0) - gRPC message definitions
- `macroquad-client/src/network/grpc_client.rs` - Client networking

**Client Rendering & Input:**
- [macroquad-client/src/main.rs](cci:7://file:///home/kiel/stage/cl-rust-generator/examples/20_webprox_avif/macroquad-client/src/main.rs:0:0-0:0) - Main client logic
- [macroquad-client/src/render/ui.rs](cci:7://file:///home/kiel/stage/cl-rust-generator/examples/20_webprox_avif/macroquad-client/src/render/ui.rs:0:0-0:0) - UI rendering and input handling
- [macroquad-client/src/render/main_loop.rs](cci:7://file:///home/kiel/stage/cl-rust-generator/examples/20_webprox_avif/macroquad-client/src/render/main_loop.rs:0:0-0:0) - Main render loop
- [macroquad-client/src/state.rs](cci:7://file:///home/kiel/stage/cl-rust-generator/examples/20_webprox_avif/macroquad-client/src/state.rs:0:0-0:0) - Client state management

**Server Processing:**
- [cloud-render-srv/src/main.rs](cci:7://file:///home/kiel/stage/cl-rust-generator/examples/20_webprox_avif/cloud-render-srv/src/main.rs:0:0-0:0) - Server main logic
- `cloud-render-srv/src/browser/dom_injection.rs` - DOM coordinate extraction

**Configuration & Testing:**
- `calibration.html` - Visual debugging webpage
- All `Cargo.toml` files - Dependencies 


cd  ~/stage/cl-rust-generator/examples/20_webprox_avif/

declare -a FILES=(
        01_goal.md
        03_plan.md
        04_implementation_plan.md
        05_architecture_and_validation.md
        06_implementation_deviation.md
        proto-def/proto/browser_stream.proto
        macroquad-client/src/main.rs
        macroquad-client/src/render/ui.rs
        macroquad-client/src/render/main_loop.rs
        macroquad-client/src/state.rs
        macroquad-client/src/network/grpc_client.rs
        cloud-render-srv/src/main.rs
        cloud-render-srv/src/browser/dom_injection.rs
        macroquad-client/Cargo.toml
        cloud-render-srv/Cargo.toml
        Cargo.toml
        calibration.html
)
for i in "${FILES[@]}"; do
        echo "// start of $i"
        cat "$i"
done | xclip


for i in "${FILES[@]}"; do
        echo "// start of $i"
        ls "$i"
done

PROBLEM:
After scrolling, the displayed server image doesn't match the link highlight rectangles that scroll correctly with the mouse. The blue link boxes move properly with local scrolling, but the server-rendered image content appears misaligned or delayed, causing a visual disconnect between the interactive link overlays and the underlying webpage content.

use the following code to identify the problem or devise a plan to enhance it by logging or other calibration methods to help fix it.