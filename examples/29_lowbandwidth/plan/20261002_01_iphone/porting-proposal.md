Yes, porting the client to iPhone (iOS) is not only possible, it is **well-suited to the current codebase**.

The existing project is already cleanly split: `android_client/rust-core` already stripped out Macroquad and left a headless engine (`Engine`, `Scene`, `rav1d` decoder, `Assembler`, protocol handling). You can reuse almost all of that work for iOS.

---

### 1. Suggested Approach: Rust Kernel + Native Swift/UIKit

You should strongly favor a **Rust Core + Native Swift UI** approach.

#### Why "Port Everything to Rust" (e.g. Macroquad on iOS) is painful:
* **Virtual Keyboard & IME:** On iOS, getting a soft keyboard up, handling auto-correct, dead keys, unicode composition, and hardware keyboard shortcuts in Macroquad or SDL is notoriously frustrating and buggy.
* **Apple Human Interface & Gestures:** iOS users expect fluid pinch-to-zoom with inertia, native selection magnifiers, Dynamic Island / notch safe area handling, and smooth gesture transitions. Game loops fighting UIKit gesture recognizers feel foreign.
* **App Store review:** Background suspension, memory warnings, and view controller lifecycle are much harder to coordinate inside a pure game-engine loop.

#### Why a Full Swift Port is wasteful:
* Re-implementing AV1 tile assembly, protocol framing, CTC delta tracking, and `rav1d` bindings in pure Swift would be a massive amount of redundant code.

#### The Recommended Model:
```
┌─────────────────────────────────────────────────────────────┐
│                       iOS App (Swift)                       │
│  - SwiftUI / UIKit (ScreenView, VirtualKeybar)              │
│  - Viewport, TouchInput, StickyMods (ports of Kotlin logic) │
│  - MetalKit / CoreGraphics (Canvas texture blit + Unifont)  │
│  - UIKeyInput / UITextInput (Soft- & Hardware keyboards)    │
└──────────────────────────────┬──────────────────────────────┘
                               │ C-ABI / Swift Bridging Header
┌──────────────────────────────┴──────────────────────────────┐
│                    lbw-ios-core (Rust)                      │
│  - Engine, Scene, Assembler (from lbw-client)               │
│  - rav1d (AV1 decoder, compiles natively to aarch64-apple-ios)│
│  - Optional: SSH Tunnel built-in via russh                  │
└─────────────────────────────────────────────────────────────┘
```

Notice that several Kotlin files in `android-app` are already **pure JVM logic with zero Android dependencies**:
* `04_Viewport.kt` (zoom/pan math)
* `05_TouchInput.kt` (trackpad/direct/select gesture state machine)
* `06_VirtualKeybar.kt` (`StickyMods` logic)
* `03_TextItems.kt` (binary blob parser & font scale calculations)

These can be translated line-by-line into Swift in an afternoon.

---

### 2. How to Establish the SSH Tunnel on iPhone

On iOS, sandboxed apps **are allowed** to open TCP connections to remote hosts and bind to `127.0.0.1` locally (used by many popular iOS apps like Termius, Blink Shell, and Screens).

There are two primary ways to do this:

#### Option A: In Rust via `russh` (Recommended)
Instead of using JSch (Java) on Android and another library on iOS, you can move the SSH tunnel directly into the Rust engine using **[`russh`](https://crates.io/crates/russh)** (a pure-Rust, async SSH-2 client):
1. Establish the SSH session to the remote server.
2. Open a `direct-tcpip` channel (`localHost:localPort` $\to$ `remoteHost:remotePort`).
3. You can either:
   * Bind to `127.0.0.1:0` inside the Rust process and let `lbw_client::net` connect to it (identical to the JSch model).
   * Or even better: pipe the channel stream directly into the client network loop without needing a local loopback socket.
4. **Benefit:** Both iOS and Android can share the exact same SSH tunneling, host-key pinning (TOFU), and key/password authentication code.

#### Option B: In Swift via `Citadel` (SwiftNIO SSH)
If you prefer managing network connections in Swift:
* Use **[Citadel](https://github.com/orlandos-nl/Citadel)**, an actively maintained SSH client library built on top of Apple's **SwiftNIO**.
* It supports password/key authentication, host-key validation, and opening direct-TCP/IP port-forwarding channels:
  ```swift
  let client = try await SSHClient.connect(to: host, ...)
  // Open port forward channel:
  let channel = try await client.openDirectTCPIPChannel(
      forwarding: .init(host: "127.0.0.1", port: 7878),
      from: .init(host: "127.0.0.1", port: localPort)
  )
  ```

*(Note: Avoid C-based libraries like `libssh2` / `NMSSH` if you can, as cross-compiling OpenSSL and libssh2 for both `aarch64-apple-ios` and `aarch64-apple-ios-sim` into an XCFramework is tedious).*

---

### 3. Implementation Blueprint for iOS

#### A. Compiling the Rust Core
`rav1d` is 100% Rust, which makes cross-compiling to iOS straightforward:
```bash
rustup target add aarch64-apple-ios aarch64-apple-ios-sim
cargo build --target aarch64-apple-ios --release
cargo build --target aarch64-apple-ios-sim --release
```
Pack these into an `.xcframework` using `lipo` and `xcodebuild -create-xcframework`.

#### B. The C-FFI Bridge
Instead of JNI, C-FFI for Swift is much simpler. In your Rust core:
```rust
#[repr(C)]
pub struct EngineHandle(Mutex<Engine>);

#[no_mangle]
pub extern "C" fn lbw_engine_new(addr: *const c_char, dead_after_s: u32) -> *mut EngineHandle { ... }

#[no_mangle]
pub extern "C" fn lbw_engine_poll(handle: *mut EngineHandle, out_rgba: *mut u8, len: usize) -> i32 { ... }

#[no_mangle]
pub extern "C" fn lbw_engine_free(handle: *mut EngineHandle) { ... }
```
Swift can import these functions directly through a Bridging Header or Swift Package without any JNI boilerplate.

#### C. Rendering on Screen
* **Canvas (Background & Icons):**
  * When `lbw_engine_poll` returns the `FRAME` flag, update an `MTLTexture` or a `CGContext` from the raw RGBA buffer.
  * Disable bilinear filtering on the layer (`minificationFilter = .nearest`, `magnificationFilter = .nearest`), matching your desktop and Android clients.
* **Texts (Unifont):**
  * Bundle `unifont.otf` in your Xcode project.
  * Use `CoreText` (`CTFont` / `NSAttributedString`) or `UILabel` recycling.
  * `CoreText` allows setting horizontal scale aspect (`kCTForegroundColorAttributeName`, glyph matrix) exactly like `TextItems.scaleXFor`.

#### D. Input & Keyboards
* Create a custom `UIView` that conforms to `UIKeyInput`.
* Implement `insertText(_ text: String)` and `deleteBackward()` to send typed characters through the Rust engine.
* For external keyboards (iPad Magic Keyboard / Bluetooth keyboard), override:
  ```swift
  override func pressesBegan(_ presses: Set<UIPress>, with event: UIPressesEvent?)
  override func pressesEnded(_ presses: Set<UIPress>, with event: UIPressesEvent?)
  ```
  to map keycodes (Arrows, Enter, Escape, Function keys) directly to Keysyms.

---

### Summary
1. **Yes, it is completely feasible.**
2. **Architecture:** Use a **Rust core + Native Swift frontend**. The headless code you wrote for `android_client/rust-core` can be ported directly into an iOS-compatible static library/XCFramework.
3. **SSH Tunnel:** Best done directly inside the Rust engine via **`russh`** (pure Rust) or in Swift via **`Citadel`** (SwiftNIO). Both support local port forwarding without requiring root or special entitlements.
