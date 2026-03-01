# Plan 07: Iced Migration Strategy

## Context & Problem Statement

Following the successful resolution of AVIF decoding challenges in Plan 06, we've encountered a critical architectural issue with the macroquad framework. The OpenGL-based rendering system in macroquad creates fundamental threading conflicts with tokio's async runtime, making the gRPC streaming architecture unstable and prone to deadlocks.

## Root Cause Analysis

### Threading Incompatibility
- **macroquad**: Requires OpenGL context on main thread, blocking render loop
- **tokio**: Requires its own runtime with thread-local storage for async operations
- **Conflict**: Both systems compete for thread control, causing race conditions

### Current Architecture Problems
```rust
// PROBLEMATIC: Complex multi-threading approach
let tokio_runtime = tokio::runtime::Runtime::new().unwrap();
tokio_runtime.spawn(async { network_loop(state).await });
std::thread::spawn(|| { macroquad::start(gui_handler) });
// Results in: Deadlocks, race conditions, synchronization overhead
```

## Solution: Iced Framework Migration

### Why Iced?
- **Native Tokio Integration**: Built-in async subscription system
- **Cross-platform**: Consistent behavior across Linux, Windows, macOS
- **GPU Accelerated**: Hardware-accelerated rendering via wgpu
- **Type Safety**: Strong typing with compile-time guarantees
- **Event-driven**: Natural fit for gRPC streaming architecture

### Migration Strategy

#### Phase 1: Foundation Setup
1. **Create iced-client crate** with proper dependencies
2. **Establish basic window** with Iced framework
3. **Verify build system** and basic rendering works
4. **Create working demo** to validate approach

#### Phase 2: Architecture Redesign
1. **Replace threading model** with Iced's subscription system
2. **Implement gRPC client** using Iced's async integration
3. **Design message passing** between network and UI components
4. **Create state management** without Arc<Mutex<>> synchronization

#### Phase 3: Feature Migration
1. **Port AVIF decoder** integration to Iced
2. **Implement canvas rendering** for video frames
3. **Add input handling** (mouse, keyboard, scrolling)
4. **Migrate CLI test modes** for AI agent compatibility

#### Phase 4: Integration & Testing
1. **End-to-end testing** with cloud-render-srv
2. **Performance benchmarking** vs macroquad approach
3. **CLI compatibility** verification for test automation
4. **Documentation updates** for new architecture

## Technical Implementation Plan

### New Architecture
```rust
// SOLUTION: Single-threaded with native async integration
impl Application for AvifClient {
    fn subscription(&self) -> Subscription<Message> {
        // Native tokio integration without threading conflicts
        GrpcSubscription::subscription().map(|result| match result {
            Ok(update) => Message::ServerUpdate(update),
            Err(error) => Message::Error(error.to_string()),
        })
    }
    
    fn update(&mut self, message: Message) -> Command<Message> {
        match message {
            Message::ServerUpdate(update) => {
                // Handle gRPC updates in main thread
                self.handle_server_update(update)
            }
            // ... other message handling
        }
    }
}
```

### Key Components

#### 1. gRPC Subscription System
```rust
pub struct GrpcSubscription;

impl GrpcSubscription {
    pub fn subscription() -> Subscription<Result<ServerUpdate, Status>> {
        Subscription::run_with_id(
            Id::new(0),
            stream::unfold((), |_| async {
                // Async gRPC streaming without threading conflicts
                let mut client = RemoteBrowserClient::connect("http://127.0.0.1:50051").await?;
                let mut stream = client.stream_session(request).await?.into_inner();
                
                while let Some(update) = stream.next().await {
                    yield Some(update);
                }
                
                None
            })
        )
    }
}
```

#### 2. Canvas Rendering System
```rust
pub struct CanvasRenderer {
    texture: Option<Texture>,
    decoder: AvifDecoder,
}

impl CanvasRenderer {
    pub fn handle_frame(&mut self, frame: VideoFrame) -> Result<(), Error> {
        // Decode AVIF frame
        let rgba_data = self.decoder.decode(&frame.av1_data)?;
        
        // Update GPU texture
        if let Some(ref mut texture) = self.texture {
            texture.update(&rgba_data);
        }
        
        Ok(())
    }
}
```

#### 3. State Management
```rust
#[derive(Debug, Clone)]
pub struct AppState {
    pub current_url: String,
    pub scroll_position: f32,
    pub video_frames: HashMap<i32, Vec<u8>>,
    pub links: Vec<LinkBox>,
    pub search_results: Vec<SearchMatch>,
}

impl Application for AvifClient {
    type State = AppState;
    type Message = Message;
    
    // ... implementation
}
```

## Migration Benefits

### Technical Benefits
- **Eliminated Threading Conflicts**: No more OpenGL/tokio conflicts
- **Simplified Architecture**: Single-threaded event loop
- **Better Performance**: Reduced synchronization overhead
- **Type Safety**: Compile-time guarantees for message passing
- **Maintainability**: Cleaner, more idiomatic Rust code

### Development Benefits
- **Faster Development**: Less complex debugging
- **Better Testing**: Easier to write and maintain tests
- **Documentation**: Better community support and examples
- **Future-proof**: Active development and modern GPU APIs

## Risk Mitigation

### Technical Risks
1. **Learning Curve**: Team unfamiliar with Iced API
   - *Mitigation*: Start with simple demo, incremental migration
2. **API Changes**: Iced 0.13 introduces breaking changes
   - *Mitigation*: Pin to stable version, track updates
3. **Performance**: Unknown performance characteristics
   - *Mitigation*: Benchmark against macroquad baseline

### Project Risks
1. **Timeline**: Migration may take longer than expected
   - *Mitigation*: Parallel development, phased approach
2. **Compatibility**: CLI test modes may need updates
   - *Mitigation*: Maintain compatibility layer during transition

## Success Criteria

### Phase 1 Success
- [ ] Basic Iced window opens and displays content
- [ ] Build system works without errors
- [ ] Simple demo runs successfully

### Phase 2 Success
- [ ] gRPC client connects and receives messages
- [ ] Message passing works without threading issues
- [ ] State management functions correctly

### Phase 3 Success
- [ ] AVIF frames decode and display
- [ ] User input handling works
- [ ] CLI test modes function

### Phase 4 Success
- [ ] Full end-to-end functionality
- [ ] Performance meets or exceeds macroquad
- [ ] All automated tests pass

## Timeline

### Week 1: Foundation
- Day 1-2: Setup iced-client crate and dependencies
- Day 3-4: Create basic window and demo application
- Day 5: Verify build system and basic functionality

### Week 2: Architecture
- Day 1-3: Implement gRPC subscription system
- Day 4-5: Design and implement message passing

### Week 3: Features
- Day 1-3: Port AVIF decoder and canvas rendering
- Day 4-5: Add input handling and state management

### Week 4: Integration
- Day 1-3: Complete feature migration and testing
- Day 4-5: Performance optimization and documentation

## Next Steps

1. **Immediate**: Create iced-client crate and basic demo
2. **Short-term**: Implement gRPC subscription system
3. **Medium-term**: Complete full feature migration
4. **Long-term**: Optimize and document new architecture

This migration addresses the fundamental threading issues while providing a more robust foundation for the AVIF remote browser client. The Iced framework's native async integration eliminates the core architectural problems that made the macroquad approach unstable.
