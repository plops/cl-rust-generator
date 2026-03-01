use macroquad::prelude::*;
use std::sync::{Arc, Mutex};
use tokio::sync::mpsc;
use log::{info, warn, error, debug};

// Import modules
mod state;
mod network;
mod render;
mod video;

// Integration test module - only include when feature is enabled
#[cfg(feature = "integration-test")]
mod integration_test;

#[cfg(feature = "integration-test")]
use integration_test::parse_integration_test_args;

use state::ClientState;
use network::grpc_client;
use render::main_loop;

// Initialize logging at startup
fn init_logging() {
    core_utils::logging::init_logging("macroquad-client")
        .expect("Failed to initialize logging");
}

#[macroquad::main("AV1 Remote Browser")]
async fn main() {
    init_logging();
    
    #[cfg(feature = "integration-test")]
    {
        // Parse CLI arguments for integration test mode
        let cli = clap::Parser::parse();
        
        // Check for integration test mode
        if let Some(test_args) = parse_integration_test_args(cli) {
            info!("Running in integration test mode");
            
            // Run integration test directly with tokio runtime
            let rt = tokio::runtime::Runtime::new().unwrap();
            rt.block_on(async {
                integration_test::run_integration_test(
                    &test_args.test_image,
                    &test_args.server_addr,
                    test_args.tolerance,
                    test_args.output_result.as_deref(),
                ).await
            });
            return;
        }
    }
    
    // Normal GUI mode - use proper threading model
    info!("Starting normal GUI mode");
    let shared_state = Arc::new(Mutex::new(ClientState {
        frame_width: 1280,
        frame_height: 720,
        server_viewport_y: 0,
        local_scroll_y: 0,
        ..Default::default()
    }));
    
    let (tx_events, rx_events) = tokio::sync::mpsc::unbounded_channel();
    
    let state_clone = shared_state.clone();
    
    // Spawn network completely independent of macroquad's executor
    info!("Spawning network thread");
    std::thread::spawn(move || {
        let rt = tokio::runtime::Runtime::new().unwrap();
        rt.block_on(async {
            grpc_client::start_grpc_client(state_clone, rx_events).await;
        });
    });

    // Main thread is now correctly owned by Macroquad
    info!("Starting render loop");
    main_loop::run_render_loop(shared_state, tx_events).await;
}

