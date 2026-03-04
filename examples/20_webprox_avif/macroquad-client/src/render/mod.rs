// Module file aliases for the numbered files
#[path = "01_main_loop.rs"]
pub mod main_loop;

#[path = "02_ui.rs"]
pub mod ui;

#[path = "03_input.rs"]
pub mod input;

// Re-export for backwards compatibility
pub use main_loop::run_render_loop;
