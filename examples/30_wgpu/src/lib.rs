//! Treemap-Disk-Visualisierer: Bibliothek (Module) für Binärprogramm und Tests.
//!
//! Module in Datenfluss-Reihenfolge: Typen → Scan → Layout → Farbe →
//! Text-Atlas → Vulkan-Renderer. (Die winit-Anwendung folgt in Phase 3.)

#[path = "04_color.rs"]
pub mod color;
#[path = "03_layout.rs"]
pub mod layout;
#[path = "06_render.rs"]
pub mod render;
#[path = "02_scan.rs"]
pub mod scan;
#[path = "05_text.rs"]
pub mod text;
#[path = "01_types.rs"]
pub mod types;
