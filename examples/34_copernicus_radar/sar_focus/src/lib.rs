//! Sentinel-1 Stripmap-Fokussierung: CPU-Referenz + GPU-Pipeline.
//!
//! Module in Datenflussreihenfolge (Dateinamen nummeriert):
//! Typen → Metadaten → Ephemeriden → Chirp → Range → RDA → TDBP →
//! cuFFT → Kernel → Pipeline → Quicklook.

#[path = "01_types.rs"]
pub mod types;
#[path = "02_meta.rs"]
pub mod meta;
#[path = "03_ephem.rs"]
pub mod ephem;
#[path = "04_chirp.rs"]
pub mod chirp;
#[path = "05_range.rs"]
pub mod range;
#[path = "06_rda.rs"]
pub mod rda;
#[path = "07_tdbp.rs"]
pub mod tdbp;

pub use types::Error;
