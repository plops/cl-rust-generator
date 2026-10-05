//! Sentinel-1 Stripmap-Fokussierung: CPU-Referenz + GPU-Pipeline.
//!
//! Module in Datenflussreihenfolge (Dateinamen nummeriert):
//! Typen → Metadaten → Ephemeriden → Chirp → Range → RDA → TDBP →
//! cuFFT → Kernel → Pipeline → Quicklook.

#[path = "04_chirp.rs"]
pub mod chirp;
#[path = "08_cufft.rs"]
pub mod cufft;
#[path = "03_ephem.rs"]
pub mod ephem;
#[path = "10_gpu.rs"]
pub mod gpu;
#[path = "12_ingest.rs"]
pub mod ingest;
#[path = "09_kernel.rs"]
pub mod kernel;
#[path = "11_look.rs"]
pub mod look;
#[path = "02_meta.rs"]
pub mod meta;
#[path = "05_range.rs"]
pub mod range;
#[path = "06_rda.rs"]
pub mod rda;
#[path = "07_tdbp.rs"]
pub mod tdbp;
#[path = "01_types.rs"]
pub mod types;

pub use types::Error;
