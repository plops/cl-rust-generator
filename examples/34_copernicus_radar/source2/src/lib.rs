//! `sar_source2` — Einheitlicher Sentinel-1 Level-0 SAR-Prozessor für Stripmap (SM),
//! Interferometric Wide Swath (IW TOPS) und Extra Wide Swath (EW TOPS) auf CPU und GPU (`cuda-oxide`).

#[path = "04_chirp.rs"]
pub mod chirp;
#[path = "08_cufft.rs"]
pub mod cufft;
#[path = "03_ephem.rs"]
pub mod ephem;
#[path = "09_gpu.rs"]
pub mod gpu;
#[path = "05_ingest.rs"]
pub mod ingest;
#[path = "11_insar.rs"]
pub mod insar;
#[path = "02_l0.rs"]
pub mod l0;
#[path = "10_mosaic.rs"]
pub mod mosaic;
#[path = "06_rda.rs"]
pub mod rda;
#[path = "07_tdbp.rs"]
pub mod tdbp;
#[path = "01_types.rs"]
pub mod types;

pub use types::Error;
