//! GPU-beschleunigte 2D-SPH-Fluidsimulation (cuda-oxide, macroquad).
//!
//! Nur Moduldeklarationen und Re-Exports; Logik liegt in den nummerierten
//! Moduldateien (`01_types.rs` … `09_headless.rs`).

#[path = "01_types.rs"]
pub mod types;

#[path = "02_params.rs"]
pub mod params;

#[path = "03_sph_math.rs"]
pub mod sph_math;

#[path = "04_spatial_grid.rs"]
pub mod spatial_grid;

#[cfg(feature = "gpu")]
#[path = "05a_sort_kernels.rs"]
pub mod sort_kernels;

#[cfg(feature = "gpu")]
#[path = "05_gpu_kernels.rs"]
pub mod gpu_kernels;

#[path = "06_backend.rs"]
pub mod backend;

#[cfg(feature = "gpu")]
#[path = "06a_gpu_backend.rs"]
pub mod gpu_backend;

#[path = "07_renderer.rs"]
pub mod renderer;

#[path = "07a_water_style.rs"]
pub mod water_style;

#[path = "08_app.rs"]
pub mod app;

#[path = "09_headless.rs"]
pub mod headless;
