//! `lbw-common` — geteiltes MVP-Protokoll: Typen, Framing, YUV.
//! Nur Modul-Deklarationen.

#[path = "01_types.rs"]
pub mod types;

#[path = "02_framing.rs"]
pub mod framing;

#[path = "03_yuv.rs"]
pub mod yuv;

pub use types::{ClientMsg, Rect, ServerMsg, TextItem};
