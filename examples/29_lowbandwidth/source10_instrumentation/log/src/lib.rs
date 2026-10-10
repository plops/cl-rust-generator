//! `lbw-log` — `.lbwlog`-Aufzeichnungen: Record-Typen, Datei-IO, Statistik.
//! Nur Modul-Deklarationen.

#[path = "01_record.rs"]
pub mod record;

#[path = "02_io.rs"]
pub mod io;

#[path = "03_stats.rs"]
pub mod stats;

#[path = "04_summary.rs"]
pub mod summary;

#[path = "05_deep.rs"]
pub mod deep;

#[path = "06_export.rs"]
pub mod export;

#[path = "07_legacy.rs"]
pub mod legacy;

#[path = "08_delivery.rs"]
pub mod delivery;

pub use io::Recorder;
pub use record::{
    Dir, Fnv1a64, FrameMs, GapEvent, LOG_MAGIC, LOG_VERSION, LogRecord, MsgKind, Stamp, TileStat,
    fnv1a64,
};
