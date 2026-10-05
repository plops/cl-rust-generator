//! Sentinel-1 SAR raw space-packet decoder.
//!
//! Rust port of [copernicus-radar](https://github.com/plops/copernicus-radar)
//! (C++14, CMake), which decodes Copernicus Sentinel-1 level-0 raw data
//! products (`.dat` files from `scihub.copernicus.eu`).
//!
//! # Module map
//!
//! | Rust module        | C++ source                                    |
//! |--------------------|-----------------------------------------------|
//! | `mmap`             | `copernicus_01_mmap.cpp`                      |
//! | `collect_headers`  | `copernicus_02_collect_packet_headers.cpp`    |
//! | `process_headers`  | `copernicus_03_process_packet_headers.cpp`    |
//! | `decode_packet`    | `copernicus_04_decode_packet.cpp`             |
//! | `decode_type_ab`   | `copernicus_05_decode_type_ab_packet.cpp`     |
//! | `sub_commutated`   | `copernicus_06_decode_sub_commutated_data.cpp`|
//! | `decode_type_c`    | `copernicus_07_decode_type_c_packet.cpp`      |
//! | `header_export`    | `--export-headers` CSV (replaces the C++ embedded Python shell) |
//! | `header`           | header-field fragments in `main`              |
//! | `tables`           | reconstruction tables in `04` / `07`          |
//! | `utils`            | `utils.h` + `consume_padding_bits`            |
//! | `State` (here)     | `globals.h`                                   |
//! | binary `main`      | `copernicus_00_main.cpp`                      |

pub mod collect_headers;
pub mod decode_packet;
pub mod decode_type_ab;
pub mod decode_type_c;
pub mod error;
pub mod header;
pub mod header_export;
pub mod mmap;
pub mod process_headers;
pub mod sub_commutated;
pub mod tables;
pub mod utils;

use std::collections::HashMap;
use std::time::Instant;

pub use error::{Error, Result};

use crate::header_export::PacketHeaderTable;

/// Global decoder state (`struct State` in `globals.h`).
///
/// The C++ program keeps one mutable global; the Rust port threads an owned
/// `State` through the pipeline instead.
#[derive(Debug, Default)]
pub struct State {
    /// Processing start time for log timestamps.
    pub start_time: Option<Instant>,
    /// Input file name.
    pub filename: String,
    /// File offset of each packet.
    pub header_offset: Vec<usize>,
    /// First 68 bytes of each packet.
    pub header_data: Vec<[u8; 68]>,
    /// Mapped file size in bytes.
    pub mmap_filesize: usize,
    /// Packet-header column table (populated on decode error, like C++).
    pub packet_header: PacketHeaderTable,
    /// Histogram: elevation beam address -> quad count (signal packets).
    pub map_ele: HashMap<u32, u64>,
    /// Histogram: calibration type -> packet count.
    pub map_cal: HashMap<u32, u64>,
    /// Histogram: signal type -> packet count.
    pub map_sig: HashMap<u32, u64>,
}

impl State {
    /// Nanoseconds since `start_time` (0 when unset) for log lines.
    pub fn elapsed_nanos(&self) -> u128 {
        self.start_time.map(|t| t.elapsed().as_nanos()).unwrap_or(0)
    }
}
