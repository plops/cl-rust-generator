//! Error type for the Sentinel-1 SAR raw packet decoder.
//!
//! The C++ program signals decode failures with `assert(0)` (abort) or
//! `std::out_of_range` (caught only around the per-packet decode calls).
//! Every such site maps to a [`Error`] variant; [`Error::is_fatal`]
//! distinguishes aborts (bad sync marker, truncated file) from recoverable
//! per-packet decode failures.

use std::fmt;
use std::io;

/// All failures the decoder can report.
#[derive(Debug)]
pub enum Error {
    /// Underlying I/O failure (open/read/write/mmap).
    Io(io::Error),
    /// The bit reader ran past the end of the mapped file.
    Truncated {
        /// Absolute byte offset that could not be read.
        offset: usize,
        /// Total file size in bytes.
        filesize: usize,
    },
    /// A bit-rate code outside the valid range 0..=4 was read.
    BadBrc {
        /// The out-of-range code.
        brc: u32,
        /// Absolute byte offset of the reader when the code was read.
        offset: usize,
    },
    /// The packet sync marker differs from `0x352EF853`.
    BadSync {
        /// Index of the offending space packet.
        packet_idx: usize,
        /// Marker value actually found.
        found: u32,
    },
    /// A magnitude code exceeds the maximum for its BRC / BAQ mode.
    McodeTooLarge {
        /// The out-of-range magnitude code.
        mcode: u32,
        /// BRC (FDBAQ) or bit width (type C) in effect.
        brc: u8,
    },
    /// A `.at()` table lookup failed (threshold index or mcode).
    TableIndex {
        /// What was being looked up, e.g. `"nrl"` or `"sf"`.
        table: &'static str,
        /// The out-of-range index.
        index: usize,
    },
    /// A sub-commutated word index outside 0..65 was fed to the decoder.
    BadAncillaryIndex {
        /// The out-of-range index.
        index: usize,
    },
    /// File ended in the middle of a 68-byte packet header.
    HeaderTooShort {
        /// Absolute byte offset where the header starts.
        offset: usize,
    },
    /// The file contains no packets, or no signal packets for the selected
    /// elevation beam address.
    NoPackets(&'static str),
    /// An output buffer would have overflowed (the C++ program has a fixed
    /// 512-echo cap and writes past it; this port refuses instead).
    OutputOverflow(&'static str),
    /// A packet uses a BAQ mode its decoder does not support (the C++
    /// `assert`s on the mode abort the program).
    UnsupportedBaqMode {
        /// Index of the offending space packet.
        packet_idx: usize,
        /// The unsupported mode.
        baq_mode: u32,
    },
}

impl Error {
    /// True for failures that aborted the C++ program (`assert(0)` outside
    /// any `try` block, or a failed file operation): processing cannot
    /// continue. Per-packet decode failures return false; the caller logs
    /// them, snapshots the packet-header table, and continues with the next
    /// packet, mirroring the `catch (std::out_of_range)` in `main`.
    pub fn is_fatal(&self) -> bool {
        matches!(
            self,
            Error::Io(_)
                | Error::BadSync { .. }
                | Error::HeaderTooShort { .. }
                | Error::NoPackets(_)
                | Error::OutputOverflow(_)
                | Error::BadAncillaryIndex { .. }
                | Error::UnsupportedBaqMode { .. }
        )
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Io(e) => write!(f, "i/o error: {e}"),
            Error::Truncated { offset, filesize } => write!(
                f,
                "bit reader past end of file at byte {offset} (filesize {filesize})"
            ),
            Error::BadBrc { brc, offset } => {
                write!(f, "bit-rate code {brc} out of range at byte {offset}")
            }
            Error::BadSync { packet_idx, found } => write!(
                f,
                "packet {packet_idx}: sync marker 0x{found:08X} != 0x352EF853"
            ),
            Error::McodeTooLarge { mcode, brc } => {
                write!(f, "magnitude code {mcode} too large for brc {brc}")
            }
            Error::TableIndex { table, index } => {
                write!(f, "table {table}: index {index} out of range")
            }
            Error::BadAncillaryIndex { index } => {
                write!(f, "sub-commutated data index {index} out of range 0..65")
            }
            Error::HeaderTooShort { offset } => {
                write!(f, "truncated 68-byte header at offset {offset}")
            }
            Error::NoPackets(what) => write!(f, "no packets: {what}"),
            Error::OutputOverflow(what) => write!(f, "output overflow: {what}"),
            Error::UnsupportedBaqMode {
                packet_idx,
                baq_mode,
            } => write!(f, "packet {packet_idx}: unsupported baq mode {baq_mode}"),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::Io(e) => Some(e),
            _ => None,
        }
    }
}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        Error::Io(e)
    }
}

/// Crate result alias.
pub type Result<T> = std::result::Result<T, Error>;
