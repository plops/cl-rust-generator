//! Memory-mapped input file (`copernicus_01_mmap.cpp`).
//!
//! The C++ code uses POSIX `open`/`mmap`/`munmap` directly; this module wraps
//! [`memmap2`] so unmapping happens automatically on drop.

use std::fs::File;
use std::path::Path;

use memmap2::Mmap;

/// Read-only memory mapping of a Sentinel-1 raw `.dat` file.
pub struct MappedFile {
    mmap: Mmap,
    filesize: usize,
}

impl MappedFile {
    /// Map `path` read-only into memory (`init_mmap`).
    pub fn open(path: &Path) -> std::io::Result<MappedFile> {
        let file = File::open(path)?;
        let filesize = file.metadata()?.len() as usize;
        // SAFETY: the mapping is read-only and the file is not mutated
        // through this handle; `memmap2` documents this usage.
        let mmap = unsafe { Mmap::map(&file)? };
        Ok(MappedFile { mmap, filesize })
    }

    /// File size in bytes (`get_filesize`).
    pub fn filesize(&self) -> usize {
        self.filesize
    }

    /// The mapped bytes.
    pub fn bytes(&self) -> &[u8] {
        &self.mmap
    }
}
