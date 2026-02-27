use zstd::bulk::{Compressor, Decompressor};

const COMPRESSION_LEVEL: i32 = 3;

/// Persistent-context zstd compressor for DOM patches
pub struct PatchCompressor {
    compressor: Compressor<'static>,
}

impl PatchCompressor {
    pub fn new() -> Self {
        let compressor = Compressor::new(COMPRESSION_LEVEL).expect("Failed to create zstd compressor");
        Self { compressor }
    }

    /// Compress a patch payload using zstd
    pub fn compress(&mut self, data: &[u8]) -> Vec<u8> {
        self.compressor
            .compress(data)
            .unwrap_or_else(|_| data.to_vec())
    }
}

/// Decompressor for the client side
#[allow(dead_code)]
pub struct PatchDecompressor {
    decompressor: Decompressor<'static>,
}

#[allow(dead_code)]
impl PatchDecompressor {
    pub fn new() -> Self {
        let decompressor = Decompressor::new().expect("Failed to create zstd decompressor");
        Self { decompressor }
    }

    /// Decompress a patch payload
    pub fn decompress(&mut self, data: &[u8], max_size: usize) -> Result<Vec<u8>, String> {
        self.decompressor
            .decompress(data, max_size)
            .map_err(|e| format!("Decompression failed: {}", e))
    }
}
