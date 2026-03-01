use aom_decode::avif::{Avif, parse_avif};
use anyhow::Result;

pub struct AvifDecoder {
    // Decoder state could be added here if needed
}

impl AvifDecoder {
    pub fn new() -> Self {
        Self {}
    }
    
    pub fn decode_frame(&mut self, av1_data: &[u8]) -> Result<(u32, u32, Vec<u8>)> {
        println!("[Client] Decoding AVIF frame: {} bytes", av1_data.len());
        
        // Use aom-decode to decode AVIF data
        let avif = parse_avif(av1_data)?;
        
        // Get image dimensions
        let width = avif.width();
        let height = avif.height();
        
        // Decode to RGBA
        let rgba_data = avif.decode_rgba()?;
        
        println!("[Client] Successfully decoded AVIF: {}x{}, {} bytes", 
                width, height, rgba_data.len());
        
        Ok((width, height, rgba_data))
    }
}

impl Default for AvifDecoder {
    fn default() -> Self {
        Self::new()
    }
}
