pub struct AvifDecoder {
    // Simple decoder for demo
}

impl AvifDecoder {
    pub fn new() -> Self {
        Self {}
    }
    
    pub fn decode_frame(&mut self, _av1_data: &[u8]) -> Result<(u32, u32, Vec<u8>), String> {
        // For demo, return a simple test frame
        Ok((640, 480, vec![0u8; 640 * 480 * 4]))
    }
}

impl Default for AvifDecoder {
    fn default() -> Self {
        Self::new()
    }
}
