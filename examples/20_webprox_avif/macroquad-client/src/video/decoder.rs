use dav1d::{Settings, Dav1dContext, Dav1dPicture};
use yuv::*;
use log::{info, error, debug};

pub struct Av1Decoder {
    ctx: Dav1dContext,
}

impl Av1Decoder {
    pub fn new() -> Result<Self, Box<dyn std::error::Error>> {
        let mut settings = Settings::new();
        // Customize settings if needed (e.g. threads)
        let ctx = Dav1dContext::new(&settings).map_err(|e| format!("Failed to create dav1d context: {:?}", e))?;
        Ok(Self { ctx })
    }

    pub fn decode_av1_packet(&mut self, packet_data: &[u8]) -> Result<Option<Vec<u8>>, Box<dyn std::error::Error>> {
        // 1. Send data to decoder
        self.ctx.send_data(packet_data, None, None, None).map_err(|e| format!("Failed to send data to decoder: {:?}", e))?;

        // 2. Receive decoded picture
        match self.ctx.get_picture() {
            Ok(pic) => {
                // 3. YUV -> RGB conversion (using yuv crate)
                let width = pic.width();
                let height = pic.height();
                let mut rgba = vec![0u8; (width * height * 4) as usize];

                // Map Dav1dPicture planes to yuv crate expects
                // For 4:2:0:
                let y = pic.plane(0);
                let u = pic.plane(1);
                let v = pic.plane(2);
                
                let y_stride = pic.stride(0) as u32;
                let u_stride = pic.stride(1) as u32;
                let v_stride = pic.stride(2) as u32;

                yuv420_to_rgb(
                    y, y_stride,
                    u, u_stride,
                    v, v_stride,
                    &mut rgba, (width * 4) as u32,
                    width, height,
                    YuvRange::Limited,
                    YuvStandardMatrix::Bt601,
                ).map_err(|e| format!("RGB conversion failed: {:?}", e))?;

                Ok(Some(rgba))
            }
            Err(e) if e == dav1d::Error::Again => {
                // Need more data
                Ok(None)
            }
            Err(e) => {
                Err(format!("Decoding error: {:?}", e).into())
            }
        }
    }
}
