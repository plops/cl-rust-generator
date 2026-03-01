use rav1e::prelude::*;
use yuv::*;
use std::sync::Arc;
use log::{info, error, debug};

pub struct Av1Encoder {
    ctx: Context<u8>,
    width: usize,
    height: usize,
}

impl Av1Encoder {
    pub fn new(width: usize, height: usize, speed: u8) -> Result<Self, Box<dyn std::error::Error>> {
        let mut enc = EncoderConfig::default();
        enc.width = width;
        enc.height = height;
        enc.bit_depth = 8;
        enc.chroma_sampling = ChromaSampling::Cs420;
        enc.speed_settings = SpeedSettings::from_preset(speed);
        enc.low_latency = true; // Crucial for interactive remote browser
        enc.min_keyint = 60;
        enc.max_keyint = 60; // Periodic keyframes for sync

        let cfg = Config::new()
            .with_encoder_config(enc)
            .with_threads(num_cpus::get());

        let ctx: Context<u8> = cfg.new_context()?;

        Ok(Self { ctx, width, height })
    }

    pub fn encode_rgba_frame(&mut self, rgba_pixels: &[u8]) -> Result<Vec<u8>, Box<dyn std::error::Error>> {
        // 1. RGB -> YUV Conversion (using yuv crate)
        let mut planar_image = YuvPlanarImageMut::<u8>::alloc(
            self.width as u32,
            self.height as u32,
            YuvChromaSubsampling::Yuv420,
        );

        let rgba_stride = self.width * 4;

        rgb_to_yuv420(
            &mut planar_image,
            rgba_pixels,
            rgba_stride as u32,
            YuvRange::Limited,
            YuvStandardMatrix::Bt601,
        ).map_err(|e| format!("YUV conversion failed: {:?}", e))?;

        // 2. Prepare Frame for rav1e
        let mut frame = self.ctx.new_frame();
        frame.planes[0].copy_from_raw_u8(planar_image.y.data, planar_image.y.stride as usize, 1);
        frame.planes[1].copy_from_raw_u8(planar_image.u.data, planar_image.u.stride as usize, 1);
        frame.planes[2].copy_from_raw_u8(planar_image.v.data, planar_image.v.stride as usize, 1);

        // 3. Encoder Interaction
        self.ctx.send_frame(Arc::new(frame))?;

        let mut encoded_data = Vec::new();
        loop {
            match self.ctx.receive_packet() {
                Ok(packet) => {
                    encoded_data.extend_from_slice(&packet.data);
                }
                Err(EncoderStatus::NeedMoreData) => break,
                Err(EncoderStatus::Encoded) => continue,
                Err(e) => return Err(format!("Encoder error: {:?}", e).into()),
            }
        }

        Ok(encoded_data)
    }
}
