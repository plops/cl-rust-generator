//! `05_av1` — AV1-Still-Picture-Encoder (rav1e) für einzelne Bildkacheln.
//!
//! Jede Kachel ist ein eigenständiges Intra-Bild (wie in AVIF, nur ohne
//! Container — spart ~300 Byte je Kachel). Ausgabe: rohe OBUs, direkt
//! dekodierbar mit dav1d/rav1d. Aus `source6/server/09_av1.rs` übernommen,
//! ohne `asm`-Feature (kein `nasm` nötig).

use lbw_common::yuv::rgb_to_yuv420;
use rav1e::color::{ChromaSampling, PixelRange};
use rav1e::prelude::*;

/// Kleinste Boxkante (AV1 arbeitet in 8×8-Blöcken).
pub const MIN_TILE: usize = 16;

/// Kodiert ein RGB8-Bild (`w*h*3`) als AV1-Still-Picture.
/// `w`,`h` müssen gerade und ≥ [`MIN_TILE`] sein; `quantizer` 0..=255
/// (höher = kleiner/schlechter). Speed-Preset 10 und 4 Threads sind fest
/// verdrahtet (MVP: nie per CLI erreichbar gewesen).
pub fn encode_rgb(rgb: &[u8], w: usize, h: usize, quantizer: usize) -> Result<Vec<u8>, String> {
    if w < MIN_TILE || h < MIN_TILE || !w.is_multiple_of(2) || !h.is_multiple_of(2) {
        return Err(format!("ungültige Boxgröße {w}x{h}"));
    }
    let yuv = rgb_to_yuv420(rgb, w, h);
    let mut enc = EncoderConfig::with_speed_preset(10);
    enc.width = w;
    enc.height = h;
    enc.bit_depth = 8;
    enc.chroma_sampling = ChromaSampling::Cs420;
    enc.pixel_range = PixelRange::Full;
    enc.still_picture = true;
    enc.low_latency = true;
    enc.quantizer = quantizer.min(255);
    enc.min_quantizer = quantizer.min(255) as u8;
    enc.max_key_frame_interval = 1;
    let cfg = Config::new().with_encoder_config(enc).with_threads(4);
    let mut ctx: Context<u8> = cfg.new_context().map_err(|e| format!("rav1e: {e:?}"))?;

    let mut frame = ctx.new_frame();
    frame.planes[0].copy_from_raw_u8(&yuv.y, w, 1);
    frame.planes[1].copy_from_raw_u8(&yuv.u, yuv.cw(), 1);
    frame.planes[2].copy_from_raw_u8(&yuv.v, yuv.cw(), 1);
    ctx.send_frame(frame)
        .map_err(|e| format!("send_frame: {e:?}"))?;
    ctx.flush();

    let mut out = Vec::new();
    loop {
        match ctx.receive_packet() {
            Ok(pkt) => out.extend_from_slice(&pkt.data),
            Err(EncoderStatus::Encoded) => {}
            Err(EncoderStatus::LimitReached) => break,
            Err(e) => return Err(format!("receive_packet: {e:?}")),
        }
    }
    if out.is_empty() {
        return Err("rav1e lieferte kein Paket".into());
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn flat_tile_is_tiny() {
        let rgb = [40u8, 80, 160].repeat(64 * 64);
        let bytes = encode_rgb(&rgb, 64, 64, 180).unwrap();
        assert!(
            !bytes.is_empty() && bytes.len() < 200,
            "{} Byte",
            bytes.len()
        );
    }

    #[test]
    fn rejects_odd_or_tiny_sizes() {
        let rgb = vec![0u8; 15 * 16 * 3];
        assert!(encode_rgb(&rgb, 15, 16, 180).is_err());
        assert!(encode_rgb(&rgb, 8, 8, 180).is_err());
    }

    #[test]
    fn higher_quantizer_is_smaller() {
        let rgb: Vec<u8> = (0..128 * 128 * 3)
            .map(|i: usize| (i.wrapping_mul(2_654_435_761) >> 13) as u8)
            .collect();
        let lo = encode_rgb(&rgb, 128, 128, 60);
        let hi = encode_rgb(&rgb, 128, 128, 240);
        assert!(hi.unwrap().len() < lo.unwrap().len());
    }
}
