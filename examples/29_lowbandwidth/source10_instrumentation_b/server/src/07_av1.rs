//! `05_av1` — AV1-Still-Picture-Encoder (rav1e) für einzelne Bildkacheln.
//!
//! Jede Kachel ist ein eigenständiges Intra-Bild (wie in AVIF, nur ohne
//! Container — spart ~300 Byte je Kachel). Ausgabe: rohe OBUs, direkt
//! dekodierbar mit dav1d/rav1d. Aus `source6/server/09_av1.rs` übernommen,
//! ohne `asm`-Feature (kein `nasm` nötig).

use image::RgbImage;
use lbw_common::Rect;
use lbw_common::yuv::rgb_to_yuv420;
use rav1e::color::{ChromaSampling, PixelRange};
use rav1e::prelude::*;

use crate::tiles::{crop_rgb, dirty_in};

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

/// Kodierte Teil-Kacheln: (Box, OBUs).
pub type Parts = Vec<(Rect, Vec<u8>)>;

/// Max. Zerlegungstiefe (Sicherung gegen Endlos-Rekursion).
const MAX_DEPTH: u32 = 4;

/// Kodiert die Änderung `r` (`prev` → `cur`) als eine oder mehrere
/// AV1-Kacheln, jede höchstens `max_bytes` groß (`0` = unbegrenzt).
///
/// Review B1: Eine 42-kB-Kachel belegt einen 6-kB/s-Kanal 7 s am Stück
/// (Head-of-Line). Darum: erst die ganze Box kodieren — passt sie, bleibt
/// es bei *einer* Kachel (ein Header, wie source10). Sonst proportional
/// zerlegen: `k = ⌈Bytes / max⌉` waagrechte Bänder (zu flach → senkrechte
/// Spalten), jedes Band auf seine eigene Änderung verkleinert (unveränderte
/// Bänder fallen weg) und parallel neu kodiert; zu große Bänder rekursiv.
/// Ergebnis: (Box, OBUs) in Bildreihenfolge.
pub fn encode_capped(
    prev: Option<&RgbImage>,
    cur: &RgbImage,
    r: Rect,
    quantizer: usize,
    max_bytes: usize,
) -> Result<Parts, String> {
    capped(prev, cur, r, quantizer, max_bytes, 0)
}

fn capped(
    prev: Option<&RgbImage>,
    cur: &RgbImage,
    r: Rect,
    q: usize,
    max: usize,
    depth: u32,
) -> Result<Parts, String> {
    let data = encode_rgb(&crop_rgb(cur, r), r.w as usize, r.h as usize, q)?;
    let t = MIN_TILE as u16;
    let can_split = r.h >= 2 * t || r.w >= 2 * t;
    if max == 0 || data.len() <= max || !can_split || depth >= MAX_DEPTH {
        return Ok(vec![(r, data)]);
    }
    let k = data.len().div_ceil(max) as u16;
    let parts = split(r, k.max(2));
    // Teile parallel kodieren (rav1e-Kontexte sind unabhängig).
    let results: Vec<Result<Parts, String>> = std::thread::scope(|sc| {
        let hs: Vec<_> = parts
            .into_iter()
            .filter_map(|p| dirty_in(prev, cur, p))
            .map(|p| sc.spawn(move || capped(prev, cur, p, q, max, depth + 1)))
            .collect();
        hs.into_iter()
            .map(|h| h.join().unwrap_or_else(|_| Err("Encoder-Thread".into())))
            .collect()
    });
    let mut out = Vec::new();
    for r in results {
        out.extend(r?);
    }
    Ok(out)
}

/// Zerlegt `r` in bis zu `k` gleich hohe Bänder (gerade, ≥ [`MIN_TILE`]);
/// ist `r` zu flach, in Spalten.
fn split(r: Rect, k: u16) -> Vec<Rect> {
    let t = MIN_TILE as u16;
    let by_rows = r.h >= 2 * t;
    let len = if by_rows { r.h } else { r.w };
    let k = k.min(len / t).max(1);
    let mut step = len.div_ceil(k);
    step += step & 1;
    let step = step.max(t);
    let mut out = Vec::new();
    let mut o = 0;
    while o < len {
        let n = step.min(len - o);
        out.push(if by_rows {
            Rect::new(r.x, r.y + o, r.w, n)
        } else {
            Rect::new(r.x + o, r.y, n, r.h)
        });
        o += n;
    }
    out
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

    fn noise(w: u32, h: u32, seed: u32) -> RgbImage {
        let mut x = seed.max(1);
        RgbImage::from_fn(w, h, |_, _| {
            x ^= x << 13;
            x ^= x >> 17;
            x ^= x << 5;
            image::Rgb([x as u8, (x >> 8) as u8, (x >> 16) as u8])
        })
    }

    #[test]
    fn split_covers_the_box_with_even_parts() {
        let parts = split(Rect::new(0, 0, 1280, 720), 7);
        assert_eq!(parts.iter().map(|p| u32::from(p.h)).sum::<u32>(), 720);
        assert!(
            parts
                .iter()
                .all(|p| p.h % 2 == 0 && p.h >= 16 && p.w == 1280)
        );
        assert!(parts.len() <= 7);
        // Flache Box → Spalten.
        let parts = split(Rect::new(0, 0, 640, 16), 4);
        assert_eq!(parts.len(), 4);
        assert!(parts.iter().all(|p| p.h == 16 && p.w == 160));
    }

    #[test]
    fn small_change_stays_one_tile() {
        let a = noise(256, 128, 1);
        let mut b = a.clone();
        b.put_pixel(10, 10, image::Rgb([0; 3]));
        let r = crate::tiles::dirty_bbox(Some(&a), &b).unwrap();
        let parts = encode_capped(Some(&a), &b, r, 180, 2400).unwrap();
        assert_eq!(parts.len(), 1);
        assert_eq!(parts[0].0, r);
    }

    #[test]
    fn big_noise_frame_is_capped_and_decodable_cover() {
        // Rauschen 640×256: monolithisch weit über dem Deckel.
        let cur = noise(640, 256, 7);
        let full = Rect::new(0, 0, 640, 256);
        let mono = encode_rgb(&crop_rgb(&cur, full), 640, 256, 180).unwrap();
        let max = 2400;
        assert!(mono.len() > 3 * max, "{}", mono.len());
        let parts = encode_capped(None, &cur, full, 180, max).unwrap();
        assert!(parts.len() > 1);
        // Jeder Teil unter dem Deckel (oder nicht mehr teilbar).
        for (r, d) in &parts {
            assert!(
                d.len() <= max || (r.w < 32 && r.h < 32),
                "{r:?}: {}",
                d.len()
            );
        }
        // Lückenlose Abdeckung der Box.
        let mut hit = vec![false; 640 * 256];
        for (r, _) in &parts {
            for y in r.y..r.y2() {
                for x in r.x..r.x2() {
                    hit[usize::from(y) * 640 + usize::from(x)] = true;
                }
            }
        }
        assert!(hit.iter().all(|h| *h));
        // Ohne Deckel: genau eine Kachel.
        assert_eq!(encode_capped(None, &cur, full, 180, 0).unwrap().len(), 1);
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
