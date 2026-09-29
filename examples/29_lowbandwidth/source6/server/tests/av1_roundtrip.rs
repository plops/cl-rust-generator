//! Encoder (rav1e, Server) → Decoder (rav1d, Client): Größe, Farbe und
//! Struktur müssen die Reise überstehen.

use lbw_client::av1::Decoder;
use lbw_server::av1::{Av1Params, encode_rgb};

/// Testbild: vier Farbquadranten plus weißer Balken.
fn test_image(w: usize, h: usize) -> Vec<u8> {
    let mut rgb = Vec::with_capacity(w * h * 3);
    for y in 0..h {
        for x in 0..w {
            let c = match (x < w / 2, y < h / 2) {
                (true, true) => [200, 30, 30],
                (false, true) => [30, 200, 30],
                (true, false) => [30, 30, 200],
                (false, false) => [220, 220, 40],
            };
            let c = if (h / 2 - 4..h / 2 + 4).contains(&y) {
                [255; 3]
            } else {
                c
            };
            rgb.extend_from_slice(&c);
        }
    }
    rgb
}

fn assert_close(rgba: &[u8], w: usize, x: usize, y: usize, want: [u8; 3], tol: u8) {
    let i = (y * w + x) * 4;
    let got = [rgba[i], rgba[i + 1], rgba[i + 2]];
    for (g, e) in got.iter().zip(want) {
        assert!(g.abs_diff(e) <= tol, "({x},{y}): {got:?} vs {want:?}");
    }
}

#[test]
fn roundtrip_preserves_colors_and_size() {
    let mut dec = Decoder::new(1).unwrap();
    for (w, h) in [(640, 640), (128, 48), (16, 16), (200, 90)] {
        let rgb = test_image(w, h);
        let obu = encode_rgb(
            &rgb,
            w,
            h,
            Av1Params {
                quantizer: 120,
                ..Default::default()
            },
        )
        .unwrap();
        let img = dec.decode(&obu).unwrap();
        assert_eq!((img.w, img.h), (w, h));
        assert_close(&img.data, w, w / 4, h / 4 - 1, [200, 30, 30], 24);
        assert_close(&img.data, w, 3 * w / 4, 3 * h / 4 + 1, [220, 220, 40], 24);
        eprintln!("{w}x{h}: {} Byte", obu.len());
    }
}

#[test]
fn decoder_is_reusable_across_sizes() {
    let mut dec = Decoder::new(1).unwrap();
    for _ in 0..3 {
        for (w, h) in [(64, 32), (32, 64)] {
            let obu = encode_rgb(&test_image(w, h), w, h, Av1Params::default()).unwrap();
            let img = dec.decode(&obu).unwrap();
            assert_eq!((img.w, img.h), (w, h));
        }
    }
}
