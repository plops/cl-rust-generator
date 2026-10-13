//! Render-Host-Test: Fixture → PNG, Pixel-Stichproben, 4 Ansichten.

mod common;

use opus_pure::Application;
use opus_vis::bitstream::{Source, Sources};
use opus_vis::{analyze, bitstream, decode, png, render, spectrum};

fn sine_2s() -> (opus_vis::types::Analysis, Sources) {
    let data = common::encode_mono(
        Application::Audio,
        64_000,
        None,
        None,
        100, // 2 s.
        |fr, off| common::sine_block(880.0, fr, off),
    );
    let a = analyze::analyze_reader(&data[..], data.len() as u64).unwrap();
    let pcm = decode::decode_mono(&data).unwrap();
    let cols = spectrum::stft(&pcm.samples);
    let bs = bitstream::analyze_bytes(&data).unwrap();
    (a, Sources::build(cols, bs))
}

#[test]
fn two_second_fixture_renders_valid_png() {
    let (a, src) = sine_2s();
    let img = render::render(&a, &src, Source::Spectrum, 320);
    assert_eq!(img.width, 56 + 320 + 8);
    assert_eq!(img.rgb.len(), img.width as usize * img.height as usize * 3);
    // Modus-Farbe (CELT-amber) kommt im Streifen vor.
    let lay = render::layout(320);
    let amber = render::mode_color(opus_vis::types::Mode::Celt);
    let mut found = false;
    for x in 0..lay.plot_w {
        if img.get(lay.plot_x + x, lay.strip_y) == amber {
            found = true;
            break;
        }
    }
    assert!(found, "kein CELT im Streifen");
    // PNG-Huelle stimmt.
    let bytes = png::encode_rgb(img.width, img.height, &img.rgb);
    assert_eq!(&bytes[..8], &png::MAGIC);
    let w = u32::from_be_bytes(bytes[16..20].try_into().unwrap());
    assert_eq!(w, img.width);
}

#[test]
fn all_four_views_differ_in_waterfall() {
    let (a, src) = sine_2s();
    let lay = render::layout(320);
    let imgs: Vec<_> = Source::ALL
        .iter()
        .map(|v| render::render(&a, &src, *v, 320))
        .collect();
    for img in &imgs {
        assert_eq!((img.width, img.height), (lay.width, lay.height));
    }
    // Wasserfall-Region paarweise: jede Ansicht zeigt anderes Bild.
    let region = |img: &opus_vis::canvas::Image| {
        let mut v = Vec::new();
        for y in 0..256 {
            for x in (0..lay.plot_w).step_by(7) {
                v.extend_from_slice(&img.get(lay.plot_x + x, lay.plot_y + y));
            }
        }
        v
    };
    let regions: Vec<_> = imgs.iter().map(region).collect();
    for (i, r1) in regions.iter().enumerate() {
        for r2 in regions.iter().skip(i + 1) {
            let diff = r1.iter().zip(r2.iter()).filter(|(a, b)| a != b).count();
            assert!(
                diff * 100 / r1.len() > 5,
                "ansichten {i} zu aehnlich ({diff} px)"
            );
        }
    }
}

#[test]
fn divergence_nan_renders_neutral_gray() {
    use opus_vis::canvas::NAN_COLOR;
    // Synthetisch: alles unverfuegbar → Divergenz-NaN → Grau (kein Daten-Farbton).
    let stft = vec![[-60.0; 256]; 4];
    let bs = opus_vis::bitstream::Bitstream {
        spectrum: vec![[-72.0; 256]; 4],
        density: vec![[0.0; 256]; 4],
        available: vec![false; 4],
        transient: vec![false; 4],
        frame_bits: vec![0.0; 4],
        packets: vec![None; 4],
    };
    let src = Sources::build(stft, bs);
    let bytes = sine_bytes();
    let a = analyze::analyze_reader(&bytes[..], 0).unwrap();
    let img = render::render(&a, &src, Source::Divergence, 64);
    let lay = render::layout(64);
    // Modus-Toenung liegt ueber NaN-Grau: Farbe dazwischen, aber weder BG noch Skala.
    let p = img.get(lay.plot_x + 3, lay.plot_y + 100);
    assert_ne!(p, [13, 17, 23]);
    let mut grays = 0;
    for y in 0..256 {
        let q = img.get(lay.plot_x + 3, lay.plot_y + y);
        let spread = q[0].max(q[1]).max(q[2]) - q[0].min(q[1]).min(q[2]);
        if spread < 40 && q[0] > 30 {
            grays += 1;
        }
    }
    assert!(grays > 200, "nur {grays} neutrale Pixel");
    assert_eq!(NAN_COLOR, [74, 76, 84]);
}

fn sine_bytes() -> Vec<u8> {
    common::encode_mono(Application::Audio, 64_000, None, None, 4, |fr, off| {
        common::sine_block(880.0, fr, off)
    })
}
