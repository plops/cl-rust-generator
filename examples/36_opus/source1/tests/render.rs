//! Render-Host-Test: Fixture → PNG, Pixel-Stichproben.

mod common;

use opus_pure::Application;
use opus_vis::{analyze, decode, png, render, spectrum};

#[test]
fn two_second_fixture_renders_valid_png() {
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
    let img = render::render(&a, &cols, 320);
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
