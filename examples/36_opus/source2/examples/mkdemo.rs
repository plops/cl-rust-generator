//! Demo-Datei bauen: 3 s Sprache (SILK) + 3 s Musik (CELT).
//!
//! ```
//! cargo run --release --example mkdemo -- /tmp/demo.opus
//! ```

use opus_pure::{
    Application, Bandwidth, MAX_PACKET_BYTES, OggOpusWriter, OpusEncoder, OpusHead, Signal,
};

fn speech(frame: usize, offset: usize) -> Vec<f32> {
    let mut out = vec![0.0f32; frame];
    for (i, s) in out.iter_mut().enumerate() {
        let t = (offset + i) as f32 / 48_000.0;
        let trem = 0.6 + 0.4 * (2.0 * std::f32::consts::PI * 5.0 * t).sin();
        *s = trem
            * 0.4
            * ((2.0 * std::f32::consts::PI * 130.0 * t).sin()
                + 0.3 * (2.0 * std::f32::consts::PI * 260.0 * t).sin()
                + 0.15 * (2.0 * std::f32::consts::PI * 900.0 * t).sin());
    }
    out
}

fn music(frame: usize, offset: usize) -> Vec<f32> {
    (0..frame)
        .map(|i| {
            // Musik-Segment laeuft absolut bei 3–6 s, Sweep bei 0–3 s.
            let t = (offset + i) as f32 / 48_000.0 - 3.0;
            // Sweep 300→3000 Hz ueber 3 s + Oktave (Phase = Integral).
            let ph = 2.0 * std::f32::consts::PI * 300.0 * 3.0 * ((10.0f32).powf(t / 3.0) - 1.0)
                / (10.0f32).ln();
            0.3 * ph.sin() + 0.15 * (2.0 * ph).sin()
        })
        .collect()
}

fn encode_seg(
    enc: &mut OpusEncoder,
    writer: &mut OggOpusWriter<std::fs::File>,
    raw: &mut [u8],
    frames: usize,
    base: usize,
    block: fn(usize, usize) -> Vec<f32>,
) {
    for f in 0..frames {
        let pcm = block(960, base + f * 960);
        let n = enc.encode(&pcm, 960, raw).unwrap();
        writer.write_packet(&raw[..n]).unwrap();
    }
}

fn main() {
    let path = std::env::args().nth(1).expect("Zielpfad fehlt");
    let mut voip = OpusEncoder::new(48_000, 1, Application::Voip).unwrap();
    voip.bitrate_bps = 24_000;
    voip.force_bandwidth = Some(Bandwidth::Wideband);
    voip.signal_type = Some(Signal::Voice);
    let mut audio = OpusEncoder::new(48_000, 1, Application::Audio).unwrap();
    audio.bitrate_bps = 96_000;
    let head = OpusHead::for_encoder(&voip, 48_000);
    let file = std::fs::File::create(&path).unwrap();
    let mut writer = OggOpusWriter::new(file, head).unwrap();
    let mut raw = vec![0u8; MAX_PACKET_BYTES];
    encode_seg(&mut voip, &mut writer, &mut raw, 150, 0, speech);
    encode_seg(&mut audio, &mut writer, &mut raw, 150, 150 * 960, music);
    writer.finish().unwrap();
    eprintln!("demo: {path}");
}
