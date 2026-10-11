//! Fixture-Generator: `.opus`-Bytes via `opus-pure`-Encoder erzeugen.
//!
//! Damit testen wir ohne externe Dateien; die einzige committete Datei
//! (`data/ref.opus`) stammt aus `opusenc` und prueft Fremd-Kompatibilitaet.

use opus_pure::{
    Application, Bandwidth, MAX_PACKET_BYTES, OggOpusWriter, OpusEncoder,
    OpusHead, Signal,
};

/// Sprach-aehnliches Signal: 130 Hz + Obertoene, 5-Hz-Tremolo.
pub fn speech_block(frame: usize, offset: usize) -> Vec<f32> {
    let mut out = vec![0.0f32; frame];
    for (i, s) in out.iter_mut().enumerate() {
        let t = (offset + i) as f32 / 48_000.0;
        let trem = 0.6 + 0.4 * (2.0 * std::f32::consts::PI * 5.0 * t).sin();
        *s = trem * 0.4
            * ((2.0 * std::f32::consts::PI * 130.0 * t).sin()
                + 0.3 * (2.0 * std::f32::consts::PI * 260.0 * t).sin()
                + 0.15 * (2.0 * std::f32::consts::PI * 900.0 * t).sin());
    }
    out
}

/// Sinus mit Frequenz `freq` (Musik-Test).
pub fn sine_block(freq: f32, frame: usize, offset: usize) -> Vec<f32> {
    (0..frame)
        .map(|i| {
            let t = (offset + i) as f32 / 48_000.0;
            0.5 * (2.0 * std::f32::consts::PI * freq * t).sin()
        })
        .collect()
}

/// Encodiert Bloecke in eine `.opus`-Datei (mono, 20-ms-Frames).
pub fn encode_mono(
    app: Application,
    bitrate: i32,
    force_bw: Option<Bandwidth>,
    signal: Option<Signal>,
    frames: usize,
    mut block: impl FnMut(usize, usize) -> Vec<f32>,
) -> Vec<u8> {
    let mut enc = OpusEncoder::new(48_000, 1, app).unwrap();
    enc.bitrate_bps = bitrate;
    enc.force_bandwidth = force_bw;
    enc.signal_type = signal;
    let head = OpusHead::for_encoder(&enc, 48_000);
    let mut writer = OggOpusWriter::new(Vec::new(), head).unwrap();
    let mut raw = vec![0u8; MAX_PACKET_BYTES];
    for f in 0..frames {
        let pcm = block(960, f * 960);
        let n = enc.encode(&pcm, 960, &mut raw).unwrap();
        writer.write_packet(&raw[..n]).unwrap();
    }
    writer.finish().unwrap()
}

/// Erste TOC-Config je Paket (zur Modus-Kontrolle in Tests).
pub fn configs(data: &[u8]) -> Vec<u8> {
    let mut reader =
        opus_pure::OggOpusReader::new(std::io::Cursor::new(data)).unwrap();
    let mut out = Vec::new();
    while let Some(p) = reader.read_packet().unwrap() {
        out.push(p.data[0] >> 3);
    }
    out
}
