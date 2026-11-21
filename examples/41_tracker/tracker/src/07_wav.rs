//! Offline rendering to WAV and loading WAV files for sample instruments.

use crate::engine::Engine;
use crate::types::Song;
use anyhow::{Context, Result};
use std::path::Path;
use std::sync::Arc;

/// Render `seconds` of `song` (played from the start of the order list).
pub fn render(song: Song, seconds: f32, sample_rate: u32) -> Vec<f32> {
    let mut e = Engine::new(Box::new(song), sample_rate as f32);
    e.play(0, None);
    let mut out = vec![0.0; (seconds * sample_rate as f32) as usize * 2];
    e.render(&mut out);
    out
}

pub fn write_wav(path: &Path, stereo: &[f32], sample_rate: u32) -> Result<()> {
    let spec = hound::WavSpec {
        channels: 2,
        sample_rate,
        bits_per_sample: 16,
        sample_format: hound::SampleFormat::Int,
    };
    let mut w = hound::WavWriter::create(path, spec).with_context(|| format!("{path:?}"))?;
    for &x in stereo {
        w.write_sample((x.clamp(-1.0, 1.0) * i16::MAX as f32) as i16)?;
    }
    w.finalize()?;
    Ok(())
}

/// Load any PCM WAV as mono f32 (channels averaged). Returns (data, sample rate).
pub fn load_wav(path: &Path) -> Result<(Arc<[f32]>, f32)> {
    let mut r = hound::WavReader::open(path).with_context(|| format!("{path:?}"))?;
    let spec = r.spec();
    let ch = spec.channels.max(1) as usize;
    let raw: Vec<f32> = match spec.sample_format {
        hound::SampleFormat::Float => r.samples::<f32>().collect::<Result<_, _>>()?,
        hound::SampleFormat::Int => {
            let scale = 1.0 / (1u64 << (spec.bits_per_sample - 1)) as f32;
            r.samples::<i32>()
                .map(|s| s.map(|v| v as f32 * scale))
                .collect::<Result<_, _>>()?
        }
    };
    let mono: Vec<f32> = raw
        .chunks(ch)
        .map(|f| f.iter().sum::<f32>() / ch as f32)
        .collect();
    Ok((Arc::from(mono), spec.sample_rate as f32))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn wav_roundtrip() {
        let dir = std::env::temp_dir().join(format!("dnb-wav-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let p = dir.join("t.wav");
        let data: Vec<f32> = (0..2000)
            .map(|i| ((i / 2) as f32 * 0.01).sin() * 0.5)
            .collect();
        write_wav(&p, &data, 44100).unwrap();
        let (mono, sr) = load_wav(&p).unwrap();
        assert_eq!(sr, 44100.0);
        assert_eq!(mono.len(), 1000);
        for (i, &m) in mono.iter().enumerate() {
            assert!((m - data[i * 2]).abs() < 1e-3);
        }
        std::fs::remove_dir_all(dir).ok();
    }
}
