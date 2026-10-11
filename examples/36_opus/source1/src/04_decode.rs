//! PCM-Decodierung: `.opus`-Bytes → 48-kHz-Mono-float.
//!
//! Ein Decoder pro Datei; `Trim` entfernt Pre-Skip und End-Trim (RFC 7845).

use opus_pure::{MAX_PACKET_SAMPLES, OpusHead, Trim};

use crate::types::Error;

/// Decodiertes Mono-Signal (48 kHz, float, Bereich grob [-1, 1]).
#[derive(Debug, Clone)]
pub struct Decoded {
    /// Mono-Samples (Stereo wird gemittelt).
    pub samples: Vec<f32>,
    /// Kanaele der Datei (1/2).
    pub channels: u8,
}

/// Decodiert komplette `.opus`-Bytes zu Mono (nur 1–2 Kanaele).
pub fn decode_mono(data: &[u8]) -> Result<Decoded, Error> {
    let mut reader = opus_pure::OggOpusReader::new(std::io::Cursor::new(data))?;
    let head: OpusHead = reader.head().clone();
    let channels = usize::from(head.channel_count);
    if channels > 2 {
        return Err(Error::UnsupportedChannels(head.channel_count));
    }
    let mut decoder = head.decoder(48_000)?;
    let mut trim = Trim::new(&head, 48_000, channels)?;
    let mut block = vec![0.0f32; MAX_PACKET_SAMPLES * channels];
    let mut samples = Vec::new();
    while let Some(packet) = reader.read_packet()? {
        let n = decoder.decode(&packet.data, MAX_PACKET_SAMPLES, &mut block)?;
        let kept = trim.keep(&packet, &block[..n * channels]);
        if channels == 2 {
            samples.extend(kept.as_chunks::<2>().0.iter().map(|s| 0.5 * (s[0] + s[1])));
        } else {
            samples.extend_from_slice(kept);
        }
    }
    Ok(Decoded {
        samples,
        channels: head.channel_count,
    })
}
