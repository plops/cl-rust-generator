//! PCM-Decodierung: `.opus`-Bytes → 48-kHz-Mono-float.
//!
//! Ein Decoder pro Datei; `Trim` entfernt Pre-Skip und End-Trim (RFC 7845).

use opus_pure::{MAX_PACKET_SAMPLES, OpusHead, Trim};

use crate::analyze::PacketSet;
use crate::types::Error;

/// Pegel-Statistik je Kanal, vor dem Mono-Mix gemessen.
#[derive(Debug, Clone, PartialEq)]
pub struct ChannelStats {
    /// Spitzenpegel je Kanal (linear, 0..).
    pub peak: [f32; 2],
    /// RMS-Pegel je Kanal (linear, 0..).
    pub rms: [f32; 2],
    /// Samples mit |x| >= 0.999 (beide Kanaele zusammen).
    pub clipped: u64,
    /// Pearson-Korrelation links/rechts (`None` bei Mono).
    pub stereo_corr: Option<f32>,
}

/// Decodiertes Mono-Signal (48 kHz, float, Bereich grob [-1, 1]).
#[derive(Debug, Clone)]
pub struct Decoded {
    /// Mono-Samples (Stereo wird gemittelt).
    pub samples: Vec<f32>,
    /// Kanaele der Datei (1/2).
    pub channels: u8,
    /// Kanal-Statistik vor dem Mix (fuer Fakten/Report).
    pub stats: ChannelStats,
}

/// Decodiert komplette `.opus`-Bytes zu Mono (nur 1–2 Kanaele).
pub fn decode_mono(data: &[u8]) -> Result<Decoded, Error> {
    let ps = crate::analyze::read_packets(data, data.len() as u64)?;
    decode_packets(&ps)
}

/// Decodiert eine einmal gelesene Paketliste zu Mono.
pub fn decode_packets(ps: &PacketSet) -> Result<Decoded, Error> {
    let head: OpusHead = ps.head.clone();
    let channels = usize::from(head.channel_count);
    if channels > 2 {
        return Err(Error::UnsupportedChannels(head.channel_count));
    }
    let mut decoder = head.decoder(48_000)?;
    let mut trim = Trim::new(&head, 48_000, channels)?;
    let mut block = vec![0.0f32; MAX_PACKET_SAMPLES * channels];
    let mut samples = Vec::new();
    let mut peak = [0.0f32; 2];
    let mut sum_sq = [0.0f64; 2];
    let mut count = [0u64; 2];
    let mut clipped = 0u64;
    let (mut sxy, mut sx, mut sy, mut sxx, mut syy) = (0.0f64, 0.0f64, 0.0f64, 0.0f64, 0.0f64);
    let mut pairs = 0u64;
    for packet in &ps.packets {
        let n = decoder.decode(&packet.data, MAX_PACKET_SAMPLES, &mut block)?;
        let kept = trim.keep(packet, &block[..n * channels]);
        for (i, s) in kept.iter().enumerate() {
            let ch = i % channels;
            let v = s.abs();
            peak[ch] = peak[ch].max(v);
            sum_sq[ch] += f64::from(*s) * f64::from(*s);
            count[ch] += 1;
            if v >= 0.999 {
                clipped += 1;
            }
        }
        if channels == 2 {
            for s in kept.as_chunks::<2>().0 {
                let (x, y) = (f64::from(s[0]), f64::from(s[1]));
                sxy += x * y;
                sx += x;
                sy += y;
                sxx += x * x;
                syy += y * y;
                pairs += 1;
            }
            samples.extend(kept.as_chunks::<2>().0.iter().map(|s| 0.5 * (s[0] + s[1])));
        } else {
            samples.extend_from_slice(kept);
        }
    }
    let rms = [rms_of(sum_sq[0], count[0]), rms_of(sum_sq[1], count[1])];
    let stereo_corr = if channels == 2 && pairs > 0 {
        let n = pairs as f64;
        let denom = ((n * sxx - sx * sx) * (n * syy - sy * sy)).sqrt();
        if denom > 0.0 {
            Some(((n * sxy - sx * sy) / denom) as f32)
        } else {
            Some(1.0)
        }
    } else {
        None
    };
    Ok(Decoded {
        samples,
        channels: head.channel_count,
        stats: ChannelStats {
            peak,
            rms,
            clipped,
            stereo_corr,
        },
    })
}

/// RMS aus Quadratsumme und Zaehler (0 bei Leere statt NaN).
fn rms_of(sum_sq: f64, count: u64) -> f32 {
    if count > 0 {
        (sum_sq / count as f64).sqrt() as f32
    } else {
        0.0
    }
}
