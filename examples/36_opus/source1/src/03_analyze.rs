//! Datei-Analyse: Ogg lesen, TOC parsen, Zeitachse + Summary bauen.
//!
//! Streaming: Pakete werden einzeln gelesen und sofort in `FrameMeta`
//! verwandelt; nur die Metadaten liegen am Ende komplett vor.

use std::io::Read;
use std::path::Path;

use opus_pure::OggOpusReader;

use crate::toc;
use crate::types::{Analysis, Error, FrameMeta, Mode, Summary};

/// Analysiert einen Opus-Stream; `total_bytes` = Dateigroesse (Overhead).
pub fn analyze_reader<R: Read>(
    reader: R,
    total_bytes: u64,
) -> Result<Analysis, Error> {
    let mut reader = OggOpusReader::new(reader)?;
    let channels = reader.head().channel_count;
    if channels > 2 {
        return Err(Error::UnsupportedChannels(channels));
    }
    let mut frames = Vec::new();
    let mut t_ms = 0.0;
    while let Some(packet) = reader.read_packet()? {
        let info = toc::parse(&packet.data)?;
        let kbps = packet.data.len() as f64 * 8.0 / info.duration_ms;
        frames.push(FrameMeta {
            index: frames.len(),
            t_start_ms: t_ms,
            duration_ms: info.duration_ms,
            mode: info.mode,
            bandwidth: info.bandwidth,
            channels: info.channels,
            bytes: packet.data.len(),
            kbps,
        });
        t_ms += info.duration_ms;
    }
    Ok(Analysis {
        summary: summarize(&frames, total_bytes, channels),
        frames,
    })
}

/// Analysiert eine Datei (Pfad `-` = stdin ist Sache von `cli`).
pub fn analyze_path(path: &Path) -> Result<Analysis, Error> {
    let file = std::fs::File::open(path)?;
    let total = file.metadata()?.len();
    analyze_reader(file, total)
}

/// Baut die Summary aus Frame-Metadaten (reine Funktion, gut testbar).
fn summarize(frames: &[FrameMeta], total_bytes: u64, channels: u8) -> Summary {
    let duration_ms: f64 = frames.iter().map(|f| f.duration_ms).sum();
    let audio_bytes: u64 = frames.iter().map(|f| f.bytes as u64).sum();
    let avg_kbps = rate(audio_bytes, duration_ms);
    let raw_kbps = 48.0 * f64::from(channels) * 16.0;
    let total_kbps = rate(total_bytes, duration_ms);
    let mut silk = 0.0;
    let mut hybrid = 0.0;
    let mut celt = 0.0;
    for f in frames {
        let slot = match f.mode {
            Mode::Silk => &mut silk,
            Mode::Hybrid => &mut hybrid,
            Mode::Celt => &mut celt,
        };
        *slot += f.duration_ms;
    }
    Summary {
        frames: frames.len(),
        duration_ms,
        audio_bytes,
        total_bytes,
        avg_kbps,
        raw_kbps,
        ratio: div(raw_kbps, total_kbps),
        overhead_pct: if total_bytes > 0 {
            (total_bytes - audio_bytes.min(total_bytes)) as f64 * 100.0
                / total_bytes as f64
        } else {
            0.0
        },
        frac_silk: div(silk, duration_ms),
        frac_hybrid: div(hybrid, duration_ms),
        frac_celt: div(celt, duration_ms),
        channels,
    }
}

/// kbit/s aus Bytes und Dauer (0 bei leerer Eingabe statt NaN).
fn rate(bytes: u64, duration_ms: f64) -> f64 {
    div(bytes as f64 * 8.0, duration_ms)
}

/// Division, die bei Nenner 0 eine 0 liefert (leere Dateien).
fn div(a: f64, b: f64) -> f64 {
    if b > 0.0 { a / b } else { 0.0 }
}

impl Analysis {
    /// Frame, der den Zeitpunkt `t_ms` enthaelt (fuer Modus-Toenung).
    ///
    /// `None` nur bei leerer Analyse; Zeiten hinter dem Ende klemmen auf
    /// den letzten Frame (letzter STFT-Hop ragt oft ueber das Ende).
    #[must_use]
    pub fn frame_at(&self, t_ms: f64) -> Option<&FrameMeta> {
        let frames = &self.frames;
        if frames.is_empty() || t_ms < 0.0 {
            return None;
        }
        let mut lo = 0;
        let mut hi = frames.len();
        while lo + 1 < hi {
            let mid = (lo + hi) / 2;
            if frames[mid].t_start_ms <= t_ms {
                lo = mid;
            } else {
                hi = mid;
            }
        }
        Some(&frames[lo])
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::Bandwidth;

    fn frame(mode: Mode, ms: f64, bytes: usize) -> FrameMeta {
        FrameMeta {
            index: 0,
            t_start_ms: 0.0,
            duration_ms: ms,
            mode,
            bandwidth: Bandwidth::Full,
            channels: 1,
            bytes,
            kbps: bytes as f64 * 8.0 / ms,
        }
    }

    #[test]
    fn summary_math_with_synthetic_frames() {
        // 2 × 20 ms à 160 Bytes = 320 B Audio in 40 ms.
        let frames = [frame(Mode::Silk, 20.0, 160), frame(Mode::Celt, 20.0, 160)];
        let s = summarize(&frames, 400, 1);
        assert_eq!(s.frames, 2);
        assert_eq!(s.duration_ms, 40.0);
        assert_eq!(s.audio_bytes, 320);
        assert_eq!(s.avg_kbps, 64.0); // 320*8/40
        assert_eq!(s.raw_kbps, 768.0); // 48*1*16
        assert_eq!(s.total_bytes, 400);
        assert_eq!(s.ratio, 768.0 / 80.0); // 320+80 Container
        assert_eq!(s.overhead_pct, 20.0);
        assert_eq!((s.frac_silk, s.frac_hybrid, s.frac_celt), (0.5, 0.0, 0.5));
    }

    #[test]
    fn frame_at_finds_mixed_durations() {
        let mut frames = Vec::new();
        // 10-ms-, dann 20-ms-Pakete: Grenzen bei 0/10/30/50.
        for (i, ms) in [10.0, 20.0, 20.0].iter().enumerate() {
            let mut f = frame(Mode::Celt, *ms, 80);
            f.index = i;
            f.t_start_ms = frames.iter().map(|x: &FrameMeta| x.duration_ms).sum();
            frames.push(f);
        }
        let a = Analysis {
            summary: summarize(&frames, 300, 1),
            frames,
        };
        assert_eq!(a.frame_at(0.0).unwrap().index, 0);
        assert_eq!(a.frame_at(9.9).unwrap().index, 0);
        assert_eq!(a.frame_at(10.0).unwrap().index, 1);
        assert_eq!(a.frame_at(29.9).unwrap().index, 1);
        assert_eq!(a.frame_at(30.0).unwrap().index, 2);
        assert_eq!(a.frame_at(999.0).unwrap().index, 2); // Klemmung.
        assert!(a.frame_at(-1.0).is_none());
    }

    #[test]
    fn empty_input_yields_zeros_not_nan() {
        let s = summarize(&[], 0, 1);
        assert_eq!(s.frames, 0);
        assert!(s.avg_kbps == 0.0 && s.ratio == 0.0);
        assert!(s.frac_silk == 0.0 && s.overhead_pct == 0.0);
    }
}
