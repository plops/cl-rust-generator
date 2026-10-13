//! Fakten: Datei-, Encoder- und Signal-Auswertung aus reinen Funktionen.
//!
//! Alles, was der Report und das neue JSON brauchen — Kopf und Tags,
//! Moduswechsel, Bitraten-Statistik, Stille, Transienten und
//! PCM-Pegel. Keine neuen Deps; jede Funktion ist einzeln testbar.

use opus_pure::{OpusHead, OpusTags};

use crate::decode::ChannelStats;
use crate::spectrum::{BINS, HOP, RATE, centroid_db};
use crate::types::{Analysis, Bandwidth, Mode};

/// Stille-Schwelle (RMS) und Mindestlaenge eines Stille-Abschnitts.
pub const SILENCE_DB: f32 = -60.0;
/// Siehe `SILENCE_DB`.
pub const SILENCE_MIN_MS: f64 = 200.0;
/// Pakete bis zu dieser Groesse gelten als DTX-artig (fast kein Audio).
pub const DTX_MAX_BYTES: usize = 2;
/// Clipping-Schwelle (linear), wie in `decode` gezaehlt.
pub const CLIP_LEVEL: f32 = 0.999;

/// Kopf-Fakten (alles aus `OpusHead`, nur umgerechnet).
#[derive(Debug, Clone, PartialEq)]
pub struct HeadFakten {
    /// Kanaele (1/2).
    pub channels: u8,
    /// Encoder-Delay in ms (Pre-Skip @48 kHz).
    pub pre_skip_ms: f64,
    /// Eingangs-Samplerate in Hz (0 = unspezifiziert).
    pub input_rate_hz: u32,
    /// Wiedergabe-Gain in dB.
    pub gain_db: f64,
    /// Mapping-Familie (0 = mono/stereo).
    pub mapping: u8,
    /// Opus-Streams im Paket (Familie 0: immer 1).
    pub streams: u8,
    /// Gekoppelte Stereo-Paare.
    pub coupled: u8,
}

/// Tag-Fakten (Vendor + Roh-Kommentare, woertlich aus der Datei).
#[derive(Debug, Clone, PartialEq)]
pub struct TagFakten {
    /// Encoder-Kennung.
    pub vendor: String,
    /// `NAME=Wert`-Eintraege in Datei-Reihenfolge.
    pub comments: Vec<String>,
}

/// Ein Modus-Abschnitt: von `t_start_ms` an `duration_ms` lang `mode`.
#[derive(Debug, Clone, PartialEq)]
pub struct ModeRun {
    /// Start in ms.
    pub t_start_ms: f64,
    /// Dauer in ms.
    pub duration_ms: f64,
    /// Modus des Abschnitts.
    pub mode: Mode,
}

/// Encoder-Fakten aus `Analysis` (+ Transienten aus dem Bitstrom).
#[derive(Debug, Clone, PartialEq)]
pub struct EncoderFakten {
    /// Modus-Abschnitte in Zeitreihenfolge.
    pub runs: Vec<ModeRun>,
    /// Bandbreiten-Anteile (Zeit-%) in `Bandwidth::ALL`-Reihenfolge.
    pub bw_frac: [f64; 5],
    /// Kleinste Paket-Bitrate in kbit/s.
    pub kbps_min: f64,
    /// Groesste Paket-Bitrate in kbit/s.
    pub kbps_max: f64,
    /// Median der Paket-Bitraten in kbit/s.
    pub kbps_p50: f64,
    /// 95-%-Perzentil der Paket-Bitraten in kbit/s.
    pub kbps_p95: f64,
    /// VBR-Index: Streuung / Mittel (0 = CBR-artig).
    pub vbr_index: f64,
    /// DTX-artige Pakete (<= `DTX_MAX_BYTES`).
    pub dtx_packets: usize,
    /// Dauer der DTX-artigen Pakete in ms.
    pub dtx_ms: f64,
    /// Transient-Zeitpunkte in ms (nur CELT-Hops).
    pub transients_ms: Vec<f64>,
}

/// Signal-Fakten aus PCM (+ Kanal-Statistik aus dem Decode).
#[derive(Debug, Clone, PartialEq)]
pub struct PcmFakten {
    /// Spitzenpegel links/rechts (linear).
    pub peak: [f32; 2],
    /// RMS-Pegel links/rechts (linear).
    pub rms: [f32; 2],
    /// RMS-Pegel links/rechts in dBFS.
    pub rms_db: [f32; 2],
    /// Geclippte Samples (|x| >= 0.999, beide Kanaele).
    pub clipped: u64,
    /// Stereo-Korrelation (`None` bei Mono).
    pub stereo_corr: Option<f32>,
    /// Stille-Abschnitte (Start, Dauer) in ms.
    pub silence: Vec<(f64, f64)>,
    /// Mittlerer Spektral-Schwerpunkt in Hz.
    pub centroid_hz: f32,
}

/// Kopf uebernehmen und umrechnen.
#[must_use]
pub fn head_fakten(head: &OpusHead) -> HeadFakten {
    HeadFakten {
        channels: head.channel_count,
        pre_skip_ms: f64::from(head.pre_skip) / 48.0,
        input_rate_hz: head.input_sample_rate,
        gain_db: f64::from(head.output_gain_q8) / 256.0,
        mapping: head.mapping_family,
        streams: head.stream_count,
        coupled: head.coupled_count,
    }
}

/// Tags woertlich uebernehmen (Roh-Strings, keine Normalisierung).
#[must_use]
pub fn tag_fakten(tags: &OpusTags) -> TagFakten {
    TagFakten {
        vendor: tags.vendor.clone(),
        comments: tags.comments.clone(),
    }
}

/// Encoder-Fakten aus Analyse + Transient-Hops des Bitstroms.
#[must_use]
pub fn encoder_fakten(a: &Analysis, transient: &[bool]) -> EncoderFakten {
    let mut runs: Vec<ModeRun> = Vec::new();
    for f in &a.frames {
        if let Some(last) = runs.last_mut()
            && last.mode == f.mode
        {
            last.duration_ms += f.duration_ms;
        } else {
            runs.push(ModeRun {
                t_start_ms: f.t_start_ms,
                duration_ms: f.duration_ms,
                mode: f.mode,
            });
        }
    }
    let dur = a.summary.duration_ms;
    let mut bw_frac = [0.0; 5];
    for f in &a.frames {
        let i = Bandwidth::ALL.iter().position(|b| *b == f.bandwidth);
        if let Some(i) = i
            && dur > 0.0
        {
            bw_frac[i] += f.duration_ms / dur;
        }
    }
    let mut kbps: Vec<f64> = a.frames.iter().map(|f| f.kbps).collect();
    kbps.sort_by(f64::total_cmp);
    let (kbps_min, kbps_max) = match (kbps.first(), kbps.last()) {
        (Some(lo), Some(hi)) => (*lo, *hi),
        _ => (0.0, 0.0),
    };
    let mean = kbps.iter().sum::<f64>() / kbps.len().max(1) as f64;
    let var = kbps.iter().map(|k| (k - mean).powi(2)).sum::<f64>() / kbps.len().max(1) as f64;
    let (mut dtx_packets, mut dtx_ms) = (0usize, 0.0);
    for f in &a.frames {
        if f.bytes <= DTX_MAX_BYTES {
            dtx_packets += 1;
            dtx_ms += f.duration_ms;
        }
    }
    let hop_ms = HOP as f64 / f64::from(RATE) * 1000.0;
    let transients_ms: Vec<f64> = transient
        .iter()
        .enumerate()
        .filter(|(_, t)| **t)
        .map(|(h, _)| h as f64 * hop_ms)
        .collect();
    EncoderFakten {
        runs,
        bw_frac,
        kbps_min,
        kbps_max,
        kbps_p50: percentile(&kbps, 0.50),
        kbps_p95: percentile(&kbps, 0.95),
        vbr_index: if mean > 0.0 { var.sqrt() / mean } else { 0.0 },
        dtx_packets,
        dtx_ms,
        transients_ms,
    }
}

/// Signal-Fakten aus Mono-Samples, STFT-Spalten und Kanal-Statistik.
#[must_use]
pub fn pcm_fakten(samples: &[f32], cols: &[[f32; BINS]], stats: &ChannelStats) -> PcmFakten {
    PcmFakten {
        peak: stats.peak,
        rms: stats.rms,
        rms_db: [db_of(stats.rms[0]), db_of(stats.rms[1])],
        clipped: stats.clipped,
        stereo_corr: stats.stereo_corr,
        silence: silence_spans(samples),
        centroid_hz: mean_centroid(cols),
    }
}

/// SILK-Stimm-Fakten aus den Hop-Spalten (Report + Einordnung).
#[derive(Debug, Clone, PartialEq)]
pub struct SilkFakten {
    /// SILK-Zeit in ms (verfuegbare Hops × 20 ms).
    pub silk_ms: f64,
    /// Stimmhafte Zeit in ms.
    pub voiced_ms: f64,
    /// Stimmlose Zeit in ms.
    pub unvoiced_ms: f64,
    /// Inaktive Zeit in ms.
    pub inactive_ms: f64,
    /// Median der Tonhoehe ueber stimmhafte Hops in Hz (0 ohne).
    pub pitch_med_hz: f32,
    /// Spanne der Tonhoehe in Hz (min, max; 0 ohne).
    pub pitch_min_hz: f32,
    /// Siehe `pitch_min_hz`.
    pub pitch_max_hz: f32,
    /// Gain-Spanne ueber SILK-Hops in dB (min, max).
    pub gain_min_db: f32,
    /// Siehe `gain_min_db`.
    pub gain_max_db: f32,
}

/// Stimmanteile, Tonhoehen-Statistik und Gain-Dynamik aus `Bitstream`.
#[must_use]
pub fn silk_fakten(bs: &crate::bitstream::Bitstream) -> SilkFakten {
    use crate::spectrum::HOP_MS;
    let mut voiced = Vec::new();
    let mut gains = Vec::new();
    let (mut v, mut u, mut ina) = (0u32, 0u32, 0u32);
    for i in 0..bs.silk_available.len() {
        if !bs.silk_available[i] {
            continue;
        }
        let (sig, pitch, gain) = (bs.silk_signal[i], bs.silk_pitch[i], bs.silk_gain[i]);
        match sig {
            2 => {
                v += 1;
                if pitch > 0.0 {
                    voiced.push(pitch);
                }
            }
            1 => u += 1,
            _ => ina += 1,
        }
        gains.push(gain);
    }
    voiced.sort_by(|a, b| a.total_cmp(b));
    gains.sort_by(|a, b| a.total_cmp(b));
    let n = v + u + ina;
    SilkFakten {
        silk_ms: f64::from(n) * HOP_MS,
        voiced_ms: f64::from(v) * HOP_MS,
        unvoiced_ms: f64::from(u) * HOP_MS,
        inactive_ms: f64::from(ina) * HOP_MS,
        pitch_med_hz: voiced.get(voiced.len() / 2).copied().unwrap_or(0.0),
        pitch_min_hz: voiced.first().copied().unwrap_or(0.0),
        pitch_max_hz: voiced.last().copied().unwrap_or(0.0),
        gain_min_db: gains.first().copied().unwrap_or(0.0),
        gain_max_db: gains.last().copied().unwrap_or(0.0),
    }
}

/// Nearest-Rank-Perzentil ueber sortierte Werte (0 bei Leere).
fn percentile(sorted: &[f64], q: f64) -> f64 {
    if sorted.is_empty() {
        return 0.0;
    }
    let rank = (q * sorted.len() as f64).ceil() as usize;
    sorted[rank.clamp(1, sorted.len()) - 1]
}

/// Linear-Pegel in dBFS (-inf bei 0 gibt es nicht, Boden stattdessen).
fn db_of(v: f32) -> f32 {
    if v > 0.0 { 20.0 * v.log10() } else { -120.0 }
}

/// Stille-Abschnitte: Fenster von 200 ms, RMS < `SILENCE_DB`, lueckenlos.
fn silence_spans(samples: &[f32]) -> Vec<(f64, f64)> {
    let win = (RATE as usize / 5).max(1); // 200 ms.
    let thresh = 10.0f32.powf(SILENCE_DB / 20.0);
    let hop_ms = 1000.0 / f64::from(RATE);
    let mut out = Vec::new();
    let mut start: Option<usize> = None;
    let mut s = 0;
    while s < samples.len() {
        let e = (s + win).min(samples.len());
        let n = (e - s) as f32;
        let rms = (samples[s..e].iter().map(|x| x * x).sum::<f32>() / n).sqrt();
        let quiet = rms < thresh && e - s == win;
        if quiet && start.is_none() {
            start = Some(s);
        }
        if !quiet && let Some(st) = start.take() {
            let dur_ms = (s - st) as f64 * hop_ms;
            if dur_ms >= SILENCE_MIN_MS {
                out.push((st as f64 * hop_ms, dur_ms));
            }
        }
        s += win;
    }
    if let Some(st) = start {
        let dur_ms = (samples.len() - st) as f64 * hop_ms;
        if dur_ms >= SILENCE_MIN_MS {
            out.push((st as f64 * hop_ms, dur_ms));
        }
    }
    out
}

/// Mittlerer Schwerpunkt ueber alle Spalten (0 bei Leere).
fn mean_centroid(cols: &[[f32; BINS]]) -> f32 {
    if cols.is_empty() {
        return 0.0;
    }
    cols.iter().map(centroid_db).sum::<f32>() / cols.len() as f32
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{FrameMeta, Summary};

    fn frame(mode: Mode, t: f64, ms: f64, bytes: usize, kbps: f64) -> FrameMeta {
        FrameMeta {
            index: 0,
            t_start_ms: t,
            duration_ms: ms,
            mode,
            bandwidth: Bandwidth::Full,
            channels: 1,
            bytes,
            kbps,
            config: 31,
            packing: 0,
            pad_bytes: 0,
        }
    }

    fn analysis(frames: Vec<FrameMeta>) -> Analysis {
        let dur: f64 = frames.iter().map(|f| f.duration_ms).sum();
        Analysis {
            summary: Summary {
                frames: frames.len(),
                duration_ms: dur,
                audio_bytes: 0,
                total_bytes: 0,
                avg_kbps: 0.0,
                raw_kbps: 768.0,
                ratio: 0.0,
                overhead_pct: 0.0,
                frac_silk: 0.0,
                frac_hybrid: 0.0,
                frac_celt: 0.0,
                channels: 1,
            },
            frames,
            split_fallbacks: 0,
        }
    }

    #[test]
    fn runs_merge_same_mode_and_split_on_change() {
        let a = analysis(vec![
            frame(Mode::Silk, 0.0, 20.0, 60, 24.0),
            frame(Mode::Silk, 20.0, 20.0, 60, 24.0),
            frame(Mode::Celt, 40.0, 20.0, 180, 72.0),
        ]);
        let e = encoder_fakten(&a, &[]);
        assert_eq!(e.runs.len(), 2);
        assert_eq!(e.runs[0].duration_ms, 40.0);
        assert_eq!(e.runs[1].mode, Mode::Celt);
        assert_eq!(e.runs[1].t_start_ms, 40.0);
    }

    #[test]
    fn percentiles_and_vbr_behave() {
        let a = analysis(
            (1..=100)
                .map(|i| frame(Mode::Celt, 0.0, 20.0, 100, f64::from(i)))
                .collect(),
        );
        let e = encoder_fakten(&a, &[]);
        assert_eq!((e.kbps_min, e.kbps_max), (1.0, 100.0));
        assert_eq!(e.kbps_p50, 50.0);
        assert_eq!(e.kbps_p95, 95.0);
        assert!(e.vbr_index > 0.4, "vbr {}", e.vbr_index);
        // Konstant = CBR-artig.
        let flat = analysis(vec![frame(Mode::Celt, 0.0, 20.0, 100, 64.0); 10]);
        assert_eq!(encoder_fakten(&flat, &[]).vbr_index, 0.0);
    }

    #[test]
    fn bw_shares_sum_to_one_and_dtx_counts() {
        let mut f1 = frame(Mode::Silk, 0.0, 30.0, 60, 16.0);
        f1.bandwidth = Bandwidth::Narrow;
        let mut f2 = frame(Mode::Celt, 30.0, 10.0, 180, 144.0);
        f2.bandwidth = Bandwidth::Full;
        let dtx = frame(Mode::Silk, 40.0, 20.0, 1, 0.4);
        let a = analysis(vec![f1, f2, dtx]);
        let e = encoder_fakten(&a, &[false; 3]);
        let sum: f64 = e.bw_frac.iter().sum();
        assert!((sum - 1.0).abs() < 1e-9, "summe {sum}");
        assert_eq!((e.dtx_packets, e.dtx_ms), (1, 20.0));
        assert!(e.transients_ms.is_empty());
    }

    #[test]
    fn transients_map_hop_to_ms() {
        let a = analysis(vec![frame(Mode::Celt, 0.0, 60.0, 180, 24.0)]);
        let e = encoder_fakten(&a, &[false, true, false]);
        assert_eq!(e.transients_ms, vec![20.0]);
    }

    #[test]
    fn silence_spans_need_two_windows() {
        // 600 ms Stille: drei 200-ms-Fenster → ein Abschnitt.
        let stats = ChannelStats {
            peak: [0.0; 2],
            rms: [0.0; 2],
            clipped: 0,
            stereo_corr: None,
        };
        let p = pcm_fakten(&vec![0.0; 28_800], &[], &stats);
        assert_eq!(p.silence.len(), 1);
        assert!((p.silence[0].1 - 600.0).abs() < 1.0, "{:?}", p.silence);
        assert_eq!(p.rms_db, [-120.0, -120.0]);
        assert_eq!(p.centroid_hz, 0.0);
        // 100 ms Stille: zu kurz → kein Abschnitt.
        let p = pcm_fakten(&vec![0.0; 4800], &[], &stats);
        assert!(p.silence.is_empty());
    }

    #[test]
    fn empty_analysis_yields_zeros() {
        let e = encoder_fakten(&analysis(vec![]), &[]);
        assert!(e.runs.is_empty() && e.kbps_p50 == 0.0 && e.vbr_index == 0.0);
        assert_eq!(e.bw_frac, [0.0; 5]);
    }

    #[test]
    fn head_gain_converts_q8_to_db() {
        let head = OpusHead {
            channel_count: 2,
            pre_skip: 312,
            input_sample_rate: 48_000,
            output_gain_q8: 256,
            mapping_family: 0,
            stream_count: 1,
            coupled_count: 1,
            channel_mapping: vec![],
        };
        let h = head_fakten(&head);
        assert_eq!((h.pre_skip_ms, h.gain_db), (6.5, 1.0));
        assert_eq!(h.input_rate_hz, 48_000);
    }
}

#[cfg(test)]
mod silk_tests {
    use super::*;
    use crate::bitstream::Bitstream;
    use crate::silk::SilkSummary;
    use crate::spectrum::BINS;

    fn bitstream_mit_silk(signals: &[i8]) -> Bitstream {
        let n = signals.len();
        Bitstream {
            spectrum: vec![[0.0; BINS]; n],
            density: vec![[0.0; BINS]; n],
            available: vec![false; n],
            transient: vec![false; n],
            frame_bits: vec![0.0; n],
            packets: vec![None; n],
            silk_packets: vec![None; n],
            silk_available: vec![true; n],
            silk_signal: signals.to_vec(),
            silk_pitch: signals
                .iter()
                .map(|&s| if s == 2 { 140.0 } else { 0.0 })
                .collect(),
            silk_gain: vec![30.0; n],
        }
    }

    #[test]
    fn silk_fakten_zaehlt_stimmanteile() {
        let bs = bitstream_mit_silk(&[2, 2, 1, 0]);
        let f = silk_fakten(&bs);
        assert_eq!(
            (f.voiced_ms, f.unvoiced_ms, f.inactive_ms),
            (40.0, 20.0, 20.0)
        );
        assert_eq!(f.silk_ms, 80.0);
        assert_eq!(f.pitch_med_hz, 140.0);
        assert_eq!((f.gain_min_db, f.gain_max_db), (30.0, 30.0));
        let leer = bitstream_mit_silk(&[]);
        let l = silk_fakten(&leer);
        assert_eq!((l.silk_ms, l.pitch_med_hz), (0.0, 0.0));
    }

    #[test]
    fn silk_summary_typ_name() {
        let _ = SilkSummary {
            signal: 2,
            gains_db: 30.0,
            pitch_hz: 140.0,
            header_bits: 40,
            pulse_bits: 300,
            n_frames: 1,
            nlsf_q15: [0; 16],
            order: 16,
            fs_khz: 16,
        };
    }
}
