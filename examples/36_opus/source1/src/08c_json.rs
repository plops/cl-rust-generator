//! 08c — JSON-Ausgabe: Metadaten-Schema 2 (Frames, Kennzahlen).
//!
//! Aus `08_cli.rs` ausgelagert (600er-Regel): reine Serialisierung,
//! keine Argument- oder Ausgabelogik.

use crate::bitstream::Bitstream;
use crate::fakten::{EncoderFakten, HeadFakten, PcmFakten, TagFakten};
use crate::types::{Analysis, Mode};

/// Modus als JSON-Kennung (klein, stabil).
pub(crate) fn mode_id(mode: Mode) -> &'static str {
    match mode {
        Mode::Silk => "silk",
        Mode::Hybrid => "hybrid",
        Mode::Celt => "celt",
    }
}

/// JSON-String escapen (Anfuehrung, Backslash, Steuerzeichen).
pub(crate) fn esc(s: &str, o: &mut String) {
    o.push('"');
    for c in s.chars() {
        match c {
            '"' => o.push_str("\\\""),
            '\\' => o.push_str("\\\\"),
            '\n' => o.push_str("\\n"),
            '\r' => o.push_str("\\r"),
            '\t' => o.push_str("\\t"),
            c if (c as u32) < 0x20 => {
                o.push_str(&format!("\\u{:04x}", c as u32));
            }
            c => o.push(c),
        }
    }
    o.push('"');
}

/// Komplette Datei als JSON (Schema 2, neu entworfen): Kopf, Tags,
/// Summary, Encoder- und PCM-Fakten plus alle Frames mit Config,
/// Packing, Pad und — wo CELT — Band-Energien/Bits.
#[must_use]
pub fn json(
    a: &Analysis,
    #[allow(clippy::too_many_arguments)] head: &HeadFakten,
    tags: &TagFakten,
    enc: &EncoderFakten,
    pcm: &PcmFakten,
    bs: &Bitstream,
) -> String {
    use std::fmt::Write as _;
    let mut o = String::from("{\"schema\":2,");
    let _ = write!(
        o,
        "\"head\":{{\"channels\":{},\"pre_skip_ms\":{:.3},\"input_rate_hz\":{},\
         \"gain_db\":{:.2},\"mapping\":{},\"streams\":{},\"coupled\":{}}},",
        head.channels,
        head.pre_skip_ms,
        head.input_rate_hz,
        head.gain_db,
        head.mapping,
        head.streams,
        head.coupled,
    );
    o.push_str("\"tags\":{\"vendor\":");
    esc(&tags.vendor, &mut o);
    o.push_str(",\"comments\":[");
    for (i, c) in tags.comments.iter().enumerate() {
        if i > 0 {
            o.push(',');
        }
        esc(c, &mut o);
    }
    let s = &a.summary;
    let _ = write!(
        o,
        "]}},\"summary\":{{\"frames\":{},\"duration_ms\":{:.3},\
         \"audio_bytes\":{},\"total_bytes\":{},\"avg_kbps\":{:.3},\
         \"raw_kbps\":{:.1},\"ratio\":{:.3},\"overhead_pct\":{:.2},\
         \"silk_frac\":{:.4},\"hybrid_frac\":{:.4},\"celt_frac\":{:.4},\
         \"channels\":{},\"split_fallbacks\":{}}},",
        s.frames,
        s.duration_ms,
        s.audio_bytes,
        s.total_bytes,
        s.avg_kbps,
        s.raw_kbps,
        s.ratio,
        s.overhead_pct,
        s.frac_silk,
        s.frac_hybrid,
        s.frac_celt,
        s.channels,
        a.split_fallbacks,
    );
    o.push_str("\"encoder\":{\"runs\":[");
    for (i, r) in enc.runs.iter().enumerate() {
        if i > 0 {
            o.push(',');
        }
        let _ = write!(
            o,
            "{{\"t_start_ms\":{:.3},\"duration_ms\":{:.3},\"mode\":\"{}\"}}",
            r.t_start_ms,
            r.duration_ms,
            mode_id(r.mode),
        );
    }
    o.push_str("],\"bw_frac\":[");
    for (i, v) in enc.bw_frac.iter().enumerate() {
        if i > 0 {
            o.push(',');
        }
        let _ = write!(o, "{v:.4}");
    }
    let _ = write!(
        o,
        "],\"kbps_min\":{:.3},\"kbps_max\":{:.3},\"kbps_p50\":{:.3},\
         \"kbps_p95\":{:.3},\"vbr_index\":{:.4},\"dtx_packets\":{},\
         \"dtx_ms\":{:.3},\"transients_ms\":[",
        enc.kbps_min,
        enc.kbps_max,
        enc.kbps_p50,
        enc.kbps_p95,
        enc.vbr_index,
        enc.dtx_packets,
        enc.dtx_ms,
    );
    for (i, t) in enc.transients_ms.iter().enumerate() {
        if i > 0 {
            o.push(',');
        }
        let _ = write!(o, "{t:.1}");
    }
    o.push_str("]},\"pcm\":{");
    let _ = write!(
        o,
        "\"peak\":[{:.4},{:.4}],\"rms\":[{:.4},{:.4}],\
         \"rms_db\":[{:.2},{:.2}],\"clipped\":{},\"stereo_corr\":",
        pcm.peak[0], pcm.peak[1], pcm.rms[0], pcm.rms[1], pcm.rms_db[0], pcm.rms_db[1], pcm.clipped,
    );
    match pcm.stereo_corr {
        Some(c) => {
            let _ = write!(o, "{c:.4}");
        }
        None => o.push_str("null"),
    }
    o.push_str(",\"silence\":[");
    for (i, (t, d)) in pcm.silence.iter().enumerate() {
        if i > 0 {
            o.push(',');
        }
        let _ = write!(o, "[{t:.1},{d:.1}]");
    }
    let _ = write!(o, "],\"centroid_hz\":{:.1}}},\"frames\":[", pcm.centroid_hz);
    for (i, f) in a.frames.iter().enumerate() {
        if i > 0 {
            o.push(',');
        }
        let _ = write!(
            o,
            "{{\"index\":{},\"t_start_ms\":{:.3},\"duration_ms\":{:.3},\
             \"mode\":\"{}\",\"bw\":\"{}\",\"channels\":{},\"config\":{},\
             \"packing\":{},\"pad_bytes\":{},\"bytes\":{},\"kbps\":{:.3},\"celt\":",
            f.index,
            f.t_start_ms,
            f.duration_ms,
            mode_id(f.mode),
            f.bandwidth.short(),
            f.channels,
            f.config,
            f.packing,
            f.pad_bytes,
            f.bytes,
            f.kbps,
        );
        match bs.packets.get(i).and_then(|c| c.as_ref()) {
            Some(c) => {
                o.push_str("{\"channels\":");
                let _ = write!(o, "{},\"bands_db\":[", c.channels);
                for ch in 0..c.channels {
                    if ch > 0 {
                        o.push(',');
                    }
                    o.push('[');
                    for (b, v) in c.bands_db[ch].iter().enumerate() {
                        if b > 0 {
                            o.push(',');
                        }
                        let _ = write!(o, "{v:.2}");
                    }
                    o.push(']');
                }
                o.push_str("],\"bits\":[");
                for (b, v) in c.bits.iter().enumerate() {
                    if b > 0 {
                        o.push(',');
                    }
                    let _ = write!(o, "{v:.2}");
                }
                o.push_str("]}");
            }
            None => o.push_str("null"),
        }
        o.push_str(",\"silk\":");
        match bs.silk_packets.get(i).and_then(|s| s.as_ref()) {
            Some(s) => {
                let _ = write!(
                    o,
                    "{{\"signal\":{},\"gains_db\":{:.2},\"pitch_hz\":{:.1},\
                     \"header_bits\":{},\"pulse_bits\":{},\"frames\":{}}}",
                    s.signal, s.gains_db, s.pitch_hz, s.header_bits, s.pulse_bits, s.n_frames,
                );
            }
            None => o.push_str("null"),
        }
        o.push('}');
    }
    o.push_str("]}");
    o
}
