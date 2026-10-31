//! 10c — JSON-Ausgabe, Schema 3: Kopf, Tags, Fakten, Pakete bis zum Bit.
//!
//! Neu gegenueber Schema 2: je Paket `final_range` (zum Gegenpruefen mit
//! jedem Decoder), `budget_bits` (wohin jedes Bit ging), CELT-Pulse je
//! Band und Prefilter, SILK mit FEC/Mid-only; auf Datei-Ebene `bits`
//! (Budget-Anteile, Redundanz, Orakel). Der Writer baut Objekte aus
//! `(Schluessel, Wert)`-Listen — keine Hand-Komma-Logik mehr.

use crate::fakten::{BitFakten, EncoderFakten, Fakten, HeadFakten, PcmFakten};
use crate::model::{Model, PacketInfo};
use crate::types::{Analysis, Mode};

/// Modus als JSON-Kennung (klein, stabil).
pub(crate) fn mode_id(mode: Mode) -> &'static str {
    match mode {
        Mode::Silk => "silk",
        Mode::Hybrid => "hybrid",
        Mode::Celt => "celt",
    }
}

/// JSON-String (Anfuehrung, Backslash, Steuerzeichen escapen).
pub(crate) fn esc(s: &str) -> String {
    let mut o = String::with_capacity(s.len() + 2);
    o.push('"');
    for c in s.chars() {
        match c {
            '"' => o.push_str("\\\""),
            '\\' => o.push_str("\\\\"),
            '\n' => o.push_str("\\n"),
            '\r' => o.push_str("\\r"),
            '\t' => o.push_str("\\t"),
            c if (c as u32) < 0x20 => o.push_str(&format!("\\u{:04x}", c as u32)),
            c => o.push(c),
        }
    }
    o.push('"');
    o
}

/// Zahl mit `prec` Nachkommastellen; nicht-endlich → `null`.
fn num(v: f64, prec: usize) -> String {
    if v.is_finite() {
        format!("{v:.prec$}")
    } else {
        "null".into()
    }
}

/// Objekt aus Schluessel/Wert-Paaren (Werte sind fertiges JSON).
fn obj(fields: &[(&str, String)]) -> String {
    let body: Vec<String> = fields
        .iter()
        .map(|(k, v)| format!("{}:{v}", esc(k)))
        .collect();
    format!("{{{}}}", body.join(","))
}

/// Array aus fertigen JSON-Werten.
fn arr(items: impl IntoIterator<Item = String>) -> String {
    format!("[{}]", items.into_iter().collect::<Vec<_>>().join(","))
}

fn opt(v: Option<String>) -> String {
    v.unwrap_or_else(|| "null".into())
}

fn head_json(h: &HeadFakten) -> String {
    obj(&[
        ("channels", h.channels.to_string()),
        ("pre_skip_ms", num(h.pre_skip_ms, 3)),
        ("input_rate_hz", h.input_rate_hz.to_string()),
        ("gain_db", num(h.gain_db, 2)),
        ("mapping", h.mapping.to_string()),
        ("streams", h.streams.to_string()),
        ("coupled", h.coupled.to_string()),
    ])
}

fn summary_json(a: &Analysis) -> String {
    let s = &a.summary;
    obj(&[
        ("frames", s.frames.to_string()),
        ("duration_ms", num(s.duration_ms, 3)),
        ("audio_bytes", s.audio_bytes.to_string()),
        ("total_bytes", s.total_bytes.to_string()),
        ("avg_kbps", num(s.avg_kbps, 3)),
        ("raw_kbps", num(s.raw_kbps, 1)),
        ("ratio", num(s.ratio, 3)),
        ("overhead_pct", num(s.overhead_pct, 2)),
        ("silk_frac", num(s.frac_silk, 4)),
        ("hybrid_frac", num(s.frac_hybrid, 4)),
        ("celt_frac", num(s.frac_celt, 4)),
        ("channels", s.channels.to_string()),
        ("split_fallbacks", a.split_fallbacks.to_string()),
    ])
}

fn encoder_json(e: &EncoderFakten) -> String {
    obj(&[
        (
            "runs",
            arr(e.runs.iter().map(|r| {
                obj(&[
                    ("t_start_ms", num(r.t_start_ms, 3)),
                    ("duration_ms", num(r.duration_ms, 3)),
                    ("mode", esc(mode_id(r.mode))),
                ])
            })),
        ),
        ("bw_frac", arr(e.bw_frac.iter().map(|v| num(*v, 4)))),
        ("kbps_min", num(e.kbps_min, 3)),
        ("kbps_max", num(e.kbps_max, 3)),
        ("kbps_p50", num(e.kbps_p50, 3)),
        ("kbps_p95", num(e.kbps_p95, 3)),
        ("vbr_index", num(e.vbr_index, 4)),
        ("dtx_packets", e.dtx_packets.to_string()),
        ("dtx_ms", num(e.dtx_ms, 3)),
        (
            "transients_ms",
            arr(e.transients_ms.iter().map(|t| num(*t, 1))),
        ),
    ])
}

fn pcm_json(p: &PcmFakten) -> String {
    let pair = |a: [f32; 2], prec| arr(a.iter().map(|v| num(f64::from(*v), prec)));
    obj(&[
        ("peak", pair(p.peak, 4)),
        ("rms", pair(p.rms, 4)),
        ("rms_db", pair(p.rms_db, 2)),
        ("clipped", p.clipped.to_string()),
        (
            "stereo_corr",
            opt(p.stereo_corr.map(|c| num(f64::from(c), 4))),
        ),
        (
            "silence",
            arr(p.silence.iter().map(|(t, d)| arr([num(*t, 1), num(*d, 1)]))),
        ),
        ("centroid_hz", num(f64::from(p.centroid_hz), 1)),
    ])
}

fn bits_json(b: &BitFakten) -> String {
    let names: Vec<String> = crate::lanes::BUDGET_PARTS
        .iter()
        .map(|(n, _)| n.to_ascii_lowercase().replace('-', "_"))
        .chain(std::iter::once("unused".to_string()))
        .collect();
    let shares: Vec<(&str, String)> = names
        .iter()
        .zip(b.shares.iter())
        .map(|(n, v)| (n.as_str(), num(*v, 4)))
        .collect();
    obj(&[
        ("shares", obj(&shares)),
        ("redundancy_celt_to_silk", b.redundancy.0.to_string()),
        ("redundancy_silk_to_celt", b.redundancy.1.to_string()),
        ("lbrr_packets", b.lbrr_packets.to_string()),
        ("lbrr_frames", b.lbrr_frames.to_string()),
        ("mid_only_frames", b.mid_only.to_string()),
        ("plc_frames", b.plc_frames.to_string()),
        ("final_range_compared", b.oracle.0.to_string()),
        ("final_range_equal", b.oracle.1.to_string()),
    ])
}

/// Budget eines Pakets in ganzen Bits (1/8 → Bit, 3 Stellen).
fn budget_json(p: &PacketInfo) -> String {
    let b = &p.budget;
    let f = |v: i32| num(f64::from(v) / 8.0, 3);
    obj(&[
        ("init", f(b.init)),
        ("silk_flags", f(b.silk_flags)),
        ("silk_lbrr", f(b.silk_lbrr)),
        ("silk_stereo", f(b.silk_stereo)),
        ("silk_side", f(b.silk_side)),
        ("silk_pulses", f(b.silk_pulses)),
        ("red_header", f(b.red_header)),
        ("celt_header", f(b.celt_header)),
        ("celt_energy", f(b.celt_energy)),
        ("celt_pvq", f(b.celt_pvq)),
        ("redundancy", f(b.redundancy)),
        ("unused", f(b.unused)),
    ])
}

fn celt_json(p: &PacketInfo) -> String {
    opt(p.celt.as_ref().map(|c| {
        obj(&[
            ("channels", c.channels.to_string()),
            ("start_band", c.start.to_string()),
            ("coded_bands", c.coded_to.to_string()),
            ("transient", c.transient.to_string()),
            ("prefilter_period", opt(c.prefilter.map(|v| v.to_string()))),
            (
                "bands_db",
                arr(c.bands_db[..c.channels]
                    .iter()
                    .map(|ch| arr(ch.iter().map(|v| num(f64::from(*v), 2))))),
            ),
            ("bits", arr(c.bits.iter().map(|v| num(f64::from(*v), 3)))),
            ("pulses", arr(c.pulses.iter().map(u32::to_string))),
        ])
    }))
}

fn silk_json(p: &PacketInfo) -> String {
    opt(p.silk.as_ref().map(|s| {
        obj(&[
            ("signal", s.signal.to_string()),
            ("gains_db", num(f64::from(s.gains_db), 2)),
            ("pitch_hz", num(f64::from(s.pitch_hz), 1)),
            ("header_bits", s.header_bits.to_string()),
            ("pulse_bits", s.pulse_bits.to_string()),
            ("frames", s.n_frames.to_string()),
            ("lbrr_frames", p.lbrr_frames.to_string()),
            ("mid_only_frames", p.mid_only.to_string()),
        ])
    }))
}

/// Komplette Datei als JSON (Schema 3).
#[must_use]
pub fn json(a: &Analysis, f: &Fakten, model: &Model) -> String {
    let (head, tags, enc, pcm, bits) = (&f.head, &f.tags, &f.encoder, &f.pcm, &f.bits);
    let frames = a.frames.iter().enumerate().map(|(i, f)| {
        let p = model.packets.get(i).cloned().unwrap_or_default();
        obj(&[
            ("index", f.index.to_string()),
            ("t_start_ms", num(f.t_start_ms, 3)),
            ("duration_ms", num(f.duration_ms, 3)),
            ("mode", esc(mode_id(f.mode))),
            ("bw", esc(f.bandwidth.short())),
            ("channels", f.channels.to_string()),
            ("config", f.config.to_string()),
            ("packing", f.packing.to_string()),
            ("pad_bytes", f.pad_bytes.to_string()),
            ("bytes", f.bytes.to_string()),
            ("kbps", num(f.kbps, 3)),
            ("final_range", p.final_range.to_string()),
            (
                "redundancy",
                opt(p
                    .redundancy
                    .map(|c| esc(if c { "celt_to_silk" } else { "silk_to_celt" }))),
            ),
            ("budget_bits", budget_json(&p)),
            ("celt", celt_json(&p)),
            ("silk", silk_json(&p)),
        ])
    });
    obj(&[
        ("schema", "3".into()),
        ("head", head_json(head)),
        (
            "tags",
            obj(&[
                ("vendor", esc(&tags.vendor)),
                ("comments", arr(tags.comments.iter().map(|c| esc(c)))),
            ]),
        ),
        ("summary", summary_json(a)),
        ("encoder", encoder_json(enc)),
        ("pcm", pcm_json(pcm)),
        ("bits", bits_json(bits)),
        ("frames", arr(frames)),
    ])
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn writer_builds_valid_nesting_and_escapes() {
        assert_eq!(obj(&[]), "{}");
        assert_eq!(arr(Vec::<String>::new()), "[]");
        assert_eq!(
            obj(&[("a", "1".into()), ("b", arr(["2".into(), "null".into()]))]),
            r#"{"a":1,"b":[2,null]}"#
        );
        assert_eq!(esc("enc \"x\" \\ y\nä"), r#""enc \"x\" \\ y\nä""#);
        assert_eq!(esc("A=B\x01C"), r#""A=B\u0001C""#);
        assert_eq!(num(f64::NAN, 2), "null");
        assert_eq!(num(1.23456, 2), "1.23");
    }
}
