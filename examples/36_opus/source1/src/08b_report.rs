//! Report: deutsche Text-Ausgabe, die eine `.opus`-Datei erklaert.
//!
//! Zahlen kommen aus [`crate::fakten`], Deutung aus einer kleinen
//! deterministischen Regel-Tabelle ([`erklaerungen`], je Regel ein
//! Test). Reine Funktionen, keine Deps ausser `std`.

use crate::fakten::{EncoderFakten, HeadFakten, PcmFakten, SilkFakten, TagFakten};
use crate::types::Analysis;

/// Hoechstens so viele Einordnungs-Saetze stehen im Report.
pub const MAX_SAETZE: usize = 5;

/// Kompletter Report als Text (Abschnitte + Einordnung).
#[must_use]
pub fn report(
    name: &str,
    a: &Analysis,
    head: &HeadFakten,
    tags: &TagFakten,
    enc: &EncoderFakten,
    pcm: &PcmFakten,
    silk: &SilkFakten,
) -> String {
    let s = &a.summary;
    let mut o = format!("opus-report: {name}\n");
    o.push_str(&format!(
        "datei: {} bytes, {:.3} s, {} kanal, pre-skip {:.3} ms\n",
        s.total_bytes,
        s.duration_ms / 1000.0,
        s.channels,
        head.pre_skip_ms,
    ));
    o.push_str(&format!(
        "encoder: {} (eingang {} hz, gain {:+.2} db, mapping {})\n",
        tags.vendor, head.input_rate_hz, head.gain_db, head.mapping,
    ));
    o.push_str(&format!(
        "bitrate: avg {:.3} kbit/s (min {:.3}, p50 {:.3}, p95 {:.3}, max {:.3}), vbr-index {:.3}\n",
        s.avg_kbps, enc.kbps_min, enc.kbps_p50, enc.kbps_p95, enc.kbps_max, enc.vbr_index,
    ));
    o.push_str(&format!(
        "kompression: roh {:.1} kbit/s -> {:.3}x (overhead {:.2} %, {} split-fallbacks)\n",
        s.raw_kbps, s.ratio, s.overhead_pct, a.split_fallbacks,
    ));
    let abschnitte = if enc.runs.len() == 1 {
        "1 abschnitt".to_string()
    } else {
        format!("{} abschnitte", enc.runs.len())
    };
    o.push_str(&format!(
        "modi: silk {:.1} %, hybrid {:.1} %, celt {:.1} % ({abschnitte})\n",
        s.frac_silk * 100.0,
        s.frac_hybrid * 100.0,
        s.frac_celt * 100.0,
    ));
    o.push_str(&format!("wechsel: {}\n", runs_zeile(enc)));
    o.push_str(&format!("bandbreite: {}\n", bw_zeile(enc)));
    o.push_str(&format!("stimme: {}\n", stimme_zeile(silk)));
    o.push_str(&format!(
        "signal: peak {:.3} / rms {:.1} dbfs, clipping {}, schwerpunkt {:.0} hz{}\n",
        pcm.peak[0].max(pcm.peak[1]),
        pcm.rms_db[0].max(pcm.rms_db[1]),
        pcm.clipped,
        pcm.centroid_hz,
        stereo_zeile(pcm),
    ));
    o.push_str(&format!(
        "stille: {} (dtx-pakete {})\n",
        stille_zeile(pcm, enc),
        enc.dtx_packets,
    ));
    o.push_str(&format!(
        "transienten: {}\n",
        if enc.transients_ms.is_empty() {
            "keine".to_string()
        } else {
            format!(
                "{} (z. b. bei {:.1} s)",
                enc.transients_ms.len(),
                enc.transients_ms[0] / 1000.0,
            )
        }
    ));
    o.push_str("einordnung:\n");
    for satz in erklaerungen(a, enc, pcm, head, silk) {
        o.push_str(&format!("- {satz}\n"));
    }
    o
}

/// Stimm-Zeile: Anteile, Tonhoehe, Gain-Dynamik (oder ehrlich nichts).
fn stimme_zeile(silk: &SilkFakten) -> String {
    if silk.silk_ms <= 0.0 {
        return "kein SILK (nichts Stimmhaftes parsbar)".to_string();
    }
    let voiced_pct = silk.voiced_ms / silk.silk_ms * 100.0;
    format!(
        "stimmhaft {:.1} % von {:.1} s SILK, tonhoehe median {:.0} hz ({:.0}-{:.0}), gain-dynamik {:.0} db",
        voiced_pct,
        silk.silk_ms / 1000.0,
        silk.pitch_med_hz,
        silk.pitch_min_hz,
        silk.pitch_max_hz,
        silk.gain_max_db - silk.gain_min_db,
    )
}

/// Moduswechsel kompakt: bis zu 6 Abschnitte, sonst gezaehlt.
fn runs_zeile(enc: &EncoderFakten) -> String {
    if enc.runs.is_empty() {
        return "keine frames".to_string();
    }
    let mut parts = Vec::new();
    for r in enc.runs.iter().take(6) {
        parts.push(format!(
            "[{:.1}-{:.1}] {}",
            r.t_start_ms / 1000.0,
            (r.t_start_ms + r.duration_ms) / 1000.0,
            r.mode,
        ));
    }
    if enc.runs.len() > 6 {
        parts.push(format!("... ({} insgesamt)", enc.runs.len()));
    }
    parts.join(", ")
}

/// Bandbreiten-Anteile, nur belegte Stufen.
fn bw_zeile(enc: &EncoderFakten) -> String {
    use crate::types::Bandwidth;
    let mut parts = Vec::new();
    for (b, v) in Bandwidth::ALL.iter().zip(enc.bw_frac.iter()) {
        if *v > 0.0005 {
            parts.push(format!("{} {:.1} %", b.short(), v * 100.0));
        }
    }
    if parts.is_empty() {
        return "keine".to_string();
    }
    parts.join(", ")
}

/// Stereo-Zusatz fuer die Signal-Zeile.
fn stereo_zeile(pcm: &PcmFakten) -> String {
    match pcm.stereo_corr {
        Some(c) => format!(", stereo-korrelation {c:.2}"),
        None => ", mono".to_string(),
    }
}

/// Stille-Zeile: Abschnitte oder Entwarnung.
fn stille_zeile(pcm: &PcmFakten, enc: &EncoderFakten) -> String {
    if pcm.silence.is_empty() && enc.dtx_ms == 0.0 {
        return "keine abschnitte ab 200 ms".to_string();
    }
    let mut parts: Vec<String> = pcm
        .silence
        .iter()
        .take(5)
        .map(|(t, d)| format!("{t:.1} s +{d:.1} s"))
        .collect();
    if pcm.silence.len() > 5 {
        parts.push(format!("... ({} insgesamt)", pcm.silence.len()));
    }
    if enc.dtx_ms > 0.0 {
        parts.push(format!("dtx {:.1} s", enc.dtx_ms / 1000.0));
    }
    parts.join(", ")
}

/// Einordnungs-Saetze: Heuristiken mit Schwellen, als solche formuliert.
#[must_use]
pub fn erklaerungen(
    a: &Analysis,
    enc: &EncoderFakten,
    pcm: &PcmFakten,
    head: &HeadFakten,
    silk: &SilkFakten,
) -> Vec<String> {
    let s = &a.summary;
    let mut out = Vec::new();
    let schmal = enc.bw_frac[0] + enc.bw_frac[1] + enc.bw_frac[2];
    if s.frac_silk > 0.7 && schmal > 0.7 {
        out.push(
            "spricht fuer sprache: viel SILK bei schmaler bandbreite (encoder nutzt sprachmodell)"
                .to_string(),
        );
    }
    if s.frac_celt > 0.8 && enc.bw_frac[4] > 0.5 {
        out.push(
            "spricht fuer musik in vollband: fast nur CELT bis 20 khz (encoder nutzt musikmodell)"
                .to_string(),
        );
    }
    if s.frac_hybrid > 0.3 {
        out.push(
            "viel hybrid: mischung aus sprache und musik, z. b. gesang mit begleitung".to_string(),
        );
    }
    if silk.silk_ms > 0.0 && silk.voiced_ms / silk.silk_ms > 0.6 {
        out.push(format!(
            "spricht fuer gesprochene sprache: stimmhaft-dominiert ({:.0} %, median {:.0} hz)",
            silk.voiced_ms / silk.silk_ms * 100.0,
            silk.pitch_med_hz,
        ));
    } else if silk.silk_ms > 0.0 && (silk.unvoiced_ms + silk.inactive_ms) / silk.silk_ms > 0.5 {
        out.push(
            "viele stimmlose anteile: fluestern, zischlaute oder pausen im sprachsignal"
                .to_string(),
        );
    }
    if enc.vbr_index > 0.5 {
        out.push(
            "stark variable bitrate: der encoder verteilt bits je nach signal (vbr)".to_string(),
        );
    } else if enc.vbr_index < 0.05 && s.frames > 3 {
        out.push(
            "nahezu konstante bitrate: der encoder bleibt am zielwert (cbr-artig)".to_string(),
        );
    }
    if enc.runs.len() == 1 && s.frames > 1 {
        out.push("durchgehend ein modus: keine umschaltung erkannt".to_string());
    } else if enc.runs.len() > 6 {
        out.push(format!(
            "haeufige moduswechsel ({} abschnitte): wechselhaftes signal",
            enc.runs.len()
        ));
    }
    if pcm.clipped > 0 {
        out.push(format!(
            "achtung: {} samples am clipping (pegel >= 0.999)",
            pcm.clipped
        ));
    }
    if !pcm.silence.is_empty() {
        out.push(format!(
            "{} stille-abschnitte ab 200 ms (laengster {:.1} s)",
            pcm.silence.len(),
            pcm.silence.iter().map(|(_, d)| *d).fold(0.0, f64::max) / 1000.0,
        ));
    } else if enc.dtx_packets > 0 {
        out.push(format!(
            "{} dtx-pakete: pausen, in denen der encoder fast nichts sendet",
            enc.dtx_packets
        ));
    }
    if enc.transients_ms.len() > 10 {
        out.push(format!(
            "viele transiente ({}): perkussives oder ploetzliches signal",
            enc.transients_ms.len()
        ));
    }
    match pcm.stereo_corr {
        Some(c) if c > 0.9 && head.channels == 2 => {
            out.push("stereo fast mono: beide kanaele nahezu identisch".to_string());
        }
        Some(c) if c < 0.3 => {
            out.push("breites stereo: kanaele deutlich unterschiedlich".to_string());
        }
        _ => {}
    }
    if head.gain_db != 0.0 {
        out.push(format!(
            "wiedergabe-gain {:+.2} db steht im kopf (player pegeln nach)",
            head.gain_db
        ));
    }
    if out.is_empty() {
        out.push("keine auffaelligkeiten: gleichmaessige datei".to_string());
    }
    out.truncate(MAX_SAETZE);
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::decode::ChannelStats;
    use crate::fakten::{HeadFakten, PcmFakten, SilkFakten, TagFakten};
    use crate::types::{Bandwidth, FrameMeta, Mode, Summary};

    fn analysis(modes: &[(Mode, f64)], kbps: &[f64]) -> Analysis {
        let mut t = 0.0;
        let frames = modes
            .iter()
            .zip(kbps.iter().cycle())
            .enumerate()
            .map(|(i, ((m, ms), k))| {
                let f = FrameMeta {
                    index: i,
                    t_start_ms: t,
                    duration_ms: *ms,
                    mode: *m,
                    bandwidth: Bandwidth::Full,
                    channels: 1,
                    bytes: 100,
                    kbps: *k,
                    config: 31,
                    packing: 0,
                    pad_bytes: 0,
                };
                t += *ms;
                f
            })
            .collect::<Vec<_>>();
        let dur: f64 = modes.iter().map(|(_, ms)| *ms).sum();
        let (mut silk, mut hybrid, mut celt) = (0.0, 0.0, 0.0);
        for (m, ms) in modes {
            match m {
                Mode::Silk => silk += *ms,
                Mode::Hybrid => hybrid += *ms,
                Mode::Celt => celt += *ms,
            }
        }
        Analysis {
            summary: Summary {
                frames: frames.len(),
                duration_ms: dur,
                audio_bytes: 0,
                total_bytes: 0,
                avg_kbps: 64.0,
                raw_kbps: 768.0,
                ratio: 12.0,
                overhead_pct: 1.0,
                frac_silk: silk / dur,
                frac_hybrid: hybrid / dur,
                frac_celt: celt / dur,
                channels: 1,
            },
            frames,
            split_fallbacks: 0,
        }
    }

    fn fakten(
        a: &Analysis,
        transients: &[bool],
        corr: Option<f32>,
        clipped: u64,
    ) -> (EncoderFakten, PcmFakten, HeadFakten, TagFakten) {
        let enc = crate::fakten::encoder_fakten(a, transients);
        let stats = ChannelStats {
            peak: [0.5, 0.0],
            rms: [0.1, 0.0],
            clipped,
            stereo_corr: corr,
        };
        let pcm = crate::fakten::pcm_fakten(&[], &[], &stats);
        let head = HeadFakten {
            channels: 1,
            pre_skip_ms: 6.5,
            input_rate_hz: 48_000,
            gain_db: 0.0,
            mapping: 0,
            streams: 1,
            coupled: 0,
        };
        let tags = TagFakten {
            vendor: "test".to_string(),
            comments: vec![],
        };
        (enc, pcm, head, tags)
    }

    fn silk_f(voiced_ms: f64, unvoiced_ms: f64) -> SilkFakten {
        SilkFakten {
            silk_ms: voiced_ms + unvoiced_ms,
            voiced_ms,
            unvoiced_ms,
            inactive_ms: 0.0,
            pitch_med_hz: if voiced_ms > 0.0 { 142.0 } else { 0.0 },
            pitch_min_hz: if voiced_ms > 0.0 { 100.0 } else { 0.0 },
            pitch_max_hz: if voiced_ms > 0.0 { 200.0 } else { 0.0 },
            gain_min_db: 20.0,
            gain_max_db: 40.0,
        }
    }

    #[test]
    fn sprache_regel_feuert_bei_silk_und_schmal() {
        let mut a = analysis(&[(Mode::Silk, 80.0), (Mode::Celt, 20.0)], &[24.0]);
        for f in &mut a.frames {
            if f.mode == Mode::Silk {
                f.bandwidth = Bandwidth::Wide;
            }
        }
        let (enc, pcm, head, _) = fakten(&a, &[], None, 0);
        let s = erklaerungen(&a, &enc, &pcm, &head, &silk_f(0.0, 0.0));
        assert!(s.iter().any(|x| x.contains("sprache")), "{s:?}");
    }

    #[test]
    fn musik_regel_feuert_bei_celt_vollband() {
        let a = analysis(&[(Mode::Celt, 100.0)], &[96.0]);
        let (enc, pcm, head, _) = fakten(&a, &[], None, 0);
        let s = erklaerungen(&a, &enc, &pcm, &head, &silk_f(0.0, 0.0));
        assert!(s.iter().any(|x| x.contains("musik")), "{s:?}");
    }

    #[test]
    fn vbr_und_cbr_regeln_schliessen_sich_aus() {
        let vbr = analysis(&[(Mode::Celt, 20.0); 10], &[10.0, 200.0]);
        let (enc, pcm, head, _) = fakten(&vbr, &[], None, 0);
        let s = erklaerungen(&vbr, &enc, &pcm, &head, &silk_f(0.0, 0.0));
        assert!(s.iter().any(|x| x.contains("variable bitrate")), "{s:?}");
        let cbr = analysis(&[(Mode::Celt, 20.0); 10], &[64.0]);
        let (enc, pcm, head, _) = fakten(&cbr, &[], None, 0);
        let s = erklaerungen(&cbr, &enc, &pcm, &head, &silk_f(0.0, 0.0));
        assert!(s.iter().any(|x| x.contains("konstante bitrate")), "{s:?}");
    }

    #[test]
    fn clipping_und_stereo_regeln() {
        let a = analysis(&[(Mode::Celt, 100.0)], &[96.0]);
        let (enc, pcm, head, _) = fakten(&a, &[], Some(0.1), 7);
        let s = erklaerungen(&a, &enc, &pcm, &head, &silk_f(0.0, 0.0));
        assert!(s.iter().any(|x| x.contains("clipping")), "{s:?}");
        assert!(s.iter().any(|x| x.contains("breites stereo")), "{s:?}");
    }

    #[test]
    fn leerlauf_gibt_fallback_satz() {
        let a = analysis(&[(Mode::Celt, 20.0)], &[64.0]);
        let (enc, pcm, head, _) = fakten(&a, &[], None, 0);
        let s = erklaerungen(&a, &enc, &pcm, &head, &silk_f(0.0, 0.0));
        assert!(!s.is_empty() && s.len() <= MAX_SAETZE, "{s:?}");
    }

    #[test]
    fn report_enthaelt_alle_abschnitte() {
        let a = analysis(&[(Mode::Silk, 40.0), (Mode::Celt, 60.0)], &[24.0, 96.0]);
        let (enc, pcm, head, tags) = fakten(&a, &[false, true], None, 0);
        let r = report(
            "demo.opus",
            &a,
            &head,
            &tags,
            &enc,
            &pcm,
            &silk_f(30.0, 10.0),
        );
        for abschnitt in [
            "opus-report:",
            "datei:",
            "encoder:",
            "bitrate:",
            "kompression:",
            "modi:",
            "wechsel:",
            "bandbreite:",
            "stimme:",
            "signal:",
            "stille:",
            "transienten:",
            "einordnung:",
        ] {
            assert!(r.contains(abschnitt), "{abschnitt} fehlt in:\n{r}");
        }
        assert!(r.contains("stimmhaft 75.0 %"), "stimm-zeile fehlt in:\n{r}");
    }

    #[test]
    fn stimmhaft_regel_feuert_bei_voiced_dominanz() {
        let a = analysis(&[(Mode::Silk, 100.0)], &[24.0]);
        let (enc, pcm, head, _) = fakten(&a, &[], None, 0);
        let s = erklaerungen(&a, &enc, &pcm, &head, &silk_f(80.0, 20.0));
        assert!(s.iter().any(|x| x.contains("gesprochene sprache")), "{s:?}");
    }

    #[test]
    fn stimmlos_regel_feuert_bei_unvoiced_dominanz() {
        let a = analysis(&[(Mode::Silk, 100.0)], &[24.0]);
        let (enc, pcm, head, _) = fakten(&a, &[], None, 0);
        let s = erklaerungen(&a, &enc, &pcm, &head, &silk_f(20.0, 80.0));
        assert!(s.iter().any(|x| x.contains("stimmlose anteile")), "{s:?}");
    }

    #[test]
    fn erklaerungen_sind_auf_fuenf_begrenzt() {
        // Alles feuert gleichzeitig: trotzdem max. 5 Saetze.
        let mut a = analysis(&[(Mode::Silk, 80.0), (Mode::Celt, 20.0)], &[10.0, 300.0]);
        for f in &mut a.frames {
            f.bandwidth = Bandwidth::Full;
        }
        let (enc, pcm, head, _) = fakten(&a, &[true; 20], Some(0.95), 3);
        let mut head = head;
        head.gain_db = 2.0;
        let s = erklaerungen(&a, &enc, &pcm, &head, &silk_f(0.0, 0.0));
        assert!(s.len() <= MAX_SAETZE, "{s:?}");
    }
}
