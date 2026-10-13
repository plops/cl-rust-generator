//! SILK-Header-Walk: Fixture-Sweep, Orakel und Sync-Beweise.

mod common;

use opus_vis::analyze::read_packets;
use opus_vis::silk::{Bandwidth as SilkBw, SilkState, walk_packet};
use opus_vis::split::split;
use opus_vis::toc::parse;
use opus_vis::types::{Bandwidth, Mode};

fn silk_bw(bw: Bandwidth) -> SilkBw {
    match bw {
        Bandwidth::Narrow => SilkBw::Narrow,
        Bandwidth::Medium => SilkBw::Medium,
        _ => SilkBw::Wide,
    }
}

fn fixture() -> Vec<u8> {
    include_bytes!("data/silk_sample.opus").to_vec()
}

struct Row {
    pkt: usize,
    sig: i8,
    gains_db: [f32; 4],
    gains_idx: [i8; 4],
    pitch_hz: [f32; 4],
    total: i32,
    end_tell: i32,
    end_frac: i32,
    red: Option<(bool, usize)>,
    sections: [i32; 6],
}

/// Sondiert alle SILK-Pakete (ein Payload je Paket in dieser Fixture).
fn sweep(data: &[u8]) -> Vec<Row> {
    let ps = read_packets(data, data.len() as u64).unwrap();
    let mut state = SilkState::new();
    let mut last_mode: Option<Mode> = None;
    let mut rows = Vec::new();
    for (pi, p) in ps.packets.iter().enumerate() {
        let toc = parse(&p.data).unwrap();
        if Some(toc.mode) != last_mode {
            state.reset();
            last_mode = Some(toc.mode);
        }
        if toc.mode != Mode::Silk {
            continue;
        }
        let sp = split(&p.data).unwrap();
        let payloads: Vec<&[u8]> = sp.frames.iter().copied().collect();
        let walked = walk_packet(
            &payloads,
            toc.frame_ms,
            silk_bw(toc.bandwidth),
            toc.channels as usize,
            &mut state,
        );
        assert!(!walked.unsupported, "Fixture ist mono ohne LBRR");
        for (f, t) in walked.frames.iter().zip(walked.trail.iter()) {
            rows.push(Row {
                pkt: pi,
                sig: f.signal_type,
                gains_db: f.gains_db,
                gains_idx: f.gains_idx,
                pitch_hz: f.pitch_hz,
                total: t.total,
                end_tell: t.end_tell,
                end_frac: t.end_frac,
                red: t.red,
                sections: [
                    f.bits.signal,
                    f.bits.gains,
                    f.bits.nlsf,
                    f.bits.ltp,
                    f.bits.seed,
                    f.bits.pulses,
                ],
            });
        }
    }
    rows
}

/// Autokorrelations-F0 eines 20-ms-Abschnitts (48 kHz, 50–800 Hz).
fn autocorr_f0(seg: &[f32]) -> f32 {
    let mut best_lag = 0;
    let mut best_r = f32::NEG_INFINITY;
    for lag in 60..601.min(seg.len()) {
        let mut r = 0.0f32;
        for (i, s) in seg.iter().enumerate().take(seg.len() - lag) {
            r += s * seg[i + lag];
        }
        if r > best_r {
            best_r = r;
            best_lag = lag;
        }
    }
    if best_lag == 0 {
        0.0
    } else {
        48_000.0 / best_lag as f32
    }
}

#[test]
fn pitch_folgt_autokorrelation() {
    use opus_vis::decode::decode_mono;
    let data = fixture();
    let rows = sweep(&data);
    let dec = decode_mono(&data).unwrap();
    // Getrimmtes PCM: Paket i beginnt bei i*960 - pre_skip.
    let ps = read_packets(&data, data.len() as u64).unwrap();
    let skip = ps.head.pre_skip as usize;
    let mut ratios = Vec::new();
    for (i, r) in rows.iter().enumerate() {
        if r.sig != 2 || r.pitch_hz[0] <= 0.0 {
            continue;
        }
        let s0 = i * 960;
        if s0 < skip || s0 + 960 - skip > dec.samples.len() {
            continue;
        }
        let seg = &dec.samples[s0 - skip..s0 + 960 - skip];
        let f0 = autocorr_f0(seg);
        if f0 > 0.0 {
            ratios.push(r.pitch_hz[0] / f0);
        }
    }
    assert!(ratios.len() > 50, "zu wenig stimmhafte Frames");
    ratios.sort_by(|a, b| a.total_cmp(b));
    let med = ratios[ratios.len() / 2];
    let ok = ratios.iter().filter(|r| (**r - 1.0).abs() < 0.1).count();
    eprintln!(
        "stimmhaft={} median(walk/auto)={med:.3} anteil±10%: {ok}/{}",
        ratios.len(),
        ratios.len()
    );
    assert!((med - 1.0).abs() < 0.1, "Pitch liegt neben Autokorrelation");
    let bad: Vec<f32> = ratios
        .iter()
        .copied()
        .filter(|r| (r - 1.0).abs() >= 0.25)
        .collect();
    eprintln!("ausreisser(>=25%): {bad:.2?}");
    assert!(
        ok as f32 / ratios.len() as f32 > 0.9,
        "zu viele abweichende Pitches"
    );
}

#[test]
fn walk_ist_deterministisch() {
    let data = fixture();
    let a = sweep(&data);
    let b = sweep(&data);
    assert_eq!(a.len(), b.len());
    for (x, y) in a.iter().zip(b.iter()) {
        assert_eq!(
            (x.sig, x.gains_idx, x.pitch_hz, x.total, x.end_tell),
            (y.sig, y.gains_idx, y.pitch_hz, y.total, y.end_tell)
        );
    }
}

#[test]
fn nlsf_ist_sortiert_und_begrenzt() {
    use opus_vis::silk::{SilkState, walk_packet};
    let ps = read_packets(&fixture(), fixture().len() as u64).unwrap();
    let mut state = SilkState::new();
    let mut n = 0;
    for p in &ps.packets {
        let toc = parse(&p.data).unwrap();
        if toc.mode != Mode::Silk {
            continue;
        }
        let sp = split(&p.data).unwrap();
        let payloads: Vec<&[u8]> = sp.frames.iter().copied().collect();
        let w = walk_packet(
            &payloads,
            toc.frame_ms,
            silk_bw(toc.bandwidth),
            1,
            &mut state,
        );
        for f in &w.frames {
            let v = &f.nlsf_q15[..f.lpc_order];
            assert!(v.windows(2).all(|w| w[0] <= w[1]), "NLSF unsortiert");
            assert!(
                v.iter().all(|&x| (0..32768).contains(&(x as i32))),
                "NLSF ausserhalb"
            );
            n += 1;
        }
    }
    assert!(n > 400);
}

/// Anker-Orakel: synthetische 130-Hz-Sprache muss stimmhaft mit ~130 Hz lesen.
#[test]
fn synthetische_sprache_ist_stimmhaft_bei_130hz() {
    use opus_pure::{Application, Bandwidth as PureBw, Signal as PureSig};
    use opus_vis::silk::{SilkState, walk_packet};
    let bytes = common::encode_mono(
        Application::Voip,
        24_000,
        Some(PureBw::Wideband),
        Some(PureSig::Voice),
        40,
        common::speech_block,
    );
    let cfgs = common::configs(&bytes);
    assert!(
        cfgs.iter().all(|&c| (8..12).contains(&c)),
        "reines SILK erwartet"
    );
    let ps = read_packets(&bytes, bytes.len() as u64).unwrap();
    let mut state = SilkState::new();
    let mut pitches = Vec::new();
    let mut voiced = 0;
    let mut n = 0;
    for p in &ps.packets {
        let toc = parse(&p.data).unwrap();
        if toc.mode != Mode::Silk {
            continue;
        }
        let sp = split(&p.data).unwrap();
        let payloads: Vec<&[u8]> = sp.frames.iter().copied().collect();
        let w = walk_packet(
            &payloads,
            toc.frame_ms,
            silk_bw(toc.bandwidth),
            1,
            &mut state,
        );
        assert!(!w.unsupported);
        for f in &w.frames {
            n += 1;
            if f.signal_type == 2 {
                voiced += 1;
                pitches.push(f.pitch_hz[0]);
            }
        }
    }
    assert!(n >= 35, "genug Frames");
    assert!(
        voiced as f32 / n as f32 > 0.7,
        "meist stimmhaft: {voiced}/{n}"
    );
    pitches.sort_by(|a, b| a.total_cmp(b));
    let med = pitches[pitches.len() / 2];
    eprintln!("synth: voiced={voiced}/{n} median-pitch={med:.1}Hz");
    assert!((med - 130.0).abs() < 15.0, "Pitch liegt neben 130 Hz");
}

/// Bit-Bilanz: Schwaenze erkannt, Rest in Terminierungs-Band.
///
/// Ehrliche Formulierung (04-Lehre): Exaktheit wie bei CELT
/// (`Baender + Rest + Overhead = Input`) ist hier unerreichbar —
/// `tell()` traegt Prefetch-Vorhalt (Referenz-identisch, Null-Probe),
/// der Encoder schreibt Terminierungs-Fuellung plus optionale
/// Redundanz-Schwaenze. Sync beweisen Anker (130 Hz) und Autokorrelation;
/// diese Regel pinnt die Bilanz-Verteilung: Schwanz-Anteil stabil,
/// Rest im Zehner-Band (Desync wuerde Hunderte liefern, s. Null-Probe).
#[test]
fn bilanz_schwaenze_und_rest_in_band() {
    let rows = sweep(&fixture());
    let mut plain = Vec::new();
    let mut reds = 0;
    for r in &rows {
        match r.red {
            Some((_, rb)) => {
                reds += 1;
                assert!(
                    (2..r.total as usize / 8).contains(&rb),
                    "Schwanz unguelting"
                );
            }
            None => plain.push(r.total - r.end_tell),
        }
    }
    assert!((30..=90).contains(&reds), "Schwanz-Anteil driftet: {reds}");
    plain.sort();
    let med = plain[plain.len() / 2];
    eprintln!(
        "bilanz: reds={reds} ohne={} min/max={}/{} med={med}",
        plain.len(),
        plain[0],
        plain[plain.len() - 1]
    );
    assert!((med - 1).abs() <= 5, "Median driftet: {med}");
    assert!(
        plain.iter().all(|&x| (-40..=24).contains(&x)),
        "Rest ausserhalb Terminierungs-Band"
    );
}
