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
    sig: i8,
    gains_idx: [i8; 4],
    pitch_hz: [f32; 4],
    total: i32,
    end_tell: i32,
    red: Option<(bool, usize)>,
}

/// Sondiert alle SILK-Pakete (ein Payload je Paket in dieser Fixture).
fn sweep(data: &[u8]) -> Vec<Row> {
    let ps = read_packets(data, data.len() as u64).unwrap();
    let mut state = SilkState::new();
    let mut last_mode: Option<Mode> = None;
    let mut rows = Vec::new();
    for p in &ps.packets {
        let toc = parse(&p.data).unwrap();
        if Some(toc.mode) != last_mode {
            state.reset();
            last_mode = Some(toc.mode);
        }
        if toc.mode != Mode::Silk {
            continue;
        }
        let sp = split(&p.data).unwrap();
        let payloads: Vec<&[u8]> = sp.frames.to_vec();
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
                sig: f.signal_type,
                gains_idx: f.gains_idx,
                pitch_hz: f.pitch_hz,
                total: t.total,
                end_tell: t.end_tell,
                red: t.red,
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
        let payloads: Vec<&[u8]> = sp.frames.to_vec();
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
        let payloads: Vec<&[u8]> = sp.frames.to_vec();
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
    // Reines SILK ohne Moduswechsel: keine Redundanz erwartet (S3-Fund:
    // die alten 30–90 waren Fehlalarme auf verschobener Puls-Position;
    // korrekt ausgerichtet sagt das Bit ueberall 0).
    assert!((0..=5).contains(&reds), "Schwanz-Anteil driftet: {reds}");
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

/// Formant-Vokal synthetisieren: 120-Hz-Harmonische mit geformter
/// Huelle (Bumps bei 700 + 2200 Hz).
fn vokal_block(fr: usize, off: usize) -> Vec<f32> {
    let mut out = [0.0f32; 960];
    for (i, s) in out.iter_mut().enumerate() {
        let t = (off + i) as f32 / 48_000.0;
        let mut v = 0.0f32;
        let mut k = 1u32;
        while 120.0 * k as f32 <= 7900.0 {
            let f = 120.0 * k as f32;
            let bump = (-((f - 700.0) / 400.0).powi(2)).exp()
                + 0.8 * (-((f - 2200.0) / 800.0).powi(2)).exp()
                + 0.01;
            v += bump * (2.0 * std::f32::consts::PI * f * t).sin();
            k += 1;
        }
        *s = 0.05 * v;
    }
    let _ = fr;
    out.to_vec()
}

/// End-zu-End-Orakel: bekannte Formanten muessen in der gewalkten Huelle stehen.
#[test]
fn huelle_trifft_eingeformte_formanten() {
    use opus_pure::{Application, Bandwidth as PureBw, Signal as PureSig};
    use opus_vis::silk::{SilkState, walk_packet};
    use opus_vis::silk_dq::lpc_envelope_db;
    use opus_vis::spectrum::{BINS, FMAX, FMIN};
    let bytes = common::encode_mono(
        Application::Voip,
        24_000,
        Some(PureBw::Wideband),
        Some(PureSig::Voice),
        20,
        vokal_block,
    );
    let ps = read_packets(&bytes, bytes.len() as u64).unwrap();
    let mut state = SilkState::new();
    let mut treffer = 0u32;
    let mut stimmhaft = 0u32;
    for p in &ps.packets {
        let toc = parse(&p.data).unwrap();
        if toc.mode != Mode::Silk {
            continue;
        }
        let sp = split(&p.data).unwrap();
        let payloads: Vec<&[u8]> = sp.frames.to_vec();
        let w = walk_packet(
            &payloads,
            toc.frame_ms,
            silk_bw(toc.bandwidth),
            1,
            &mut state,
        );
        for f in &w.frames {
            if f.signal_type != 2 {
                continue;
            }
            stimmhaft += 1;
            let env = lpc_envelope_db(&f.nlsf_q15, f.lpc_order, f.fs_khz);
            let mut peaks = Vec::new();
            for b in 1..BINS - 1 {
                let f_hz = FMIN * (FMAX / FMIN).powf((b as f32 + 0.5) / BINS as f32);
                if f_hz > 7500.0 {
                    break;
                }
                if env[b] > env[b - 1] && env[b] >= env[b + 1] && env[b] > -25.0 {
                    peaks.push((f_hz, env[b]));
                }
            }
            peaks.sort_by(|a, b| b.1.total_cmp(&a.1));
            // Welligkeit-tolerant: Spitze in Naehe UND hoeher als Max-12 dB.
            let nah = |ziel: f32| {
                peaks
                    .iter()
                    .any(|(f, h)| (f / ziel - 1.0).abs() < 0.25 && *h > -12.0)
            };
            if nah(700.0) && nah(2200.0) {
                treffer += 1;
            }
        }
    }
    eprintln!("vokal: stimmhaft={stimmhaft} beide-formanten={treffer}");
    assert!(stimmhaft >= 10, "zu wenig stimmhaft");
    assert!(
        treffer as f32 / stimmhaft as f32 > 0.5,
        "Formanten fehlen systematisch"
    );
}

/// Huelle ist hop-spezifisch: gleiche Huelle vs. STFT sieben Hops weiter
/// korreliert deutlich schlechter (Permutations-Kontrolle, Tilt inklusive).
#[test]
fn huelle_ist_hop_spezifisch() {
    use opus_vis::decode::decode_mono;
    use opus_vis::spectrum::{BINS, FMAX, FMIN, stft};
    let data = fixture();
    let bs = opus_vis::bitstream::analyze_bytes(&data).unwrap();
    let pcm = decode_mono(&data).unwrap();
    let cols = stft(&pcm.samples);
    let bin_of = |f: f32| {
        ((BINS as f32 * (f / FMIN).ln() / (FMAX / FMIN).ln()) - 0.5)
            .round()
            .clamp(0.0, (BINS - 1) as f32) as usize
    };
    let harm_r = |h: usize, s0: usize| {
        let f0 = bs.silk_pitch[h];
        let (mut e, mut s) = (Vec::new(), Vec::new());
        let mut k = 1u32;
        while f0 * (k as f32) < 7500.0 {
            let b = bin_of(f0 * (k as f32));
            if cols[s0][b].is_finite() {
                e.push(bs.spectrum[h][b]);
                s.push(cols[s0][b]);
            }
            k += 1;
        }
        if e.len() < 5 {
            return f32::NAN;
        }
        let me = e.iter().sum::<f32>() / e.len() as f32;
        let ms = s.iter().sum::<f32>() / s.len() as f32;
        let (mut num, mut de, mut ds) = (0.0f32, 0.0f32, 0.0f32);
        for (a, b) in e.iter().zip(s.iter()) {
            num += (a - me) * (b - ms);
            de += (a - me) * (a - me);
            ds += (b - ms) * (b - ms);
        }
        num / (de * ds + 1e-12).sqrt()
    };
    let n = cols.len().min(bs.spectrum.len());
    let mut own = Vec::new();
    let mut null = Vec::new();
    for h in 0..n {
        if !bs.silk_available[h] || bs.silk_signal[h] != 2 || bs.silk_pitch[h] <= 0.0 {
            continue;
        }
        own.push(harm_r(h, h));
        if h + 7 < n {
            null.push(harm_r(h, h + 7));
        }
    }
    own.retain(|r| r.is_finite());
    null.retain(|r| r.is_finite());
    own.sort_by(|a, b| a.total_cmp(b));
    null.sort_by(|a, b| a.total_cmp(b));
    let med = |v: &[f32]| v[v.len() / 2];
    eprintln!(
        "eigene-hop r={:.2}, fremd-hop r={:.2}",
        med(&own),
        med(&null)
    );
    assert!(med(&own) >= 0.45, "Huelle folgt eigenem Hop nicht");
    assert!(med(&null) <= 0.40, "Kontrolle unerwartet hoch");
    assert!(med(&own) - med(&null) >= 0.15, "kein Hop-Signal");
}
