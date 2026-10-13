//! Hybrid-Dividende: SILK-Praefix + CELT-Oberband auf einem Coder.
//!
//! Orakel: `opus-pure` decodiert dieselben Pakete (unabhaengiger Decoder);
//! Hochband-Energie aus decodiertem PCM gegen gewalkte Band-Energien.

mod common;

use opus_pure::OpusDecoder;
use opus_vis::alloc::EBANDS;
use opus_vis::analyze::read_packets;
use opus_vis::bitstream::analyze_bytes;
use opus_vis::celt::endband_of;
use opus_vis::toc::parse;
use opus_vis::types::Mode;

fn fixture() -> Vec<u8> {
    include_bytes!("data/hybrid_sample.opus").to_vec()
}

/// Mittlere DFT-Bin-Leistung (einseitig, Bins `k0..k1`), Rechteck-Fenster.
fn band_power(pcm: &[f32], k0: usize, k1: usize) -> f64 {
    let n = pcm.len() as f64;
    let mut sum = 0.0;
    let mut cnt = 0u32;
    for k in k0..k1.min(pcm.len() / 2) {
        let (mut re, mut im) = (0.0f64, 0.0f64);
        for (i, s) in pcm.iter().enumerate() {
            let ph = 2.0 * std::f64::consts::PI * k as f64 * i as f64 / n;
            re += *s as f64 * ph.cos();
            im -= *s as f64 * ph.sin();
        }
        sum += 2.0 * (re * re + im * im) / (n * n);
        cnt += 1;
    }
    sum / cnt.max(1) as f64
}

/// Hochband-Orakel: decodiertes PCM gegen gewalkte Baender 17..`end`.
#[test]
fn hybrid_highband_matches_reference_decode() {
    let data = fixture();
    let ps = read_packets(&data, data.len() as u64).unwrap();
    let bs = analyze_bytes(&data).unwrap();
    let mut dec = OpusDecoder::new(48_000, 1).unwrap();
    let mut deltas = Vec::new();
    let mut n = 0;
    for (i, p) in ps.packets.iter().enumerate() {
        if n >= 30 {
            break;
        }
        let toc = parse(&p.data).unwrap();
        assert_eq!(toc.mode, Mode::Hybrid, "Fixture ist 100 % Hybrid");
        assert_eq!(toc.channels, 1);
        assert!((toc.frame_ms - 20.0).abs() < 1e-6);
        let mut pcm = vec![0.0f32; 960];
        let got = dec.decode(&p.data, 960, &mut pcm).unwrap();
        assert_eq!(got, 960);
        let end = endband_of(toc.bandwidth).min(21);
        let lo = EBANDS[17] as usize * 200;
        let hi = EBANDS[end] as usize * 200;
        let p_pcm = band_power(&pcm, lo / 50, hi / 50);
        let pkt = bs.packets[i].as_ref().expect("Hybrid traegt CELT");
        let silk = bs.silk_packets[i].as_ref().expect("Hybrid traegt SILK");
        assert_eq!(silk.n_frames, 1);
        let mut p_walk = 0.0f64;
        let mut nbins_tot = 0u32;
        for b in 17..end {
            let blo = EBANDS[b] as usize * 200;
            let bhi = EBANDS[b + 1] as usize * 200;
            let nbins = (bhi - blo) / 50;
            nbins_tot += nbins as u32;
            p_walk += 10.0f64.powf(pkt.bands_db[0][b] as f64 / 10.0) * nbins as f64;
        }
        p_walk /= nbins_tot.max(1) as f64;
        let d = 10.0 * (p_walk / p_pcm.max(1e-12)).log10();
        eprintln!("pkt {i}: walk={:.1}dB pcm={:.1}dB delta={:+.1}dB", 10.0 * p_walk.log10(), 10.0 * p_pcm.log10(), d);
        deltas.push(d);
        n += 1;
    }
    assert_eq!(n, 30);
    deltas.sort_by(|a, b| a.partial_cmp(b).unwrap());
    let med = deltas[deltas.len() / 2];
    eprintln!("median={med:.2} min={:.2} max={:.2}", deltas[0], deltas[deltas.len() - 1]);
    assert!(med.abs() < 3.0, "Hochband systematisch daneben: {med:.2} dB");
    assert!(
        deltas.iter().all(|d| d.abs() < 8.0),
        "Ausreisser im Hochband-Orakel"
    );
}

/// Aufteilung stabil: jeder Hybrid-Anteil in (0, 1), SILK ueberall da.
#[test]
fn hybrid_split_identity() {
    let data = fixture();
    let ps = read_packets(&data, data.len() as u64).unwrap();
    let bs = analyze_bytes(&data).unwrap();
    assert_eq!(bs.hybrid_silk.len(), ps.packets.len());
    for (i, p) in ps.packets.iter().enumerate() {
        let toc = parse(&p.data).unwrap();
        assert_eq!(toc.mode, Mode::Hybrid);
        let frac = bs.hybrid_silk[i].expect("Hybrid-Anteil je Paket");
        assert!(
            (0.05..0.98).contains(&frac),
            "Paket {i}: SILK-Anteil {frac:.3} ausserhalb"
        );
        assert!(bs.silk_packets[i].is_some());
        assert!(bs.packets[i].is_some());
    }
}

/// Determinismus: zwei Laeufe bitidentisch.
#[test]
fn hybrid_walk_is_deterministic() {
    let data = fixture();
    let a = analyze_bytes(&data).unwrap();
    let b = analyze_bytes(&data).unwrap();
    assert_eq!(a.spectrum, b.spectrum);
    assert_eq!(a.density, b.density);
    assert_eq!(a.hybrid_silk, b.hybrid_silk);
}

/// Ansichten 2–4 tragen Hybrid-Hops (kein ehrliches Grau mehr noetig).
#[test]
fn hybrid_views_are_available() {
    let data = fixture();
    let bs = analyze_bytes(&data).unwrap();
    let avail = bs.available.iter().filter(|v| **v).count();
    assert!(avail > bs.available.len() * 9 / 10, "nur {avail} Hops verfuegbar");
    assert!(bs.silk_available.iter().any(|v| *v), "F0-Lane leer");
    assert!(bs.hybrid.iter().any(|v| v.is_some()), "kein Hop-Anteil");
}
