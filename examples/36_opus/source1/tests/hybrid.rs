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

/// Cutoff-Orakel: Baender ab `coded_from` baut der Decoder nicht aus
/// (kein PVQ) — decodiertes PCM ist dort still, der Walk zeigt Boden.
/// Baender darunter sind Tracker-Schaetzung (PVQ-Skip, dokumentiert).
#[test]
fn hybrid_cutoff_bands_are_silent_in_reference_decode() {
    let data = fixture();
    let ps = read_packets(&data, data.len() as u64).unwrap();
    let bs = analyze_bytes(&data).unwrap();
    let mut dec = OpusDecoder::new(48_000, 1).unwrap();
    let mut worst = f64::NEG_INFINITY;
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
        let pkt = bs.packets[i].as_ref().expect("Hybrid traegt CELT");
        let silk = bs.silk_packets[i].as_ref().expect("Hybrid traegt SILK");
        assert_eq!(silk.n_frames, 1);
        assert!(
            (17..=end).contains(&pkt.coded_from),
            "Cutoff {} ausserhalb [17, {}]",
            pkt.coded_from,
            end
        );
        // Intern konsistent: jenseits des Cutoffs nur Drain-Restbits.
        for b in pkt.coded_from..end {
            assert!(
                pkt.bits[b] < 3.0,
                "Paket {i} Band {b}: {:.1} Bits jenseits Cutoff",
                pkt.bits[b]
            );
        }
        // Extern still: Decoder-PCM jenseits des Cutoffs am Boden.
        for b in pkt.coded_from..end {
            let blo = EBANDS[b] as usize * 200;
            let bhi = EBANDS[b + 1] as usize * 200;
            let pwr = band_power(&pcm, blo / 50 + 1, bhi / 50);
            let db = 10.0 * pwr.max(1e-12).log10();
            worst = worst.max(db);
            assert!(
                db < -70.0,
                "Paket {i} Band {b}: PCM {db:.1} dB jenseits Cutoff"
            );
        }
        n += 1;
    }
    assert_eq!(n, 30);
    eprintln!("cutoff-silence worst={worst:.1} dB (30 Pakete)");
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
    assert!(
        avail > bs.available.len() * 9 / 10,
        "nur {avail} Hops verfuegbar"
    );
    assert!(bs.silk_available.iter().any(|v| *v), "F0-Lane leer");
    assert!(bs.hybrid.iter().any(|v| v.is_some()), "kein Hop-Anteil");
}
