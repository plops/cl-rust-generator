//! Hybrid-Dividende: SILK-Praefix + CELT-Oberband auf einem Coder.
//!
//! Orakel: `opus-pure` decodiert dieselben Pakete (unabhaengiger Decoder);
//! Hochband-Energie aus decodiertem PCM gegen gewalkte Band-Energien.

mod common;

use opus_pure::OpusDecoder;
use opus_vis::alloc::EBANDS;
use opus_vis::analyze::read_packets;
use opus_vis::celt::endband_of;
use opus_vis::model::Model;
use opus_vis::toc::parse;
use opus_vis::types::Mode;

fn fixture() -> Vec<u8> {
    include_bytes!("data/hybrid_sample.opus").to_vec()
}

fn model(data: &[u8]) -> Model {
    let ps = read_packets(data, data.len() as u64).unwrap();
    Model::from_set(&ps).unwrap()
}

/// SILK-Anteil an den gelesenen Bits eines Pakets.
fn silk_share(p: &opus_vis::model::PacketInfo) -> f32 {
    let b = &p.budget;
    let silk = b.silk_flags + b.silk_lbrr + b.silk_stereo + b.silk_side + b.silk_pulses;
    silk as f32 / (b.total() - b.unused - b.redundancy) as f32
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

/// Cutoff-Orakel: Baender ab `coded_bands` baut der Decoder nicht aus
/// (kein PVQ) — der Walk liest dort exakt 0 Pulse, decodiertes PCM ist still.
#[test]
fn hybrid_cutoff_bands_are_silent_in_reference_decode() {
    let data = fixture();
    let ps = read_packets(&data, data.len() as u64).unwrap();
    let m = model(&data);
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
        let pkt = m.packets[i].celt.as_ref().expect("Hybrid traegt CELT");
        let silk = m.packets[i].silk.as_ref().expect("Hybrid traegt SILK");
        assert_eq!(silk.n_frames, 1);
        assert!(
            (17..=end).contains(&pkt.coded_to),
            "Cutoff {} ausserhalb [17, {}]",
            pkt.coded_to,
            end
        );
        // Intern exakt: jenseits des Cutoffs keine Pulse.
        for b in pkt.coded_to..end {
            assert_eq!(
                pkt.pulses[b], 0,
                "Paket {i} Band {b}: Pulse jenseits Cutoff"
            );
        }
        // Extern still: Decoder-PCM jenseits des Cutoffs am Boden.
        for b in pkt.coded_to..end {
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
    let m = model(&data);
    assert_eq!(m.packets.len(), ps.packets.len());
    for (i, p) in ps.packets.iter().enumerate() {
        let toc = parse(&p.data).unwrap();
        assert_eq!(toc.mode, Mode::Hybrid);
        let frac = silk_share(&m.packets[i]);
        assert!(
            (0.05..0.98).contains(&frac),
            "Paket {i}: SILK-Anteil {frac:.3} ausserhalb"
        );
        assert!(m.packets[i].silk.is_some());
        assert!(m.packets[i].celt.is_some());
    }
}

/// Determinismus: zwei Laeufe bitidentisch.
#[test]
fn hybrid_walk_is_deterministic() {
    let data = fixture();
    let (a, b) = (model(&data), model(&data));
    assert_eq!(a.bitstream, b.bitstream);
    assert_eq!(a.density, b.density);
    assert_eq!(a.hops, b.hops);
}

/// Ansichten 2–4 tragen Hybrid-Hops (kein ehrliches Grau mehr noetig).
#[test]
fn hybrid_views_are_available() {
    let data = fixture();
    let m = model(&data);
    let avail = m.hops.iter().filter(|h| h.celt).count();
    assert!(avail > m.hops.len() * 9 / 10, "nur {avail} Hops verfuegbar");
    assert!(m.hops.iter().any(|h| h.silk), "F0-Lane leer");
    assert!(
        m.hops.iter().any(|h| h.hybrid_silk.is_some()),
        "kein Hop-Anteil"
    );
}
