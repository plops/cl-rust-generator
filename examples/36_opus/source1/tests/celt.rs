//! CELT-Bitstream-Parser: Sweep ueber Fixtures + offizielle Testvektoren.

mod common;

use opus_pure::{Application, OggOpusReader};
use opus_vis::celt::{CeltState, endband_of, energy_db, lm_of_ms as lm_of, parse_celt_frame};
use opus_vis::types::Mode;

/// Parst alle CELT-Frames der Pakete; gibt (frames, max_sum_dev) zurueck.
/// `reset_on_mode_change` spiegelt `celt_dec.reset()` im Decoder.
fn sweep_packets(packets: &[Vec<u8>]) -> (usize, f32, f32) {
    let mut state = CeltState::new();
    let mut last_mode: Option<Mode> = None;
    let mut n = 0;
    let mut max_dev: i32 = 0;
    let mut max_e = 0.0f32;
    let mut worst = String::new();
    for (pi, p) in packets.iter().enumerate() {
        let toc = opus_vis::toc::parse(p).unwrap();
        if Some(toc.mode) != last_mode {
            state.reset();
            last_mode = Some(toc.mode);
        }
        if toc.mode != Mode::Celt {
            continue;
        }
        let split = opus_vis::split::split(p).unwrap();
        let lm = lm_of(toc.frame_ms);
        let end = endband_of(toc.bandwidth);
        let ch = toc.channels as usize;
        for (fi, f) in split.frames.iter().enumerate() {
            let cf = parse_celt_frame(f, lm, ch, 0, end, &mut state).unwrap();
            for c in 0..ch {
                for b in 0..end {
                    let e = cf.log_e[c][b];
                    assert!(e.is_finite(), "Energie nicht finit");
                    if e.abs() > max_e {
                        max_e = e.abs();
                        if max_e > 40.0 {
                            eprintln!(
                                "HOT pkt {pi} frm {fi} ch {c} band {b}: E={e:.1} sil={} intra={} tr={} all={:.0?}",
                                cf.silence,
                                cf.intra,
                                cf.transient,
                                cf.log_e[c].map(|v| v as i32)
                            );
                        }
                    }
                }
            }
            // Summen-Identitaet: Baender + Rest + Header-Overhead = Input.
            let mut sum = cf.balance + cf.overhead;
            for b in 0..end {
                sum += cf.coarse_bits[b] + (ch as i32) * cf.ebits[b] * 8;
            }
            let dev = (sum - cf.alloc_bits.max(0)).abs();
            if dev > max_dev {
                max_dev = dev;
                worst = format!(
                    "pkt {pi} frm {fi}: sum {sum} rsv {} coded {} int {} dual {} trim {} ch {ch} lm {lm} end {end} len {} bal {} ovh {}\n  coarse {:?}\n  ebits {:?}",
                    cf.total_rsv,
                    cf.coded_bands,
                    cf.intensity,
                    cf.dual,
                    cf.trim,
                    f.len(),
                    cf.balance,
                    cf.overhead,
                    cf.coarse_bits,
                    cf.ebits
                );
            }
            n += 1;
        }
    }
    if max_dev > 24 {
        eprintln!("WORST: {worst}");
    }
    (n, max_dev as f32 / 8.0, max_e)
}

fn ogg_packets(data: &[u8]) -> Vec<Vec<u8>> {
    let mut reader = OggOpusReader::new(data).unwrap();
    let mut out = Vec::new();
    while let Some(p) = reader.read_packet().unwrap() {
        out.push(p.data);
    }
    out
}

#[test]
fn celt_frames_parse_on_encoder_fixture() {
    let data = common::encode_mono(Application::Audio, 96_000, None, None, 10, |fr, off| {
        common::sweep_block(10, fr, off)
    });
    let packets = ogg_packets(&data);
    let (n, dev_bits, max_e) = sweep_packets(&packets);
    assert!(n >= 8, "zu wenig CELT-Frames: {n}");
    assert_eq!(dev_bits, 0.0, "Summen-Identitaet verletzt");
    assert!(max_e < 60.0, "Energie unplausibel: {max_e}");
}

#[test]
fn ref_opus_fixture_character() {
    // ref.opus ist komplett Hybrid (opusenc, Sprache) — die CELT-Schicht
    // teilt sich dort einen Range-Coder mit SILK und ist ohne SILK-Decode
    // nicht exakt parbar (B3-markiert, nicht hier geparst).
    let data = std::fs::read("tests/data/ref.opus").unwrap();
    let packets = ogg_packets(&data);
    assert!(!packets.is_empty());
    for p in &packets {
        assert_eq!(opus_vis::toc::parse(p).unwrap().mode, Mode::Hybrid);
    }
    let (n, _, _) = sweep_packets(&packets);
    assert_eq!(n, 0, "unerwartete CELT-Frames in ref.opus");
}

#[test]
fn celt_parse_is_deterministic() {
    let data = common::encode_mono(Application::Audio, 96_000, None, None, 4, |fr, off| {
        common::sine_block(880.0, fr, off)
    });
    let packets = ogg_packets(&data);
    let p = packets
        .iter()
        .find(|p| opus_vis::toc::parse(p).unwrap().mode == Mode::Celt);
    let p = p.expect("kein CELT-Paket");
    let toc = opus_vis::toc::parse(p).unwrap();
    let split = opus_vis::split::split(p).unwrap();
    let (lm, end, ch) = (
        lm_of(toc.frame_ms),
        endband_of(toc.bandwidth),
        toc.channels as usize,
    );
    let mut s1 = CeltState::new();
    let mut s2 = CeltState::new();
    for f in &split.frames {
        let a = parse_celt_frame(f, lm, ch, 0, end, &mut s1).unwrap();
        let b = parse_celt_frame(f, lm, ch, 0, end, &mut s2).unwrap();
        assert_eq!(a.log_e, b.log_e);
        assert_eq!(a.coarse_bits, b.coarse_bits);
        assert_eq!(a.ebits, b.ebits);
    }
}

#[test]
fn energy_db_matches_log2_definition() {
    // e_means[0] = 6.4375 -> 0.0 log-Energie = 6.4375 * 6.0206 dB.
    let db = energy_db(0.0, 0);
    assert!((db - 38.758).abs() < 0.01, "db = {db}");
}

/// Bitstream-Energien vs. decodiertes PCM: Pearson-Korrelation ueber alle
/// (Frame, Band)-Paare. Der entscheidende Sync-Beweis — falsch gelesene
/// Bits gaeben unkorrelierten Muell (der Range-Coder meldet nie Fehler).
#[test]
fn bitstream_energy_correlates_with_decoded_audio() {
    use opus_vis::alloc::EBANDS;
    use opus_vis::celt::energy_db;
    use opus_vis::spectrum::{FFT_SIZE, fft, hann};

    const FRAMES: usize = 40;
    let data = common::encode_mono(Application::Audio, 96_000, None, None, FRAMES, |fr, off| {
        common::sweep_block(FRAMES, fr, off)
    });
    let packets = ogg_packets(&data);
    let pcm = opus_vis::decode::decode_mono(&data).unwrap().samples;
    // Trim entfernt das Pre-Skip: PCM[0] = Stream-Sample Nr. pre_skip.
    let pre_skip = opus_pure::OggOpusReader::new(&data[..])
        .unwrap()
        .head()
        .pre_skip as isize;

    // CELT-Bandgrenzen (25-Hz-MDCT-Bins bei lm=3) -> FFT-Bins (1024, 48 kHz).
    let to_fft = |mdct: i32| ((mdct as f32 * 25.0) / 46_875.0 * 1000.0) as usize;
    let window = hann(960);
    let mut xs = Vec::new();
    let mut ys = Vec::new();
    let mut state = CeltState::new();
    let mut last_mode: Option<Mode> = None;
    let mut sample_off = 0isize;
    let mut peak_dev = 0usize;
    let mut n_peak = 0usize;
    for p in &packets {
        let toc = opus_vis::toc::parse(p).unwrap();
        let pkt_samples = (toc.duration_ms * 48.0) as isize;
        if Some(toc.mode) != last_mode {
            state.reset();
            last_mode = Some(toc.mode);
        }
        if toc.mode == Mode::Celt && toc.frame_ms == 20.0 && toc.channels == 1 {
            let split = opus_vis::split::split(p).unwrap();
            let end = endband_of(toc.bandwidth);
            for (fi, f) in split.frames.iter().enumerate() {
                let cf = parse_celt_frame(f, 3, 1, 0, end, &mut state).unwrap();
                // Sweep-Peak: wandert der lauteste Band mit der Soll-Frequenz?
                let frame_no = (sample_off / 960) as usize + fi;
                let expect_f = 200.0 * 20.0f32.powf((frame_no as f32 + 0.5) / FRAMES as f32);
                let expect_b = EBANDS
                    .windows(2)
                    .position(|w| expect_f < w[1] as f32 * 200.0)
                    .unwrap_or(20);
                let peak_b = (0..end)
                    .max_by(|&a, &b| cf.log_e[0][a].total_cmp(&cf.log_e[0][b]))
                    .unwrap();
                peak_dev = peak_dev.max(peak_b.abs_diff(expect_b));
                n_peak += 1;
                let off = sample_off + fi as isize * 960 - pre_skip;
                if off < 0 || off as usize + 960 > pcm.len() {
                    continue; // Anlauf/Flush ausserhalb des getrimmten PCMs.
                }
                let off = off as usize;
                let mut re = vec![0.0f32; FFT_SIZE];
                let mut im = vec![0.0f32; FFT_SIZE];
                for (i, w) in window.iter().enumerate() {
                    re[i] = pcm[off + i] * w;
                }
                fft(&mut re, &mut im);
                for b in 0..end {
                    let lo = to_fft(EBANDS[b] * 8);
                    let hi = to_fft(EBANDS[b + 1] * 8).max(lo + 1);
                    let mut pow = 0.0f32;
                    for k in lo..hi.min(FFT_SIZE / 2) {
                        pow += re[k] * re[k] + im[k] * im[k];
                    }
                    xs.push(energy_db(cf.log_e[0][b], b));
                    ys.push(10.0 * (pow / (hi - lo) as f32 + 1e-12).log10());
                }
            }
        }
        sample_off += pkt_samples;
    }
    assert!(xs.len() > 400, "zu wenig Paare: {}", xs.len());
    let (mx, my) = (
        xs.iter().sum::<f32>() / xs.len() as f32,
        ys.iter().sum::<f32>() / ys.len() as f32,
    );
    let (mut cov, mut vx, mut vy) = (0.0f32, 0.0f32, 0.0f32);
    for (x, y) in xs.iter().zip(&ys) {
        cov += (x - mx) * (y - my);
        vx += (x - mx) * (x - mx);
        vy += (y - my) * (y - my);
    }
    let r = cov / (vx * vy).sqrt();
    eprintln!("Energie-Korrelation: r = {r:.3} ({} Paare)", xs.len());
    eprintln!("Sweep-Peak: max-Abw {peak_dev} Baender ({n_peak} Frames)");
    assert!(r > 0.7, "Korrelation zu schwach: r = {r:.3}");
    assert!(peak_dev <= 2, "Sweep-Peak wandert nicht mit: {peak_dev}");
}

/// Offizielle libopus-Testvektoren als Orakel (nur lokal, `/tmp/tv`).
/// Fehlt das Verzeichnis (CI), wird uebersprungen.
#[test]
fn celt_sweep_over_reference_vectors() {
    use std::io::Read;
    let dir = std::path::Path::new("/tmp/tv/opus_testvectors");
    if !dir.exists() {
        eprintln!("skip: /tmp/tv fehlt");
        return;
    }
    let mut total = 0;
    let mut files = std::fs::read_dir(dir).unwrap().collect::<Vec<_>>();
    files.sort_by_key(|e| e.as_ref().unwrap().file_name());
    for f in files {
        let path = f.unwrap().path();
        if path.extension().map(|e| e != "bit").unwrap_or(true) {
            continue;
        }
        let mut raw = Vec::new();
        std::fs::File::open(&path)
            .unwrap()
            .read_to_end(&mut raw)
            .unwrap();
        // Framing: len(BE32) + rng(BE32) + Paket (aus test_opus_encode.c).
        let mut packets = Vec::new();
        let mut off = 0;
        while off + 8 <= raw.len() {
            let n = i32::from_be_bytes(raw[off..off + 4].try_into().unwrap());
            if n < 0 || off + 8 + n as usize > raw.len() {
                break;
            }
            if n > 0 {
                packets.push(raw[off + 8..off + 8 + n as usize].to_vec());
            }
            off += 8 + n as usize;
        }
        let (n, dev_bits, max_e) = sweep_packets(&packets);
        eprintln!(
            "{}: {n} CELT-Frames, max-Abw {dev_bits:.2} Bit, max|E| {max_e:.1}",
            path.file_name().unwrap().to_string_lossy()
        );
        assert_eq!(dev_bits, 0.0, "Summe verletzt in {path:?}");
        assert!(max_e < 60.0, "Energie unplausibel in {path:?}");
        total += n;
    }
    assert!(total > 5000, "zu wenig Vektor-Frames: {total}");
}
