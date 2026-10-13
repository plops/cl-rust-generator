//! 05b — Bitstrom-Modell: CELT-Energien/Bits → Spektrum-/Dichte-Spalten.
//!
//! Baut aus den geparsten Paketen je 20-ms-Hop (wie [`crate::spectrum`])
//! zwei Spalten: das Bitstrom-Spektrum (21 Encoder-Baender in dB,
//! auf 256 Log-Bins expandiert) und die spektrale Informationsdichte
//! (Bits/Band, gleichmaessig auf die Bins des Bands verteilt).
//! SILK-Hops liefern die LPC-Huelle, Hybrid-Hops Huelle unten + CELT
//! oben (Startband 17, ein Coder); nur CELT-fremde Modi liefern Boden
//! (`DB_FLOOR`, Dichte 0, `available = false`).
//!
//! [`divergence`] ist die punktweise Differenz STFT − Bitstrom in dB
//! (`NaN` wo unverfuegbar — ehrlich statt 0 dB vorgetaeuschter
//! Uebereinstimmung).

use crate::alloc::{EBANDS, NB, drain_shares};
use crate::celt::{
    CeltState, endband_of, energy_db, lm_of_ms, parse_celt_frame, parse_celt_frame_rc,
};
use crate::range::RangeDec;
use crate::silk::{
    Bandwidth as SilkBw, SilkCtx, SilkState, SilkSummary, packet_flags, walk_packet,
    walk_silk_frame,
};
use crate::silk_dq::{lpc_envelope_db, summarize};
use crate::silk_tab::{CODE_CONDITIONALLY, CODE_INDEPENDENTLY};
use crate::spectrum::{BINS, DB_FLOOR, FMAX, FMIN, HOP, RATE};
use crate::types::{Bandwidth, Error, Mode};

/// Preemphasis-Koeffizient (CELT @48 kHz): y[i] = x[i] − 0,85·x[i−1].
const PREEMPH: f32 = 0.85;
/// Globale Kalibrierung Bitstrom → dBFS (MDCT-Gain + int16-Skala +
/// Fenster), gemessen an der Sinus-Fixture (s. `tests/spectrum.rs`).
const CAL_DB: f32 = -76.5;

/// De-Emphasis-Korrektur je Band in dB (wird addiert): Der Encoder misst
/// auf preemphasiertem Signal; tiefe Baender laegen sonst ~16 dB zu tief.
fn deemph_db(band: usize) -> f32 {
    let lo = (EBANDS[band] * 200) as f32;
    let hi = (EBANDS[band + 1] * 200) as f32;
    let w = 2.0 * std::f32::consts::PI * (lo + hi) / 2.0 / RATE;
    let re = 1.0 - PREEMPH * w.cos();
    let im = PREEMPH * w.sin();
    -20.0 * (re * re + im * im).sqrt().log10()
}

/// Band-Energie in dBFS: Roh-dB + Kalibrierung + De-Emphasis.
pub fn bitstream_db(log_e: f32, band: usize) -> f32 {
    energy_db(log_e, band) + CAL_DB + deemph_db(band.min(20))
}

/// CELT-Band eines Log-Bins (Bin-Mitte → Band).
pub fn band_of_bin(bin: usize) -> usize {
    let f = FMIN * (FMAX / FMIN).powf((bin as f32 + 0.5) / BINS as f32);
    // Bandoberkanten in Hz: EBANDS * 8 MDCT-Bins * 25 Hz.
    let mut b = 0;
    while b + 1 < NB && (EBANDS[b + 1] * 200) as f32 <= f {
        b += 1;
    }
    b
}

/// CELT-Band je Log-Bin (fuer die Spalten-Expansion).
fn bin_to_band() -> [usize; BINS] {
    let mut map = [0usize; BINS];
    for (j, m) in map.iter_mut().enumerate() {
        *m = band_of_bin(j);
    }
    map
}

/// CELT-Banddaten eines Pakets (ueber seine Frames gemittelt).
#[derive(Debug, Clone, PartialEq)]
pub struct PacketCelt {
    /// Band-Energien in dB je Kanal (`NB` Baender, nur `channels` belegt).
    pub bands_db: [[f32; NB]; 2],
    /// Signal-Bits je Band (Coarse + Fine + Drain-Anteil).
    pub bits: [f32; NB],
    /// Belegte Kanaele (1/2).
    pub channels: usize,
}

/// Bitstrom-Spalten einer Datei, hop-aligned zu [`crate::spectrum::stft`].
pub struct Bitstream {
    /// dB-Spalten (`DB_FLOOR` wo unverfuegbar).
    pub spectrum: Vec<[f32; BINS]>,
    /// Bits je Bin (summiert = Allokation − Header-Overhead; 0 wo n/a).
    pub density: Vec<[f32; BINS]>,
    /// `true` wo ein CELT-Paket den Hop traegt.
    pub available: Vec<bool>,
    /// Transient-Flag je Hop (Oder ueber Frames; Hover).
    pub transient: Vec<bool>,
    /// Signal-Bits je Hop (Coarse + Fine; Hover).
    pub frame_bits: Vec<f32>,
    /// Banddaten je Paket (`None` wo kein CELT; fuer JSON/Report).
    pub packets: Vec<Option<PacketCelt>>,
    /// SILK-Kennzahlen je Paket (`None` wo kein SILK; fuer JSON/Report).
    pub silk_packets: Vec<Option<SilkSummary>>,
    /// SILK-Bit-Anteil je Hybrid-Paket (`None` wo kein Hybrid; Hover).
    pub hybrid_silk: Vec<Option<f32>>,
    /// SILK-Bit-Anteil je Hop (`None` wo kein Hybrid; Hover).
    pub hybrid: Vec<Option<f32>>,
    /// `true` wo ein SILK-Paket den Hop traegt.
    pub silk_available: Vec<bool>,
    /// Stimmhaftestes Signal je Hop (0/1/2).
    pub silk_signal: Vec<i8>,
    /// Mittlere Tonhoehe je Hop in Hz (0 wo stimmlos).
    pub silk_pitch: Vec<f32>,
    /// Mittlerer Gain je Hop in dB.
    pub silk_gain: Vec<f32>,
}

/// SILK-Bandbreite aus TOC (NB/MB direkt, Rest = WB-Decodierung).
fn silk_bw(bw: Bandwidth) -> SilkBw {
    match bw {
        Bandwidth::Narrow => SilkBw::Narrow,
        Bandwidth::Medium => SilkBw::Medium,
        _ => SilkBw::Wide,
    }
}

/// Wasserfall-Quelle (Ansicht 1–4).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Source {
    /// Decodiertes STFT-Spektrum in dB.
    #[default]
    Spectrum,
    /// Encoder-Band-Energien in dB.
    Bitstream,
    /// STFT − Bitstrom in dB (divergierend).
    Divergence,
    /// Bits je Bin (Informationsdichte).
    Density,
}

impl Source {
    /// Alle vier in Tasten-Reihenfolge.
    pub const ALL: [Source; 4] = [
        Source::Spectrum,
        Source::Bitstream,
        Source::Divergence,
        Source::Density,
    ];

    /// Anzeigename (Kopfzeile, `--view`).
    #[must_use]
    pub fn name(self) -> &'static str {
        match self {
            Source::Spectrum => "SPEKTRUM",
            Source::Bitstream => "BITSTROM",
            Source::Divergence => "DIVERGENZ",
            Source::Density => "DICHTE",
        }
    }

    /// Aus `--view`-Wert (case-insensitiv) oder Taste 1–4.
    #[must_use]
    pub fn parse(s: &str) -> Option<Source> {
        match s.to_ascii_uppercase().as_str() {
            "SPEKTRUM" | "SPECTRUM" | "1" => Some(Source::Spectrum),
            "BITSTROM" | "BITSTREAM" | "2" => Some(Source::Bitstream),
            "DIVERGENZ" | "DIVERGENCE" | "3" => Some(Source::Divergence),
            "DICHTE" | "DENSITY" | "4" => Some(Source::Density),
            _ => None,
        }
    }
}

/// Alle vier Spalten-Saetze, auf gemeinsame Laenge gekuerzt.
///
/// `bitstream` wird per Move uebernommen (keine Spalten-Kopie):
/// STFT-Spalten kommen als owned `Vec`, der Rest wandert aus `bs`.
pub struct Sources {
    /// STFT-Spalten (dB).
    pub spectrum: Vec<[f32; BINS]>,
    /// Bitstrom-Spalten (dB, Dichte, Verfuegbarkeit je Hop).
    pub bitstream: Bitstream,
    /// Divergenz-Spalten (dB, NaN wo unverfuegbar).
    pub divergence: Vec<[f32; BINS]>,
    /// Globales Dichte-Maximum (Normierung).
    pub density_max: f32,
}

impl Sources {
    /// Baut aus STFT + Bitstrom alle Ansichten (kuerzt auf Minimum).
    #[must_use]
    pub fn build(mut stft: Vec<[f32; BINS]>, mut bs: Bitstream) -> Self {
        let n = stft.len().min(bs.spectrum.len());
        stft.truncate(n);
        bs.spectrum.truncate(n);
        bs.density.truncate(n);
        bs.available.truncate(n);
        bs.transient.truncate(n);
        bs.frame_bits.truncate(n);
        bs.silk_available.truncate(n);
        bs.silk_signal.truncate(n);
        bs.silk_pitch.truncate(n);
        bs.silk_gain.truncate(n);
        bs.hybrid.truncate(n);
        let mut div_cols = Vec::with_capacity(n);
        let mut density_max = 0.0f32;
        for (h, scol) in stft.iter().enumerate().take(n) {
            div_cols.push(divergence(
                scol,
                &bs.spectrum[h],
                bs.available[h] || bs.silk_available[h],
            ));
            for v in &bs.density[h] {
                density_max = density_max.max(*v);
            }
        }
        Self {
            spectrum: stft,
            bitstream: bs,
            divergence: div_cols,
            density_max: density_max.max(1e-6),
        }
    }

    /// Spalten einer Quelle (fuer Renderer/Hover).
    #[must_use]
    pub fn cols(&self, src: Source) -> &[[f32; BINS]] {
        match src {
            Source::Spectrum => &self.spectrum,
            Source::Bitstream => &self.bitstream.spectrum,
            Source::Divergence => &self.divergence,
            Source::Density => &self.bitstream.density,
        }
    }
}

/// Parst alle Pakete und baut die Hop-Spalten (Pre-Skip beachtet).
pub fn analyze_bytes(data: &[u8]) -> Result<Bitstream, Error> {
    let ps = crate::analyze::read_packets(data, data.len() as u64)?;
    analyze_packets(&ps)
}

/// Parst eine einmal gelesene Paketliste (TOC je Paket genau einmal).
pub fn analyze_packets(ps: &crate::analyze::PacketSet) -> Result<Bitstream, Error> {
    let pre_skip = ps.head.pre_skip as usize;
    let packets = &ps.packets;
    // TOC einmal je Paket parsen (fuer Grenzen und Frame-Walk).
    let mut tocs = Vec::with_capacity(packets.len());
    for p in packets {
        tocs.push(crate::toc::parse(&p.data)?);
    }
    // Paketgrenzen in Stream-Samples (48 kHz).
    let mut bounds = Vec::with_capacity(packets.len() + 1);
    bounds.push(0usize);
    for toc in &tocs {
        let last = *bounds.last().unwrap();
        bounds.push(last + (toc.duration_ms * 48.0) as usize);
    }
    let total = *bounds.last().unwrap();
    // CELT-/SILK-Frames sequentiell parsen (Zustand + Reset bei Moduswechsel).
    let mut state = CeltState::new();
    let mut silk_state = SilkState::new();
    let mut last_mode: Option<Mode> = None;
    // Je Paket: gemittelte Band-Energien (dB) + summierte Bits/Band +
    // Transient-Oder + Signal-Bits (Hover).
    type PacketBands = ([[f32; NB]; 2], [f32; NB], bool, f32);
    let mut per_packet: Vec<Option<PacketBands>> = Vec::new();
    let mut celt_packets: Vec<Option<PacketCelt>> = Vec::new();
    let mut silk_packets: Vec<Option<SilkSummary>> = Vec::new();
    let mut hybrid_silk: Vec<Option<f32>> = Vec::new();
    for (p, toc) in packets.iter().zip(tocs.iter()) {
        if Some(toc.mode) != last_mode {
            state.reset();
            silk_state.reset();
            last_mode = Some(toc.mode);
        }
        if toc.mode == Mode::Silk {
            let summary = if toc.channels == 1 {
                let split = crate::split::split(&p.data)?;
                let payloads: Vec<&[u8]> = split.frames.to_vec();
                let walked = walk_packet(
                    &payloads,
                    toc.frame_ms,
                    silk_bw(toc.bandwidth),
                    toc.channels as usize,
                    &mut silk_state,
                );
                if walked.unsupported {
                    None
                } else {
                    summarize(&walked.frames)
                }
            } else {
                None
            };
            per_packet.push(None);
            celt_packets.push(None);
            silk_packets.push(summary);
            hybrid_silk.push(None);
            continue;
        }
        if toc.mode == Mode::Hybrid {
            // Hybrid: SILK-Praefix + CELT-Oberband teilen sich einen Coder
            // (`decoder.rs`, Hybrid-Arm) — derselbe `RangeDec` laeuft nach
            // dem SILK-Frame mit der effektiven CELT-Laenge weiter (Start 17).
            let split = crate::split::split(&p.data)?;
            let (lm, end, ch) = (
                lm_of_ms(toc.frame_ms),
                endband_of(toc.bandwidth),
                toc.channels as usize,
            );
            let mut pow = [[0.0f32; NB]; 2];
            let mut bits = [0.0f32; NB];
            let mut n = 0;
            let mut transient = false;
            let mut silk_frames = Vec::new();
            let mut silk_bits_sum = 0i32;
            let mut total_bits_sum = 0i32;
            let mut ok = ch == 1;
            let nb_subfr = if toc.frame_ms < 15.0 { 2 } else { 4 };
            let mut g = 0usize;
            for f in &split.frames {
                if !ok {
                    break;
                }
                let mut rc = RangeDec::new(f);
                let vad = match packet_flags(&mut rc, 1) {
                    Some(v) => v,
                    None => {
                        ok = false;
                        break;
                    }
                };
                let ctx = SilkCtx {
                    bw: silk_bw(toc.bandwidth),
                    nb_subfr,
                    vad: vad[0],
                    cond: if g == 0 {
                        CODE_INDEPENDENTLY
                    } else {
                        CODE_CONDITIONALLY
                    },
                };
                silk_frames.push(walk_silk_frame(&mut rc, &ctx, &mut silk_state));
                g += 1;
                silk_bits_sum += rc.tell();
                total_bits_sum += f.len() as i32 * 8;
                // Hybrid-Redundanz: Bit(12), Bit(1), uint(256)+2 Rest-Bytes.
                let total = f.len() as i32 * 8;
                let mut effective = f.len();
                if rc.tell() + 37 <= total && rc.decode_bit_logp(12) {
                    let _ = rc.decode_bit_logp(1);
                    let rb = rc.dec_uint(256) as usize + 2;
                    if rb <= effective {
                        effective -= rb;
                    }
                }
                let cf =
                    parse_celt_frame_rc(&mut rc, effective as i32 * 8, lm, ch, 17, end, &mut state)
                        .map_err(Error::BadPacket)?;
                transient |= cf.transient;
                for (pc, lc) in pow.iter_mut().zip(cf.log_e.iter()).take(ch) {
                    for (b, (p, l)) in pc.iter_mut().zip(lc.iter()).enumerate().take(end) {
                        // Uncodiert (kein Coarse, kein Fine): die Prediction
                        // ist kein Signal — ehrlich Boden statt Geister-Pegel.
                        if cf.coarse_bits[b] == 0 && cf.ebits[b] == 0 {
                            continue;
                        }
                        *p += 10.0f32.powf(bitstream_db(*l, b) / 10.0);
                    }
                }
                for (dst, (crs, eb)) in bits
                    .iter_mut()
                    .zip(cf.coarse_bits.iter().zip(cf.ebits.iter()))
                    .take(end)
                {
                    *dst += *crs as f32 / 8.0 + ch as f32 * *eb as f32;
                }
                for (dst, sh) in bits
                    .iter_mut()
                    .zip(drain_shares(cf.balance, cf.coded_bands.max(0) as usize, 0).iter())
                    .take(end)
                {
                    *dst += *sh as f32 / 8.0;
                }
                n += 1;
            }
            if !ok || n == 0 {
                per_packet.push(None);
                celt_packets.push(None);
                silk_packets.push(None);
                hybrid_silk.push(None);
                continue;
            }
            let mut db = [[DB_FLOOR; NB]; 2];
            for (dc, pc) in db.iter_mut().zip(pow.iter()).take(ch) {
                for (d, p) in dc.iter_mut().zip(pc.iter()).take(end) {
                    *d = 10.0 * (p / n as f32 + 1e-12).log10();
                }
            }
            // Allokations-Identitaet: SILK-Bits ueber die Baender 0..16 nach
            // Huellen-Leistung verteilen (Form aus NLSF, Summe = gemessene
            // Bits) — Dichte und Hover zeigen dann alle Payload-Bits.
            let summary = summarize(&silk_frames);
            if let Some(ref s) = summary {
                let env = lpc_envelope_db(&s.nlsf_q15, s.order, s.fs_khz);
                let mut w = [0.0f32; NB];
                for (j, v) in env.iter().enumerate() {
                    let b = band_of_bin(j);
                    if b < 17 {
                        w[b] += 10.0f32.powf(v / 10.0);
                    }
                }
                let tot: f32 = w.iter().sum();
                for (dst, share) in bits.iter_mut().zip(w.iter()).take(17) {
                    let share = if tot > 0.0 { *share / tot } else { 1.0 / 17.0 };
                    *dst += share * silk_bits_sum as f32;
                }
            }
            per_packet.push(Some((db, bits, transient, bits.iter().sum())));
            celt_packets.push(Some(PacketCelt {
                bands_db: db,
                bits,
                channels: ch,
            }));
            silk_packets.push(summary);
            hybrid_silk.push(Some(silk_bits_sum as f32 / total_bits_sum.max(1) as f32));
            continue;
        }
        if toc.mode != Mode::Celt {
            per_packet.push(None);
            celt_packets.push(None);
            silk_packets.push(None);
            hybrid_silk.push(None);
            continue;
        }
        let split = crate::split::split(&p.data)?;
        let (lm, end, ch) = (
            lm_of_ms(toc.frame_ms),
            endband_of(toc.bandwidth),
            toc.channels as usize,
        );
        let mut pow = [[0.0f32; NB]; 2];
        let mut bits = [0.0f32; NB];
        let mut n = 0;
        let mut transient = false;
        for f in &split.frames {
            let cf = parse_celt_frame(f, lm, ch, 0, end, &mut state).map_err(Error::BadPacket)?;
            transient |= cf.transient;
            for (pc, lc) in pow.iter_mut().zip(cf.log_e.iter()).take(ch) {
                for (b, (p, l)) in pc.iter_mut().zip(lc.iter()).enumerate().take(end) {
                    // Uncodiert (kein Coarse, kein Fine): die Prediction ist
                    // kein Signal — ehrlich Boden statt Geister-Pegel (S3).
                    if cf.coarse_bits[b] == 0 && cf.ebits[b] == 0 {
                        continue;
                    }
                    *p += 10.0f32.powf(bitstream_db(*l, b) / 10.0);
                }
            }
            for (dst, (crs, eb)) in bits
                .iter_mut()
                .zip(cf.coarse_bits.iter().zip(cf.ebits.iter()))
                .take(end)
            {
                *dst += *crs as f32 / 8.0 + ch as f32 * *eb as f32;
            }
            // Bilanz-Reservoir fliesst per Drain-Regel an die Baender.
            for (dst, sh) in bits
                .iter_mut()
                .zip(drain_shares(cf.balance, cf.coded_bands.max(0) as usize, 0).iter())
                .take(end)
            {
                *dst += *sh as f32 / 8.0;
            }
            n += 1;
        }
        let mut db = [[DB_FLOOR; NB]; 2];
        for (dc, pc) in db.iter_mut().zip(pow.iter()).take(ch) {
            for (d, p) in dc.iter_mut().zip(pc.iter()).take(end) {
                *d = 10.0 * (p / n as f32 + 1e-12).log10();
            }
        }
        per_packet.push(Some((db, bits, transient, bits.iter().sum())));
        celt_packets.push(Some(PacketCelt {
            bands_db: db,
            bits,
            channels: ch,
        }));
        silk_packets.push(None);
        hybrid_silk.push(None);
    }
    // Hops: Hop h traegt das Paket seiner Hop-Mitte.
    let map = bin_to_band();
    // Erster Bin in Band 17 (Hybrid-Naht zwischen Huelle und CELT).
    let seam = map.iter().position(|&b| b == 17).unwrap_or(BINS - 1);
    let mut bincount = [0usize; NB];
    for b in map {
        bincount[b] += 1;
    }
    let mut out = Bitstream {
        spectrum: Vec::new(),
        density: Vec::new(),
        available: Vec::new(),
        transient: Vec::new(),
        frame_bits: Vec::new(),
        packets: celt_packets,
        silk_packets,
        hybrid_silk,
        hybrid: Vec::new(),
        silk_available: Vec::new(),
        silk_signal: Vec::new(),
        silk_pitch: Vec::new(),
        silk_gain: Vec::new(),
    };
    let mut h = 0usize;
    loop {
        let center = pre_skip + h * HOP + HOP / 2;
        if center >= total {
            break;
        }
        let pi = bounds.partition_point(|&s| s <= center) - 1;
        let (mut col, mut den) = ([DB_FLOOR; BINS], [0.0f32; BINS]);
        let (mut avail, mut trans, mut fbits) = (false, false, 0.0);
        let (mut s_avail, mut s_sig, mut s_pitch, mut s_gain) = (false, 0i8, 0.0f32, DB_FLOOR);
        if let Some(s) = out.silk_packets.get(pi).and_then(|o| o.as_ref()) {
            (s_avail, s_sig, s_pitch, s_gain) = (true, s.signal, s.pitch_hz, s.gains_db);
            // SILK-Huelle statt Boden: Ansicht 2 zeigt LPC, Ansicht 3 STFT-Huelle.
            col = lpc_envelope_db(&s.nlsf_q15, s.order, s.fs_khz);
        }
        // Hybrid: unten SILK-Huelle, oben CELT (Startband 17) — ein Coder,
        // zwei Schichten wie im Bitstrom.
        let hyb = out.hybrid_silk.get(pi).copied().flatten();
        if let Some((db, bits, tr, fb)) = &per_packet[pi] {
            (avail, trans, fbits) = (true, *tr, *fb);
            // Naht-Pegel der Huelle (wird unten ueberschrieben).
            let env_seam = if hyb.is_some() { Some(col[seam]) } else { None };
            for (j, b) in map.iter().enumerate() {
                if hyb.is_some() && *b < 17 {
                    continue;
                }
                // Kanaele im Leistungs-Mittel; Dichte gleichmaessig je Bin.
                let p = (10.0f32.powf(db[0][*b] / 10.0) + 10.0f32.powf(db[1][*b] / 10.0)) / 2.0;
                col[j] = 10.0 * (p + 1e-12).log10();
                den[j] = bits[*b] / bincount[*b] as f32;
            }
            // Naht-Abgleich: Huellen-Form behalten, Pegel an das gemessene
            // CELT-Band 17 haengen (Peak-0 ist relativ; entfllt wo Band 17
            // uncodiert ist).
            if let Some(env) = env_seam
                && col[seam] > DB_FLOOR + 1.0
            {
                let delta = col[seam] - env;
                for (j, b) in map.iter().enumerate() {
                    if *b < 17 {
                        col[j] += delta;
                    }
                }
            }
        }
        out.spectrum.push(col);
        out.density.push(den);
        out.available.push(avail);
        out.transient.push(trans);
        out.frame_bits.push(fbits);
        out.silk_available.push(s_avail);
        out.silk_signal.push(s_sig);
        out.silk_pitch.push(s_pitch);
        out.silk_gain.push(s_gain);
        out.hybrid.push(hyb);
        h += 1;
    }
    Ok(out)
}

/// Divergenz STFT − Bitstrom in dB (`NaN` wo `available` falsch).
pub fn divergence(stft_col: &[f32; BINS], bs_col: &[f32; BINS], available: bool) -> [f32; BINS] {
    let mut out = [f32::NAN; BINS];
    if available {
        for (o, (s, b)) in out.iter_mut().zip(stft_col.iter().zip(bs_col.iter())) {
            *o = s - b;
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bin_map_covers_all_bands_monotonically() {
        let map = bin_to_band();
        assert!(map.windows(2).all(|w| w[0] <= w[1]));
        assert_eq!(map[0], 0);
        // 20-kHz-Oberkante faellt in Band 20 (12–20 kHz).
        assert_eq!(map[BINS - 1], 20);
        // Jedes Band 0..=20 bekommt mindestens einen Bin.
        for b in 0..=20 {
            assert!(map.contains(&b), "Band {b} ohne Bin");
        }
    }

    #[test]
    fn divergence_is_nan_where_unavailable() {
        let d = divergence(&[0.0; BINS], &[0.0; BINS], false);
        assert!(d.iter().all(|v| v.is_nan()));
        let d = divergence(&[1.0; BINS], &[0.5; BINS], true);
        assert!(d.iter().all(|v| (*v - 0.5).abs() < 1e-6));
    }
}
