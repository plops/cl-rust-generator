//! 05b — Bitstrom-Modell: CELT-Energien/Bits → Spektrum-/Dichte-Spalten.
//!
//! Baut aus den geparsten Paketen je 20-ms-Hop (wie [`crate::spectrum`])
//! zwei Spalten: das Bitstrom-Spektrum (21 Encoder-Baender in dB,
//! auf 256 Log-Bins expandiert) und die spektrale Informationsdichte
//! (Bits/Band, gleichmaessig auf die Bins des Bands verteilt).
//! SILK/Hybrid-Hops sind nicht parbar (s. `03c_celt.rs`) und liefern
//! Boden (`DB_FLOOR`, Dichte 0, `available = false`).
//!
//! [`divergence`] ist die punktweise Differenz STFT − Bitstrom in dB
//! (`NaN` wo unverfuegbar — ehrlich statt 0 dB vorgetaeuschter
//! Uebereinstimmung).

use opus_pure::OggOpusReader;

use crate::alloc::{EBANDS, NB};
use crate::celt::{CeltState, endband_of, energy_db, lm_of_ms, parse_celt_frame};
use crate::spectrum::{BINS, DB_FLOOR, FMAX, FMIN, HOP, RATE};
use crate::types::{Error, Mode};

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

/// Bitstrom-Spalten einer Datei, hop-aligned zu [`crate::spectrum::stft`].
pub struct Bitstream {
    /// dB-Spalten (`DB_FLOOR` wo unverfuegbar).
    pub spectrum: Vec<[f32; BINS]>,
    /// Bits je Bin (summiert ≈ Frame-Bits; 0 wo unverfuegbar).
    pub density: Vec<[f32; BINS]>,
    /// `true` wo ein CELT-Paket den Hop traegt.
    pub available: Vec<bool>,
}

/// Parst alle Pakete und baut die Hop-Spalten (Pre-Skip beachtet).
pub fn analyze_bytes(data: &[u8]) -> Result<Bitstream, Error> {
    let mut reader = OggOpusReader::new(data)?;
    let pre_skip = reader.head().pre_skip as usize;
    let mut packets = Vec::new();
    while let Some(p) = reader.read_packet()? {
        packets.push(p.data);
    }
    // Paketgrenzen in Stream-Samples (48 kHz).
    let mut bounds = Vec::with_capacity(packets.len() + 1);
    bounds.push(0usize);
    for p in &packets {
        let toc = crate::toc::parse(p)?;
        let last = *bounds.last().unwrap();
        bounds.push(last + (toc.duration_ms * 48.0) as usize);
    }
    let total = *bounds.last().unwrap();
    // CELT-Frames sequentiell parsen (Zustand + Reset bei Moduswechsel).
    let mut state = CeltState::new();
    let mut last_mode: Option<Mode> = None;
    // Je Paket: gemittelte Band-Energien (dB) + summierte Bits/Band.
    type PacketBands = ([[f32; NB]; 2], [f32; NB]);
    let mut per_packet: Vec<Option<PacketBands>> = Vec::new();
    for p in &packets {
        let toc = crate::toc::parse(p)?;
        if Some(toc.mode) != last_mode {
            state.reset();
            last_mode = Some(toc.mode);
        }
        if toc.mode != Mode::Celt {
            per_packet.push(None);
            continue;
        }
        let split = crate::split::split(p)?;
        let (lm, end, ch) = (
            lm_of_ms(toc.frame_ms),
            endband_of(toc.bandwidth),
            toc.channels as usize,
        );
        let mut pow = [[0.0f32; NB]; 2];
        let mut bits = [0.0f32; NB];
        let mut n = 0;
        for f in &split.frames {
            let cf = parse_celt_frame(f, lm, ch, 0, end, &mut state).map_err(Error::BadPacket)?;
            for (pc, lc) in pow.iter_mut().zip(cf.log_e.iter()).take(ch) {
                for (b, (p, l)) in pc.iter_mut().zip(lc.iter()).enumerate().take(end) {
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
            n += 1;
        }
        let mut db = [[DB_FLOOR; NB]; 2];
        for (dc, pc) in db.iter_mut().zip(pow.iter()).take(ch) {
            for (d, p) in dc.iter_mut().zip(pc.iter()).take(end) {
                *d = 10.0 * (p / n as f32 + 1e-12).log10();
            }
        }
        per_packet.push(Some((db, bits)));
    }
    // Hops: Hop h traegt das Paket seiner Hop-Mitte.
    let map = bin_to_band();
    let mut bincount = [0usize; NB];
    for b in map {
        bincount[b] += 1;
    }
    let mut out = Bitstream {
        spectrum: Vec::new(),
        density: Vec::new(),
        available: Vec::new(),
    };
    let mut h = 0usize;
    loop {
        let center = pre_skip + h * HOP + HOP / 2;
        if center >= total {
            break;
        }
        let pi = bounds.partition_point(|&s| s <= center) - 1;
        let (mut col, mut den) = ([DB_FLOOR; BINS], [0.0f32; BINS]);
        let mut avail = false;
        if let Some((db, bits)) = &per_packet[pi] {
            avail = true;
            for (j, b) in map.iter().enumerate() {
                // Kanaele im Leistungs-Mittel; Dichte gleichmaessig je Bin.
                let p = (10.0f32.powf(db[0][*b] / 10.0) + 10.0f32.powf(db[1][*b] / 10.0)) / 2.0;
                col[j] = 10.0 * (p + 1e-12).log10();
                den[j] = bits[*b] / bincount[*b] as f32;
            }
        }
        out.spectrum.push(col);
        out.density.push(den);
        out.available.push(avail);
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
