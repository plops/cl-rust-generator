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

use crate::alloc::{EBANDS, NB, drain_shares};
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
        let mut div_cols = Vec::with_capacity(n);
        let mut density_max = 0.0f32;
        for (h, scol) in stft.iter().enumerate().take(n) {
            div_cols.push(divergence(scol, &bs.spectrum[h], bs.available[h]));
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
    // CELT-Frames sequentiell parsen (Zustand + Reset bei Moduswechsel).
    let mut state = CeltState::new();
    let mut last_mode: Option<Mode> = None;
    // Je Paket: gemittelte Band-Energien (dB) + summierte Bits/Band +
    // Transient-Oder + Signal-Bits (Hover).
    type PacketBands = ([[f32; NB]; 2], [f32; NB], bool, f32);
    let mut per_packet: Vec<Option<PacketBands>> = Vec::new();
    let mut celt_packets: Vec<Option<PacketCelt>> = Vec::new();
    for (p, toc) in packets.iter().zip(tocs.iter()) {
        if Some(toc.mode) != last_mode {
            state.reset();
            last_mode = Some(toc.mode);
        }
        if toc.mode != Mode::Celt {
            per_packet.push(None);
            celt_packets.push(None);
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
        transient: Vec::new(),
        frame_bits: Vec::new(),
        packets: celt_packets,
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
        if let Some((db, bits, tr, fb)) = &per_packet[pi] {
            (avail, trans, fbits) = (true, *tr, *fb);
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
        out.transient.push(trans);
        out.frame_bits.push(fbits);
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
