//! 09b — Modell: Walk-Ergebnisse je Paket → Hops → Spalten der Ansichten.
//!
//! Ein Paket ([`PacketInfo`]) fasst seine Opus-Frames zusammen (CELT-Baender,
//! SILK-Kennzahlen, Bit-Budget, `final_range`). Jeder 20-ms-Hop der STFT
//! ([`crate::spectrum`]) zeigt auf *ein* Paket (das seiner Hop-Mitte,
//! Pre-Skip beachtet) und bekommt zwei Spalten:
//!
//! - **Bitstrom**: CELT-Bandenergien in dBFS, darunter (SILK/Hybrid) die
//!   LPC-Huelle (Peak = 0 dB, relativ).
//! - **Dichte**: *gelesene* Bits je Log-Bin. CELT-Baender tragen ihre
//!   gemessenen Bits (Coarse+Fine+PVQ+Finalise), SILK-Bits werden nach
//!   gedaempfter Huelle ueber 0..Nyquist verteilt (SILK kennt keine Baender —
//!   das ist die einzige Modellannahme, im Handbuch erklaert).
//!
//! `source1` hielt dieselben Daten in 13 parallelen Vektoren; hier gibt es
//! genau eine Liste je Ebene (`packets`, `hops`) plus die Spalten.

use crate::alloc::{EBANDS, NB};
use crate::celt::{CeltFrame, energy_db};
use crate::silk::SilkSummary;
use crate::silk_dq::{lpc_envelope_db, summarize};
use crate::spectrum::{BINS, DB_FLOOR, FMAX, FMIN, HOP, RATE};
use crate::walk::{Budget, PacketWalk};

/// Preemphasis-Koeffizient (CELT @48 kHz): y[i] = x[i] − 0,85·x[i−1].
const PREEMPH: f32 = 0.85;
/// Kalibrierung Bitstrom → dBFS (MDCT-Gain + int16-Skala + Fenster),
/// gemessen an der Sinus-Fixture (`tests/spectrum.rs`). `source1` hatte
/// -76,5: dort mittelte Mono den leeren 2. Kanal mit (-3 dB) und die
/// Energie-Historie lief ohne Finalise (~2 dB Drift) — beides behoben.
const CAL_DB: f32 = -81.5;

/// De-Emphasis-Korrektur je Band in dB (Encoder misst preemphasiert).
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
    let mut b = 0;
    while b + 1 < NB && (EBANDS[b + 1] * 200) as f32 <= f {
        b += 1;
    }
    b
}

/// CELT-Banddaten eines Pakets (ueber seine Frames gemittelt/summiert).
#[derive(Debug, Clone, PartialEq)]
pub struct CeltInfo {
    /// Band-Energien in dBFS je Kanal (`DB_FLOOR` wo uncodiert).
    pub bands_db: [[f32; NB]; 2],
    /// Gelesene Bits je Band (Summe ueber Frames).
    pub bits: [f32; NB],
    /// PVQ-Pulse je Band (Summe ueber Frames).
    pub pulses: [u32; NB],
    pub channels: usize,
    /// Erstes CELT-Band (0, Hybrid 17).
    pub start: usize,
    /// Erstes Band ohne Ausbau (Allokation `coded_bands`).
    pub coded_to: usize,
    pub transient: bool,
    /// Prefilter-Periode in Samples (Pitch-Kammfilter), falls aktiv.
    pub prefilter: Option<u32>,
}

/// Alles Darstellbare eines Pakets.
#[derive(Debug, Clone, Default)]
pub struct PacketInfo {
    pub celt: Option<CeltInfo>,
    pub silk: Option<SilkSummary>,
    /// Bit-Budget, Summe ueber die Opus-Frames (1/8 Bit).
    pub budget: Budget,
    /// Range-Coder-Endzustand (letzter Frame; 0 bei PLC wie libopus).
    pub final_range: u32,
    /// Redundanz-Frame: `Some(true)` CELT→SILK, `Some(false)` SILK→CELT.
    pub redundancy: Option<bool>,
    /// LBRR-Kopien (In-Band-FEC) im Paket.
    pub lbrr_frames: usize,
    /// SILK-Frames mit nur Mid-Kanal (Stereo).
    pub mid_only: usize,
    /// Leere Frames (DTX/PLC).
    pub plc_frames: usize,
}

impl PacketInfo {
    /// Aus einem gewalkten Paket zusammenfassen.
    #[must_use]
    pub fn from_walk(pw: &PacketWalk) -> Self {
        let mut info = PacketInfo {
            final_range: pw.frames.last().map_or(0, |f| f.rng),
            ..PacketInfo::default()
        };
        let mut celt_frames: Vec<&CeltFrame> = Vec::new();
        let mut mids = Vec::new();
        for f in &pw.frames {
            let b = f.budget();
            info.budget = add_budget(&info.budget, &b);
            info.plc_frames += usize::from(f.plc);
            if let Some(r) = &f.redundancy {
                info.redundancy = Some(r.celt_to_silk);
            }
            if let Some(s) = &f.silk {
                info.lbrr_frames += s.lbrr.len();
                info.mid_only += s.stereo.iter().filter(|x| x.mid_only).count();
                for (fr, ch) in s.frames.iter().zip(s.channel.iter()) {
                    if *ch == 0 {
                        mids.push(fr.clone());
                    }
                }
            }
            if let Some(c) = &f.celt {
                celt_frames.push(c);
            }
        }
        info.silk = summarize(&mids);
        info.celt = celt_info(&celt_frames);
        info
    }

    /// Gelesene Bits des Pakets (ohne Redundanz, ohne ungenutzte).
    #[must_use]
    pub fn used_bits(&self) -> f32 {
        let b = &self.budget;
        (b.total() - b.unused - b.redundancy) as f32 / 8.0
    }
}

fn add_budget(a: &Budget, b: &Budget) -> Budget {
    Budget {
        init: a.init + b.init,
        silk_flags: a.silk_flags + b.silk_flags,
        silk_lbrr: a.silk_lbrr + b.silk_lbrr,
        silk_stereo: a.silk_stereo + b.silk_stereo,
        silk_side: a.silk_side + b.silk_side,
        silk_pulses: a.silk_pulses + b.silk_pulses,
        red_header: a.red_header + b.red_header,
        celt_header: a.celt_header + b.celt_header,
        celt_energy: a.celt_energy + b.celt_energy,
        celt_pvq: a.celt_pvq + b.celt_pvq,
        redundancy: a.redundancy + b.redundancy,
        unused: a.unused + b.unused,
    }
}

/// CELT-Frames eines Pakets zu Energien/Bits/Pulsen verdichten.
fn celt_info(frames: &[&CeltFrame]) -> Option<CeltInfo> {
    let first = frames.first()?;
    let (ch, start) = (first.channels, first.start);
    let mut pow = [[0.0f32; NB]; 2];
    let mut bits = [0.0f32; NB];
    let mut pulses = [0u32; NB];
    let (mut transient, mut prefilter, mut coded_to) = (false, None, first.end);
    for f in frames {
        transient |= f.transient;
        prefilter = prefilter.or(f.prefilter.map(|p| p.0));
        coded_to = coded_to.min(f.coded_bands.max(0) as usize);
        for b in f.start..f.end {
            bits[b] += f.band_bits8[b] as f32 / 8.0;
            pulses[b] += f.pulses[b];
            // Uncodiert (weder Allokation noch Fine): Praediktion ist kein
            // Signal — ehrlich Boden statt Geister-Pegel.
            if f.alloc8[b] == 0 && f.ebits[b] == 0 {
                continue;
            }
            for (c, pc) in pow.iter_mut().enumerate().take(ch) {
                pc[b] += 10.0f32.powf(bitstream_db(f.log_e[c][b], b) / 10.0);
            }
        }
    }
    let n = frames.len() as f32;
    let mut bands_db = [[DB_FLOOR; NB]; 2];
    for (dc, pc) in bands_db.iter_mut().zip(pow.iter()).take(ch) {
        for (d, p) in dc.iter_mut().zip(pc.iter()).skip(start) {
            if *p > 0.0 {
                *d = 10.0 * (p / n).log10();
            }
        }
    }
    Some(CeltInfo {
        bands_db,
        bits,
        pulses,
        channels: ch,
        start,
        coded_to,
        transient,
        prefilter,
    })
}

/// Kennzahlen je 20-ms-Hop (Hover, Lanes, Fakten).
#[derive(Debug, Clone, Copy, Default, PartialEq)]
pub struct HopInfo {
    /// Paket-Index der Hop-Mitte.
    pub packet: usize,
    /// CELT-Schicht vorhanden.
    pub celt: bool,
    /// SILK-Schicht vorhanden.
    pub silk: bool,
    pub transient: bool,
    /// Gelesene Bits des Pakets.
    pub frame_bits: f32,
    /// SILK: 0 inaktiv, 1 stimmlos, 2 stimmhaft.
    pub silk_signal: i8,
    /// SILK: mittlere Tonhoehe in Hz (0 = stimmlos).
    pub silk_pitch: f32,
    /// SILK: mittlerer Gain in dB.
    pub silk_gain: f32,
    /// Hybrid: SILK-Anteil an den gelesenen Bits.
    pub hybrid_silk: Option<f32>,
}

/// Modell einer Datei: Pakete, Hops, Spalten.
pub struct Model {
    pub packets: Vec<PacketInfo>,
    pub hops: Vec<HopInfo>,
    /// Bitstrom-Spalten (dB).
    pub bitstream: Vec<[f32; BINS]>,
    /// Dichte-Spalten (gelesene Bits je Bin).
    pub density: Vec<[f32; BINS]>,
}

impl Model {
    /// Bequemer Einstieg: Paketliste walken und Modell bauen.
    pub fn from_set(ps: &crate::analyze::PacketSet) -> Result<Self, crate::types::Error> {
        let walks = crate::walk::walk_packets(ps)?;
        Ok(Self::build(&walks, usize::from(ps.head.pre_skip)))
    }

    /// Aus gewalkten Paketen bauen; `pre_skip` in 48-kHz-Samples.
    #[must_use]
    pub fn build(walks: &[PacketWalk], pre_skip: usize) -> Self {
        let packets: Vec<PacketInfo> = walks.iter().map(PacketInfo::from_walk).collect();
        let mut bounds = Vec::with_capacity(walks.len() + 1);
        bounds.push(0usize);
        for w in walks {
            let last = *bounds.last().unwrap_or(&0);
            bounds.push(last + (w.toc.duration_ms * 48.0) as usize);
        }
        let total = *bounds.last().unwrap_or(&0);
        let map: Vec<usize> = (0..BINS).map(band_of_bin).collect();
        let mut bincount = [0usize; NB];
        for b in &map {
            bincount[*b] += 1;
        }
        let mut m = Model {
            packets,
            hops: Vec::new(),
            bitstream: Vec::new(),
            density: Vec::new(),
        };
        let mut h = 0usize;
        loop {
            let center = pre_skip + h * HOP + HOP / 2;
            if center >= total {
                break;
            }
            let pi = bounds.partition_point(|&s| s <= center) - 1;
            let (hop, col, den) = hop_columns(&m.packets[pi], pi, &map, &bincount);
            m.hops.push(hop);
            m.bitstream.push(col);
            m.density.push(den);
            h += 1;
        }
        m
    }
}

/// Spalten + Kennzahlen eines Hops aus seinem Paket.
fn hop_columns(
    p: &PacketInfo,
    pi: usize,
    map: &[usize],
    bincount: &[usize; NB],
) -> (HopInfo, [f32; BINS], [f32; BINS]) {
    let mut col = [DB_FLOOR; BINS];
    let mut den = [0.0f32; BINS];
    let mut hop = HopInfo {
        packet: pi,
        frame_bits: p.used_bits(),
        silk_gain: DB_FLOOR,
        ..HopInfo::default()
    };
    let b = &p.budget;
    let silk_bits =
        (b.silk_flags + b.silk_lbrr + b.silk_stereo + b.silk_side + b.silk_pulses) as f32 / 8.0;
    if let Some(s) = &p.silk {
        hop.silk = true;
        (hop.silk_signal, hop.silk_pitch, hop.silk_gain) = (s.signal, s.pitch_hz, s.gains_db);
        let env = lpc_envelope_db(&s.nlsf_q15, s.order, s.fs_khz);
        col = env;
        // SILK-Bits bis zur SILK-Nyquist verteilen, gewichtet mit der
        // *gedaempften* Huelle (4. Wurzel der Leistung): Die rohe Huelle hat
        // 40+ dB Dynamik und kippte alle Bits in einen Formant-Bin.
        let ny = s.fs_khz as f32 * 500.0;
        let w: Vec<f32> = env
            .iter()
            .enumerate()
            .map(|(j, v)| {
                let f = FMIN * (FMAX / FMIN).powf((j as f32 + 0.5) / BINS as f32);
                if f <= ny { 10.0f32.powf(v / 40.0) } else { 0.0 }
            })
            .collect();
        let tot: f32 = w.iter().sum();
        if tot > 0.0 {
            for (d, wj) in den.iter_mut().zip(w.iter()) {
                *d += silk_bits * wj / tot;
            }
        }
    }
    if let Some(c) = &p.celt {
        hop.celt = true;
        hop.transient = c.transient;
        if p.silk.is_some() {
            hop.hybrid_silk = Some(silk_bits / hop.frame_bits.max(1e-6));
        }
        for (j, band) in map.iter().enumerate() {
            if *band < c.start {
                continue;
            }
            let pw = (10.0f32.powf(c.bands_db[0][*band] / 10.0)
                + 10.0f32.powf(c.bands_db[c.channels - 1][*band] / 10.0))
                / 2.0;
            col[j] = 10.0 * (pw + 1e-12).log10();
            den[j] += c.bits[*band] / bincount[*band] as f32;
        }
    }
    (hop, col, den)
}

/// Wasserfall-Quelle (Ansicht 1–4).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Source {
    /// Decodiertes STFT-Spektrum in dB.
    #[default]
    Spectrum,
    /// Encoder-Modell: CELT-Baender + SILK-Huelle in dB.
    Bitstream,
    /// STFT − Bitstrom in dB (divergierend).
    Divergence,
    /// Gelesene Bits je Bin (Informationsdichte).
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

/// Alle vier Spalten-Saetze auf gemeinsamer Laenge (Renderer, Viewer).
pub struct Sources {
    /// STFT-Spalten (dB).
    pub spectrum: Vec<[f32; BINS]>,
    /// Modell (Pakete, Hops, Bitstrom-/Dichte-Spalten).
    pub model: Model,
    /// Divergenz-Spalten (dB, NaN wo kein Modell).
    pub divergence: Vec<[f32; BINS]>,
    /// Dichte-Normierung: 99-%-Perzentil aller Bin-Werte (Ausreisser-fest).
    pub density_max: f32,
}

impl Sources {
    /// STFT + Modell zusammenfuehren (kuerzt auf die kleinere Hop-Zahl).
    #[must_use]
    pub fn build(mut stft: Vec<[f32; BINS]>, mut model: Model) -> Self {
        let n = stft.len().min(model.hops.len());
        stft.truncate(n);
        model.hops.truncate(n);
        model.bitstream.truncate(n);
        model.density.truncate(n);
        let mut divergence = Vec::with_capacity(n);
        for ((s, hop), (bs, den)) in stft
            .iter()
            .zip(&model.hops)
            .zip(model.bitstream.iter().zip(&model.density))
        {
            let _ = den;
            divergence.push(divergence_col(s, bs, hop.celt || hop.silk));
        }
        let density_max = percentile99(&model.density);
        Self {
            spectrum: stft,
            model,
            divergence,
            density_max: density_max.max(1e-6),
        }
    }

    /// Spalten einer Quelle.
    #[must_use]
    pub fn cols(&self, src: Source) -> &[[f32; BINS]] {
        match src {
            Source::Spectrum => &self.spectrum,
            Source::Bitstream => &self.model.bitstream,
            Source::Divergence => &self.divergence,
            Source::Density => &self.model.density,
        }
    }
}

/// 99-%-Perzentil der positiven Dichtewerte (Farbskala der Ansicht 4).
fn percentile99(cols: &[[f32; BINS]]) -> f32 {
    let mut v: Vec<f32> = cols
        .iter()
        .flatten()
        .copied()
        .filter(|x| *x > 0.0)
        .collect();
    if v.is_empty() {
        return 1e-6;
    }
    v.sort_by(f32::total_cmp);
    v[(v.len() * 99 / 100).min(v.len() - 1)]
}

/// Divergenz STFT − Bitstrom in dB (`NaN` wo kein Modell).
pub fn divergence_col(stft: &[f32; BINS], bs: &[f32; BINS], available: bool) -> [f32; BINS] {
    let mut out = [f32::NAN; BINS];
    if available {
        for (o, (s, b)) in out.iter_mut().zip(stft.iter().zip(bs.iter())) {
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
        let map: Vec<usize> = (0..BINS).map(band_of_bin).collect();
        assert!(map.windows(2).all(|w| w[0] <= w[1]));
        assert_eq!((map[0], map[BINS - 1]), (0, 20));
        for b in 0..NB {
            assert!(map.contains(&b), "Band {b} ohne Bin");
        }
    }

    #[test]
    fn divergence_is_nan_where_unavailable() {
        assert!(
            divergence_col(&[0.0; BINS], &[0.0; BINS], false)
                .iter()
                .all(|v| v.is_nan())
        );
        let d = divergence_col(&[1.0; BINS], &[0.5; BINS], true);
        assert!(d.iter().all(|v| (*v - 0.5).abs() < 1e-6));
    }

    #[test]
    fn density_sums_to_read_bits_on_real_fixture() {
        // Jede Hop-Spalte verteilt genau die gelesenen Bits ihres Pakets.
        let path = concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/data/gen/switch_voip.opus"
        );
        let bytes = std::fs::read(path).expect("fixture");
        let ps = crate::analyze::read_packets(&bytes, bytes.len() as u64).expect("ogg");
        let mut w = crate::walk::Walker::new();
        let walks: Vec<_> = ps
            .packets
            .iter()
            .map(|p| w.packet(&p.data).expect("walk"))
            .collect();
        let m = Model::build(&walks, ps.head.pre_skip as usize);
        assert!(
            m.hops.iter().any(|h| h.celt && h.silk) && m.hops.iter().any(|h| h.celt && !h.silk)
        );
        for (h, d) in m.hops.iter().zip(m.density.iter()) {
            let p = &m.packets[h.packet];
            let b = &p.budget;
            let want = (b.silk_flags
                + b.silk_lbrr
                + b.silk_stereo
                + b.silk_side
                + b.silk_pulses
                + b.celt_header
                + b.celt_energy
                + b.celt_pvq) as f32
                / 8.0;
            let celt_hdr = b.celt_header as f32 / 8.0;
            let got: f32 = d.iter().sum();
            // CELT-Header-Bits gehoeren keinem Band (nur Energie+PVQ je Band).
            assert!(
                (got - (want - celt_hdr)).abs() < 0.05 * want.max(1.0),
                "{got} vs {want}"
            );
        }
    }
}
