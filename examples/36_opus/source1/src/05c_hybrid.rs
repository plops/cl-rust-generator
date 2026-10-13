//! 05c — Hybrid-Pakete: SILK-Praefix + CELT-Oberband auf einem Coder.
//!
//! Ein Hybrid-Frame traegt unten SILK (Sprache) und oben CELT ab Startband
//! 17; beide Schichten teilen sich einen `RangeDec` (`decoder.rs`,
//! Hybrid-Arm): Nach VAD/LBRR-Flags und SILK-Frame laeuft derselbe Coder
//! mit der effektiven CELT-Laenge (nach Hybrid-Redundanz) weiter.
//! Der Walk ist bit-exakt bis in die Allokation (Orakel: Symbole,
//! Coarse-Energien, Offsets, Ebits, Cutoff stimmen mit der Referenz);
//! die Anzeige nutzt Tracker-Energien wo codiert und Boden jenseits des
//! Cutoffs — pulslose Baender darunter bleiben dokumentierte Schaetzung
//! (PVQ-Skip, Spiegel des CELT-Vorgehens).

use crate::alloc::{NB, drain_shares};
use crate::bitstream::{band_of_bin, bitstream_db, silk_bw};
use crate::celt::{CeltState, endband_of, lm_of_ms, parse_celt_frame_rc};
use crate::range::RangeDec;
use crate::silk::{SilkCtx, SilkState, SilkSummary, packet_flags, walk_silk_frame};
use crate::silk_dq::{lpc_envelope_db, summarize};
use crate::silk_tab::{CODE_CONDITIONALLY, CODE_INDEPENDENTLY};
use crate::spectrum::DB_FLOOR;
use crate::types::{Bandwidth, Error};

/// CELT-Startband im Hybrid-Modus (Referenz: `HYBRID_START_BAND`).
const HYBRID_START: usize = 17;

/// Aggregierte Hybrid-Daten eines Pakets (fuer `05b_bitstream`).
pub struct HybridPacket {
    /// Band-Energien in dB je Kanal (Boden jenseits des Cutoffs).
    pub db: [[f32; NB]; 2],
    /// Signal-Bits je Band (CELT + verteilte SILK-Bits).
    pub bits: [f32; NB],
    /// Transient-Oder ueber Frames.
    pub transient: bool,
    /// SILK-Kennzahlen (F0-Lane, JSON, Report).
    pub summary: Option<SilkSummary>,
    /// SILK-Bit-Anteil (Allokations-Identitaet, Hover).
    pub frac: f32,
    /// Erstes Band ohne Ausbau (Decoder versucht dort kein PVQ).
    pub coded_from: usize,
}

/// Ein Hybrid-Paket walken (`None` = Stereo/LBRR/leer — ehrlich grau).
/// `bandwidth`/`frame_ms`/`channels` kommen aus dem TOC.
pub fn walk_hybrid_packet(
    data: &[u8],
    frame_ms: f64,
    bandwidth: Bandwidth,
    channels: usize,
    silk_state: &mut SilkState,
    celt_state: &mut CeltState,
) -> Result<Option<HybridPacket>, Error> {
    let (lm, end, ch) = (
        lm_of_ms(frame_ms),
        endband_of(bandwidth),
        channels.clamp(1, 2),
    );
    let split = crate::split::split(data)?;
    let mut pow = [[0.0f32; NB]; 2];
    let mut bits = [0.0f32; NB];
    let mut n = 0;
    let mut transient = false;
    let mut silk_frames = Vec::new();
    let mut silk_bits_sum = 0i32;
    let mut total_bits_sum = 0i32;
    let mut coded_from = end;
    let mut ok = channels == 1;
    let nb_subfr = if frame_ms < 15.0 { 2 } else { 4 };
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
            bw: silk_bw(bandwidth),
            nb_subfr,
            vad: vad[0],
            cond: if g == 0 {
                CODE_INDEPENDENTLY
            } else {
                CODE_CONDITIONALLY
            },
        };
        silk_frames.push(walk_silk_frame(&mut rc, &ctx, silk_state));
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
        let cf = parse_celt_frame_rc(
            &mut rc,
            effective as i32 * 8,
            lm,
            ch,
            HYBRID_START,
            end,
            celt_state,
        )
        .map_err(Error::BadPacket)?;
        transient |= cf.transient;
        coded_from = coded_from.min(cf.coded_bands.max(0) as usize);
        for (pc, lc) in pow.iter_mut().zip(cf.log_e.iter()).take(ch) {
            for (b, (p, l)) in pc.iter_mut().zip(lc.iter()).enumerate().take(end) {
                // Uncodiert (kein Coarse, kein Fine, kein Rest): die
                // Prediction ist kein Signal — ehrlich Boden.
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
            .zip(drain_shares(cf.balance, cf.coded_bands.max(0) as usize, HYBRID_START).iter())
            .take(end)
        {
            *dst += *sh as f32 / 8.0;
        }
        n += 1;
    }
    if !ok || n == 0 {
        return Ok(None);
    }
    let mut db = [[DB_FLOOR; NB]; 2];
    for (dc, pc) in db.iter_mut().zip(pow.iter()).take(ch) {
        for (d, p) in dc.iter_mut().zip(pc.iter()).take(end) {
            *d = 10.0 * (p / n as f32 + 1e-12).log10();
        }
    }
    // Allokations-Identitaet: SILK-Bits ueber die Baender 0..16 nach
    // Huellen-Leistung verteilen (Form aus NLSF, Summe = gemessene Bits).
    let summary = summarize(&silk_frames);
    if let Some(ref s) = summary {
        let env = lpc_envelope_db(&s.nlsf_q15, s.order, s.fs_khz);
        let mut w = [0.0f32; NB];
        for (j, v) in env.iter().enumerate() {
            let b = band_of_bin(j);
            if b < HYBRID_START {
                w[b] += 10.0f32.powf(v / 10.0);
            }
        }
        let tot: f32 = w.iter().sum();
        for (dst, share) in bits.iter_mut().zip(w.iter()).take(HYBRID_START) {
            let share = if tot > 0.0 { *share / tot } else { 1.0 / 17.0 };
            *dst += share * silk_bits_sum as f32;
        }
    }
    Ok(Some(HybridPacket {
        db,
        bits,
        transient,
        summary,
        frac: silk_bits_sum as f32 / total_bits_sum.max(1) as f32,
        coded_from,
    }))
}
