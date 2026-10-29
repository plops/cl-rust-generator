//! Einheitlicher Level-0-Ingest, Demultiplexer und Mehrkern-FDBAQ-Entpacker
//! für Stripmap (SM), Interferometric Wide Swath (IW) und Extra Wide Swath (EW).
//!
//! Datenfluss:
//! 1. Speichergemappte `.dat`-Datei (`memmap2::Mmap`) scannen (`scan_packet_headers`).
//! 2. Sub-kommutierte 64-Worte-Orbitblöcke extrahieren (`extract_ephemeris`).
//! 3. Aufnahmemodus (`SarMode::Stripmap | Iw | Ew`) anhand der `swath_number`-Verteilung
//!    der nominalen Echo-Pakete erkennen.
//! 4. Echo-Pakete nach `swath_number` und PRI-Kontinuität (`pri_diff <= 50`) in
//!    konsistente `BurstBlock`s gliedern.
//! 5. Innerhalb jedes Blocks:
//!    - Gemeinsames Range-Fenster $[t_{\text{fast,min}}, t_{\text{fast,max}}]$ bestimmen
//!      und auf glatte FFT-Größe $N_{\text{rg}}$ (`smooth_fft_len`) runden.
//!    - Fehlende PRI-Zeilen im Raster `pri_count - pri_count_0` mit Nullen auffüllen.
//!    - Alle Echo-Pakete des Blocks parallel via `std::thread::scope` direkt in die
//!      SWST-verschobenen Zielzeilen dekodieren.
//!    - Sendezeitpunkte durch lineare Regression über den PRI-Index glätten und
//!      Orbit/Geschwindigkeit/$V_{\text{eff}}(r)$ berechnen.

use crate::chirp::{
    decode_rgdec_fs_hz, decode_txprr_hz_s, decode_txpsf_hz, suppressed_data_time_s,
};
use crate::ephem::{EphemPoint, compute_veff_vector, extract_ephemeris, interp_pos, interp_vel};
use crate::l0::{PacketRecord, decode_packet_into, scan_packet_headers};
use crate::types::{BurstBlock, C_LIGHT, Complex32, Error, F_REF, RadarParams, SarMode, SwathData};
use memmap2::Mmap;
use std::collections::BTreeMap;
use std::fs::File;
use std::path::Path;

/// Vollständiger Datensatz einer Level-0 `.dat`-Datei nach Demultiplexing.
pub struct L0Dataset {
    pub mode: SarMode,
    pub total_packets: usize,
    pub echo_packets: usize,
    pub ephemeris: Vec<EphemPoint>,
    pub swaths: Vec<SwathData>,
}

/// Kleinste glatte FFT-Länge $\ge n$ mit Primfaktoren $\in \{2, 3, 5, 7\}$.
pub fn smooth_fft_len(n: usize) -> usize {
    fn is_smooth(mut m: usize) -> bool {
        if m == 0 {
            return false;
        }
        for p in [2, 3, 5, 7] {
            while m.is_multiple_of(p) {
                m /= p;
            }
        }
        m == 1
    }
    (n.max(1)..).find(|&m| is_smooth(m)).unwrap_or(n)
}

/// Menschenlesbarer Sub-Swath-Name aus `swath_number`.
pub fn swath_name(swath_number: u8) -> String {
    match swath_number {
        1..=6 => format!("S{swath_number}"),
        10..=12 => format!("IW{}", swath_number - 9),
        20..=24 => format!("EW{}", swath_number - 19),
        other => format!("SW{other}"),
    }
}

/// Erkennt den SAR-Modus aus den `swath_number`-Werten der Echo-Pakete.
pub fn detect_sar_mode(records: &[PacketRecord]) -> Result<SarMode, Error> {
    let mut sm = 0usize;
    let mut iw = 0usize;
    let mut ew = 0usize;
    for r in records {
        if r.is_nominal_echo() {
            match r.swath_number {
                1..=6 => sm += 1,
                10..=12 => iw += 1,
                20..=24 => ew += 1,
                _ => {}
            }
        }
    }
    if iw > sm && iw >= ew {
        Ok(SarMode::Iw)
    } else if ew > sm && ew > iw {
        Ok(SarMode::Ew)
    } else if sm > 0 {
        Ok(SarMode::Stripmap)
    } else {
        Err(Error("Keine nominalen SM/IW/EW-Echopakete gefunden".into()))
    }
}

/// Berechnet `RadarParams` aus einem repräsentativen Header und einem gemeinsamen `fast_time_start_s`.
pub fn build_radar_params(
    rec: &PacketRecord,
    fast_time_start_s: f64,
    swl_s: f64,
) -> Result<RadarParams, Error> {
    let fs_hz = decode_rgdec_fs_hz(rec.rgdec)?;
    let pri_s = f64::from(rec.pri_code) / F_REF;
    let prf_hz = 1.0 / pri_s;
    let swst_s = f64::from(rec.swst_code) / F_REF;
    let tx_ramp_rate_hz_s = decode_txprr_hz_s(rec.txprr_code);
    let tx_start_freq_hz = decode_txpsf_hz(rec.txpsf_code, tx_ramp_rate_hz_s);
    let tx_pulse_length_s = f64::from(rec.txpl_code) / F_REF;
    let r0_m = 0.5 * C_LIGHT * fast_time_start_s;
    let dr_m = 0.5 * C_LIGHT / fs_hz;
    let chirp_samples = (tx_pulse_length_s * fs_hz) as usize;

    Ok(RadarParams {
        swath_number: rec.swath_number,
        elevation_beam: rec.elevation_beam_address,
        rgdec: rec.rgdec,
        rank: rec.rank,
        fs_hz,
        prf_hz,
        pri_s,
        swst_s,
        swl_s,
        tx_ramp_rate_hz_s,
        tx_start_freq_hz,
        tx_pulse_length_s,
        fast_time_start_s,
        r0_m,
        dr_m,
        chirp_samples,
    })
}

#[inline]
fn fast_time_of(rec: &PacketRecord) -> f64 {
    let pri_s = f64::from(rec.pri_code) / F_REF;
    let swst_s = f64::from(rec.swst_code) / F_REF;
    f64::from(rec.rank) * pri_s + swst_s + suppressed_data_time_s()
}

/// Dekodiert eine Liste zusammengehöriger Echo-Pakete in einen ausgerichteten `BurstBlock`.
///
/// Wenn `align_fast_time_start_opt` angegeben ist (z.B. gemeinsamer Start über alle Bursts
/// eines Sub-Swaths), wird jedes Paket relativ zu diesem Startzeitpunkt ausgerichtet.
pub fn decode_burst_block(
    file_bytes: &[u8],
    records: &[PacketRecord],
    ephem: &[EphemPoint],
    burst_index: usize,
    align_fast_time_start_opt: Option<f64>,
    align_n_rg_opt: Option<usize>,
) -> Result<BurstBlock, Error> {
    if records.is_empty() {
        return Err(Error("Leerer BurstBlock".into()));
    }
    let r0 = &records[0];
    let fs_hz = decode_rgdec_fs_hz(r0.rgdec)?;
    let pri_s = f64::from(r0.pri_code) / F_REF;

    // 1. PRI-Raster bestimmen (inkl. Lückenfüllung bis max_pri_span).
    let pri0 = r0.pri_count;
    let mut row_of_pkt = Vec::with_capacity(records.len());
    let mut cur_row = 0usize;
    row_of_pkt.push(0usize);
    for w in records.windows(2) {
        let dp = w[1].pri_count.wrapping_sub(w[0].pri_count) as usize;
        let step = if (1..=64).contains(&dp) { dp } else { 1 };
        cur_row += step;
        row_of_pkt.push(cur_row);
    }
    let n_az = cur_row + 1;

    // 2. Gemeinsame Range-Ausrichtung über Fast-Time
    let t_fast_min = align_fast_time_start_opt.unwrap_or_else(|| {
        records
            .iter()
            .map(fast_time_of)
            .fold(f64::INFINITY, f64::min)
    });
    let bases: Vec<usize> = records
        .iter()
        .map(|r| ((fast_time_of(r) - t_fast_min).max(0.0) * fs_hz).round() as usize)
        .collect();
    let max_len = records
        .iter()
        .zip(bases.iter())
        .map(|(r, &b)| b + r.num_samples())
        .max()
        .unwrap_or(0);
    let n_rg = align_n_rg_opt.unwrap_or_else(|| smooth_fft_len(max_len));
    let swl_s = (n_rg as f64) / fs_hz;
    let params = build_radar_params(r0, t_fast_min, swl_s)?;

    // 3. Mehrsträngige Paketdekodierung direkt in die Zielzeilen
    let mut raw = vec![Complex32::ZERO; n_az * n_rg];
    decode_packets_parallel(file_bytes, records, &row_of_pkt, &bases, n_rg, &mut raw)?;

    // 4. Zeitregression und Azimuth-Beam-Address-Tracking
    let mid_pkt = records.len() / 2;
    let mid_row = row_of_pkt[mid_pkt];
    let t_mid = records[mid_pkt].timestamp_s();
    let pulse_times: Vec<f64> = (0..n_az)
        .map(|m| t_mid + (m as f64 - mid_row as f64) * pri_s)
        .collect();

    let mut az_beam_addr = vec![r0.azimuth_beam_address; n_az];
    for (k, rec) in records.iter().enumerate() {
        let row = row_of_pkt[k];
        az_beam_addr[row] = rec.azimuth_beam_address;
    }
    // Lücken in az_beam_addr durch Nachbarwert füllen
    for m in 1..n_az {
        if az_beam_addr[m] == r0.azimuth_beam_address && m > 0 {
            az_beam_addr[m] = az_beam_addr[m - 1];
        }
    }

    // 5. Orbit-Interpolation und V_eff
    let (pos_ecef, vel_ecef, veff) = if !ephem.is_empty() {
        let pos: Vec<_> = pulse_times.iter().map(|&t| interp_pos(ephem, t)).collect();
        let vel: Vec<_> = pulse_times.iter().map(|&t| interp_vel(ephem, t)).collect();
        let mid = n_az / 2;
        let v = compute_veff_vector(pos[mid], vel[mid], params.r0_m, params.dr_m, n_rg);
        (pos, vel, v)
    } else {
        (Vec::new(), Vec::new(), vec![7100.0; n_rg])
    };

    let _ = pri0;
    Ok(BurstBlock {
        burst_index,
        swath_number: r0.swath_number,
        params,
        n_az,
        n_rg,
        raw,
        pulse_times,
        az_beam_addr,
        veff,
        pos_ecef,
        vel_ecef,
    })
}

/// Dekodiert `records` parallel mit `std::thread::scope` in die Zielmatrix `raw` (`[n_az × n_rg]`).
pub fn decode_packets_parallel(
    file_bytes: &[u8],
    records: &[PacketRecord],
    row_of_pkt: &[usize],
    bases: &[usize],
    n_rg: usize,
    raw: &mut [Complex32],
) -> Result<(), Error> {
    if records.is_empty() {
        return Ok(());
    }
    let n_pkts = records.len();
    let n_threads = std::thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(4)
        .min(n_pkts.max(1));
    let pkts_per_thread = n_pkts.div_ceil(n_threads);

    #[derive(Clone, Copy)]
    struct SendPtr(*mut Complex32);
    unsafe impl Send for SendPtr {}
    unsafe impl Sync for SendPtr {}
    impl SendPtr {
        #[inline]
        unsafe fn row_slice_mut(self, row: usize, n_rg: usize) -> &'static mut [Complex32] {
            unsafe { std::slice::from_raw_parts_mut(self.0.add(row * n_rg), n_rg) }
        }
    }

    let ptr = SendPtr(raw.as_mut_ptr());
    let mut thread_err: Option<Error> = None;

    std::thread::scope(|s| {
        let mut handles = Vec::with_capacity(n_threads);
        for t in 0..n_threads {
            let p0 = t * pkts_per_thread;
            let p1 = ((t + 1) * pkts_per_thread).min(n_pkts);
            if p0 >= p1 {
                continue;
            }
            handles.push(s.spawn(move || -> Result<(), Error> {
                let mut scratch = Vec::new();
                for k in p0..p1 {
                    let rec = &records[k];
                    let row = row_of_pkt[k];
                    let base = bases[k];
                    let n_samp = rec.num_samples();
                    // SAFETY: Jedes Paket `k` hat einen streng monoton steigenden `row = row_of_pkt[k]`,
                    // sodass keine zwei Threads jemals dieselbe Zeile beschreiben.
                    let dst_row = unsafe { ptr.row_slice_mut(row, n_rg) };
                    if base + n_samp <= n_rg {
                        decode_packet_into(file_bytes, rec, &mut dst_row[base..base + n_samp])?;
                    } else {
                        scratch.resize(n_samp, Complex32::ZERO);
                        decode_packet_into(file_bytes, rec, &mut scratch)?;
                        let take = n_rg.saturating_sub(base).min(n_samp);
                        dst_row[base..base + take].copy_from_slice(&scratch[..take]);
                    }
                }
                Ok(())
            }));
        }
        for h in handles {
            if let Ok(Err(e)) = h.join()
                && thread_err.is_none()
            {
                thread_err = Some(e);
            }
        }
    });

    if let Some(e) = thread_err {
        return Err(e);
    }
    Ok(())
}

/// Lädt eine Sentinel-1 Level-0 `.dat`-Datei, erkennt den Modus (SM, IW, EW) und
/// demultiplext alle nominalen Echos in `SwathData` und `BurstBlock`s.
///
/// Optional begrenzen `az_window` (für Stripmap: `Some((az0, az1))`) oder `burst_range`
/// (für TOPS: `Some((b0, b1))`) die geladene Datenmenge.
pub fn load_l0_dataset(
    path: &Path,
    az_window: Option<(usize, usize)>,
    burst_range: Option<(usize, usize)>,
) -> Result<L0Dataset, Error> {
    let file = File::open(path)?;
    // SAFETY: Read-only Memory-Map auf unveränderte Eingabedatei.
    let mmap = unsafe { Mmap::map(&file)? };
    let bytes = &mmap[..];

    let all_records = scan_packet_headers(bytes)?;
    let total_packets = all_records.len();
    let ephemeris = extract_ephemeris(&all_records);
    let mode = detect_sar_mode(&all_records)?;

    let valid_swath = |sw: u8| -> bool {
        match mode {
            SarMode::Stripmap => (1..=6).contains(&sw),
            SarMode::Iw => (10..=12).contains(&sw),
            SarMode::Ew => (20..=24).contains(&sw),
        }
    };

    let echo_records: Vec<PacketRecord> = all_records
        .into_iter()
        .filter(|r| r.is_nominal_echo() && valid_swath(r.swath_number))
        .collect();
    let echo_packets = echo_records.len();

    match mode {
        SarMode::Stripmap => {
            // Dominanten Swath auswählen
            let mut counts: BTreeMap<u8, usize> = BTreeMap::new();
            for r in &echo_records {
                *counts.entry(r.swath_number).or_insert(0) += 1;
            }
            let (&sw, _) = counts
                .iter()
                .max_by_key(|&(_, &c)| c)
                .ok_or_else(|| Error("Kein Stripmap-Swath gefunden".into()))?;
            let sw_pkts: Vec<PacketRecord> = echo_records
                .into_iter()
                .filter(|r| r.swath_number == sw)
                .collect();
            let (az0, az1) = az_window.unwrap_or((0, sw_pkts.len()));
            let az1 = az1.min(sw_pkts.len());
            if az0 >= az1 {
                return Err(Error(format!("Ungültiges Azimut-Fenster {az0}..{az1}")));
            }
            let slice = &sw_pkts[az0..az1];
            let block = decode_burst_block(bytes, slice, &ephemeris, 0, None, None)?;
            let params = block.params;
            let swath = SwathData {
                swath_number: sw,
                swath_name: swath_name(sw),
                params,
                bursts: vec![block],
            };
            Ok(L0Dataset {
                mode,
                total_packets,
                echo_packets,
                ephemeris,
                swaths: vec![swath],
            })
        }
        SarMode::Iw | SarMode::Ew => {
            // Echo-Pakete in aufeinanderfolgende Bursts gruppieren:
            // Ein neuer Burst beginnt, wenn `swath_number` wechselt oder `pri_count` um > 50 springt.
            let mut raw_bursts: BTreeMap<u8, Vec<Vec<PacketRecord>>> = BTreeMap::new();
            let mut cur_burst: Vec<PacketRecord> = Vec::new();
            for rec in echo_records {
                if let Some(last) = cur_burst.last() {
                    let dp = rec.pri_count.wrapping_sub(last.pri_count);
                    if rec.swath_number != last.swath_number || dp > 50 {
                        if cur_burst.len() >= 64 {
                            raw_bursts
                                .entry(last.swath_number)
                                .or_default()
                                .push(std::mem::take(&mut cur_burst));
                        } else {
                            cur_burst.clear();
                        }
                    }
                }
                cur_burst.push(rec);
            }
            if let Some(last) = cur_burst.last() {
                let sw = last.swath_number;
                if cur_burst.len() >= 64 {
                    raw_bursts.entry(sw).or_default().push(cur_burst);
                }
            }

            let mut swaths = Vec::with_capacity(raw_bursts.len());
            for (sw, burst_pkts_list) in raw_bursts {
                let (b0, b1) = burst_range.unwrap_or((0, burst_pkts_list.len()));
                let b1 = b1.min(burst_pkts_list.len());
                if b0 >= b1 {
                    continue;
                }
                let selected = &burst_pkts_list[b0..b1];

                // Gemeinsames Range-Raster für alle Bursts desselben Sub-Swaths bestimmen
                let fs_hz = decode_rgdec_fs_hz(selected[0][0].rgdec)?;
                let t_fast_min = selected
                    .iter()
                    .flat_map(|v| v.iter())
                    .map(fast_time_of)
                    .fold(f64::INFINITY, f64::min);
                let max_len = selected
                    .iter()
                    .flat_map(|v| v.iter())
                    .map(|r| {
                        let b = ((fast_time_of(r) - t_fast_min).max(0.0) * fs_hz).round() as usize;
                        b + r.num_samples()
                    })
                    .max()
                    .unwrap_or(0);
                let common_n_rg = smooth_fft_len(max_len);

                let mut bursts = Vec::with_capacity(selected.len());
                for (local_idx, pkts) in selected.iter().enumerate() {
                    let blk = decode_burst_block(
                        bytes,
                        pkts,
                        &ephemeris,
                        b0 + local_idx,
                        Some(t_fast_min),
                        Some(common_n_rg),
                    )?;
                    bursts.push(blk);
                }
                if let Some(first) = bursts.first() {
                    swaths.push(SwathData {
                        swath_number: sw,
                        swath_name: swath_name(sw),
                        params: first.params,
                        bursts,
                    });
                }
            }
            Ok(L0Dataset {
                mode,
                total_packets,
                echo_packets,
                ephemeris,
                swaths,
            })
        }
    }
}
