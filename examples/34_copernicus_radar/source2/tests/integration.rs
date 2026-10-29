//! Umfassende Integrations- und Verifikationstests für `sar_source2`:
//! - CCSDS-Space-Packet-Header und FDBAQ/Bypass-Dekodierung
//! - Ephemeriden-Dekodierung, Interpolation und effektive Radar-Geschwindigkeit $V_{\text{eff}}$
//! - LFM-Chirp-Replika, Range-Matched-Filter (mit eingefaltetem Shift) und Punktziel-PSR (> 20 dB)
//! - 2D-Synthetikziel: Stripmap-RDA vs. Time-Domain-Backprojection (TDBP) Korrelation (> 0.90)
//! - GPU (`cuda-oxide` + cuFFT) vs. CPU Äquivalenz für Stripmap-RDA und TOPS-Deramping-RDA
//! - TOPS Unwrapped Sub-Aperture Clutterlock ($f_{\text{DC,mid}}$, $k_t$)
//! - TOPS-Debursting & Multi-Swath-Mosaikierung

use sar_source2::chirp::{
    ChirpParams, RangeCompressor, build_chirp_replica, correlation_shift_samples,
    decode_rgdec_fs_hz, decode_txprr_hz_s, decode_txpsf_hz, embed_start,
};
use sar_source2::ephem::{
    EphemPoint, WGS84_A_M, WGS84_B_M, compute_veff_vector, decode_block, effective_velocity,
    inertial_vel, interp_pos, interp_vel,
};
use sar_source2::gpu::GpuSarContext;
use sar_source2::ingest::load_l0_dataset;
use sar_source2::insar::{
    compute_coherence_map, compute_orbit_baseline, downsample_panel_png_in_place,
    goldstein_filter_interferogram, project_target_to_slave, resample_and_form_ifg_cpu,
    run_insar_pipeline, solve_zero_doppler_target_ecef, unwrap_phase_2d, unwrap_phase_2d_poisson,
};
use sar_source2::l0::{HEADER_LEN, PacketRecord, SYNC_MARKER};
use sar_source2::mosaic::{
    DeburstedSwath, FocusedBurst, deburst_swath, mosaic_swaths, multilook_power,
};
use sar_source2::rda::{
    clutterlock_fdc, d_factor, estimate_tops_steering, focus_stripmap_block_cpu,
    focus_tops_burst_cpu, ka_rate_hz_s,
};
use sar_source2::tdbp::{TdbpGrid, TdbpInput, correlate_log_power, tdbp_cpu_parallel};
use sar_source2::types::{BurstBlock, C_LIGHT, Complex32, LAMBDA, RadarParams, Vec3d};
use std::f64::consts::PI;
use std::path::Path;

#[test]
fn test_ccsds_header_parsing_and_bitfields() {
    let mut h = [0u8; HEADER_LEN];
    // packet_data_len = 100 - 1 = 99
    h[4..6].copy_from_slice(&99u16.to_be_bytes());
    // coarse_time = 1_500_000_000, fine_time = 32768 (0.5 s)
    h[6..10].copy_from_slice(&1_500_000_000u32.to_be_bytes());
    h[10..12].copy_from_slice(&32768u16.to_be_bytes());
    // sync marker 0x352EF853
    h[12..16].copy_from_slice(&SYNC_MARKER.to_be_bytes());
    // subcom_index = 5, subcom_word = 0x1234
    h[26] = 5;
    h[27..29].copy_from_slice(&0x1234u16.to_be_bytes());
    // space_packet_count = 42, pri_count = 1000
    h[29..33].copy_from_slice(&42u32.to_be_bytes());
    h[33..37].copy_from_slice(&1000u32.to_be_bytes());
    // baq_mode = 12 (FDBAQ Nominal)
    h[37] = 12;
    h[38] = 32;
    h[40] = 3; // rgdec = 3
    // elevation_beam_address = 2 (IW3), azimuth_beam_address = 0x1A5 = 421
    let beam_word: u16 = (2u16 << 12) | 421u16;
    h[60..62].copy_from_slice(&beam_word.to_be_bytes());
    // calibration_mode = 0, tx_pulse_number = 7
    h[62] = 7;
    // signal_type = 0 (Echo), swap_flag = 1
    h[63] = 0x01;
    // swath_number = 12 (IW3)
    h[64] = 12;
    // number_of_quads = 11158
    h[65..67].copy_from_slice(&11158u16.to_be_bytes());

    let rec = PacketRecord::parse(&h, 128).expect("Header sollte fehlerfrei parsen");
    assert_eq!(rec.offset, 128);
    assert_eq!(rec.packet_data_len, 100);
    assert_eq!(rec.coarse_time, 1_500_000_000);
    assert_eq!(rec.fine_time, 32768);
    assert_eq!(rec.subcom_index, 5);
    assert_eq!(rec.subcom_word, 0x1234);
    assert_eq!(rec.baq_mode, 12);
    assert_eq!(rec.rgdec, 3);
    assert_eq!(rec.elevation_beam_address, 2);
    assert_eq!(rec.azimuth_beam_address, 421);
    assert_eq!(rec.tx_pulse_number, 7);
    assert_eq!(rec.signal_type, 0);
    assert!(rec.swap_flag);
    assert_eq!(rec.swath_number, 12);
    assert_eq!(rec.number_of_quads, 11158);
    assert_eq!(rec.num_samples(), 22316);
    assert!(rec.is_nominal_echo());
}

#[test]
fn test_ephemeris_decode_and_effective_velocity() {
    let mut words = [0u16; 64];
    let put_f64 = |w: &mut [u16], val: f64| {
        let b = val.to_be_bytes();
        for i in 0..4 {
            w[i] = u16::from_be_bytes([b[2 * i], b[2 * i + 1]]);
        }
    };
    let put_f32 = |w: &mut [u16], val: f32| {
        let b = val.to_be_bytes();
        for i in 0..2 {
            w[i] = u16::from_be_bytes([b[2 * i], b[2 * i + 1]]);
        }
    };
    // Typischer Sentinel-1 LEO-Zustand (Höhe ~700 km über Äquator)
    put_f64(&mut words[0..4], 7_071_000.0);
    put_f64(&mut words[4..8], 0.0);
    put_f64(&mut words[8..12], 0.0);
    put_f32(&mut words[12..14], 0.0);
    put_f32(&mut words[14..16], 1500.0);
    put_f32(&mut words[16..18], 7400.0);
    words[18] = 0;
    words[19] = 100; // 25600 s
    words[20] = 0;
    words[21] = 0;
    put_f32(&mut words[22..24], 0.5);
    put_f32(&mut words[24..26], -0.5);
    put_f32(&mut words[26..28], 0.5);
    put_f32(&mut words[28..30], 0.5);

    let ep = decode_block(&words);
    assert!((ep.pos.x - 7_071_000.0).abs() < 1e-3);
    assert!((ep.vel.z - 7400.0).abs() < 1e-2);
    assert!((ep.time_s - 25600.0).abs() < 1e-6);

    let ep2 = EphemPoint {
        time_s: 25601.0,
        pos: ep.pos + ep.vel,
        vel: ep.vel,
        quat: ep.quat,
    };
    let p_mid = interp_pos(&[ep, ep2], 25600.5);
    let v_mid = interp_vel(&[ep, ep2], 25600.5);
    assert!((p_mid.z - 3700.0).abs() < 1.0);
    assert!((v_mid.z - 7400.0).abs() < 1.0);

    let vs = inertial_vel(p_mid, v_mid).norm();
    let veff = effective_velocity(vs, p_mid, 850_000.0);
    // Für LEO (~700 km) liegt V_eff zwischen ~6900 und ~7400 m/s (< V_s)
    assert!(
        veff > 6800.0 && veff < vs,
        "Unerwartetes V_eff={veff}, V_s={vs}"
    );
    let veff_vec = compute_veff_vector(p_mid, v_mid, 800_000.0, 2.0, 1024);
    assert_eq!(veff_vec.len(), 1024);
    assert!(veff_vec[0] > veff_vec[1023]);
}

#[test]
fn test_chirp_synthesis_and_range_compression_psr() {
    let kr = decode_txprr_hz_s(0x84CE);
    let f0 = decode_txpsf_hz(0x7B5E, kr);
    let fs = decode_rgdec_fs_hz(1).unwrap();
    assert!(kr > 0.0, "S6 TXPRR 0x84CE muss positiven Up-Chirp ergeben");
    assert!(f0 < 0.0, "Startfrequenz muss negativ (~ -B/2) sein");
    assert!((fs - 100.09e6).abs() < 1e5);

    let n_rg = 2048usize;
    let p = ChirpParams {
        txpsf_hz: -20.0e6,
        txpl_s: 25.0e-6,
        txprr_hz_s: 1.6e12,
        fs_hz: 64.0e6,
    };
    let rep = build_chirp_replica(&p);
    let ntx = rep.len();
    let start = embed_start(n_rg, ntx);
    let shift = correlation_shift_samples(ntx, n_rg);
    let target_bin = 900usize;

    // Synthetisches Punktziel: Replika so eingebettet, dass nach der Range-Kompression
    // der Peak exakt bei `target_bin` liegt.
    let mut row = vec![Complex32::ZERO; n_rg];
    for (i, &c) in rep.iter().enumerate() {
        let idx = (start + i + target_bin + n_rg - shift) % n_rg;
        row[idx] = c;
    }

    let rcomp = RangeCompressor::new(&p, n_rg);
    rcomp.compress_row(&mut row);

    let mut max_p = 0.0f32;
    let mut max_idx = 0usize;
    let mut side_sum = 0.0f64;
    let mut side_cnt = 0usize;
    for (idx, c) in row.iter().enumerate() {
        let pw = c.norm_sqr();
        if pw > max_p {
            max_p = pw;
            max_idx = idx;
        }
        if idx.abs_diff(target_bin) > 16 {
            side_sum += f64::from(pw);
            side_cnt += 1;
        }
    }
    let psr_db = 10.0 * (f64::from(max_p) / (side_sum / side_cnt as f64)).log10();
    assert_eq!(
        max_idx, target_bin,
        "Peak-Index {max_idx} != Ziel-Bin {target_bin}"
    );
    assert!(
        psr_db > 30.0,
        "Range-Kompression PSR zu niedrig: {psr_db:.2} dB"
    );
}

#[test]
fn test_rda_vs_tdbp_synthetic_point_target_correlation() {
    // Synthetisches 2D-Stripmap-Punktziel in einer 128 x 256 Szene:
    // Vergleiche fokussierte Intensität von Stripmap-RDA gegen Time-Domain-Backprojection (TDBP).
    let n_az = 128usize;
    let n_rg = 256usize;
    let prf_hz = 1600.0;
    let pri_s = 1.0 / prf_hz;
    let fs_hz = 32.0e6;
    let dt_s = 1.0 / fs_hz;
    let r0_m = 800_000.0;
    let dr_m = 0.5 * C_LIGHT * dt_s;
    let t0_s = 2.0 * r0_m / C_LIGHT;
    let veff_val = 7100.0;

    let chirp = ChirpParams {
        txpsf_hz: -10.0e6,
        txpl_s: 2.0e-6, // 64 Samples
        txprr_hz_s: 1.0e13,
        fs_hz,
    };
    let rep = build_chirp_replica(&chirp);
    let ntx = rep.len();
    let start_embed = embed_start(n_rg, ntx);
    let shift = correlation_shift_samples(ntx, n_rg);

    // Ziel in der Szenenmitte: Azimut x_t = 0.0, Slant-Range r_t = r0_m + (n_rg/2)*dr_m
    let target_az = n_az / 2;
    let target_rg = n_rg / 2;
    let target_r = r0_m + (target_rg as f64) * dr_m;
    let target_pos = Vec3d::new(0.0, target_r, 0.0);

    let mut plat_pos = Vec::with_capacity(n_az);
    let mut pulse_times = Vec::with_capacity(n_az);
    let mut raw = vec![Complex32::ZERO; n_az * n_rg];

    for a in 0..n_az {
        let t_az = (a as f64 - target_az as f64) * pri_s;
        pulse_times.push(t_az);
        let p_pos = Vec3d::new(veff_val * t_az, 0.0, 0.0);
        plat_pos.push(p_pos);

        let d = (p_pos - target_pos).norm();
        let az_phase = -4.0 * PI * d / LAMBDA;
        let az_phasor = Complex32::from_polar_unit(az_phase);

        let bin_float = (d - r0_m) / dr_m;
        let bin_int = bin_float.round() as isize;
        let row = &mut raw[a * n_rg..(a + 1) * n_rg];
        for (i, &c) in rep.iter().enumerate() {
            let k = bin_int + (start_embed as isize) + (i as isize) - (shift as isize);
            if k >= 0 && (k as usize) < n_rg {
                row[k as usize] += c * az_phasor;
            }
        }
    }

    let blk = BurstBlock {
        swath_number: 6,
        burst_index: 0,
        n_az,
        n_rg,
        params: RadarParams {
            swath_number: 6,
            elevation_beam: 5,
            rgdec: 1,
            rank: 9,
            fs_hz,
            prf_hz,
            pri_s,
            swst_s: t0_s,
            swl_s: (n_rg as f64) * dt_s,
            tx_ramp_rate_hz_s: chirp.txprr_hz_s,
            tx_start_freq_hz: chirp.txpsf_hz,
            tx_pulse_length_s: chirp.txpl_s,
            fast_time_start_s: t0_s,
            r0_m,
            dr_m,
            chirp_samples: ntx,
        },
        pulse_times,
        pos_ecef: plat_pos.clone(),
        vel_ecef: vec![Vec3d::new(veff_val, 0.0, 0.0); n_az],
        veff: vec![veff_val; n_rg],
        az_beam_addr: vec![0; n_az],
        raw: raw.clone(),
    };

    let fdc = vec![0.0; n_rg];
    let rda_slc = focus_stripmap_block_cpu(&blk, &fdc, true);

    // 1D-Range-Kompression für TDBP
    let mut rc = raw;
    RangeCompressor::new(&chirp, n_rg).compress_matrix_parallel(&mut rc);

    // 32 x 32 Patch um das Ziel für TDBP und RDA auswerten
    let patch_w = 32usize;
    let dx_az = veff_val * pri_s;
    let grid = TdbpGrid {
        n_az: patch_w,
        n_rg: patch_w,
        x0: -((patch_w / 2) as f64 + 0.5) * dx_az,
        dx_az,
        y0: target_r - ((patch_w / 2) as f64 + 0.5) * dr_m,
        dy_rg: dr_m,
        re_m: 0.0,
    };
    let tdbp_in = TdbpInput {
        rc_data: &rc,
        n_pulses: n_az,
        n_samples: n_rg,
        plat_pos: &plat_pos,
        t0_s,
        dt_s,
        lambda_m: LAMBDA,
    };
    let tdbp_patch = tdbp_cpu_parallel(&grid, &tdbp_in, n_az);

    let mut rda_patch = Vec::with_capacity(patch_w * patch_w);
    let a0 = target_az - patch_w / 2;
    let r0 = target_rg - patch_w / 2;
    for da in 0..patch_w {
        for dr in 0..patch_w {
            rda_patch.push(rda_slc[(a0 + da) * n_rg + (r0 + dr)]);
        }
    }

    let corr = correlate_log_power(&rda_patch, &tdbp_patch);
    assert!(
        corr > 0.90,
        "Korrelation zwischen Stripmap-RDA und TDBP muss > 0.90 sein, erhalten: {corr:.4}"
    );
}

#[test]
fn test_tops_steering_clutterlock_and_gpu_cpu_equivalence() {
    // Prüfe Unwrapped Sub-Aperture Clutterlock für einen synthetischen TOPS-Burst
    // mit k_t = +4900 Hz/s (mehrfacher PRF-Durchlauf bei PRF = 1685 Hz!)
    let n_az = 512usize;
    let n_rg = 256usize;
    let prf_hz = 1685.0;
    let pri_s = 1.0 / prf_hz;
    let true_fdc = -120.0;
    let true_kt = 4900.0;

    let mut rc = vec![Complex32::ZERO; n_az * n_rg];
    let mid_az = (n_az as f64) * 0.5;
    for a in 0..n_az {
        let t = (a as f64 - mid_az) * pri_s;
        let ph = 2.0 * PI * true_fdc * t + PI * true_kt * t * t;
        let c = Complex32::from_polar_unit(ph);
        for r in 0..n_rg {
            rc[a * n_rg + r] = c;
        }
    }
    let st = estimate_tops_steering(&rc, n_az, n_rg, pri_s, &[]);
    assert!(
        (st.kt_hz_s - true_kt).abs() < 50.0,
        "TOPS k_t Schätzung ungenau: erwartet {true_kt}, erhalten {}",
        st.kt_hz_s
    );
    assert!(
        (st.fdc_mid_hz - true_fdc).abs() < 25.0,
        "TOPS f_DC,mid Schätzung ungenau: erwartet {true_fdc}, erhalten {}",
        st.fdc_mid_hz
    );

    // Stripmap-Clutterlock Test
    let mut rc_sm = vec![Complex32::ZERO; n_az * n_rg];
    for a in 0..n_az {
        let t = (a as f64) * pri_s;
        let c = Complex32::from_polar_unit(2.0 * PI * 210.0 * t);
        for r in 0..n_rg {
            rc_sm[a * n_rg + r] = c;
        }
    }
    let fdc_est = clutterlock_fdc(&rc_sm, n_az, n_rg, pri_s, 8);
    assert!((fdc_est[n_rg / 2] - 210.0).abs() < 2.0);
    assert!(d_factor(200.0, 7100.0) < 1.0 && ka_rate_hz_s(7100.0, 850_000.0) < 0.0);

    // Falls GPU verfügbar ist, verifiziere GPU- vs. CPU-Fokussierung
    if let Ok(gpu) = GpuSarContext::new() {
        let fs_hz = 32.0e6;
        let dt_s = 1.0 / fs_hz;
        let blk = BurstBlock {
            swath_number: 10,
            burst_index: 0,
            n_az: 128,
            n_rg: 256,
            params: RadarParams {
                swath_number: 10,
                elevation_beam: 0,
                rgdec: 3,
                rank: 9,
                fs_hz,
                prf_hz,
                pri_s,
                swst_s: 0.005,
                swl_s: 256.0 * dt_s,
                tx_ramp_rate_hz_s: 1.0e13,
                tx_start_freq_hz: -10.0e6,
                tx_pulse_length_s: 2.0e-6,
                fast_time_start_s: 0.005,
                r0_m: 820_000.0,
                dr_m: 0.5 * C_LIGHT * dt_s,
                chirp_samples: 64,
            },
            pulse_times: (0..128).map(|a| (a as f64) * pri_s).collect(),
            pos_ecef: vec![Vec3d::new(7_000_000.0, 0.0, 0.0); 128],
            vel_ecef: vec![Vec3d::new(0.0, 7_500.0, 0.0); 128],
            veff: vec![7_100.0; 256],
            az_beam_addr: vec![500; 128],
            raw: rc[..128 * 256].to_vec(),
        };
        let (cpu_tops, n_az_cpu, _, _) = focus_tops_burst_cpu(&blk, true);
        let (gpu_tops, n_az_gpu, _, _) = gpu.focus_tops_burst_gpu(&blk, true).unwrap();
        assert_eq!(n_az_cpu, n_az_gpu);
        let corr = correlate_log_power(&cpu_tops, &gpu_tops);
        assert!(
            corr > 0.99,
            "CPU vs GPU TOPS Korrelation zu niedrig: {corr}"
        );
    }
}

#[test]
fn test_tops_deburst_and_multiswath_mosaic() {
    let n_az_raw = 256usize;
    let n_az_fft = 512usize;
    let n_rg = 256usize;
    let pri_s = 1.0 / 1600.0;
    let b0 = FocusedBurst {
        burst_index: 0,
        n_az_raw,
        n_az_fft,
        n_rg,
        n_valid_rg: 240,
        mid_time_s: 100.0,
        pri_s,
        r0_m: 800_000.0,
        dr_m: 10.0,
        veff_mid: 7100.0,
        kt_hz_s: 4500.0,
        fdc_mid_hz: 0.0,
        data: vec![Complex32::new(2.0, 1.0); n_az_fft * n_rg],
    };
    let b1 = FocusedBurst {
        burst_index: 1,
        mid_time_s: 102.75,
        ..b0.clone()
    };
    let sw1 = deburst_swath(10, "IW1", &[b0, b1]).expect("Debursting sollte gelingen");
    assert_eq!(sw1.n_rg, 240);
    assert!(sw1.n_az > n_az_raw);

    let sw2 = DeburstedSwath {
        swath_number: 11,
        swath_name: "IW2".into(),
        n_az: sw1.n_az,
        n_rg: sw1.n_rg,
        t_start_s: sw1.t_start_s,
        dt_az_s: sw1.dt_az_s,
        r0_m: 800_000.0 + 200.0 * 10.0, // 40 Samples Überlappung mit IW1
        dr_m: 10.0,
        power: vec![5.0f32; sw1.n_az * sw1.n_rg],
    };

    let (mos, oaz, org, r_min, r_max) = mosaic_swaths(&[sw1, sw2], 2, 2).unwrap();
    assert_eq!(mos.len(), oaz * org);
    assert!((r_min - 800_000.0).abs() < 1e-3);
    assert!((r_max - 804_400.0).abs() < 1e-3);
    let (ml, maz, mrg) = multilook_power(&mos, oaz, org, 2, 2);
    assert_eq!(ml.len(), maz * mrg);
}

#[test]
fn test_insar_orbit_baseline_and_zero_doppler_geolocation() {
    // Synthetischer Sentinel-1 LEO-Orbit (Höhe 693 km über dem Äquator, Polarbahn entlang +Z)
    let r_sat = WGS84_A_M + 693_000.0;
    let v_sat = 7_500.0;
    let t0_m = 1_475_000_000.0;
    let day_offset = 12.0 * 86_400.0;
    let dt_ca_true = 0.0495; // +49.5 ms Closest-Approach-Offset im Slave
    let b_rad_true = 9.2; // +9.2 m radial (u_r = +X)
    let b_cross_true = -14.3; // -14.3 m rechts-quer zur Flugrichtung (u_c = u_v × u_r = +Z × +X = +Y)

    let mut ephem_m = Vec::new();
    let mut ephem_s = Vec::new();
    for k in -10..=10 {
        let dt = k as f64;
        let tm = t0_m + dt;
        let pos_m = Vec3d::new(r_sat, 0.0, v_sat * dt);
        let vel_m = Vec3d::new(0.0, 0.0, v_sat);
        ephem_m.push(EphemPoint {
            time_s: tm,
            pos: pos_m,
            vel: vel_m,
            quat: [1.0, 0.0, 0.0, 0.0],
        });

        // Slave passiert denselben Breitengrad um dt_ca_true später mit festem Basislinien-Versatz
        let ts = t0_m + day_offset + dt;
        let dt_rel = dt - dt_ca_true;
        let pos_s = Vec3d::new(r_sat + b_rad_true, b_cross_true, v_sat * dt_rel);
        let vel_s = Vec3d::new(0.0, 0.0, v_sat);
        ephem_s.push(EphemPoint {
            time_s: ts,
            pos: pos_s,
            vel: vel_s,
            quat: [1.0, 0.0, 0.0, 0.0],
        });
    }

    let r_mid = 850_000.0;
    let bm = compute_orbit_baseline(&ephem_m, &ephem_s, r_mid);
    let expected_b3d = b_rad_true.hypot(b_cross_true);
    assert!(
        (bm.b_3d_m - expected_b3d).abs() < 0.05,
        "B_3D erwartet {expected_b3d:.3}, erhalten {:.3}",
        bm.b_3d_m
    );
    assert!(
        (bm.b_radial_m - b_rad_true).abs() < 0.05,
        "B_radial erwartet {b_rad_true:.3}, erhalten {:.3}",
        bm.b_radial_m
    );
    assert!(
        (bm.b_cross_m - b_cross_true).abs() < 0.05,
        "B_cross erwartet {b_cross_true:.3}, erhalten {:.3}",
        bm.b_cross_m
    );
    assert!(
        (bm.dt_closest_approach_s - dt_ca_true).abs() < 1e-4,
        "dt_CA erwartet {dt_ca_true:.6}, erhalten {:.6}",
        bm.dt_closest_approach_s
    );
    assert!(bm.height_ambiguity_m.is_finite() && bm.height_ambiguity_m > 100.0);

    // Prüfe Rechts-Sicht WGS-84 Zero-Doppler-Geolocation und Konsistenz der inversen Projektion
    let t_test = t0_m + 1.25;
    let pos_m = interp_pos(&ephem_m, t_test);
    let vel_m = interp_vel(&ephem_m, t_test);
    let h_ellip = 250.0;
    let target = solve_zero_doppler_target_ecef(pos_m, vel_m, r_mid, h_ellip);

    let a_e = WGS84_A_M + h_ellip;
    let b_e = WGS84_B_M + h_ellip;
    let ellip_err = ((target.x * target.x + target.y * target.y) / (a_e * a_e)
        + (target.z * target.z) / (b_e * b_e)
        - 1.0)
        .abs();
    assert!(
        ellip_err < 1e-8,
        "Ziel liegt nicht auf WGS-84-Ellipsoid: Fehler={ellip_err:e}"
    );

    let los = target - pos_m;
    assert!(
        (los.norm() - r_mid).abs() < 1e-4,
        "Slant-Range Abweichung zu groß: {}",
        (los.norm() - r_mid).abs()
    );
    let doppler_dot = los.dot(vel_m) / (los.norm() * vel_m.norm());
    assert!(
        doppler_dot.abs() < 1e-12,
        "Zero-Doppler-Orthogonalität verletzt: {doppler_dot:e}"
    );
    // Rechts-Blick-Geometrie: (vel × pos) . los > 0
    assert!(
        vel_m.cross(pos_m).dot(los) > 0.0,
        "Ziel liegt nicht auf der rechten Schwadseite"
    );

    // Selbst-Projektion auf Master-Orbit muss (t_test, r_mid) exakt reproduzieren
    let (t_rec, r_rec) = project_target_to_slave(&ephem_m, target, t_test + 0.3);
    assert!(
        (t_rec - t_test).abs() < 1e-6,
        "Inverse Zero-Doppler-Zeit ungenau: erwartet {t_test}, erhalten {t_rec}"
    );
    assert!(
        (r_rec - r_mid).abs() < 1e-3,
        "Inverser Slant-Range ungenau: erwartet {r_mid}, erhalten {r_rec}"
    );

    // Projektion auf Slave-Orbit muss den Along-Track-Versatz dt_ca_true widerspiegeln
    let (t_s, r_s) = project_target_to_slave(&ephem_s, target, t_test + day_offset);
    assert!(
        ((t_s - day_offset - t_test) - dt_ca_true).abs() < 1e-4,
        "Slave Zero-Doppler-Zeitversatz unerwartet: {}",
        t_s - day_offset - t_test
    );
    assert!((r_s - r_mid).abs() < 30.0);
}

#[test]
fn test_insar_resample_and_form_ifg_cpu_gpu_parity() {
    let n_az = 64usize;
    let n_rg = 128usize;
    let mid_az = (n_az as f64) * 0.5;
    let mid_rg = (n_rg as f64) * 0.5;

    let da_coeffs = [0.35, 1.5e-4, -2.0e-4];
    let dr_coeffs = [-0.42, -1.0e-4, 1.2e-4];
    let phi_coeffs = [0.45, 0.018, -0.024, 1.5e-5];

    // Erzeuge synthetische Master- und Slave-Felder mit bekanntem Phasenmodell
    let mut master = vec![Complex32::ZERO; n_az * n_rg];
    let mut slave = vec![Complex32::ZERO; n_az * n_rg];
    let true_topo_phase = 0.65f64;

    for a in 0..n_az {
        let af = a as f64 - mid_az;
        for r in 0..n_rg {
            let rf = r as f64 - mid_rg;
            let amp = 1.5 + 0.4 * (0.11 * a as f64).sin() * (0.07 * r as f64).cos();
            let phi_fe =
                phi_coeffs[0] + phi_coeffs[1] * af + phi_coeffs[2] * rf + phi_coeffs[3] * rf * rf;
            // Slave ist glatt in der Phase, Master trägt Flat-Earth + konstante Topographiephase
            slave[a * n_rg + r] = Complex32::new(amp as f32, 0.0);
            master[a * n_rg + r] =
                Complex32::from_polar_unit(phi_fe + true_topo_phase) * (amp as f32);
        }
    }

    let (ifg_cpu, pm_cpu, ps_cpu) = resample_and_form_ifg_cpu(
        &master, &slave, n_az, n_rg, da_coeffs, dr_coeffs, phi_coeffs,
    );

    // Im inneren gültigen Bereich muss nach Flat-Earth-Abzug die Phase nahe `true_topo_phase` liegen
    for a in 4..(n_az - 4) {
        for r in 4..(n_rg - 4) {
            let idx = a * n_rg + r;
            assert!(pm_cpu[idx] > 0.5 && ps_cpu[idx] > 0.5);
            let ph_err = (f64::from(ifg_cpu[idx].arg()) - true_topo_phase).abs();
            assert!(
                ph_err < 1e-3,
                "Flat-Earth-Demodulation ungenau bei ({a},{r}): Fehler={ph_err}"
            );
        }
    }

    if let Ok(gpu) = GpuSarContext::new() {
        let (ifg_gpu, pm_gpu, ps_gpu) = gpu
            .resample_and_form_ifg_gpu(
                &master, &slave, n_az, n_rg, da_coeffs, dr_coeffs, phi_coeffs,
            )
            .expect("GPU InSAR-Resampling sollte fehlerfrei laufen");

        let mut max_ifg_diff = 0.0f32;
        let mut max_p_diff = 0.0f32;
        for i in 0..(n_az * n_rg) {
            let d_re = (ifg_cpu[i].re - ifg_gpu[i].re).abs();
            let d_im = (ifg_cpu[i].im - ifg_gpu[i].im).abs();
            max_ifg_diff = max_ifg_diff.max(d_re.hypot(d_im));
            max_p_diff = max_p_diff
                .max((pm_cpu[i] - pm_gpu[i]).abs())
                .max((ps_cpu[i] - ps_gpu[i]).abs());
        }
        assert!(
            max_ifg_diff < 1e-4 && max_p_diff < 1e-4,
            "Abweichung zwischen CPU und GPU resample_and_form_ifg zu groß: ifg={max_ifg_diff}, p={max_p_diff}"
        );
    }
}

#[test]
fn test_insar_coherence_goldstein_and_poisson_phase_unwrap() {
    let n_az = 64usize;
    let n_rg = 64usize;
    let n_total = n_az * n_rg;

    // Synthetische 2D-Phasenrampe über mehrere 2*pi-Zyklen hinweg ([-9 rad, +9 rad])
    let ka = 0.14f32;
    let kr = -0.11f32;
    let mut true_phase = vec![0.0f32; n_total];
    let mut ifg_clean = vec![Complex32::ZERO; n_total];
    let mut ifg_noisy = vec![Complex32::ZERO; n_total];
    let pm = vec![1.0f32; n_total];
    let ps = vec![1.0f32; n_total];

    for a in 0..n_az {
        for r in 0..n_rg {
            let idx = a * n_rg + r;
            let ph = ka * (a as f32 - 32.0) + kr * (r as f32 - 32.0);
            true_phase[idx] = ph;
            let c = Complex32::from_polar_unit(f64::from(ph));
            ifg_clean[idx] = c;

            // Deterministisches hochfrequentes Phasen-/Amplitudenrauschen für Goldstein-Test
            let n_re = 0.38 * ((a * 7 + r * 13) as f32).sin();
            let n_im = 0.38 * ((a * 11 + r * 5) as f32).cos();
            ifg_noisy[idx] = c + Complex32::new(n_re, n_im);
        }
    }

    // 1. Kohärenzkarte muss trotz steiler 2D-Fringe-Rampe dank Gradienten-Kompensation > 0.98 sein
    let coh = compute_coherence_map(&ifg_clean, &pm, &ps, n_az, n_rg, 2, 2);
    let mean_coh: f32 = coh.iter().sum::<f32>() / (n_total as f32);
    assert!(
        mean_coh > 0.98,
        "Erwartete mittlere Kohärenz > 0.98 auf glatter 2D-Rampe, erhalten: {mean_coh:.4}"
    );

    // 2. Goldstein 2D-FFT-Filter muss das Phasen-RMSE auf dem verrauschten Interferogramm deutlich senken
    let ifg_filt = goldstein_filter_interferogram(&ifg_noisy, n_az, n_rg, 0.75);
    let phase_rmse = |field: &[Complex32]| -> f64 {
        let mut sum_sq = 0.0f64;
        let mut cnt = 0usize;
        for a in 8..(n_az - 8) {
            for r in 8..(n_rg - 8) {
                let idx = a * n_rg + r;
                let d = field[idx] * ifg_clean[idx].conj();
                let err = f64::from(d.arg());
                sum_sq += err * err;
                cnt += 1;
            }
        }
        (sum_sq / cnt as f64).sqrt()
    };
    let rmse_before = phase_rmse(&ifg_noisy);
    let rmse_after = phase_rmse(&ifg_filt);
    assert!(
        rmse_after < rmse_before * 0.65,
        "Goldstein-Filter hat Phasenrauschen nicht ausreichend reduziert: vorher={rmse_before:.4}, nachher={rmse_after:.4}"
    );

    // 3. 2D Quality-Guided & 2D gewichtetes CG-Poisson Phase-Unwrapping auf der gewrappten 2D-Rampe
    let unw_qg = unwrap_phase_2d(&ifg_clean, &coh, n_az, n_rg);
    let unw_cg = unwrap_phase_2d_poisson(&ifg_clean, &coh, n_az, n_rg);

    let true_mean: f32 = true_phase.iter().sum::<f32>() / (n_total as f32);
    let qg_mean: f32 = unw_qg.iter().sum::<f32>() / (n_total as f32);
    let cg_mean: f32 = unw_cg.iter().sum::<f32>() / (n_total as f32);

    let mut err_qg_sq = 0.0f64;
    let mut err_cg_sq = 0.0f64;
    for i in 0..n_total {
        let t0 = true_phase[i] - true_mean;
        let dq = f64::from((unw_qg[i] - qg_mean) - t0);
        let dc = f64::from((unw_cg[i] - cg_mean) - t0);
        err_qg_sq += dq * dq;
        err_cg_sq += dc * dc;
    }
    let rmse_qg = (err_qg_sq / n_total as f64).sqrt();
    let rmse_cg = (err_cg_sq / n_total as f64).sqrt();
    assert!(
        rmse_qg < 1e-3 && rmse_cg < 1e-3,
        "2D-Phase-Unwrapping RMSE zu hoch: QG={rmse_qg:e}, CG-Poisson={rmse_cg:e}"
    );
}

#[test]
fn test_insar_real_south_taiwan_single_burst_and_panel_downsample() {
    // 1. Falls das große Süd-Taiwan-Panel existiert, stelle sicher, dass es auf < 10 MB heruntergetastet ist
    let taiwan_panel = Path::new("out/insar_taiwan/taiwan_insar_insar_panel.png");
    if taiwan_panel.exists() {
        let (w, h, sz) = downsample_panel_png_in_place(taiwan_panel)
            .expect("Downsampling von taiwan_insar_insar_panel.png fehlgeschlagen");
        assert!(
            w <= 3000 && h <= 2600 && sz < 10_000_000,
            "Panel-PNG zu groß nach Downsampling: {w}x{h}, {sz} Bytes"
        );
    }

    // 2. Realdaten-Integrationstest auf einem 1-Burst-Ausschnitt (--bursts 5,6) des Süd-Taiwan-Paares
    let master_path = Path::new(
        "../data/insar/S1C_IW_RAW__0SDV_20260928T215149_20260928T215222_009653_01336F_D2EE.SAFE/s1c-iw-raw-s-vv-20260928t215149-20260928t215222-009653-01336f.dat",
    );
    let slave_path = Path::new(
        "../data/insar/S1C_IW_RAW__0SDV_20261010T215149_20261010T215222_009828_013950_CEB1.SAFE/s1c-iw-raw-s-vv-20261010t215149-20261010t215222-009828-013950.dat",
    );
    if !master_path.exists() || !slave_path.exists() {
        return;
    }

    let master_ds = load_l0_dataset(master_path, None, Some((5, 6)))
        .expect("Master L0-Burst 5..6 konnte nicht geladen werden");
    let slave_ds = load_l0_dataset(slave_path, None, Some((5, 6)))
        .expect("Slave L0-Burst 5..6 konnte nicht geladen werden");

    let gpu_opt = GpuSarContext::new().ok();
    let out_dir = Path::new("out/insar_test_b5");
    let summary = run_insar_pipeline(
        &master_ds,
        &slave_ds,
        gpu_opt.as_ref(),
        Some((6, 24)),
        0.65,
        2,
        out_dir,
        "taiwan_b5",
    )
    .expect("InSAR-Pipeline auf Burst 5..6 fehlgeschlagen");

    assert!(
        summary.baseline.b_3d_m > 10.0 && summary.baseline.b_3d_m < 25.0,
        "Unerwartete 3D-Basislinie: {} m",
        summary.baseline.b_3d_m
    );
    assert!(
        summary.baseline.height_ambiguity_m.is_finite()
            && summary.baseline.height_ambiguity_m > 100.0
    );
    assert!(
        summary.mean_coherence_raw > 0.12 && summary.mean_coherence_filtered > 0.25,
        "Zu geringe Landkohärenz auf Burst 5: raw={}, filt={}",
        summary.mean_coherence_raw,
        summary.mean_coherence_filtered
    );
    assert_eq!(summary.swath_metrics.len(), 3);
    for (name, coh, da, dr) in &summary.swath_metrics {
        assert!(
            coh.is_finite() && *coh > 0.10 && da.is_finite() && dr.is_finite(),
            "Ungültige Swath-Metriken für {name}: coh={coh}, da={da}, dr={dr}"
        );
    }
    let _ = std::fs::remove_dir_all(out_dir);
}
