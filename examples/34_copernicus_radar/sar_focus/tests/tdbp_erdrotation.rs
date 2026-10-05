//! Erdrotation: Die Brücke bekommt `ECEF(t_p)` (mitrotierend, wie
//! Echtdaten), die Echos entstehen physikalisch (inertial). Ohne
//! Derotation ins Epochen-System läge der Peak 10000+ px daneben.

use sar_focus::chirp::ChirpParams;
use sar_focus::ephem::{EARTH_OMEGA_RAD_S, inertial_vel};
use sar_focus::ingest::LoadedWindow;
use sar_focus::range::RangeCompressor;
use sar_focus::tdbp::{LocalFrame, TdbpInput, peak_power, tdbp_cpu};
use sar_focus::tdbp_geo::geo_for_window;
use sar_focus::types::{Complex32, SPEED_OF_LIGHT, Vec3d, WGS84_A_M, WGS84_B_M};

fn rot_z(p: Vec3d, th: f64) -> Vec3d {
    let (s, c) = th.sin_cos();
    Vec3d::new(c * p.x - s * p.y, s * p.x + c * p.y, p.z)
}

#[test]
fn erdrotation_wird_kompensiert() {
    let re = (WGS84_A_M + WGS84_B_M) / 2.0;
    let (h, naz, n0) = (693_000.0, 512, 100);
    let (pri, fs) = (6.0e-4, 46_918_402.8);
    let mid = naz / 2;
    // Wahre (inertiale) Bahn: POLAR (Nord-Süd, wie S1), 100-m-Raster.
    // Nur so fällt die Erdrotation (Ost-West) in die Blickrichtung —
    // bei prograder Äquator-Bahn wäre sie quer dazu (unsichtbar!).
    let truth: Vec<Vec3d> = (0..naz)
        .map(|p| Vec3d::new(re + h, 0.0, (p as f64 - mid as f64) * 100.0))
        .collect();
    // Brücken-Eingabe wie Echtdaten: ECEF(t_p), rotiert.
    let line_pos: Vec<Vec3d> = truth
        .iter()
        .enumerate()
        .map(|(p, &q)| rot_z(q, -EARTH_OMEGA_RAD_S * (p as f64 - mid as f64) * pri))
        .collect();
    let line_vel: Vec<Vec3d> = (0..naz).map(|_| Vec3d::new(0.0, 0.0, 7_500.0)).collect();
    let slant: Vec<f64> = (0..n0)
        .map(|j| 900_000.0 + j as f64 * SPEED_OF_LIGHT / 2.0 / fs)
        .collect();
    let chirp = ChirpParams {
        txpsf_hz: -5.0e5,
        txpl_s: 1.0e-6,
        txprr_hz_s: 1.0e12,
        fs_hz: fs,
    };
    let win = LoadedWindow {
        raw: vec![Complex32::zero(); naz * n0],
        naz,
        n0,
        n0raw: n0,
        slant,
        chirp,
        bw_hz: 10.0e6,
        veff: vec![7_100.0; n0],
        pri_s: pri,
        fs_hz: fs,
        metas: Vec::new(),
        line_pos,
        line_vel,
        az0_abs: 1000,
        beam: 5,
    };
    let g = geo_for_window(&win, 1000, 1008, 10, 40).unwrap();
    // Simulation mit WAHRHEIT (Puls-Mitte ist unrotiert → selber Frame).
    let vel = inertial_vel(win.line_pos[mid], win.line_vel[mid]);
    let frame = LocalFrame::from_orbit(win.line_pos[mid], vel);
    let plat_true: Vec<Vec3d> = truth.iter().map(|&q| frame.to_local(q)).collect();
    let tgt = g.grid.pixel_pos(3, 17);
    let (k, txpl, lambda) = (1.0e12, win.chirp.txpl_s, SPEED_OF_LIGHT / 5.405e9);
    let ntx = sar_focus::chirp::num_tx_samples(&win.chirp);
    let mut raw = vec![Complex32::zero(); naz * n0];
    for (p, plat) in plat_true.iter().enumerate() {
        let d = plat.dist(tgt);
        let s0 = (2.0 * d / SPEED_OF_LIGHT - g.t0_s) / g.dt_s;
        let ph_c = -4.0 * std::f64::consts::PI * d / lambda;
        let (cm_re, cm_im) = (ph_c.cos(), ph_c.sin());
        for i in 0..ntx {
            let t = (i as f64 - (ntx - 1) as f64 / 2.0) / fs;
            if t.abs() > txpl / 2.0 {
                continue;
            }
            let s = (s0 + (i as f64 - (ntx - 1) as f64 / 2.0)).round() as isize;
            if s < 0 || s >= n0 as isize {
                continue;
            }
            let ph = 2.0 * std::f64::consts::PI * (k / 2.0 * t * t);
            let (cr, ci) = (ph.cos(), ph.sin());
            raw[p * n0 + s as usize] = Complex32::new(
                (cr * cm_re - ci * cm_im) as f32,
                (cr * cm_im + ci * cm_re) as f32,
            );
        }
    }
    let comp = RangeCompressor::new(&win.chirp, n0);
    comp.compress_rows(&mut raw);
    let shift = sar_focus::rda::correlation_shift_samples(ntx, n0);
    for p in 0..naz {
        raw[p * n0..(p + 1) * n0].rotate_right(shift);
    }
    let inp = TdbpInput {
        data: &raw,
        npulse: naz,
        nsample: n0,
        plat: &g.plat,
        t0_s: g.t0_s,
        dt_s: g.dt_s,
        lambda,
    };
    let img = tdbp_cpu(&g.grid, &inp, naz);
    let (peak, _) = peak_power(&img);
    let (pa, pr) = (peak / g.grid.nrange, peak % g.grid.nrange);
    assert!(
        pa.abs_diff(3) <= 1 && pr.abs_diff(17) <= 1,
        "Peak bei ({pa}, {pr}), erwartet (3, 17)"
    );
}
