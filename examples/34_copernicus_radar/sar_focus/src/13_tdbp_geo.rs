//! TDBP-Echtdatengeometrie: RDA-Pixel-Fenster → Grid + Plattformpositionen.
//!
//! Brücke zwischen RDA-Raster ([`LoadedWindow`][crate::ingest::LoadedWindow])
//! und TDBP ([`TdbpGrid`][crate::tdbp::TdbpGrid]): Der lokale Rahmen kommt
//! aus den echten Ephemeriden der Aperturmitte
//! ([`LocalFrame`][crate::tdbp::LocalFrame], mit INERTIALER Geschwindigkeit
//! — ECEF-vel zeigt 3,5° neben along-track!), die Plattformpositionen
//! kommen aus `ECEF(t_p)` und werden ins Epochen-System `ECEF(t_mid)`
//! zurückgedreht ([`ecef_to_epoch`]) — Abstände sind nur GLEICHZEITIG
//! rotationsinvariant! Ohne Derotation läge Puls `p` um bis zu ±320 m
//! (voll in S1-Blickrichtung!) daneben. Das Zielraster folgt den RDA-Pixeln
//! (`x = (a−a_ref)·v_eff·PRI`, Bogenlänge aus Slant über Kosinus-Satz) —
//! TDBP-Pixel entsprechen nominell 1:1 RDA-Output-Pixeln (bis
//! RDA-Lageversatz durch `f_DC`-Fehler). Die Ziele liegen auf der Kugel
//! (Flach-Erde wäre 28 km falsch, Schwad 600 km neben Nadir).

use crate::ingest::LoadedWindow;
use crate::tdbp::{LocalFrame, TdbpGrid};
use crate::types::{Error, SPEED_OF_LIGHT, Vec3d, WGS84_A_M, WGS84_B_M};

/// TDBP-Geometrie für ein RDA-Pixel-Fenster (plus Zeitraster).
pub struct TdbpGeo {
    pub grid: TdbpGrid,
    pub plat: Vec<Vec3d>,
    pub t0_s: f64,
    pub dt_s: f64,
}

/// Position aus `ECEF(t)` ins Epochen-System `ECEF(t_epoch)` drehen.
///
/// ECEF rotiert mit `+ω` um die Z-Achse (ostwärts). Die inertiale Position
/// ist `R_z(+ωt)·ecef(t)`; zurückgedreht ins Epochen-System:
/// `R_z(ω·(t − t_epoch))·ecef(t)`. `dt_from_epoch_s` ist `t − t_epoch`
/// (für Puls `p`: `(p − mid)·PRI`).
fn ecef_to_epoch(pos: Vec3d, dt_from_epoch_s: f64) -> Vec3d {
    let th = crate::ephem::EARTH_OMEGA_RAD_S * dt_from_epoch_s;
    let (s, c) = th.sin_cos();
    Vec3d::new(c * pos.x - s * pos.y, s * pos.x + c * pos.y, pos.z)
}

/// Baut Grid + Plattformpositionen für RDA-Output-Pixel
/// `waz0..waz1` (ABSOLUT = Echo-Nummern) × `wrg0..wrg1` (nach Wrap-Crop).
pub fn geo_for_window(
    win: &LoadedWindow,
    waz0: usize,
    waz1: usize,
    wrg0: usize,
    wrg1: usize,
) -> Result<TdbpGeo, Error> {
    let npulse = win.naz;
    if npulse == 0 {
        return Err(Error("keine Pulse".to_string()));
    }
    if waz0 >= waz1 {
        return Err(Error(format!("leeres Azimut-Fenster {waz0}..{waz1}")));
    }
    if wrg0 >= wrg1 {
        return Err(Error(format!("leeres Range-Fenster {wrg0}..{wrg1}")));
    }
    let ntx = crate::chirp::num_tx_samples(&win.chirp);
    if wrg1 + ntx > win.n0 {
        return Err(Error(format!(
            "Range-Fenster {wrg0}..{wrg1} + Wrap {ntx} überragt n0={}",
            win.n0
        )));
    }
    // Rahmen aus Aperturmitte (inertiale Geschwindigkeit!).
    let mid = npulse / 2;
    let vel = crate::ephem::inertial_vel(win.line_pos[mid], win.line_vel[mid]);
    let frame = LocalFrame::from_orbit(win.line_pos[mid], vel);
    // Alle Plattformen ins Epochen-System ECEF(t_mid) drehen (s. ecef_to_epoch).
    let pri = win.pri_s;
    let plat: Vec<Vec3d> = win
        .line_pos
        .iter()
        .enumerate()
        .map(|(p, &q)| frame.to_local(ecef_to_epoch(q, (p as f64 - mid as f64) * pri)))
        .collect();
    let re = (WGS84_A_M + WGS84_B_M) / 2.0;
    let h = win.line_pos[mid].norm() - re;
    // Azimut: RDA-Pixelmaß (v_eff·PRI), Mitte = Puls-Mitte.
    let veff = win.veff[win.n0 / 2];
    let dx = veff * win.pri_s;
    let a_ref = win.az0_abs as f64 + npulse as f64 / 2.0;
    let x0 = (waz0 as f64 - a_ref) * dx - dx / 2.0;
    // Range: Bogen-Ground-Range aus Slant (Kosinus-Satz; Output j ↔ slant[j+ntx]).
    // Flach-Erde wäre hier 28 km falsch (Schwad 600 km neben Nadir!) und
    // defokussierte total — die Ziele liegen auf der Kugel (s. TdbpGrid).
    let arc_of = |r: usize| {
        let s = win.slant[r + ntx];
        let cth = (re * re + (re + h) * (re + h) - s * s) / (2.0 * re * (re + h));
        re * cth.clamp(-1.0, 1.0).acos()
    };
    if win.slant[wrg0 + ntx] <= h {
        return Err(Error(
            "Slant < Plattformhöhe (unmögliche Geometrie)".to_string(),
        ));
    }
    let dy = if wrg0 + 1 + ntx < win.n0 {
        arc_of(wrg0 + 1) - arc_of(wrg0)
    } else if wrg0 > 0 {
        arc_of(wrg0) - arc_of(wrg0 - 1)
    } else {
        SPEED_OF_LIGHT / 2.0 / win.fs_hz
    };
    let grid = TdbpGrid {
        naz: waz1 - waz0,
        nrange: wrg1 - wrg0,
        x0,
        dx_az: dx,
        y_near: arc_of(wrg0) - dy / 2.0,
        dy_gr: dy,
        re_m: re,
    };
    Ok(TdbpGeo {
        grid,
        plat,
        t0_s: 2.0 * win.slant[0] / SPEED_OF_LIGHT,
        dt_s: 1.0 / win.fs_hz,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::chirp::ChirpParams;

    /// Synthetisches Fenster: 8 Pulse, Äquator-Geometrie, Slant 900–950 km.
    fn fake_window() -> LoadedWindow {
        let re = (WGS84_A_M + WGS84_B_M) / 2.0;
        let h = 693_000.0;
        let v = 7_500.0;
        let naz = 8;
        let n0 = 100;
        let line_pos: Vec<Vec3d> = (0..naz)
            .map(|p| Vec3d::new(re + h, (p as f64 - 3.5) * 100.0, 0.0))
            .collect();
        // ECEF-vel prograd (y-Richtung); inertial_vel addiert ω×r (z-Anteil).
        let line_vel: Vec<Vec3d> = (0..naz).map(|_| Vec3d::new(0.0, v, 0.0)).collect();
        // Konsistentes Sample-Raster: Schritt c/2/fs wie in Produktion.
        let slant: Vec<f64> = (0..n0)
            .map(|j| 900_000.0 + j as f64 * SPEED_OF_LIGHT / 2.0 / 46_918_402.8)
            .collect();
        LoadedWindow {
            raw: vec![crate::types::Complex32::zero(); naz * n0],
            naz,
            n0,
            n0raw: n0,
            slant,
            chirp: ChirpParams {
                // φ₁ = TXPSF + TXPRR·TXPL/2 = 0 (Basisband wie S6).
                txpsf_hz: -5.0e5,
                txpl_s: 1.0e-6, // ntx = 46 < n0 = 100
                txprr_hz_s: 1.0e12,
                fs_hz: 46_918_402.8,
            },
            bw_hz: 10.0e6,
            veff: vec![7_100.0; n0],
            pri_s: 6.0e-4,
            fs_hz: 46_918_402.8,
            metas: Vec::new(),
            line_pos,
            line_vel,
            az0_abs: 1000,
            beam: 5,
        }
    }

    #[test]
    fn grid_folgt_rda_pixeln() {
        let win = fake_window();
        let ntx = crate::chirp::num_tx_samples(&win.chirp);
        let g = geo_for_window(&win, 1000, 1004, 10, 14).unwrap();
        assert_eq!(g.grid.naz, 4);
        assert_eq!(g.grid.nrange, 4);
        assert_eq!(g.plat.len(), 8);
        // dx = v_eff·PRI = 4,26 m; x(1000) = (1000−1004)·dx.
        assert!((g.grid.dx_az - 4.26).abs() < 1e-9);
        let x1000 = (1000.0 - 1004.0) * 4.26;
        let c = g.grid.pixel_pos(0, 0);
        assert!((c.x - x1000).abs() < 1e-9, "x = {}", c.x);
        // Kugel-Konsistenz: Ziel auf Kugel (|origin + c| = re) und im
        // Slant-Abstand zur Mitten-Plattform (bis x-Offset, mm-Genauigkeit).
        let re = (WGS84_A_M + WGS84_B_M) / 2.0;
        let rr = (c.y * c.y + (re + c.z) * (re + c.z)).sqrt();
        assert!((rr - re).abs() < 1e-6, "r = {rr}");
        let d = g.plat[4].dist(c);
        assert!((d - win.slant[10 + ntx]).abs() < 0.01, "d = {d}");
        // Plattform Mitte ≈ (0, 0, h) — x along-track sortiert.
        assert!(g.plat[4].z > 692_999.0 && g.plat[4].z < 693_001.0);
        assert!(g.plat[0].x < g.plat[7].x);
        // Zeitraster: t0 = 2·slant0/c, dt = 1/fs.
        assert!((g.t0_s - 2.0 * 900_000.0 / SPEED_OF_LIGHT).abs() < 1e-12);
        assert!((g.dt_s - 1.0 / 46_918_402.8).abs() < 1e-18);
    }

    #[test]
    fn leere_fenster_sind_fehler() {
        let win = fake_window();
        assert!(geo_for_window(&win, 1002, 1002, 10, 14).is_err());
        assert!(geo_for_window(&win, 1000, 1004, 12, 12).is_err());
        assert!(geo_for_window(&win, 1000, 1004, 10, 1000).is_err());
    }

    #[test]
    fn rundtrip_punktziel_an_grid_position() {
        // Vorwärts-Simulation (Up-Chirp + Trägerphase) durch die Brücken-
        // Geometrie, dann TDBP: Peak muss am Ziel-Pixel liegen (Lage ±1 px).
        use crate::range::RangeCompressor;
        use crate::tdbp::{TdbpInput, tdbp_cpu};
        use crate::types::Complex32;
        let win = fake_window();
        let naz = win.naz;
        let n0 = win.n0;
        let g = geo_for_window(&win, 1000, 1008, 10, 40).unwrap();
        assert_eq!((g.grid.naz, g.grid.nrange), (8, 30));
        // Ziel an Grid-Pixel (3, 17), Up-Chirp (Rate wie Test-Chirp).
        let tgt = g.grid.pixel_pos(3, 17);
        let k = 1.0e12;
        let txpl = win.chirp.txpl_s;
        let fs = win.fs_hz;
        let lambda = SPEED_OF_LIGHT / 5.405e9;
        let mut raw = vec![Complex32::zero(); naz * n0];
        for (p, plat) in g.plat.iter().enumerate() {
            let d = plat.dist(tgt);
            let s0 = (2.0 * d / SPEED_OF_LIGHT - g.t0_s) / g.dt_s;
            let ph_c = -4.0 * std::f64::consts::PI * d / lambda;
            let (cm_re, cm_im) = (ph_c.cos(), ph_c.sin());
            for i in 0..ntx_len(&win) {
                let t = (i as f64 - (ntx_len(&win) - 1) as f64 / 2.0) / fs;
                if t.abs() > txpl / 2.0 {
                    continue;
                }
                let s = (s0 + (i as f64 - (ntx_len(&win) - 1) as f64 / 2.0)).round() as isize;
                if s < 0 || s >= n0 as isize {
                    continue;
                }
                let ph = 2.0 * std::f64::consts::PI * (k / 2.0 * t * t);
                let (cr, ci) = (ph.cos(), ph.sin());
                let c = &mut raw[p * n0 + s as usize];
                // Echo = Chirp × Trägerphase (akkumuliert, hier 1 Ziel).
                *c = Complex32::new(
                    (cr * cm_re - ci * cm_im) as f32,
                    (cr * cm_im + ci * cm_re) as f32,
                );
            }
        }
        // Produktionskette: komprimieren + rückversetzen + TDBP.
        let comp = RangeCompressor::new(&win.chirp, n0);
        comp.compress_rows(&mut raw);
        let shift = crate::rda::correlation_shift_samples(ntx_len(&win), n0);
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
        let (peak, _) = crate::tdbp::peak_power(&img);
        let (pa, pr) = (peak / g.grid.nrange, peak % g.grid.nrange);
        assert!(
            pa.abs_diff(3) <= 1 && pr.abs_diff(17) <= 1,
            "Peak bei ({pa}, {pr}), erwartet (3, 17)"
        );
    }

    /// Replika-Länge des Test-Chirps (ntx aus TXPL·fs).
    fn ntx_len(win: &LoadedWindow) -> usize {
        crate::chirp::num_tx_samples(&win.chirp)
    }
}
