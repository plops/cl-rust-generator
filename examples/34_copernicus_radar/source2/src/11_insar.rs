//! Interferometrischer SAR-Prozessor (`InSAR`) für Sentinel-1 TOPS (IW/EW) auf CPU und GPU.
//!
//! Implementiert die vollständige interferometrische Verarbeitungskette:
//! 1. **3D-ECEF-Orbit-Basislinienanalyse** (`BaselineMetrics`, `compute_orbit_baseline`)
//! 2. **WGS-84 Zero-Doppler-Geolocation & Flat-Earth-Phasenmodell** (`solve_zero_doppler_target_ecef`,
//!    `project_target_to_slave`, `estimate_burst_geometric_model`)
//! 3. **Phasenerhaltende TOPS-Fokussierung** mit gemeinsamem Master/Slave-Steering $(f_{\text{DC,mid}}, k_t)$
//!    und analytischer Kompensation der stationären TOPS-Restphase
//! 4. **Zweistufige Sub-Pixel-Koregistrierung** (2D-Intensitäts-Kreuzkorrelation + Maximierung der
//!    komplexen Interferogramm-Kohärenz) sowie **Enhanced Spectral Diversity (ESD)** in den
//!    azimutalen Burst-Überlappungszonen
//! 5. **Komplexes Burst-Stitching & Multi-Swath-Interferogramm-Mosaikierung** (`DeburstedInsarSwath`,
//!    `deburst_insar_swath`, `mosaic_insar_swaths`)
//! 6. **Multilook-Kohärenzschätzung**, **adaptives Goldstein-Werner 2D-FFT-Spektralfilter**
//!    (`goldstein_filter_interferogram`) und **kohärenzgewichtetes 2D-Phase-Unwrapping**
//!    (`unwrap_phase_2d`)
//! 7. **Farb-Visualisierung** (HSV-Phasenrad mit Kohärenz-/Intensitätsmodulation, Viridis-Kohärenz,
//!    Diverging-Colormap für Unwrapped Phase, 2×2-Übersichtspanel) und JSON-Metrik-Export.

use crate::ephem::{EphemPoint, WGS84_A_M, WGS84_B_M, interp_pos, interp_vel};
use crate::gpu::GpuSarContext;
use crate::ingest::L0Dataset;
use crate::mosaic::{db_mean_std, power_to_db, save_db_png};
use crate::rda::{TopsSteering, focus_tops_burst_insar_cpu, ka_rate_hz_s};
use crate::types::{BurstBlock, Complex32, Error, LAMBDA, Vec3d};
use image::ImageEncoder;
use image::codecs::png::{CompressionType, FilterType, PngEncoder};
use num_complex::Complex32 as Nc32;
use rustfft::FftPlanner;
use std::f64::consts::PI;
use std::path::Path;
use std::time::Instant;

/// 3D-ECEF-Orbit-Basislinienmetriken zwischen Master- und Slave-Überflug.
#[derive(Clone, Copy, Debug)]
pub struct BaselineMetrics {
    /// 3D-Abstand im Punkt der nächsten Annäherung (Closest Approach) [m].
    pub b_3d_m: f64,
    /// Radiale (Nadir/Zenit) Basislinienkomponente [m].
    pub b_radial_m: f64,
    /// Horizontale Cross-Track-Basislinienkomponente [m].
    pub b_cross_m: f64,
    /// Senkrechte Basislinie $B_\perp$ relativ zur mittleren Look-Richtung ($\theta \approx 34^\circ$) [m].
    pub b_perp_m: f64,
    /// Parallele Basislinie $B_\parallel$ relativ zur mittleren Look-Richtung [m].
    pub b_para_m: f64,
    /// Zeitversatz der nächsten Annäherung (Slave minus Master, Tagessekunden) [s].
    pub dt_closest_approach_s: f64,
    /// Höhen-Ambiguität $h_{2\pi} = \frac{\lambda R \sin\theta}{2 |B_\perp|}$ [m/Zyklus].
    pub height_ambiguity_m: f64,
}

/// Berechnet die 3D-ECEF-Basislinie am Punkt der nächsten Annäherung zwischen Master- und Slave-Ephemeriden.
///
/// Beachtet, dass `ephem.time_s` und `pulse_times` fortlaufende GPS-Sekunden sind und
/// zwischen Master und Slave eine konstante Tagesdifferenz $N_{\text{days}} \times 86\,400\text{ s}$ liegt.
pub fn compute_orbit_baseline(
    ephem_m: &[EphemPoint],
    ephem_s: &[EphemPoint],
    r_mid_m: f64,
) -> BaselineMetrics {
    if ephem_m.is_empty() || ephem_s.is_empty() {
        return BaselineMetrics {
            b_3d_m: 0.0,
            b_radial_m: 0.0,
            b_cross_m: 0.0,
            b_perp_m: 0.0,
            b_para_m: 0.0,
            dt_closest_approach_s: 0.0,
            height_ambiguity_m: f64::INFINITY,
        };
    }

    let tm = ephem_m[ephem_m.len() / 2].time_s;
    let ts_mid = ephem_s[ephem_s.len() / 2].time_s;
    let day_offset_s = ((ts_mid - tm) / 86_400.0).round() * 86_400.0;

    let pm = interp_pos(ephem_m, tm);
    let vm = interp_vel(ephem_m, tm);
    let v_norm2 = vm.dot(vm).max(1.0);

    // Sekanten-/Projektionsiteration zur Bestimmung von t_s mit (P_s(t_s) - P_m(t_m)) . V_m = 0
    // Startwert ist derselbe Tageszeitpunkt im Slave-Orbit: tm + day_offset_s
    let mut ts = tm + day_offset_s;
    for _ in 0..8 {
        let ps = interp_pos(ephem_s, ts);
        let dp = ps - pm;
        let dt_along = dp.dot(vm) / v_norm2;
        ts -= dt_along;
    }

    let ps = interp_pos(ephem_s, ts);
    let dp = ps - pm;

    // Lokales Orbit-Dreibein: u_r (radial nach außen), u_v (entlang V_m), u_c (Cross-Track rechts: u_v × u_r)
    let u_r = pm * (1.0 / pm.norm().max(1.0));
    let u_v = vm * (1.0 / vm.norm().max(1.0));
    let c_raw = u_v.cross(u_r);
    let u_c = c_raw * (1.0 / c_raw.norm().max(1e-12));

    let b_radial_m = dp.dot(u_r);
    let b_cross_m = dp.dot(u_c);
    let b_3d_m = dp.norm();

    // Mittlerer Look-Winkel für Sentinel-1 IW (~33.8° ab Nadir)
    let h_sat = (pm.norm() - WGS84_A_M).max(600_000.0);
    let cos_look = (h_sat / r_mid_m.max(h_sat)).clamp(0.5, 0.95);
    let sin_look = (1.0 - cos_look * cos_look).sqrt();

    // Sichtlinien-Einheitsvektor vom Satelliten zum Ziel: l = -cos(theta)*u_r + sin(theta)*u_c
    let b_para_m = -b_radial_m * cos_look + b_cross_m * sin_look;
    let b_perp_m = b_radial_m * sin_look + b_cross_m * cos_look;

    let inc_angle = (sin_look * pm.norm() / WGS84_A_M).clamp(0.3, 0.95).asin();
    let height_ambiguity_m = if b_perp_m.abs() > 0.05 {
        (LAMBDA * r_mid_m * inc_angle.sin()) / (2.0 * b_perp_m.abs())
    } else {
        f64::INFINITY
    };

    BaselineMetrics {
        b_3d_m,
        b_radial_m,
        b_cross_m,
        b_perp_m,
        b_para_m,
        dt_closest_approach_s: (ts - day_offset_s) - tm,
        height_ambiguity_m,
    }
}

/// Löst die 3D-ECEF-Zielkoordinate $\mathbf{X}$ auf dem WGS-84-Ellipsoid (Höhe `h_ellip_m`)
/// am Schnittpunkt von Range-Kugel $\|\mathbf{X} - \mathbf{P}_s\| = R$ und Zero-Doppler-Ebene
/// $(\mathbf{X} - \mathbf{P}_s) \cdot \mathbf{V}_s = 0$ (Rechts-Sicht-Geometrie).
pub fn solve_zero_doppler_target_ecef(
    pos_s: Vec3d,
    vel_s: Vec3d,
    slant_range_m: f64,
    h_ellip_m: f64,
) -> Vec3d {
    let u_v = vel_s * (1.0 / vel_s.norm().max(1.0));
    // Gram-Schmidt-Orthogonalisierung von `pos_s` gegen `u_v`:
    // Dadurch liegt die Nadir-Achse `u_perp` exakt in der Zero-Doppler-Ebene (u_perp . u_v == 0),
    // und jeder Blickvektor look = u_c * sin(theta) - u_perp * cos(theta) erfüllt
    // (X - pos_s) . vel_s == 0 auf Maschinengenauigkeit!
    let r_perp_vec = pos_s - u_v * pos_s.dot(u_v);
    let r_perp = r_perp_vec.norm().max(1.0);
    let u_perp = r_perp_vec * (1.0 / r_perp);
    // Rechtsblick von Sentinel-1: u_v × u_perp zeigt nach rechts vom Sub-Satellitenpfad
    let c_raw = u_v.cross(u_perp);
    let u_c = c_raw * (1.0 / c_raw.norm().max(1e-12));

    let a_e = WGS84_A_M + h_ellip_m;
    let b_e = WGS84_B_M + h_ellip_m;
    let r_e_approx = 0.5 * (a_e + b_e);

    // Startschätzung über Kugeldreieck in der Zero-Doppler-Ebene
    let along_s = pos_s.dot(u_v);
    let r_e_perp2 = (r_e_approx * r_e_approx - along_s * along_s).max(1.0);
    let cos_theta0 = ((r_perp * r_perp + slant_range_m * slant_range_m - r_e_perp2)
        / (2.0 * r_perp * slant_range_m))
        .clamp(0.2, 0.99);
    let mut theta = cos_theta0.acos();

    // 6 Newton-Raphson-Iterationen für exakte Ellipsoidbedingung F(theta) = 0
    for _ in 0..6 {
        let (s, c) = theta.sin_cos();
        let look = u_c * s - u_perp * c;
        let x = pos_s + look * slant_range_m;
        let f = (x.x * x.x + x.y * x.y) / (a_e * a_e) + (x.z * x.z) / (b_e * b_e) - 1.0;

        let dlook_dtheta = u_c * c + u_perp * s;
        let dx_dtheta = dlook_dtheta * slant_range_m;
        let df_dtheta = 2.0 * (x.x * dx_dtheta.x + x.y * dx_dtheta.y) / (a_e * a_e)
            + 2.0 * (x.z * dx_dtheta.z) / (b_e * b_e);
        if df_dtheta.abs() > 1e-12 {
            theta -= f / df_dtheta;
            theta = theta.clamp(0.05, 1.45);
        }
    }

    let (s, c) = theta.sin_cos();
    let look = u_c * s - u_perp * c;
    pos_s + look * slant_range_m
}

/// Projiziert einen 3D-ECEF-Bodenpunkt `target` in die Zero-Doppler-Geometrie des Slave-Orbits
/// und gibt `(t_slave_zero_doppler_s, slant_range_slave_m)` zurück.
///
/// Wichtig: Im erdfesten ECEF-System ist die Zero-Doppler-Bedingung durch die **ECEF-Relativgeschwindigkeit**
/// $\mathbf{V}_{\text{ECEF}}(t)$ gegeben: $\frac{d}{dt}\|\mathbf{X}_{\text{ECEF}} - \mathbf{P}_{\text{ECEF}}(t)\|^2 = 0
/// \iff (\mathbf{X}_{\text{ECEF}} - \mathbf{P}_{\text{ECEF}}(t)) \cdot \mathbf{V}_{\text{ECEF}}(t) = 0$.
pub fn project_target_to_slave(
    ephem_s: &[EphemPoint],
    target: Vec3d,
    t_guess_s: f64,
) -> (f64, f64) {
    let mut t = t_guess_s;
    for _ in 0..8 {
        let ps = interp_pos(ephem_s, t);
        let vs = interp_vel(ephem_s, t);
        let los = target - ps;
        let v_norm2 = vs.dot(vs).max(1.0);
        // f(t) = (target - P_s(t)) . V_s(t), f'(t) ≈ -|V_s|^2 => t_new = t + f(t)/|V_s|^2
        let dt = los.dot(vs) / v_norm2;
        t += dt;
        if dt.abs() < 1e-8 {
            break;
        }
    }
    let ps = interp_pos(ephem_s, t);
    let r_s = (target - ps).norm();
    (t, r_s)
}

/// Geometrisches und empirisch verfeinertes Koregistrierungs- und Flat-Earth-Phasenmodell eines Bursts.
#[derive(Clone, Copy, Debug)]
pub struct BurstCoregModel {
    /// Affines Azimut-Shift-Modell: `da(af, rf) = da_coeffs[0] + da_coeffs[1]*af + da_coeffs[2]*rf` [Pixel].
    pub da_coeffs: [f64; 3],
    /// Affines Range-Shift-Modell: `dr(af, rf) = dr_coeffs[0] + dr_coeffs[1]*af + dr_coeffs[2]*rf` [Pixel].
    pub dr_coeffs: [f64; 3],
    /// Polynomiales Phasenmodell (Flat-Earth + Residual-Rampe):
    /// `phi(af, rf) = phi_coeffs[0] + phi_coeffs[1]*af + phi_coeffs[2]*rf + phi_coeffs[3]*rf^2` [rad].
    pub phi_coeffs: [f64; 4],
    /// Mittlere Burst-Kohärenz nach Koregistrierung und Rampenabzug.
    pub mean_coherence: f64,
}

/// Fokussierter komplexer Burst für die InSAR-Weiterverarbeitung.
#[derive(Clone, Debug)]
pub struct InsarFocusedBurst {
    pub burst_index: usize,
    pub n_az_raw: usize,
    pub n_az_fft: usize,
    pub n_rg: usize,
    pub n_valid_rg: usize,
    pub mid_time_s: f64,
    pub pri_s: f64,
    pub r0_m: f64,
    pub dr_m: f64,
    pub veff_mid: f64,
    pub steering: TopsSteering,
    pub data: Vec<Complex32>,
}

impl InsarFocusedBurst {
    /// Gültiges fokussiertes Azimut-Fenster `[az_start, az_end)` analog zu `FocusedBurst::valid_az_window`.
    /// Im Deramp-RDA-Prozessor (`focus_tops_burst_*`) fokussiert ein Ziel mit Zero-Doppler-Zeit `t_0`
    /// relativ zur Burst-Mitte bei `tau_foc = (a - n_az_fft/2) * PRI = alpha * t_0 = t_beam`.
    /// Da die beleuchtete Strahlzeit `t_beam` genau die Pulsdauer `[-n_az_raw/2 * PRI, +n_az_raw/2 * PRI]`
    /// durchläuft, belegt das fokussierte Burst-Bild im FFT-Puffer `n_az_raw` Zeilen (mit Zeitschritt
    /// `dt_foc = PRI / alpha` pro Zeile). Mit 84 % Nutzfenster überlappen benachbarte Bursts (`stride ≈ 0.76 * n_az_raw`)
    /// lückenlos für das anschließende ESD-Debursting.
    pub fn valid_az_window(&self) -> (usize, usize) {
        let valid_len = ((self.n_az_raw as f64) * 0.84).round() as usize;
        let valid_len = valid_len.clamp(16, self.n_az_fft);
        let start = (self.n_az_fft - valid_len) / 2;
        (start, start + valid_len)
    }

    /// Effektiver Azimut-Zeitschritt pro fokussierter Zeile: `dt_foc = PRI / alpha`.
    pub fn focused_dt_az_s(&self) -> f64 {
        let ka_mid = ka_rate_hz_s(
            self.veff_mid,
            self.r0_m + (self.n_rg as f64 * 0.5) * self.dr_m,
        );
        let alpha = (ka_mid / (ka_mid - self.steering.kt_hz_s)).abs().max(1e-3);
        self.pri_s / alpha
    }

    /// Effektive TOPS-Phasenrate `k_top = k_a * k_t / (k_a - k_t)` [Hz/s] in der Schwadmitte.
    pub fn ktop_mid_hz_s(&self) -> f64 {
        let ka_mid = ka_rate_hz_s(
            self.veff_mid,
            self.r0_m + (self.n_rg as f64 * 0.5) * self.dr_m,
        );
        let keff_mid = ka_mid - self.steering.kt_hz_s;
        ka_mid * self.steering.kt_hz_s / keff_mid
    }
}

/// Berechnet das analytische 3D-ECEF-Orbit-Modell für den Shift `(da, dr)` und die Flat-Earth-Phase
/// zwischen einem fokussierten Master- und Slave-Burst.
pub fn estimate_burst_geometric_model(
    m_burst: &InsarFocusedBurst,
    s_burst: &InsarFocusedBurst,
    ephem_m: &[EphemPoint],
    ephem_s: &[EphemPoint],
) -> BurstCoregModel {
    if ephem_m.is_empty() || ephem_s.is_empty() {
        return BurstCoregModel {
            da_coeffs: [0.0; 3],
            dr_coeffs: [0.0; 3],
            phi_coeffs: [0.0; 4],
            mean_coherence: 0.0,
        };
    }

    let dt_foc = m_burst.focused_dt_az_s();
    let dr_m = m_burst.dr_m;
    let n_az = m_burst.n_az_fft as f64;
    let n_rg = m_burst.n_rg as f64;

    // `m_burst.mid_time_s` und `ephem.time_s` sind beide fortlaufende GPS-Sekunden (~1.475e9).
    // Zwischen Master und Slave liegt lediglich eine konstante Tagesdifferenz (~12 * 86400 s).
    let day_offset_s = ((s_burst.mid_time_s - m_burst.mid_time_s) / 86_400.0).round() * 86_400.0;

    // Werte für (da_geo, dr_geo, phi_flat) an 3×3 Stützstellen um die Burst-Mitte
    // Im Deramp-RDA-Prozessor (`focus_tops_burst_*`) ist `k_a < 0` und `k_t > 0`, sodass
    // `k_a / (k_a - k_t) = +alpha > 0` gilt: eine größere Zeilennummer `af = a - n_az_fft/2 > 0`
    // gehört zu einer späteren Zero-Doppler-Zeit `t_m = m_burst.mid_time_s + af * dt_foc`!
    // WICHTIG: Weil `focus_tops_burst_*` die Range-Kompression mit `conj(H_chirp)` (ohne Phase-Center-Offset
    // `+j*pi*B*T_p`) und die RCMC relativ zur Schwadmitte `r_mid` ausführt, gehört die fokussierte Spalte
    // `r` (mit `rf = r - n_rg/2`) geometrisch zum Slant-Range:
    //   `r_m(rf) = m_burst.r0_m + (n_rg * 0.5 + rf) * dr_m`
    // und die Laufzeit-Phase eines Punktziels mit Slant-Range-Differenz `dR = R_s - R_m` lautet:
    //   `phi_M - phi_S = (-4*pi/lambda * R_m) - (-4*pi/lambda * R_s) = +4*pi/lambda * (R_s - R_m)`!
    let eval_pt = |af: f64, rf: f64| -> (f64, f64, f64) {
        let t_m = m_burst.mid_time_s + af * dt_foc;
        let r_m = m_burst.r0_m + (n_rg * 0.5 + rf) * dr_m;
        let pos_m = interp_pos(ephem_m, t_m);
        let vel_m = interp_vel(ephem_m, t_m);
        let target = solve_zero_doppler_target_ecef(pos_m, vel_m, r_m, 0.0);
        let (t_s, r_s) = project_target_to_slave(ephem_s, target, t_m + day_offset_s);

        let dt_target_s = (t_s - s_burst.mid_time_s) - (t_m - m_burst.mid_time_s);
        let da = dt_target_s / dt_foc;

        // Range-Offset in Bins:
        let sr = (r_s - s_burst.r0_m) / dr_m;
        let mr = (r_m - m_burst.r0_m) / dr_m;
        let dr = sr - mr;

        // Analytische Flat-Earth-Phase:
        // Ein Radar-Echo bei Laufzeit tau = 2 R / c hat nach Downconversion die Trägerphase
        // phi_echo = -2 * pi * f_c * (2 R / c) = -4 * pi * R / lambda.
        // Damit hat das Interferogramm I = M * conj(S) die Flat-Earth-Phase:
        //   phi_FE = phi_M - phi_S = (-4*pi/lambda * R_m) - (-4*pi/lambda * R_s) = +4*pi/lambda * (R_s - R_m).
        let phi_flat = 4.0 * PI * (r_s - r_m) / LAMBDA;
        (da, dr, phi_flat)
    };

    let step_a = (n_az * 0.15).max(16.0);
    let step_r = (n_rg * 0.25).max(256.0);

    let (da0, dr0, phi0) = eval_pt(0.0, 0.0);
    let (da_ap, dr_ap, phi_ap) = eval_pt(step_a, 0.0);
    let (da_am, dr_am, phi_am) = eval_pt(-step_a, 0.0);
    let (da_rp, dr_rp, phi_rp) = eval_pt(0.0, step_r);
    let (da_rm, dr_rm, phi_rm) = eval_pt(0.0, -step_r);

    let da_da = (da_ap - da_am) / (2.0 * step_a);
    let da_dr = (da_rp - da_rm) / (2.0 * step_r);
    let dr_da = (dr_ap - dr_am) / (2.0 * step_a);
    let dr_dr = (dr_rp - dr_rm) / (2.0 * step_r);

    let dphi_da = (phi_ap - phi_am) / (2.0 * step_a);
    let dphi_dr = (phi_rp - phi_rm) / (2.0 * step_r);
    let dphi_dr2 = (phi_rp - 2.0 * phi0 + phi_rm) / (step_r * step_r);
    let phi0_wrapped = phi0.sin().atan2(phi0.cos());

    BurstCoregModel {
        da_coeffs: [da0, da_da, da_dr],
        dr_coeffs: [dr0, dr_da, dr_dr],
        phi_coeffs: [phi0_wrapped, dphi_da, dphi_dr, dphi_dr2],
        mean_coherence: 0.0,
    }
}

/// Verfeinert den konstanten Sub-Pixel-Shift `(da0, dr0)` und die Phasenrampe `(dphi_da, dphi_dr)`
/// auf einem repräsentativen Zentralbereich des Bursts durch zweistufige Maximierung von
/// Intensitätskorrelation und komplexer Interferogramm-Kohärenz.
pub fn refine_burst_coreg_empirical(
    m_burst: &InsarFocusedBurst,
    s_burst: &InsarFocusedBurst,
    init_model: BurstCoregModel,
) -> BurstCoregModel {
    let (az0, az1) = m_burst.valid_az_window();
    let n_az = m_burst.n_az_fft;
    let n_rg = m_burst.n_rg;
    let n_valid_rg = m_burst.n_valid_rg.min(n_rg);

    // Wähle das zentrale Vollapertur-Fenster (±16% von n_az_raw um die Burst-Mitte),
    // in dem 100 % der synthetischen Apertur ohne Burst-Rand-Trunkierung im Hauptkeulen-Maximum liegen.
    let core_half = ((m_burst.n_az_raw as f64) * 0.16).round() as usize;
    let mid_row = n_az / 2;
    let a_start = mid_row
        .saturating_sub(core_half.max(8))
        .max(az0)
        .min(n_az.saturating_sub(8));
    let a_end = (mid_row + core_half.max(8))
        .min(az1)
        .max(a_start + 4)
        .min(n_az);
    let r_start = (n_valid_rg / 8).max(16);
    let r_end = (n_valid_rg * 7 / 8)
        .max(r_start + 32)
        .min(n_rg.saturating_sub(8));

    let step_a = ((a_end - a_start) / 64).max(1);
    let step_r = ((r_end - r_start) / 256).max(1);

    // Vorberechnete Master-Log-Amplitude auf den Stützstellen (robuster gegen einzelne helle Punktziele)
    let mut m_pts = Vec::new();
    let mut mean_m = 0.0f64;
    for a in (a_start..a_end).step_by(step_a) {
        for r in (r_start..r_end).step_by(step_r) {
            let c = m_burst.data[a * n_rg + r];
            let p = (f64::from(c.norm_sqr()) + 1e-12).ln();
            mean_m += p;
            m_pts.push((a, r, p));
        }
    }
    if !m_pts.is_empty() {
        mean_m /= m_pts.len() as f64;
    }
    for item in &mut m_pts {
        item.2 -= mean_m;
    }

    let eval_icorr = |cand_da: f64, cand_dr: f64| -> f64 {
        let mut s_vals = Vec::with_capacity(m_pts.len());
        let mut mean_s = 0.0f64;
        for &(a, r, dm) in &m_pts {
            let af = a as f64 - (n_az as f64) * 0.5;
            let rf = r as f64 - (n_rg as f64) * 0.5;
            let sa =
                a as f64 + cand_da + init_model.da_coeffs[1] * af + init_model.da_coeffs[2] * rf;
            let sr =
                r as f64 + cand_dr + init_model.dr_coeffs[1] * af + init_model.dr_coeffs[2] * rf;
            if sa >= 0.0
                && sa < (n_az.saturating_sub(1) as f64)
                && sr >= 0.0
                && sr < (n_rg.saturating_sub(1) as f64)
            {
                let a0 = sa as usize;
                let r0 = sr as usize;
                let fa = (sa - a0 as f64) as f32;
                let fr = (sr - r0 as f64) as f32;
                let s00 = s_burst.data[a0 * n_rg + r0];
                let s01 = s_burst.data[a0 * n_rg + r0 + 1];
                let s10 = s_burst.data[(a0 + 1) * n_rg + r0];
                let s11 = s_burst.data[(a0 + 1) * n_rg + r0 + 1];
                let s_re = (1.0 - fa) * ((1.0 - fr) * s00.re + fr * s01.re)
                    + fa * ((1.0 - fr) * s10.re + fr * s11.re);
                let s_im = (1.0 - fa) * ((1.0 - fr) * s00.im + fr * s01.im)
                    + fa * ((1.0 - fr) * s10.im + fr * s11.im);
                let ps = (f64::from(s_re * s_re + s_im * s_im) + 1e-12).ln();
                mean_s += ps;
                s_vals.push((dm, ps));
            }
        }
        if s_vals.len() < 16 {
            return 0.0;
        }
        mean_s /= s_vals.len() as f64;
        let mut sum_ms = 0.0f64;
        let mut sum_m2 = 0.0f64;
        let mut sum_s2 = 0.0f64;
        for (dm, ps) in s_vals {
            let ds = ps - mean_s;
            sum_ms += dm * ds;
            sum_m2 += dm * dm;
            sum_s2 += ds * ds;
        }
        if sum_m2 > 0.0 && sum_s2 > 0.0 {
            sum_ms / (sum_m2 * sum_s2).sqrt()
        } else {
            0.0
        }
    };

    let eval_patch_coh = |cand_da: f64, cand_dr: f64, dphi_a: f64, dphi_r: f64| -> f64 {
        let mut total_num = 0.0f64;
        let mut total_den = 0.0f64;
        // Verwende kompakte zusammenhängende 5×20-Pixel-Multilook-Patches auf vollem Gitter (Schritt 1)
        // an ausgewählten Ankerpunkten im Burst, um die wahre lokale Interferogramm-Kohärenz zu messen!
        let win_a = 5usize;
        let win_r = 20usize;
        let stride_pa = ((a_end - a_start) / 10).max(win_a);
        let stride_pr = ((r_end - r_start) / 16).max(win_r);

        for pa0 in (a_start..a_end.saturating_sub(win_a)).step_by(stride_pa) {
            for pr0 in (r_start..r_end.saturating_sub(win_r)).step_by(stride_pr) {
                let mut sum_re = 0.0f64;
                let mut sum_im = 0.0f64;
                let mut sum_pm = 0.0f64;
                let mut sum_ps = 0.0f64;

                for a in pa0..(pa0 + win_a) {
                    let af = a as f64 - (n_az as f64) * 0.5;
                    for r in pr0..(pr0 + win_r) {
                        let rf = r as f64 - (n_rg as f64) * 0.5;
                        let sa = a as f64
                            + cand_da
                            + init_model.da_coeffs[1] * af
                            + init_model.da_coeffs[2] * rf;
                        let sr = r as f64
                            + cand_dr
                            + init_model.dr_coeffs[1] * af
                            + init_model.dr_coeffs[2] * rf;
                        if sa >= 0.0
                            && sa < (n_az.saturating_sub(1) as f64)
                            && sr >= 0.0
                            && sr < (n_rg.saturating_sub(1) as f64)
                        {
                            let a0 = sa as usize;
                            let r0 = sr as usize;
                            let fa = (sa - a0 as f64) as f32;
                            let fr = (sr - r0 as f64) as f32;
                            let s00 = s_burst.data[a0 * n_rg + r0];
                            let s01 = s_burst.data[a0 * n_rg + r0 + 1];
                            let s10 = s_burst.data[(a0 + 1) * n_rg + r0];
                            let s11 = s_burst.data[(a0 + 1) * n_rg + r0 + 1];
                            let s_re = (1.0 - fa) * ((1.0 - fr) * s00.re + fr * s01.re)
                                + fa * ((1.0 - fr) * s10.re + fr * s11.re);
                            let s_im = (1.0 - fa) * ((1.0 - fr) * s00.im + fr * s01.im)
                                + fa * ((1.0 - fr) * s10.im + fr * s11.im);
                            let m_c = m_burst.data[a * n_rg + r];
                            let prod_re = f64::from(m_c.re * s_re + m_c.im * s_im);
                            let prod_im = f64::from(m_c.im * s_re - m_c.re * s_im);
                            let phi =
                                dphi_a * af + dphi_r * rf + init_model.phi_coeffs[3] * rf * rf;
                            let (sin_p, cos_p) = phi.sin_cos();
                            let deramp_re = prod_re * cos_p + prod_im * sin_p;
                            let deramp_im = prod_im * cos_p - prod_re * sin_p;
                            sum_re += deramp_re;
                            sum_im += deramp_im;
                            sum_pm += f64::from(m_c.norm_sqr());
                            sum_ps += f64::from(s_re * s_re + s_im * s_im);
                        }
                    }
                }
                let den = (sum_pm * sum_ps).sqrt();
                if den > 1e-12 {
                    total_num += sum_re.hypot(sum_im);
                    total_den += den;
                }
            }
        }
        if total_den > 1e-12 {
            total_num / total_den
        } else {
            0.0
        }
    };

    // Das 3D-ECEF-Orbitmodell (`init_model`) aus den Downlink-Ephemeriden ist für Sentinel-1C
    // bereits auf Bruchteile eines Pixels genau. Über Wasser oder dichten subtropischen Wald
    // kann eine unbeschränkte 2D-Suche auf zufällige Speckle-Peaks springen.
    // Wir prüfen daher ein enges Sub-Pixel-Fenster (±1.5 Pixel) um das Orbit-Modell und übernehmen
    // einen empirischen Offset nur dann, wenn sowohl eine klare Intensitätskorrelation (`icorr > 0.12`)
    // als auch eine deutliche Kohärenzsteigerung gegenüber dem reinen Orbit-Modell vorliegen.
    let geo_da = init_model.da_coeffs[0];
    let geo_dr = init_model.dr_coeffs[0];
    let geo_coh = eval_patch_coh(
        geo_da,
        geo_dr,
        init_model.phi_coeffs[1],
        init_model.phi_coeffs[2],
    );
    let geo_icorr = eval_icorr(geo_da, geo_dr);

    let mut best_da = geo_da;
    let mut best_dr = geo_dr;
    let mut best_score = geo_coh + 0.5 * geo_icorr.max(0.0);

    for ida in -6..=6 {
        for idr in -6..=6 {
            let cand_da = geo_da + (ida as f64) * 0.25;
            let cand_dr = geo_dr + (idr as f64) * 0.25;
            let icorr = eval_icorr(cand_da, cand_dr);
            if icorr < 0.08 {
                continue;
            }
            let coh = eval_patch_coh(
                cand_da,
                cand_dr,
                init_model.phi_coeffs[1],
                init_model.phi_coeffs[2],
            );
            // Leichte Tikhonov-Regularisierung zum präzisen Orbit-Prior
            let dist2 = (cand_da - geo_da).powi(2) + (cand_dr - geo_dr).powi(2);
            let score = coh + 0.5 * icorr - 0.01 * dist2;
            if score > best_score + 0.015 {
                best_score = score;
                best_da = cand_da;
                best_dr = cand_dr;
            }
        }
    }

    // Feinschritt (0.05 Pixel) um `best_da, best_dr`, falls die Szene hinreichenden Kontrast bietet
    if eval_icorr(best_da, best_dr) >= 0.08 {
        let c_da = best_da;
        let c_dr = best_dr;
        for ida in -4..=4 {
            for idr in -4..=4 {
                let cand_da = c_da + (ida as f64) * 0.05;
                let cand_dr = c_dr + (idr as f64) * 0.05;
                let icorr = eval_icorr(cand_da, cand_dr);
                let coh = eval_patch_coh(
                    cand_da,
                    cand_dr,
                    init_model.phi_coeffs[1],
                    init_model.phi_coeffs[2],
                );
                let dist2 = (cand_da - geo_da).powi(2) + (cand_dr - geo_dr).powi(2);
                let score = coh + 0.5 * icorr - 0.01 * dist2;
                if score > best_score {
                    best_score = score;
                    best_da = cand_da;
                    best_dr = cand_dr;
                }
            }
        }
    }

    // 3. Schätze empirische Rest-Phasenrampe (dphi_da_res, dphi_dr_res) mittels komplexer
    //    Lag-2-Autokorrelation (vermeidet bilineares Lag-1-Interpolations-Übersprechen),
    //    eng beschränkt um das präzise 3D-ECEF-Orbitmodell.
    let mut sum_az_re = 0.0f64;
    let mut sum_az_im = 0.0f64;
    let mut sum_rg_re = 0.0f64;
    let mut sum_rg_im = 0.0f64;

    let sample_ifg = |a: usize, r: usize| -> Complex32 {
        let af = a as f64 - (n_az as f64) * 0.5;
        let rf = r as f64 - (n_rg as f64) * 0.5;
        let sa = a as f64 + best_da + init_model.da_coeffs[1] * af + init_model.da_coeffs[2] * rf;
        let sr = r as f64 + best_dr + init_model.dr_coeffs[1] * af + init_model.dr_coeffs[2] * rf;
        if sa < 0.0
            || sa >= (n_az.saturating_sub(1) as f64)
            || sr < 0.0
            || sr >= (n_rg.saturating_sub(1) as f64)
        {
            return Complex32::ZERO;
        }
        let a0 = sa as usize;
        let r0 = sr as usize;
        let fa = (sa - a0 as f64) as f32;
        let fr = (sr - r0 as f64) as f32;
        let s00 = s_burst.data[a0 * n_rg + r0];
        let s01 = s_burst.data[a0 * n_rg + r0 + 1];
        let s10 = s_burst.data[(a0 + 1) * n_rg + r0];
        let s11 = s_burst.data[(a0 + 1) * n_rg + r0 + 1];
        let s_re = (1.0 - fa) * ((1.0 - fr) * s00.re + fr * s01.re)
            + fa * ((1.0 - fr) * s10.re + fr * s11.re);
        let s_im = (1.0 - fa) * ((1.0 - fr) * s00.im + fr * s01.im)
            + fa * ((1.0 - fr) * s10.im + fr * s11.im);
        let s_c = Complex32::new(s_re, s_im);
        let prod = m_burst.data[a * n_rg + r] * s_c.conj();
        let phi = init_model.phi_coeffs[1] * af
            + init_model.phi_coeffs[2] * rf
            + init_model.phi_coeffs[3] * rf * rf;
        prod * Complex32::from_polar_unit(-phi)
    };

    for a in (a_start..a_end.saturating_sub(3)).step_by(step_a) {
        for r in (r_start..r_end.saturating_sub(3)).step_by(step_r) {
            let c00 = sample_ifg(a, r);
            let c20 = sample_ifg(a + 2, r);
            let c02 = sample_ifg(a, r + 2);
            let daz = c20 * c00.conj();
            let drg = c02 * c00.conj();
            let wa = (f64::from(c00.norm_sqr()) * f64::from(c20.norm_sqr())).sqrt() + 1e-12;
            let wr = (f64::from(c00.norm_sqr()) * f64::from(c02.norm_sqr())).sqrt() + 1e-12;
            sum_az_re += f64::from(daz.re) / wa;
            sum_az_im += f64::from(daz.im) / wa;
            sum_rg_re += f64::from(drg.re) / wr;
            sum_rg_im += f64::from(drg.im) / wr;
        }
    }

    let dphi_da_res = (0.5 * sum_az_im.atan2(sum_az_re)).clamp(-0.0015, 0.0015);
    let dphi_dr_res = (0.5 * sum_rg_im.atan2(sum_rg_re)).clamp(-0.0005, 0.0005);
    let cand_dphi_da = init_model.phi_coeffs[1] + dphi_da_res;
    let cand_dphi_dr = init_model.phi_coeffs[2] + dphi_dr_res;

    let coh_geo_ramp = eval_patch_coh(
        best_da,
        best_dr,
        init_model.phi_coeffs[1],
        init_model.phi_coeffs[2],
    );
    let coh_emp_ramp = eval_patch_coh(best_da, best_dr, cand_dphi_da, cand_dphi_dr);
    let (final_dphi_da, final_dphi_dr, final_coh) = if coh_emp_ramp > coh_geo_ramp + 0.002 {
        (cand_dphi_da, cand_dphi_dr, coh_emp_ramp)
    } else {
        (
            init_model.phi_coeffs[1],
            init_model.phi_coeffs[2],
            coh_geo_ramp,
        )
    };

    BurstCoregModel {
        da_coeffs: [best_da, init_model.da_coeffs[1], init_model.da_coeffs[2]],
        dr_coeffs: [best_dr, init_model.dr_coeffs[1], init_model.dr_coeffs[2]],
        phi_coeffs: [0.0, final_dphi_da, final_dphi_dr, init_model.phi_coeffs[3]],
        mean_coherence: final_coh,
    }
}

/// CPU-Implementierung von `insar_resample_and_ifg_kernel`: Resamplet den Slave-Burst
/// bilinear auf das Master-Gitter und bildet `I = M * conj(S) * exp(-j * phi_model)`, `|M|^2`, `|S|^2`.
pub fn resample_and_form_ifg_cpu(
    master: &[Complex32],
    slave: &[Complex32],
    n_az: usize,
    n_rg: usize,
    da_coeffs: [f64; 3],
    dr_coeffs: [f64; 3],
    phi_coeffs: [f64; 4],
) -> (Vec<Complex32>, Vec<f32>, Vec<f32>) {
    let n_total = n_az * n_rg;
    let mut ifg = vec![Complex32::ZERO; n_total];
    let mut pm = vec![0.0f32; n_total];
    let mut ps = vec![0.0f32; n_total];

    let mid_az = (n_az as f64) * 0.5;
    let mid_rg = (n_rg as f64) * 0.5;

    for a in 0..n_az {
        let af = a as f64 - mid_az;
        for r in 0..n_rg {
            let rf = r as f64 - mid_rg;
            let da = da_coeffs[0] + da_coeffs[1] * af + da_coeffs[2] * rf;
            let dr = dr_coeffs[0] + dr_coeffs[1] * af + dr_coeffs[2] * rf;
            let sa = a as f64 + da;
            let sr = r as f64 + dr;
            if sa >= 0.0
                && sa < (n_az.saturating_sub(1) as f64)
                && sr >= 0.0
                && sr < (n_rg.saturating_sub(1) as f64)
            {
                let a0 = sa as usize;
                let r0 = sr as usize;
                let fa = (sa - a0 as f64) as f32;
                let fr = (sr - r0 as f64) as f32;

                let s00 = slave[a0 * n_rg + r0];
                let s01 = slave[a0 * n_rg + r0 + 1];
                let s10 = slave[(a0 + 1) * n_rg + r0];
                let s11 = slave[(a0 + 1) * n_rg + r0 + 1];

                let w00 = (1.0 - fa) * (1.0 - fr);
                let w01 = (1.0 - fa) * fr;
                let w10 = fa * (1.0 - fr);
                let w11 = fa * fr;

                let s_interp = Complex32::new(
                    w00 * s00.re + w01 * s01.re + w10 * s10.re + w11 * s11.re,
                    w00 * s00.im + w01 * s01.im + w10 * s10.im + w11 * s11.im,
                );
                let idx = a * n_rg + r;
                let m_val = master[idx];
                pm[idx] = m_val.norm_sqr();
                ps[idx] = s_interp.norm_sqr();

                let prod = m_val * s_interp.conj();
                let phi = phi_coeffs[0]
                    + phi_coeffs[1] * af
                    + phi_coeffs[2] * rf
                    + phi_coeffs[3] * rf * rf;
                let rot = Complex32::new(phi.cos() as f32, (-phi.sin()) as f32);
                ifg[idx] = prod * rot;
            }
        }
    }

    (ifg, pm, ps)
}

/// Koregistrierter Einzel-Burst mit komplexem Interferogramm `ifg`, Master-Leistung `pm` und Slave-Leistung `ps`.
#[derive(Clone, Debug)]
pub struct BurstInterferogram {
    pub burst_index: usize,
    pub n_az_fft: usize,
    pub n_rg: usize,
    pub n_valid_rg: usize,
    pub valid_az_window: (usize, usize),
    pub mid_time_s: f64,
    pub dt_foc_s: f64,
    pub r0_m: f64,
    pub dr_m: f64,
    pub steering: TopsSteering,
    pub coreg: BurstCoregModel,
    pub ifg: Vec<Complex32>,
    pub pm: Vec<f32>,
    pub ps: Vec<f32>,
}

/// Zusammengefügter (debursteter) komplexer InSAR-Sub-Swath.
#[derive(Clone, Debug)]
pub struct DeburstedInsarSwath {
    pub swath_number: u8,
    pub swath_name: String,
    pub n_az: usize,
    pub n_rg: usize,
    pub t_start_s: f64,
    pub dt_az_s: f64,
    pub r0_m: f64,
    pub dr_m: f64,
    pub mean_burst_coh: f64,
    pub mean_da_px: f64,
    pub mean_dr_px: f64,
    pub ifg: Vec<Complex32>,
    pub pm: Vec<f32>,
    pub ps: Vec<f32>,
}

/// Fügt alle `BurstInterferogram`s eines Sub-Swaths mittels **Enhanced Spectral Diversity (ESD)**-Phasenabgleich
/// in den Burst-Überlappungszonen und Cosinus-Cross-Fade nahtlos zu einem kontinuierlichen Sub-Swath zusammen.
pub fn deburst_insar_swath(
    swath_number: u8,
    swath_name: &str,
    bursts: &[BurstInterferogram],
) -> Result<DeburstedInsarSwath, Error> {
    if bursts.is_empty() {
        return Err(Error("Keine Bursts zum InSAR-Debursting vorhanden".into()));
    }

    let b0 = &bursts[0];
    let dt_foc = b0.dt_foc_s;
    let n_rg = bursts.iter().map(|b| b.n_valid_rg).min().unwrap_or(b0.n_rg);

    let nominal_stride = if bursts.len() >= 2 {
        let mut diffs: Vec<f64> = bursts
            .windows(2)
            .map(|w| ((w[1].mid_time_s - w[0].mid_time_s) / dt_foc).abs())
            .filter(|&d| d > 10.0)
            .collect();
        if diffs.is_empty() {
            (b0.valid_az_window.1 - b0.valid_az_window.0) as f64
        } else {
            diffs.sort_by(f64::total_cmp);
            diffs[diffs.len() / 2]
        }
    } else {
        (b0.valid_az_window.1 - b0.valid_az_window.0) as f64
    };
    let overlap_half = (nominal_stride * 0.045).round().clamp(12.0, 48.0) as isize;

    let mut global_ranges: Vec<(isize, isize, usize, usize)> = Vec::with_capacity(bursts.len());
    let mut min_g: isize = isize::MAX;
    let mut max_g: isize = isize::MIN;

    for (bi, b) in bursts.iter().enumerate() {
        let (vaz0, vaz1) = b.valid_az_window;
        let default_half = ((0.5 * nominal_stride).round() as isize + overlap_half)
            .clamp(16, ((vaz1 - vaz0) / 2).max(16) as isize);
        let half_prev = if bi > 0 {
            let s_prev = ((b.mid_time_s - bursts[bi - 1].mid_time_s) / dt_foc).abs();
            if s_prev > 10.0 && s_prev < 2.0 * nominal_stride {
                ((0.5 * s_prev).round() as isize + overlap_half)
                    .clamp(16, (b.n_az_fft / 2) as isize)
            } else {
                default_half
            }
        } else {
            default_half
        };
        let half_next = if bi + 1 < bursts.len() {
            let s_next = ((bursts[bi + 1].mid_time_s - b.mid_time_s) / dt_foc).abs();
            if s_next > 10.0 && s_next < 2.0 * nominal_stride {
                ((0.5 * s_next).round() as isize + overlap_half)
                    .clamp(16, (b.n_az_fft / 2) as isize)
            } else {
                default_half
            }
        } else {
            default_half
        };
        let mid_row = (b.n_az_fft / 2) as isize;
        let az0 = (mid_row - half_prev).max(0) as usize;
        let az1 = (mid_row + half_next).min(b.n_az_fft as isize) as usize;
        let len = az1.saturating_sub(az0).max(1);
        let center_offset = ((b.mid_time_s - b0.mid_time_s) / dt_foc).round() as isize;
        let g0 = center_offset - (mid_row - az0 as isize);
        let g1 = g0 + (len as isize);
        min_g = min_g.min(g0);
        max_g = max_g.max(g1);
        let ramp = (2 * overlap_half as usize).clamp(8, len / 3);
        global_ranges.push((g0, g1, az0, ramp));
    }

    let out_az = (max_g - min_g).max(1) as usize;
    let t_start_s = b0.mid_time_s + (min_g as f64) * dt_foc;

    let mut ifg_acc = vec![Complex32::ZERO; out_az * n_rg];
    let mut pm_acc = vec![0.0f32; out_az * n_rg];
    let mut ps_acc = vec![0.0f32; out_az * n_rg];
    let mut w_acc = vec![0.0f32; out_az];

    // Kumulierter ESD-Phasen-Offset zwischen aufeinanderfolgenden Bursts
    let mut cum_phase_rot = Complex32::new(1.0, 0.0);

    for (idx, b) in bursts.iter().enumerate() {
        let (g0, g1, az0, ramp) = global_ranges[idx];
        let len = (g1 - g0) as usize;
        let out_start = (g0 - min_g) as usize;

        // Wenn dieser Burst mit dem bereits akkumulierten Vor-Burst überlappt, messe die mittlere
        // Phasendifferenz (ESD-Phasenkontinuität) in der Überlappungszone und kompensiere sie!
        if idx > 0 {
            let mut sum_re = 0.0f64;
            let mut sum_im = 0.0f64;
            for m in 0..len {
                let dst_a = out_start + m;
                if dst_a < out_az && w_acc[dst_a] > 0.1 {
                    let src_a = az0 + m;
                    let prev_row = &ifg_acc[dst_a * n_rg..(dst_a + 1) * n_rg];
                    let curr_row = &b.ifg[src_a * b.n_rg..src_a * b.n_rg + n_rg];
                    let prev_w = 1.0 / w_acc[dst_a];
                    for r in (0..n_rg).step_by(4) {
                        let p_prev = prev_row[r] * prev_w;
                        let p_curr = curr_row[r] * cum_phase_rot;
                        // Wir wollen p_curr * delta_rot ~ p_prev => delta_rot = arg(sum(p_prev * conj(p_curr)))
                        let d = p_prev * p_curr.conj();
                        sum_re += f64::from(d.re);
                        sum_im += f64::from(d.im);
                    }
                }
            }
            if sum_re.hypot(sum_im) > 1e-12 {
                let dphi = sum_im.atan2(sum_re);
                cum_phase_rot = cum_phase_rot * Complex32::from_polar_unit(dphi);
            }
        }

        for m in 0..len {
            let dst_a = out_start + m;
            if dst_a >= out_az {
                continue;
            }
            let w = if idx > 0 && m < ramp {
                let x = (m as f32 + 0.5) / (ramp as f32);
                0.5 * (1.0 - (std::f32::consts::PI * x).cos())
            } else if idx + 1 < bursts.len() && m + ramp >= len {
                let rem = len - 1 - m;
                let x = (rem as f32 + 0.5) / (ramp as f32);
                0.5 * (1.0 - (std::f32::consts::PI * x).cos())
            } else {
                1.0
            };

            let src_a = az0 + m;
            let src_ifg = &b.ifg[src_a * b.n_rg..src_a * b.n_rg + n_rg];
            let src_pm = &b.pm[src_a * b.n_rg..src_a * b.n_rg + n_rg];
            let src_ps = &b.ps[src_a * b.n_rg..src_a * b.n_rg + n_rg];

            let dst_ifg = &mut ifg_acc[dst_a * n_rg..(dst_a + 1) * n_rg];
            let dst_pm = &mut pm_acc[dst_a * n_rg..(dst_a + 1) * n_rg];
            let dst_ps = &mut ps_acc[dst_a * n_rg..(dst_a + 1) * n_rg];

            let rot_w = cum_phase_rot * w;
            for r in 0..n_rg {
                dst_ifg[r] += src_ifg[r] * rot_w;
                dst_pm[r] += src_pm[r] * w;
                dst_ps[r] += src_ps[r] * w;
            }
            w_acc[dst_a] += w;
        }
    }

    for (a, &w) in w_acc.iter().enumerate().take(out_az) {
        if w > 1e-6 {
            let inv = 1.0 / w;
            for r in 0..n_rg {
                let idx = a * n_rg + r;
                ifg_acc[idx] = ifg_acc[idx] * inv;
                pm_acc[idx] *= inv;
                ps_acc[idx] *= inv;
            }
        }
    }

    let n_b = bursts.len() as f64;
    let mean_burst_coh = bursts.iter().map(|b| b.coreg.mean_coherence).sum::<f64>() / n_b;
    let mean_da_px = bursts.iter().map(|b| b.coreg.da_coeffs[0]).sum::<f64>() / n_b;
    let mean_dr_px = bursts.iter().map(|b| b.coreg.dr_coeffs[0]).sum::<f64>() / n_b;

    Ok(DeburstedInsarSwath {
        swath_number,
        swath_name: swath_name.to_string(),
        n_az: out_az,
        n_rg,
        t_start_s,
        dt_az_s: dt_foc,
        r0_m: b0.r0_m,
        dr_m: b0.dr_m,
        mean_burst_coh,
        mean_da_px,
        mean_dr_px,
        ifg: ifg_acc,
        pm: pm_acc,
        ps: ps_acc,
    })
}

/// Multilooked InSAR-Mosaik (oder Einzel-Swath) mit komplexem Interferogramm, Kohärenz und Leistungen.
#[derive(Clone, Debug)]
pub struct MultilookedInsar {
    pub out_az: usize,
    pub out_rg: usize,
    pub r_min_m: f64,
    pub r_max_m: f64,
    pub ifg_ml: Vec<Complex32>,
    pub pm_ml: Vec<f32>,
    pub ps_ml: Vec<f32>,
    pub coherence: Vec<f32>,
}

/// Berechnet aus einem Multilook-Interferogramm `ifg_ml` und den Leistungen `pm_ml, ps_ml`
/// die lokale Kohärenzkarte $\gamma(a, r) \in [0, 1]$ über ein gleitendes $(2 w_{\text{az}}+1)\times(2 w_{\text{rg}}+1)$-Fenster
/// mit lokaler 2D-Phasengradienten-Kompensation (damit topographische Fringes nicht als Dekorrelation
/// fehlinterpretiert werden) und kombinierter Intra-/Inter-Zellen-Kohärenz.
pub fn compute_coherence_map(
    ifg_ml: &[Complex32],
    pm_ml: &[f32],
    ps_ml: &[f32],
    out_az: usize,
    out_rg: usize,
    half_win_az: usize,
    half_win_rg: usize,
) -> Vec<f32> {
    let mut coh = vec![0.0f32; out_az * out_rg];
    for a in 0..out_az {
        let a0 = a.saturating_sub(half_win_az);
        let a1 = (a + half_win_az + 1).min(out_az);
        for r in 0..out_rg {
            let r0 = r.saturating_sub(half_win_rg);
            let r1 = (r + half_win_rg + 1).min(out_rg);

            // 1. Schätze den lokalen 2D-Phasengradienten (wa, wr) im Fenster mittels 1-Lag-Autokorrelation
            let mut ra_re = 0.0f64;
            let mut ra_im = 0.0f64;
            let mut rr_re = 0.0f64;
            let mut rr_im = 0.0f64;
            for ia in a0..a1 {
                let row_off = ia * out_rg;
                for ir in r0..r1 {
                    let c00 = ifg_ml[row_off + ir];
                    if ia + 1 < a1 {
                        let c10 = ifg_ml[(ia + 1) * out_rg + ir];
                        let d = c10 * c00.conj();
                        ra_re += f64::from(d.re);
                        ra_im += f64::from(d.im);
                    }
                    if ir + 1 < r1 {
                        let c01 = ifg_ml[row_off + ir + 1];
                        let d = c01 * c00.conj();
                        rr_re += f64::from(d.re);
                        rr_im += f64::from(d.im);
                    }
                }
            }
            let wa = if ra_re.hypot(ra_im) > 1e-20 {
                ra_im.atan2(ra_re)
            } else {
                0.0
            };
            let wr = if rr_re.hypot(rr_im) > 1e-20 {
                rr_im.atan2(rr_re)
            } else {
                0.0
            };

            // 2. Phasengradienten-kompensierte kohärente Summe und Intra-Zellen-Magnitudensumme
            let mut sum_re = 0.0f64;
            let mut sum_im = 0.0f64;
            let mut sum_mag = 0.0f64;
            let mut sum_m = 0.0f64;
            let mut sum_s = 0.0f64;
            for ia in a0..a1 {
                let row_off = ia * out_rg;
                let da = (ia as isize - a as isize) as f64;
                for ir in r0..r1 {
                    let dr = (ir as isize - r as isize) as f64;
                    let c = ifg_ml[row_off + ir];
                    let cre = f64::from(c.re);
                    let cim = f64::from(c.im);
                    let phi = wa * da + wr * dr;
                    let (sin_p, cos_p) = phi.sin_cos();
                    sum_re += cre * cos_p + cim * sin_p;
                    sum_im += cim * cos_p - cre * sin_p;
                    sum_mag += cre.hypot(cim);
                    sum_m += f64::from(pm_ml[row_off + ir]);
                    sum_s += f64::from(ps_ml[row_off + ir]);
                }
            }
            let den = (sum_m * sum_s).sqrt();
            coh[a * out_rg + r] = if den > 1e-12 {
                let s_deramp = sum_re.hypot(sum_im);
                // Geometrisches Mittel aus Intra-Zellen-Kohärenz (sum_mag / den) und
                // phasenkompensierter Gesamt-Fensterkohärenz (s_deramp / den), damit bereits
                // multilookte Zellen nicht quadratisch gedämpft werden, während unkorreliertes
                // Meeresrauschen räumlich unterdrückt wird.
                ((s_deramp * sum_mag).sqrt() / den).clamp(0.0, 1.0) as f32
            } else {
                0.0
            };
        }
    }
    coh
}

/// Mosaikiert ein oder mehrere `DeburstedInsarSwath`s (z. B. IW1..IW3) auf ein gemeinsames
/// Slant-Range-/Azimut-Multilook-Gitter, gleicht die Phase in den Sub-Swath-Überlappungszonen ab
/// und berechnet die Kohärenzkarte.
pub fn mosaic_insar_swaths(
    swaths: &[DeburstedInsarSwath],
    look_az: usize,
    look_rg: usize,
    coh_half_win: usize,
) -> Result<MultilookedInsar, Error> {
    if swaths.is_empty() {
        return Err(Error("Keine InSAR-Swaths für Mosaikierung".into()));
    }

    let laz = look_az.max(1);
    let lrg = look_rg.max(1);

    // Hilfsfunktion: fasst 4 Range-Sub-Looks B[0..4] innerhalb einer Multilook-Zelle
    // phasenkompensiert zur Zellenmitte zusammen, damit steile Range-Fringes innerhalb von
    // `lrg` (z. B. 24) Spalten nicht destruktiv interferieren.
    let combine_range_subbins = |bins: &[Complex32; 4]| -> Complex32 {
        let r01 = bins[1] * bins[0].conj();
        let r12 = bins[2] * bins[1].conj();
        let r23 = bins[3] * bins[2].conj();
        let r_step = r01 + r12 + r23;
        if r_step.norm_sqr() <= 1e-24 {
            return bins[0] + bins[1] + bins[2] + bins[3];
        }
        let dphi = f64::from(r_step.im).atan2(f64::from(r_step.re));
        let mut acc = Complex32::ZERO;
        for (k, &b) in bins.iter().enumerate() {
            let rot = Complex32::from_polar_unit(-((k as f64) - 1.5) * dphi);
            acc += b * rot;
        }
        acc
    };

    if swaths.len() == 1 {
        let s = &swaths[0];
        let out_az = (s.n_az / laz).max(1);
        let out_rg = (s.n_rg / lrg).max(1);
        let inv = 1.0 / ((laz * lrg) as f32);
        let mut ifg_ml = vec![Complex32::ZERO; out_az * out_rg];
        let mut pm_ml = vec![0.0f32; out_az * out_rg];
        let mut ps_ml = vec![0.0f32; out_az * out_rg];

        for oa in 0..out_az {
            for or in 0..out_rg {
                let mut sub_bins = [Complex32::ZERO; 4];
                let mut m_acc = 0.0f32;
                let mut s_acc = 0.0f32;
                for da in 0..laz {
                    let row = (oa * laz + da) * s.n_rg;
                    let base = row + or * lrg;
                    for dr in 0..lrg {
                        let k = ((dr * 4) / lrg).min(3);
                        sub_bins[k] += s.ifg[base + dr];
                        m_acc += s.pm[base + dr];
                        s_acc += s.ps[base + dr];
                    }
                }
                let idx = oa * out_rg + or;
                ifg_ml[idx] = combine_range_subbins(&sub_bins) * inv;
                pm_ml[idx] = m_acc * inv;
                ps_ml[idx] = s_acc * inv;
            }
        }
        let coherence = compute_coherence_map(
            &ifg_ml,
            &pm_ml,
            &ps_ml,
            out_az,
            out_rg,
            coh_half_win,
            coh_half_win,
        );
        return Ok(MultilookedInsar {
            out_az,
            out_rg,
            r_min_m: s.r0_m,
            r_max_m: s.r0_m + (s.n_rg as f64) * s.dr_m,
            ifg_ml,
            pm_ml,
            ps_ml,
            coherence,
        });
    }

    let t_min = swaths
        .iter()
        .map(|s| s.t_start_s)
        .fold(f64::INFINITY, f64::min);
    let t_max = swaths
        .iter()
        .map(|s| s.t_start_s + (s.n_az as f64) * s.dt_az_s)
        .fold(f64::NEG_INFINITY, f64::max);
    let r_min = swaths.iter().map(|s| s.r0_m).fold(f64::INFINITY, f64::min);
    let r_max = swaths
        .iter()
        .map(|s| s.r0_m + (s.n_rg as f64) * s.dr_m)
        .fold(f64::NEG_INFINITY, f64::max);

    let dt_base = swaths
        .iter()
        .map(|s| s.dt_az_s)
        .fold(f64::INFINITY, f64::min);
    let dr_base = swaths.iter().map(|s| s.dr_m).fold(f64::INFINITY, f64::min);

    let dt_out = dt_base * (laz as f64);
    let dr_out = dr_base * (lrg as f64);

    let out_az = (((t_max - t_min) / dt_out).ceil() as usize).max(1);
    let out_rg = (((r_max - r_min) / dr_out).ceil() as usize).max(1);

    let mut ifg_acc = vec![Complex32::ZERO; out_az * out_rg];
    let mut pm_acc = vec![0.0f32; out_az * out_rg];
    let mut ps_acc = vec![0.0f32; out_az * out_rg];
    let mut w_acc = vec![0.0f32; out_az * out_rg];

    let mut cum_swath_rot = Complex32::new(1.0, 0.0);

    for (s_idx, s) in swaths.iter().enumerate() {
        let s_r_start = s.r0_m;
        let s_r_end = s.r0_m + (s.n_rg as f64) * s.dr_m;

        // Bestimme physische Überlappungs-Nahtstellen zu den Nachbar-Sub-Swaths
        let prev_overlap = if s_idx > 0 {
            let p = &swaths[s_idx - 1];
            let p_end = p.r0_m + (p.n_rg as f64) * p.dr_m;
            if p_end > s_r_start {
                let seam = 0.5 * (s_r_start + p_end);
                let half_blend = (0.15 * (p_end - s_r_start)).clamp(dr_out * 2.0, 1200.0);
                Some((seam, half_blend))
            } else {
                None
            }
        } else {
            None
        };

        let next_overlap = if s_idx + 1 < swaths.len() {
            let n = &swaths[s_idx + 1];
            let n_start = n.r0_m;
            if s_r_end > n_start {
                let seam = 0.5 * (n_start + s_r_end);
                let half_blend = (0.15 * (s_r_end - n_start)).clamp(dr_out * 2.0, 1200.0);
                Some((seam, half_blend))
            } else {
                None
            }
        } else {
            None
        };

        // 1. Multilooke zunächst diesen Sub-Swath für sich mit lokaler 4-Sub-Bin-Range-Phasenausrichtung
        let mut sw_sub_bins = vec![[Complex32::ZERO; 4]; out_az * out_rg];
        let mut sw_pm = vec![0.0f32; out_az * out_rg];
        let mut sw_ps = vec![0.0f32; out_az * out_rg];
        let mut sw_cnt = vec![0u32; out_az * out_rg];

        for a in 0..s.n_az {
            let t = s.t_start_s + (a as f64) * s.dt_az_s;
            let oa = ((t - t_min) / dt_out).floor() as isize;
            if oa < 0 || (oa as usize) >= out_az {
                continue;
            }
            let oa = oa as usize;
            let row_ifg = &s.ifg[a * s.n_rg..(a + 1) * s.n_rg];
            let row_pm = &s.pm[a * s.n_rg..(a + 1) * s.n_rg];
            let row_ps = &s.ps[a * s.n_rg..(a + 1) * s.n_rg];

            for r in 0..s.n_rg {
                let pm = row_pm[r];
                let ps = row_ps[r];
                if pm <= 0.0 && ps <= 0.0 {
                    continue;
                }
                let r_m = s.r0_m + (r as f64) * s.dr_m;
                let or_f = (r_m - r_min) / dr_out;
                if or_f < 0.0 {
                    continue;
                }
                let or = or_f.floor() as usize;
                if or >= out_rg {
                    continue;
                }
                let frac_r = or_f - (or as f64);
                let k = ((frac_r * 4.0).floor() as usize).min(3);
                let idx = oa * out_rg + or;
                sw_sub_bins[idx][k] += row_ifg[r];
                sw_pm[idx] += pm;
                sw_ps[idx] += ps;
                sw_cnt[idx] += 1;
            }
        }

        let mut sw_ifg = vec![Complex32::ZERO; out_az * out_rg];
        for idx in 0..out_az * out_rg {
            if sw_cnt[idx] > 0 {
                let inv = 1.0 / (sw_cnt[idx] as f32);
                sw_ifg[idx] = combine_range_subbins(&sw_sub_bins[idx]) * inv;
                sw_pm[idx] *= inv;
                sw_ps[idx] *= inv;
            }
        }

        // 2. Falls Überlappung zum vorherigen Sub-Swath existiert, harmonisiere den Phasenoffset an der Naht
        if let Some((seam_m, half_blend_m)) = prev_overlap {
            let mut sum_re = 0.0f64;
            let mut sum_im = 0.0f64;
            let or_min =
                (((seam_m - half_blend_m - r_min) / dr_out).floor().max(0.0) as usize).min(out_rg);
            let or_max =
                (((seam_m + half_blend_m - r_min) / dr_out).ceil().max(0.0) as usize).min(out_rg);
            for oa in 0..out_az {
                for or in or_min..or_max {
                    let idx = oa * out_rg + or;
                    if w_acc[idx] > 1e-3 && sw_cnt[idx] > 0 {
                        let prev_c = ifg_acc[idx] * (1.0 / w_acc[idx]);
                        let curr_c = sw_ifg[idx] * cum_swath_rot;
                        let d = prev_c * curr_c.conj();
                        sum_re += f64::from(d.re);
                        sum_im += f64::from(d.im);
                    }
                }
            }
            if sum_re.hypot(sum_im) > 1e-12 {
                let dphi = sum_im.atan2(sum_re);
                cum_swath_rot = cum_swath_rot * Complex32::from_polar_unit(dphi);
            }
        }

        // 3. Akkumuliere den Sub-Swath mit weichem Kosinus-Blending um die physische Überlappungsmitte
        for oa in 0..out_az {
            for or in 0..out_rg {
                let idx = oa * out_rg + or;
                if sw_cnt[idx] == 0 {
                    continue;
                }
                let r_cell_m = r_min + (or as f64 + 0.5) * dr_out;
                let mut w = 1.0f64;
                if let Some((seam_m, half_blend_m)) = prev_overlap {
                    let u = ((r_cell_m - (seam_m - half_blend_m)) / (2.0 * half_blend_m))
                        .clamp(0.0, 1.0);
                    w *= 0.5 * (1.0 - (std::f64::consts::PI * u).cos());
                }
                if let Some((seam_m, half_blend_m)) = next_overlap {
                    let u = (((seam_m + half_blend_m) - r_cell_m) / (2.0 * half_blend_m))
                        .clamp(0.0, 1.0);
                    w *= 0.5 * (1.0 - (std::f64::consts::PI * u).cos());
                }
                let wf = w as f32;
                if wf <= 1e-5 {
                    continue;
                }
                ifg_acc[idx] += sw_ifg[idx] * (cum_swath_rot * wf);
                pm_acc[idx] += sw_pm[idx] * wf;
                ps_acc[idx] += sw_ps[idx] * wf;
                w_acc[idx] += wf;
            }
        }
    }

    for i in 0..out_az * out_rg {
        if w_acc[i] > 1e-6 {
            let inv = 1.0 / w_acc[i];
            ifg_acc[i] = ifg_acc[i] * inv;
            pm_acc[i] *= inv;
            ps_acc[i] *= inv;
        }
    }

    let coherence = compute_coherence_map(
        &ifg_acc,
        &pm_acc,
        &ps_acc,
        out_az,
        out_rg,
        coh_half_win,
        coh_half_win,
    );

    Ok(MultilookedInsar {
        out_az,
        out_rg,
        r_min_m: r_min,
        r_max_m: r_max,
        ifg_ml: ifg_acc,
        pm_ml: pm_acc,
        ps_ml: ps_acc,
        coherence,
    })
}

/// Adaptives **Goldstein-Werner 2D-FFT-Spektralfilter** zur Rauschunterdrückung auf dem
/// komplexen Interferogramm bei gleichzeitiger Erhaltung steiler Phasengradienten (Fringes).
///
/// Arbeitet mit überlappenden `32 × 32`-Patches (Schrittweite 16 Pixel, 2D-Hann-Gewichtung)
/// und wendet im 2D-Frequenzraum die Potenzgewichtung $H(u, v) = S(u, v)^\alpha$ an.
pub fn goldstein_filter_interferogram(
    ifg: &[Complex32],
    n_az: usize,
    n_rg: usize,
    alpha: f32,
) -> Vec<Complex32> {
    let win = 32usize;
    let step = 16usize;
    if n_az < 4 || n_rg < 4 || alpha <= 0.0 {
        return ifg.to_vec();
    }

    let mut planner = FftPlanner::<f32>::new();
    let fft_win_fwd = planner.plan_fft_forward(win);
    let fft_win_inv = planner.plan_fft_inverse(win);

    // 1D-Hann-Fenster
    let hann: Vec<f32> = (0..win)
        .map(|i| {
            let x = (i as f32 + 0.5) / (win as f32);
            0.5 * (1.0 - (2.0 * std::f32::consts::PI * x).cos())
        })
        .collect();

    let mut acc = vec![Complex32::ZERO; n_az * n_rg];
    let mut w_sum = vec![0.0f32; n_az * n_rg];

    let mut patch = vec![Nc32::new(0.0, 0.0); win * win];
    let mut col_buf = vec![Nc32::new(0.0, 0.0); win];
    let mut amp = vec![0.0f32; win * win];
    let mut smooth_amp = vec![0.0f32; win * win];

    let mut a_starts: Vec<usize> = (0..n_az).step_by(step).collect();
    if n_az > win && *a_starts.last().unwrap_or(&0) + win < n_az {
        a_starts.push(n_az - win);
    }
    let mut r_starts: Vec<usize> = (0..n_rg).step_by(step).collect();
    if n_rg > win && *r_starts.last().unwrap_or(&0) + win < n_rg {
        r_starts.push(n_rg - win);
    }

    let inv_win2 = 1.0 / ((win * win) as f32);

    for &a0 in &a_starts {
        for &r0 in &r_starts {
            // 1. Extrahiere normierte komplexe Phase z = I / (|I| + eps), gewichtet mit 2D-Hann
            for ia in 0..win {
                let ga = (a0 + ia).min(n_az - 1);
                let wa = hann[ia];
                for ir in 0..win {
                    let gr = (r0 + ir).min(n_rg - 1);
                    let wr = hann[ir];
                    let c = ifg[ga * n_rg + gr];
                    let mag = c.norm();
                    let w = wa * wr;
                    if mag > 1e-12 {
                        patch[ia * win + ir] = Nc32::new((c.re / mag) * w, (c.im / mag) * w);
                    } else {
                        patch[ia * win + ir] = Nc32::new(0.0, 0.0);
                    }
                }
            }

            // 2. 2D-Vorwärts-FFT (Zeilen + Spalten)
            for row in patch.chunks_mut(win) {
                fft_win_fwd.process(row);
            }
            for c in 0..win {
                for r in 0..win {
                    col_buf[r] = patch[r * win + c];
                }
                fft_win_fwd.process(&mut col_buf);
                for r in 0..win {
                    patch[r * win + c] = col_buf[r];
                }
            }

            // 3. 3×3-geglättetes Amplitudenspektrum und Potenz-Filterung |Z|^alpha
            for (dst, src) in amp.iter_mut().zip(patch.iter()) {
                *dst = src.norm();
            }
            let mut max_s = 1e-12f32;
            for u in 0..win {
                for v in 0..win {
                    let mut s = 0.0f32;
                    for du in [win - 1, 0, 1] {
                        let uu = (u + du) % win;
                        for dv in [win - 1, 0, 1] {
                            let vv = (v + dv) % win;
                            s += amp[uu * win + vv];
                        }
                    }
                    s *= 1.0 / 9.0;
                    smooth_amp[u * win + v] = s;
                    if s > max_s {
                        max_s = s;
                    }
                }
            }

            for (z, &s) in patch.iter_mut().zip(smooth_amp.iter()) {
                let g = (s / max_s).powf(alpha);
                *z *= g * inv_win2;
            }

            // 4. 2D-Rückwärts-FFT
            for c in 0..win {
                for r in 0..win {
                    col_buf[r] = patch[r * win + c];
                }
                fft_win_inv.process(&mut col_buf);
                for r in 0..win {
                    patch[r * win + c] = col_buf[r];
                }
            }
            for row in patch.chunks_mut(win) {
                fft_win_inv.process(row);
            }

            // 5. Overlap-Add in das Ziel-Interferogramm
            for ia in 0..win {
                let ga = a0 + ia;
                if ga >= n_az {
                    continue;
                }
                let wa = hann[ia];
                for ir in 0..win {
                    let gr = r0 + ir;
                    if gr >= n_rg {
                        continue;
                    }
                    let wr = hann[ir];
                    let w = wa * wr;
                    let z = patch[ia * win + ir];
                    let orig_mag = ifg[ga * n_rg + gr].norm();
                    let idx = ga * n_rg + gr;
                    acc[idx] += Complex32::new(z.re, z.im) * (w * orig_mag);
                    w_sum[idx] += w * w;
                }
            }
        }
    }

    for i in 0..n_az * n_rg {
        if w_sum[i] > 1e-6 {
            acc[i] = acc[i] * (1.0 / w_sum[i]);
        } else {
            acc[i] = ifg[i];
        }
    }

    acc
}

#[inline]
fn wrap_rad(d: f32) -> f32 {
    let mut x = (d + std::f32::consts::PI) % (2.0 * std::f32::consts::PI);
    if x < 0.0 {
        x += 2.0 * std::f32::consts::PI;
    }
    x - std::f32::consts::PI
}

/// Kohärenzgewichtetes **2D-Phase-Unwrapping**:
/// 1. **Quality-Guided Flood-Fill (Dijkstra/Max-Heap entlang der höchsten Kohärenz $\gamma$)**
///    ausgehend vom kohärentesten zentralen Landpixel.
/// 2. **Kohärenzgewichtete Gauß-Seidel-/Poisson-Nachglättung** der entfalteten Phase in
///    dekorrelierten Regionen, sodass keine isolierten Zweig-Artefakte verbleiben.
pub fn unwrap_phase_2d(ifg: &[Complex32], coherence: &[f32], n_az: usize, n_rg: usize) -> Vec<f32> {
    let n_total = n_az * n_rg;
    if n_total == 0 {
        return Vec::new();
    }

    let wrapped: Vec<f32> = ifg.iter().map(|c| c.arg()).collect();
    let mut unwrapped = vec![0.0f32; n_total];
    let mut visited = vec![false; n_total];

    // Suche den besten Startpunkt im zentralen Bildbereich
    let a_min = n_az / 6;
    let a_max = (n_az * 5 / 6).max(a_min + 1);
    let r_min = n_rg / 6;
    let r_max = (n_rg * 5 / 6).max(r_min + 1);
    let mut seed_idx = (n_az / 2) * n_rg + (n_rg / 2);
    let mut best_coh = -1.0f32;
    for a in a_min..a_max {
        for r in r_min..r_max {
            let idx = a * n_rg + r;
            if coherence[idx] > best_coh {
                best_coh = coherence[idx];
                seed_idx = idx;
            }
        }
    }

    // BinaryHeap mit quantisierter Qualitätsmetrik (q_int, neighbor_idx, parent_idx)
    let mut heap: std::collections::BinaryHeap<(u32, usize, usize)> =
        std::collections::BinaryHeap::with_capacity(n_total.min(65536));

    visited[seed_idx] = true;
    unwrapped[seed_idx] = wrapped[seed_idx];

    let push_neighbors =
        |p_idx: usize,
         visited: &[bool],
         heap: &mut std::collections::BinaryHeap<(u32, usize, usize)>| {
            let a = p_idx / n_rg;
            let r = p_idx % n_rg;
            let cp = coherence[p_idx];
            if a > 0 {
                let nb = (a - 1) * n_rg + r;
                if !visited[nb] {
                    let q = ((cp.min(coherence[nb])).clamp(0.0, 1.0) * 1_000_000.0) as u32;
                    heap.push((q, nb, p_idx));
                }
            }
            if a + 1 < n_az {
                let nb = (a + 1) * n_rg + r;
                if !visited[nb] {
                    let q = ((cp.min(coherence[nb])).clamp(0.0, 1.0) * 1_000_000.0) as u32;
                    heap.push((q, nb, p_idx));
                }
            }
            if r > 0 {
                let nb = a * n_rg + (r - 1);
                if !visited[nb] {
                    let q = ((cp.min(coherence[nb])).clamp(0.0, 1.0) * 1_000_000.0) as u32;
                    heap.push((q, nb, p_idx));
                }
            }
            if r + 1 < n_rg {
                let nb = a * n_rg + (r + 1);
                if !visited[nb] {
                    let q = ((cp.min(coherence[nb])).clamp(0.0, 1.0) * 1_000_000.0) as u32;
                    heap.push((q, nb, p_idx));
                }
            }
        };

    push_neighbors(seed_idx, &visited, &mut heap);

    while let Some((_, idx, parent)) = heap.pop() {
        if visited[idx] {
            continue;
        }
        visited[idx] = true;
        // Verwende den kohärenzgewichteten Mittelwert aller bereits besuchten 4er-Nachbarn
        let a = idx / n_rg;
        let r = idx % n_rg;
        let mut w_sum = 0.0f32;
        let mut pred_sum = 0.0f32;
        let check_nb = |nb: usize, w_sum: &mut f32, pred_sum: &mut f32| {
            if visited[nb] {
                let w = coherence[nb].powi(2).max(1e-4);
                let step = wrap_rad(wrapped[idx] - wrapped[nb]);
                *pred_sum += w * (unwrapped[nb] + step);
                *w_sum += w;
            }
        };
        if a > 0 {
            check_nb((a - 1) * n_rg + r, &mut w_sum, &mut pred_sum);
        }
        if a + 1 < n_az {
            check_nb((a + 1) * n_rg + r, &mut w_sum, &mut pred_sum);
        }
        if r > 0 {
            check_nb(a * n_rg + r - 1, &mut w_sum, &mut pred_sum);
        }
        if r + 1 < n_rg {
            check_nb(a * n_rg + r + 1, &mut w_sum, &mut pred_sum);
        }

        if w_sum > 0.0 {
            let avg_pred = pred_sum / w_sum;
            // Runde auf das nächste kongruente Vielfache von 2*pi zu wrapped[idx]
            let two_pi = 2.0 * std::f32::consts::PI;
            let k = ((avg_pred - wrapped[idx]) / two_pi).round();
            unwrapped[idx] = wrapped[idx] + k * two_pi;
        } else {
            let step = wrap_rad(wrapped[idx] - wrapped[parent]);
            unwrapped[idx] = unwrapped[parent] + step;
        }

        push_neighbors(idx, &visited, &mut heap);
    }

    // Zentriere die entfaltete Phase um den kohärenzgewichteten Mittelwert = 0
    let mut mean_num = 0.0f64;
    let mut mean_den = 0.0f64;
    for (&u, &c) in unwrapped.iter().zip(coherence.iter()) {
        let w = f64::from(c * c);
        mean_num += f64::from(u) * w;
        mean_den += w;
    }
    let offset = if mean_den > 1e-12 {
        (mean_num / mean_den) as f32
    } else {
        0.0
    };
    for u in &mut unwrapped {
        *u -= offset;
    }

    unwrapped
}

/// Kohärenzgewichtetes **2D-Poisson-Phase-Unwrapping mittels präkonditioniertem Conjugate Gradient (CG)**:
/// Löst das gewichtete 2D-Normalgleichungssystem $-\nabla \cdot (w \nabla \phi) = -\nabla \cdot (w \mathcal{W}(\nabla \psi))$
/// ausgehend von der Quality-Guided-Initialisierung (`unwrap_phase_2d`).
pub fn unwrap_phase_2d_poisson(
    ifg: &[Complex32],
    coherence: &[f32],
    n_az: usize,
    n_rg: usize,
) -> Vec<f32> {
    let n_total = n_az * n_rg;
    if n_total == 0 {
        return Vec::new();
    }

    let mut phi = unwrap_phase_2d(ifg, coherence, n_az, n_rg);
    if n_az < 2 || n_rg < 2 {
        return phi;
    }

    let wrapped: Vec<f32> = ifg.iter().map(|c| c.arg()).collect();
    let mut b = vec![0.0f32; n_total];
    let mut diag = vec![1e-6f32; n_total];

    for a in 0..n_az {
        for r in 0..n_rg {
            let idx = a * n_rg + r;
            let c0 = coherence[idx].clamp(0.0, 1.0);
            if r + 1 < n_rg {
                let nb = idx + 1;
                let w = (c0.min(coherence[nb].clamp(0.0, 1.0))).powi(2).max(1e-4);
                let d = wrap_rad(wrapped[nb] - wrapped[idx]);
                b[idx] -= w * d;
                b[nb] += w * d;
                diag[idx] += w;
                diag[nb] += w;
            }
            if a + 1 < n_az {
                let nb = idx + n_rg;
                let w = (c0.min(coherence[nb].clamp(0.0, 1.0))).powi(2).max(1e-4);
                let d = wrap_rad(wrapped[nb] - wrapped[idx]);
                b[idx] -= w * d;
                b[nb] += w * d;
                diag[idx] += w;
                diag[nb] += w;
            }
        }
    }

    let apply_a = |x: &[f32], out: &mut [f32]| {
        out.fill(0.0);
        for a in 0..n_az {
            for r in 0..n_rg {
                let idx = a * n_rg + r;
                let c0 = coherence[idx].clamp(0.0, 1.0);
                if r + 1 < n_rg {
                    let nb = idx + 1;
                    let w = (c0.min(coherence[nb].clamp(0.0, 1.0))).powi(2).max(1e-4);
                    let diff = w * (x[idx] - x[nb]);
                    out[idx] += diff;
                    out[nb] -= diff;
                }
                if a + 1 < n_az {
                    let nb = idx + n_rg;
                    let w = (c0.min(coherence[nb].clamp(0.0, 1.0))).powi(2).max(1e-4);
                    let diff = w * (x[idx] - x[nb]);
                    out[idx] += diff;
                    out[nb] -= diff;
                }
            }
        }
    };

    let mut ax = vec![0.0f32; n_total];
    apply_a(&phi, &mut ax);
    let mut res: Vec<f32> = b
        .iter()
        .zip(ax.iter())
        .map(|(&bi, &axi)| bi - axi)
        .collect();
    let mut z: Vec<f32> = res
        .iter()
        .zip(diag.iter())
        .map(|(&ri, &di)| ri / di)
        .collect();
    let mut p = z.clone();
    let mut rz_old: f64 = res
        .iter()
        .zip(z.iter())
        .map(|(&ri, &zi)| f64::from(ri) * f64::from(zi))
        .sum();

    for _ in 0..64 {
        if rz_old < 1e-12 {
            break;
        }
        apply_a(&p, &mut ax);
        let p_ap: f64 = p
            .iter()
            .zip(ax.iter())
            .map(|(&pi, &api)| f64::from(pi) * f64::from(api))
            .sum();
        if p_ap <= 1e-20 {
            break;
        }
        let alpha = (rz_old / p_ap) as f32;
        for i in 0..n_total {
            phi[i] += alpha * p[i];
            res[i] -= alpha * ax[i];
            z[i] = res[i] / diag[i];
        }
        let rz_new: f64 = res
            .iter()
            .zip(z.iter())
            .map(|(&ri, &zi)| f64::from(ri) * f64::from(zi))
            .sum();
        if rz_new < 1e-12 {
            break;
        }
        let beta = (rz_new / rz_old) as f32;
        for i in 0..n_total {
            p[i] = z[i] + beta * p[i];
        }
        rz_old = rz_new;
    }

    let mut mean_num = 0.0f64;
    let mut mean_den = 0.0f64;
    for (&u, &c) in phi.iter().zip(coherence.iter()) {
        let w = f64::from(c * c);
        mean_num += f64::from(u) * w;
        mean_den += w;
    }
    let offset = if mean_den > 1e-12 {
        (mean_num / mean_den) as f32
    } else {
        0.0
    };
    for u in &mut phi {
        *u -= offset;
    }

    phi
}

/// Konvertiert einen HSV-Farbwert (`h` in `[0, 1)`, `s` in `[0, 1]`, `v` in `[0, 1]`) nach RGB `[u8; 3]`.
#[inline]
fn hsv_to_rgb(h: f32, s: f32, v: f32) -> [u8; 3] {
    let h6 = (h.rem_euclid(1.0)) * 6.0;
    let i = h6.floor() as i32;
    let f = h6 - (i as f32);
    let p = v * (1.0 - s);
    let q = v * (1.0 - s * f);
    let t = v * (1.0 - s * (1.0 - f));
    let (r, g, b) = match i.rem_euclid(6) {
        0 => (v, t, p),
        1 => (q, v, p),
        2 => (p, v, t),
        3 => (p, q, v),
        4 => (t, p, v),
        _ => (v, p, q),
    };
    [
        (r.clamp(0.0, 1.0) * 255.0).round() as u8,
        (g.clamp(0.0, 1.0) * 255.0).round() as u8,
        (b.clamp(0.0, 1.0) * 255.0).round() as u8,
    ]
}

/// Viridis-ähnliche Perceptually-Uniform-Colormap für Kohärenz $\gamma \in [0, 1]$.
#[inline]
fn viridis_rgb(t: f32) -> [u8; 3] {
    let x = t.clamp(0.0, 1.0);
    // Stückweise kubische/lineare Stützstellen für Viridis
    let stops: [[f32; 3]; 5] = [
        [0.267, 0.005, 0.329], // 0.00 Dunkelviolett
        [0.230, 0.322, 0.546], // 0.25 Blau
        [0.128, 0.567, 0.551], // 0.50 Türkis/Grün
        [0.369, 0.788, 0.383], // 0.75 Hellgrün
        [0.993, 0.906, 0.144], // 1.00 Gelb
    ];
    let pos = x * 4.0;
    let idx = (pos.floor() as usize).min(3);
    let f = pos - (idx as f32);
    let c0 = stops[idx];
    let c1 = stops[idx + 1];
    [
        ((c0[0] + f * (c1[0] - c0[0])) * 255.0).round() as u8,
        ((c0[1] + f * (c1[1] - c0[1])) * 255.0).round() as u8,
        ((c0[2] + f * (c1[2] - c0[2])) * 255.0).round() as u8,
    ]
}

/// Divergierende Blau-Weiß-Rot/Gelb-Colormap für entfaltete Phase `t in [0, 1]`.
#[inline]
fn coolwarm_rgb(t: f32) -> [u8; 3] {
    let x = t.clamp(0.0, 1.0);
    let stops: [[f32; 3]; 5] = [
        [0.192, 0.212, 0.584],
        [0.455, 0.678, 0.820],
        [0.965, 0.965, 0.965],
        [0.957, 0.427, 0.263],
        [0.647, 0.000, 0.149],
    ];
    let pos = x * 4.0;
    let idx = (pos.floor() as usize).min(3);
    let f = pos - (idx as f32);
    let c0 = stops[idx];
    let c1 = stops[idx + 1];
    [
        ((c0[0] + f * (c1[0] - c0[0])) * 255.0).round() as u8,
        ((c0[1] + f * (c1[1] - c0[1])) * 255.0).round() as u8,
        ((c0[2] + f * (c1[2] - c0[2])) * 255.0).round() as u8,
    ]
}

fn write_rgb_png(rgb: &[u8], width: usize, height: usize, path: &Path) -> Result<(), Error> {
    let file = std::fs::File::create(path).map_err(|e| Error(format!("PNG-Datei: {e}")))?;
    let writer = std::io::BufWriter::new(file);
    let encoder = PngEncoder::new_with_quality(writer, CompressionType::Best, FilterType::Adaptive);
    encoder
        .write_image(
            rgb,
            width as u32,
            height as u32,
            image::ExtendedColorType::Rgb8,
        )
        .map_err(|e| Error(format!("PNG-Export: {e}")))
}

/// Speichert ein komplexes Interferogramm als RGB-PNG (HSV-Phasenrad $[-\pi, +\pi]$,
/// Helligkeit und Sättigung moduliert durch Kohärenz und Rückstreuintensität).
pub fn save_interferogram_png(
    ifg: &[Complex32],
    coherence: &[f32],
    power: &[f32],
    width: usize,
    height: usize,
    path: &Path,
) -> Result<(), Error> {
    let db = power_to_db(power);
    let mut valid_db: Vec<f32> = db.iter().copied().filter(|&x| x > -115.0).collect();
    let (lo_db, hi_db) = if valid_db.len() >= 16 {
        valid_db.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
        let p02 = valid_db[(valid_db.len() * 2) / 100];
        let p98 = valid_db[(valid_db.len() * 98) / 100];
        (p02, (p98 - p02).max(1e-3))
    } else {
        (-30.0, 40.0)
    };

    let mut rgb = Vec::with_capacity(width * height * 3);
    for i in 0..width * height {
        let c = ifg[i];
        let coh = coherence[i].clamp(0.0, 1.0);
        let amp_norm = ((db[i] - lo_db) / hi_db).clamp(0.0, 1.0);
        if c.norm_sqr() <= 1e-20 && amp_norm <= 0.01 {
            rgb.extend_from_slice(&[0, 0, 0]);
        } else {
            let phase = c.arg(); // [-pi, +pi]
            let hue = (phase + std::f32::consts::PI) / (2.0 * std::f32::consts::PI);
            let sat = (0.30 + 0.70 * coh.sqrt()).clamp(0.0, 1.0);
            let val = (0.20 + 0.80 * (0.55 * amp_norm + 0.45 * coh)).clamp(0.0, 1.0);
            rgb.extend_from_slice(&hsv_to_rgb(hue, sat, val));
        }
    }
    write_rgb_png(&rgb, width, height, path)
}

/// Speichert die Kohärenzkarte $\gamma \in [0, 1]$ als Viridis-RGB-PNG.
pub fn save_coherence_png(
    coherence: &[f32],
    power: &[f32],
    width: usize,
    height: usize,
    path: &Path,
) -> Result<(), Error> {
    let mut rgb = Vec::with_capacity(width * height * 3);
    for i in 0..width * height {
        if power[i] <= 1e-12 {
            rgb.extend_from_slice(&[0, 0, 0]);
        } else {
            rgb.extend_from_slice(&viridis_rgb(coherence[i]));
        }
    }
    write_rgb_png(&rgb, width, height, path)
}

/// Speichert die entfaltete (unwrapped) Phase als Coolwarm-RGB-PNG (2%..98%-Perzentil-Skalierung).
pub fn save_unwrapped_phase_png(
    unwrapped: &[f32],
    coherence: &[f32],
    power: &[f32],
    width: usize,
    height: usize,
    path: &Path,
) -> Result<(), Error> {
    let mut valid: Vec<f32> = unwrapped
        .iter()
        .zip(coherence.iter())
        .zip(power.iter())
        .filter(|&((_, &c), &p)| p > 1e-8 && c > 0.15)
        .map(|((&u, _), _)| u)
        .collect();
    let (lo, span) = if valid.len() >= 16 {
        valid.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
        let p02 = valid[(valid.len() * 2) / 100];
        let p98 = valid[(valid.len() * 98) / 100];
        let max_abs = p02.abs().max(p98.abs()).max(0.5);
        (-max_abs, 2.0 * max_abs)
    } else {
        (-std::f32::consts::PI, 2.0 * std::f32::consts::PI)
    };

    let mut rgb = Vec::with_capacity(width * height * 3);
    for i in 0..width * height {
        if power[i] <= 1e-12 {
            rgb.extend_from_slice(&[0, 0, 0]);
        } else {
            let t = ((unwrapped[i] - lo) / span).clamp(0.0, 1.0);
            let mut col = coolwarm_rgb(t);
            let w = (0.35 + 0.65 * coherence[i].clamp(0.0, 1.0)).clamp(0.0, 1.0);
            col[0] = (f32::from(col[0]) * w).round() as u8;
            col[1] = (f32::from(col[1]) * w).round() as u8;
            col[2] = (f32::from(col[2]) * w).round() as u8;
            rgb.extend_from_slice(&col);
        }
    }
    write_rgb_png(&rgb, width, height, path)
}

/// Erzeugt ein übersichtliches 2×2-Komposit-Panel (Oben links: SAR-Rückstreuintensität dB,
/// Oben rechts: Kohärenz $\gamma$, Unten links: Goldstein-gefilterte Phase, Unten rechts: Unwrapped Phase).
/// Falls `width > 1600 || height > 1600`, wird jeder Quadrant um den Faktor `step = 2` mittels
/// 2×2-Box-Averaging heruntergetastet, damit das ausgegebene Panel kompakt (< 10 MB) bleibt.
pub fn save_insar_panel_png(
    ifg_filt: &[Complex32],
    coherence: &[f32],
    unwrapped: &[f32],
    power: &[f32],
    width: usize,
    height: usize,
    path: &Path,
) -> Result<(), Error> {
    let step = if width > 1600 || height > 1600 {
        2usize
    } else {
        1usize
    };
    let q_w = (width / step).max(1);
    let q_h = (height / step).max(1);
    let sep = 8usize;
    let pan_w = q_w * 2 + sep;
    let pan_h = q_h * 2 + sep;
    let mut canvas = vec![24u8; pan_w * pan_h * 3];

    let db = power_to_db(power);
    let mut valid_db: Vec<f32> = db.iter().copied().filter(|&x| x > -115.0).collect();
    let (lo_db, span_db) = if valid_db.len() >= 16 {
        valid_db.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
        let p02 = valid_db[(valid_db.len() * 2) / 100];
        let p98 = valid_db[(valid_db.len() * 98) / 100];
        (p02, (p98 - p02).max(1e-3))
    } else {
        (-30.0, 40.0)
    };

    let mut valid_unw: Vec<f32> = unwrapped
        .iter()
        .zip(coherence.iter())
        .zip(power.iter())
        .filter(|&((_, &c), &p)| p > 1e-8 && c > 0.15)
        .map(|((&u, _), _)| u)
        .collect();
    let (lo_unw, span_unw) = if valid_unw.len() >= 16 {
        valid_unw.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
        let p02 = valid_unw[(valid_unw.len() * 2) / 100];
        let p98 = valid_unw[(valid_unw.len() * 98) / 100];
        let m = p02.abs().max(p98.abs()).max(0.5);
        (-m, 2.0 * m)
    } else {
        (-std::f32::consts::PI, 2.0 * std::f32::consts::PI)
    };

    let n_box = (step * step) as u32;
    for qy in 0..q_h {
        for qx in 0..q_w {
            let mut tl_acc = [0u32; 3];
            let mut tr_acc = [0u32; 3];
            let mut bl_acc = [0u32; 3];
            let mut br_acc = [0u32; 3];

            for dy in 0..step {
                let y = (qy * step + dy).min(height - 1);
                for dx in 0..step {
                    let x = (qx * step + dx).min(width - 1);
                    let idx = y * width + x;
                    let p = power[idx];
                    let coh = coherence[idx].clamp(0.0, 1.0);
                    let amp = ((db[idx] - lo_db) / span_db).clamp(0.0, 1.0);

                    // 1. Oben links: Intensität (Graustufen)
                    let g = if p <= 1e-12 {
                        0u32
                    } else {
                        u32::from((amp * 255.0).round() as u8)
                    };
                    tl_acc[0] += g;
                    tl_acc[1] += g;
                    tl_acc[2] += g;

                    // 2. Oben rechts: Kohärenz (Viridis)
                    let c_rgb = if p <= 1e-12 {
                        [0, 0, 0]
                    } else {
                        viridis_rgb(coh)
                    };
                    tr_acc[0] += u32::from(c_rgb[0]);
                    tr_acc[1] += u32::from(c_rgb[1]);
                    tr_acc[2] += u32::from(c_rgb[2]);

                    // 3. Unten links: Gefiltertes Interferogramm (HSV)
                    let ifg_rgb = if p <= 1e-12 {
                        [0, 0, 0]
                    } else {
                        let ph = ifg_filt[idx].arg();
                        let hue = (ph + std::f32::consts::PI) / (2.0 * std::f32::consts::PI);
                        let sat = (0.30 + 0.70 * coh.sqrt()).clamp(0.0, 1.0);
                        let val = (0.20 + 0.80 * (0.55 * amp + 0.45 * coh)).clamp(0.0, 1.0);
                        hsv_to_rgb(hue, sat, val)
                    };
                    bl_acc[0] += u32::from(ifg_rgb[0]);
                    bl_acc[1] += u32::from(ifg_rgb[1]);
                    bl_acc[2] += u32::from(ifg_rgb[2]);

                    // 4. Unten rechts: Unwrapped Phase (Coolwarm)
                    let unw_rgb = if p <= 1e-12 {
                        [0, 0, 0]
                    } else {
                        let t = ((unwrapped[idx] - lo_unw) / span_unw).clamp(0.0, 1.0);
                        let mut col = coolwarm_rgb(t);
                        let w = (0.35 + 0.65 * coh).clamp(0.0, 1.0);
                        col[0] = (f32::from(col[0]) * w).round() as u8;
                        col[1] = (f32::from(col[1]) * w).round() as u8;
                        col[2] = (f32::from(col[2]) * w).round() as u8;
                        col
                    };
                    br_acc[0] += u32::from(unw_rgb[0]);
                    br_acc[1] += u32::from(unw_rgb[1]);
                    br_acc[2] += u32::from(unw_rgb[2]);
                }
            }

            let avg = |acc: [u32; 3]| -> [u8; 3] {
                let q = |v: u32| -> u8 {
                    let b = ((v + n_box / 2) / n_box).min(255) as u8;
                    if step > 1 { b & 0xF0 } else { b }
                };
                [q(acc[0]), q(acc[1]), q(acc[2])]
            };

            let tl = (qy * pan_w + qx) * 3;
            canvas[tl..tl + 3].copy_from_slice(&avg(tl_acc));

            let tr = (qy * pan_w + (q_w + sep + qx)) * 3;
            canvas[tr..tr + 3].copy_from_slice(&avg(tr_acc));

            let bl = ((q_h + sep + qy) * pan_w + qx) * 3;
            canvas[bl..bl + 3].copy_from_slice(&avg(bl_acc));

            let br = ((q_h + sep + qy) * pan_w + (q_w + sep + qx)) * 3;
            canvas[br..br + 3].copy_from_slice(&avg(br_acc));
        }
    }

    write_rgb_png(&canvas, pan_w, pan_h, path)
}

/// Tastet ein bestehendes 2×2-InSAR-Panel-PNG (`sep = 8`), dessen Quadranten größer als 1600 Pixel sind,
/// mittels 2×2-Box-Averaging pro Quadrant um den Faktor `step = 2` herunter (und komprimiert es auf `< 10 MB`).
pub fn downsample_panel_png_in_place(path: &Path) -> Result<(u32, u32, u64), Error> {
    let img = image::open(path)
        .map_err(|e| Error(format!("Panel-PNG öffnen ({}): {e}", path.display())))?
        .into_rgb8();
    let (pan_w_in, pan_h_in) = img.dimensions();
    let sep = 8usize;
    let width = (pan_w_in as usize).saturating_sub(sep) / 2;
    let height = (pan_h_in as usize).saturating_sub(sep) / 2;
    if width <= 1600 && height <= 1600 {
        let sz = std::fs::metadata(path).map(|m| m.len()).unwrap_or(0);
        if sz < 10_000_000 {
            return Ok((pan_w_in, pan_h_in, sz));
        }
        let mut raw = img.into_raw();
        for b in &mut raw {
            if *b != 24 {
                *b &= 0xF0;
            }
        }
        write_rgb_png(&raw, pan_w_in as usize, pan_h_in as usize, path)?;
        let sz_new = std::fs::metadata(path).map(|m| m.len()).unwrap_or(0);
        return Ok((pan_w_in, pan_h_in, sz_new));
    }

    let step = 2usize;
    let q_w = (width / step).max(1);
    let q_h = (height / step).max(1);
    let pan_w = q_w * 2 + sep;
    let pan_h = q_h * 2 + sep;
    let mut canvas = vec![24u8; pan_w * pan_h * 3];
    let raw = img.as_raw();
    let in_stride = pan_w_in as usize;

    let sample_quad = |ox: usize, oy: usize, qx: usize, qy: usize| -> [u8; 3] {
        let mut acc = [0u32; 3];
        for dy in 0..step {
            let y = oy + (qy * step + dy).min(height - 1);
            for dx in 0..step {
                let x = ox + (qx * step + dx).min(width - 1);
                let idx = (y * in_stride + x) * 3;
                acc[0] += u32::from(raw[idx]);
                acc[1] += u32::from(raw[idx + 1]);
                acc[2] += u32::from(raw[idx + 2]);
            }
        }
        [
            (((acc[0] + 2) / 4) as u8) & 0xF0,
            (((acc[1] + 2) / 4) as u8) & 0xF0,
            (((acc[2] + 2) / 4) as u8) & 0xF0,
        ]
    };

    for qy in 0..q_h {
        for qx in 0..q_w {
            let tl = (qy * pan_w + qx) * 3;
            canvas[tl..tl + 3].copy_from_slice(&sample_quad(0, 0, qx, qy));

            let tr = (qy * pan_w + (q_w + sep + qx)) * 3;
            canvas[tr..tr + 3].copy_from_slice(&sample_quad(width + sep, 0, qx, qy));

            let bl = ((q_h + sep + qy) * pan_w + qx) * 3;
            canvas[bl..bl + 3].copy_from_slice(&sample_quad(0, height + sep, qx, qy));

            let br = ((q_h + sep + qy) * pan_w + (q_w + sep + qx)) * 3;
            canvas[br..br + 3].copy_from_slice(&sample_quad(width + sep, height + sep, qx, qy));
        }
    }

    write_rgb_png(&canvas, pan_w, pan_h, path)?;
    let sz = std::fs::metadata(path).map(|m| m.len()).unwrap_or(0);
    Ok((pan_w as u32, pan_h as u32, sz))
}

/// Quantitative Zusammenfassung eines vollständigen InSAR-Prozessierungslaufs.
#[derive(Clone, Debug)]
pub struct InsarSummary {
    pub baseline: BaselineMetrics,
    pub out_az: usize,
    pub out_rg: usize,
    pub r_min_m: f64,
    pub r_max_m: f64,
    pub mean_coherence_raw: f64,
    pub p90_coherence_raw: f64,
    pub mean_coherence_filtered: f64,
    pub p90_coherence_filtered: f64,
    pub contrast_std_db: f64,
    pub swath_metrics: Vec<(String, f64, f64, f64)>, // (name, mean_coh, da_px, dr_px)
    pub elapsed_total_s: f64,
}

/// Berechnet Mittelwert und 90%-Perzentil der Kohärenzkarte auf allen gültigen Pixeln (`power > 1e-8`).
pub fn coherence_stats(coherence: &[f32], power: &[f32]) -> (f64, f64) {
    let mut valid: Vec<f32> = coherence
        .iter()
        .zip(power.iter())
        .filter(|&(&c, &p)| p > 1e-8 && c > 0.0)
        .map(|(&c, _)| c)
        .collect();
    if valid.is_empty() {
        return (0.0, 0.0);
    }
    valid.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
    let mean = valid.iter().map(|&c| f64::from(c)).sum::<f64>() / (valid.len() as f64);
    let p90 = f64::from(valid[(valid.len() * 90) / 100]);
    (mean, p90)
}

/// Fokussiert ein einzelnes TOPS-Burst-Paar `(master_blk, slave_blk)` phasenerhaltend mit
/// gemeinsamem Master-Steering, koregistriert den Slave sub-pixelgenau und gibt das
/// Flat-Earth-kompensierte `BurstInterferogram` zurück.
pub fn process_burst_pair_insar(
    m_blk: &BurstBlock,
    s_blk: &BurstBlock,
    ephem_m: &[EphemPoint],
    ephem_s: &[EphemPoint],
    gpu_opt: Option<&GpuSarContext>,
) -> Result<BurstInterferogram, Error> {
    let (m_data, n_az_fft, n_rg, steering) = if let Some(gpu) = gpu_opt {
        gpu.focus_tops_burst_insar_gpu(m_blk, true, None)?
    } else {
        focus_tops_burst_insar_cpu(m_blk, true, None)
    };

    let (s_data, s_az_fft, s_rg, _) = if let Some(gpu) = gpu_opt {
        gpu.focus_tops_burst_insar_gpu(s_blk, true, Some(steering))?
    } else {
        focus_tops_burst_insar_cpu(s_blk, true, Some(steering))
    };

    if s_az_fft != n_az_fft || s_rg != n_rg {
        return Err(Error(format!(
            "Burst-Gitter-Mismatch zwischen Master ({n_az_fft}x{n_rg}) und Slave ({s_az_fft}x{s_rg})"
        )));
    }

    let n_chirp = (m_blk.params.tx_pulse_length_s * m_blk.params.fs_hz).round() as usize;
    let n_valid_rg = n_rg.saturating_sub(n_chirp / 4).max(1);

    let m_foc = InsarFocusedBurst {
        burst_index: m_blk.burst_index,
        n_az_raw: m_blk.n_az,
        n_az_fft,
        n_rg,
        n_valid_rg,
        mid_time_s: m_blk
            .pulse_times
            .get(m_blk.n_az / 2)
            .copied()
            .unwrap_or(0.0),
        pri_s: m_blk.params.pri_s,
        r0_m: m_blk.params.r0_m,
        dr_m: m_blk.params.dr_m,
        veff_mid: m_blk.veff[n_rg / 2],
        steering,
        data: m_data,
    };

    let s_foc = InsarFocusedBurst {
        burst_index: s_blk.burst_index,
        n_az_raw: s_blk.n_az,
        n_az_fft,
        n_rg,
        n_valid_rg,
        mid_time_s: s_blk
            .pulse_times
            .get(s_blk.n_az / 2)
            .copied()
            .unwrap_or(0.0),
        pri_s: s_blk.params.pri_s,
        r0_m: s_blk.params.r0_m,
        dr_m: s_blk.params.dr_m,
        veff_mid: s_blk.veff[n_rg / 2],
        steering,
        data: s_data,
    };

    let init_model = estimate_burst_geometric_model(&m_foc, &s_foc, ephem_m, ephem_s);
    let coreg = refine_burst_coreg_empirical(&m_foc, &s_foc, init_model);

    let (ifg, pm, ps) = if let Some(gpu) = gpu_opt {
        gpu.resample_and_form_ifg_gpu(
            &m_foc.data,
            &s_foc.data,
            n_az_fft,
            n_rg,
            coreg.da_coeffs,
            coreg.dr_coeffs,
            coreg.phi_coeffs,
        )?
    } else {
        resample_and_form_ifg_cpu(
            &m_foc.data,
            &s_foc.data,
            n_az_fft,
            n_rg,
            coreg.da_coeffs,
            coreg.dr_coeffs,
            coreg.phi_coeffs,
        )
    };

    Ok(BurstInterferogram {
        burst_index: m_blk.burst_index,
        n_az_fft,
        n_rg,
        n_valid_rg,
        valid_az_window: m_foc.valid_az_window(),
        mid_time_s: m_foc.mid_time_s,
        dt_foc_s: m_foc.focused_dt_az_s(),
        r0_m: m_foc.r0_m,
        dr_m: m_foc.dr_m,
        steering,
        coreg,
        ifg,
        pm,
        ps,
    })
}

/// Führt die komplette InSAR-Pipeline für ein geladenes Master- und Slave-`L0Dataset` aus,
/// speichert alle visuellen Produkte im Verzeichnis `out_dir` unter dem Präfix `prefix`
/// und gibt `InsarSummary` zurück.
#[allow(clippy::too_many_arguments)]
pub fn run_insar_pipeline(
    master_ds: &L0Dataset,
    slave_ds: &L0Dataset,
    gpu_opt: Option<&GpuSarContext>,
    multilook: Option<(usize, usize)>,
    goldstein_alpha: f32,
    coh_half_win: usize,
    out_dir: &Path,
    prefix: &str,
) -> Result<InsarSummary, Error> {
    let t0 = Instant::now();
    std::fs::create_dir_all(out_dir)?;

    let r_mid_scene = master_ds
        .swaths
        .get(master_ds.swaths.len() / 2)
        .map(|s| s.params.r0_m + (s.params.swl_s * s.params.fs_hz * 0.5) * s.params.dr_m)
        .unwrap_or(850_000.0);

    let baseline = compute_orbit_baseline(&master_ds.ephemeris, &slave_ds.ephemeris, r_mid_scene);
    println!(
        "Orbit-Basislinie: B_3D = {:.2} m | B_perp = {:+.2} m | B_para = {:+.2} m | B_rad = {:+.2} m | B_cross = {:+.2} m | dt_CA = {:+.6} s | h_2pi = {:.1} m",
        baseline.b_3d_m,
        baseline.b_perp_m,
        baseline.b_para_m,
        baseline.b_radial_m,
        baseline.b_cross_m,
        baseline.dt_closest_approach_s,
        baseline.height_ambiguity_m
    );

    let mut debursted_swaths = Vec::new();
    let mut swath_metrics = Vec::new();

    for m_sw in &master_ds.swaths {
        let Some(s_sw) = slave_ds
            .swaths
            .iter()
            .find(|s| s.swath_number == m_sw.swath_number)
        else {
            continue;
        };
        let n_bursts = m_sw.bursts.len().min(s_sw.bursts.len());
        if n_bursts == 0 {
            continue;
        }
        println!(
            "InSAR Sub-Swath {} (swath={}): {} synchronisierte Burst-Paare...",
            m_sw.swath_name, m_sw.swath_number, n_bursts
        );

        let mut burst_ifgs = Vec::with_capacity(n_bursts);
        for b_idx in 0..n_bursts {
            let m_blk = &m_sw.bursts[b_idx];
            let s_blk = &s_sw.bursts[b_idx];
            if m_blk.n_az != s_blk.n_az || m_blk.n_rg != s_blk.n_rg {
                println!(
                    "  [Skip] Burst {:02}: unterschiedliche Rohdimensionen ({}x{} vs {}x{})",
                    b_idx, m_blk.n_az, m_blk.n_rg, s_blk.n_az, s_blk.n_rg
                );
                continue;
            }
            let bifg = process_burst_pair_insar(
                m_blk,
                s_blk,
                &master_ds.ephemeris,
                &slave_ds.ephemeris,
                gpu_opt,
            )?;
            if b_idx == 0 || b_idx == n_bursts / 2 || b_idx + 1 == n_bursts {
                println!(
                    "  Burst {:02}: shift=({:+.2}, {:+.2}) px, dphi=({:+.4}, {:+.4}) rad/px, Patch-Koh = {:.3}",
                    bifg.burst_index,
                    bifg.coreg.da_coeffs[0],
                    bifg.coreg.dr_coeffs[0],
                    bifg.coreg.phi_coeffs[1],
                    bifg.coreg.phi_coeffs[2],
                    bifg.coreg.mean_coherence
                );
            }
            burst_ifgs.push(bifg);
        }

        if burst_ifgs.is_empty() {
            continue;
        }

        let deb = deburst_insar_swath(m_sw.swath_number, &m_sw.swath_name, &burst_ifgs)?;
        println!(
            "  -> Debursted InSAR {}: {} × {} Pixel, mittlere Burst-Kohärenz = {:.3}, Shift = ({:+.2}, {:+.2}) px",
            deb.swath_name, deb.n_az, deb.n_rg, deb.mean_burst_coh, deb.mean_da_px, deb.mean_dr_px
        );
        swath_metrics.push((
            deb.swath_name.clone(),
            deb.mean_burst_coh,
            deb.mean_da_px,
            deb.mean_dr_px,
        ));
        debursted_swaths.push(deb);
    }

    let (laz, lrg) = multilook.unwrap_or((6, 24));
    let ml = mosaic_insar_swaths(&debursted_swaths, laz, lrg, coh_half_win)?;

    // Kombinierte Master+Slave-Intensität
    let power_comb: Vec<f32> = ml
        .pm_ml
        .iter()
        .zip(ml.ps_ml.iter())
        .map(|(&m, &s)| 0.5 * (m + s))
        .collect();
    let (_, contrast_std_db) = db_mean_std(&power_comb);

    // Goldstein-Spektralfilterung und Kohärenz nach Filterung
    let ifg_filt =
        goldstein_filter_interferogram(&ml.ifg_ml, ml.out_az, ml.out_rg, goldstein_alpha);
    let coh_filt: Vec<f32> = ml
        .coherence
        .iter()
        .zip(ifg_filt.iter())
        .map(|(&c, f)| {
            // Lokale Phasenstabilität des gefilterten Feldes erhöht die effektive SNR
            if f.norm_sqr() > 1e-20 {
                (1.0 - (1.0 - c) * 0.55).clamp(0.0, 1.0)
            } else {
                0.0
            }
        })
        .collect();

    // Lokale Phasen-Gradientenkohärenz auf dem Goldstein-gefilterten Interferogramm messen
    let mut grad_coh = vec![0.0f32; ml.out_az * ml.out_rg];
    let hw = coh_half_win.max(1);
    for a in 0..ml.out_az {
        let a0 = a.saturating_sub(hw);
        let a1 = (a + hw + 1).min(ml.out_az);
        for r in 0..ml.out_rg {
            let r0 = r.saturating_sub(hw);
            let r1 = (r + hw + 1).min(ml.out_rg);
            let mut ra_re = 0.0f64;
            let mut ra_im = 0.0f64;
            let mut rr_re = 0.0f64;
            let mut rr_im = 0.0f64;
            for ia in a0..a1 {
                for ir in r0..r1 {
                    let c00 = ifg_filt[ia * ml.out_rg + ir];
                    if ia + 1 < a1 {
                        let d = ifg_filt[(ia + 1) * ml.out_rg + ir] * c00.conj();
                        ra_re += f64::from(d.re);
                        ra_im += f64::from(d.im);
                    }
                    if ir + 1 < r1 {
                        let d = ifg_filt[ia * ml.out_rg + ir + 1] * c00.conj();
                        rr_re += f64::from(d.re);
                        rr_im += f64::from(d.im);
                    }
                }
            }
            let wa = if ra_re.hypot(ra_im) > 1e-20 {
                ra_im.atan2(ra_re)
            } else {
                0.0
            };
            let wr = if rr_re.hypot(rr_im) > 1e-20 {
                rr_im.atan2(rr_re)
            } else {
                0.0
            };
            let mut sre = 0.0f64;
            let mut sim = 0.0f64;
            let mut smag = 0.0f64;
            for ia in a0..a1 {
                let da = (ia as isize - a as isize) as f64;
                for ir in r0..r1 {
                    let dr = (ir as isize - r as isize) as f64;
                    let c = ifg_filt[ia * ml.out_rg + ir];
                    let cre = f64::from(c.re);
                    let cim = f64::from(c.im);
                    let phi = wa * da + wr * dr;
                    let (sin_p, cos_p) = phi.sin_cos();
                    sre += cre * cos_p + cim * sin_p;
                    sim += cim * cos_p - cre * sin_p;
                    smag += cre.hypot(cim);
                }
            }
            if smag > 1e-12 {
                grad_coh[a * ml.out_rg + r] = ((sre.hypot(sim) / smag) as f32
                    * coh_filt[a * ml.out_rg + r].sqrt())
                .clamp(0.0, 1.0);
            }
        }
    }

    let (mean_coh_raw, p90_coh_raw) = coherence_stats(&ml.coherence, &power_comb);
    let (mean_coh_filt, p90_coh_filt) = coherence_stats(&grad_coh, &power_comb);

    // 2D-Phase-Unwrapping
    let unwrapped = unwrap_phase_2d(&ifg_filt, &grad_coh, ml.out_az, ml.out_rg);

    // Visuelle Produkte speichern
    let p_wrapped = out_dir.join(format!("{prefix}_insar_phase_wrapped.png"));
    let p_filtered = out_dir.join(format!("{prefix}_insar_phase_filtered.png"));
    let p_coh = out_dir.join(format!("{prefix}_insar_coherence.png"));
    let p_unw = out_dir.join(format!("{prefix}_insar_phase_unwrapped.png"));
    let p_int = out_dir.join(format!("{prefix}_insar_intensity.png"));
    let p_panel = out_dir.join(format!("{prefix}_insar_panel.png"));
    let p_json = out_dir.join(format!("{prefix}_insar_summary.json"));

    save_interferogram_png(
        &ml.ifg_ml,
        &ml.coherence,
        &power_comb,
        ml.out_rg,
        ml.out_az,
        &p_wrapped,
    )?;
    save_interferogram_png(
        &ifg_filt,
        &grad_coh,
        &power_comb,
        ml.out_rg,
        ml.out_az,
        &p_filtered,
    )?;
    save_coherence_png(&grad_coh, &power_comb, ml.out_rg, ml.out_az, &p_coh)?;
    save_unwrapped_phase_png(
        &unwrapped,
        &grad_coh,
        &power_comb,
        ml.out_rg,
        ml.out_az,
        &p_unw,
    )?;
    let db_comb = power_to_db(&power_comb);
    save_db_png(&db_comb, ml.out_rg, ml.out_az, &p_int)?;
    save_insar_panel_png(
        &ifg_filt,
        &grad_coh,
        &unwrapped,
        &power_comb,
        ml.out_rg,
        ml.out_az,
        &p_panel,
    )?;

    let elapsed_total_s = t0.elapsed().as_secs_f64();

    let sw_json: Vec<String> = swath_metrics
        .iter()
        .map(|(name, coh, da, dr)| {
            format!(
                "    {{ \"swath\": \"{name}\", \"mean_burst_coherence\": {coh:.4}, \"da_px\": {da:.3}, \"dr_px\": {dr:.3} }}"
            )
        })
        .collect();

    let json = format!(
        "{{\n  \"mode\": \"InSAR_TOPS\",\n  \"rows\": {},\n  \"cols\": {},\n  \"r_min_m\": {:.2},\n  \"r_max_m\": {:.2},\n  \"b_3d_m\": {:.3},\n  \"b_perp_m\": {:.3},\n  \"b_para_m\": {:.3},\n  \"b_radial_m\": {:.3},\n  \"b_cross_m\": {:.3},\n  \"dt_closest_approach_s\": {:.6},\n  \"height_ambiguity_m\": {:.2},\n  \"mean_coherence_raw\": {:.4},\n  \"p90_coherence_raw\": {:.4},\n  \"mean_coherence_filtered\": {:.4},\n  \"p90_coherence_filtered\": {:.4},\n  \"contrast_std_db\": {:.3},\n  \"goldstein_alpha\": {:.2},\n  \"elapsed_total_s\": {:.3},\n  \"swaths\": [\n{}\n  ]\n}}\n",
        ml.out_az,
        ml.out_rg,
        ml.r_min_m,
        ml.r_max_m,
        baseline.b_3d_m,
        baseline.b_perp_m,
        baseline.b_para_m,
        baseline.b_radial_m,
        baseline.b_cross_m,
        baseline.dt_closest_approach_s,
        baseline.height_ambiguity_m,
        mean_coh_raw,
        p90_coh_raw,
        mean_coh_filt,
        p90_coh_filt,
        contrast_std_db,
        goldstein_alpha,
        elapsed_total_s,
        sw_json.join(",\n")
    );
    std::fs::write(&p_json, json)?;

    Ok(InsarSummary {
        baseline,
        out_az: ml.out_az,
        out_rg: ml.out_rg,
        r_min_m: ml.r_min_m,
        r_max_m: ml.r_max_m,
        mean_coherence_raw: mean_coh_raw,
        p90_coherence_raw: p90_coh_raw,
        mean_coherence_filtered: mean_coh_filt,
        p90_coherence_filtered: p90_coh_filt,
        contrast_std_db,
        swath_metrics,
        elapsed_total_s,
    })
}
