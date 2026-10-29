//! Sub-kommutierte Orbit-Ephemeriden, Attitude-Quaternionen und effektive Geschwindigkeit $V_{\text{eff}}$.
//!
//! Liest 64-Wort-Zyklen (`subcom_index = 1..=64`) im Big-Endian-IEEE-754-Layout:
//! - Worte 1..12: ECEF-Position $(X, Y, Z)$ als $3 \times \text{f64}$ [m]
//! - Worte 13..18: ECEF-Geschwindigkeit $(V_x, V_y, V_z)$ als $3 \times \text{f32}$ [m/s]
//! - Worte 19..22: PVT-Zeitstempel [s]
//! - Worte 23..30: Attitude-Quaternion $(q_0, q_1, q_2, q_3)$ als $4 \times \text{f32}$

use crate::l0::PacketRecord;
use crate::types::Vec3d;

/// WGS-84 große Halbachse $a$ [m].
pub const WGS84_A_M: f64 = 6_378_137.0;
/// WGS-84 kleine Halbachse $b$ [m].
pub const WGS84_B_M: f64 = 6_356_752.314_245;
/// Erdrotationsrate $\omega_\oplus$ [rad/s].
pub const EARTH_OMEGA_RAD_S: f64 = 7.292_115_9e-5;

/// Ein dekodierter Orbit- + Attitude-Zustandsvektor aus einem 64-Pakete-Zyklus.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct EphemPoint {
    pub time_s: f64,
    pub pos: Vec3d,
    pub vel: Vec3d,
    pub quat: [f32; 4],
}

fn be_f64(words: &[u16]) -> f64 {
    let mut b = [0u8; 8];
    for (i, w) in words.iter().enumerate().take(4) {
        b[2 * i..2 * i + 2].copy_from_slice(&w.to_be_bytes());
    }
    f64::from_be_bytes(b)
}

fn be_f32(words: &[u16]) -> f32 {
    let mut b = [0u8; 4];
    for (i, w) in words.iter().enumerate().take(2) {
        b[2 * i..2 * i + 2].copy_from_slice(&w.to_be_bytes());
    }
    f32::from_be_bytes(b)
}

/// Dekodiert einen 64-Worte-Sub-Kommutationsblock (0-basiert übergeben für Indizes 1..=64).
pub fn decode_block(d: &[u16; 64]) -> EphemPoint {
    let pos = Vec3d::new(be_f64(&d[0..4]), be_f64(&d[4..8]), be_f64(&d[8..12]));
    let vel = Vec3d::new(
        f64::from(be_f32(&d[12..14])),
        f64::from(be_f32(&d[14..16])),
        f64::from(be_f32(&d[16..18])),
    );
    let pvt = f64::from(d[18]) * 16_777_216.0
        + f64::from(d[19]) * 256.0
        + f64::from(d[20]) / 256.0
        + f64::from(d[21]) / 16_777_216.0;
    let quat = [
        be_f32(&d[22..24]),
        be_f32(&d[24..26]),
        be_f32(&d[26..28]),
        be_f32(&d[28..30]),
    ];
    EphemPoint {
        time_s: pvt,
        pos,
        vel,
        quat,
    }
}

/// Extrahiert alle validen Ephemeriden-Zustandsvektoren aus der Paketliste.
///
/// Unterstützt auch lückenhafte Zyklen, sofern zumindest die ersten 22 Worte
/// (`subcom_index = 1..=22` für Position, Geschwindigkeit und PVT) aufeinanderfolgend
/// vorhanden sind.
pub fn extract_ephemeris(records: &[PacketRecord]) -> Vec<EphemPoint> {
    let mut out = Vec::new();
    let mut buf = [0u16; 64];
    let mut filled_mask = 0u64;
    for rec in records {
        let idx = rec.subcom_index;
        if idx == 1 {
            if (filled_mask & ((1u64 << 22) - 1)) == ((1u64 << 22) - 1) {
                let pt = decode_block(&buf);
                if pt.pos.norm() > 6_000_000.0 && pt.pos.norm() < 8_000_000.0 {
                    out.push(pt);
                }
            }
            buf = [0u16; 64];
            filled_mask = 0;
        }
        if (1..=64).contains(&idx) {
            let k = (idx - 1) as usize;
            buf[k] = rec.subcom_word;
            filled_mask |= 1u64 << k;
        }
    }
    if (filled_mask & ((1u64 << 22) - 1)) == ((1u64 << 22) - 1) {
        let pt = decode_block(&buf);
        if pt.pos.norm() > 6_000_000.0 && pt.pos.norm() < 8_000_000.0 {
            out.push(pt);
        }
    }
    // Deduplizieren aufeinanderfolgender identischer PVT-Zeitstempel
    let mut unique: Vec<EphemPoint> = Vec::with_capacity(out.len());
    for pt in out {
        if let Some(last) = unique.last()
            && (pt.time_s - last.time_s).abs() < 1e-4
        {
            continue;
        }
        unique.push(pt);
    }
    unique
}

#[inline]
fn lerp(a: f64, b: f64, f: f64) -> f64 {
    a + (b - a) * f
}

#[inline]
fn hermite(p0: f64, v0: f64, p1: f64, v1: f64, dt: f64, f: f64) -> f64 {
    let f2 = f * f;
    let f3 = f2 * f;
    let h00 = 2.0 * f3 - 3.0 * f2 + 1.0;
    let h10 = f3 - 2.0 * f2 + f;
    let h01 = -2.0 * f3 + 3.0 * f2;
    let h11 = f3 - f2;
    h00 * p0 + h10 * dt * v0 + h01 * p1 + h11 * dt * v1
}

#[inline]
fn hermite_deriv(p0: f64, v0: f64, p1: f64, v1: f64, dt: f64, f: f64) -> f64 {
    let f2 = f * f;
    let dh00 = 6.0 * (f2 - f);
    let dh10 = 3.0 * f2 - 4.0 * f + 1.0;
    let dh01 = -dh00;
    let dh11 = 3.0 * f2 - 2.0 * f;
    (dh00 * p0 + dh01 * p1) / dt + dh10 * v0 + dh11 * v1
}

fn bracket(points: &[EphemPoint], t: f64) -> (EphemPoint, EphemPoint, f64) {
    if points.len() == 1 || t <= points[0].time_s {
        return (points[0], points[0], 0.0);
    }
    let last = points[points.len() - 1];
    if t >= last.time_s {
        return (last, last, 0.0);
    }
    let mut lo = 0;
    while lo + 1 < points.len() && points[lo + 1].time_s < t {
        lo += 1;
    }
    let a = points[lo];
    let b = points[lo + 1];
    let f = if b.time_s > a.time_s {
        (t - a.time_s) / (b.time_s - a.time_s)
    } else {
        0.0
    };
    (a, b, f)
}

/// Löst ein kleines symmetrisches Gleichungssystem `A * x = b` der Dimension `D` (Gauß-Jordan mit Pivotisierung).
fn solve_sym<const D: usize>(mut a: [[f64; D]; D], mut b: [f64; D]) -> [f64; D] {
    for i in 0..D {
        let mut piv = i;
        let mut max_v = a[i][i].abs();
        for (r, row) in a.iter().enumerate().take(D).skip(i + 1) {
            if row[i].abs() > max_v {
                max_v = row[i].abs();
                piv = r;
            }
        }
        if max_v < 1e-14 {
            continue;
        }
        if piv != i {
            a.swap(i, piv);
            b.swap(i, piv);
        }
        let inv = 1.0 / a[i][i];
        for item in a[i].iter_mut().take(D).skip(i) {
            *item *= inv;
        }
        b[i] *= inv;
        for r in 0..D {
            if r != i {
                let f = a[r][i];
                if f.abs() > 0.0 {
                    for c in i..D {
                        a[r][c] -= f * a[i][c];
                    }
                    b[r] -= f * b[i];
                }
            }
        }
    }
    b
}

/// Glatte globale Polynom-Ausgleichung (Grad 3, gemeinsam angepasst an Position `pos` und
/// Doppler-Geschwindigkeit `vel`) über den Szenen-Orbitbogen (`<= 180 s`).
/// Liefert `(pos(t), vel(t))` ohne 1-Hz-GPS-Quantisierungssprünge, ohne Runge-Randfehler
/// und mit exakt konsistenter analytischer Ableitung `vel(t) == d/dt pos(t)`.
fn eval_smooth_orbit_poly(points: &[EphemPoint], t: f64) -> Option<(Vec3d, Vec3d)> {
    if points.len() < 4 {
        return None;
    }
    let t0 = points.first()?.time_s;
    let t1 = points.last()?.time_s;
    let span = t1 - t0;
    if !(2.0..=180.0).contains(&span) {
        return None;
    }
    let t_mid = 0.5 * (t0 + t1);
    let half = 0.5 * span;

    let mut ata = [[0.0f64; 4]; 4];
    let mut atx = [0.0f64; 4];
    let mut aty = [0.0f64; 4];
    let mut atz = [0.0f64; 4];

    // Gewichtung der Geschwindigkeits-Beobachtungen `half * v(u) = c1 + 2*c2*u + 3*c3*u^2`:
    // Da die GPS-Doppler-Geschwindigkeit (`p.vel`) frei von den +-0.4 m entlang-Track-Positionssprüngen
    // einzelner 1-Hz-Pakete ist, stabilisiert `w_vel` die 1. und 2. Ableitung bis an die Intervallränder.
    let w_vel = 4.0f64;

    for p in points {
        let u = (p.time_s - t_mid) / half;
        let u2 = u * u;
        let b_pos = [1.0, u, u2, u2 * u];
        let b_vel = [0.0, 1.0, 2.0 * u, 3.0 * u2];
        let vx_scaled = p.vel.x * half;
        let vy_scaled = p.vel.y * half;
        let vz_scaled = p.vel.z * half;
        for i in 0..4 {
            for j in i..4 {
                ata[i][j] += b_pos[i] * b_pos[j] + w_vel * b_vel[i] * b_vel[j];
            }
            atx[i] += b_pos[i] * p.pos.x + w_vel * b_vel[i] * vx_scaled;
            aty[i] += b_pos[i] * p.pos.y + w_vel * b_vel[i] * vy_scaled;
            atz[i] += b_pos[i] * p.pos.z + w_vel * b_vel[i] * vz_scaled;
        }
    }
    for i in 0..4 {
        for j in 0..i {
            ata[i][j] = ata[j][i];
        }
    }

    let cx = solve_sym(ata, atx);
    let cy = solve_sym(ata, aty);
    let cz = solve_sym(ata, atz);

    let u = ((t - t_mid) / half).clamp(-1.5, 1.5);
    let u2 = u * u;
    let u3 = u2 * u;
    let pos = Vec3d::new(
        cx[0] + cx[1] * u + cx[2] * u2 + cx[3] * u3,
        cy[0] + cy[1] * u + cy[2] * u2 + cy[3] * u3,
        cz[0] + cz[1] * u + cz[2] * u2 + cz[3] * u3,
    );
    let inv_half = 1.0 / half;
    let vel = Vec3d::new(
        (cx[1] + 2.0 * cx[2] * u + 3.0 * cx[3] * u2) * inv_half,
        (cy[1] + 2.0 * cy[2] * u + 3.0 * cy[3] * u2) * inv_half,
        (cz[1] + 2.0 * cz[2] * u + 3.0 * cz[3] * u2) * inv_half,
    );
    Some((pos, vel))
}

/// Interpolierte ECEF-Position [m] zur Zeit `t` [s] mittels glatter Polynom-Ausgleichung (bzw. kubischer
/// Hermite-Spline-Interpolation für kurze Punktlisten).
pub fn interp_pos(points: &[EphemPoint], t: f64) -> Vec3d {
    if let Some((pos, _)) = eval_smooth_orbit_poly(points, t) {
        return pos;
    }
    let (a, b, f) = bracket(points, t);
    let dt = b.time_s - a.time_s;
    if dt > 1e-4 && dt < 30.0 {
        Vec3d::new(
            hermite(a.pos.x, a.vel.x, b.pos.x, b.vel.x, dt, f),
            hermite(a.pos.y, a.vel.y, b.pos.y, b.vel.y, dt, f),
            hermite(a.pos.z, a.vel.z, b.pos.z, b.vel.z, dt, f),
        )
    } else {
        Vec3d::new(
            lerp(a.pos.x, b.pos.x, f),
            lerp(a.pos.y, b.pos.y, f),
            lerp(a.pos.z, b.pos.z, f),
        )
    }
}

/// Interpolierte ECEF-Geschwindigkeit [m/s] zur Zeit `t` [s].
pub fn interp_vel(points: &[EphemPoint], t: f64) -> Vec3d {
    if let Some((_, vel)) = eval_smooth_orbit_poly(points, t) {
        return vel;
    }
    let (a, b, f) = bracket(points, t);
    let dt = b.time_s - a.time_s;
    if dt > 1e-4 && dt < 30.0 {
        Vec3d::new(
            hermite_deriv(a.pos.x, a.vel.x, b.pos.x, b.vel.x, dt, f),
            hermite_deriv(a.pos.y, a.vel.y, b.pos.y, b.vel.y, dt, f),
            hermite_deriv(a.pos.z, a.vel.z, b.pos.z, b.vel.z, dt, f),
        )
    } else {
        Vec3d::new(
            lerp(a.vel.x, b.vel.x, f),
            lerp(a.vel.y, b.vel.y, f),
            lerp(a.vel.z, b.vel.z, f),
        )
    }
}

/// Inertiale Geschwindigkeit aus ECEF-Zustand: $\mathbf{v}_i = \mathbf{v}_e + \boldsymbol{\omega}_\oplus \times \mathbf{r}$.
#[inline]
pub fn inertial_vel(pos: Vec3d, ecef_vel: Vec3d) -> Vec3d {
    let wxr = Vec3d::new(-EARTH_OMEGA_RAD_S * pos.y, EARTH_OMEGA_RAD_S * pos.x, 0.0);
    ecef_vel + wxr
}

/// Effektive Radar-Geschwindigkeit $V_{\text{eff}}(R) = \sqrt{V_s \cdot V_g(R)}$ [m/s] auf dem WGS-84-Ellipsoid.
pub fn effective_velocity(space_vel: f64, pos: Vec3d, slant_range: f64) -> f64 {
    let a = WGS84_A_M;
    let b = WGS84_B_M;
    let h = pos.norm();
    let w = space_vel / h;
    let lat = pos.z.atan2(pos.x.hypot(pos.y));
    let (sin, cos) = lat.sin_cos();
    let local_re = (((a * a * cos).powi(2) + (b * b * sin).powi(2))
        / ((a * cos).powi(2) + (b * sin).powi(2)))
    .sqrt();
    let cos_beta = (local_re * local_re + h * h - slant_range * slant_range) / (2.0 * local_re * h);
    let ground_vel = local_re * w * cos_beta.clamp(-1.0, 1.0);
    (space_vel * ground_vel).sqrt()
}

/// Berechnet $V_{\text{eff}}(r)$ für jedes Range-Bin der Mittellinie eines Blocks.
pub fn compute_veff_vector(
    pos_mid: Vec3d,
    vel_mid_ecef: Vec3d,
    r0_m: f64,
    dr_m: f64,
    n_rg: usize,
) -> Vec<f64> {
    let vs = inertial_vel(pos_mid, vel_mid_ecef).norm();
    (0..n_rg)
        .map(|j| effective_velocity(vs, pos_mid, r0_m + (j as f64) * dr_m))
        .collect()
}
