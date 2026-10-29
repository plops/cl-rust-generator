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

/// Interpolierte ECEF-Position [m] zur Zeit `t` [s].
pub fn interp_pos(points: &[EphemPoint], t: f64) -> Vec3d {
    let (a, b, f) = bracket(points, t);
    Vec3d::new(
        lerp(a.pos.x, b.pos.x, f),
        lerp(a.pos.y, b.pos.y, f),
        lerp(a.pos.z, b.pos.z, f),
    )
}

/// Interpolierte ECEF-Geschwindigkeit [m/s] zur Zeit `t` [s].
pub fn interp_vel(points: &[EphemPoint], t: f64) -> Vec3d {
    let (a, b, f) = bracket(points, t);
    Vec3d::new(
        lerp(a.vel.x, b.vel.x, f),
        lerp(a.vel.y, b.vel.y, f),
        lerp(a.vel.z, b.vel.z, f),
    )
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
