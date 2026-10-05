//! Orbit-Ephemeriden: SubCom-Worte → ECEF-Position/Geschwindigkeit.
//!
//! Dekodierung nach `sentinel1decoder.utilities.read_subcommed_data`:
//! **big-endian**, 1-basiert ab SubCom-Index 1 (Positionen 3×`>f8`,
//! Geschwindigkeiten 3×`>f4`). Der `copernicus-radar`-Decoder liest hier
//! fälschlich little-endian (s. Plan §4.3) — daher eigene Implementierung.
//!
//! Liefert außerdem die effektive Geschwindigkeit (SSFocus-Formel) und den
//! geometrischen Doppler-Centroid (S1: Null-Doppler-gesteuert, ≈ 0).

use crate::types::{TX_WAVELENGTH_M, Vec3d, WGS84_A_M, WGS84_B_M};

/// Erdrotationsrate in rad/s (WGS84).
pub const EARTH_OMEGA_RAD_S: f64 = 7.292_115_9e-5;

/// Ein Ephemeridenpunkt: POD-Zeitstempel + ECEF-Zustandsvektor.
#[derive(Clone, Copy, Debug)]
pub struct EphemPoint {
    pub time_s: f64,
    pub pos: Vec3d,
    pub vel: Vec3d,
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

/// Dekodiert einen 64-Worte-Block (SubCom-Indizes 1..=64, 0-basiert übergeben).
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
    EphemPoint {
        time_s: pvt,
        pos,
        vel,
    }
}

/// Sammelt Ephemeridenblöcke aus dem SubCom-Strom (Paketreihenfolge).
///
/// Wie sentinel1decoder: ein Block beginnt bei Index 1, gefolgt von
/// lückenlos 1..=64.
pub fn collect_blocks(stream: &[(u8, u16)]) -> Vec<EphemPoint> {
    let mut out = Vec::new();
    let mut i = 0;
    while i + 64 <= stream.len() {
        if stream[i].0 == 1 && (0..64).all(|k| stream[i + k].0 as usize == 1 + k) {
            let mut d = [0u16; 64];
            for (k, slot) in d.iter_mut().enumerate() {
                *slot = stream[i + k].1;
            }
            out.push(decode_block(&d));
            i += 64;
        } else {
            i += 1;
        }
    }
    out
}

/// Lineare Interpolation zwischen Stützstellen (Enden geklemmt).
fn lerp(a: f64, b: f64, f: f64) -> f64 {
    a + (b - a) * f
}

fn bracket(points: &[EphemPoint], t: f64) -> (EphemPoint, EphemPoint, f64) {
    if t <= points[0].time_s {
        return (points[0], points[0], 0.0);
    }
    if t >= points[points.len() - 1].time_s {
        let p = points[points.len() - 1];
        return (p, p, 0.0);
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

/// Interpolierte ECEF-Position zur Pulszeit `t`.
pub fn interp_pos(points: &[EphemPoint], t: f64) -> Vec3d {
    let (a, b, f) = bracket(points, t);
    Vec3d::new(
        lerp(a.pos.x, b.pos.x, f),
        lerp(a.pos.y, b.pos.y, f),
        lerp(a.pos.z, b.pos.z, f),
    )
}

/// Interpolierte ECEF-Geschwindigkeit zur Pulszeit `t`.
pub fn interp_vel(points: &[EphemPoint], t: f64) -> Vec3d {
    let (a, b, f) = bracket(points, t);
    Vec3d::new(
        lerp(a.vel.x, b.vel.x, f),
        lerp(a.vel.y, b.vel.y, f),
        lerp(a.vel.z, b.vel.z, f),
    )
}

/// Effektive Geschwindigkeit in m/s (SSFocus `focus.py`):
/// `v_eff = √(v_sat · v_boden)` mit WGS84-Erdradius und Schrägentfernung.
pub fn effective_velocity(space_vel: f64, pos: Vec3d, slant_range: f64) -> f64 {
    let a = WGS84_A_M;
    let b = WGS84_B_M;
    let h = pos.norm();
    let w = space_vel / h;
    // Breiten-Näherung aus focus.py (dort atan; atan2 identisch für x > 0).
    let lat = pos.z.atan2(pos.x);
    let (sin, cos) = lat.sin_cos();
    let local_re = (((a * a * cos).powi(2) + (b * b * sin).powi(2))
        / ((a * cos).powi(2) + (b * sin).powi(2)))
    .sqrt();
    let cos_beta = (local_re * local_re + h * h - slant_range * slant_range) / (2.0 * local_re * h);
    let ground_vel = local_re * w * cos_beta.clamp(-1.0, 1.0);
    (space_vel * ground_vel).sqrt()
}

/// Inertiale Geschwindigkeit aus ECEF-Zustand: `v_i = v_e + ω×r`.
///
/// Die SubCom-Geschwindigkeiten sind erdfest (Echtdaten: `|v| ≈ 7589 m/s`,
/// erst `|v + ω×r| ≈ 7501,6 m/s` erfüllt vis-viva). `effective_velocity`
/// braucht die inertiale Bahngeschwindigkeit — roh wäre `v_eff` 1,1 %
/// daneben (≈ 9 rad Defokus).
pub fn inertial_vel(pos: Vec3d, ecef_vel: Vec3d) -> Vec3d {
    let wxr = Vec3d::new(-EARTH_OMEGA_RAD_S * pos.y, EARTH_OMEGA_RAD_S * pos.x, 0.0);
    ecef_vel + wxr
}

/// Geometrischer Doppler-Centroid in Hz: `f_DC = 2·(v·u)/λ`
/// mit Einheits-Blickvektor `u` (Plattform → Ziel).
///
/// `vel` ist die **ECEF**-Geschwindigkeit: Das Ziel ist erdfest, also
/// `f_DC = 2·v_e·u/λ` direkt (Erdrotation steckt in `v_e`).
pub fn doppler_centroid_hz(ecef_vel: Vec3d, look_unit: Vec3d) -> f64 {
    2.0 * ecef_vel.dot(look_unit) / TX_WAVELENGTH_M
}

/// Geometrisches `f_DC`-Raster je Range-Bin (Chunk-Mitte, Null-Schiel-Blick).
pub fn fdc_range_grid(pos: Vec3d, ecef_vel: Vec3d, slant_m: &[f64]) -> Vec<f64> {
    slant_m
        .iter()
        .map(|&r| doppler_centroid_hz(ecef_vel, look_unit_zero_squint(pos, ecef_vel, r)))
        .collect()
}

/// Blickvektor (Einheit) zum Szenenreferenzpunkt: rechts-schauend (S1),
/// Schiel-frei, Off-Nadir-Winkel aus Kosinussatz mit Schrägentfernung.
pub fn look_unit_zero_squint(pos: Vec3d, vel: Vec3d, slant_range: f64) -> Vec3d {
    let h = pos.norm();
    let re = (WGS84_A_M + WGS84_B_M) / 2.0;
    // Winkel am Satelliten zwischen Nadir und Ziel (Kosinussatz).
    let cos_theta = (h * h + slant_range * slant_range - re * re) / (2.0 * h * slant_range);
    let theta = cos_theta.clamp(-1.0, 1.0).acos();
    let nadir = pos.scale(-1.0 / h);
    // Rechts = V × P (prograd: Blick nach rechts, S1-Standard).
    let right = Vec3d::new(
        vel.y * pos.z - vel.z * pos.y,
        vel.z * pos.x - vel.x * pos.z,
        vel.x * pos.y - vel.y * pos.x,
    );
    let rn = right.norm();
    let right = right.scale(1.0 / rn);
    nadir.scale(theta.cos()) + right.scale(theta.sin())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn pack_f64(v: f64) -> [u16; 4] {
        let b = v.to_be_bytes();
        [
            u16::from_be_bytes([b[0], b[1]]),
            u16::from_be_bytes([b[2], b[3]]),
            u16::from_be_bytes([b[4], b[5]]),
            u16::from_be_bytes([b[6], b[7]]),
        ]
    }

    fn pack_f32(v: f32) -> [u16; 2] {
        let b = v.to_be_bytes();
        [
            u16::from_be_bytes([b[0], b[1]]),
            u16::from_be_bytes([b[2], b[3]]),
        ]
    }

    /// Erster echter S6-Block (Verifikation der Byte-Reihenfolge).
    #[test]
    fn be_decode_trifft_orbit() {
        let px = 3_956_459.7;
        let py = -5_019_370.8;
        let pz = -3_046_402.0;
        let (vx, vy, vz) = (675.51f32, -3_519.68f32, 6_689.21f32);
        let mut d = [0u16; 64];
        d[0..4].copy_from_slice(&pack_f64(px));
        d[4..8].copy_from_slice(&pack_f64(py));
        d[8..12].copy_from_slice(&pack_f64(pz));
        d[12..14].copy_from_slice(&pack_f32(vx));
        d[14..16].copy_from_slice(&pack_f32(vy));
        d[16..18].copy_from_slice(&pack_f32(vz));
        // PVT = 1474753398.0 = 87·2²⁴ + 59123·2⁸ + 30208·2⁻⁸.
        d[18] = 87;
        d[19] = 59123;
        d[20] = 30208;
        let p = decode_block(&d);
        assert!((p.pos.x - px).abs() < 1e-6);
        assert!((p.pos.y - py).abs() < 1e-6);
        assert!((p.pos.z - pz).abs() < 1e-6);
        assert!((p.pos.norm() - 7_080_128.7).abs() < 1.0);
        assert!((p.vel.x - f64::from(vx)).abs() < 1e-3);
        assert!((p.vel.norm() - 7588.8).abs() < 0.5);
        assert!((p.time_s - 1_474_753_398.0).abs() < 1e-6);
    }

    #[test]
    fn blocksuche_und_interpolation() {
        // Strom: Rauschen, dann zwei Blöcke mit Rampe auf der Position.
        let mut stream: Vec<(u8, u16)> = vec![(7, 0), (3, 0)];
        for base in [0u16, 100u16] {
            for i in 1..=64u8 {
                stream.push((i, base + u16::from(i)));
            }
        }
        stream.push((5, 0));
        let pts = collect_blocks(&stream);
        assert_eq!(pts.len(), 2);
        // Lineare Interpolation: Mitte zweier Punkte exakt.
        let a = EphemPoint {
            time_s: 10.0,
            pos: Vec3d::new(0.0, 0.0, 0.0),
            vel: Vec3d::new(0.0, 0.0, 0.0),
        };
        let b = EphemPoint {
            time_s: 20.0,
            pos: Vec3d::new(100.0, 0.0, 0.0),
            vel: Vec3d::new(10.0, 0.0, 0.0),
        };
        let pts = [a, b];
        assert_eq!(interp_pos(&pts, 15.0).x, 50.0);
        assert_eq!(interp_vel(&pts, 15.0).x, 5.0);
        assert_eq!(interp_pos(&pts, 0.0).x, 0.0); // geklemmt
        assert_eq!(interp_pos(&pts, 99.0).x, 100.0);
    }

    #[test]
    fn effektive_geschwindigkeit_plausibel() {
        // Echte S6-Geometrie: S1-Literaturwert ≈ 7,1 km/s.
        let pos = Vec3d::new(3_956_459.7, -5_019_370.8, -3_046_402.0);
        let veff = effective_velocity(7588.81, pos, 950_000.0);
        assert!((veff - 7100.0).abs() < 150.0, "v_eff = {veff}");
        // Unter Satelliten-, über Bodengeschwindigkeit.
        assert!(veff < 7588.81 && veff > 6000.0);
    }

    #[test]
    fn doppler_null_bei_kreisbahn() {
        // Synthetische Kreisbahn (P·V = 0): schiel-freie Konstruktion
        // steht senkrecht auf V → f_DC exakt ≈ 0.
        let pos = Vec3d::new(7_080_000.0, 0.0, 0.0);
        let vel = Vec3d::new(0.0, 7_590.0, 0.0);
        let u = look_unit_zero_squint(pos, vel, 950_000.0);
        assert!((u.norm() - 1.0).abs() < 1e-12);
        let fdc = doppler_centroid_hz(vel, u);
        assert!(fdc.abs() < 1e-6, "f_DC = {fdc}");
    }

    #[test]
    fn doppler_realbahn_klein_gegen_prf() {
        // Echte S6-Bahn: Radialkomponente (~5 m/s) erzeugt ≈ 150 Hz —
        // klein gegen PRF 1663 Hz (Null-Doppler-Steuerung bestätigt).
        let pos = Vec3d::new(3_956_459.7, -5_019_370.8, -3_046_402.0);
        let vel = Vec3d::new(675.51, -3_519.68, 6_689.21);
        let u = look_unit_zero_squint(pos, vel, 950_000.0);
        let fdc = doppler_centroid_hz(vel, u);
        assert!((fdc - 154.0).abs() < 30.0, "f_DC = {fdc}");
        // Off-Nadir-Winkel ≈ 39° für S6 (Plausibilität).
        let nadir = pos.scale(-1.0 / pos.norm());
        let theta = u.dot(nadir).acos().to_degrees();
        assert!((theta - 39.0).abs() < 2.0, "theta = {theta}");
    }
}
