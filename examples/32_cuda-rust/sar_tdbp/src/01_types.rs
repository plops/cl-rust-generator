//! Grundtypen: Komplexe Zahlen, Vektoren, Radar- und Szenenparameter.
//!
//! `Complex32`/`Vec3` sind `repr(C)` + `DeviceCopy` und werden unverändert
//! zwischen Host und GPU-Kernel geteilt. Alle Methoden sind absichtlich
//! `std`-frei (nur Kern-Arithmetik + `sin`/`cos`/`sqrt`, die `cuda-oxide`
//! auf CUDA-libdevice senkt), damit sie auch im `#[kernel]` laufen.

use cuda_core::DeviceCopy;
use std::ops::{Add, Mul};

/// Lichtgeschwindigkeit in m/s.
pub const SPEED_OF_LIGHT: f32 = 299_792_458.0;

/// Komplexe Zahl (`f32`), GPU-kompatibel.
#[repr(C)]
#[derive(Clone, Copy, Debug, Default, PartialEq, DeviceCopy)]
pub struct Complex32 {
    pub re: f32,
    pub im: f32,
}

impl Complex32 {
    pub const fn new(re: f32, im: f32) -> Self {
        Self { re, im }
    }

    pub fn zero() -> Self {
        Self::new(0.0, 0.0)
    }

    /// Betrag-Quadrat (meidet die Wurzel beim Peak-Vergleich).
    pub fn norm_sqr(self) -> f32 {
        self.re * self.re + self.im * self.im
    }

    pub fn norm(self) -> f32 {
        self.norm_sqr().sqrt()
    }

    /// `mag * e^{j·phase}` — Matched-Filter-Drehung im TDBP-Kernel.
    pub fn from_polar(mag: f32, phase: f32) -> Self {
        Self::new(mag * phase.cos(), mag * phase.sin())
    }
}

impl Add for Complex32 {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        Self::new(self.re + rhs.re, self.im + rhs.im)
    }
}

impl Mul for Complex32 {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self {
        Self::new(
            self.re * rhs.re - self.im * rhs.im,
            self.re * rhs.im + self.im * rhs.re,
        )
    }
}

/// 3D-Position in Metern (Boden-Koordinatensystem), GPU-kompatibel.
#[repr(C)]
#[derive(Clone, Copy, Debug, Default, PartialEq, DeviceCopy)]
pub struct Vec3 {
    pub x: f32,
    pub y: f32,
    pub z: f32,
}

impl Vec3 {
    pub const fn new(x: f32, y: f32, z: f32) -> Self {
        Self { x, y, z }
    }

    /// Euklidischer Abstand — die TDBP-Kernoperation (`R = |r_p − r|`).
    pub fn dist(self, other: Self) -> f32 {
        let dx = self.x - other.x;
        let dy = self.y - other.y;
        let dz = self.z - other.z;
        (dx * dx + dy * dy + dz * dz).sqrt()
    }
}

/// Radar-Kennwerte (reine Host-Struktur).
#[derive(Clone, Copy, Debug)]
pub struct RadarParams {
    /// Mittenfrequenz in Hz (z. B. X-Band: 10 GHz).
    pub f0: f32,
    /// Bandbreite in Hz (bestimmt die Range-Auflösung).
    pub bandwidth: f32,
}

impl RadarParams {
    pub fn x_band() -> Self {
        Self {
            f0: 10.0e9,
            bandwidth: 300.0e6,
        }
    }

    /// Wellenlänge λ = c / f0 (hier: 3 cm).
    pub fn lambda(self) -> f32 {
        SPEED_OF_LIGHT / self.f0
    }

    /// Theoretische Range-Auflösung ΔR = c / (2B) (hier: 0,5 m).
    pub fn range_resolution(self) -> f32 {
        SPEED_OF_LIGHT / (2.0 * self.bandwidth)
    }
}

/// Szenen- und Aufnahmegeometrie (reine Host-Struktur).
#[derive(Clone, Copy, Debug)]
pub struct SceneGeometry {
    pub width: u32,
    pub height: u32,
    /// Szenen-Ursprung (untere linke Bildecke) in Metern.
    pub x0: f32,
    pub y0: f32,
    /// Pixelabstand in Metern.
    pub dx: f32,
    pub dy: f32,
    /// Flughöhe der Plattform in Metern.
    pub platform_height: f32,
    /// Länge der synthetischen Apertur in Metern (Flugstrecke).
    pub aperture_len: f32,
    pub num_pulses: u32,
}

impl SceneGeometry {
    /// Standard-Szene: 40 × 40 m, Plattform auf 100 m Höhe, Seitenblick:
    /// die Szenenmitte liegt 60 m neben der Flugbahn. (Reines Nadir — Szene
    /// direkt unter der Bahn — hätte keine Boden-Range-Auflösung, da die
    /// Sichtlinie dort senkrecht zur y-Achse steht.)
    pub fn default_scene(width: u32, height: u32, num_pulses: u32) -> Self {
        let extent = 40.0;
        Self {
            width,
            height,
            x0: -extent / 2.0,
            y0: 60.0 - extent / 2.0,
            dx: extent / width as f32,
            dy: extent / height as f32,
            platform_height: 100.0,
            aperture_len: 40.0,
            num_pulses,
        }
    }

    /// Szenenmitte am Boden (z = 0).
    pub fn center(self) -> Vec3 {
        Vec3::new(
            self.x0 + self.width as f32 * self.dx / 2.0,
            self.y0 + self.height as f32 * self.dy / 2.0,
            0.0,
        )
    }

    /// Bodenkoordinate der Pixelmitte (z = 0).
    pub fn pixel_pos(self, px: u32, py: u32) -> Vec3 {
        Vec3::new(
            self.x0 + (px as f32 + 0.5) * self.dx,
            self.y0 + (py as f32 + 0.5) * self.dy,
            0.0,
        )
    }

    /// Antennenposition von Puls `p`: Gerade entlang x auf Höhe h.
    pub fn pulse_pos(self, p: u32) -> Vec3 {
        let x = if self.num_pulses <= 1 {
            0.0
        } else {
            -self.aperture_len / 2.0 + self.aperture_len * p as f32 / (self.num_pulses - 1) as f32
        };
        Vec3::new(x, 0.0, self.platform_height)
    }

    /// Theoretische Azimuth-Auflösung δx ≈ λ·R / (2·L),
    /// R = Schrägentfernung zur Szenenmitte.
    pub fn azimuth_resolution(self, radar: RadarParams) -> f32 {
        let r = Vec3::new(0.0, 0.0, self.platform_height).dist(self.center());
        radar.lambda() * r / (2.0 * self.aperture_len)
    }

    /// Theoretische Boden-Range-Auflösung Δy ≈ ΔR·R/y_c (Seitenblick).
    pub fn ground_range_resolution(self, radar: RadarParams) -> f32 {
        let c = self.center();
        let r = Vec3::new(0.0, 0.0, self.platform_height).dist(c);
        radar.range_resolution() * r / c.y
    }
}

/// Betrag in dB, auf Dynamik `[−dyn_range_db, 0]` begrenzt und auf
/// `[0, 1]` normiert: `20·log10(mag/max)`.
pub fn mag_to_unit_db(mag: f32, max: f32, dyn_range_db: f32) -> f32 {
    if max.is_nan() || mag.is_nan() || max <= 0.0 || mag <= 0.0 {
        return 0.0;
    }
    let db = 20.0 * (mag / max).log10();
    ((db + dyn_range_db) / dyn_range_db).clamp(0.0, 1.0)
}

/// Turbo-Colormap (Polynom-Näherung nach Google Research, „Turbo, An
/// Improved Rainbow Colormap“): `t ∈ [0,1]` → `(r, g, b) ∈ [0,1]³`.
pub fn turbo(t: f32) -> (f32, f32, f32) {
    let t = t.clamp(0.0, 1.0);
    let t2 = t * t;
    let t3 = t2 * t;
    let t4 = t3 * t;
    let t5 = t4 * t;
    let r = 0.13572138 + 4.615_392_7 * t - 42.660_324 * t2 + 132.131_09 * t3 - 152.942_4 * t4
        + 59.286_38 * t5;
    let g = 0.09140261 + 2.194_188_4 * t + 4.842_966_6 * t2 - 14.185_034 * t3
        + 4.277_298_5 * t4
        + 2.829_566 * t5;
    let b = 0.106_673_3 + 12.641_946 * t - 60.582_047 * t2 + 110.362_77 * t3 - 89.903_11 * t4
        + 27.348_25 * t5;
    (r.clamp(0.0, 1.0), g.clamp(0.0, 1.0), b.clamp(0.0, 1.0))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn complex_arithmetik() {
        let a = Complex32::new(1.0, 2.0);
        let b = Complex32::new(3.0, 4.0);
        assert_eq!(a + b, Complex32::new(4.0, 6.0));
        // (1+2j)(3+4j) = -5+10j
        assert_eq!(a * b, Complex32::new(-5.0, 10.0));
        assert_eq!(a.norm_sqr(), 5.0);
        let p = Complex32::from_polar(2.0, 0.0);
        assert!((p.re - 2.0).abs() < 1e-6 && p.im.abs() < 1e-6);
        let q = Complex32::from_polar(1.0, std::f32::consts::FRAC_PI_2);
        assert!(q.re.abs() < 1e-6 && (q.im - 1.0).abs() < 1e-6);
    }

    #[test]
    fn vec3_abstand() {
        let a = Vec3::new(0.0, 0.0, 100.0);
        let b = Vec3::new(0.0, 0.0, 0.0);
        assert_eq!(a.dist(b), 100.0);
        let c = Vec3::new(3.0, 4.0, 0.0);
        assert!((c.dist(Vec3::new(0.0, 0.0, 0.0)) - 5.0).abs() < 1e-6);
    }

    #[test]
    fn radar_kennwerte() {
        let r = RadarParams::x_band();
        assert!((r.lambda() - 0.03).abs() < 1e-4);
        assert!((r.range_resolution() - 0.5).abs() < 1e-3);
    }

    #[test]
    fn geometrie_raster() {
        let g = SceneGeometry::default_scene(256, 256, 256);
        // Azimuth zentriert um 0, Range 40..80 m (Seitenblick, Mitte 60 m).
        let lo = g.pixel_pos(0, 0);
        assert!(lo.x < -19.0 && (lo.y - 40.0).abs() < 0.2);
        let hi = g.pixel_pos(255, 255);
        assert!(hi.x > 19.0 && (hi.y - 80.0).abs() < 0.2);
        let mid = g.pixel_pos(128, 128);
        assert!(mid.x.abs() < 0.2 && (mid.y - 60.0).abs() < 0.2);
        // Apertur symmetrisch um 0 auf Höhe h.
        let p0 = g.pulse_pos(0);
        let pn = g.pulse_pos(255);
        assert!((p0.x + 20.0).abs() < 1e-4 && (pn.x - 20.0).abs() < 1e-4);
        assert_eq!((p0.y, p0.z), (0.0, 100.0));
        // R = √(60²+100²) ≈ 116,62 m.
        // Azimuth: 0,03·116,62/80 ≈ 4,37 cm; Boden-Range: 0,5·116,62/60 ≈ 0,97 m.
        let radar = RadarParams::x_band();
        assert!((g.azimuth_resolution(radar) - 0.0437).abs() < 1e-4);
        assert!((g.ground_range_resolution(radar) - 0.9718).abs() < 1e-3);
    }

    #[test]
    fn db_skalierung() {
        assert_eq!(mag_to_unit_db(1.0, 1.0, 30.0), 1.0);
        assert_eq!(mag_to_unit_db(0.0, 1.0, 30.0), 0.0);
        assert_eq!(mag_to_unit_db(1.0, 0.0, 30.0), 0.0);
        // −30 dB → 0, −15 dB → 0,5.
        let lo = 10.0f32.powf(-30.0 / 20.0);
        assert!(mag_to_unit_db(lo, 1.0, 30.0).abs() < 1e-5);
        let mid = 10.0f32.powf(-15.0 / 20.0);
        assert!((mag_to_unit_db(mid, 1.0, 30.0) - 0.5).abs() < 1e-5);
    }

    #[test]
    fn turbo_endpunkte() {
        let (r0, g0, b0) = turbo(0.0);
        let (r1, g1, b1) = turbo(1.0);
        // Fast schwarz → dunkelrot, Mitte grünlich.
        assert!(r0 < 0.25 && g0 < 0.25 && b0 < 0.25);
        assert!(r1 > 0.4 && g1 < 0.3 && b1 < 0.3);
        let (rm, gm, bm) = turbo(0.5);
        assert!(gm > rm && gm > bm);
        for i in 0..=100 {
            let (r, g, b) = turbo(i as f32 / 100.0);
            assert!((0.0..=1.0).contains(&r));
            assert!((0.0..=1.0).contains(&g));
            assert!((0.0..=1.0).contains(&b));
        }
    }
}
