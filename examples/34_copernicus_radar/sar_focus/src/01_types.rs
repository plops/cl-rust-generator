//! Grundtypen: GPU-kompatible Komplexe Zahlen/Vektoren, Konstanten, Fehler.
//!
//! `Complex32` ist `repr(C)` + `DeviceCopy` und wird unverändert zwischen
//! Host und GPU-Kernel geteilt (Muster aus `sar_tdbp`, dortige Lehre Nr. 5:
//! kein Fremd-Crate auf dem Device). Für CPU-FFT-Puffer (`rustfft`) wird nach
//! `num_complex::Complex32` konvertiert.

use cuda_core::DeviceCopy;
use std::ops::{Add, Mul, Sub};

/// Referenzfrequenz in Hz (sentinel1decoder `F_REF`): skaliert PRI, SWST,
/// SWL, TXPL, TXPSF, TXPRR von Rohzählwerten in Sekunden/Hertz.
pub const F_REF_HZ: f64 = 37_534_722.24;
/// Lichtgeschwindigkeit in m/s.
pub const SPEED_OF_LIGHT: f64 = 299_792_458.0;
/// Sentinel-1 Sendefrequenz in Hz (C-Band).
pub const TX_FREQ_HZ: f64 = 5.405e9;
/// Sendewellenlänge in m (c/f ≈ 5,55 cm).
pub const TX_WAVELENGTH_M: f64 = SPEED_OF_LIGHT / TX_FREQ_HZ;
/// WGS84 große Halbachse in m.
pub const WGS84_A_M: f64 = 6_378_137.0;
/// WGS84 kleine Halbachse in m.
pub const WGS84_B_M: f64 = 6_356_752.314_2;

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

    pub fn conj(self) -> Self {
        Self::new(self.re, -self.im)
    }

    pub fn scale(self, s: f32) -> Self {
        Self::new(self.re * s, self.im * s)
    }

    /// Betrag-Quadrat (meidet die Wurzel beim Peak-Vergleich).
    pub fn norm_sqr(self) -> f32 {
        self.re * self.re + self.im * self.im
    }

    pub fn norm(self) -> f32 {
        self.norm_sqr().sqrt()
    }

    /// `mag * e^{j·phase}`.
    pub fn from_polar(mag: f32, phase: f32) -> Self {
        Self::new(mag * phase.cos(), mag * phase.sin())
    }

    pub fn to_num(self) -> num_complex::Complex32 {
        num_complex::Complex32::new(self.re, self.im)
    }

    pub fn from_num(c: num_complex::Complex32) -> Self {
        Self::new(c.re, c.im)
    }
}

impl Add for Complex32 {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        Self::new(self.re + rhs.re, self.im + rhs.im)
    }
}

impl Sub for Complex32 {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self {
        Self::new(self.re - rhs.re, self.im - rhs.im)
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

/// 3D-Position in Metern (`f32`), GPU-kompatibel (Pixelgitter, Nahbereich).
#[repr(C)]
#[derive(Clone, Copy, Debug, Default, PartialEq, DeviceCopy)]
pub struct Vec3 {
    pub x: f32,
    pub y: f32,
    pub z: f32,
}

/// 3D-Position in Metern (`f64`), GPU-kompatibel.
///
/// Plattformpositionen (Orbitradius ~7·10⁶ m, Schrägentfernung ~9·10⁵ m)
/// brauchen `f64`: `f32` hätte bei 900 km bereits 6 cm Quantisierung —
/// das wären ~7 rad Phasenfehler im TDBP-Matched-Filter.
#[repr(C)]
#[derive(Clone, Copy, Debug, Default, PartialEq, DeviceCopy)]
pub struct Vec3d {
    pub x: f64,
    pub y: f64,
    pub z: f64,
}

impl Vec3d {
    pub const fn new(x: f64, y: f64, z: f64) -> Self {
        Self { x, y, z }
    }

    pub fn zero() -> Self {
        Self::new(0.0, 0.0, 0.0)
    }

    pub fn norm(self) -> f64 {
        (self.x * self.x + self.y * self.y + self.z * self.z).sqrt()
    }

    pub fn dist(self, other: Self) -> f64 {
        let dx = self.x - other.x;
        let dy = self.y - other.y;
        let dz = self.z - other.z;
        (dx * dx + dy * dy + dz * dz).sqrt()
    }

    pub fn scale(self, s: f64) -> Self {
        Self::new(self.x * s, self.y * s, self.z * s)
    }

    pub fn dot(self, other: Self) -> f64 {
        self.x * other.x + self.y * other.y + self.z * other.z
    }
}

impl std::ops::Add for Vec3d {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        Self::new(self.x + other.x, self.y + other.y, self.z + other.z)
    }
}

impl std::ops::Sub for Vec3d {
    type Output = Self;
    fn sub(self, other: Self) -> Self {
        Self::new(self.x - other.x, self.y - other.y, self.z - other.z)
    }
}

/// Crate-weiter Fehler (Meldungen als Text, wie `sar_tdbp::PipelineError`).
#[derive(Debug)]
pub struct Error(pub String);

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "sar_focus: {}", self.0)
    }
}

impl std::error::Error for Error {}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error(e.to_string())
    }
}

impl From<copernicus_radar::Error> for Error {
    fn from(e: copernicus_radar::Error) -> Self {
        Error(e.to_string())
    }
}

pub fn err<E: std::fmt::Display>(e: E) -> Error {
    Error(e.to_string())
}

/// Betrag in dB auf `[0, 1]` normiert: `20·log10(mag/max)`, begrenzt auf
/// `[-dyn_range_db, 0]`.
pub fn mag_to_unit_db(mag: f32, max: f32, dyn_range_db: f32) -> f32 {
    if max.is_nan() || mag.is_nan() || max <= 0.0 || mag <= 0.0 {
        return 0.0;
    }
    let db = 20.0 * (mag / max).log10();
    ((db + dyn_range_db) / dyn_range_db).clamp(0.0, 1.0)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn komplexe_arithmetik() {
        let a = Complex32::new(1.0, 2.0);
        let b = Complex32::new(3.0, 4.0);
        assert_eq!(a + b, Complex32::new(4.0, 6.0));
        assert_eq!(a - b, Complex32::new(-2.0, -2.0));
        assert_eq!(a * b, Complex32::new(-5.0, 10.0));
        assert_eq!(a.conj(), Complex32::new(1.0, -2.0));
        assert_eq!(a.scale(2.0), Complex32::new(2.0, 4.0));
        assert_eq!(a.norm_sqr(), 5.0);
        // Roundtrip zum rustfft-Typ.
        assert_eq!(Complex32::from_num(a.to_num()), a);
    }

    #[test]
    fn vec3d_distanz() {
        let a = Vec3d::new(3.0, 4.0, 0.0);
        assert!((a.norm() - 5.0).abs() < 1e-12);
        // Orbitmaßstab: 7·10⁶ m braucht f64 (f32-Epsilon wäre ~0,5 m).
        let p = Vec3d::new(3_956_459.7, -5_019_370.8, -3_046_402.0);
        assert!((p.norm() - 7_080_128.7).abs() < 1.0);
        assert!((p.dist(p) - 0.0).abs() < 1e-9);
    }

    #[test]
    fn konstanten() {
        assert!((TX_WAVELENGTH_M - 0.055_465_7).abs() < 1e-7);
        assert_eq!(F_REF_HZ, 37_534_722.24);
    }
}
