//! Gemeinsame Grundtypen, physikalische Konstanten und Datenmodell (`BurstBlock`, `SwathData`).
//!
//! `Complex32` und `Vec3d` sind `#[repr(C)]` + `cuda_core::DeviceCopy`, sodass
//! derselbe Typ ohne Konvertierung auf Host und GPU verwendet wird.

use cuda_core::DeviceCopy;
use std::ops::{Add, Mul, Sub};

/// Vakuum-Lichtgeschwindigkeit $c$ [m/s] (2019 SI).
pub const C_LIGHT: f64 = 299_792_458.0;
/// Sentinel-1 Instrument-Referenzfrequenz $f_{\text{ref}}$ [Hz] (ESA Level-0 Spezifikation).
pub const F_REF: f64 = 37_534_722.24;
/// Sentinel-1 C-Band Trägerfrequenz $f_c$ [Hz].
pub const F_CARRIER: f64 = 5.405e9;
/// Trägerwellenlänge $\lambda = c / f_c$ [m] ($\approx 5{,}546576\text{ cm}$).
pub const LAMBDA: f64 = C_LIGHT / F_CARRIER;

/// Komplexe `f32`-Zahl in `repr(C)`-Layout (kompatibel mit `cufftComplex` und `.cf`).
#[repr(C)]
#[derive(Clone, Copy, Debug, Default, PartialEq, DeviceCopy)]
pub struct Complex32 {
    pub re: f32,
    pub im: f32,
}

impl Complex32 {
    pub const ZERO: Self = Self { re: 0.0, im: 0.0 };

    #[inline]
    pub const fn new(re: f32, im: f32) -> Self {
        Self { re, im }
    }

    /// Einheitszeiger $\exp(j\phi) = \cos\phi + j\sin\phi$ (Rechnung in `f64`).
    #[inline]
    pub fn from_polar_unit(phase_rad: f64) -> Self {
        let (s, c) = phase_rad.sin_cos();
        Self {
            re: c as f32,
            im: s as f32,
        }
    }

    #[inline]
    pub const fn conj(self) -> Self {
        Self {
            re: self.re,
            im: -self.im,
        }
    }

    #[inline]
    pub const fn norm_sqr(self) -> f32 {
        self.re * self.re + self.im * self.im
    }

    #[inline]
    pub fn norm(self) -> f32 {
        self.norm_sqr().sqrt()
    }

    #[inline]
    pub const fn scale(self, s: f32) -> Self {
        Self {
            re: self.re * s,
            im: self.im * s,
        }
    }

    #[inline]
    pub fn arg(self) -> f32 {
        self.im.atan2(self.re)
    }
}

impl std::ops::AddAssign for Complex32 {
    #[inline]
    fn add_assign(&mut self, rhs: Self) {
        self.re += rhs.re;
        self.im += rhs.im;
    }
}

impl Mul<f32> for Complex32 {
    type Output = Self;
    #[inline]
    fn mul(self, rhs: f32) -> Self {
        self.scale(rhs)
    }
}

impl Add for Complex32 {
    type Output = Self;
    #[inline]
    fn add(self, rhs: Self) -> Self {
        Self {
            re: self.re + rhs.re,
            im: self.im + rhs.im,
        }
    }
}

impl Sub for Complex32 {
    type Output = Self;
    #[inline]
    fn sub(self, rhs: Self) -> Self {
        Self {
            re: self.re - rhs.re,
            im: self.im - rhs.im,
        }
    }
}

impl Mul for Complex32 {
    type Output = Self;
    #[inline]
    fn mul(self, rhs: Self) -> Self {
        Self {
            re: self.re * rhs.re - self.im * rhs.im,
            im: self.re * rhs.im + self.im * rhs.re,
        }
    }
}

impl From<num_complex::Complex<f32>> for Complex32 {
    #[inline]
    fn from(c: num_complex::Complex<f32>) -> Self {
        Self { re: c.re, im: c.im }
    }
}

impl From<Complex32> for num_complex::Complex<f32> {
    #[inline]
    fn from(c: Complex32) -> Self {
        Self::new(c.re, c.im)
    }
}

/// 3D-Vektor in `f64` (ECEF-Positionen [m] und Geschwindigkeiten [m/s]).
#[repr(C)]
#[derive(Clone, Copy, Debug, Default, PartialEq, DeviceCopy)]
pub struct Vec3d {
    pub x: f64,
    pub y: f64,
    pub z: f64,
}

impl Vec3d {
    #[inline]
    pub const fn new(x: f64, y: f64, z: f64) -> Self {
        Self { x, y, z }
    }

    #[inline]
    pub fn dot(self, other: Self) -> f64 {
        self.x * other.x + self.y * other.y + self.z * other.z
    }

    #[inline]
    pub fn cross(self, other: Self) -> Self {
        Self {
            x: self.y * other.z - self.z * other.y,
            y: self.z * other.x - self.x * other.z,
            z: self.x * other.y - self.y * other.x,
        }
    }

    #[inline]
    pub fn norm_sqr(self) -> f64 {
        self.dot(self)
    }

    #[inline]
    pub fn norm(self) -> f64 {
        self.norm_sqr().sqrt()
    }

    #[inline]
    pub fn scale(self, s: f64) -> Self {
        Self::new(self.x * s, self.y * s, self.z * s)
    }
}

impl Mul<f64> for Vec3d {
    type Output = Self;
    #[inline]
    fn mul(self, rhs: f64) -> Self {
        self.scale(rhs)
    }
}

impl Add for Vec3d {
    type Output = Self;
    #[inline]
    fn add(self, other: Self) -> Self {
        Self::new(self.x + other.x, self.y + other.y, self.z + other.z)
    }
}

impl Sub for Vec3d {
    type Output = Self;
    #[inline]
    fn sub(self, other: Self) -> Self {
        Self::new(self.x - other.x, self.y - other.y, self.z - other.z)
    }
}

/// Aufnahmemodus eines Sentinel-1 Level-0 Produkts.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SarMode {
    /// Stripmap (SM S1..S6, `swath_number` 1..=6).
    Stripmap,
    /// Interferometric Wide Swath TOPS (IW1..IW3, `swath_number` 10..=12).
    Iw,
    /// Extra Wide Swath TOPS (EW1..EW5, `swath_number` 20..=24).
    Ew,
}

impl SarMode {
    pub fn as_str(self) -> &'static str {
        match self {
            SarMode::Stripmap => "SM",
            SarMode::Iw => "IW",
            SarMode::Ew => "EW",
        }
    }

    pub fn is_tops(self) -> bool {
        matches!(self, SarMode::Iw | SarMode::Ew)
    }
}

/// Abgeleitete Radar-Parameter eines konsistenten Blocks/Bursts (SI-Einheiten).
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct RadarParams {
    pub swath_number: u8,
    pub elevation_beam: u8,
    pub rgdec: u8,
    pub rank: u8,
    pub fs_hz: f64,
    pub prf_hz: f64,
    pub pri_s: f64,
    pub swst_s: f64,
    pub swl_s: f64,
    pub tx_ramp_rate_hz_s: f64,
    pub tx_start_freq_hz: f64,
    pub tx_pulse_length_s: f64,
    pub fast_time_start_s: f64,
    pub r0_m: f64,
    pub dr_m: f64,
    pub chirp_samples: usize,
}

impl RadarParams {
    #[inline]
    pub fn slant_range(&self, j: usize) -> f64 {
        self.r0_m + (j as f64) * self.dr_m
    }
}

/// Einheitlicher Echodaten-Block: repräsentiert entweder einen kompletten Stripmap-Abschnitt
/// oder einen einzelnen TOPS-Burst eines Sub-Swaths (`[n_az × n_rg]` zeilenweise).
#[derive(Clone, Debug)]
pub struct BurstBlock {
    pub burst_index: usize,
    pub swath_number: u8,
    pub params: RadarParams,
    pub n_az: usize,
    pub n_rg: usize,
    /// Komplexe Roh-Echos (`n_az * n_rg`, SWST-ausgerichtet und PRI-lückengefüllt).
    pub raw: Vec<Complex32>,
    /// Geglättete Sendezeitpunkte je Azimut-Zeile [s].
    pub pulse_times: Vec<f64>,
    /// Azimuth-Beam-Address je Zeile (für TOPS-Diagnose).
    pub az_beam_addr: Vec<u16>,
    /// Effektive Geschwindigkeit $V_{\text{eff}}(r)$ je Range-Bin [m/s].
    pub veff: Vec<f64>,
    /// Plattform-Positionen in ECEF je Azimut-Zeile [m].
    pub pos_ecef: Vec<Vec3d>,
    /// Plattform-Geschwindigkeiten in ECEF je Azimut-Zeile [m/s].
    pub vel_ecef: Vec<Vec3d>,
}

/// Alle Bursts eines Sub-Swaths (bei Stripmap: 1 Block; bei IW/EW: 12..23 Bursts).
#[derive(Clone, Debug)]
pub struct SwathData {
    pub swath_number: u8,
    pub swath_name: String,
    pub params: RadarParams,
    pub bursts: Vec<BurstBlock>,
}

/// Crate-weiter Fehlertyp.
#[derive(Debug)]
pub struct Error(pub String);

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "sar_source2: {}", self.0)
    }
}

impl std::error::Error for Error {}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error(e.to_string())
    }
}

pub fn err<E: std::fmt::Display>(e: E) -> Error {
    Error(e.to_string())
}

/// Betrag in dB auf `[0, 1]` normiert: `20·log10(mag/max)`, begrenzt auf `[-dyn_range_db, 0]`.
#[inline]
pub fn mag_to_unit_db(mag: f32, max: f32, dyn_range_db: f32) -> f32 {
    if max.is_nan() || mag.is_nan() || max <= 0.0 || mag <= 0.0 {
        return 0.0;
    }
    let db = 20.0 * (mag / max).log10();
    ((db + dyn_range_db) / dyn_range_db).clamp(0.0, 1.0)
}
