//! GPU-Kernel (`cuda-oxide`) + Geräte-Kontext.
//!
//! Alle elementweisen Kernel arbeiten out-of-place (`src` lesen, `dst`
//! schreiben, danach Puffer tauschen): `DisjointSlice` erlaubt nur
//! Schreiben, und dasselbe Device-Buffer-Handle kann hostseitig nicht
//! gleichzeitig als `&` und `&mut` ausgeliehen werden. 1D-Grids mit
//! Block 256 nach `sar_tdbp`-Muster.

use crate::types::{Complex32, Error, Vec3d, err};
use cuda_core::{CudaContext, CudaStream, DeviceBuffer, LaunchConfig1D};
use cuda_device::{DisjointSlice, kernel, launch_bounds, launch_contract, thread};
use cuda_host::cuda_module;
use std::sync::Arc;

/// Geräte-Kontext: Primary Context, Stream und geladenes Kernel-Modul.
pub struct GpuContext {
    pub ctx: Arc<CudaContext>,
    pub stream: Arc<CudaStream>,
    pub module: kernels::LoadedModule,
}

impl GpuContext {
    pub fn new() -> Result<Self, Error> {
        let ctx = CudaContext::new(0).map_err(err)?;
        let stream = ctx.default_stream();
        // SAFETY: `kernels` ist das eigene eingebettete Modul dieser Crate.
        let module = unsafe { kernels::load(&ctx) }.map_err(err)?;
        Ok(Self {
            ctx,
            stream,
            module,
        })
    }

    pub fn upload(&self, host: &[Complex32]) -> Result<DeviceBuffer<Complex32>, Error> {
        DeviceBuffer::from_host(&self.stream, host).map_err(err)
    }

    pub fn upload_vec3d(&self, host: &[Vec3d]) -> Result<DeviceBuffer<Vec3d>, Error> {
        DeviceBuffer::from_host(&self.stream, host).map_err(err)
    }

    pub fn alloc(&self, n: usize) -> Result<DeviceBuffer<Complex32>, Error> {
        DeviceBuffer::<Complex32>::zeroed(&self.stream, n).map_err(err)
    }

    pub fn download(&self, dev: &DeviceBuffer<Complex32>) -> Result<Vec<Complex32>, Error> {
        dev.to_host_vec(&self.stream).map_err(err)
    }

    pub fn synchronize(&self) -> Result<(), Error> {
        self.stream.synchronize().map_err(err)
    }

    fn blocks(n: usize) -> u32 {
        n.div_ceil(256) as u32
    }

    /// `dst[i] = src[i]·filt[i]` (2D-Filter, RCMC/Azimut).
    pub fn launch_cmul_2d(
        &self,
        src: &DeviceBuffer<Complex32>,
        filt: &DeviceBuffer<Complex32>,
        dst: &mut DeviceBuffer<Complex32>,
        n: usize,
    ) -> Result<(), Error> {
        let prep = self
            .module
            .prepare_cmul_2d(LaunchConfig1D::new(Self::blocks(n), 256, 0))
            .map_err(err)?;
        self.module
            .cmul_2d(&self.stream, &prep, src, filt, dst, n as u32)
            .map_err(err)
    }

    /// `dst[a·nr+r] = src[a·nr+r]·row[r]` (Range-Zeilenfilter).
    pub fn launch_cmul_row(
        &self,
        src: &DeviceBuffer<Complex32>,
        row: &DeviceBuffer<Complex32>,
        dst: &mut DeviceBuffer<Complex32>,
        nrange: usize,
        nrows: usize,
    ) -> Result<(), Error> {
        let prep = self
            .module
            .prepare_cmul_row(LaunchConfig1D::new(Self::blocks(nrange * nrows), 256, 0))
            .map_err(err)?;
        self.module
            .cmul_row(
                &self.stream,
                &prep,
                src,
                row,
                dst,
                nrange as u32,
                nrows as u32,
            )
            .map_err(err)
    }

    /// Zeilen um `shift` nach rechts rotieren (Range-Rückversatz).
    pub fn launch_rotate_rows(
        &self,
        src: &DeviceBuffer<Complex32>,
        dst: &mut DeviceBuffer<Complex32>,
        nrange: usize,
        nrows: usize,
        shift: usize,
    ) -> Result<(), Error> {
        let prep = self
            .module
            .prepare_rotate_rows(LaunchConfig1D::new(Self::blocks(nrange * nrows), 256, 0))
            .map_err(err)?;
        self.module
            .rotate_rows(
                &self.stream,
                &prep,
                src,
                dst,
                nrange as u32,
                nrows as u32,
                shift as u32,
            )
            .map_err(err)
    }

    /// Spalten um `up` nach oben rotieren (FFT-Shifts).
    pub fn launch_shift_cols(
        &self,
        src: &DeviceBuffer<Complex32>,
        dst: &mut DeviceBuffer<Complex32>,
        naz: usize,
        nrange: usize,
        up: usize,
    ) -> Result<(), Error> {
        let prep = self
            .module
            .prepare_shift_cols(LaunchConfig1D::new(Self::blocks(naz * nrange), 256, 0))
            .map_err(err)?;
        self.module
            .shift_cols(
                &self.stream,
                &prep,
                src,
                dst,
                naz as u32,
                nrange as u32,
                up as u32,
            )
            .map_err(err)
    }

    /// TDBP-Kernel starten (ein Thread pro Pixel, 1D-Grid).
    /// Ausgabe `[naz × nrange]`, Azimut = langsame Achse (wie Host).
    #[allow(clippy::too_many_arguments)]
    pub fn launch_tdbp(
        &self,
        raw: &DeviceBuffer<Complex32>,
        plat: &DeviceBuffer<Vec3d>,
        out: &mut DeviceBuffer<Complex32>,
        naz: u32,
        nrange: u32,
        num_pulses: u32,
        num_samples: u32,
        pulse_limit: u32,
        lambda: f64,
        c: f64,
        dt: f64,
        t0: f64,
        x0: f64,
        y0: f64,
        dx: f64,
        dy: f64,
    ) -> Result<(), Error> {
        let pixels = naz
            .checked_mul(nrange)
            .ok_or(Error("Bild zu groß".into()))?;
        let prep = self
            .module
            .prepare_tdbp(LaunchConfig1D::new(pixels.div_ceil(256), 256, 0))
            .map_err(err)?;
        self.module
            .tdbp(
                &self.stream,
                &prep,
                raw,
                plat,
                out,
                naz,
                nrange,
                num_pulses,
                num_samples,
                pulse_limit,
                lambda,
                c,
                dt,
                t0,
                x0,
                y0,
                dx,
                dy,
            )
            .map_err(err)
    }
}

#[cuda_module]
pub mod kernels {
    use super::*;
    use crate::types::{Complex32, Vec3d};

    /// Elementweise komplexe Multiplikation mit 2D-Filter.
    #[kernel]
    #[launch_bounds(256)]
    #[launch_contract(domain = 1, block = (256, 1, 1))]
    pub fn cmul_2d(
        src: &[Complex32],
        filt: &[Complex32],
        mut dst: DisjointSlice<Complex32>,
        n: u32,
    ) {
        let idx = thread::index_1d();
        let i = idx.get() as u32;
        if i >= n {
            return;
        }
        if let Some(o) = dst.get_mut(idx) {
            *o = src[i as usize] * filt[i as usize];
        }
    }

    /// Multiplikation mit Zeilenfilter (`row`, über Zeilen gebroadcastet).
    #[kernel]
    #[launch_bounds(256)]
    #[launch_contract(domain = 1, block = (256, 1, 1))]
    pub fn cmul_row(
        src: &[Complex32],
        row: &[Complex32],
        mut dst: DisjointSlice<Complex32>,
        nrange: u32,
        nrows: u32,
    ) {
        let idx = thread::index_1d();
        let i = idx.get() as u32;
        if i >= nrange * nrows {
            return;
        }
        if let Some(o) = dst.get_mut(idx) {
            *o = src[i as usize] * row[(i % nrange) as usize];
        }
    }

    /// Zeilenrotation nach rechts (Korrelations-Rückversatz auf dem Device).
    #[kernel]
    #[launch_bounds(256)]
    #[launch_contract(domain = 1, block = (256, 1, 1))]
    pub fn rotate_rows(
        src: &[Complex32],
        mut dst: DisjointSlice<Complex32>,
        nrange: u32,
        nrows: u32,
        shift: u32,
    ) {
        let idx = thread::index_1d();
        let i = idx.get() as u32;
        if i >= nrange * nrows {
            return;
        }
        let r = i % nrange;
        let a = i / nrange;
        let s = (r + nrange - shift % nrange) % nrange;
        if let Some(o) = dst.get_mut(idx) {
            *o = src[(a * nrange + s) as usize];
        }
    }

    /// Spaltenrotation nach oben (FFT-Shifts auf dem Device).
    #[kernel]
    #[launch_bounds(256)]
    #[launch_contract(domain = 1, block = (256, 1, 1))]
    pub fn shift_cols(
        src: &[Complex32],
        mut dst: DisjointSlice<Complex32>,
        naz: u32,
        nrange: u32,
        up: u32,
    ) {
        let idx = thread::index_1d();
        let i = idx.get() as u32;
        if i >= naz * nrange {
            return;
        }
        let r = i % nrange;
        let a = i / nrange;
        let s = (a + up % naz) % naz;
        if let Some(o) = dst.get_mut(idx) {
            *o = src[(s * nrange + r) as usize];
        }
    }

    /// Time-Domain Backprojection mit `f64`-Geometrie: `raw` ist puls-major
    /// `[num_pulses × num_samples]` (range-komprimiert, rückversetzt),
    /// `plat` die Plattformpositionen (`f64`, Orbitmaßstab).
    #[kernel]
    #[launch_bounds(256)]
    #[launch_contract(domain = 1, block = (256, 1, 1))]
    // Flache Skalar-ABI ist Absicht (sar_tdbp-Muster): Geometrie direkt aus
    // dem PTX-Parameterraum, ohne struct-Deserialisierung auf dem Device.
    #[allow(clippy::too_many_arguments)]
    pub fn tdbp(
        raw: &[Complex32],
        plat: &[Vec3d],
        mut out: DisjointSlice<Complex32>,
        naz: u32,
        nrange: u32,
        num_pulses: u32,
        num_samples: u32,
        pulse_limit: u32,
        lambda: f64,
        c: f64,
        dt: f64,
        t0: f64,
        x0: f64,
        y0: f64,
        dx: f64,
        dy: f64,
    ) {
        if naz == 0 || nrange == 0 {
            return;
        }
        let idx = thread::index_1d();
        let lin = idx.get() as u32;
        // Langsame Achse = Azimut (a), schnelle = Range (r), wie Host.
        let r = lin % nrange;
        let a = lin / nrange;
        if a >= naz || r >= nrange {
            return;
        }
        let pos_x = x0 + (a as f64 + 0.5) * dx;
        let pos_y = y0 + (r as f64 + 0.5) * dy;
        let mut acc_re = 0.0f64;
        let mut acc_im = 0.0f64;
        let np = pulse_limit.min(num_pulses);
        let mut p = 0u32;
        while p < np {
            let ap = plat[p as usize];
            let ddx = ap.x - pos_x;
            let ddy = ap.y - pos_y;
            let ddz = ap.z - 0.0;
            let d = (ddx * ddx + ddy * ddy + ddz * ddz).sqrt();
            // Range-Interpolation (gleiche Formel wie `07_tdbp`).
            let s = (2.0 * d / c - t0) / dt;
            let n = num_samples as f64;
            let (s_re, s_im) = if s >= 0.0 && s <= n - 1.0 {
                let s0 = s as u32;
                let a = raw[(p * num_samples + s0) as usize];
                if s0 + 1 >= num_samples {
                    (a.re as f64, a.im as f64)
                } else {
                    let b = raw[(p * num_samples + s0 + 1) as usize];
                    let f = s - s0 as f64;
                    (
                        a.re as f64 + (b.re as f64 - a.re as f64) * f,
                        a.im as f64 + (b.im as f64 - a.im as f64) * f,
                    )
                }
            } else {
                (0.0, 0.0)
            };
            // Matched-Filter e^{+j·4πd/λ} (f64-Phase, wie Host).
            let ph = 4.0 * core::f64::consts::PI * d / lambda;
            let m_re = ph.cos();
            let m_im = ph.sin();
            acc_re += s_re * m_re - s_im * m_im;
            acc_im += s_re * m_im + s_im * m_re;
            p += 1;
        }
        if let Some(o) = out.get_mut(idx) {
            *o = Complex32::new(acc_re as f32, acc_im as f32);
        }
    }
}
