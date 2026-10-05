//! GPU-Prozessoren: RDA- und TDBP-Fokus auf dem Device.
//!
//! Gleiche Mathematik wie die CPU-Referenzen: RDA nutzt cuFFT für die vier
//! FFT-Stufen und Fusions-Kernel für Filter/Shifts (Ping-Pong-Puffer), TDBP
//! den `f64`-Kernel aus `09_kernel`. Die RDA-Filter werden pro Zelle
//! on-the-fly aus 1D-Vektoren berechnet — keine 2D-Filtermatrizen, kein
//! mehrfacher PCIe-Transfer. Der Prozessor wird einmal je Geometrie erzeugt
//! und über alle Chunks wiederverwendet (Pläne + Puffer persistent).

use crate::cufft::{CufftPlan, Direction};
use crate::kernel::GpuContext;
use crate::range::RangeCompressor;
use crate::rda::{self, RdaParams};
use crate::tdbp::TdbpGrid;
use crate::types::{Complex32, Error, SPEED_OF_LIGHT, TX_WAVELENGTH_M, Vec3d};
use cuda_core::DeviceBuffer;

/// RDA-Fokus auf GPU (ein Prozessor je Chunk-Geometrie, über Chunks geteilt).
pub struct RdaGpuProcessor {
    g: GpuContext,
    /// Zeilen-FFTs (Richtung ist Exec-Parameter, daher je Geometrie einer).
    row: CufftPlan,
    /// Spalten-FFTs (Schrittweite = Zeilenlänge).
    col: CufftPlan,
    /// Range-Matched-Filter (1D, `nrange`).
    rf: DeviceBuffer<Complex32>,
    /// Schrägentfernung, `v_eff`, Doppler-Centroid je Range-Bin (1D).
    slant: DeviceBuffer<f64>,
    veff: DeviceBuffer<f64>,
    fdc: DeviceBuffer<f64>,
    cur: DeviceBuffer<Complex32>,
    tmp: DeviceBuffer<Complex32>,
    naz: usize,
    nrange: usize,
    shift: usize,
    prf_hz: f64,
    fs_hz: f64,
    r0_m: f64,
    apply_rcmc: bool,
}

impl RdaGpuProcessor {
    pub fn new(p: &RdaParams) -> Result<Self, Error> {
        let g = GpuContext::new()?;
        let n = p.naz * p.nrange;
        // Nur 1D-Vektoren bauen + laden (< 1 MB statt 2,64 GB 2D-Filter).
        let comp = RangeCompressor::new(&p.chirp, p.nrange);
        let rf: Vec<Complex32> = comp
            .filter()
            .iter()
            .map(|c| Complex32::new(c.re, c.im))
            .collect();
        let rf = g.upload(&rf)?;
        let slant = g.upload_f64(p.slant_m)?;
        let veff = g.upload_f64(p.veff_range)?;
        let fdc = g.upload_f64(p.fdc_range)?;
        let cur = g.alloc(n)?;
        let tmp = g.alloc(n)?;
        // cuFFT-Pläne (einmalig; Richtung wählt `exec_inplace`).
        let mut row = CufftPlan::plan_rows(p.nrange, p.naz)?;
        let mut col = CufftPlan::plan_strided(p.naz, p.nrange, 1, p.nrange)?;
        row.set_stream(&g.stream)?;
        col.set_stream(&g.stream)?;
        let ntx = crate::chirp::num_tx_samples(&p.chirp);
        Ok(Self {
            g,
            row,
            col,
            rf,
            slant,
            veff,
            fdc,
            cur,
            tmp,
            naz: p.naz,
            nrange: p.nrange,
            shift: rda::correlation_shift_samples(ntx, p.nrange),
            prf_hz: 1.0 / p.pri_s,
            fs_hz: p.chirp.fs_hz,
            r0_m: p.slant_m[p.nrange / 2],
            apply_rcmc: p.apply_rcmc,
        })
    }

    /// Fokussiert einen Chunk in-place (gleiche Stufen wie CPU-`focus`).
    pub fn focus(&mut self, data: &mut [Complex32]) -> Result<(), Error> {
        if data.len() != self.naz * self.nrange {
            return Err(Error(format!(
                "Chunk {} passt nicht zu {}×{}",
                data.len(),
                self.naz,
                self.nrange
            )));
        }
        self.cur
            .copy_from_host(&self.g.stream, data)
            .map_err(crate::types::err)?;
        let (naz, nr) = (self.naz, self.nrange);
        // SAFETY: Pläne passen zu den Puffern (Konstruktor), Stream gebunden.
        unsafe {
            self.row
                .exec_inplace(self.cur.cu_deviceptr(), Direction::Forward)?;
        }
        unsafe {
            self.col
                .exec_inplace(self.cur.cu_deviceptr(), Direction::Forward)?;
        }
        // fftshift: um ceil(naz/2) nach oben.
        self.g
            .launch_shift_cols(&self.cur, &mut self.tmp, naz, nr, naz.div_ceil(2))?;
        std::mem::swap(&mut self.cur, &mut self.tmp);
        self.g.launch_range_rcmc(
            &self.cur,
            &self.rf,
            &self.veff,
            &self.fdc,
            &mut self.tmp,
            naz,
            nr,
            self.prf_hz,
            self.fs_hz,
            self.r0_m,
            TX_WAVELENGTH_M,
            SPEED_OF_LIGHT,
            self.apply_rcmc,
        )?;
        std::mem::swap(&mut self.cur, &mut self.tmp);
        unsafe {
            self.row
                .exec_inplace(self.cur.cu_deviceptr(), Direction::Inverse)?;
        }
        self.g
            .launch_rotate_rows(&self.cur, &mut self.tmp, nr, naz, self.shift)?;
        std::mem::swap(&mut self.cur, &mut self.tmp);
        self.g.launch_az(
            &self.cur,
            &self.slant,
            &self.veff,
            &self.fdc,
            &mut self.tmp,
            naz,
            nr,
            self.prf_hz,
            TX_WAVELENGTH_M,
        )?;
        std::mem::swap(&mut self.cur, &mut self.tmp);
        // ifftshift: um floor(naz/2) nach oben.
        self.g
            .launch_shift_cols(&self.cur, &mut self.tmp, naz, nr, naz / 2)?;
        std::mem::swap(&mut self.cur, &mut self.tmp);
        unsafe {
            self.col
                .exec_inplace(self.cur.cu_deviceptr(), Direction::Inverse)?;
        }
        // Normierung auf dem Device, Download direkt in den Zielpuffer.
        let s = 1.0 / (naz * nr) as f32;
        self.g.launch_scale(&self.cur, &mut self.tmp, naz * nr, s)?;
        std::mem::swap(&mut self.cur, &mut self.tmp);
        self.cur
            .copy_to_host(&self.g.stream, data)
            .map_err(crate::types::err)?;
        Ok(())
    }
}

/// TDBP-Fokus auf GPU (Puffer werden über Läufe wiederverwendet).
pub struct TdbpGpuProcessor {
    g: GpuContext,
    raw: DeviceBuffer<Complex32>,
    plat: DeviceBuffer<Vec3d>,
    out: DeviceBuffer<Complex32>,
    grid: TdbpGrid,
    npulse: usize,
    nsample: usize,
    t0_s: f64,
    dt_s: f64,
    lambda: f64,
}

impl TdbpGpuProcessor {
    pub fn new(
        grid: TdbpGrid,
        npulse: usize,
        nsample: usize,
        t0_s: f64,
        dt_s: f64,
        lambda: f64,
    ) -> Result<Self, Error> {
        let g = GpuContext::new()?;
        let raw = g.alloc(npulse * nsample)?;
        let plat = g.upload_vec3d(&vec![Vec3d::zero(); npulse])?;
        let out = g.alloc(grid.naz * grid.nrange)?;
        Ok(Self {
            g,
            raw,
            plat,
            out,
            grid,
            npulse,
            nsample,
            t0_s,
            dt_s,
            lambda,
        })
    }

    /// Fokussiert mit den ersten `pulse_limit` Pulsen (Upload + Kernel +
    /// Download); gibt das komplexe Bild (`naz × nrange`) zurück.
    pub fn focus(
        &mut self,
        raw_host: &[Complex32],
        plat_host: &[Vec3d],
        pulse_limit: usize,
    ) -> Result<Vec<Complex32>, Error> {
        if raw_host.len() != self.npulse * self.nsample || plat_host.len() != self.npulse {
            return Err(Error("TDBP-Eingang passt nicht zur Geometrie".into()));
        }
        self.raw
            .copy_from_host(&self.g.stream, raw_host)
            .map_err(crate::types::err)?;
        self.plat
            .copy_from_host(&self.g.stream, plat_host)
            .map_err(crate::types::err)?;
        self.g.launch_tdbp(
            &self.raw,
            &self.plat,
            &mut self.out,
            self.grid.naz as u32,
            self.grid.nrange as u32,
            self.npulse as u32,
            self.nsample as u32,
            pulse_limit.min(self.npulse) as u32,
            self.lambda,
            crate::types::SPEED_OF_LIGHT,
            self.dt_s,
            self.t0_s,
            self.grid.x0,
            self.grid.y_near,
            self.grid.dx_az,
            self.grid.dy_gr,
            self.grid.re_m,
        )?;
        self.g.download(&self.out)
    }
}
