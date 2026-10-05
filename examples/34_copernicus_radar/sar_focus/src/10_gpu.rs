//! GPU-Prozessoren: RDA- und TDBP-Fokus auf dem Device.
//!
//! Gleiche Mathematik wie die CPU-Referenzen: RDA nutzt cuFFT für die vier
//! FFT-Stufen und Kernel für Filter/Shifts (Ping-Pong-Puffer), TDBP den
//! `f64`-Kernel aus `09_kernel`. Filter werden hostseitig mit denselben
//! Funktionen wie CPU gebaut und einmalig hochgeladen.

use crate::cufft::{CufftPlan, Direction};
use crate::kernel::GpuContext;
use crate::range::RangeCompressor;
use crate::rda::{self, RdaParams};
use crate::tdbp::TdbpGrid;
use crate::types::{Complex32, Error, Vec3d};
use cuda_core::DeviceBuffer;

/// RDA-Fokus auf GPU (ein Prozessor je Chunk-Geometrie).
pub struct RdaGpuProcessor {
    g: GpuContext,
    row_fwd: CufftPlan,
    row_inv: CufftPlan,
    col_fwd: CufftPlan,
    col_inv: CufftPlan,
    /// Fusionierter Range×RCMC-Filter (2D, device-resident).
    filt_rr: DeviceBuffer<Complex32>,
    /// Azimut-Filter (2D, device-resident).
    filt_az: DeviceBuffer<Complex32>,
    cur: DeviceBuffer<Complex32>,
    tmp: DeviceBuffer<Complex32>,
    naz: usize,
    nrange: usize,
    shift: usize,
}

impl RdaGpuProcessor {
    pub fn new(p: &RdaParams) -> Result<Self, Error> {
        let g = GpuContext::new()?;
        let n = p.naz * p.nrange;
        // Filter hostseitig (identische Funktionen wie CPU) bauen + laden.
        let comp = RangeCompressor::new(&p.chirp, p.nrange);
        let fa = rda::az_freqs(p.naz, p.pri_s);
        let fr = rda::range_freqs_unshifted(p.nrange, p.chirp.fs_hz);
        let rcmc = rda::rcmc_filter(
            p.naz,
            p.nrange,
            &fa,
            &fr,
            p.slant_m[p.nrange / 2],
            p.veff_range,
            p.fdc_range,
        );
        let az = rda::azimuth_filter(p.naz, p.nrange, &fa, p.slant_m, p.veff_range, p.fdc_range);
        let rf = comp.filter();
        let mut rr = Vec::with_capacity(n);
        let mut azh = Vec::with_capacity(n);
        for a in 0..p.naz {
            let base = a * p.nrange;
            for (r, &f) in rf.iter().enumerate().take(p.nrange) {
                let i = base + r;
                let mut v = f;
                if p.apply_rcmc {
                    v *= rcmc[i];
                }
                rr.push(Complex32::new(v.re, v.im));
                azh.push(Complex32::new(az[i].re, az[i].im));
            }
        }
        let filt_rr = g.upload(&rr)?;
        let filt_az = g.upload(&azh)?;
        let cur = g.alloc(n)?;
        let tmp = g.alloc(n)?;
        // cuFFT-Pläne (Zeilen kontiguierlich, Spalten mit Schrittweite).
        let mut row_fwd = CufftPlan::plan_rows(p.nrange, p.naz)?;
        let mut row_inv = CufftPlan::plan_rows(p.nrange, p.naz)?;
        let mut col_fwd = CufftPlan::plan_strided(p.naz, p.nrange, 1, p.nrange)?;
        let mut col_inv = CufftPlan::plan_strided(p.naz, p.nrange, 1, p.nrange)?;
        for plan in [&mut row_fwd, &mut row_inv, &mut col_fwd, &mut col_inv] {
            plan.set_stream(&g.stream)?;
        }
        let ntx = crate::chirp::num_tx_samples(&p.chirp);
        Ok(Self {
            g,
            row_fwd,
            row_inv,
            col_fwd,
            col_inv,
            filt_rr,
            filt_az,
            cur,
            tmp,
            naz: p.naz,
            nrange: p.nrange,
            shift: rda::correlation_shift_samples(ntx, p.nrange),
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
            self.row_fwd
                .exec_inplace(self.cur.cu_deviceptr(), Direction::Forward)?;
        }
        unsafe {
            self.col_fwd
                .exec_inplace(self.cur.cu_deviceptr(), Direction::Forward)?;
        }
        // fftshift: um ceil(naz/2) nach oben.
        self.g
            .launch_shift_cols(&self.cur, &mut self.tmp, naz, nr, naz.div_ceil(2))?;
        std::mem::swap(&mut self.cur, &mut self.tmp);
        self.g
            .launch_cmul_2d(&self.cur, &self.filt_rr, &mut self.tmp, naz * nr)?;
        std::mem::swap(&mut self.cur, &mut self.tmp);
        unsafe {
            self.row_inv
                .exec_inplace(self.cur.cu_deviceptr(), Direction::Inverse)?;
        }
        self.g
            .launch_rotate_rows(&self.cur, &mut self.tmp, nr, naz, self.shift)?;
        std::mem::swap(&mut self.cur, &mut self.tmp);
        self.g
            .launch_cmul_2d(&self.cur, &self.filt_az, &mut self.tmp, naz * nr)?;
        std::mem::swap(&mut self.cur, &mut self.tmp);
        // ifftshift: um floor(naz/2) nach oben.
        self.g
            .launch_shift_cols(&self.cur, &mut self.tmp, naz, nr, naz / 2)?;
        std::mem::swap(&mut self.cur, &mut self.tmp);
        unsafe {
            self.col_inv
                .exec_inplace(self.cur.cu_deviceptr(), Direction::Inverse)?;
        }
        let mut out = self.g.download(&self.cur)?;
        let s = 1.0 / (naz * nr) as f32;
        for v in out.iter_mut() {
            *v = v.scale(s);
        }
        data.copy_from_slice(&out);
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
        )?;
        self.g.download(&self.out)
    }
}
