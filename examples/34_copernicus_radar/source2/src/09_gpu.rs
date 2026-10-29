//! `cuda-oxide`-GPU-Kernel (`#[cuda_module]`) und `GpuSarContext` für Stripmap- und TOPS-Fokussierung.
//!
//! Architektur-Innovationen gegenüber `sar_focus`:
//! 1. **Einziger Arbeits-Puffer (In-Place)** über `DisjointSlice::get_mut(idx)`
//!    (in Spike 1 verifiziert) — spart den zweiten 660-MB-Ping-Pong-Puffer!
//! 2. **Null explizite `fftshift` / `rotate_rows`-Kernel**:
//!    - Der Range-Korrelationsversatz ist bereits als linearer Phasenterm in den
//!      1D-Range-Matched-Filter `rf` eingefaltet (`build_range_matched_filter(..., true)`).
//!    - Die Azimut-Frequenz `fa_unshifted(a, n_az, prf)` wird direkt in der unshifteten
//!      FFT-Ordnung ausgewertet.
//! 3. **Gemeinsame `#[inline]` Device-Hilfsfunktionen** (`cmul`, `d_factor_dev`, `fa_unshifted`)
//!    innerhalb des `#[cuda_module]`-Moduls (in Spike 1 verifiziert).
//! 4. **TOPS-Unterstützung auf der GPU**:
//!    - `tops_deramp_pad`: Kopiert einen range-komprimierten Burst (`n_az_raw × n_rg`)
//!      zentriert in den gepaddeten Puffer (`n_az_fft × n_rg`) und multipliziert jede Zeile
//!      mit dem TOPS-Deramping-Phasor $\exp(-j(2\pi f_{\text{DC,mid}} t_m + \pi k_t t_m^2))$.
//!    - `tops_rcmc_inplace`: 2D-Frequenzbereichs-RCMC + Normierung `1 / n_rg`.
//!    - `tops_az_compress_inplace`: Azimut-Kompression mit effektiver FM-Rate
//!      $k_{\text{eff}}(r) = k_a(r) - k_t$ + Normierung `1 / n_az_fft`.

use crate::chirp::{ChirpParams, build_range_matched_filter};
use crate::cufft::{CufftPlan, Direction};
use crate::rda::{TopsSteering, estimate_tops_steering, tops_padded_az_len};
use crate::types::{BurstBlock, C_LIGHT, Complex32, Error, LAMBDA, err};
use cuda_core::{CudaContext, CudaStream, DeviceBuffer, LaunchConfig1D};
use cuda_device::{DisjointSlice, kernel, launch_bounds, launch_contract, thread};
use cuda_host::cuda_module;
use std::sync::Arc;

#[cuda_module]
pub mod sar_kernels {
    use super::*;
    use crate::types::Complex32;

    const PI: f64 = core::f64::consts::PI;

    #[inline]
    pub fn cmul(a: Complex32, b: Complex32) -> Complex32 {
        Complex32::new(a.re * b.re - a.im * b.im, a.re * b.im + a.im * b.re)
    }

    #[inline]
    pub fn fa_unshifted(a: u32, n_az: u32, prf: f64) -> f64 {
        let half = n_az.div_ceil(2);
        let kk = if a < half {
            f64::from(a)
        } else {
            f64::from(a) - f64::from(n_az)
        };
        kk * prf / f64::from(n_az)
    }

    #[inline]
    pub fn d_factor_dev(df: f64, v: f64, lam: f64) -> f64 {
        let x = lam * lam * df * df / (4.0 * v * v);
        (1.0 - x.min(0.999_999)).sqrt()
    }

    /// Multipliziert jede Zeile im Range-Frequenzbereich in-place mit `rf[r] * scale`.
    #[kernel]
    #[launch_bounds(256)]
    #[launch_contract(domain = 1, block = (256, 1, 1))]
    pub fn range_filter_inplace(
        mut buf: DisjointSlice<Complex32>,
        rf: &[Complex32],
        n_total: u32,
        n_rg: u32,
        scale: f32,
    ) {
        let idx = thread::index_1d();
        let i = idx.get() as u32;
        if i >= n_total {
            return;
        }
        let r = (i % n_rg) as usize;
        if let Some(o) = buf.get_mut(idx) {
            let c = cmul(*o, rf[r]);
            *o = Complex32::new(c.re * scale, c.im * scale);
        }
    }

    /// Stripmap: In-place Range-Matched-Filter (inkl. Shift) + RCMC im unshifteten 2D-Frequenzbereich.
    #[kernel]
    #[launch_bounds(256)]
    #[launch_contract(domain = 1, block = (256, 1, 1))]
    #[allow(clippy::too_many_arguments)]
    pub fn sm_range_rcmc_inplace(
        mut buf: DisjointSlice<Complex32>,
        rf: &[Complex32],
        veff: &[f64],
        fdc: &[f64],
        n_az: u32,
        n_rg: u32,
        prf: f64,
        fs: f64,
        r_mid: f64,
        lam: f64,
        c_light: f64,
        apply_rcmc: u32,
    ) {
        let idx = thread::index_1d();
        let i = idx.get() as u32;
        if i >= n_az * n_rg {
            return;
        }
        let a = i / n_rg;
        let r = i % n_rg;
        let ri = r as usize;
        let fa = fa_unshifted(a, n_az, prf);
        let mut h = rf[ri];
        if apply_rcmc != 0 {
            let fr = fa_unshifted(r, n_rg, fs);
            let d = d_factor_dev(fa - fdc[ri], veff[ri], lam);
            let shift = r_mid * (1.0 / d - 1.0);
            let ph = 4.0 * PI * fr * shift / c_light;
            let rot = Complex32::new(ph.cos() as f32, ph.sin() as f32);
            h = cmul(h, rot);
        }
        if let Some(o) = buf.get_mut(idx) {
            *o = cmul(*o, h);
        }
    }

    /// Stripmap: In-place hyperbolischer Azimut-Matched-Filter + Skalierung `scale`.
    #[kernel]
    #[launch_bounds(256)]
    #[launch_contract(domain = 1, block = (256, 1, 1))]
    #[allow(clippy::too_many_arguments)]
    pub fn sm_az_compress_inplace(
        mut buf: DisjointSlice<Complex32>,
        veff: &[f64],
        fdc: &[f64],
        n_az: u32,
        n_rg: u32,
        prf: f64,
        r0_m: f64,
        dr_m: f64,
        lam: f64,
        scale: f32,
    ) {
        let idx = thread::index_1d();
        let i = idx.get() as u32;
        if i >= n_az * n_rg {
            return;
        }
        let a = i / n_rg;
        let r = i % n_rg;
        let ri = r as usize;
        let fa = fa_unshifted(a, n_az, prf);
        let d = d_factor_dev(fa - fdc[ri], veff[ri], lam);
        let r_m = r0_m + f64::from(r) * dr_m;
        let ph = 4.0 * PI * r_m * d / lam;
        let h = Complex32::new((ph.cos() as f32) * scale, (ph.sin() as f32) * scale);
        if let Some(o) = buf.get_mut(idx) {
            *o = cmul(*o, h);
        }
    }

    /// TOPS: Kopiert den range-komprimierten Burst (`n_az_raw × n_rg`) zentriert in den
    /// gepaddeten Zielpuffer (`n_az_fft × n_rg`) und wendet das quadratische Zeitbereichs-Deramping an.
    #[kernel]
    #[launch_bounds(256)]
    #[launch_contract(domain = 1, block = (256, 1, 1))]
    #[allow(clippy::too_many_arguments)]
    pub fn tops_deramp_pad(
        src: &[Complex32],
        mut dst: DisjointSlice<Complex32>,
        n_az_raw: u32,
        n_az_fft: u32,
        n_rg: u32,
        pad_top: u32,
        pri_s: f64,
        fdc_mid_hz: f64,
        kt_hz_s: f64,
    ) {
        let idx = thread::index_1d();
        let i = idx.get() as u32;
        if i >= n_az_fft * n_rg {
            return;
        }
        let a = i / n_rg;
        let r = i % n_rg;
        let val = if a >= pad_top && a < pad_top + n_az_raw {
            let m = a - pad_top;
            let t = (f64::from(m) - f64::from(n_az_raw) * 0.5) * pri_s;
            let ph = -2.0 * PI * fdc_mid_hz * t - PI * kt_hz_s * t * t;
            let rot = Complex32::new(ph.cos() as f32, ph.sin() as f32);
            cmul(src[(m * n_rg + r) as usize], rot)
        } else {
            Complex32::new(0.0, 0.0)
        };
        if let Some(o) = dst.get_mut(idx) {
            *o = val;
        }
    }

    /// TOPS: In-place RCMC im 2D-Frequenzbereich + Skalierung `scale_rg`.
    #[kernel]
    #[launch_bounds(256)]
    #[launch_contract(domain = 1, block = (256, 1, 1))]
    #[allow(clippy::too_many_arguments)]
    pub fn tops_rcmc_inplace(
        mut buf: DisjointSlice<Complex32>,
        n_az_fft: u32,
        n_rg: u32,
        prf: f64,
        fs: f64,
        r_mid: f64,
        veff_mid: f64,
        lam: f64,
        c_light: f64,
        scale_rg: f32,
    ) {
        let idx = thread::index_1d();
        let i = idx.get() as u32;
        if i >= n_az_fft * n_rg {
            return;
        }
        let a = i / n_rg;
        let r = i % n_rg;
        let fa = fa_unshifted(a, n_az_fft, prf);
        let fr = fa_unshifted(r, n_rg, fs);
        let d = d_factor_dev(fa, veff_mid, lam);
        let shift_m = r_mid * (1.0 / d - 1.0);
        let ph = 4.0 * PI * fr * shift_m / c_light;
        let h = Complex32::new((ph.cos() as f32) * scale_rg, (ph.sin() as f32) * scale_rg);
        if let Some(o) = buf.get_mut(idx) {
            *o = cmul(*o, h);
        }
    }

    /// TOPS: In-place Azimut-Kompression mit $k_{\text{eff}}(r) = k_a(r) - k_t$ + Skalierung `scale_az`.
    #[kernel]
    #[launch_bounds(256)]
    #[launch_contract(domain = 1, block = (256, 1, 1))]
    #[allow(clippy::too_many_arguments)]
    pub fn tops_az_compress_inplace(
        mut buf: DisjointSlice<Complex32>,
        veff: &[f64],
        n_az_fft: u32,
        n_rg: u32,
        prf: f64,
        r0_m: f64,
        dr_m: f64,
        lam: f64,
        kt_hz_s: f64,
        scale_az: f32,
    ) {
        let idx = thread::index_1d();
        let i = idx.get() as u32;
        if i >= n_az_fft * n_rg {
            return;
        }
        let a = i / n_rg;
        let r = i % n_rg;
        let ri = r as usize;
        let fa = fa_unshifted(a, n_az_fft, prf);
        let r_m = r0_m + f64::from(r) * dr_m;
        let v = veff[ri];
        let ka = -2.0 * v * v / (lam * r_m);
        let keff = ka - kt_hz_s;
        let ph = PI * fa * fa / keff;
        let h = Complex32::new((ph.cos() as f32) * scale_az, (ph.sin() as f32) * scale_az);
        if let Some(o) = buf.get_mut(idx) {
            *o = cmul(*o, h);
        }
    }

    /// TOPS InSAR: Kompensiert in-place die analytische stationäre TOPS-Restphase
    /// $\exp(+j(2\pi f_{\text{DC,mid}} (\tau_{\text{foc}} - \tau_0) + \pi \frac{k_a(r) k_t}{k_a(r) - k_t} \tau_0^2))$,
    /// sodass der fokussierte Burst zu einem reinen Zero-Doppler-Basisband-SLC wird.
    #[kernel]
    #[launch_bounds(256)]
    #[launch_contract(domain = 1, block = (256, 1, 1))]
    #[allow(clippy::too_many_arguments)]
    pub fn tops_insar_phase_comp_inplace(
        mut buf: DisjointSlice<Complex32>,
        veff: &[f64],
        n_az_fft: u32,
        n_rg: u32,
        pri_s: f64,
        r0_m: f64,
        dr_m: f64,
        lam: f64,
        fdc_mid_hz: f64,
        kt_hz_s: f64,
    ) {
        let idx = thread::index_1d();
        let i = idx.get() as u32;
        if i >= n_az_fft * n_rg {
            return;
        }
        let a = i / n_rg;
        let r = i % n_rg;
        let ri = r as usize;
        let mid_az = f64::from(n_az_fft) * 0.5;
        let tau_foc = (f64::from(a) - mid_az) * pri_s;
        let r_m = r0_m + f64::from(r) * dr_m;
        let v = veff[ri];
        let ka = -2.0 * v * v / (lam * r_m);
        let keff = ka - kt_hz_s;
        let tau_0 = tau_foc * (keff / ka);
        let k_top = (ka * kt_hz_s) / keff;
        let ph = 2.0 * PI * fdc_mid_hz * (tau_foc - tau_0) + PI * k_top * tau_0 * tau_0;
        let rot = Complex32::new(ph.cos() as f32, ph.sin() as f32);
        if let Some(o) = buf.get_mut(idx) {
            *o = cmul(*o, rot);
        }
    }

    /// TOPS InSAR: Sub-Pixel-bilineare komplexe Interpolation des Slave-Bursts auf das
    /// Master-Burst-Gitter inklusive affinem Shift-Modell und Flat-Earth-/Ramp-Phasenabzug.
    #[kernel]
    #[launch_bounds(256)]
    #[launch_contract(domain = 1, block = (256, 1, 1))]
    #[allow(clippy::too_many_arguments)]
    pub fn insar_resample_and_ifg_kernel(
        master: &[Complex32],
        slave: &[Complex32],
        mut ifg_out: DisjointSlice<Complex32>,
        mut pm_out: DisjointSlice<f32>,
        mut ps_out: DisjointSlice<f32>,
        n_az: u32,
        n_rg: u32,
        da0: f64,
        da_da: f64,
        da_dr: f64,
        dr0: f64,
        dr_da: f64,
        dr_dr: f64,
        phi0: f64,
        dphi_da: f64,
        dphi_dr: f64,
        dphi_dr2: f64,
    ) {
        let idx = thread::index_1d();
        let i = idx.get() as u32;
        if i >= n_az * n_rg {
            return;
        }
        let a = i / n_rg;
        let r = i % n_rg;
        let af = f64::from(a) - f64::from(n_az) * 0.5;
        let rf = f64::from(r) - f64::from(n_rg) * 0.5;

        let da = da0 + da_da * af + da_dr * rf;
        let dr = dr0 + dr_da * af + dr_dr * rf;
        let sa = f64::from(a) + da;
        let sr = f64::from(r) + dr;

        let mut ig = Complex32::new(0.0, 0.0);
        let mut pm = 0.0f32;
        let mut ps = 0.0f32;

        if sa >= 0.0
            && sa < f64::from(n_az.saturating_sub(1))
            && sr >= 0.0
            && sr < f64::from(n_rg.saturating_sub(1))
        {
            let a0 = sa as u32;
            let r0 = sr as u32;
            let fa = (sa - f64::from(a0)) as f32;
            let fr = (sr - f64::from(r0)) as f32;

            let s00 = slave[(a0 * n_rg + r0) as usize];
            let s01 = slave[(a0 * n_rg + r0 + 1) as usize];
            let s10 = slave[((a0 + 1) * n_rg + r0) as usize];
            let s11 = slave[((a0 + 1) * n_rg + r0 + 1) as usize];

            let w00 = (1.0 - fa) * (1.0 - fr);
            let w01 = (1.0 - fa) * fr;
            let w10 = fa * (1.0 - fr);
            let w11 = fa * fr;

            let s_interp = Complex32::new(
                w00 * s00.re + w01 * s01.re + w10 * s10.re + w11 * s11.re,
                w00 * s00.im + w01 * s01.im + w10 * s10.im + w11 * s11.im,
            );
            let m_val = master[i as usize];
            pm = m_val.re * m_val.re + m_val.im * m_val.im;
            ps = s_interp.re * s_interp.re + s_interp.im * s_interp.im;

            // M * conj(S)
            let prod = Complex32::new(
                m_val.re * s_interp.re + m_val.im * s_interp.im,
                m_val.im * s_interp.re - m_val.re * s_interp.im,
            );
            // Flat-Earth- und Rampen-Kompensation exp(-j * phi_model)
            let phi_mod = phi0 + dphi_da * af + dphi_dr * rf + dphi_dr2 * rf * rf;
            let rot = Complex32::new(phi_mod.cos() as f32, (-phi_mod.sin()) as f32);
            ig = cmul(prod, rot);
        }

        if let Some(o) = ifg_out.get_mut(idx) {
            *o = ig;
        }
        if let Some(o) = pm_out.get_mut(thread::index_1d()) {
            *o = pm;
        }
        if let Some(o) = ps_out.get_mut(thread::index_1d()) {
            *o = ps;
        }
    }
}

/// Persistenter GPU-Kontext für Stripmap- und TOPS-SAR-Fokussierung.
pub struct GpuSarContext {
    pub ctx: Arc<CudaContext>,
    pub stream: Arc<CudaStream>,
    pub module: sar_kernels::LoadedModule,
    pub device_name: String,
}

impl GpuSarContext {
    pub fn new() -> Result<Self, Error> {
        let ctx = CudaContext::new(0).map_err(err)?;
        let stream = ctx.default_stream();
        // SAFETY: `sar_kernels` ist das eigene eingebettete Modul dieser Crate.
        let module = unsafe { sar_kernels::load(&ctx) }.map_err(err)?;
        Ok(Self {
            ctx,
            stream,
            module,
            device_name: "CUDA Device 0 (cuda-oxide)".into(),
        })
    }

    #[inline]
    fn cfg_1d(n: usize) -> LaunchConfig1D {
        LaunchConfig1D::new((n.div_ceil(256) as u32).max(1), 256, 0)
    }

    /// Führt eine schnelle 1D-Range-Kompression eines Blocks (`n_az × n_rg`) auf der GPU aus
    /// (inkl. eingefaltetem Range-Shift).
    pub fn range_compress_block(
        &self,
        raw: &[Complex32],
        n_az: usize,
        n_rg: usize,
        chirp: &ChirpParams,
    ) -> Result<Vec<Complex32>, Error> {
        let n_total = n_az * n_rg;
        let rf_host = build_range_matched_filter(chirp, n_rg, true);
        let rf_dev = DeviceBuffer::from_host(&self.stream, &rf_host).map_err(err)?;
        let mut buf_dev = DeviceBuffer::from_host(&self.stream, raw).map_err(err)?;

        let mut row_plan = CufftPlan::plan_rows(n_rg, n_az)?;
        row_plan.set_stream(&self.stream)?;

        unsafe {
            row_plan.exec_inplace(buf_dev.cu_deviceptr(), Direction::Forward)?;
        }
        let prep = self
            .module
            .prepare_range_filter_inplace(Self::cfg_1d(n_total))
            .map_err(err)?;
        self.module
            .range_filter_inplace(
                &self.stream,
                &prep,
                &mut buf_dev,
                &rf_dev,
                n_total as u32,
                n_rg as u32,
                1.0 / (n_rg as f32),
            )
            .map_err(err)?;
        unsafe {
            row_plan.exec_inplace(buf_dev.cu_deviceptr(), Direction::Inverse)?;
        }

        buf_dev.to_host_vec(&self.stream).map_err(err)
    }

    /// Fokussiert einen Stripmap-Block (`BurstBlock`) vollständig in-place auf der GPU
    /// (ohne Ping-Pong-Zweitpuffer und ohne separate Shift-/Rotate-Kernel).
    pub fn focus_stripmap_block_gpu(
        &self,
        block: &BurstBlock,
        fdc_range: &[f64],
        apply_rcmc: bool,
    ) -> Result<Vec<Complex32>, Error> {
        let n_az = block.n_az;
        let n_rg = block.n_rg;
        let n_total = n_az * n_rg;
        let chirp = ChirpParams {
            txpsf_hz: block.params.tx_start_freq_hz,
            txpl_s: block.params.tx_pulse_length_s,
            txprr_hz_s: block.params.tx_ramp_rate_hz_s,
            fs_hz: block.params.fs_hz,
        };
        let rf_host = build_range_matched_filter(&chirp, n_rg, true);
        let rf_dev = DeviceBuffer::from_host(&self.stream, &rf_host).map_err(err)?;
        let veff_dev = DeviceBuffer::from_host(&self.stream, &block.veff).map_err(err)?;
        let fdc_dev = DeviceBuffer::from_host(&self.stream, fdc_range).map_err(err)?;
        let mut buf_dev = DeviceBuffer::from_host(&self.stream, &block.raw).map_err(err)?;

        let mut row_plan = CufftPlan::plan_rows(n_rg, n_az)?;
        let mut col_plan = CufftPlan::plan_strided(n_az, n_rg, 1, n_rg)?;
        row_plan.set_stream(&self.stream)?;
        col_plan.set_stream(&self.stream)?;

        // 1. Range-FFT + Azimut-FFT in-place
        unsafe {
            row_plan.exec_inplace(buf_dev.cu_deviceptr(), Direction::Forward)?;
            col_plan.exec_inplace(buf_dev.cu_deviceptr(), Direction::Forward)?;
        }

        // 2. In-place Range-Filter (inkl. Shift) + RCMC
        let prep_rcmc = self
            .module
            .prepare_sm_range_rcmc_inplace(Self::cfg_1d(n_total))
            .map_err(err)?;
        self.module
            .sm_range_rcmc_inplace(
                &self.stream,
                &prep_rcmc,
                &mut buf_dev,
                &rf_dev,
                &veff_dev,
                &fdc_dev,
                n_az as u32,
                n_rg as u32,
                block.params.prf_hz,
                block.params.fs_hz,
                block.params.slant_range(n_rg / 2),
                LAMBDA,
                C_LIGHT,
                u32::from(apply_rcmc),
            )
            .map_err(err)?;

        // 3. Range-iFFT in-place
        unsafe {
            row_plan.exec_inplace(buf_dev.cu_deviceptr(), Direction::Inverse)?;
        }

        // 4. In-place Azimut-Matched-Filter + 1/(n_az*n_rg)-Skalierung
        let prep_az = self
            .module
            .prepare_sm_az_compress_inplace(Self::cfg_1d(n_total))
            .map_err(err)?;
        self.module
            .sm_az_compress_inplace(
                &self.stream,
                &prep_az,
                &mut buf_dev,
                &veff_dev,
                &fdc_dev,
                n_az as u32,
                n_rg as u32,
                block.params.prf_hz,
                block.params.r0_m,
                block.params.dr_m,
                LAMBDA,
                1.0 / (n_total as f32),
            )
            .map_err(err)?;

        // 5. Azimut-iFFT in-place
        unsafe {
            col_plan.exec_inplace(buf_dev.cu_deviceptr(), Direction::Inverse)?;
        }

        buf_dev.to_host_vec(&self.stream).map_err(err)
    }

    /// Fokussiert einen einzelnen TOPS-Burst (`BurstBlock`) auf der GPU:
    /// 1. 1D-Range-Kompression auf der GPU
    /// 2. Unwrapped Sub-Aperture Clutterlock für $(f_{\text{DC,mid}}, k_t)$
    /// 3. GPU-Deramping & Padding auf `n_az_fft`
    /// 4. GPU-Azimut-FFT, optionales RCMC, GPU-Azimut-Kompression mit $k_{\text{eff}}(r)$ und GPU-Azimut-iFFT.
    pub fn focus_tops_burst_gpu(
        &self,
        block: &BurstBlock,
        apply_rcmc: bool,
    ) -> Result<(Vec<Complex32>, usize, usize, TopsSteering), Error> {
        let n_az = block.n_az;
        let n_rg = block.n_rg;
        let pri_s = block.params.pri_s;
        let prf = block.params.prf_hz;
        let fs = block.params.fs_hz;
        let chirp = ChirpParams {
            txpsf_hz: block.params.tx_start_freq_hz,
            txpl_s: block.params.tx_pulse_length_s,
            txprr_hz_s: block.params.tx_ramp_rate_hz_s,
            fs_hz: fs,
        };

        // 1. Range-Kompression auf der GPU
        let n_raw_total = n_az * n_rg;
        let rf_host = build_range_matched_filter(&chirp, n_rg, true);
        let rf_dev = DeviceBuffer::from_host(&self.stream, &rf_host).map_err(err)?;
        let mut rc_dev = DeviceBuffer::from_host(&self.stream, &block.raw).map_err(err)?;

        let mut row_raw_plan = CufftPlan::plan_rows(n_rg, n_az)?;
        row_raw_plan.set_stream(&self.stream)?;
        unsafe {
            row_raw_plan.exec_inplace(rc_dev.cu_deviceptr(), Direction::Forward)?;
        }
        let prep_rf = self
            .module
            .prepare_range_filter_inplace(Self::cfg_1d(n_raw_total))
            .map_err(err)?;
        self.module
            .range_filter_inplace(
                &self.stream,
                &prep_rf,
                &mut rc_dev,
                &rf_dev,
                n_raw_total as u32,
                n_rg as u32,
                1.0 / (n_rg as f32),
            )
            .map_err(err)?;
        unsafe {
            row_raw_plan.exec_inplace(rc_dev.cu_deviceptr(), Direction::Inverse)?;
        }

        let rc_host = rc_dev.to_host_vec(&self.stream).map_err(err)?;

        // 2. Unwrapped Sub-Aperture Clutterlock auf dem range-komprimierten Burst
        let steering = estimate_tops_steering(&rc_host, n_az, n_rg, pri_s, &block.az_beam_addr);

        // 3. GPU-Deramping + Padding auf n_az_fft
        let r_mid = block.params.slant_range(n_rg / 2);
        let veff_mid = block.veff[n_rg / 2];
        let n_az_fft = tops_padded_az_len(n_az, pri_s, veff_mid, r_mid);
        let pad_top = (n_az_fft - n_az) / 2;
        let n_pad_total = n_az_fft * n_rg;

        let mut pad_dev =
            DeviceBuffer::<Complex32>::zeroed(&self.stream, n_pad_total).map_err(err)?;
        let prep_deramp = self
            .module
            .prepare_tops_deramp_pad(Self::cfg_1d(n_pad_total))
            .map_err(err)?;
        self.module
            .tops_deramp_pad(
                &self.stream,
                &prep_deramp,
                &rc_dev,
                &mut pad_dev,
                n_az as u32,
                n_az_fft as u32,
                n_rg as u32,
                pad_top as u32,
                pri_s,
                steering.fdc_mid_hz,
                steering.kt_hz_s,
            )
            .map_err(err)?;

        // 4. Spalten-FFT der Länge n_az_fft
        let mut col_plan = CufftPlan::plan_strided(n_az_fft, n_rg, 1, n_rg)?;
        col_plan.set_stream(&self.stream)?;
        unsafe {
            col_plan.exec_inplace(pad_dev.cu_deviceptr(), Direction::Forward)?;
        }

        // 5. Optionales RCMC im 2D-Frequenzbereich
        if apply_rcmc {
            let mut row_pad_plan = CufftPlan::plan_rows(n_rg, n_az_fft)?;
            row_pad_plan.set_stream(&self.stream)?;
            unsafe {
                row_pad_plan.exec_inplace(pad_dev.cu_deviceptr(), Direction::Forward)?;
            }
            let prep_rcmc = self
                .module
                .prepare_tops_rcmc_inplace(Self::cfg_1d(n_pad_total))
                .map_err(err)?;
            self.module
                .tops_rcmc_inplace(
                    &self.stream,
                    &prep_rcmc,
                    &mut pad_dev,
                    n_az_fft as u32,
                    n_rg as u32,
                    prf,
                    fs,
                    r_mid,
                    veff_mid,
                    LAMBDA,
                    C_LIGHT,
                    1.0 / (n_rg as f32),
                )
                .map_err(err)?;
            unsafe {
                row_pad_plan.exec_inplace(pad_dev.cu_deviceptr(), Direction::Inverse)?;
            }
        }

        // 6. In-place Azimut-Kompression mit k_eff(r) = k_a(r) - k_t
        let veff_dev = DeviceBuffer::from_host(&self.stream, &block.veff).map_err(err)?;
        let prep_az = self
            .module
            .prepare_tops_az_compress_inplace(Self::cfg_1d(n_pad_total))
            .map_err(err)?;
        self.module
            .tops_az_compress_inplace(
                &self.stream,
                &prep_az,
                &mut pad_dev,
                &veff_dev,
                n_az_fft as u32,
                n_rg as u32,
                prf,
                block.params.r0_m,
                block.params.dr_m,
                LAMBDA,
                steering.kt_hz_s,
                1.0 / (n_az_fft as f32),
            )
            .map_err(err)?;

        // 7. Spalten-iFFT zurück in den fokussierten Azimut-Zeitbereich
        unsafe {
            col_plan.exec_inplace(pad_dev.cu_deviceptr(), Direction::Inverse)?;
        }

        let out = pad_dev.to_host_vec(&self.stream).map_err(err)?;
        Ok((out, n_az_fft, n_rg, steering))
    }

    /// Fokussiert einen TOPS-Burst für InSAR auf der GPU mit optional vorgegebenem
    /// Referenz-Steering (`override_steering`) und wendet direkt im GPU-Speicher die
    /// analytische stationäre TOPS-Restphasen-Kompensation (`tops_insar_phase_comp_inplace`) an.
    pub fn focus_tops_burst_insar_gpu(
        &self,
        block: &BurstBlock,
        apply_rcmc: bool,
        override_steering: Option<TopsSteering>,
    ) -> Result<(Vec<Complex32>, usize, usize, TopsSteering), Error> {
        let n_az = block.n_az;
        let n_rg = block.n_rg;
        let pri_s = block.params.pri_s;
        let prf = block.params.prf_hz;
        let fs = block.params.fs_hz;
        let chirp = ChirpParams {
            txpsf_hz: block.params.tx_start_freq_hz,
            txpl_s: block.params.tx_pulse_length_s,
            txprr_hz_s: block.params.tx_ramp_rate_hz_s,
            fs_hz: fs,
        };

        // 1. Range-Kompression auf der GPU
        let n_raw_total = n_az * n_rg;
        let rf_host = build_range_matched_filter(&chirp, n_rg, true);
        let rf_dev = DeviceBuffer::from_host(&self.stream, &rf_host).map_err(err)?;
        let mut rc_dev = DeviceBuffer::from_host(&self.stream, &block.raw).map_err(err)?;

        let mut row_raw_plan = CufftPlan::plan_rows(n_rg, n_az)?;
        row_raw_plan.set_stream(&self.stream)?;
        unsafe {
            row_raw_plan.exec_inplace(rc_dev.cu_deviceptr(), Direction::Forward)?;
        }
        let prep_rf = self
            .module
            .prepare_range_filter_inplace(Self::cfg_1d(n_raw_total))
            .map_err(err)?;
        self.module
            .range_filter_inplace(
                &self.stream,
                &prep_rf,
                &mut rc_dev,
                &rf_dev,
                n_raw_total as u32,
                n_rg as u32,
                1.0 / (n_rg as f32),
            )
            .map_err(err)?;
        unsafe {
            row_raw_plan.exec_inplace(rc_dev.cu_deviceptr(), Direction::Inverse)?;
        }

        // 2. Entweder Master-Steering schätzen oder übergebenes Referenz-Steering nutzen
        let steering = if let Some(s) = override_steering {
            s
        } else {
            let rc_host = rc_dev.to_host_vec(&self.stream).map_err(err)?;
            estimate_tops_steering(&rc_host, n_az, n_rg, pri_s, &block.az_beam_addr)
        };

        // 3. GPU-Deramping + Padding auf n_az_fft
        let r_mid = block.params.slant_range(n_rg / 2);
        let veff_mid = block.veff[n_rg / 2];
        let n_az_fft = tops_padded_az_len(n_az, pri_s, veff_mid, r_mid);
        let pad_top = (n_az_fft - n_az) / 2;
        let n_pad_total = n_az_fft * n_rg;

        let mut pad_dev =
            DeviceBuffer::<Complex32>::zeroed(&self.stream, n_pad_total).map_err(err)?;
        let prep_deramp = self
            .module
            .prepare_tops_deramp_pad(Self::cfg_1d(n_pad_total))
            .map_err(err)?;
        self.module
            .tops_deramp_pad(
                &self.stream,
                &prep_deramp,
                &rc_dev,
                &mut pad_dev,
                n_az as u32,
                n_az_fft as u32,
                n_rg as u32,
                pad_top as u32,
                pri_s,
                steering.fdc_mid_hz,
                steering.kt_hz_s,
            )
            .map_err(err)?;

        // 4. Spalten-FFT der Länge n_az_fft
        let mut col_plan = CufftPlan::plan_strided(n_az_fft, n_rg, 1, n_rg)?;
        col_plan.set_stream(&self.stream)?;
        unsafe {
            col_plan.exec_inplace(pad_dev.cu_deviceptr(), Direction::Forward)?;
        }

        // 5. Optionales RCMC im 2D-Frequenzbereich
        if apply_rcmc {
            let mut row_pad_plan = CufftPlan::plan_rows(n_rg, n_az_fft)?;
            row_pad_plan.set_stream(&self.stream)?;
            unsafe {
                row_pad_plan.exec_inplace(pad_dev.cu_deviceptr(), Direction::Forward)?;
            }
            let prep_rcmc = self
                .module
                .prepare_tops_rcmc_inplace(Self::cfg_1d(n_pad_total))
                .map_err(err)?;
            self.module
                .tops_rcmc_inplace(
                    &self.stream,
                    &prep_rcmc,
                    &mut pad_dev,
                    n_az_fft as u32,
                    n_rg as u32,
                    prf,
                    fs,
                    r_mid,
                    veff_mid,
                    LAMBDA,
                    C_LIGHT,
                    1.0 / (n_rg as f32),
                )
                .map_err(err)?;
            unsafe {
                row_pad_plan.exec_inplace(pad_dev.cu_deviceptr(), Direction::Inverse)?;
            }
        }

        // 6. In-place Azimut-Kompression mit k_eff(r) = k_a(r) - k_t
        let veff_dev = DeviceBuffer::from_host(&self.stream, &block.veff).map_err(err)?;
        let prep_az = self
            .module
            .prepare_tops_az_compress_inplace(Self::cfg_1d(n_pad_total))
            .map_err(err)?;
        self.module
            .tops_az_compress_inplace(
                &self.stream,
                &prep_az,
                &mut pad_dev,
                &veff_dev,
                n_az_fft as u32,
                n_rg as u32,
                prf,
                block.params.r0_m,
                block.params.dr_m,
                LAMBDA,
                steering.kt_hz_s,
                1.0 / (n_az_fft as f32),
            )
            .map_err(err)?;

        // 7. Spalten-iFFT zurück in den fokussierten Azimut-Zeitbereich
        unsafe {
            col_plan.exec_inplace(pad_dev.cu_deviceptr(), Direction::Inverse)?;
        }

        let out = pad_dev.to_host_vec(&self.stream).map_err(err)?;
        Ok((out, n_az_fft, n_rg, steering))
    }

    /// Berechnet das koregistrierte und Flat-Earth-kompensierte Burst-Interferogramm
    /// `M * conj(S_resamp) * exp(-j * phi_model)` sowie `|M|^2` und `|S|^2` auf der GPU.
    #[allow(clippy::too_many_arguments, clippy::type_complexity)]
    pub fn resample_and_form_ifg_gpu(
        &self,
        master: &[Complex32],
        slave: &[Complex32],
        n_az: usize,
        n_rg: usize,
        da_coeffs: [f64; 3],
        dr_coeffs: [f64; 3],
        phi_coeffs: [f64; 4],
    ) -> Result<(Vec<Complex32>, Vec<f32>, Vec<f32>), Error> {
        let n_total = n_az * n_rg;
        let m_dev = DeviceBuffer::from_host(&self.stream, master).map_err(err)?;
        let s_dev = DeviceBuffer::from_host(&self.stream, slave).map_err(err)?;
        let mut ifg_dev = DeviceBuffer::<Complex32>::zeroed(&self.stream, n_total).map_err(err)?;
        let mut pm_dev = DeviceBuffer::<f32>::zeroed(&self.stream, n_total).map_err(err)?;
        let mut ps_dev = DeviceBuffer::<f32>::zeroed(&self.stream, n_total).map_err(err)?;

        let prep = self
            .module
            .prepare_insar_resample_and_ifg_kernel(Self::cfg_1d(n_total))
            .map_err(err)?;
        self.module
            .insar_resample_and_ifg_kernel(
                &self.stream,
                &prep,
                &m_dev,
                &s_dev,
                &mut ifg_dev,
                &mut pm_dev,
                &mut ps_dev,
                n_az as u32,
                n_rg as u32,
                da_coeffs[0],
                da_coeffs[1],
                da_coeffs[2],
                dr_coeffs[0],
                dr_coeffs[1],
                dr_coeffs[2],
                phi_coeffs[0],
                phi_coeffs[1],
                phi_coeffs[2],
                phi_coeffs[3],
            )
            .map_err(err)?;

        let ifg = ifg_dev.to_host_vec(&self.stream).map_err(err)?;
        let pm = pm_dev.to_host_vec(&self.stream).map_err(err)?;
        let ps = ps_dev.to_host_vec(&self.stream).map_err(err)?;
        Ok((ifg, pm, ps))
    }
}
