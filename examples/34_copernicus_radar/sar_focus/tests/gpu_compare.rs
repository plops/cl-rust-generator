//! GPU-gegen-CPU-Vergleich (Pflicht): Jede GPU-Rechnung gegen die
//! schlichte CPU-Referenz mit Schranke für die maximale relative
//! Abweichung (Vorbild TDBP-Tabelle: 2,4·10⁻⁴, Schranke 10⁻³).
//!
//! Braucht GPU + `cargo oxide test` (wie `sar_tdbp`-GPU-Tests).

mod common;

use common::{FS, NAZ, NR, PRI, V, chirp, simulate_raw};
use rustfft::FftPlanner;
use sar_focus::cufft::{CufftPlan, Direction};
use sar_focus::kernel::GpuContext;
use sar_focus::range::RangeCompressor;
use sar_focus::types::Complex32;

/// Max. relative Abweichung (peak-normiert), wie sar_tdbp-Tabelle.
fn max_rel_diff(a: &[Complex32], b: &[Complex32]) -> f32 {
    assert_eq!(a.len(), b.len());
    let peak = b.iter().map(|c| c.norm()).fold(0.0f32, f32::max).max(1e-30);
    a.iter()
        .zip(b.iter())
        .map(|(x, y)| (x.re - y.re).hypot(x.im - y.im) / peak)
        .fold(0.0f32, f32::max)
}

#[test]
fn cufft_stimmt_mit_rustfft() {
    // Zeilen- (kontiguierlich) und Spalten-FFTs (schrittweise) gegen rustfft.
    let g = GpuContext::new().unwrap();
    let (naz, nr) = (64usize, 256usize);
    let mut host = vec![Complex32::zero(); naz * nr];
    for (i, c) in host.iter_mut().enumerate() {
        // Deterministisches Muster (Impuls + Rampe + Schwingung).
        let x = i as f32;
        *c = Complex32::new(
            (x * 0.37).sin() + if i % 97 == 0 { 3.0 } else { 0.0 },
            (x * 0.73).cos(),
        );
    }
    // Referenz: rustfft vorwärts auf allen Zeilen.
    let mut planner = FftPlanner::<f32>::new();
    let fwd = planner.plan_fft_forward(nr);
    let mut expect: Vec<num_complex::Complex32> = host
        .iter()
        .map(|c| num_complex::Complex32::new(c.re, c.im))
        .collect();
    for row in expect.chunks_mut(nr) {
        fwd.process(row);
    }
    // GPU: dieselben Zeilen-FFTs via cuFFT.
    let dev = g.upload(&host).unwrap();
    let mut plan = CufftPlan::plan_rows(nr, naz).unwrap();
    plan.set_stream(&g.stream).unwrap();
    unsafe {
        plan.exec_inplace(dev.cu_deviceptr(), Direction::Forward)
            .unwrap();
    }
    let got = g.download(&dev).unwrap();
    let expect32: Vec<Complex32> = expect.iter().map(|c| Complex32::new(c.re, c.im)).collect();
    let d = max_rel_diff(&got, &expect32);
    assert!(d < 1e-5, "Zeilen-FFT max rel. Abw. {d}");
    // Spalten-FFTs (Schrittweite nr): vorwärts + rückwärts = Identität × naz.
    let mut cplan = CufftPlan::plan_strided(naz, nr, 1, nr).unwrap();
    cplan.set_stream(&g.stream).unwrap();
    unsafe {
        cplan
            .exec_inplace(dev.cu_deviceptr(), Direction::Forward)
            .unwrap();
        cplan
            .exec_inplace(dev.cu_deviceptr(), Direction::Inverse)
            .unwrap();
    }
    g.synchronize().unwrap();
    let back = g.download(&dev).unwrap();
    // Roundtrip-Skalierung naz (beide Libs unnormiert).
    let d2 = max_rel_diff(
        &back
            .iter()
            .map(|c| c.scale(1.0 / naz as f32))
            .collect::<Vec<_>>(),
        &expect32,
    );
    assert!(d2 < 1e-4, "Spalten-Roundtrip max rel. Abw. {d2}");
}

#[test]
fn rda_gpu_stimmt_mit_cpu() {
    use sar_focus::rda::{RdaParams, RdaProcessor};
    let (data, slant, _) = simulate_raw();
    let veff = vec![V; NR];
    let fdc = vec![0.0; NR];
    let mk = |apply_rcmc| RdaParams {
        chirp: chirp(),
        naz: NAZ,
        nrange: NR,
        pri_s: PRI,
        slant_m: &slant,
        veff_range: &veff,
        fdc_range: &fdc,
        apply_rcmc,
    };
    let mut cpu = data.clone();
    RdaProcessor::new(&mk(true)).focus(&mut cpu);
    let mut gpu = data;
    sar_focus::gpu::RdaGpuProcessor::new(&mk(true))
        .unwrap()
        .focus(&mut gpu)
        .unwrap();
    let d = max_rel_diff(&gpu, &cpu);
    assert!(d < 1e-3, "RDA GPU-vs-CPU max rel. Abw. {d}");
}

#[test]
fn rda_gpu_stimmt_mit_cpu_schief() {
    // Schielende Geometrie: Die On-the-fly-Kernel indizieren `fdc`/`veff` je
    // Range-Bin — mit konstanten Vektoren (Test oben) fiele ein Indexfehler
    // nicht auf. Rampen + Offset zwingen jede Zelle auf eigenen Pfad.
    use sar_focus::rda::{RdaParams, RdaProcessor};
    let (data, slant, _) = simulate_raw();
    let veff: Vec<f64> = (0..NR)
        .map(|r| V + (r as f64 - NR as f64 / 2.0) * 0.05)
        .collect();
    let fdc: Vec<f64> = (0..NR)
        .map(|r| -35.0 + 70.0 * r as f64 / NR as f64)
        .collect();
    for apply_rcmc in [true, false] {
        let mk = RdaParams {
            chirp: chirp(),
            naz: NAZ,
            nrange: NR,
            pri_s: PRI,
            slant_m: &slant,
            veff_range: &veff,
            fdc_range: &fdc,
            apply_rcmc,
        };
        let mut cpu = data.clone();
        RdaProcessor::new(&mk).focus(&mut cpu);
        let mut gpu = data.clone();
        sar_focus::gpu::RdaGpuProcessor::new(&mk)
            .unwrap()
            .focus(&mut gpu)
            .unwrap();
        let d = max_rel_diff(&gpu, &cpu);
        assert!(d < 1e-3, "RDA schief (rcmc={apply_rcmc}) max rel. Abw. {d}");
    }
}

#[test]
fn tdbp_gpu_stimmt_mit_cpu() {
    use sar_focus::chirp::num_tx_samples;
    use sar_focus::rda::correlation_shift_samples;
    use sar_focus::tdbp::{TdbpGrid, TdbpInput, tdbp_cpu};
    use sar_focus::types::{TX_WAVELENGTH_M, Vec3d};
    // Gleiches Fenster wie tests/tdbp_point.rs (65×65, 129 Pulse).
    const WIN: usize = 65;
    const NP: usize = 129;
    let (mut data, _, t0) = simulate_raw();
    let comp = RangeCompressor::new(&chirp(), NR);
    comp.compress_rows(&mut data[..NP * NR]);
    let shift = correlation_shift_samples(num_tx_samples(&chirp()), NR);
    for p in 0..NP {
        data[p * NR..(p + 1) * NR].rotate_right(shift);
    }
    let dx = V * PRI;
    let h = 693_000.0f64;
    let r0 = 950_000.0f64;
    let yc = (r0 * r0 - h * h).sqrt();
    let dy = (sar_focus::types::SPEED_OF_LIGHT / 2.0 / FS) / (yc / r0);
    let plat: Vec<Vec3d> = (0..NP)
        .map(|p| Vec3d::new((p as f64 - 512.0) * dx, 0.0, h))
        .collect();
    let grid = TdbpGrid {
        naz: WIN,
        nrange: WIN,
        x0: (480.0 - 512.0) * dx - 0.5 * dx,
        dx_az: dx,
        y_near: yc + (2016.0 - 2048.0) * dy - 0.5 * dy,
        dy_gr: dy,
        re_m: 0.0,
    };
    let inp = TdbpInput {
        data: &data[..NP * NR],
        npulse: NP,
        nsample: NR,
        plat: &plat,
        t0_s: t0,
        dt_s: 1.0 / FS,
        lambda: TX_WAVELENGTH_M,
    };
    let cpu = tdbp_cpu(&grid, &inp, NP);
    let gpu = sar_focus::gpu::TdbpGpuProcessor::new(grid, NP, NR, t0, 1.0 / FS, TX_WAVELENGTH_M)
        .unwrap()
        .focus(&data[..NP * NR], &plat, NP)
        .unwrap();
    let d = max_rel_diff(&gpu, &cpu);
    assert!(d < 1e-3, "TDBP GPU-vs-CPU max rel. Abw. {d}");
}
