//! Host-Orchestrierung: Upload, Kernel-Launch, Download.
//!
//! `SarPipeline` hält Kontext, Stream und Device-Buffer über viele
//! `run(limit)`-Aufrufe hinweg — die GUI variiert nur `pulse_limit`.
//! Dazu reine Ausgabe-Helfer (dB→RGB, PNG, ASCII-Vorschau).

use crate::kernel::{kernels, peak_power, tdbp_cpu_parallel};
use crate::phantom::{PhantomKind, build as build_phantom};
use crate::simulator::{RawData, simulate};
use crate::types::{
    Complex32, RadarParams, SPEED_OF_LIGHT, SceneGeometry, Vec3, mag_to_unit_db, turbo,
};
use cuda_core::{CudaContext, CudaStream, DeviceBuffer, LaunchConfig1D};
use std::path::{Path, PathBuf};
use std::sync::Arc;

/// Pipeline-Fehler (CUDA- oder Geometrie-Meldungen als Text).
#[derive(Debug)]
pub struct PipelineError(pub String);

impl std::fmt::Display for PipelineError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Pipeline: {}", self.0)
    }
}

impl std::error::Error for PipelineError {}

fn err<E: std::fmt::Display>(e: E) -> PipelineError {
    PipelineError(e.to_string())
}

/// GPU-Pipeline: Rohdaten + Plattformpositionen liegen dauerhaft auf dem
/// Device; `run` startet nur den Kernel neu und lädt das Bild zurück.
pub struct SarPipeline {
    _ctx: Arc<CudaContext>,
    stream: Arc<CudaStream>,
    module: kernels::LoadedModule,
    raw_dev: DeviceBuffer<Complex32>,
    plat_dev: DeviceBuffer<Vec3>,
    out_dev: DeviceBuffer<Complex32>,
    geo: SceneGeometry,
    radar: RadarParams,
    t0: f32,
    dt: f32,
    num_samples: u32,
    blocks: u32,
}

impl SarPipeline {
    /// Legt Kontext/Stream an, lädt das Kernel-Modul und kopiert `raw`
    /// sowie die Pulspositionen auf das Device.
    pub fn new(
        geo: SceneGeometry,
        radar: RadarParams,
        raw: &RawData,
    ) -> Result<Self, PipelineError> {
        if raw.num_pulses != geo.num_pulses
            || raw.samples.len() != (raw.num_pulses * raw.num_samples) as usize
        {
            return Err(PipelineError(format!(
                "RawData ({}×{}) passt nicht zur Geometrie ({})",
                raw.num_pulses, raw.num_samples, geo.num_pulses
            )));
        }
        let pixels = geo
            .width
            .checked_mul(geo.height)
            .ok_or_else(|| PipelineError("Bild zu groß (Überlauf)".to_string()))?;
        if pixels == 0 {
            return Err(PipelineError("Bildgröße muss > 0 sein".to_string()));
        }
        let ctx = CudaContext::new(0).map_err(err)?;
        let stream = ctx.default_stream();
        let plat: Vec<Vec3> = (0..geo.num_pulses).map(|p| geo.pulse_pos(p)).collect();
        let raw_dev = DeviceBuffer::from_host(&stream, &raw.samples).map_err(err)?;
        let plat_dev = DeviceBuffer::from_host(&stream, &plat).map_err(err)?;
        let out_dev = DeviceBuffer::<Complex32>::zeroed(&stream, pixels as usize).map_err(err)?;
        // SAFETY: `kernels` ist das eigene eingebettete Modul dieser Crate.
        let module = unsafe { kernels::load(&ctx) }.map_err(err)?;
        Ok(Self {
            _ctx: ctx,
            stream,
            module,
            raw_dev,
            plat_dev,
            out_dev,
            geo,
            radar,
            t0: raw.t0,
            dt: raw.dt,
            num_samples: raw.num_samples,
            blocks: pixels.div_ceil(256),
        })
    }

    /// Fokussiert mit den ersten `pulse_limit` Pulsen und lädt das
    /// komplexe Bild (`width × height`, zeilenmajor) zurück auf den Host.
    pub fn run(&mut self, pulse_limit: u32) -> Result<Vec<Complex32>, PipelineError> {
        let prepared = self
            .module
            .prepare_tdbp(LaunchConfig1D::new(self.blocks, 256, 0))
            .map_err(err)?;
        self.module
            .tdbp(
                &self.stream,
                &prepared,
                &self.raw_dev,
                &self.plat_dev,
                &mut self.out_dev,
                self.geo.width,
                self.geo.height,
                self.geo.num_pulses,
                self.num_samples,
                pulse_limit,
                self.radar.lambda(),
                SPEED_OF_LIGHT,
                self.dt,
                self.t0,
                self.geo.x0,
                self.geo.y0,
                self.geo.dx,
                self.geo.dy,
            )
            .map_err(err)?;
        self.out_dev.to_host_vec(&self.stream).map_err(err)
    }
}

/// Dynamikumfang der dB-Darstellung (0 dB = Maximum).
pub const DYN_RANGE_DB: f32 = 40.0;

/// Komplexbild → RGB (dB-Skala + Turbo-Colormap), zeilenmajor.
/// `py = 0` (Bildzeile 0, nahes Range) liegt auf Zeile 0 des Puffers.
pub fn image_to_rgb(img: &[Complex32], width: u32, height: u32) -> Vec<u8> {
    let max = img.iter().fold(0.0f32, |m, c| m.max(c.norm()));
    let mut rgb = Vec::with_capacity(img.len() * 3);
    for c in img {
        let t = mag_to_unit_db(c.norm(), max, DYN_RANGE_DB);
        let (r, g, b) = turbo(t);
        rgb.push((r * 255.0) as u8);
        rgb.push((g * 255.0) as u8);
        rgb.push((b * 255.0) as u8);
    }
    assert_eq!(rgb.len(), (width * height * 3) as usize);
    rgb
}

/// Schreibt RGB-Daten als PNG (Headless-Modus, Tests).
pub fn save_png(path: &Path, rgb: &[u8], width: u32, height: u32) -> Result<(), String> {
    image::save_buffer(path, rgb, width, height, image::ColorType::Rgb8)
        .map_err(|e| format!("PNG schreiben: {e}"))
}

/// ASCII-Vorschau fürs Terminal (Headless-Kontrolle ohne Display).
/// Gibt `rows` Zeilen à `cols` Zeichen zurück, gleiche Orientierung wie
/// PNG und GUI (Nahbereich oben).
pub fn ascii_preview(img: &[Complex32], width: u32, height: u32, cols: u32, rows: u32) -> String {
    const RAMP: &[u8] = b" .:-=+*#%@";
    let max = img.iter().fold(0.0f32, |m, c| m.max(c.norm()));
    let mut out = String::new();
    for r in 0..rows {
        // Oberste Ausgabezeile = Nahbereich (py 0), wie in PNG/GUI.
        let py = (height - 1) * r / rows.max(1).saturating_sub(1).max(1);
        for c in 0..cols {
            let px = width * c / cols.max(1);
            let v = img[(py * width + px) as usize].norm();
            let t = mag_to_unit_db(v, max, DYN_RANGE_DB);
            let i = (t * (RAMP.len() - 1) as f32).round() as usize;
            out.push(RAMP[i] as char);
        }
        out.push('\n');
    }
    out
}

/// Headless-Auftrag: Szene rechnen und als PNG ablegen (CLI: `--headless`).
#[derive(Clone, Debug)]
pub struct HeadlessJob {
    pub phantom: PhantomKind,
    pub size: u32,
    pub num_pulses: u32,
    /// Genutzte Pulse; `None` = alle.
    pub limit: Option<u32>,
    pub output: PathBuf,
}

/// Ergebnis eines Headless-Laufs (für stdout-Protokoll und Tests).
#[derive(Clone, Debug)]
pub struct HeadlessReport {
    pub width: u32,
    pub height: u32,
    pub pulses_used: u32,
    pub peak_px: u32,
    pub peak_py: u32,
    pub peak_mag: f32,
    pub ascii: String,
}

/// Führt einen Headless-Auftrag aus: Phantom → Simulation → GPU-TDBP →
/// PNG + Kennzahlen.
pub fn run_headless(job: &HeadlessJob) -> Result<HeadlessReport, PipelineError> {
    if job.size == 0 || job.num_pulses == 0 {
        return Err(PipelineError("size/pulses müssen > 0 sein".to_string()));
    }
    let geo = SceneGeometry::default_scene(job.size, job.size, job.num_pulses);
    let radar = RadarParams::x_band();
    let targets = build_phantom(job.phantom, geo);
    let raw = simulate(geo, radar, &targets);
    let limit = job.limit.unwrap_or(job.num_pulses).min(job.num_pulses);
    let mut pipe = SarPipeline::new(geo, radar, &raw)?;
    let img = pipe.run(limit)?;
    let (peak, peak_v) = peak_power(&img);
    let rgb = image_to_rgb(&img, geo.width, geo.height);
    save_png(&job.output, &rgb, geo.width, geo.height).map_err(PipelineError)?;
    Ok(HeadlessReport {
        width: geo.width,
        height: geo.height,
        pulses_used: limit,
        peak_px: peak as u32 % geo.width,
        peak_py: peak as u32 / geo.width,
        peak_mag: peak_v.sqrt(),
        ascii: ascii_preview(&img, geo.width, geo.height, 64, 20),
    })
}

/// Feste Benchmark-Last (Vergleichbarkeit über Läufe/Rechner hinweg):
/// 128²-Bild, 256 Pulse, RUST-Phantom.
pub const BENCH_SIZE: u32 = 128;
pub const BENCH_PULSES: u32 = 256;

/// CPU-GPU-Vergleich: misst Simulation, Upload, GPU-Lauf (Median aus 5
/// nach einem Warm-up) und CPU-Läufe mit 1/2/4/8/allen Strängen.
/// Gibt einen druckfertigen Bericht zurück (inkl. Korrektheits-Gegenprobe).
pub fn run_benchmark() -> Result<String, PipelineError> {
    use std::time::Instant;
    let mut rep = String::new();
    rep.push_str(&format!(
        "Benchmark: {BENCH_SIZE}x{BENCH_SIZE} Pixel, {BENCH_PULSES} Pulse, RUST-Phantom\n"
    ));
    let geo = SceneGeometry::default_scene(BENCH_SIZE, BENCH_SIZE, BENCH_PULSES);
    let radar = RadarParams::x_band();
    let targets = build_phantom(PhantomKind::Rust, geo);

    let t = Instant::now();
    let raw = simulate(geo, radar, &targets);
    let sim_ms = t.elapsed().as_secs_f64() * 1000.0;
    rep.push_str(&format!("Simulation (CPU, 1 Strang): {sim_ms:8.1} ms\n",));

    let t = Instant::now();
    let mut pipe = SarPipeline::new(geo, radar, &raw)?;
    let up_ms = t.elapsed().as_secs_f64() * 1000.0;
    rep.push_str(&format!("Upload (Kontext+Buffer):    {up_ms:8.1} ms\n",));

    pipe.run(u32::MAX)?; // Warm-up (GPU-Takt, Caches).
    let mut gpu_ms = Vec::with_capacity(5);
    let mut gpu_img = Vec::new();
    for _ in 0..5 {
        let t = Instant::now();
        gpu_img = pipe.run(u32::MAX)?;
        gpu_ms.push(t.elapsed().as_secs_f64() * 1000.0);
    }
    gpu_ms.sort_by(|a, b| a.total_cmp(b));
    let gpu_med = gpu_ms[2];
    rep.push_str(&format!(
        "GPU TDBP+Download (Median): {gpu_med:8.1} ms  [{:.1} .. {:.1}]\n",
        gpu_ms[0], gpu_ms[4]
    ));

    let max_t = std::thread::available_parallelism()
        .map(|n| n.get() as u32)
        .unwrap_or(4);
    let mut counts = vec![1, 2, 4, 8, max_t];
    counts.sort_unstable();
    counts.dedup();
    let mut cpu_img = Vec::new();
    for n in counts {
        let t = Instant::now();
        cpu_img = tdbp_cpu_parallel(&raw, geo, radar, u32::MAX, n);
        let ms = t.elapsed().as_secs_f64() * 1000.0;
        rep.push_str(&format!(
            "CPU TDBP ({n:2} Stränge):          {ms:8.1} ms  ({:6.1}x vs. GPU)\n",
            ms / gpu_med.max(1e-9)
        ));
    }

    // Gegenprobe: GPU-Bild gegen schnellsten CPU-Lauf.
    let (_, peak) = peak_power(&cpu_img);
    let mut max_rel = 0.0f32;
    for (g, c) in gpu_img.iter().zip(cpu_img.iter()) {
        max_rel = max_rel.max((g.re - c.re).hypot(g.im - c.im) / peak.sqrt());
    }
    rep.push_str(&format!("GPU-vs-CPU max. rel. Abw.: {max_rel:.3e}\n"));
    Ok(rep)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn impulse(w: u32, h: u32) -> Vec<Complex32> {
        let mut img = vec![Complex32::zero(); (w * h) as usize];
        img[(h as usize / 2 * w as usize) + w as usize / 2] = Complex32::new(1.0, 0.0);
        img
    }

    #[test]
    fn rgb_peak_hell_hintergrund_dunkel() {
        let rgb = image_to_rgb(&impulse(8, 8), 8, 8);
        assert_eq!(rgb.len(), 8 * 8 * 3);
        let center = (4 * 8 + 4) * 3;
        // Peak = Turbo-Maximum (rötlich hell), Ecke = Turbo-Minimum (dunkel).
        assert!(rgb[center] > 100);
        assert!(rgb[0] < 80 && rgb[1] < 80 && rgb[2] < 80);
    }

    #[test]
    fn ascii_zeigt_peak() {
        let art = ascii_preview(&impulse(16, 16), 16, 16, 16, 16);
        let lines: Vec<&str> = art.lines().collect();
        assert_eq!(lines.len(), 16);
        assert!(art.contains('@'));
        // Peak bei (8, 8): Zeile r mit py = r = 8.
        // (Oberste Zeile = Nahbereich, wie in PNG/GUI.)
        assert!(lines[8].contains('@'));
        assert_eq!(lines[8].as_bytes()[8], b'@');
    }
}
