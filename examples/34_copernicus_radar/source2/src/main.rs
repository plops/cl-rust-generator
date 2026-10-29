//! Kommandozeilen-Treiber für `sar_source2`: verarbeitet Sentinel-1 Level-0 `.dat`-Dateien
//! der Modi SM (Stripmap), IW (Interferometric Wide Swath TOPS) und EW (Extra Wide Swath TOPS).

use sar_source2::chirp::{ChirpParams, RangeCompressor};
use sar_source2::gpu::GpuSarContext;
use sar_source2::ingest::load_l0_dataset;
use sar_source2::mosaic::{
    DeburstedSwath, FocusedBurst, db_mean_std, deburst_swath, focus_stripmap_overlap_save,
    mosaic_swaths, power_to_db, save_cf_and_json, save_db_png,
};
use sar_source2::rda::{clutterlock_fdc, focus_stripmap_block_cpu, focus_tops_burst_cpu};
use sar_source2::types::{Error, SarMode};
use std::path::{Path, PathBuf};
use std::time::Instant;

struct CliArgs {
    input: PathBuf,
    out_dir: PathBuf,
    prefix: String,
    use_cpu: bool,
    az_window: Option<(usize, usize)>,
    burst_range: Option<(usize, usize)>,
    multilook: Option<(usize, usize)>,
}

fn parse_pair(s: &str) -> Option<(usize, usize)> {
    let (a, b) = s.split_once(',').or_else(|| s.split_once(':'))?;
    Some((a.trim().parse().ok()?, b.trim().parse().ok()?))
}

fn parse_args() -> Result<CliArgs, Error> {
    let mut args = std::env::args().skip(1);
    let mut input: Option<PathBuf> = None;
    let mut out_dir = PathBuf::from("out");
    let mut prefix = String::from("sar");
    let mut use_cpu = false;
    let mut az_window = None;
    let mut burst_range = None;
    let mut multilook = None;

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--input" | "-i" => {
                input =
                    Some(PathBuf::from(args.next().ok_or_else(|| {
                        Error("Fehlendes Argument für --input".into())
                    })?));
            }
            "--out-dir" | "-o" => {
                out_dir = PathBuf::from(
                    args.next()
                        .ok_or_else(|| Error("Fehlendes Argument für --out-dir".into()))?,
                );
            }
            "--prefix" | "-p" => {
                prefix = args
                    .next()
                    .ok_or_else(|| Error("Fehlendes Argument für --prefix".into()))?;
            }
            "--cpu" => {
                use_cpu = true;
            }
            "--az-window" => {
                let s = args
                    .next()
                    .ok_or_else(|| Error("Fehlendes Argument für --az-window".into()))?;
                az_window = Some(
                    parse_pair(&s)
                        .ok_or_else(|| Error("Ungültiges Format für --az-window (a,b)".into()))?,
                );
            }
            "--bursts" => {
                let s = args
                    .next()
                    .ok_or_else(|| Error("Fehlendes Argument für --bursts".into()))?;
                burst_range = Some(
                    parse_pair(&s)
                        .ok_or_else(|| Error("Ungültiges Format für --bursts (b0,b1)".into()))?,
                );
            }
            "--multilook" => {
                let s = args
                    .next()
                    .ok_or_else(|| Error("Fehlendes Argument für --multilook".into()))?;
                multilook =
                    Some(parse_pair(&s).ok_or_else(|| {
                        Error("Ungültiges Format für --multilook (laz,lrg)".into())
                    })?);
            }
            other if !other.starts_with('-') && input.is_none() => {
                input = Some(PathBuf::from(other));
            }
            other => {
                return Err(Error(format!("Unbekanntes CLI-Argument: {other}")));
            }
        }
    }

    let input = input.ok_or_else(|| {
        Error(
            "Aufruf: sar_source2 --input <datei.dat> [--out-dir out] [--prefix s6|iw|ew] [--cpu] [--az-window a0,a1] [--bursts b0,b1] [--multilook laz,lrg]"
                .into(),
        )
    })?;

    Ok(CliArgs {
        input,
        out_dir,
        prefix,
        use_cpu,
        az_window,
        burst_range,
        multilook,
    })
}

fn run() -> Result<(), Error> {
    let cli = parse_args()?;
    std::fs::create_dir_all(&cli.out_dir)?;

    let t0 = Instant::now();
    println!(
        "=== sar_source2: Lade Level-0-Datensatz {} ===",
        cli.input.display()
    );
    let dataset = load_l0_dataset(&cli.input, cli.az_window, cli.burst_range)?;
    let t_ingest = t0.elapsed().as_secs_f64();

    println!(
        "Modus: {} | Pakete gesamt: {} | Echo-Pakete: {} | Orbit-Punkte: {} | Sub-Swaths: {} | Ingest: {:.2} s",
        dataset.mode.as_str(),
        dataset.total_packets,
        dataset.echo_packets,
        dataset.ephemeris.len(),
        dataset.swaths.len(),
        t_ingest
    );

    let gpu_opt = if cli.use_cpu {
        None
    } else {
        match GpuSarContext::new() {
            Ok(g) => {
                println!("GPU-Backend aktiv: {}", g.device_name);
                Some(g)
            }
            Err(e) => {
                println!("GPU nicht verfügbar ({e}), weiche auf CPU aus.");
                None
            }
        }
    };

    let t_foc_start = Instant::now();
    let mut debursted_swaths: Vec<DeburstedSwath> = Vec::with_capacity(dataset.swaths.len());

    match dataset.mode {
        SarMode::Stripmap => {
            let sw = &dataset.swaths[0];
            let blk = &sw.bursts[0];
            println!(
                "Stripmap-Swath {}: {} × {} Samples (PRF = {:.2} Hz, fs = {:.2} MHz, R0 = {:.1} km)",
                sw.swath_name,
                blk.n_az,
                blk.n_rg,
                blk.params.prf_hz,
                blk.params.fs_hz / 1e6,
                blk.params.r0_m / 1e3
            );

            let chunk_az = 4096usize;
            let overlap_az = 256usize;
            let power = focus_stripmap_overlap_save(blk, chunk_az, overlap_az, |sub| {
                let chirp = ChirpParams {
                    txpsf_hz: sub.params.tx_start_freq_hz,
                    txpl_s: sub.params.tx_pulse_length_s,
                    txprr_hz_s: sub.params.tx_ramp_rate_hz_s,
                    fs_hz: sub.params.fs_hz,
                };
                if let Some(ref gpu) = gpu_opt {
                    let rc = gpu.range_compress_block(&sub.raw, sub.n_az, sub.n_rg, &chirp)?;
                    let fdc = clutterlock_fdc(&rc, sub.n_az, sub.n_rg, sub.params.pri_s, 16);
                    gpu.focus_stripmap_block_gpu(sub, &fdc, true)
                } else {
                    let mut rc = sub.raw.clone();
                    RangeCompressor::new(&chirp, sub.n_rg).compress_matrix_parallel(&mut rc);
                    let fdc = clutterlock_fdc(&rc, sub.n_az, sub.n_rg, sub.params.pri_s, 16);
                    Ok(focus_stripmap_block_cpu(sub, &fdc, true))
                }
            })?;

            let n_chirp = (blk.params.tx_pulse_length_s * blk.params.fs_hz).round() as usize;
            let n_valid_rg = blk.n_rg.saturating_sub(n_chirp / 4).max(1);
            let mut trimmed_power = Vec::with_capacity(blk.n_az * n_valid_rg);
            for a in 0..blk.n_az {
                trimmed_power.extend_from_slice(&power[a * blk.n_rg..a * blk.n_rg + n_valid_rg]);
            }
            let t_start_s = blk.pulse_times.first().copied().unwrap_or(0.0);
            debursted_swaths.push(DeburstedSwath {
                swath_number: sw.swath_number,
                swath_name: sw.swath_name.clone(),
                n_az: blk.n_az,
                n_rg: n_valid_rg,
                t_start_s,
                dt_az_s: blk.params.pri_s,
                r0_m: blk.params.r0_m,
                dr_m: blk.params.dr_m,
                power: trimmed_power,
            });
        }
        SarMode::Iw | SarMode::Ew => {
            for sw in &dataset.swaths {
                println!(
                    "TOPS-Sub-Swath {} (swath_number={}): {} Bursts, N_rg={:.0}, PRF={:.2} Hz, fs={:.2} MHz, R0={:.1} km",
                    sw.swath_name,
                    sw.swath_number,
                    sw.bursts.len(),
                    sw.params.swl_s * sw.params.fs_hz,
                    sw.params.prf_hz,
                    sw.params.fs_hz / 1e6,
                    sw.params.r0_m / 1e3
                );
                let mut focused_bursts = Vec::with_capacity(sw.bursts.len());
                for blk in &sw.bursts {
                    let (data, n_az_fft, n_rg, steering) = if let Some(ref gpu) = gpu_opt {
                        gpu.focus_tops_burst_gpu(blk, true)?
                    } else {
                        focus_tops_burst_cpu(blk, true)
                    };
                    if blk.burst_index == 0 || blk.burst_index + 1 == sw.bursts.len() {
                        println!(
                            "  Burst {:02}: n_az={} -> n_az_fft={}, f_DC,mid={:+.1} Hz, k_t={:+.1} Hz/s, B_swept={:.0} Hz",
                            blk.burst_index,
                            blk.n_az,
                            n_az_fft,
                            steering.fdc_mid_hz,
                            steering.kt_hz_s,
                            steering.swept_bw_hz
                        );
                    }
                    let mid_time_s = blk.pulse_times.get(blk.n_az / 2).copied().unwrap_or(0.0);
                    let n_chirp =
                        (blk.params.tx_pulse_length_s * blk.params.fs_hz).round() as usize;
                    let n_valid_rg = n_rg.saturating_sub(n_chirp / 4).max(1);
                    focused_bursts.push(FocusedBurst {
                        burst_index: blk.burst_index,
                        n_az_raw: blk.n_az,
                        n_az_fft,
                        n_rg,
                        n_valid_rg,
                        mid_time_s,
                        pri_s: blk.params.pri_s,
                        r0_m: blk.params.r0_m,
                        dr_m: blk.params.dr_m,
                        veff_mid: blk.veff[n_rg / 2],
                        kt_hz_s: steering.kt_hz_s,
                        fdc_mid_hz: steering.fdc_mid_hz,
                        data,
                    });
                }
                let deb = deburst_swath(sw.swath_number, &sw.swath_name, &focused_bursts)?;
                let (_, std_db) = db_mean_std(&deb.power);
                println!(
                    "  -> Debursted {}: {} × {} Pixel, Kontrast-Std = {:.2} dB",
                    deb.swath_name, deb.n_az, deb.n_rg, std_db
                );
                let (sw_ml, sw_oaz, sw_org) =
                    sar_source2::mosaic::multilook_power(&deb.power, deb.n_az, deb.n_rg, 4, 12);
                let sw_db = power_to_db(&sw_ml);
                let sw_png = cli.out_dir.join(format!(
                    "{}_{}.png",
                    cli.prefix,
                    sw.swath_name.to_lowercase()
                ));
                save_db_png(&sw_db, sw_org, sw_oaz, &sw_png)?;
                debursted_swaths.push(deb);
            }
        }
    }

    let t_focus = t_foc_start.elapsed().as_secs_f64();

    let (laz, lrg) = cli.multilook.unwrap_or(match dataset.mode {
        SarMode::Stripmap => (10, 10),
        SarMode::Iw => (6, 24),
        SarMode::Ew => (6, 12),
    });

    let (mosaic_power, out_az, out_rg, r_min_m, r_max_m) =
        mosaic_swaths(&debursted_swaths, laz, lrg)?;
    let (mean_db, std_db) = db_mean_std(&mosaic_power);
    let mosaic_db = power_to_db(&mosaic_power);

    let png_path = cli.out_dir.join(format!("{}_mosaic.png", cli.prefix));
    let stem_path = cli.out_dir.join(format!("{}_mosaic", cli.prefix));
    save_db_png(&mosaic_db, out_rg, out_az, &png_path)?;
    save_cf_and_json(
        &stem_path,
        &mosaic_power,
        out_az,
        out_rg,
        dataset.mode.as_str(),
        r_min_m,
        r_max_m,
        std_db,
    )?;

    println!(
        "=== Fertig: {} ({} × {} Multilook-Pixel, Slant {:.1}..{:.1} km, Mittel {:.1} dB, Std {:.2} dB, Fokus+Mosaik {:.2} s) ===",
        Path::new(&png_path).display(),
        out_az,
        out_rg,
        r_min_m / 1e3,
        r_max_m / 1e3,
        mean_db,
        std_db,
        t_focus
    );
    Ok(())
}

fn main() {
    if let Err(e) = run() {
        eprintln!("Fehler: {e}");
        std::process::exit(1);
    }
}
