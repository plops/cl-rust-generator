//! `sar_focus`-CLI: `.dat` → fokussiertes Bild + Quicklook.

use copernicus_radar::collect_headers::collect_packet_headers;
use copernicus_radar::header::PacketHeader;
use copernicus_radar::mmap::MappedFile;
use sar_focus::chirp::ChirpParams;
use sar_focus::ephem;
use sar_focus::look;
use sar_focus::meta;
use sar_focus::range::RangeCompressor;
use sar_focus::rda;
use sar_focus::types::Complex32;
use std::collections::HashMap;

fn usage(program: &str) -> String {
    format!(
        "usage:\n\
         \t{program} meta <input.dat>\tHeader-/Orbit-Diagnose\n\
         \t{program} focus <input.dat> <prefix> [--cpu] [--az0 N] [--az1 M]\n\
         \t        [--chunk C] [--overlap O] [--compare] [--no-rcmc]\n\
         \t        Fokussieren + Quicklook\n\
         \t{program} ships <bild.cf> <naz> <n0> [az0 az1]\tSchiffs-PSF (opt. Tiefensuche)\n\
         \t{program} ql <bild.cf> <naz> <n0> <out.png> [az0 az1 r0 r1]\n\
         \t        \tQuicklook aus .cf (RFI-sichere Spreizung, opt. Ausschnitt)\n\
         \t{program} tdbp <input.dat> <prefix> --az0 P0 --az1 P1\n\
         \t        --waz0 A0 --waz1 A1 --wrg0 R0 --wrg1 R1 [--cpu]\n\
         \t        [--compare <rda.cf> <naz> <n0> <az0>]\tTDBP-Fenster + Vergleich\n\
         \t        (wrg in RDA-Output-Pixeln)\n\
         \t{program} --help\t\tDiese Hilfe"
    )
}

/// Diagnose: zählt Echos, zeigt Chirp-/Orbit-Kennwerte (ohne Dekodierung).
fn cmd_meta(path: &str) -> Result<(), String> {
    let mapped = MappedFile::open(std::path::Path::new(path)).map_err(|e| e.to_string())?;
    let data = mapped.bytes();
    let packets = collect_packet_headers(data).map_err(|e| e.to_string())?;
    println!("Pakete: {}", packets.len());
    let mut beams: HashMap<u32, u64> = HashMap::new();
    let mut stream: Vec<(u8, u16)> = Vec::with_capacity(packets.len());
    let mut first_echo: Option<meta::EchoMeta> = None;
    let mut n_echo = 0u64;
    for (i, h) in packets.headers.iter().enumerate() {
        let h = PacketHeader::parse(h).map_err(|e| e.to_string())?;
        stream.push((h.sub_commutated_index as u8, h.sub_commutated_data as u16));
        if !h.is_calibration() {
            *beams.entry(h.elevation()).or_insert(0) += u64::from(h.number_of_quads);
        }
        if meta::is_imaging_echo(&h) {
            n_echo += 1;
            if first_echo.is_none() {
                first_echo =
                    Some(meta::parse_echo(&h, i, packets.offsets[i]).map_err(|e| e.to_string())?);
            }
        }
    }
    let mut beams: Vec<(u32, u64)> = beams.into_iter().collect();
    beams.sort();
    println!("Abbildende Echos (FDBAQ): {n_echo}");
    println!("Beams (Ele → Quads): {beams:?}");
    if let Some(m) = first_echo {
        let bw = meta::chirp_bandwidth_hz(m.txprr_hz_s, m.txpl_s);
        let r = meta::slant_range_vec(m.rank, m.pri_s, m.swst_s, m.fs_hz, 2 * m.nquads as usize);
        println!(
            "PRF: {:.2} Hz  PRI: {:.3} µs  Rang: {}  RGDEC: {}  fs: {:.4} MHz",
            m.pri_s.recip(),
            m.pri_s * 1e6,
            m.rank,
            m.rgdec,
            m.fs_hz / 1e6
        );
        println!(
            "Chirp: TXPL {:.3} µs  TXPSF {:.2} MHz  TXPRR {:.3} MHz/µs  B {:.2} MHz  ΔR {:.2} m",
            m.txpl_s * 1e6,
            m.txpsf_hz / 1e6,
            m.txprr_hz_s / 1e12,
            bw / 1e6,
            meta::range_resolution_m(bw)
        );
        println!(
            "Slant: nah {:.1} km  fern {:.1} km  ({} Samples)",
            r[0] / 1e3,
            r[r.len() - 1] / 1e3,
            r.len()
        );
        println!(
            "data_delay: {} (als F_REF-Takte: {:.3} µs = {:.1} Samples)",
            m.data_delay,
            f64::from(m.data_delay) / sar_focus::types::F_REF_HZ * 1e6,
            f64::from(m.data_delay) / sar_focus::types::F_REF_HZ * m.fs_hz
        );
    }
    let blocks = ephem::collect_blocks(&stream);
    println!("Ephemeridenblöcke: {}", blocks.len());
    if let Some(p) = blocks.first() {
        println!(
            "|r| = {:.1} km  |v| = {:.2} m/s  t0 = {}",
            p.pos.norm() / 1e3,
            p.vel.norm(),
            p.time_s
        );
    }
    Ok(())
}

fn main() {
    let program = std::env::args()
        .next()
        .unwrap_or_else(|| "sar_focus".to_string());
    let args: Vec<String> = std::env::args().skip(1).collect();
    let res = match args
        .iter()
        .map(|s| s.as_str())
        .collect::<Vec<_>>()
        .as_slice()
    {
        ["meta", path] => cmd_meta(path),
        ["focus", rest @ ..] => cmd_focus(rest),
        ["ships", cf, naz, n0, rest @ ..] => cmd_ships(cf, naz, n0, rest),
        ["ql", cf, naz, n0, png, rest @ ..] => cmd_ql(cf, naz, n0, png, rest),
        ["tdbp", rest @ ..] => cmd_tdbp(rest),
        ["--help" | "-h"] | [] => {
            println!("{}", usage(&program));
            Ok(())
        }
        _ => Err(format!("unbekannte Argumente\n{}", usage(&program))),
    };
    if let Err(e) = res {
        eprintln!("error: {e}");
        std::process::exit(1);
    }
}

/// Fokus-Konfiguration (CLI-Flaggen).
struct FocusCfg {
    input: String,
    prefix: String,
    cpu: bool,
    az0: usize,
    az1: usize,
    chunk: usize,
    overlap: usize,
    compare: bool,
    no_rcmc: bool,
}

fn parse_focus(args: &[&str]) -> Result<FocusCfg, String> {
    if args.len() < 2 {
        return Err("focus braucht <input.dat> <prefix>".to_string());
    }
    let mut c = FocusCfg {
        input: args[0].to_string(),
        prefix: args[1].to_string(),
        cpu: false,
        az0: 0,
        az1: usize::MAX,
        chunk: 8192,
        overlap: 2048,
        compare: false,
        no_rcmc: false,
    };
    let mut i = 2;
    while i < args.len() {
        match args[i] {
            "--cpu" => c.cpu = true,
            "--compare" => c.compare = true,
            "--no-rcmc" => c.no_rcmc = true,
            "--az0" => {
                i += 1;
                c.az0 = args
                    .get(i)
                    .and_then(|v| v.parse().ok())
                    .ok_or("--az0 braucht Zahl")?;
            }
            "--az1" => {
                i += 1;
                c.az1 = args
                    .get(i)
                    .and_then(|v| v.parse().ok())
                    .ok_or("--az1 braucht Zahl")?;
            }
            "--chunk" => {
                i += 1;
                c.chunk = args
                    .get(i)
                    .and_then(|v| v.parse().ok())
                    .ok_or("--chunk braucht Zahl")?;
            }
            "--overlap" => {
                i += 1;
                c.overlap = args
                    .get(i)
                    .and_then(|v| v.parse().ok())
                    .ok_or("--overlap braucht Zahl")?;
            }
            f => return Err(format!("unbekannte Flagge {f}")),
        }
        i += 1;
    }
    if c.overlap >= c.chunk {
        return Err("--overlap muss kleiner als --chunk sein".to_string());
    }
    Ok(c)
}

/// Schreibt Komplexbild als `.cf` (roh, LE-f32-Paare, zeilenmajor).
fn write_cf(path: &std::path::Path, img: &[Complex32]) -> Result<(), String> {
    use std::io::Write;
    let file = std::fs::File::create(path).map_err(|e| e.to_string())?;
    let mut w = std::io::BufWriter::new(file);
    for c in img {
        w.write_all(&c.re.to_le_bytes())
            .map_err(|e| e.to_string())?;
        w.write_all(&c.im.to_le_bytes())
            .map_err(|e| e.to_string())?;
    }
    w.flush().map_err(|e| e.to_string())?;
    Ok(())
}

/// E2E: `.dat` → Dekodierung → RDA-Fokus (CPU/GPU) → `.cf` + PNG + Kennzahlen.
fn cmd_focus(args: &[&str]) -> Result<(), String> {
    let cfg = parse_focus(args)?;
    let t_start = std::time::Instant::now();
    let w =
        sar_focus::ingest::load_window(&cfg.input, cfg.az0, cfg.az1).map_err(|e| e.to_string())?;
    let t_decode = t_start.elapsed();
    // Lokale Aliase (wie bisherige Variablennamen).
    let naz = w.naz;
    if naz < 2048 {
        eprintln!(
            "WARNUNG: nur {naz} Echos (< 2048) — Apertur-Trunkierung erzeugt \
             Wrap-Linien; E2E-Verifikation braucht ≥ 2048 Echos."
        );
    }
    let n0 = w.n0;
    let n0raw = w.n0raw;
    let raw = w.raw;
    let slant = w.slant;
    let chirp = w.chirp;
    let bw = w.bw_hz;
    let veff = w.veff;
    let line_pos = w.line_pos;
    let line_vel = w.line_vel;
    let m0 = &w.metas[0];
    // Doppler-Centroid: geometrisch vs. Clutterlock (Daten). Clutterlock
    // braucht range-komprimierte Daten (Cumming & Wong) — auf Rohdaten
    // misst Lag-1 die Chirp-Struktur statt Doppler (Absurditaet −272–100 Hz
    // statt −20 Hz). Mittlere 512 Echos separat komprimieren (ms-Aufwand).
    let mid = naz / 2;
    let geo = ephem::fdc_range_grid(line_pos[mid], line_vel[mid], &slant);
    let ncl = naz.min(512);
    let ac0 = (naz - ncl) / 2;
    let mut cl_data = raw[ac0 * n0..(ac0 + ncl) * n0].to_vec();
    RangeCompressor::new(&chirp, n0).compress_rows(&mut cl_data);
    let cl = rda::clutterlock_fdc(&cl_data, ncl, m0.pri_s, 64);
    let (gmin, gmax) = minmax(&geo);
    let (cmin, cmax) = minmax(&cl);
    println!("f_DC geometrisch: {gmin:.1}–{gmax:.1} Hz");
    println!("f_DC Clutterlock: {cmin:.1}–{cmax:.1} Hz");
    // Fokussieren (CPU einteilig, GPU Overlap-Save-Chunks).
    let t_pre = t_start.elapsed();
    let mut img = raw;
    if cfg.compare {
        let d = compare_cpu_gpu(&img, &chirp, naz, n0, m0.pri_s, &slant, &veff, &cl)?;
        println!("CPU↔GPU max. rel. Abw.: {d:.3e}");
    }
    if cfg.cpu {
        let p = rda::RdaParams {
            chirp,
            naz,
            nrange: n0,
            pri_s: m0.pri_s,
            slant_m: &slant,
            veff_range: &veff,
            fdc_range: &cl,
            apply_rcmc: !cfg.no_rcmc,
        };
        println!("CPU-Fokus …");
        rda::RdaProcessor::new(&p).focus(&mut img);
    } else {
        focus_gpu_chunked(
            &cfg, &mut img, &chirp, naz, n0, m0.pri_s, &slant, &veff, &cl,
        )?;
    }
    let t_focus = t_start.elapsed();
    println!(
        "Zeit: Dekodierung {:.1} s, Orbit+f_DC {:.1} s, Fokus {:.1} s (gesamt {:.1} s)",
        t_decode.as_secs_f64(),
        (t_pre - t_decode).as_secs_f64(),
        (t_focus - t_pre).as_secs_f64(),
        t_focus.as_secs_f64()
    );
    println!("Host-Speicher (Peak): {:.2} GB", peak_rss_gb());
    // Range-Rand beschneiden: erste ntx Spalten tragen zyklischen Wrap,
    // Pad-Schwanz dahinter fällt ebenfalls weg.
    let ntx = sar_focus::chirp::num_tx_samples(&chirp);
    let n0out = n0raw.saturating_sub(ntx.min(n0raw));
    let mut cropped = vec![Complex32::zero(); naz * n0out];
    for a in 0..naz {
        cropped[a * n0out..(a + 1) * n0out]
            .copy_from_slice(&img[a * n0 + ntx..a * n0 + ntx + n0out]);
    }
    let img = cropped;
    let n0 = n0out;
    println!("Ausgabe-Raster: {naz} × {n0} (Wrap-Rand {ntx} beschnitten)");
    // Artefakte: .cf + PNG-Quicklook + ASCII + Kennzahlen.
    let cf_path = format!("{}.cf", cfg.prefix);
    write_cf(std::path::Path::new(&cf_path), &img)?;
    println!(
        "geschrieben: {cf_path} ({:.2} GB)",
        (img.len() * 8) as f64 / 1e9
    );
    let power = write_quicklook(&img, naz, n0, &cfg.prefix)?;
    // Schiffs-PSF: Top-Peaks + FWHM-Schnitte gegen Theorie.
    report_ships(&power, naz, n0, bw, m0.fs_hz, m0.pri_s, &veff, ntx);
    Ok(())
}

/// Peak-RSS in GB (VmHWM aus /proc, Linux).
fn peak_rss_gb() -> f64 {
    std::fs::read_to_string("/proc/self/status")
        .ok()
        .and_then(|s| {
            s.lines()
                .find(|l| l.starts_with("VmHWM:"))
                .and_then(|l| l.split_whitespace().nth(1)?.parse::<f64>().ok())
        })
        .map(|kb| kb / 1e6)
        .unwrap_or(f64::NAN)
}

/// Minimum/Maximum eines Schnitts.
fn minmax(v: &[f64]) -> (f64, f64) {
    v.iter()
        .fold((f64::INFINITY, f64::NEG_INFINITY), |(a, b), &x| {
            (a.min(x), b.max(x))
        })
}

/// GPU-Fokus in Overlap-Save-Chunks (Ränder verwerfen, Mitte behalten).
#[allow(clippy::too_many_arguments)]
fn focus_gpu_chunked(
    cfg: &FocusCfg,
    img: &mut [Complex32],
    chirp: &ChirpParams,
    naz: usize,
    n0: usize,
    pri_s: f64,
    slant: &[f64],
    veff: &[f64],
    fdc: &[f64],
) -> Result<(), String> {
    let chunk = cfg.chunk.min(naz);
    let ov = cfg.overlap.min(chunk.saturating_sub(1));
    let step = chunk - ov;
    let mut out = vec![Complex32::zero(); naz * n0];
    let mut starts = Vec::new();
    let mut s = 0;
    while s < naz {
        // Letzter Chunk: zurückschieben auf volle Größe (glatte FFT-Länge).
        if s + chunk >= naz && s > 0 {
            s = naz - chunk;
        }
        starts.push(s);
        if s + chunk >= naz {
            break;
        }
        s += step;
    }
    println!("GPU-Chunks: {} à {chunk} (Overlap {ov})", starts.len());
    // Persistenter Prozessor: Pläne + Puffer einmalig, über alle Chunks
    // wiederverwendet (keine Allokations-/Planungsschleife mehr). Alle Chunks
    // haben volle Größe (letzter zurückgeschoben, s. oben).
    let p = rda::RdaParams {
        chirp: *chirp,
        naz: chunk,
        nrange: n0,
        pri_s,
        slant_m: slant,
        veff_range: veff,
        fdc_range: fdc,
        apply_rcmc: !cfg.no_rcmc,
    };
    let mut proc = sar_focus::gpu::RdaGpuProcessor::new(&p).map_err(|e| e.to_string())?;
    for (ci, &cs) in starts.iter().enumerate() {
        let ce = (cs + chunk).min(naz);
        let cn = ce - cs;
        debug_assert_eq!(cn, chunk, "Chunk-Geometrie wechselt (Prozessor teilen!)");
        println!("  Chunk {}/{}, Zeilen {cs}..{ce} …", ci + 1, starts.len());
        let mut buf = img[cs * n0..ce * n0].to_vec();
        proc.focus(&mut buf).map_err(|e| e.to_string())?;
        // Gültig: Mitte ohne Overlap-Rand (erster/letzter Chunk: Kante dazu).
        let keep0 = if cs == 0 { 0 } else { ov / 2 };
        let keep1 = if ce == naz { cn } else { cn - ov / 2 };
        out[(cs + keep0) * n0..(cs + keep1) * n0].copy_from_slice(&buf[keep0 * n0..keep1 * n0]);
    }
    img.copy_from_slice(&out);
    Ok(())
}

/// CPU↔GPU-Vergleich auf dem ersten Chunk (max. rel. Abw., peak-normiert).
#[allow(clippy::too_many_arguments)]
fn compare_cpu_gpu(
    img: &[Complex32],
    chirp: &ChirpParams,
    naz: usize,
    n0: usize,
    pri_s: f64,
    slant: &[f64],
    veff: &[f64],
    fdc: &[f64],
) -> Result<f32, String> {
    let cn = naz.min(2048);
    let mk = || rda::RdaParams {
        chirp: *chirp,
        naz: cn,
        nrange: n0,
        pri_s,
        slant_m: slant,
        veff_range: veff,
        fdc_range: fdc,
        apply_rcmc: true,
    };
    let mut cpu = img[..cn * n0].to_vec();
    rda::RdaProcessor::new(&mk()).focus(&mut cpu);
    let mut gpu = img[..cn * n0].to_vec();
    sar_focus::gpu::RdaGpuProcessor::new(&mk())
        .map_err(|e| e.to_string())?
        .focus(&mut gpu)
        .map_err(|e| e.to_string())?;
    let peak = cpu
        .iter()
        .map(|c| c.norm())
        .fold(0.0f32, f32::max)
        .max(1e-30);
    Ok(cpu
        .iter()
        .zip(gpu.iter())
        .map(|(a, b)| (a.re - b.re).hypot(a.im - b.im) / peak)
        .fold(0.0f32, f32::max))
}

/// Schiffs-PSF-Bericht: Top-Peaks mit FWHM in Range/Azimut gegen Theorie.
#[allow(clippy::too_many_arguments)]
fn report_ships(
    power: &[f32],
    naz: usize,
    n0: usize,
    bw_hz: f64,
    fs_hz: f64,
    pri_s: f64,
    veff: &[f64],
    range_off: usize,
) {
    // Azimut-Profil (32 Blöcke): Ozean dunkel, Land hell, RFI als Zacken.
    let nb = 32;
    print!("Az-Profil: ");
    for b in 0..nb {
        let (mut s, mut n) = (0.0f64, 0u64);
        for a in b * naz / nb..(b + 1) * naz / nb {
            for r in (0..n0).step_by(97) {
                s += f64::from(power[a * n0 + r]);
                n += 1;
            }
        }
        print!("{:.2e} ", s / n as f64);
        if b % 8 == 7 {
            println!();
            print!("           ");
        }
    }
    println!();
    let (res_r, res_a) = look::theoretical_resolution_m(bw_hz, 12.3);
    let dr = sar_focus::types::SPEED_OF_LIGHT / (2.0 * fs_hz);
    println!("Theorie-Auflösung: Range {res_r:.2} m, Azimut {res_a:.2} m");
    // RFI-Maske: Zeilen mit > 8× Medianleistung sind Störzeilen (Suche
    // überspringt sie; FWHM wird weiter auf Originaldaten gemessen).
    let mut rowmean = vec![0.0f64; naz];
    for (a, m) in rowmean.iter_mut().enumerate() {
        let (mut s, mut n) = (0.0f64, 0u64);
        for r in (0..n0).step_by(97) {
            s += f64::from(power[a * n0 + r]);
            n += 1;
        }
        *m = s / n as f64;
    }
    let mut sorted = rowmean.clone();
    sorted.sort_by(f64::total_cmp);
    let med = sorted[naz / 2];
    let masked: Vec<bool> = rowmean.iter().map(|&m| m > 8.0 * med).collect();
    let nmask = masked.iter().filter(|&&x| x).count();
    println!("RFI-Zeilen maskiert: {nmask}/{naz} (Median {med:.2e})");
    let mut search = power.to_vec();
    for (a, &m) in masked.iter().enumerate() {
        if m {
            for r in 0..n0 {
                search[a * n0 + r] = 0.0;
            }
        }
    }
    // Pro Viertel: Top-12, punktförmig (FWHM ≤ 3 px beidseits) = Schiff.
    for q in 0..4 {
        let (qa0, qa1) = (q * naz / 4, (q + 1) * naz / 4);
        println!("Viertel {q} (az {qa0}..{qa1}):");
        let seg = &search[qa0 * n0..qa1 * n0];
        for (k, &(a, r, v)) in look::find_peaks(seg, qa1 - qa0, n0, 12, 64)
            .iter()
            .enumerate()
        {
            let a = a + qa0;
            let da = veff[r + range_off] * pri_s;
            let c0 = r.saturating_sub(32);
            let c1 = (r + 33).min(n0);
            let rcut: Vec<f32> = power[a * n0 + c0..a * n0 + c1].to_vec();
            let mut acut = Vec::with_capacity(65);
            let aa0 = a.saturating_sub(32);
            for aa in aa0..(a + 33).min(naz) {
                acut.push(power[aa * n0 + r]);
            }
            let fr = look::cut_fwhm(&rcut, r - c0);
            let fa = look::cut_fwhm(&acut, a - aa0);
            // Ring-Untergrund (±48, ohne ±6 Kern): Kontrast in dB.
            let mut ring = Vec::with_capacity(256);
            for aa in a.saturating_sub(48)..(a + 49).min(naz) {
                for rr in r.saturating_sub(48)..(r + 49).min(n0) {
                    if aa.abs_diff(a) > 6 || rr.abs_diff(r) > 6 {
                        ring.push(power[aa * n0 + rr]);
                    }
                }
            }
            ring.sort_by(f32::total_cmp);
            let bg = ring[ring.len() / 2].max(1e-30);
            let contrast = 10.0 * (v / bg).log10();
            let tag = if fr <= 3.0 && fa <= 3.0 && contrast > 10.0 && bg < 2e5 {
                "SCHIFF"
            } else if fr <= 3.0 && fa <= 3.0 {
                "punktförmig"
            } else {
                ""
            };
            println!(
                "  Peak {}: (az {a}, rg {r}) P={v:.3e}, FWHM rg {fr:.2}px/{:.1}m az {fa:.2}px/{:.1}m K={contrast:.1}dB {tag}",
                k + 1,
                fr * dr,
                fa * da
            );
        }
    }
}

/// Quicklook-Artefakte (PNG + ASCII + Leistungs-Kennzahlen, druckt selbst).
/// Gibt das Leistungsbild zurück (für Folgeberichte).
fn write_quicklook(
    img: &[Complex32],
    naz: usize,
    n0: usize,
    prefix: &str,
) -> Result<Vec<f32>, String> {
    let power: Vec<f32> = img.iter().map(|c| c.norm_sqr()).collect();
    let (mean, std) = look::mean_std(&power);
    println!(
        "Leistung: Mittel {mean:.3e}, Std {std:.3e}, Kontrast {:.2}",
        std / mean.max(1e-30)
    );
    let lrg = (n0 / 2048).max(1);
    let laz = (naz / 2048).max(1);
    let (ml, oaz, org) = look::multilook(&power, naz, n0, laz, lrg);
    let db = look::power_db(
        &ml.iter()
            .map(|&p| Complex32::new(p.sqrt(), 0.0))
            .collect::<Vec<_>>(),
    );
    let png_path = format!("{prefix}.png");
    let (lo, hi) = look::stretch_lo_hi(&db, 0.02, 0.995);
    look::render_png_gray(&db, org, oaz, std::path::Path::new(&png_path), lo, hi)?;
    println!("geschrieben: {png_path} ({org}×{oaz}, {lo:.1}–{hi:.1} dB)");
    println!("{}", look::render_ascii(&db, org, oaz, 100));
    Ok(power)
}

/// Liest `.cf` als Leistungsbild.
fn read_cf_power(cf: &str, naz: usize, n0: usize) -> Result<Vec<f32>, String> {
    let bytes = std::fs::read(cf).map_err(|e| e.to_string())?;
    if bytes.len() != naz * n0 * 8 {
        return Err(format!("Größe {} passt nicht zu {naz}×{n0}", bytes.len()));
    }
    let mut power = vec![0.0f32; naz * n0];
    for (i, p) in power.iter_mut().enumerate() {
        let re = f32::from_le_bytes(bytes[8 * i..8 * i + 4].try_into().unwrap());
        let im = f32::from_le_bytes(bytes[8 * i + 4..8 * i + 8].try_into().unwrap());
        *p = re * re + im * im;
    }
    Ok(power)
}

/// Liest `.cf` als Komplexbild.
fn read_cf_complex(cf: &str, naz: usize, n0: usize) -> Result<Vec<Complex32>, String> {
    let bytes = std::fs::read(cf).map_err(|e| e.to_string())?;
    if bytes.len() != naz * n0 * 8 {
        return Err(format!("Größe {} passt nicht zu {naz}×{n0}", bytes.len()));
    }
    let mut img = Vec::with_capacity(naz * n0);
    for i in 0..naz * n0 {
        let re = f32::from_le_bytes(bytes[8 * i..8 * i + 4].try_into().unwrap());
        let im = f32::from_le_bytes(bytes[8 * i + 4..8 * i + 8].try_into().unwrap());
        img.push(Complex32::new(re, im));
    }
    Ok(img)
}

/// Quicklook aus `.cf` (RFI-sichere Spreizung, optionaler Ausschnitt).
fn cmd_ql(cf: &str, naz: &str, n0: &str, png: &str, rest: &[&str]) -> Result<(), String> {
    let (naz, n0): (usize, usize) = (
        naz.parse().map_err(|_| "naz Zahl?")?,
        n0.parse().map_err(|_| "n0 Zahl?")?,
    );
    let power = read_cf_power(cf, naz, n0)?;
    let (mut az0, mut az1, mut r0, mut r1) = (0, naz, 0, n0);
    if !rest.is_empty() {
        if rest.len() != 4 {
            return Err("Ausschnitt: az0 az1 r0 r1".to_string());
        }
        az0 = rest[0].parse().map_err(|_| "az0 Zahl?")?;
        az1 = rest[1].parse().map_err(|_| "az1 Zahl?")?;
        r0 = rest[2].parse().map_err(|_| "r0 Zahl?")?;
        r1 = rest[3].parse().map_err(|_| "r1 Zahl?")?;
    }
    let (hnaz, hn0) = (az1 - az0, r1 - r0);
    // RFI-Zeilen für die Spreizung maskieren (> 8× Median).
    let mut rowmean = vec![0.0f64; hnaz];
    for a in 0..hnaz {
        let (mut s, mut n) = (0.0f64, 0u64);
        for r in (0..hn0).step_by(97) {
            s += f64::from(power[(az0 + a) * n0 + r0 + r]);
            n += 1;
        }
        rowmean[a] = s / n as f64;
    }
    let mut sorted = rowmean.clone();
    sorted.sort_by(f64::total_cmp);
    let med = sorted[hnaz / 2];
    let keep: Vec<usize> = (0..hnaz).filter(|&a| rowmean[a] <= 8.0 * med).collect();
    // dB auf behaltenen Zeilen, Spreizung aus deren Perzentilen.
    let mut db = vec![0.0f32; keep.len() * hn0];
    for (i, &a) in keep.iter().enumerate() {
        for r in 0..hn0 {
            let p = power[(az0 + a) * n0 + r0 + r].max(1e-30);
            db[i * hn0 + r] = 10.0 * p.log10();
        }
    }
    // Multilook auf ≤ 2048 px Breite (Ausschnitt: nativ, max 2048).
    let lrg = (hn0 / 2048).max(1);
    let laz = (keep.len() / 2048).max(1);
    let (oaz, org) = (keep.len() / laz, hn0 / lrg);
    let mut ml = vec![0.0f32; oaz * org];
    for a in 0..oaz {
        for r in 0..org {
            let (mut s, mut n) = (0.0f64, 0u64);
            for da in 0..laz {
                for dr in 0..lrg {
                    s += f64::from(db[(a * laz + da) * hn0 + r * lrg + dr]);
                    n += 1;
                }
            }
            ml[a * org + r] = (s / n as f64) as f32;
        }
    }
    let (lo, hi) = look::stretch_lo_hi(&ml, 0.05, 0.98);
    println!(
        "ql: {org}×{oaz}, {lo:.1}–{hi:.1} dB, RFI-Zeilen gedroppt: {}",
        hnaz - keep.len()
    );
    look::render_png_gray(&ml, org, oaz, std::path::Path::new(png), lo, hi)?;
    Ok(())
}

/// Nachanalyse: `.cf` lesen → Schiffs-PSF-Bericht (ohne Refokus).
fn cmd_ships(cf: &str, naz: &str, n0: &str, rest: &[&str]) -> Result<(), String> {
    let (naz, n0): (usize, usize) = (
        naz.parse().map_err(|_| "naz Zahl?")?,
        n0.parse().map_err(|_| "n0 Zahl?")?,
    );
    let power = read_cf_power(cf, naz, n0)?;
    // S6-Nennwerte (wie E2E-Lauf): B 42,19 MHz, fs 46,9184 MHz, PRI 601,15 µs.
    let veff = vec![7100.0; n0 + 2397];
    if rest.len() == 2 {
        let (az0, az1): (usize, usize) = (
            rest[0].parse().map_err(|_| "az0 Zahl?")?,
            rest[1].parse().map_err(|_| "az1 Zahl?")?,
        );
        deep_ocean_search(&power, naz, n0, az0, az1);
        return Ok(());
    }
    if !rest.is_empty() {
        return Err("ships: [az0 az1]".to_string());
    }
    report_ships(&power, naz, n0, 42.19e6, 46.9184e6, 601.15e-6, &veff, 2397);
    Ok(())
}

/// Tiefensuche: Top-2000 im Fenster, RFI-lokal maskiert, Kontrast-gefiltert.
fn deep_ocean_search(power: &[f32], naz: usize, n0: usize, az0: usize, az1: usize) {
    let (az0, az1) = (az0.min(naz), az1.min(naz));
    // Lokale RFI-Maske (Fenster-Median, Ozean dunkel → empfindlich).
    let mut rowmean = vec![0.0f64; az1 - az0];
    for (i, m) in rowmean.iter_mut().enumerate() {
        let (mut s, mut n) = (0.0f64, 0u64);
        for r in (0..n0).step_by(53) {
            s += f64::from(power[(az0 + i) * n0 + r]);
            n += 1;
        }
        *m = s / n as f64;
    }
    let mut sorted = rowmean.clone();
    sorted.sort_by(f64::total_cmp);
    let med = sorted[rowmean.len() / 2];
    let mut search = power[az0 * n0..az1 * n0].to_vec();
    let mut nmask = 0;
    for (i, &m) in rowmean.iter().enumerate() {
        if m > 6.0 * med {
            nmask += 1;
            for r in 0..n0 {
                search[i * n0 + r] = 0.0;
            }
        }
    }
    println!("Fenster az {az0}..{az1}, Median {med:.2e}, RFI-Zeilen: {nmask}");
    let cands = look::find_peaks(&search, az1 - az0, n0, 2000, 16);
    let dr = sar_focus::types::SPEED_OF_LIGHT / (2.0 * 46.9184e6);
    let da = 7100.0 * 601.15e-6;
    let mut shown = 0;
    for &(a, r, v) in &cands {
        if shown >= 15 {
            break;
        }
        let a = a + az0;
        // Ring-Kontrast.
        let mut ring = Vec::with_capacity(512);
        for aa in a.saturating_sub(48)..(a + 49).min(naz) {
            for rr in r.saturating_sub(48)..(r + 49).min(n0) {
                if aa.abs_diff(a) > 6 || rr.abs_diff(r) > 6 {
                    ring.push(power[aa * n0 + rr]);
                }
            }
        }
        ring.sort_by(f32::total_cmp);
        let bg = ring[ring.len() / 2].max(1e-30);
        let contrast = 10.0 * (v / bg).log10();
        if contrast < 10.0 || bg >= 2e5 {
            continue;
        }
        let c0 = r.saturating_sub(32);
        let c1 = (r + 33).min(n0);
        let rcut: Vec<f32> = power[a * n0 + c0..a * n0 + c1].to_vec();
        let aa0 = a.saturating_sub(32);
        let mut acut = Vec::with_capacity(65);
        for aa in aa0..(a + 33).min(naz) {
            acut.push(power[aa * n0 + r]);
        }
        let fr = look::cut_fwhm(&rcut, r - c0);
        let fa = look::cut_fwhm(&acut, a - aa0);
        if fr > 3.5 || fa > 3.5 {
            continue;
        }
        shown += 1;
        println!(
            "  SCHIFF {shown}: (az {a}, rg {r}) P={v:.2e} bg={bg:.1e} K={contrast:.1}dB, FWHM rg {fr:.2}px/{:.1}m az {fa:.2}px/{:.1}m",
            fr * dr,
            fa * da
        );
    }
    if shown == 0 {
        println!("  keine Schiffskandidaten (punktförmig, K>10dB, dunkel)");
    }
}

struct TdbpCfg {
    input: String,
    prefix: String,
    az0: usize,
    az1: usize,
    waz0: usize,
    waz1: usize,
    wrg0: usize,
    wrg1: usize,
    cpu: bool,
    compare: Option<(String, usize, usize, usize)>,
}

fn parse_tdbp(args: &[&str]) -> Result<TdbpCfg, String> {
    if args.len() < 2 {
        return Err("tdbp braucht <input.dat> <prefix>".to_string());
    }
    let mut c = TdbpCfg {
        input: args[0].to_string(),
        prefix: args[1].to_string(),
        az0: 0,
        az1: usize::MAX,
        waz0: 0,
        waz1: usize::MAX,
        wrg0: 0,
        wrg1: usize::MAX,
        cpu: false,
        compare: None,
    };
    let num = |args: &[&str], i: &mut usize, flag: &str| -> Result<usize, String> {
        *i += 1;
        args.get(*i)
            .and_then(|v| v.parse().ok())
            .ok_or_else(|| format!("{flag} braucht Zahl"))
    };
    let mut i = 2;
    while i < args.len() {
        match args[i] {
            "--cpu" => c.cpu = true,
            "--az0" => c.az0 = num(args, &mut i, "--az0")?,
            "--az1" => c.az1 = num(args, &mut i, "--az1")?,
            "--waz0" => c.waz0 = num(args, &mut i, "--waz0")?,
            "--waz1" => c.waz1 = num(args, &mut i, "--waz1")?,
            "--wrg0" => c.wrg0 = num(args, &mut i, "--wrg0")?,
            "--wrg1" => c.wrg1 = num(args, &mut i, "--wrg1")?,
            "--compare" => {
                if i + 4 >= args.len() {
                    return Err("--compare braucht <rda.cf> <naz> <n0> <az0>".to_string());
                }
                let p = |s: &str| {
                    s.parse()
                        .map_err(|_| "--compare: naz/n0/az0 Zahlen?".to_string())
                };
                c.compare = Some((
                    args[i + 1].to_string(),
                    p(args[i + 2])?,
                    p(args[i + 3])?,
                    p(args[i + 4])?,
                ));
                i += 4;
            }
            f => return Err(format!("unbekannte Flagge {f}")),
        }
        i += 1;
    }
    if c.waz1 == usize::MAX {
        c.waz1 = c.az1;
    }
    if c.waz0 >= c.waz1 || c.wrg0 >= c.wrg1 {
        return Err("leeres TDBP-Fenster (waz/wrg prüfen)".to_string());
    }
    Ok(c)
}

/// TDBP auf Echtdaten: Pulse laden → Geometrie-Brücke → Rückprojektion.
fn cmd_tdbp(args: &[&str]) -> Result<(), String> {
    let cfg = parse_tdbp(args)?;
    let t_start = std::time::Instant::now();
    let w =
        sar_focus::ingest::load_window(&cfg.input, cfg.az0, cfg.az1).map_err(|e| e.to_string())?;
    let t_decode = t_start.elapsed();
    let naz = w.naz;
    let n0 = w.n0;
    let chirp = w.chirp;
    // wrg in RDA-Output-Pixeln; die Brücke addiert den Wrap-Rand ntx
    // selbst (slant[r+ntx]) — hier NICHT vorab addieren (war doppelt:
    // +2·ntx → Bogen 10,6 km falsch → TDBP-Versatz!).
    let ntx = sar_focus::chirp::num_tx_samples(&chirp);
    let geo = sar_focus::tdbp_geo::geo_for_window(&w, cfg.waz0, cfg.waz1, cfg.wrg0, cfg.wrg1)
        .map_err(|e| e.to_string())?;
    println!(
        "TDBP: {} Pulse ({}..{}), Grid {}×{} (az {}..{}, rg {}..{})",
        naz,
        cfg.az0,
        cfg.az0 + naz,
        geo.grid.naz,
        geo.grid.nrange,
        cfg.waz0,
        cfg.waz1,
        cfg.wrg0,
        cfg.wrg1
    );
    // Range-komprimieren + rückversetzen (TDBP-Eingang).
    let mut rc = w.raw;
    RangeCompressor::new(&chirp, n0).compress_rows(&mut rc);
    let shift = rda::correlation_shift_samples(ntx, n0);
    for p in 0..naz {
        rc[p * n0..(p + 1) * n0].rotate_right(shift);
    }
    let t_pre = t_start.elapsed();
    let t_focus = std::time::Instant::now();
    let img = if cfg.cpu {
        let threads = std::thread::available_parallelism()
            .map(|n| n.get())
            .unwrap_or(4);
        println!("TDBP-CPU ({threads} Stränge) …");
        let inp = sar_focus::tdbp::TdbpInput {
            data: &rc,
            npulse: naz,
            nsample: n0,
            plat: &geo.plat,
            t0_s: geo.t0_s,
            dt_s: geo.dt_s,
            lambda: sar_focus::types::TX_WAVELENGTH_M,
        };
        sar_focus::tdbp::tdbp_cpu_parallel(&geo.grid, &inp, naz, threads)
    } else {
        println!("TDBP-GPU …");
        sar_focus::gpu::TdbpGpuProcessor::new(
            geo.grid,
            naz,
            n0,
            geo.t0_s,
            geo.dt_s,
            sar_focus::types::TX_WAVELENGTH_M,
        )
        .map_err(|e| e.to_string())?
        .focus(&rc, &geo.plat, naz)
        .map_err(|e| e.to_string())?
    };
    let dt = t_focus.elapsed();
    let pp = naz as f64 * (geo.grid.naz * geo.grid.nrange) as f64;
    println!(
        "TDBP-Fokus: {:.1} s ({:.2e} Puls·Pixel, {:.1} M/s)",
        dt.as_secs_f64(),
        pp,
        pp / dt.as_secs_f64() / 1e6
    );
    println!(
        "Zeit: Dekodierung {:.1} s, Geometrie {:.1} s (gesamt {:.1} s)",
        t_decode.as_secs_f64(),
        (t_pre - t_decode).as_secs_f64(),
        t_start.elapsed().as_secs_f64()
    );
    println!("Host-Speicher (Peak): {:.2} GB", peak_rss_gb());
    let (peak_idx, peak_pw) = sar_focus::tdbp::peak_power(&img);
    println!(
        "Peak: {} (az {}, rg {}), P={:.3e}",
        peak_idx,
        cfg.waz0 + peak_idx / geo.grid.nrange,
        cfg.wrg0 + peak_idx % geo.grid.nrange,
        peak_pw
    );
    let cf_path = format!("{}.cf", cfg.prefix);
    write_cf(std::path::Path::new(&cf_path), &img)?;
    println!(
        "geschrieben: {cf_path} ({:.2} GB)",
        (img.len() * 8) as f64 / 1e9
    );
    write_quicklook(&img, geo.grid.naz, geo.grid.nrange, &cfg.prefix)?;
    // Vergleich mit RDA-Bild (Lage + Kennzahlen + registrierte Differenz).
    if let Some((path, rnaz, rn0, raz0)) = cfg.compare {
        let rda = read_cf_complex(&path, rnaz, rn0)?;
        if cfg.waz0 < raz0 {
            return Err("TDBP-Fenster vor RDA-Beginn".to_string());
        }
        let (ra0, rr0) = (cfg.waz0 - raz0, cfg.wrg0);
        if ra0 + geo.grid.naz > rnaz || rr0 + geo.grid.nrange > rn0 {
            return Err("RDA-Bild deckt TDBP-Fenster nicht ab".to_string());
        }
        let mut rcrop = Vec::with_capacity(geo.grid.naz * geo.grid.nrange);
        for a in 0..geo.grid.naz {
            let s = (ra0 + a) * rn0 + rr0;
            rcrop.extend_from_slice(&rda[s..s + geo.grid.nrange]);
        }
        let (rpeak, rpw) = sar_focus::tdbp::peak_power(&rcrop);
        let gn = geo.grid.nrange as isize;
        let daz = rpeak as isize / gn - peak_idx as isize / gn;
        let drg = rpeak as isize % gn - peak_idx as isize % gn;
        println!(
            "RDA-Peak: (az {}, rg {}), P={:.3e}; Versatz RDA−TDBP: Δaz {daz:+}, Δrg {drg:+}",
            cfg.waz0 + rpeak / geo.grid.nrange,
            cfg.wrg0 + rpeak % geo.grid.nrange,
            rpw,
        );
        // Registrierte Differenz: TDBP(a,r) ↔ RDA(a+daz,r+drg), Überlappung.
        // Norm: SpitzenAMPLITUDE (nicht Leistung — sonst 1e5 zu klein!).
        let (oaz0, oaz1) = (0.max(-daz), geo.grid.naz as isize - daz.max(0));
        let (org0, org1) = (0.max(-drg), geo.grid.nrange as isize - drg.max(0));
        let peak = peak_pw.sqrt().max(1e-30);
        let (mut acc, mut n) = (0.0f64, 0u64);
        let mut mx = 0.0f32;
        for a in oaz0..oaz1 {
            for r in org0..org1 {
                let t = img[a as usize * geo.grid.nrange + r as usize];
                let q = rcrop[(a + daz) as usize * geo.grid.nrange + (r + drg) as usize];
                let d = (t.re - q.re).hypot(t.im - q.im) / peak;
                acc += d as f64;
                n += 1;
                mx = mx.max(d);
            }
        }
        println!(
            "TDBP↔RDA registriert: max. rel. Abw. {mx:.3e}, Mittel {:.3e} (Überlapp {n} px)",
            acc / n.max(1) as f64
        );
    }
    Ok(())
}
