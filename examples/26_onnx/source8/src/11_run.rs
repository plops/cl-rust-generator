//! `11_run` — Ausführung der geparsten Kommandos (grab/detect/bench/live)
//! samt Datei-/Modell-I/O und stdout-Ausgabe.

use crate::bench;
use crate::capture::Screen;
use crate::cli::Command;
use crate::detector::{Detector, SHOW};
use crate::image::Rgb;
use crate::live::{self, LiveCfg};
use crate::session::Model;
use std::fs::File;
use std::io::{BufReader, BufWriter, Write};

/// Modell-Bytes: Datei oder (Feature `embed`) eingebettetes INT8-Modell.
fn model_bytes(path: &str) -> Result<Vec<u8>, String> {
    #[cfg(feature = "embed")]
    if path == "embedded" {
        return Ok(include_bytes!("../models/gpa_384x640_int8.onnx").to_vec());
    }
    std::fs::read(path).map_err(|e| format!("{path}: {e}"))
}

/// Bildquelle: PPM-Datei oder `x11` (liefert zusätzlich den Screen).
fn load_input(input: &str) -> Result<(Rgb, Option<Screen>), String> {
    if input == "x11" {
        let s = Screen::open()?;
        return Ok((s.grab()?, Some(s)));
    }
    let f = File::open(input).map_err(|e| format!("{input}: {e}"))?;
    Ok((
        Rgb::read_ppm(BufReader::new(f)).map_err(|e| format!("{input}: {e}"))?,
        None,
    ))
}

fn save(img: &Rgb, out: &str) -> Result<(), String> {
    let f = File::create(out).map_err(|e| format!("{out}: {e}"))?;
    img.write_ppm(BufWriter::new(f))
        .map_err(|e| format!("{out}: {e}"))
}

/// Eine Zeile nach stdout. Anders als `println!` wird ein geschlossener
/// Pipe (`| head`) zum Fehler statt zur Panik (panic=abort → Exit 134).
fn emit(line: &str) -> Result<(), String> {
    writeln!(std::io::stdout(), "{line}").map_err(|e| format!("stdout: {e}"))
}

/// Führt ein Kommando aus.
pub fn run(cmd: Command) -> Result<(), String> {
    match cmd {
        Command::Grab { out } => {
            let img = Screen::open()?.grab()?;
            save(&img, &out)?;
            emit(&format!("grab {}x{} -> {out}", img.w, img.h))?;
        }
        Command::Detect { model, input, opts } => {
            let (mut img, _) = load_input(&input)?;
            let m = Model::load(&model_bytes(&model)?, opts.device, opts.threads)?;
            let mut det = Detector::new(m);
            (det.conf, det.iou) = (opts.conf, opts.iou);
            let (dets, t) = det.detect(&img)?;
            for d in &dets {
                emit(&format!(
                    "{:.2}\t{:.2}\t{:.2}\t{:.2}\t{:.3}",
                    d.b[0], d.b[1], d.b[2], d.b[3], d.score
                ))?;
            }
            eprintln!(
                "{} boxen, provider {}, pre {:.1} ms, infer {:.1} ms, post {:.1} ms",
                dets.len(),
                det.model.provider,
                t.pre,
                t.infer,
                t.post
            );
            if let Some(out) = opts.out {
                for d in dets.iter().filter(|d| d.score >= SHOW) {
                    img.draw_rect(d.b[0], d.b[1], d.b[2], d.b[3], 2, [255, 0, 255]);
                }
                save(&img, &out)?;
            }
        }
        Command::Bench {
            input,
            models,
            opts,
        } => {
            let (img, screen) = load_input(&input)?;
            emit(bench::HEADER)?;
            for path in models {
                let bytes = model_bytes(&path)?;
                let mut det = Detector::new(Model::load(&bytes, opts.device, opts.threads)?);
                let mut row = bench::run(&mut det, &img, screen.as_ref(), opts.warmup, opts.iters)?;
                row.name = path
                    .rsplit('/')
                    .next()
                    .unwrap_or(&path)
                    .trim_end_matches(".onnx")
                    .into();
                row.threads = opts.threads;
                row.mbytes = bytes.len() as f64 / 1e6;
                emit(&row.tsv())?;
            }
        }
        Command::Live { model, opts } => {
            let screen = Screen::open()?;
            let m = Model::load(&model_bytes(&model)?, opts.device, opts.threads)?;
            let mut det = Detector::new(m);
            (det.conf, det.iou) = (opts.conf, opts.iou);
            let cfg = LiveCfg {
                x: opts.x,
                y: opts.y,
                frames: opts.frames,
            };
            let st = live::run(&mut det, &screen, cfg)?;
            emit(&format!(
                "live frames={} mean_ms={:.1} boxes={} provider={}",
                st.frames, st.mean_ms, st.last_boxes, det.model.provider
            ))?;
        }
    }
    Ok(())
}
