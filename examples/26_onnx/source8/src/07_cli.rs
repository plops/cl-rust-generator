//! `07_cli` — Argument-Parsing (std::env) und Ausführung der Kommandos.

use crate::bench;
use crate::capture::Screen;
use crate::detector::Detector;
use crate::image::Rgb;
use crate::session::{Device, Model};
use std::fs::File;
use std::io::{BufReader, BufWriter};

pub const USAGE: &str = "\
gui_detect grab <out.ppm>
    X11-Root-Window als PPM speichern
gui_detect detect <model.onnx|embedded> <in.ppm|x11> [--out annotiert.ppm]
    [--device auto|cpu|cuda] [--threads N] [--conf 0.05] [--iou 0.7]
    Boxen als TSV (x1 y1 x2 y2 score) auf stdout, Zeiten auf stderr
gui_detect bench <in.ppm|x11> <model.onnx>... [--device ..] [--threads N]
    [--iters 30] [--warmup 5]
    Median-Zeiten pro Stufe als TSV (bei x11 inkl. Grab pro Iteration)";

/// Optionen, die mehrere Kommandos teilen.
#[derive(Debug, PartialEq)]
pub struct Opts {
    pub device: Device,
    pub threads: usize,
    pub conf: f32,
    pub iou: f32,
    pub iters: usize,
    pub warmup: usize,
    pub out: Option<String>,
}

impl Default for Opts {
    fn default() -> Self {
        Self {
            device: Device::Auto,
            threads: 0,
            conf: crate::detector::CONF,
            iou: crate::detector::IOU,
            iters: 30,
            warmup: 5,
            out: None,
        }
    }
}

/// Geparstes Kommando.
#[derive(Debug, PartialEq)]
pub enum Command {
    Grab {
        out: String,
    },
    Detect {
        model: String,
        input: String,
        opts: Opts,
    },
    Bench {
        input: String,
        models: Vec<String>,
        opts: Opts,
    },
}

/// Trennt Positionsargumente von `--key value`-Optionen.
fn split(args: &[String]) -> Result<(Vec<String>, Opts), String> {
    let mut pos = Vec::new();
    let mut o = Opts::default();
    let mut it = args.iter();
    while let Some(a) = it.next() {
        let Some(key) = a.strip_prefix("--") else {
            pos.push(a.clone());
            continue;
        };
        let v = it.next().ok_or(format!("--{key}: Wert fehlt"))?;
        let num = |v: &str| {
            v.parse::<f64>()
                .map_err(|_| format!("--{key}: keine Zahl '{v}'"))
        };
        match key {
            "device" => o.device = Device::parse(v)?,
            "threads" => o.threads = num(v)? as usize,
            "conf" => o.conf = num(v)? as f32,
            "iou" => o.iou = num(v)? as f32,
            "iters" => o.iters = (num(v)? as usize).max(1),
            "warmup" => o.warmup = num(v)? as usize,
            "out" => o.out = Some(v.clone()),
            _ => return Err(format!("unbekannte Option --{key}\n{USAGE}")),
        }
    }
    Ok((pos, o))
}

/// Parst `args` ohne Programmnamen.
pub fn parse(args: &[String]) -> Result<Command, String> {
    let (pos, opts) = split(args)?;
    match pos.iter().map(String::as_str).collect::<Vec<_>>()[..] {
        ["grab", out] => Ok(Command::Grab { out: out.into() }),
        ["detect", model, input] => Ok(Command::Detect {
            model: model.into(),
            input: input.into(),
            opts,
        }),
        ["bench", input, ref models @ ..] if !models.is_empty() => Ok(Command::Bench {
            input: input.into(),
            models: models.iter().map(|m| (*m).to_string()).collect(),
            opts,
        }),
        _ => Err(USAGE.into()),
    }
}

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

/// Führt ein Kommando aus.
pub fn run(cmd: Command) -> Result<(), String> {
    match cmd {
        Command::Grab { out } => {
            let img = Screen::open()?.grab()?;
            save(&img, &out)?;
            println!("grab {}x{} -> {out}", img.w, img.h);
        }
        Command::Detect { model, input, opts } => {
            let (mut img, _) = load_input(&input)?;
            let m = Model::load(&model_bytes(&model)?, opts.device, opts.threads)?;
            let mut det = Detector::new(m);
            (det.conf, det.iou) = (opts.conf, opts.iou);
            let (dets, t) = det.detect(&img)?;
            for d in &dets {
                println!(
                    "{:.2}\t{:.2}\t{:.2}\t{:.2}\t{:.3}",
                    d.b[0], d.b[1], d.b[2], d.b[3], d.score
                );
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
                for d in dets.iter().filter(|d| d.score >= 0.25) {
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
            println!("{}", bench::HEADER);
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
                println!("{}", row.tsv());
            }
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn s(v: &[&str]) -> Vec<String> {
        v.iter().map(|x| (*x).to_string()).collect()
    }

    #[test]
    fn parse_grab() {
        assert_eq!(
            parse(&s(&["grab", "a.ppm"])),
            Ok(Command::Grab {
                out: "a.ppm".into()
            })
        );
        assert!(parse(&s(&["grab"])).is_err());
        assert!(parse(&s(&[])).is_err());
    }

    #[test]
    fn parse_detect_with_options() {
        let c = parse(&s(&[
            "detect", "m.onnx", "x11", "--device", "cpu", "--conf", "0.3", "--out", "o.ppm",
        ]))
        .unwrap();
        let Command::Detect { model, input, opts } = c else {
            panic!()
        };
        assert_eq!((model.as_str(), input.as_str()), ("m.onnx", "x11"));
        assert_eq!(
            (opts.device, opts.conf, opts.out.as_deref()),
            (Device::Cpu, 0.3, Some("o.ppm"))
        );
        assert_eq!(opts.iou, crate::detector::IOU);
    }

    #[test]
    fn parse_bench_needs_model() {
        assert!(parse(&s(&["bench", "a.ppm"])).is_err());
        let Command::Bench { models, opts, .. } = parse(&s(&[
            "bench",
            "a.ppm",
            "x.onnx",
            "y.onnx",
            "--threads",
            "8",
        ]))
        .unwrap() else {
            panic!()
        };
        assert_eq!((models.len(), opts.threads), (2, 8));
    }

    #[test]
    fn parse_rejects_bad_options() {
        assert!(parse(&s(&["detect", "m", "i", "--bogus", "1"])).is_err());
        assert!(parse(&s(&["detect", "m", "i", "--threads"])).is_err());
        assert!(parse(&s(&["detect", "m", "i", "--threads", "viele"])).is_err());
        assert!(parse(&s(&["detect", "m", "i", "--device", "tpu"])).is_err());
    }
}
