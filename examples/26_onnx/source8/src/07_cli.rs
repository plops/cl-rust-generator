//! `07_cli` — Argument-Parsing (std::env): Kommandos und Optionen.
//! Ausführung liegt in `11_run`.

use crate::session::Device;

pub const USAGE: &str = "\
gui_detect grab <out.ppm>
    X11-Root-Window als PPM speichern
gui_detect detect <model.onnx|embedded> <in.ppm|x11> [--out annotiert.ppm]
    [--device auto|cpu|cuda] [--threads N] [--conf 0.05] [--iou 0.7]
    Boxen als TSV (x1 y1 x2 y2 score) auf stdout, Zeiten auf stderr
gui_detect bench <in.ppm|x11> <model.onnx>... [--device ..] [--threads N]
    [--iters 30] [--warmup 5]
    Median-Zeiten pro Stufe als TSV (bei x11 inkl. Grab pro Iteration)
gui_detect live [model.onnx] [--x 0] [--y 0] [--frames 0] [--device ..] [--threads N]
    Ausschnitt in Modellgröße (Default models/gpa_640_int8.onnx → 640x640)
    fortlaufend grabben, detektieren, im eigenen Fenster zeigen; q/Esc = Ende";

/// Default-Modell des Live-Modus (640×640 → Ausschnitt ohne Skalierung).
pub const LIVE_MODEL: &str = "models/gpa_640_int8.onnx";

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
    /// Live: Ausschnitt-Ursprung und Frame-Limit (0 = endlos).
    pub x: usize,
    pub y: usize,
    pub frames: usize,
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
            x: 0,
            y: 0,
            frames: 0,
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
    Live {
        model: String,
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
            "x" => o.x = num(v)? as usize,
            "y" => o.y = num(v)? as usize,
            "frames" => o.frames = num(v)? as usize,
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
        ["live"] => Ok(Command::Live {
            model: LIVE_MODEL.into(),
            opts,
        }),
        ["live", model] => Ok(Command::Live {
            model: model.into(),
            opts,
        }),
        _ => Err(USAGE.into()),
    }
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
    fn parse_live_defaults_and_roi() {
        let Command::Live { model, opts } = parse(&s(&["live"])).unwrap() else {
            panic!()
        };
        assert_eq!(
            (model.as_str(), opts.x, opts.y, opts.frames),
            (LIVE_MODEL, 0, 0, 0)
        );
        let Command::Live { model, opts } = parse(&s(&[
            "live", "m.onnx", "--x", "100", "--y", "50", "--frames", "3",
        ]))
        .unwrap() else {
            panic!()
        };
        assert_eq!(
            (model.as_str(), opts.x, opts.y, opts.frames),
            ("m.onnx", 100, 50, 3)
        );
    }

    #[test]
    fn parse_rejects_bad_options() {
        assert!(parse(&s(&["detect", "m", "i", "--bogus", "1"])).is_err());
        assert!(parse(&s(&["detect", "m", "i", "--threads"])).is_err());
        assert!(parse(&s(&["detect", "m", "i", "--threads", "viele"])).is_err());
        assert!(parse(&s(&["detect", "m", "i", "--device", "tpu"])).is_err());
    }
}
