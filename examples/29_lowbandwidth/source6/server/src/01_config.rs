//! `01_config` — Kommandozeile des Servers (handgeparst, keine CLI-Crate).

use std::time::Duration;

use lbw_common::DEFAULT_PORT;

use crate::analyze::ModelPaths;
use crate::pipeline::PipeCfg;

pub const USAGE: &str = "\
lbw-server — Low-Bandwidth-Remote-Desktop (X11 → OCR/AV1 → TCP)

  --listen ADDR     Adresse (Default 127.0.0.1:7878; nur lokal, Zugriff per ssh -L/-R)
  --display D       X11-Display (Default $DISPLAY)
  --x N --y N       linke obere Ecke des Ausschnitts (Default 0 0)
  --size N          Kantenlänge des Ausschnitts (Default 640, Vielfaches von 32)
  --rate B          Sendebudget in Byte/s (Default 6000)
  --models DIR      Modellverzeichnis (Default ./models)
  --gui PATH|none   GUI-Detektor (Default DIR/gpa_640_int8.onnx)
  --threads N       ONNX-Threads (Default 8)
  --q-bg N --q-icon N  AV1-Quantizer Hintergrund/Icons (Default 180/110)
  --dead-after S    Verbindung tot nach S s ohne Daten (Default 90)
  --no-input        Eingaben des Clients ignorieren
  -v, --verbose     Pipeline-Log
";

/// Vollständige Server-Konfiguration.
#[derive(Clone, Debug)]
pub struct Config {
    pub listen: String,
    pub display: Option<String>,
    pub x: usize,
    pub y: usize,
    pub size: usize,
    pub rate: u32,
    pub models: ModelPaths,
    pub pipe: PipeCfg,
    pub dead_after: Duration,
    pub input: bool,
}

fn num<T: std::str::FromStr>(
    it: &mut impl Iterator<Item = String>,
    flag: &str,
) -> Result<T, String> {
    it.next()
        .and_then(|v| v.parse().ok())
        .ok_or_else(|| format!("{flag}: Zahl erwartet"))
}

impl Config {
    /// Parst `args` (ohne Programmnamen). `Err` enthält Meldung oder Hilfe.
    pub fn parse(args: impl IntoIterator<Item = String>) -> Result<Self, String> {
        let mut c = Config {
            listen: format!("127.0.0.1:{DEFAULT_PORT}"),
            display: None,
            x: 0,
            y: 0,
            size: 640,
            rate: 6000,
            models: ModelPaths {
                det: String::new(),
                rec: String::new(),
                dict: String::new(),
                gui: None,
                threads: 8,
            },
            pipe: PipeCfg::default(),
            dead_after: Duration::from_secs(90),
            input: true,
        };
        let (mut dir, mut gui) = ("models".to_owned(), None::<String>);
        let mut it = args.into_iter();
        while let Some(a) = it.next() {
            match a.as_str() {
                "--listen" => c.listen = it.next().ok_or("--listen: Adresse fehlt")?,
                "--display" => c.display = it.next(),
                "--x" => c.x = num(&mut it, "--x")?,
                "--y" => c.y = num(&mut it, "--y")?,
                "--size" => c.size = num(&mut it, "--size")?,
                "--rate" => c.rate = num(&mut it, "--rate")?,
                "--models" => dir = it.next().ok_or("--models: Pfad fehlt")?,
                "--gui" => gui = it.next(),
                "--threads" => c.models.threads = num(&mut it, "--threads")?,
                "--q-bg" => c.pipe.q_bg = num(&mut it, "--q-bg")?,
                "--q-icon" => c.pipe.q_icon = num(&mut it, "--q-icon")?,
                "--dead-after" => c.dead_after = Duration::from_secs(num(&mut it, "--dead-after")?),
                "--no-input" => c.input = false,
                "-v" | "--verbose" => c.pipe.verbose = true,
                "-h" | "--help" => return Err(USAGE.into()),
                _ => return Err(format!("unbekannte Option {a}\n\n{USAGE}")),
            }
        }
        if c.size == 0 || !c.size.is_multiple_of(32) || c.size > 4096 {
            return Err("--size muss ein Vielfaches von 32 sein".into());
        }
        c.models.det = format!("{dir}/PP-OCRv6_small_det.onnx");
        c.models.rec = format!("{dir}/PP-OCRv6_small_rec.onnx");
        c.models.dict = format!("{dir}/inference.yml");
        c.models.gui = match gui.as_deref() {
            Some("none") => None,
            Some(p) => Some(p.to_owned()),
            None => Some(format!("{dir}/gpa_640_int8.onnx")),
        };
        Ok(c)
    }

    /// Bindet die Adresse nicht an localhost?
    #[must_use]
    pub fn is_public(&self) -> bool {
        !(self.listen.starts_with("127.")
            || self.listen.starts_with("[::1]")
            || self.listen.starts_with("localhost"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn p(s: &str) -> Result<Config, String> {
        Config::parse(s.split_whitespace().map(str::to_owned))
    }

    #[test]
    fn defaults_are_local_and_640() {
        let c = p("").unwrap();
        assert_eq!(
            (c.listen.as_str(), c.size, c.rate),
            ("127.0.0.1:7878", 640, 6000)
        );
        assert!(!c.is_public());
        assert_eq!(c.models.gui.as_deref(), Some("models/gpa_640_int8.onnx"));
    }

    #[test]
    fn options_are_parsed() {
        let c =
            p("--listen 0.0.0.0:9 --x 10 --y 20 --rate 3000 --models /m --gui none -v --no-input")
                .unwrap();
        assert!(c.is_public());
        assert_eq!((c.x, c.y, c.rate), (10, 20, 3000));
        assert_eq!(c.models.det, "/m/PP-OCRv6_small_det.onnx");
        assert!(c.models.gui.is_none() && c.pipe.verbose && !c.input);
    }

    #[test]
    fn bad_input_is_rejected() {
        assert!(p("--size 100").is_err());
        assert!(p("--rate x").is_err());
        assert!(p("--bogus").is_err());
    }
}
