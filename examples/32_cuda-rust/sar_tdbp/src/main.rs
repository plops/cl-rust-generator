//! SAR-TDBP: Kommandozeile und oberste Verdrahtung.
//!
//! Keine Geschäftslogik — nur Argumente parsen und an die Bibliothek
//! (Headless-Pipeline bzw. Macroquad-GUI) übergeben.

use sar_tdbp::gui::{GuiConfig, run_gui};
use sar_tdbp::phantom::PhantomKind;
use sar_tdbp::pipeline::{HeadlessJob, run_benchmark, run_headless};
use std::path::PathBuf;

/// Betriebsmodus der Anwendung.
#[derive(Debug, Clone, PartialEq, Default)]
pub enum Mode {
    /// Interaktive Macroquad-GUI.
    #[default]
    Gui,
    /// Ohne Fenster: Pipeline laufen lassen, PNG + Kennzahlen schreiben.
    Headless { output: PathBuf },
    /// Fester CPU/GPU-Vergleichslauf (128², 256 Pulse) mit Zeit-Tabelle.
    Bench,
}

/// Vollständig geparste Konfiguration.
#[derive(Debug, Clone, PartialEq)]
pub struct Config {
    pub mode: Mode,
    pub phantom: PhantomKind,
    pub size: u32,
    pub num_pulses: u32,
    /// Anfangswert der Apertur (Pulse); `None` = alle Pulse.
    pub limit: Option<u32>,
    /// GUI: nach N Frames Screenshot speichern und beenden (Tests).
    pub frames: Option<u32>,
    pub screenshot: Option<PathBuf>,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            mode: Mode::default(),
            phantom: PhantomKind::default(),
            size: 256,
            // 1024 Pulse auf 40 m: 3,9-cm-Abtastung erfüllt das
            // Azimuth-Abtasttheorem (keine Gitterkeulen in der Szene).
            num_pulses: 1024,
            limit: None,
            frames: None,
            screenshot: None,
        }
    }
}

fn usage() -> &'static str {
    "sar_tdbp [OPTIONEN]\n\
     \n\
     Optionen:\n\
     \x20 --phantom single|grid|rust   Streuer-Szenario (default: grid)\n\
     \x20 --size N                     Bild N x N Pixel (default: 256)\n\
     \x20 --pulses N                   Anzahl Antennenpulse (default: 1024)\n\
     \x20 --limit K                    nur erste K Pulse nutzen (default: alle)\n\
     \x20 --headless OUT.png           ohne GUI rechnen, PNG schreiben\n\
     \x20 --bench                      CPU/GPU-Vergleich (feste Last)\n\
     \x20 --frames N --screenshot F    GUI: nach N Frames F speichern + beenden\n\
     \x20 --help                       diese Hilfe"
}

/// Parst `args` (ohne Programmname) in eine [`Config`].
pub fn parse_args(args: &[String]) -> Result<Config, String> {
    let mut cfg = Config::default();
    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "--help" | "-h" => return Err(usage().to_string()),
            "--phantom" => {
                i += 1;
                let v = args.get(i).ok_or("--phantom braucht einen Wert")?;
                cfg.phantom = match v.as_str() {
                    "single" => PhantomKind::Single,
                    "grid" => PhantomKind::Grid,
                    "rust" => PhantomKind::Rust,
                    _ => return Err(format!("unbekanntes Phantom: {v}")),
                };
            }
            "--size" => {
                i += 1;
                cfg.size = parse_u32(args.get(i), "--size")?;
            }
            "--pulses" => {
                i += 1;
                cfg.num_pulses = parse_u32(args.get(i), "--pulses")?;
            }
            "--limit" => {
                i += 1;
                cfg.limit = Some(parse_u32(args.get(i), "--limit")?);
            }
            "--headless" => {
                i += 1;
                let v = args.get(i).ok_or("--headless braucht einen Pfad")?;
                cfg.mode = Mode::Headless {
                    output: PathBuf::from(v),
                };
            }
            "--bench" => {
                cfg.mode = Mode::Bench;
            }
            "--frames" => {
                i += 1;
                cfg.frames = Some(parse_u32(args.get(i), "--frames")?);
            }
            "--screenshot" => {
                i += 1;
                let v = args.get(i).ok_or("--screenshot braucht einen Pfad")?;
                cfg.screenshot = Some(PathBuf::from(v));
            }
            other => return Err(format!("unbekannte Option: {other}\n{}", usage())),
        }
        i += 1;
    }
    if cfg.size == 0 || cfg.num_pulses == 0 {
        return Err("--size und --pulses müssen > 0 sein".to_string());
    }
    if cfg.frames.is_some() != cfg.screenshot.is_some() {
        return Err("--frames und --screenshot nur gemeinsam".to_string());
    }
    Ok(cfg)
}

fn parse_u32(value: Option<&String>, flag: &str) -> Result<u32, String> {
    value
        .ok_or_else(|| format!("{flag} braucht einen Wert"))?
        .parse::<u32>()
        .map_err(|_| format!("{flag} braucht eine ganze Zahl >= 0"))
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let cfg = match parse_args(&args) {
        Ok(cfg) => cfg,
        Err(msg) => {
            eprintln!("{msg}");
            std::process::exit(2);
        }
    };
    match &cfg.mode {
        Mode::Headless { output } => {
            let job = HeadlessJob {
                phantom: cfg.phantom,
                size: cfg.size,
                num_pulses: cfg.num_pulses,
                limit: cfg.limit,
                output: output.clone(),
            };
            match run_headless(&job) {
                Ok(rep) => {
                    println!(
                        "Bild {}x{}, Pulse {}/{}, Peak bei ({}, {}), Betrag {:.3}",
                        rep.width,
                        rep.height,
                        rep.pulses_used,
                        cfg.num_pulses,
                        rep.peak_px,
                        rep.peak_py,
                        rep.peak_mag
                    );
                    println!("PNG: {}", output.display());
                    print!("{}", rep.ascii);
                }
                Err(e) => {
                    eprintln!("Fehler: {e}");
                    std::process::exit(1);
                }
            }
        }
        Mode::Bench => match run_benchmark() {
            Ok(rep) => print!("{rep}"),
            Err(e) => {
                eprintln!("Fehler: {e}");
                std::process::exit(1);
            }
        },
        Mode::Gui => {
            let fut = run_gui(GuiConfig {
                phantom: cfg.phantom,
                size: cfg.size,
                num_pulses: cfg.num_pulses,
                limit: cfg.limit,
                frames: cfg.frames,
                screenshot: cfg.screenshot.clone(),
            });
            macroquad::Window::new("SAR TDBP", fut);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(words: &[&str]) -> Result<Config, String> {
        parse_args(&words.iter().map(|s| s.to_string()).collect::<Vec<_>>())
    }

    #[test]
    fn defaults() {
        let cfg = parse(&[]).unwrap();
        assert_eq!(cfg.mode, Mode::Gui);
        assert_eq!(cfg.phantom, PhantomKind::Grid);
        assert_eq!((cfg.size, cfg.num_pulses), (256, 1024));
        assert_eq!(cfg.limit, None);
    }

    #[test]
    fn headless_full_config() {
        let cfg = parse(&[
            "--phantom",
            "rust",
            "--size",
            "128",
            "--pulses",
            "64",
            "--limit",
            "32",
            "--headless",
            "out.png",
        ])
        .unwrap();
        assert_eq!(cfg.phantom, PhantomKind::Rust);
        assert_eq!((cfg.size, cfg.num_pulses), (128, 64));
        assert_eq!(cfg.limit, Some(32));
        assert_eq!(
            cfg.mode,
            Mode::Headless {
                output: PathBuf::from("out.png")
            }
        );
    }

    #[test]
    fn bench_mode() {
        let cfg = parse(&["--bench"]).unwrap();
        assert_eq!(cfg.mode, Mode::Bench);
    }

    #[test]
    fn rejects_inconsistent() {
        assert!(parse(&["--frames", "10"]).is_err());
        assert!(parse(&["--size", "0"]).is_err());
        assert!(parse(&["--phantom", "haus"]).is_err());
        assert!(parse(&["--nope"]).is_err());
    }
}
