//! CLI: Argumente, JSON-Ausgabe, `run()`-Verdrahtung.

use std::ffi::OsString;
use std::io::{Read, Write};
use std::path::PathBuf;

use crate::types::{Analysis, CliConfig, Error, Input, Output};

/// Hilfe-Text.
pub const HELP: &str = "\
opus-vis: Opus-Datei visualisieren

aufruf: opus-vis [OPTIONEN] <datei.opus|->

optionen:
  -o DATEI       PNG-Ausgabe (Default: <stamm>.png, - = stdout)
  --json DATEI   JSON-Metadaten schreiben (- = stdout)
  --width N      Plot-Breite in px (64-4096, Default: automatisch)
  --show         Bild zusaetzlich im X11-Fenster zeigen (Feature show)
  --frames N     Fenster nach N Frames schliessen (Test-Haken)
  -h, --help     diese Hilfe
";

/// Parse-Ergebnis: Hilfe oder Konfiguration.
#[derive(Debug, Clone, PartialEq)]
pub enum Action {
    /// Hilfe anzeigen, Exit 0.
    Help,
    /// Normal laufen.
    Run(CliConfig),
}

/// Argumente parsen (reine Funktion, gut testbar).
pub fn parse(
    args: impl Iterator<Item = OsString>,
) -> Result<Action, Error> {
    let mut args = args.peekable();
    let _argv0 = args.next();
    let mut input: Option<Input> = None;
    let mut png_out: Option<Output> = None;
    let mut json_out: Option<Output> = None;
    let mut width: Option<u32> = None;
    let mut show = false;
    let mut show_frames = u64::MAX;
    while let Some(arg) = args.next() {
        let s = arg.to_string_lossy().into_owned();
        match s.as_str() {
            "-h" | "--help" => return Ok(Action::Help),
            "-o" => png_out = Some(output(value(&mut args, "-o")?)?),
            "--json" => json_out = Some(output(value(&mut args, "--json")?)?),
            "--width" => {
                let v = value(&mut args, "--width")?;
                width = Some(v.parse::<u32>().map_err(|_| {
                    Error::Usage("--width braucht eine Zahl".into())
                })?);
            }
            "--show" => show = true,
            "--frames" => {
                let v = value(&mut args, "--frames")?;
                show_frames = v.parse::<u64>().map_err(|_| {
                    Error::Usage("--frames braucht eine Zahl".into())
                })?;
            }
            s if s.starts_with('-') && s.len() > 1 => {
                return Err(Error::Usage(format!("unbekannte Option: {s}")));
            }
            _ => {
                if input.is_some() {
                    return Err(Error::Usage("nur eine Eingabe erlaubt".into()));
                }
                input = Some(if s == "-" {
                    Input::Stdin
                } else {
                    Input::Path(PathBuf::from(s))
                });
            }
        }
    }
    let input = input.ok_or_else(|| Error::Usage("Eingabe fehlt".into()))?;
    if png_out.is_none() {
        png_out = Some(default_png(&input));
    }
    Ok(Action::Run(CliConfig {
        input,
        png_out: png_out.unwrap(),
        json_out,
        width,
        show,
        show_frames,
    }))
}

/// Optionswert holen oder Usage-Fehler.
fn value(
    args: &mut impl Iterator<Item = OsString>,
    flag: &str,
) -> Result<String, Error> {
    match args.next() {
        Some(v) => Ok(v.to_string_lossy().into_owned()),
        None => Err(Error::Usage(format!("{flag} braucht einen Wert"))),
    }
}

/// `-` → stdout, sonst Pfad.
fn output(s: String) -> Result<Output, Error> {
    if s == "-" {
        Ok(Output::Stdout)
    } else if s.is_empty() {
        Err(Error::Usage("leerer Ausgabepfad".into()))
    } else {
        Ok(Output::Path(PathBuf::from(s)))
    }
}

/// Default-PNG: Eingabe-Stamm + `.png` (stdin → `out.png`).
fn default_png(input: &Input) -> Output {
    match input {
        Input::Stdin => Output::Path(PathBuf::from("out.png")),
        Input::Path(p) => Output::Path(p.with_extension("png")),
    }
}

/// Analyse als JSON (hand-gerollt, keine Strings ausser Enums).
#[must_use]
pub fn json(a: &Analysis) -> String {
    let mut o = String::from("{\"frames\":[");
    for (i, f) in a.frames.iter().enumerate() {
        if i > 0 {
            o.push(',');
        }
        o.push_str(&format!(
            "{{\"i\":{},\"t_ms\":{:.3},\"dur_ms\":{:.3},\
             \"mode\":\"{}\",\"bw\":\"{}\",\"ch\":{},\
             \"bytes\":{},\"kbps\":{:.3}}}",
            f.index,
            f.t_start_ms,
            f.duration_ms,
            f.mode,
            f.bandwidth.short(),
            f.channels,
            f.bytes,
            f.kbps,
        ));
    }
    let s = &a.summary;
    o.push_str(&format!(
        "],\"summary\":{{\"frames\":{},\"dur_ms\":{:.3},\
         \"audio_bytes\":{},\"total_bytes\":{},\"avg_kbps\":{:.3},\
         \"raw_kbps\":{:.1},\"ratio\":{:.3},\"overhead_pct\":{:.2},\
         \"silk\":{:.4},\"hybrid\":{:.4},\"celt\":{:.4},\
         \"channels\":{}}}}}",
        s.frames,
        s.duration_ms,
        s.audio_bytes,
        s.total_bytes,
        s.avg_kbps,
        s.raw_kbps,
        s.ratio,
        s.overhead_pct,
        s.frac_silk,
        s.frac_hybrid,
        s.frac_celt,
        s.channels,
    ));
    o
}

/// End-zu-End-Lauf: lesen → analysieren → decodieren → rendern.
pub fn run(args: impl Iterator<Item = OsString>) -> Result<(), Error> {
    let cfg = match parse(args)? {
        Action::Help => {
            print!("{HELP}");
            return Ok(());
        }
        Action::Run(cfg) => cfg,
    };
    let bytes = match &cfg.input {
        Input::Stdin => {
            let mut buf = Vec::new();
            std::io::stdin().read_to_end(&mut buf)?;
            buf
        }
        Input::Path(p) => std::fs::read(p)?,
    };
    let total = bytes.len() as u64;
    let analysis = crate::analyze::analyze_reader(&bytes[..], total)?;
    let pcm = crate::decode::decode_mono(&bytes)?;
    let cols = crate::spectrum::stft(&pcm.samples);
    let width = cfg.width.unwrap_or_else(|| {
        (cols.len() as u32).clamp(320, 4096)
    });
    let img = crate::render::render(&analysis, &cols, width.clamp(64, 4096));
    let png = crate::png::encode_rgb(img.width, img.height, &img.rgb);
    write_output(&cfg.png_out, &png)?;
    if let Some(json_out) = &cfg.json_out {
        write_output(json_out, json(&analysis).as_bytes())?;
    }
    let s = &analysis.summary;
    eprintln!(
        "frames={} dauer={:.1}s avg={:.1}kbps roh={:.1}x",
        s.frames,
        s.duration_ms / 1000.0,
        s.avg_kbps,
        s.ratio,
    );
    if cfg.show {
        show_image(&img, &cfg)?;
    }
    Ok(())
}

/// Bytes in Datei oder stdout schreiben.
fn write_output(out: &Output, bytes: &[u8]) -> Result<(), Error> {
    match out {
        Output::Stdout => {
            std::io::stdout().write_all(bytes)?;
        }
        Output::Path(p) => {
            std::fs::write(p, bytes)?;
        }
    }
    Ok(())
}

/// Fenster anzeigen (nur mit Feature `show` verfuegbar).
#[cfg(feature = "show")]
fn show_image(
    img: &crate::render::Image,
    cfg: &CliConfig,
) -> Result<(), Error> {
    crate::show::show(img, "opus-vis", cfg.show_frames)
}

/// Fenster anzeigen ohne Feature → Hinweis-Fehler.
#[cfg(not(feature = "show"))]
fn show_image(
    _img: &crate::render::Image,
    _cfg: &CliConfig,
) -> Result<(), Error> {
    Err(Error::Usage("--show braucht den Build mit --features show".into()))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn args(v: &[&str]) -> impl Iterator<Item = OsString> + use<> {
        v.iter().map(OsString::from).collect::<Vec<_>>().into_iter()
    }

    #[test]
    fn full_flags_parse() {
        let a = parse(args(&[
            "opus-vis", "-o", "a.png", "--json", "b.json", "--width", "500",
            "--show", "--frames", "7", "in.opus",
        ]));
        let Action::Run(cfg) = a.unwrap() else { panic!("kein Run") };
        assert_eq!(cfg.png_out, Output::Path(PathBuf::from("a.png")));
        assert_eq!(cfg.json_out, Some(Output::Path(PathBuf::from("b.json"))));
        assert_eq!(cfg.width, Some(500));
        assert!(cfg.show);
        assert_eq!(cfg.show_frames, 7);
        assert_eq!(cfg.input, Input::Path(PathBuf::from("in.opus")));
    }

    #[test]
    fn defaults_and_stdin_dash() {
        let Action::Run(cfg) = parse(args(&["opus-vis", "-"])).unwrap() else {
            panic!("kein Run")
        };
        assert_eq!(cfg.input, Input::Stdin);
        assert_eq!(cfg.png_out, Output::Path(PathBuf::from("out.png")));
        assert_eq!(cfg.json_out, None);
        assert!(!cfg.show);
        let Action::Run(cfg) =
            parse(args(&["opus-vis", "x.opus", "-o", "-"])).unwrap()
        else {
            panic!("kein Run")
        };
        assert_eq!(cfg.png_out, Output::Stdout);
    }

    #[test]
    fn help_and_errors() {
        assert!(matches!(parse(args(&["opus-vis", "-h"])), Ok(Action::Help)));
        assert!(matches!(
            parse(args(&["opus-vis"])),
            Err(Error::Usage(_))
        ));
        assert!(matches!(
            parse(args(&["opus-vis", "--nope", "x"])),
            Err(Error::Usage(_))
        ));
        assert!(matches!(
            parse(args(&["opus-vis", "a", "b"])),
            Err(Error::Usage(_))
        ));
        assert!(matches!(
            parse(args(&["opus-vis", "-o"])),
            Err(Error::Usage(_))
        ));
    }
}
