//! 12 — CLI: Argumente, Konfiguration, `run()`-Verdrahtung.
//!
//! `CliConfig` lebt jetzt hier (in `source1` lag sie in `01_types.rs` und
//! zog damit die Ansichts-Schicht in die unterste Typ-Schicht).

use std::ffi::OsString;
use std::io::{Read, Write};
use std::path::PathBuf;

use crate::model::Source;
use crate::types::{Analysis, Error};

/// Eingabe: Datei oder stdin (`-`).
#[derive(Debug, Clone, PartialEq)]
pub enum Input {
    /// Von stdin lesen.
    Stdin,
    /// Aus Datei lesen.
    Path(PathBuf),
}

/// Ausgabe: Datei oder stdout (`-`).
#[derive(Debug, Clone, PartialEq)]
pub enum Output {
    /// Nach stdout schreiben.
    Stdout,
    /// In Datei schreiben.
    Path(PathBuf),
}

/// Geparste CLI-Argumente.
#[derive(Debug, Clone, PartialEq)]
pub struct CliConfig {
    /// Eingabedatei.
    pub input: Input,
    /// PNG-Ausgabe (Default: Eingabe-Stamm + `.png`).
    pub png_out: Output,
    /// JSON-Ausgabe (optional).
    pub json_out: Option<Output>,
    /// Plot-Breite (`None` = automatisch).
    pub width: Option<u32>,
    /// Wasserfall-Quelle (Default: Spektrum).
    pub view: Source,
    /// Fenster anzeigen.
    pub show: bool,
    /// Fenster-Frames (Test-Haken, Default `u64::MAX`).
    pub show_frames: u64,
    /// Report unterdruecken (Default: Report wird gedruckt).
    pub quiet: bool,
}

/// Hilfe-Text.
pub const HELP: &str = "\
opus-vis: Opus-Datei bis zum letzten Bit visualisieren und erklaeren

aufruf: opus-vis [OPTIONEN] <datei.opus|->

optionen:
  -o DATEI       PNG-Ausgabe (Default: <stamm>.png, - = stdout)
  --json DATEI   JSON-Metadaten schreiben (- = stdout)
  --width N      Plot-Breite in px (64-4096, Default: automatisch)
  --view NAME    ansicht: spektrum|bitstrom|divergenz|dichte (1-4)
  --show         Bild zusaetzlich im X11-Fenster zeigen (Feature show)
  --frames N     Fenster nach N Frames schliessen (Test-Haken)
  --quiet        keinen Text-Report drucken (Default: Report an)
  -h, --help     diese Hilfe

jeder lauf druckt einen deutschen text-report (kopf, encoder,
signal, einordnung) — nach stdout, bei `-o -`/`--json -` nach
stderr, damit binaere ausgaben sauber bleiben.

fenster-steuerung (--show):
  rad            zeit-zoom um cursor    links-drag  verschieben
  +/-            zoom um mitte          pfeile      verschieben
  1-4            ansicht wechseln       R           gesamtansicht
  ESC            ende                   hover       details + bit-budget
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
pub fn parse(args: impl Iterator<Item = OsString>) -> Result<Action, Error> {
    let mut args = args.peekable();
    let _argv0 = args.next();
    let mut input: Option<Input> = None;
    let mut png_out: Option<Output> = None;
    let mut json_out: Option<Output> = None;
    let mut width: Option<u32> = None;
    let mut view = Source::Spectrum;
    let mut show = false;
    let mut show_frames = u64::MAX;
    let mut quiet = false;
    while let Some(arg) = args.next() {
        let s = arg.to_string_lossy().into_owned();
        match s.as_str() {
            "-h" | "--help" => return Ok(Action::Help),
            "-o" => png_out = Some(output(value(&mut args, "-o")?)?),
            "--json" => json_out = Some(output(value(&mut args, "--json")?)?),
            "--width" => {
                let v = value(&mut args, "--width")?;
                width = Some(
                    v.parse::<u32>()
                        .map_err(|_| Error::Usage("--width braucht eine Zahl".into()))?,
                );
            }
            "--view" => {
                let v = value(&mut args, "--view")?;
                view = Source::parse(&v).ok_or_else(|| {
                    Error::Usage("--view: spektrum|bitstrom|divergenz|dichte erwartet".into())
                })?;
            }
            "--show" => show = true,
            "--quiet" => quiet = true,
            "--frames" => {
                let v = value(&mut args, "--frames")?;
                show_frames = v
                    .parse::<u64>()
                    .map_err(|_| Error::Usage("--frames braucht eine Zahl".into()))?;
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
        view,
        show,
        show_frames,
        quiet,
    }))
}

/// Optionswert holen oder Usage-Fehler.
fn value(args: &mut impl Iterator<Item = OsString>, flag: &str) -> Result<String, Error> {
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
    // Genau ein Ogg-Durchlauf: alle Stufen teilen sich die Paketliste.
    let ps = crate::analyze::read_packets(&bytes, total)?;
    let analysis = crate::analyze::analyze_packets(&ps)?;
    let pcm = crate::decode::decode_packets(&ps)?;
    let cols = crate::spectrum::stft(&pcm.samples);
    // Bitstrom-Walk: jedes Paket bis zum letzten Bit.
    let mut walker = crate::walk::Walker::new();
    let walks = ps
        .packets
        .iter()
        .map(|p| walker.packet(&p.data))
        .collect::<Result<Vec<_>, _>>()?;
    let model = crate::model::Model::build(&walks, usize::from(ps.head.pre_skip));
    drop(walks);
    // Fakten einmal berechnen (Report + JSON teilen sie sich).
    let transient: Vec<bool> = model.hops.iter().map(|h| h.transient).collect();
    let fakten = crate::fakten::Fakten {
        head: crate::fakten::head_fakten(&ps.head),
        tags: crate::fakten::tag_fakten(&ps.tags),
        encoder: crate::fakten::encoder_fakten(&analysis, &transient),
        pcm: crate::fakten::pcm_fakten(&pcm.samples, &cols, &pcm.stats),
        silk: crate::fakten::silk_fakten(&model.hops),
        bits: crate::fakten::bit_fakten(&model.packets, &pcm.final_range),
    };
    let src = crate::model::Sources::build(cols, model);
    let width = cfg
        .width
        .unwrap_or_else(|| (src.spectrum.len() as u32).clamp(320, 4096));
    let img = crate::render::render(&analysis, &src, cfg.view, width.clamp(64, 4096));
    let png = crate::png::encode_rgb(img.width, img.height, &img.rgb);
    write_output(&cfg.png_out, &png)?;
    if let Some(json_out) = &cfg.json_out {
        let text = crate::json::json(&analysis, &fakten, &src.model);
        write_output(json_out, text.as_bytes())?;
    }
    if !cfg.quiet {
        let name = match &cfg.input {
            Input::Stdin => "(stdin)".to_string(),
            Input::Path(p) => p.display().to_string(),
        };
        let text = crate::report::report(&name, &analysis, &fakten);
        // Binaere stdout-Ausgaben bleiben sauber: Report nach stderr.
        let binary_stdout =
            matches!(cfg.png_out, Output::Stdout) || matches!(cfg.json_out, Some(Output::Stdout));
        if binary_stdout {
            eprint!("{text}");
        } else {
            print!("{text}");
        }
    }
    if cfg.show {
        show_viewer(&cfg, &analysis, &src)?;
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

/// Viewer starten (nur mit Feature `show` verfuegbar).
#[cfg(feature = "show")]
fn show_viewer(
    cfg: &CliConfig,
    analysis: &Analysis,
    src: &crate::model::Sources,
) -> Result<(), Error> {
    let title = match &cfg.input {
        Input::Stdin => "opus-vis (stdin)".to_string(),
        Input::Path(p) => format!("opus-vis: {}", p.display()),
    };
    crate::show::show_interactive(analysis, src, cfg.view, &title, cfg.show_frames)
}

/// Viewer ohne Feature → Hinweis-Fehler.
#[cfg(not(feature = "show"))]
fn show_viewer(
    _cfg: &CliConfig,
    _analysis: &Analysis,
    _src: &crate::model::Sources,
) -> Result<(), Error> {
    Err(Error::Usage(
        "--show braucht den Build mit --features show".into(),
    ))
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
            "opus-vis", "-o", "a.png", "--json", "b.json", "--width", "500", "--view", "dichte",
            "--show", "--frames", "7", "--quiet", "in.opus",
        ]));
        let Action::Run(cfg) = a.unwrap() else {
            panic!("kein Run")
        };
        assert_eq!(cfg.png_out, Output::Path(PathBuf::from("a.png")));
        assert_eq!(cfg.json_out, Some(Output::Path(PathBuf::from("b.json"))));
        assert_eq!(cfg.width, Some(500));
        assert_eq!(cfg.view, Source::Density);
        assert!(cfg.show);
        assert_eq!(cfg.show_frames, 7);
        assert!(cfg.quiet);
        assert_eq!(cfg.input, Input::Path(PathBuf::from("in.opus")));
    }

    #[test]
    fn view_defaults_and_rejects_garbage() {
        let Action::Run(cfg) = parse(args(&["opus-vis", "x.opus"])).unwrap() else {
            panic!("kein Run")
        };
        assert_eq!(cfg.view, Source::Spectrum);
        let Action::Run(cfg) = parse(args(&["opus-vis", "--view", "3", "x.opus"])).unwrap() else {
            panic!("kein Run")
        };
        assert_eq!(cfg.view, Source::Divergence);
        assert!(matches!(
            parse(args(&["opus-vis", "--view", "falsch", "x.opus"])),
            Err(Error::Usage(_))
        ));
        assert!(matches!(
            parse(args(&["opus-vis", "--view"])),
            Err(Error::Usage(_))
        ));
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
        let Action::Run(cfg) = parse(args(&["opus-vis", "x.opus", "-o", "-"])).unwrap() else {
            panic!("kein Run")
        };
        assert_eq!(cfg.png_out, Output::Stdout);
    }

    #[test]
    fn help_and_errors() {
        assert!(matches!(parse(args(&["opus-vis", "-h"])), Ok(Action::Help)));
        assert!(matches!(parse(args(&["opus-vis"])), Err(Error::Usage(_))));
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
