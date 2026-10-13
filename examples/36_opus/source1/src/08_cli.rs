//! CLI: Argumente, JSON-Ausgabe, `run()`-Verdrahtung.

use std::ffi::OsString;
use std::io::{Read, Write};
use std::path::PathBuf;

use crate::bitstream::Source;
use crate::types::{Analysis, CliConfig, Error, Input, Output};

/// Hilfe-Text.
pub const HELP: &str = "\
opus-vis: Opus-Datei visualisieren

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
  ESC            ende                   hover       details + celt-zeile
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
    let bs = crate::bitstream::analyze_packets(&ps)?;
    // Fakten einmal berechnen (Report + JSON teilen sie sich).
    let head = crate::fakten::head_fakten(&ps.head);
    let tags = crate::fakten::tag_fakten(&ps.tags);
    let enc = crate::fakten::encoder_fakten(&analysis, &bs.transient);
    let pcm_f = crate::fakten::pcm_fakten(&pcm.samples, &cols, &pcm.stats);
    let silk_f = crate::fakten::silk_fakten(&bs);
    let src = crate::bitstream::Sources::build(cols, bs);
    let width = cfg
        .width
        .unwrap_or_else(|| (src.spectrum.len() as u32).clamp(320, 4096));
    let img = crate::render::render(&analysis, &src, cfg.view, width.clamp(64, 4096));
    let png = crate::png::encode_rgb(img.width, img.height, &img.rgb);
    write_output(&cfg.png_out, &png)?;
    if let Some(json_out) = &cfg.json_out {
        let text = crate::json::json(&analysis, &head, &tags, &enc, &pcm_f, &src.bitstream);
        write_output(json_out, text.as_bytes())?;
    }
    if !cfg.quiet {
        let name = match &cfg.input {
            Input::Stdin => "(stdin)".to_string(),
            Input::Path(p) => p.display().to_string(),
        };
        let text = crate::report::report(&name, &analysis, &head, &tags, &enc, &pcm_f, &silk_f);
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
    src: &crate::bitstream::Sources,
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
    _src: &crate::bitstream::Sources,
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
    fn json_escapes_tag_strings() {
        use crate::bitstream::Bitstream;
        use crate::fakten::{EncoderFakten, HeadFakten, PcmFakten, TagFakten};
        use crate::types::{Analysis, Summary};
        let a = Analysis {
            frames: vec![],
            summary: Summary {
                frames: 0,
                duration_ms: 0.0,
                audio_bytes: 0,
                total_bytes: 0,
                avg_kbps: 0.0,
                raw_kbps: 768.0,
                ratio: 0.0,
                overhead_pct: 0.0,
                frac_silk: 0.0,
                frac_hybrid: 0.0,
                frac_celt: 0.0,
                channels: 1,
            },
            split_fallbacks: 0,
        };
        let head = HeadFakten {
            channels: 1,
            pre_skip_ms: 6.5,
            input_rate_hz: 48_000,
            gain_db: 0.0,
            mapping: 0,
            streams: 1,
            coupled: 0,
        };
        let tags = TagFakten {
            vendor: "enc \"x\" \\ y\nä".to_string(),
            comments: vec!["A=B\x01C".to_string()],
        };
        let enc = EncoderFakten {
            runs: vec![],
            bw_frac: [0.0; 5],
            kbps_min: 0.0,
            kbps_max: 0.0,
            kbps_p50: 0.0,
            kbps_p95: 0.0,
            vbr_index: 0.0,
            dtx_packets: 0,
            dtx_ms: 0.0,
            transients_ms: vec![],
        };
        let pcm = PcmFakten {
            peak: [0.0; 2],
            rms: [0.0; 2],
            rms_db: [-120.0, -120.0],
            clipped: 0,
            stereo_corr: None,
            silence: vec![],
            centroid_hz: 0.0,
        };
        let bs = Bitstream {
            spectrum: vec![],
            density: vec![],
            available: vec![],
            transient: vec![],
            frame_bits: vec![],
            packets: vec![],
            silk_packets: vec![],
            silk_available: vec![],
            silk_signal: vec![],
            silk_pitch: vec![],
            silk_gain: vec![],
            hybrid_silk: Vec::new(),
            hybrid: Vec::new(),
        };
        let text = crate::json::json(&a, &head, &tags, &enc, &pcm, &bs);
        assert!(text.contains("\"schema\":2"), "schema fehlt");
        assert!(
            text.contains("\"vendor\":\"enc \\\"x\\\" \\\\ y\\nä\""),
            "vendor-escaping: {text}"
        );
        assert!(text.contains("A=B\\u0001C"), "control-escaping: {text}");
        assert!(text.contains("\"stereo_corr\":null"), "null: {text}");
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
