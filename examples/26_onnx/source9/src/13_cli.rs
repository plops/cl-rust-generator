//! `13_cli` — handgeschriebene Argumente (ohne Dep).
//!
//! ```text
//! unicode_ocr bench [--lang de,fr|all] [--gen pangram|...|all] [--size 32]
//!   [--lines 8] [--samples 20] [--seed 1] [--model auto|universal]
//!   [--models DIR] [--corpus DIR] [--font PATH] [--tsv FILE]
//! unicode_ocr render-test <lang> <out.ppm> [px]
//! ```
//! Das Fenster (`unicode_ocr` ohne Befehl) folgt in T11.

use std::path::PathBuf;

use crate::engine::Settings;
use crate::generate::GenMode;
use crate::lang::{LANGS, by_code};
use crate::models::ModelChoice;

/// Befehl nach dem Parsen.
#[derive(Debug)]
pub enum Cmd {
    /// Headless-Benchmark.
    Bench(BenchArgs),
    /// Testbild rendern (Debug).
    RenderTest {
        /// Index in `LANGS`.
        lang: usize,
        /// Ziel (`*.ppm`).
        out: PathBuf,
        /// Schriftgröße.
        px: u32,
    },
    /// Hilfe anzeigen.
    Help,
}

/// `bench`-Argumente (mit Defaults).
#[derive(Debug)]
pub struct BenchArgs {
    /// Sprachen (Indizes in `LANGS`).
    pub langs: Vec<usize>,
    /// Generatoren.
    pub gens: Vec<GenMode>,
    /// Schriftgröße.
    pub px: u32,
    /// Zeilen pro Sample.
    pub lines: usize,
    /// Samples je (Sprache, Generator).
    pub samples: usize,
    /// Basis-Seed (plus Sample-Index).
    pub seed: u64,
    /// Modellwahl.
    pub model: ModelChoice,
    /// Modellverzeichnis.
    pub models_dir: PathBuf,
    /// Korpusverzeichnis.
    pub corpus_dir: PathBuf,
    /// Schrift (`None` = Suchliste).
    pub font: Option<PathBuf>,
    /// TSV-Log (`None` = keins).
    pub tsv: Option<PathBuf>,
}

impl BenchArgs {
    /// `Engine`-Einstellung für (Sprache, Generator).
    #[must_use]
    pub fn settings(&self, lang: usize, mode: GenMode) -> Settings {
        Settings {
            lang,
            mode,
            px: self.px,
            lines: self.lines,
            model: self.model,
        }
    }
}

impl Default for BenchArgs {
    fn default() -> Self {
        Self {
            langs: (0..LANGS.len()).collect(),
            gens: vec![GenMode::Pangram],
            px: 32,
            lines: 8,
            samples: 20,
            seed: 1,
            model: ModelChoice::Auto,
            models_dir: PathBuf::from("models"),
            corpus_dir: PathBuf::from("corpus"),
            font: None,
            tsv: None,
        }
    }
}

/// Kurzhilfe.
#[must_use]
pub fn usage() -> &'static str {
    "usage: unicode_ocr bench [OPTIONEN]\n\
     \n\
     \t--lang de,fr|all   --gen pangram|words|markov|chars|all   --size 32\n\
     \t--lines 8  --samples 20  --seed 1  --model auto|universal\n\
     \t--models DIR  --corpus DIR  --font PATH  --tsv FILE\n\
     \n\
     unicode_ocr render-test <lang> <out.ppm> [px]"
}

/// Parst `argv` (ohne Programmname).
pub fn parse(argv: &[String]) -> Result<Cmd, String> {
    match argv.first().map(String::as_str) {
        None | Some("-h") | Some("--help") | Some("help") => Ok(Cmd::Help),
        Some("render-test") => parse_render(&argv[1..]),
        Some("bench") => parse_bench(&argv[1..]),
        Some(other) => Err(format!("unknown command: {other}\n{}", usage())),
    }
}

fn parse_render(args: &[String]) -> Result<Cmd, String> {
    if args.len() < 2 {
        return Err("usage: unicode_ocr render-test <lang> <out.ppm> [px]".to_string());
    }
    let lang = by_code(&args[0]).ok_or_else(|| format!("unknown language: {}", args[0]))?;
    let px = args.get(2).map_or(Ok(32), |s| parse_px(s))?;
    Ok(Cmd::RenderTest {
        lang,
        out: PathBuf::from(&args[1]),
        px,
    })
}

fn parse_bench(args: &[String]) -> Result<Cmd, String> {
    let mut b = BenchArgs::default();
    let mut i = 0;
    while i < args.len() {
        let (flag, val) = (&args[i], args.get(i + 1));
        let need = |flag: &str| val.cloned().ok_or_else(|| format!("{flag} needs a value"));
        match flag.as_str() {
            "--lang" => {
                b.langs = parse_langs(&need(flag)?)?;
                i += 2;
            }
            "--gen" => {
                b.gens = parse_gens(&need(flag)?)?;
                i += 2;
            }
            "--size" => {
                b.px = parse_px(&need(flag)?)?;
                i += 2;
            }
            "--lines" => {
                b.lines = need(flag)?.parse().map_err(|_| "--lines needs a number")?;
                i += 2;
            }
            "--samples" => {
                b.samples = need(flag)?
                    .parse()
                    .map_err(|_| "--samples needs a number")?;
                i += 2;
            }
            "--seed" => {
                b.seed = need(flag)?.parse().map_err(|_| "--seed needs a number")?;
                i += 2;
            }
            "--model" => {
                b.model = match need(flag)?.as_str() {
                    "auto" => ModelChoice::Auto,
                    "universal" => ModelChoice::Universal,
                    v => return Err(format!("--model: expected auto|universal, got {v}")),
                };
                i += 2;
            }
            "--models" => {
                b.models_dir = PathBuf::from(need(flag)?);
                i += 2;
            }
            "--corpus" => {
                b.corpus_dir = PathBuf::from(need(flag)?);
                i += 2;
            }
            "--font" => {
                b.font = Some(PathBuf::from(need(flag)?));
                i += 2;
            }
            "--tsv" => {
                b.tsv = Some(PathBuf::from(need(flag)?));
                i += 2;
            }
            other => return Err(format!("unknown flag: {other}\n{}", usage())),
        }
    }
    if b.lines == 0 {
        return Err("--lines must be >= 1".to_string());
    }
    if b.samples == 0 {
        return Err("--samples must be >= 1".to_string());
    }
    Ok(Cmd::Bench(b))
}

fn parse_px(s: &str) -> Result<u32, String> {
    match s.parse::<u32>() {
        Ok(px @ (16 | 24 | 32 | 48)) => Ok(px),
        _ => Err(format!("--size: expected 16|24|32|48, got {s}")),
    }
}

fn parse_langs(s: &str) -> Result<Vec<usize>, String> {
    if s.split(',').any(|c| c == "all") {
        return Ok((0..LANGS.len()).collect());
    }
    s.split(',')
        .map(|c| by_code(c.trim()).ok_or_else(|| format!("unknown language: {c}")))
        .collect()
}

fn parse_gens(s: &str) -> Result<Vec<GenMode>, String> {
    if s.split(',').any(|g| g == "all") {
        return Ok(GenMode::ALL.to_vec());
    }
    s.split(',')
        .map(|g| match g.trim() {
            "pangram" => Ok(GenMode::Pangram),
            "words" => Ok(GenMode::Words),
            "markov" => Ok(GenMode::Markov),
            "chars" => Ok(GenMode::Chars),
            _ => Err(format!("unknown generator: {g}")),
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn argv(words: &[&str]) -> Vec<String> {
        words.iter().map(|s| s.to_string()).collect()
    }

    fn bench(words: &[&str]) -> BenchArgs {
        match parse(&argv(words)).expect("parse") {
            Cmd::Bench(b) => b,
            other => panic!("not bench: {other:?}"),
        }
    }

    #[test]
    fn defaults_cover_all_languages() {
        let b = bench(&["bench"]);
        assert_eq!(b.langs.len(), LANGS.len());
        assert_eq!(b.gens, vec![GenMode::Pangram]);
        assert_eq!((b.px, b.lines, b.samples, b.seed), (32, 8, 20, 1));
        assert_eq!(b.model, ModelChoice::Auto);
        assert!(b.tsv.is_none() && b.font.is_none());
    }

    #[test]
    fn lists_and_scalars_parse() {
        let b = bench(&[
            "bench",
            "--lang",
            "de,fr",
            "--gen",
            "words,markov",
            "--size",
            "48",
            "--lines",
            "4",
            "--samples",
            "5",
            "--seed",
            "7",
            "--model",
            "universal",
            "--tsv",
            "o.tsv",
        ]);
        assert_eq!(
            b.langs,
            vec![by_code("de").unwrap(), by_code("fr").unwrap()]
        );
        assert_eq!(b.gens, vec![GenMode::Words, GenMode::Markov]);
        assert_eq!((b.px, b.lines, b.samples, b.seed), (48, 4, 5, 7));
        assert_eq!(b.model, ModelChoice::Universal);
        assert_eq!(b.tsv, Some(PathBuf::from("o.tsv")));
        assert_eq!(bench(&["bench", "--gen", "all"]).gens.len(), 4);
    }

    #[test]
    fn errors_are_clear() {
        for args in [
            vec!["bench", "--lang", "xx"],
            vec!["bench", "--gen", "foo"],
            vec!["bench", "--size", "20"],
            vec!["bench", "--samples", "0"],
            vec!["bench", "--lines"],
            vec!["bench", "--nope", "1"],
            vec!["frobnicate"],
            vec!["render-test", "de"],
            vec!["render-test", "xx", "o.ppm"],
        ] {
            assert!(parse(&argv(&args)).is_err(), "{args:?}");
        }
    }

    #[test]
    fn render_test_and_help_parse() {
        match parse(&argv(&["render-test", "de", "o.ppm", "16"])).unwrap() {
            Cmd::RenderTest { lang, px, .. } => {
                assert_eq!((lang, px), (by_code("de").unwrap(), 16));
            }
            other => panic!("{other:?}"),
        }
        assert!(matches!(parse(&argv(&[])).unwrap(), Cmd::Help));
    }
}
