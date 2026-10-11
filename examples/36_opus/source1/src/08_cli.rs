//! CLI: Argumente + `run()`-Verdrahtung (T1: Summary-Druck, T6: voll).

use std::ffi::OsString;
use std::path::PathBuf;

use crate::analyze;
use crate::types::Error;

/// Minimaler T1-Lauf: `opus-vis <datei>` druckt die Summary.
pub fn run(args: impl Iterator<Item = OsString>) -> Result<(), Error> {
    let input = parse_input(args)?;
    let analysis = analyze::analyze_path(&input)?;
    let s = &analysis.summary;
    println!("datei: {}", input.display());
    println!("frames: {}", s.frames);
    println!("dauer_ms: {:.0}", s.duration_ms);
    println!("avg_kbps: {:.1}", s.avg_kbps);
    println!("faktor_roh: {:.1}x", s.ratio);
    Ok(())
}

/// Erstes Argument = Eingabedatei (T6: volle Flaggen).
fn parse_input(
    mut args: impl Iterator<Item = OsString>,
) -> Result<PathBuf, Error> {
    let _argv0 = args.next();
    match args.next() {
        Some(arg) => Ok(PathBuf::from(arg)),
        None => Err(Error::Usage("aufruf: opus-vis <datei.opus>".into())),
    }
}
