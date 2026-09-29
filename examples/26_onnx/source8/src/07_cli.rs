//! `07_cli` — Argument-Parsing (std::env) und Ausführung der Kommandos.

use crate::capture::Screen;
use std::fs::File;
use std::io::BufWriter;

pub const USAGE: &str = "\
gui_detect grab <out.ppm>          X11-Root-Window als PPM speichern";

/// Geparstes Kommando.
#[derive(Debug, PartialEq)]
pub enum Command {
    Grab { out: String },
}

/// Parst `args` ohne Programmnamen.
pub fn parse(args: &[String]) -> Result<Command, String> {
    match args.first().map(String::as_str) {
        Some("grab") => {
            let out = args.get(1).ok_or("grab: Ausgabedatei fehlt")?.clone();
            Ok(Command::Grab { out })
        }
        _ => Err(USAGE.into()),
    }
}

/// Führt ein Kommando aus.
pub fn run(cmd: Command) -> Result<(), String> {
    match cmd {
        Command::Grab { out } => {
            let img = Screen::open()?.grab()?;
            let f = File::create(&out).map_err(|e| format!("{out}: {e}"))?;
            img.write_ppm(BufWriter::new(f))
                .map_err(|e| format!("{out}: {e}"))?;
            println!("grab {}x{} -> {out}", img.w, img.h);
            Ok(())
        }
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
}
