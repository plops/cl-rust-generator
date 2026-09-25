//! `00_args` — CLI-Argumente (S6).
//!
//! `parse_args` liest das echte `std::env::args` (Datei-Einlesen und
//! `--help`-Exit wie bisher); `parse_args_from` nimmt einen beliebigen
//! Iterator und ist ohne Prozess/Terminal testbar. Inhaltlich: exakt die
//! bisherigen Flags plus `-a`/`--auto` (Automation startet scharf, ohne
//! dass erst Taste `a` gedrückt werden muss — für Skript-Läufe, die pro
//! TOML-Regel einen frischen Prozess starten).

/// Geparste Kommandozeilen-Argumente.
pub struct Args {
    /// Keine echten Klicks/Tasten (nur Log).
    pub dry_run: bool,
    /// Pfad zur TOML-Regeldatei.
    pub rules_path: String,
    /// Statt TUI: nach N geänderten Frames beenden (pty-frei, CI/Smoke).
    pub headless_frames: Option<u64>,
    /// Automation startet scharf (wie Taste `a` beim Start gedrückt).
    pub auto_start: bool,
}

/// Liest die echten Prozess-Argumente.
pub fn parse_args() -> Result<Args, String> {
    parse_args_from(std::env::args().skip(1))
}

/// Parst gegebene Argumente (ohne Programmnamen); `--help` druckt und
/// beendet den Prozess wie bisher (darum in Tests nicht aufrufen).
pub fn parse_args_from<I>(args: I) -> Result<Args, String>
where
    I: IntoIterator<Item = String>,
{
    let mut dry_run = false;
    let mut rules_path = "rules.toml".to_string();
    let mut headless_frames = None;
    let mut auto_start = false;
    let mut it = args.into_iter();
    while let Some(a) = it.next() {
        match a.as_str() {
            "--dry-run" => dry_run = true,
            "-a" | "--auto" => auto_start = true,
            "--headless-frames" => {
                let n: u64 = it
                    .next()
                    .ok_or("--headless-frames braucht eine Zahl")?
                    .parse()
                    .map_err(|_| "--headless-frames braucht eine Zahl")?;
                headless_frames = Some(n);
            }
            "--rules" => {
                rules_path = it.next().ok_or("--rules braucht einen Pfad")?;
            }
            "--help" | "-h" => {
                println!(
                    "Aufruf: x11_ocr_automation [-a] [--dry-run] [--rules D] [--headless-frames N], Tasten: Pfeile/1/2/a/q"
                );
                std::process::exit(0);
            }
            other => return Err(format!("unbekanntes Argument: {other}")),
        }
    }
    Ok(Args {
        dry_run,
        rules_path,
        headless_frames,
        auto_start,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn argv(words: &[&str]) -> Vec<String> {
        words.iter().map(|s| (*s).to_string()).collect()
    }

    #[test]
    fn defaults_start_disarmed() {
        let a = parse_args_from(argv(&[])).unwrap();
        assert!(!a.auto_start);
        assert!(!a.dry_run);
        assert_eq!(a.rules_path, "rules.toml");
        assert_eq!(a.headless_frames, None);
    }

    #[test]
    fn short_flag_arms_automation() {
        let a = parse_args_from(argv(&["-a"])).unwrap();
        assert!(a.auto_start);
    }

    #[test]
    fn long_flag_arms_automation() {
        let a = parse_args_from(argv(&["--auto"])).unwrap();
        assert!(a.auto_start);
    }

    #[test]
    fn combines_with_existing_flags() {
        let a = parse_args_from(argv(&[
            "-a",
            "--dry-run",
            "--rules",
            "meta_sticker_request.toml",
            "--headless-frames",
            "30",
        ]))
        .unwrap();
        assert!(a.auto_start);
        assert!(a.dry_run);
        assert_eq!(a.rules_path, "meta_sticker_request.toml");
        assert_eq!(a.headless_frames, Some(30));
    }

    #[test]
    fn unknown_and_missing_values_fail() {
        assert!(parse_args_from(argv(&["--zauber"])).is_err());
        assert!(parse_args_from(argv(&["--rules"])).is_err());
        assert!(parse_args_from(argv(&["--headless-frames"])).is_err());
        assert!(parse_args_from(argv(&["--headless-frames", "viel"])).is_err());
    }
}
