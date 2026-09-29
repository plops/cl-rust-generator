//! `01_config` — Kommandozeile des Clients (handgeparst).

use std::time::Duration;

use lbw_common::DEFAULT_PORT;

pub const USAGE: &str = "\
lbw-client — Low-Bandwidth-Remote-Desktop-Client

  --connect ADDR    Server (Default 127.0.0.1:7878, typ. Ende von ssh -L)
  --size N          Fenstergröße = Capture-Größe (Default 640)
  --font PATH       Schrift (Default GNU Unifont aus fonts-unifont)
  --dead-after S    Verbindung neu aufbauen nach S s ohne Daten (Default 90)
  --dump-text       Text-Deltas auf stdout protokollieren
  -v, --verbose     Netz-Log

Tasten: F1 HUD, F2 Text auswählen → Zwischenablage, F3 Zwischenablage tippen.
";

#[derive(Clone, Debug)]
pub struct Config {
    pub connect: String,
    pub size: usize,
    pub font: Option<String>,
    pub dead_after: Duration,
    pub dump_text: bool,
    pub verbose: bool,
}

impl Config {
    pub fn parse(args: impl IntoIterator<Item = String>) -> Result<Self, String> {
        let mut c = Config {
            connect: format!("127.0.0.1:{DEFAULT_PORT}"),
            size: 640,
            font: None,
            dead_after: Duration::from_secs(90),
            dump_text: false,
            verbose: false,
        };
        let mut it = args.into_iter();
        let num = |v: Option<String>, f: &str| -> Result<u64, String> {
            v.and_then(|v| v.parse().ok())
                .ok_or_else(|| format!("{f}: Zahl erwartet"))
        };
        while let Some(a) = it.next() {
            match a.as_str() {
                "--connect" => c.connect = it.next().ok_or("--connect: Adresse fehlt")?,
                "--size" => c.size = num(it.next(), "--size")? as usize,
                "--font" => c.font = it.next(),
                "--dead-after" => {
                    c.dead_after = Duration::from_secs(num(it.next(), "--dead-after")?)
                }
                "--dump-text" => c.dump_text = true,
                "-v" | "--verbose" => c.verbose = true,
                "-h" | "--help" => return Err(USAGE.into()),
                _ => return Err(format!("unbekannte Option {a}\n\n{USAGE}")),
            }
        }
        if !(64..=4096).contains(&c.size) {
            return Err("--size außerhalb 64..4096".into());
        }
        Ok(c)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_defaults_and_flags() {
        let c = Config::parse(Vec::new()).unwrap();
        assert_eq!((c.connect.as_str(), c.size), ("127.0.0.1:7878", 640));
        let a = "--connect h:1 --size 320 --dump-text -v --dead-after 5";
        let c = Config::parse(a.split(' ').map(str::to_owned)).unwrap();
        assert_eq!(
            (c.connect.as_str(), c.size, c.dead_after.as_secs()),
            ("h:1", 320, 5)
        );
        assert!(c.dump_text && c.verbose);
        assert!(Config::parse(["--size".into(), "1".into()]).is_err());
    }
}
