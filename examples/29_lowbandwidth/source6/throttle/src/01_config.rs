//! `01_config` — Kommandozeile von `lbw-throttle`.

use std::time::Duration;

use crate::pipe::Cfg;

pub const USAGE: &str = "\
lbw-throttle — TCP-Proxy, der eine schmale, wackelige Leitung simuliert

  --listen ADDR       lokale Adresse (Default 127.0.0.1:7879)
  --to ADDR           Ziel (Default 127.0.0.1:7878)
  --rate B            Byte/s in beide Richtungen (Default 6000, 0 = frei)
  --up-rate B         Byte/s Client → Server (Default = --rate)
  --delay MS          Einweg-Latenz (Default 0)
  --blackout S:D      nach S s für D s nichts weiterleiten (mehrfach möglich)
  --cut-at S          nach S s alle Verbindungen abreißen (mehrfach möglich)
  --log               jede Sekunde Durchsatz auf stderr
";

/// Proxy-Konfiguration plus Zeitplan.
#[derive(Clone, Debug)]
pub struct Config {
    pub pipe: Cfg,
    pub blackouts: Vec<(Duration, Duration)>,
    pub cuts: Vec<Duration>,
    pub log: bool,
}

fn secs(s: &str) -> Result<Duration, String> {
    s.parse::<f64>()
        .map(Duration::from_secs_f64)
        .map_err(|_| format!("Sekunden erwartet: {s}"))
}

impl Config {
    pub fn parse(args: impl IntoIterator<Item = String>) -> Result<Self, String> {
        let mut c = Config {
            pipe: Cfg {
                listen: "127.0.0.1:7879".into(),
                to: "127.0.0.1:7878".into(),
                down_rate: 6000,
                up_rate: 0,
                delay: Duration::ZERO,
            },
            blackouts: Vec::new(),
            cuts: Vec::new(),
            log: false,
        };
        let mut up = None;
        let mut it = args.into_iter();
        while let Some(a) = it.next() {
            let mut v = || it.next().ok_or(format!("{a}: Wert fehlt"));
            match a.as_str() {
                "--listen" => c.pipe.listen = v()?,
                "--to" => c.pipe.to = v()?,
                "--rate" => c.pipe.down_rate = v()?.parse().map_err(|_| "--rate: Zahl")?,
                "--up-rate" => up = Some(v()?.parse().map_err(|_| "--up-rate: Zahl")?),
                "--delay" => {
                    c.pipe.delay = Duration::from_millis(v()?.parse().map_err(|_| "--delay: Zahl")?)
                }
                "--blackout" => {
                    let s = v()?;
                    let (a, b) = s.split_once(':').ok_or("--blackout S:D")?;
                    c.blackouts.push((secs(a)?, secs(b)?));
                }
                "--cut-at" => c.cuts.push(secs(&v()?)?),
                "--log" => c.log = true,
                "-h" | "--help" => return Err(USAGE.into()),
                _ => return Err(format!("unbekannte Option {a}\n\n{USAGE}")),
            }
        }
        c.pipe.up_rate = up.unwrap_or(c.pipe.down_rate);
        Ok(c)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_schedule() {
        let a = "--rate 3000 --blackout 5:60 --blackout 100:1.5 --cut-at 70 --delay 40";
        let c = Config::parse(a.split(' ').map(str::to_owned)).unwrap();
        assert_eq!((c.pipe.down_rate, c.pipe.up_rate), (3000, 3000));
        assert_eq!(
            c.blackouts[1],
            (Duration::from_secs(100), Duration::from_millis(1500))
        );
        assert_eq!(c.cuts, vec![Duration::from_secs(70)]);
        assert_eq!(c.pipe.delay, Duration::from_millis(40));
        assert!(Config::parse(["--blackout".into(), "5".into()]).is_err());
    }
}
