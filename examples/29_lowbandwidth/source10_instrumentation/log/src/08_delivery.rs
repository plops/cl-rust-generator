//! `08_delivery` — Zustell-Verzögerung Server→Client (Kreuz-Analyse).
//!
//! Reine Funktionen: Server/Client-Datei wählen, Sende-/Empfangs-Walls je
//! Verbindung segmentieren, k-te empfangene = k-te gesendete matchen
//! (TCP-Reihenfolge) und je Probe mit dem Offset-Fenster ihrer Empfangszeit
//! korrigieren (s. [`ClockWin`](crate::summary::ClockWin)).

use crate::record::{Dir, GapEvent, LogRecord};
use crate::stats::{FileLog, app_of};
use crate::summary::ClockWin;

/// Wählt Server- und Client-Datei (per App-Namen) für Kreuz-Analysen.
pub fn pick_srv_cli(files: &[FileLog]) -> (Option<&FileLog>, Option<&FileLog>) {
    let srv = files.iter().find(|f| app_of(&f.records).contains("server"));
    let cli = files.iter().find(|f| app_of(&f.records).contains("client"));
    (srv, cli)
}

/// Server→Client-Walls, aufgeteilt an Downs (ein Segment je Verbindung).
/// Leere Segmente (Refused-Retries ohne Nachrichten) fallen weg.
pub fn srv_segments(records: &[LogRecord]) -> Vec<Vec<u64>> {
    let mut segs = vec![Vec::new()];
    for r in records {
        match r {
            LogRecord::Msg {
                stamp,
                dir: Dir::SrvToCli,
                ..
            } => segs.last_mut().unwrap().push(stamp.wall_us),
            LogRecord::Gap {
                event: GapEvent::Down { .. },
                ..
            } => segs.push(Vec::new()),
            _ => {}
        }
    }
    segs.retain(|s| !s.is_empty());
    segs
}

/// Zustell-Verzögerung in Sekunden: k-te empfangene = k-te gesendete
/// (TCP-Reihenfolge, Verluste nur am Verbindungsende). Jede Probe wird mit
/// dem Offset-Fenster ihrer Empfangszeit korrigiert (s. `ClockWin`); ohne
/// Fenster ist keine Korrektur möglich (leeres Ergebnis).
pub fn delivery_delays(
    srv_segs: &[Vec<u64>],
    cli_segs: &[Vec<u64>],
    offsets: &[ClockWin],
) -> Vec<f64> {
    srv_segs
        .iter()
        .zip(cli_segs.iter())
        .flat_map(|(s, c)| {
            s.iter().zip(c.iter()).filter_map(|(sw, cw)| {
                let w = offsets
                    .iter()
                    .rev()
                    .find(|w| w.start_wall_us <= *cw)
                    .or(offsets.first())?;
                Some((*cw as f64 - *sw as f64) / 1e6 + w.ms / 1000.0)
            })
        })
        .collect()
}

/// Median je Zehntel (Verlauf der Verzögerung über die Session).
/// Bei < 10 Samples wiederholen sich Ränge (nie leer, nie Panik).
pub fn tenths(v: &[f64]) -> Vec<f64> {
    if v.is_empty() {
        return Vec::new();
    }
    let mut out = Vec::with_capacity(10);
    for q in 0..10 {
        let a = q * v.len() / 10;
        let b = ((q + 1) * v.len() / 10).max(a + 1).min(v.len());
        let mut s = v[a..b].to_vec();
        s.sort_by(|a, b| a.total_cmp(b));
        out.push(s[s.len() / 2]);
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn delivery_pairs_by_position_per_segment() {
        // Server 10 s vor (Offset), eine Nachricht verloren am Schluss.
        let srv = vec![vec![100_000_000, 101_000_000, 102_000_000]];
        let cli = vec![vec![90_100_000, 91_100_000]];
        let off = vec![ClockWin {
            start_wall_us: 0,
            ms: 10_000.0,
            samples: 2,
        }];
        let d = delivery_delays(&srv, &cli, &off);
        assert_eq!(d.len(), 2);
        assert!((d[0] - 0.1).abs() < 1e-9);
        // Zweites Fenster: spätere Probe nutzt den neuen Offset.
        let off = vec![
            ClockWin {
                start_wall_us: 0,
                ms: 10_000.0,
                samples: 1,
            },
            ClockWin {
                start_wall_us: 91_000_000,
                ms: 11_000.0,
                samples: 1,
            },
        ];
        let d = delivery_delays(&srv, &cli, &off);
        assert!((d[0] - 0.1).abs() < 1e-9);
        assert!((d[1] - 1.1).abs() < 1e-9);
        assert!(delivery_delays(&srv, &cli, &[]).is_empty());
        assert_eq!(
            tenths(&[1.0, 2.0]),
            vec![1.0, 1.0, 1.0, 1.0, 1.0, 2.0, 2.0, 2.0, 2.0, 2.0]
        );
        assert_eq!(tenths(&[]), Vec::<f64>::new());
    }

    #[test]
    fn pick_finds_server_and_client() {
        let sess = |app: &str| LogRecord::Session {
            app: app.into(),
            version: 2,
            args: vec![],
        };
        let files = vec![
            FileLog {
                name: "c".into(),
                records: vec![sess("lbw-client")],
                version: 2,
            },
            FileLog {
                name: "s".into(),
                records: vec![sess("lbw-server")],
                version: 1,
            },
        ];
        let (srv, cli) = pick_srv_cli(&files);
        let srv = srv.unwrap();
        assert_eq!((srv.name.as_str(), srv.version), ("s", 1));
        assert_eq!(cli.unwrap().name, "c");
        let (no_srv, no_cli) = pick_srv_cli(&[]);
        assert!(no_srv.is_none() && no_cli.is_none());
    }
}
