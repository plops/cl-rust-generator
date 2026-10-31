//! `09_channel` — Kanal-Simulation über die Server-Sendestempel.
//!
//! Frage: „Wie lange hätte jede Nachricht auf einem Kanal mit genau
//! `bps` Byte/s gebraucht, bis sie *vollständig* beim Client ist?" Modell:
//! eine FIFO-Leitung (TCP + SSH-Tunnel). Jede Nachricht startet, sobald sie
//! gesendet wurde **und** die Leitung frei ist; sie belegt die Leitung
//! `wire_bytes / bps` Sekunden. Zustellverzögerung = Ende − Sendezeit.
//!
//! Damit wird *Head-of-Line-Blocking* sichtbar: Ein 95-B-Text hinter einer
//! 42-kB-Kachel wartet 7 s, obwohl er selbst nur 16 ms Leitung braucht
//! (Review B1). Braucht nur das Server-Log — keine Uhrensynchronisation.

use crate::record::{Dir, GapEvent, LogRecord, MsgKind};
use crate::stats::Lat;

/// Ergebnis der Simulation.
#[derive(Clone, Debug, Default)]
pub struct ChannelSim {
    /// Zustellverzögerung der Text-Nachrichten (Clear/Add/Remove), ms.
    pub text_ms: Lat,
    /// Zustellverzögerung der Kacheln, ms.
    pub tile_ms: Lat,
    /// Größte einzelne Server-Nachricht (Leitungsbytes).
    pub max_msg_b: usize,
    /// Deren Leitungsbelegung in ms (`max_msg_b / bps`).
    pub max_msg_ms: f64,
}

/// Simuliert alle Server→Client-Nachrichten einer Datei (Sendestempel =
/// `mono_us`; bei Reconnect wird die Leitung geleert).
#[must_use]
pub fn simulate(records: &[LogRecord], bps: u64) -> ChannelSim {
    let bps = bps.max(1) as f64;
    let mut free_at = 0.0f64; // µs, wann die Leitung wieder frei ist
    let (mut text, mut tile) = (Vec::new(), Vec::new());
    let mut max_msg_b = 0;
    for r in records {
        match r {
            LogRecord::Gap {
                event: GapEvent::Up { .. },
                ..
            } => free_at = 0.0,
            LogRecord::Msg {
                stamp,
                dir: Dir::SrvToCli,
                kind,
                wire_bytes,
                ..
            } => {
                let sent = stamp.mono_us as f64;
                let start = free_at.max(sent);
                free_at = start + *wire_bytes as f64 / bps * 1e6;
                let delay_ms = (free_at - sent) / 1000.0;
                max_msg_b = max_msg_b.max(*wire_bytes);
                match kind {
                    MsgKind::ClearText | MsgKind::AddText | MsgKind::RemoveText => {
                        text.push(delay_ms);
                    }
                    MsgKind::Tile => tile.push(delay_ms),
                    _ => {}
                }
            }
            _ => {}
        }
    }
    ChannelSim {
        text_ms: Lat::of(text),
        tile_ms: Lat::of(tile),
        max_msg_b,
        max_msg_ms: max_msg_b as f64 / bps * 1000.0,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::record::Stamp;

    fn msg(mono_ms: u64, kind: MsgKind, wire: usize) -> LogRecord {
        LogRecord::Msg {
            stamp: Stamp {
                wall_us: 0,
                mono_us: mono_ms * 1000,
            },
            dir: Dir::SrvToCli,
            kind,
            wire_bytes: wire,
            hash: 0,
            body: vec![],
        }
    }

    #[test]
    fn small_texts_alone_arrive_at_line_speed() {
        let s = simulate(
            &[
                msg(0, MsgKind::AddText, 60),
                msg(1000, MsgKind::AddText, 60),
            ],
            6000,
        );
        assert_eq!(s.text_ms.n, 2);
        assert!((s.text_ms.max - 10.0).abs() < 1e-9, "{:?}", s.text_ms);
    }

    #[test]
    fn big_tile_blocks_the_text_behind_it() {
        // 12-kB-Kachel bei t=0, Text 100 ms später: wartet bis 2 s + 10 ms.
        let s = simulate(
            &[
                msg(0, MsgKind::Tile, 12_000),
                msg(100, MsgKind::AddText, 60),
            ],
            6000,
        );
        assert!((s.tile_ms.max - 2000.0).abs() < 1e-9);
        assert!((s.text_ms.max - 1910.0).abs() < 1e-9, "{:?}", s.text_ms);
        assert_eq!(s.max_msg_b, 12_000);
        assert!((s.max_msg_ms - 2000.0).abs() < 1e-9);
    }

    #[test]
    fn reconnect_clears_the_line() {
        let up = LogRecord::Gap {
            stamp: Stamp::default(),
            event: GapEvent::Up {
                peer: "p".into(),
                down_ms: None,
            },
        };
        let s = simulate(
            &[
                msg(0, MsgKind::Tile, 60_000),
                up,
                msg(1, MsgKind::AddText, 60),
            ],
            6000,
        );
        assert!((s.text_ms.max - 10.0).abs() < 1e-9);
    }
}
