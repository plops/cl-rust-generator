//! `10_echo` — Erst-Echo-Latenz: Eingabe → nächste sichtbare Änderung.
//!
//! Ausgelagert aus `03_stats` (600-Zeilen-Regel); dort wird es je
//! Echo-Art (sichtbar/Text/Kachel/Frame) einmal instanziiert.

use crate::record::{Dir, LogRecord, MsgKind};
use crate::stats::{Lat, sample_ms};

/// Diskrete Eingabe (Taste/Klick *gedrückt*, Text) — `MouseMove` ist
/// ausgenommen (Maus-Spam-Falle), ebenso das Loslassen (`down: false`):
/// Ein Tastendruck ist *eine* Absicht, nicht zwei. Undekodierbare Bodies
/// (synthetische Tests, fremde Versionen) zählen konservativ als Eingabe.
pub(crate) fn is_input(m: &LogRecord) -> bool {
    let LogRecord::Msg {
        dir: Dir::CliToSrv,
        kind: MsgKind::Key | MsgKind::Text | MsgKind::Button,
        body,
        ..
    } = m
    else {
        return false;
    };
    !matches!(
        lbw_common::framing::decode_msg::<lbw_common::ClientMsg>(body),
        Ok(lbw_common::ClientMsg::Key { down: false, .. }
            | lbw_common::ClientMsg::Button { down: false, .. })
    )
}

/// Max. Wartezeit auf ein Echo; ältere Eingaben gelten als „ohne sichtbare
/// Reaktion" (z. B. Shift allein) statt eine spätere Änderung zu kapern.
pub const ECHO_TIMEOUT_US: u64 = 10_000_000;

/// Erst-Echo-Zuordnung (viele-zu-eins): Das *nächste* Echo beantwortet
/// **alle** bis dahin wartenden Eingaben. Die frühere 1:1-FIFO-Paarung
/// (source10) ließ unbeantwortete Eingaben endlos auflaufen — fünf Tasten,
/// ein Echo → vier Altlasten, die sich mit Echos Minuten später paarten
/// (0839-Log: Input→Kachel p50 47 s, Review B4).
#[derive(Default)]
pub(crate) struct Echo {
    pending: Vec<u64>,
    samples: Vec<f64>,
    unanswered: usize,
}

impl Echo {
    pub(crate) fn input(&mut self, t: u64) {
        self.pending.push(t);
    }

    pub(crate) fn echo(&mut self, t: u64) {
        for t0 in self.pending.drain(..) {
            if t.saturating_sub(t0) <= ECHO_TIMEOUT_US {
                self.samples.push(sample_ms(t, t0));
            } else {
                self.unanswered += 1;
            }
        }
    }

    /// (Verteilung, unbeantwortet inkl. am Dateiende noch wartender).
    pub(crate) fn finish(self) -> (Lat, usize) {
        (Lat::of(self.samples), self.unanswered + self.pending.len())
    }
}

#[cfg(test)]
mod tests {
    use crate::record::{Dir, LogRecord, MsgKind, Stamp};
    use crate::stats::summarize_file;

    fn stamp(wall_us: u64, mono_us: u64) -> Stamp {
        Stamp { wall_us, mono_us }
    }

    fn msg(w: u64, m: u64, dir: Dir, kind: MsgKind, wire: usize, hash: u64) -> LogRecord {
        LogRecord::Msg {
            stamp: stamp(w, m),
            dir,
            kind,
            wire_bytes: wire,
            hash,
            body: vec![],
        }
    }

    fn session(app: &str) -> LogRecord {
        LogRecord::Session {
            app: app.into(),
            version: 3,
            args: vec![],
        }
    }

    #[test]
    fn one_echo_answers_a_burst_and_old_inputs_time_out() {
        use lbw_common::ClientMsg;
        use lbw_common::framing::encode_msg;
        let key = |m: u64, down: bool| LogRecord::Msg {
            stamp: stamp(1_000_000 + m, m),
            dir: Dir::CliToSrv,
            kind: MsgKind::Key,
            wire_bytes: 12,
            hash: 0,
            body: encode_msg(&ClientMsg::Key {
                key: "a".into(),
                down,
            })
            .unwrap(),
        };
        let mut recs = vec![session("lbw-client")];
        // Fünf Tasten (je down+up) in 50-ms-Abständen, ein Echo bei 500 ms.
        for i in 0..5 {
            recs.push(key(i * 50_000, true));
            recs.push(key(i * 50_000 + 10_000, false));
        }
        recs.push(msg(
            1_500_000,
            500_000,
            Dir::SrvToCli,
            MsgKind::AddText,
            60,
            9,
        ));
        // Eine Taste ohne Reaktion, Echo erst 11 s später → Timeout.
        recs.push(key(1_000_000, true));
        recs.push(msg(
            13_000_000,
            12_000_000,
            Dir::SrvToCli,
            MsgKind::Tile,
            900,
            8,
        ));
        let s = summarize_file("c", &recs);
        // Nur die 5 `down` zählen (500, 450, …, 300 ms), Loslassen nicht.
        assert_eq!(s.input_visible_ms.n, 5);
        assert_eq!(
            (s.input_visible_ms.max, s.input_visible_ms.p50),
            (500.0, 400.0)
        );
        assert_eq!(s.input_unanswered, 1);
    }
}
