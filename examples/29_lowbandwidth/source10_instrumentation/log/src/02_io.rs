//! `02_io` — `.lbwlog`-Dateien schreiben und lesen.
//!
//! Format: 8 Byte Magic ([`LOG_MAGIC`]), dann ein Strom von
//! `[u32-LE-Länge][bincode-Body]`-Records (selbes Prinzip wie das Protokoll-
//! Framing). Der `Writer` flusht nach jedem Record (Absturz kostet höchstens
//! den letzten); der `Reader` ist strikt: ein zerrissener Schluss (Absturz
//! mitten im Record) ist ein Fehler, kein stilles EOF.

use std::fs::File;
use std::io::{self, BufReader, BufWriter, ErrorKind, Read, Write};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use lbw_common::framing::{HEADER, encode_msg};
use lbw_common::{ClientMsg, ServerMsg};

use crate::record::{
    Dir, FrameMs, GapEvent, LOG_MAGIC, LOG_VERSION, LogRecord, MsgKind, Stamp, TileStat, fnv1a64,
};

/// Max. Record-Größe in Byte (Tiles ≤ 8 MiB + Hülle — schützt vor OOM).
pub const LOG_MAX: usize = 16 * 1024 * 1024;

/// Wall-Clock in µs seit der Unix-Epoche (0 bei Uhrfehler — läuft trotzdem).
#[must_use]
pub fn wall_us() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or(Duration::ZERO)
        .as_micros()
        .min(u128::from(u64::MAX)) as u64
}

/// Schreibt Records an eine neue Datei (Magic zuerst).
pub struct Writer {
    w: BufWriter<File>,
    t0: Instant,
}

impl Writer {
    /// Legt `path` neu an (überschreibt) und schreibt die Magic.
    pub fn create(path: &str) -> io::Result<Self> {
        let mut w = BufWriter::new(File::create(path)?);
        w.write_all(&LOG_MAGIC)?;
        w.flush()?;
        Ok(Self {
            w,
            t0: Instant::now(),
        })
    }

    /// Aktueller Doppel-Stempel (Wall + Mono seit `create`).
    #[must_use]
    pub fn stamp(&self) -> Stamp {
        Stamp {
            wall_us: wall_us(),
            mono_us: self.t0.elapsed().as_micros().min(u128::from(u64::MAX)) as u64,
        }
    }

    /// Schreibt einen Record (mit Flush); liefert die Dateibytes.
    pub fn write(&mut self, r: &LogRecord) -> io::Result<usize> {
        let body = bincode::serde::encode_to_vec(r, bincode::config::standard())
            .map_err(|e| io::Error::new(ErrorKind::InvalidInput, e))?;
        if body.len() > LOG_MAX {
            return Err(io::Error::new(ErrorKind::InvalidInput, "Record zu groß"));
        }
        self.w.write_all(&(body.len() as u32).to_le_bytes())?;
        self.w.write_all(&body)?;
        self.w.flush()?;
        Ok(4 + body.len())
    }
}

/// Liest Records aus einer Datei (prüft die Magic).
pub struct Reader {
    r: BufReader<File>,
}

impl Reader {
    /// Öffnet `path` (falsche Magic = Fehler).
    pub fn open(path: &str) -> io::Result<Self> {
        let mut r = BufReader::new(File::open(path)?);
        let mut magic = [0u8; 8];
        r.read_exact(&mut magic).map_err(|e| {
            if e.kind() == ErrorKind::UnexpectedEof {
                io::Error::new(ErrorKind::InvalidData, "keine LBWLOG-Magic")
            } else {
                e
            }
        })?;
        if magic != LOG_MAGIC {
            return Err(io::Error::new(ErrorKind::InvalidData, "keine LBWLOG-Magic"));
        }
        Ok(Self { r })
    }

    /// Nächster Record (`None` nur bei sauberem EOF am Record-Anfang).
    pub fn next_record(&mut self) -> io::Result<Option<LogRecord>> {
        let mut len = [0u8; 4];
        match self.r.read_exact(&mut len) {
            Ok(()) => {}
            Err(e) if e.kind() == ErrorKind::UnexpectedEof => return Ok(None),
            Err(e) => return Err(e),
        }
        let n = u32::from_le_bytes(len) as usize;
        if n > LOG_MAX {
            return Err(io::Error::new(
                ErrorKind::InvalidData,
                "Record-Länge zu groß",
            ));
        }
        let mut body = vec![0u8; n];
        self.r.read_exact(&mut body)?; // zerrissener Schluss = Fehler (Absturz?)
        let (rec, _): (LogRecord, usize) =
            bincode::serde::decode_from_slice(&body, bincode::config::standard())
                .map_err(|e| io::Error::new(ErrorKind::InvalidData, e))?;
        Ok(Some(rec))
    }

    /// Alle Records auf einmal (für kleine Dateien/Tests).
    pub fn all(path: &str) -> io::Result<Vec<LogRecord>> {
        let mut r = Self::open(path)?;
        let mut out = Vec::new();
        while let Some(rec) = r.next_record()? {
            out.push(rec);
        }
        Ok(out)
    }
}

/// Lädt alle Records; ein zerrissener Schluss (Absturz mitten im Record)
/// liefert den Teilstand + `true` statt eines Fehlers (Tools nutzen das).
pub fn load_lenient(path: &str) -> Result<(Vec<LogRecord>, bool), String> {
    let mut r = Reader::open(path).map_err(|e| format!("{path}: {e}"))?;
    let mut out = Vec::new();
    loop {
        match r.next_record() {
            Ok(Some(rec)) => out.push(rec),
            Ok(None) => return Ok((out, false)),
            Err(e) if e.kind() == ErrorKind::UnexpectedEof => return Ok((out, true)),
            Err(e) => return Err(format!("{path}: {e}")),
        }
    }
}

/// Thread-sichere Aufnahme-Front (`Clone` für Eingabe-/Netz-Threads).
/// `Recorder::none()` ist ein No-op ohne jede Datei. Schreibfehler werden
/// einmal auf stderr gemeldet, danach ist die Aufnahme still abgeschaltet —
/// eine volle Platte darf die Sitzung nie abbrechen. `Drop` schreibt
/// best-effort ein `End` (fehlt nur bei Kill/Absturz).
#[derive(Clone, Default)]
pub struct Recorder {
    inner: Option<Arc<Mutex<State>>>,
}

struct State {
    w: Writer,
    failed: bool,
    last_down_mono: Option<u64>,
}

impl Drop for State {
    fn drop(&mut self) {
        if !self.failed {
            let stamp = self.w.stamp();
            let _ = self.w.write(&LogRecord::End { stamp });
        }
    }
}

impl Recorder {
    /// Inaktiver Recorder (keine Datei, alle Methoden No-ops).
    #[must_use]
    pub fn none() -> Self {
        Self::default()
    }

    /// Aktiver Recorder: legt `path` neu an und schreibt `Session`.
    pub fn create(path: &str, app: &str, args: &[String]) -> Result<Self, String> {
        let mut w = Writer::create(path).map_err(|e| format!("{path}: {e}"))?;
        w.write(&LogRecord::Session {
            app: app.into(),
            version: LOG_VERSION,
            args: args.to_vec(),
        })
        .map_err(|e| format!("{path}: {e}"))?;
        Ok(Self {
            inner: Some(Arc::new(Mutex::new(State {
                w,
                failed: false,
                last_down_mono: None,
            }))),
        })
    }

    /// Nimmt auf (Datei offen und bisher fehlerfrei)?
    #[must_use]
    pub fn is_active(&self) -> bool {
        self.inner.is_some()
    }

    fn push(&self, rec: LogRecord) {
        let Some(inner) = &self.inner else { return };
        let Ok(mut s) = inner.lock() else { return };
        if s.failed {
            return;
        }
        if let Err(e) = s.w.write(&rec) {
            s.failed = true;
            eprintln!("[record] Schreiben gescheitert ({e}) — Aufnahme gestoppt");
        }
    }

    fn stamp(&self) -> Option<Stamp> {
        self.inner.as_ref()?.lock().ok().map(|s| s.w.stamp())
    }

    /// Gesendete Server-Nachricht (`wire_bytes` von `write_msg`; Body wird
    /// nach-kodiert — `bincode::standard` ist deterministisch).
    pub fn msg_server(&self, m: &ServerMsg, wire_bytes: usize) {
        let (Some(stamp), Ok(body)) = (self.stamp(), encode_msg(m)) else {
            return;
        };
        self.push(LogRecord::Msg {
            stamp,
            dir: Dir::SrvToCli,
            kind: MsgKind::of_server(m),
            wire_bytes,
            hash: fnv1a64(&body),
            body,
        });
    }

    /// Empfangene Server-Nachricht mit exakten Leitungsbytes.
    pub fn msg_server_raw(&self, m: &ServerMsg, body: &[u8]) {
        let Some(stamp) = self.stamp() else { return };
        self.push(LogRecord::Msg {
            stamp,
            dir: Dir::SrvToCli,
            kind: MsgKind::of_server(m),
            wire_bytes: HEADER + body.len(),
            hash: fnv1a64(body),
            body: body.to_vec(),
        });
    }

    /// Gesendete Client-Nachricht (s. `msg_server` zum Nach-Kodieren).
    pub fn msg_client(&self, m: &ClientMsg, wire_bytes: usize) {
        let (Some(stamp), Ok(body)) = (self.stamp(), encode_msg(m)) else {
            return;
        };
        self.push(LogRecord::Msg {
            stamp,
            dir: Dir::CliToSrv,
            kind: MsgKind::of_client(m),
            wire_bytes,
            hash: fnv1a64(&body),
            body,
        });
    }

    /// Empfangene Client-Nachricht mit exakten Leitungsbytes.
    pub fn msg_client_raw(&self, m: &ClientMsg, body: &[u8]) {
        let Some(stamp) = self.stamp() else { return };
        self.push(LogRecord::Msg {
            stamp,
            dir: Dir::CliToSrv,
            kind: MsgKind::of_client(m),
            wire_bytes: HEADER + body.len(),
            hash: fnv1a64(body),
            body: body.to_vec(),
        });
    }

    /// Server-Pipeline-Ergebnis eines Frames.
    pub fn frame(
        &self,
        frame: u64,
        texts: usize,
        text_changed: bool,
        text_bytes: usize,
        tile: Option<TileStat>,
        ms: FrameMs,
    ) {
        let Some(stamp) = self.stamp() else { return };
        self.push(LogRecord::Frame {
            stamp,
            frame,
            texts,
            text_changed,
            text_bytes,
            tile,
            ms,
        });
    }

    /// Client: AV1-Decode-Ergebnis einer Kachel.
    pub fn decode(&self, tile_bytes: usize, ms: f32, ok: bool) {
        let Some(stamp) = self.stamp() else { return };
        self.push(LogRecord::Decode {
            stamp,
            tile_bytes,
            ms,
            ok,
        });
    }

    /// Server: Eingabe-Empfang → Injektion.
    pub fn inject(&self, kind: MsgKind, ms: f32, ok: bool) {
        let Some(stamp) = self.stamp() else { return };
        self.push(LogRecord::Inject {
            stamp,
            kind,
            ms,
            ok,
        });
    }

    /// Verbindung steht (`down_ms` seit dem letzten `gap_down`, None beim Start).
    pub fn gap_up(&self, peer: &str) {
        let Some(inner) = &self.inner else { return };
        let Ok(mut s) = inner.lock() else { return };
        let stamp = s.w.stamp();
        let down_ms = s
            .last_down_mono
            .map(|d| (stamp.mono_us.saturating_sub(d)) as f32 / 1000.0);
        s.last_down_mono = None;
        if s.failed {
            return;
        }
        let rec = LogRecord::Gap {
            stamp,
            event: GapEvent::Up {
                peer: peer.into(),
                down_ms,
            },
        };
        if let Err(e) = s.w.write(&rec) {
            s.failed = true;
            eprintln!("[record] Schreiben gescheitert ({e}) — Aufnahme gestoppt");
        }
    }

    /// Verbindung weg (Grund merken + Stempel für die nächste Down-Dauer).
    pub fn gap_down(&self, reason: &str) {
        let Some(inner) = &self.inner else { return };
        let Ok(mut s) = inner.lock() else { return };
        let stamp = s.w.stamp();
        s.last_down_mono = Some(stamp.mono_us);
        if s.failed {
            return;
        }
        let rec = LogRecord::Gap {
            stamp,
            event: GapEvent::Down {
                reason: reason.into(),
            },
        };
        if let Err(e) = s.w.write(&rec) {
            s.failed = true;
            eprintln!("[record] Schreiben gescheitert ({e}) — Aufnahme gestoppt");
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::record::{Dir, FrameMs, GapEvent, LOG_VERSION, MsgKind};

    fn tmp(name: &str) -> String {
        format!(
            "{}/lbwlog-test-{}-{name}",
            std::env::temp_dir().display(),
            std::process::id()
        )
    }

    fn samples() -> Vec<LogRecord> {
        let s = Stamp {
            wall_us: 7,
            mono_us: 9,
        };
        vec![
            LogRecord::Session {
                app: "t".into(),
                version: LOG_VERSION,
                args: vec!["a".into()],
            },
            LogRecord::Msg {
                stamp: s,
                dir: Dir::SrvToCli,
                kind: MsgKind::Tile,
                wire_bytes: 100,
                hash: 1,
                body: vec![1, 2, 3],
            },
            LogRecord::Frame {
                stamp: s,
                frame: 3,
                texts: 2,
                text_changed: true,
                text_bytes: 40,
                tile: None,
                ms: FrameMs::default(),
            },
            LogRecord::Decode {
                stamp: s,
                tile_bytes: 50,
                ms: 1.5,
                ok: true,
            },
            LogRecord::Inject {
                stamp: s,
                kind: MsgKind::Key,
                ms: 0.5,
                ok: false,
            },
            LogRecord::Gap {
                stamp: s,
                event: GapEvent::Up {
                    peer: "p".into(),
                    down_ms: Some(12.0),
                },
            },
            LogRecord::End { stamp: s },
        ]
    }

    #[test]
    fn roundtrip_all_variants() {
        let p = tmp("roundtrip.lbwlog");
        let mut w = Writer::create(&p).unwrap();
        for r in &samples() {
            w.write(r).unwrap();
        }
        drop(w);
        assert_eq!(Reader::all(&p).unwrap(), samples());
        std::fs::remove_file(&p).unwrap();
    }

    #[test]
    fn wrong_magic_is_rejected() {
        let p = tmp("magic.lbwlog");
        std::fs::write(&p, b"KEINEMAGIC!!").unwrap();
        assert!(Reader::open(&p).is_err());
        std::fs::write(&p, b"kurz").unwrap();
        assert!(Reader::open(&p).is_err());
        std::fs::remove_file(&p).unwrap();
    }

    #[test]
    fn torn_tail_is_an_error_not_silent_eof() {
        let p = tmp("torn.lbwlog");
        let mut w = Writer::create(&p).unwrap();
        w.write(&samples()[0]).unwrap();
        drop(w);
        // Längen-Header für 100 Byte anhängen, aber nur 3 liefern.
        let mut f = std::fs::OpenOptions::new().append(true).open(&p).unwrap();
        f.write_all(&100u32.to_le_bytes()).unwrap();
        f.write_all(&[1, 2, 3]).unwrap();
        drop(f);
        let mut r = Reader::open(&p).unwrap();
        assert_eq!(r.next_record().unwrap(), Some(samples()[0].clone()));
        assert_eq!(
            r.next_record().unwrap_err().kind(),
            ErrorKind::UnexpectedEof
        );
        std::fs::remove_file(&p).unwrap();
    }

    #[test]
    fn stamps_are_monotonic() {
        let p = tmp("stamp.lbwlog");
        let w = Writer::create(&p).unwrap();
        let a = w.stamp();
        std::thread::sleep(Duration::from_millis(2));
        let b = w.stamp();
        assert!(b.mono_us >= a.mono_us);
        assert!(a.wall_us > 0 && b.wall_us >= a.wall_us);
        std::fs::remove_file(&p).unwrap();
    }

    #[test]
    fn recorder_tracks_gaps_and_ends_on_drop() {
        use lbw_common::ServerMsg;
        let p = tmp("rec.lbwlog");
        {
            let r = Recorder::create(&p, "t", &["t".into()]).unwrap();
            assert!(r.is_active());
            r.gap_up("peer");
            r.msg_server(&ServerMsg::Hello, 9);
            r.gap_down("weg");
            std::thread::sleep(Duration::from_millis(2));
            r.gap_up("peer");
            // `End` kommt per Drop am Blockende.
        }
        let recs = Reader::all(&p).unwrap();
        assert!(matches!(recs[0], LogRecord::Session { .. }));
        assert!(matches!(
            recs[1],
            LogRecord::Gap {
                event: GapEvent::Up { down_ms: None, .. },
                ..
            }
        ));
        assert!(matches!(
            recs[3],
            LogRecord::Gap {
                event: GapEvent::Down { .. },
                ..
            }
        ));
        let LogRecord::Gap {
            event: GapEvent::Up {
                down_ms: Some(d), ..
            },
            ..
        } = &recs[4]
        else {
            panic!("{recs:?}");
        };
        assert!(*d >= 2.0, "{d}");
        assert!(matches!(recs[5], LogRecord::End { .. }));
        assert_eq!(recs.len(), 6);
        std::fs::remove_file(&p).unwrap();

        // No-op-Recorder: keine Panik, keine Datei.
        let n = Recorder::none();
        assert!(!n.is_active());
        n.gap_up("x");
        n.msg_server(&ServerMsg::Hello, 1);
    }
}
