//! `03_stats` — Pro-Datei-Auswertung von `.lbwlog`-Aufzeichnungen.
//!
//! Reine Funktionen über Records einer Datei: Durchsatz, Gaps, Latenzen.
//! Wichtig: Server- und Client-Log beschreiben *dieselben* Bytes — Summen
//! werden daher **pro Datei** berichtet (kein Doppelzählen). Zusammenführung
//! über Dateien hinweg: `04_summary`, Tiefenanalyse: `05_deep`.

use std::collections::{BTreeMap, VecDeque};

use crate::record::{Dir, GapEvent, LogRecord, MsgKind};

/// Kanal-Budget in Byte/s (stockende 6-kB/s-Strecke).
pub const BUDGET_BPS: u64 = 6000;

/// Eine geladene Logdatei.
pub struct FileLog {
    pub name: String,
    pub records: Vec<LogRecord>,
    /// Log-Version aus dem `Session`-Record (1 = v2-Bodies, ≥ 2 = v3-Bodies).
    pub version: u16,
}

/// Log-Version einer Record-Liste (erster `Session`-Record, sonst aktuell).
pub fn log_version(records: &[LogRecord]) -> u16 {
    records
        .iter()
        .find_map(|r| match r {
            LogRecord::Session { version, .. } => Some(*version),
            _ => None,
        })
        .unwrap_or(crate::record::LOG_VERSION)
}

/// Latenz-Verteilung (ms).
#[derive(Clone, Copy, Debug, Default)]
pub struct Lat {
    pub n: usize,
    pub avg: f64,
    pub p50: f64,
    pub p90: f64,
    pub p99: f64,
    pub max: f64,
}

impl Lat {
    /// Baut die Verteilung (sortiert intern).
    pub fn of(mut v: Vec<f64>) -> Self {
        if v.is_empty() {
            return Self::default();
        }
        v.sort_by(|a, b| a.total_cmp(b));
        let n = v.len();
        let q = |p: f64| v[((p / 100.0 * n as f64) as usize).min(n - 1)];
        Self {
            n,
            avg: v.iter().sum::<f64>() / n as f64,
            p50: q(50.0),
            p90: q(90.0),
            p99: q(99.0),
            max: v[n - 1],
        }
    }
}

/// Nachrichten-Zähler je (Richtung, Art).
#[derive(Clone, Debug)]
pub struct KindStat {
    pub dir: Dir,
    pub kind: MsgKind,
    pub count: usize,
    pub bytes: u64,
}

/// Durchsatz einer Wall-Sekunde (Leitungsbytes inkl. Header).
#[derive(Clone, Copy, Debug)]
pub struct SecStat {
    pub sec: u64,
    pub srv: u64,
    pub cli: u64,
}

/// Eine Funklücke (Down … Up).
#[derive(Clone, Debug)]
pub struct GapInfo {
    pub peer: String,
    pub reason: String,
    pub down_ms: Option<f32>,
}

/// Auswertung einer einzelnen Datei (kein Doppelzählen über Dateien hinweg).
#[derive(Clone, Debug)]
pub struct FileSummary {
    pub name: String,
    pub app: String,
    pub records: usize,
    pub wall_secs: f64,
    pub mono_secs: f64,
    pub has_end: bool,
    pub msgs: usize,
    pub bytes_srv: u64,
    pub bytes_cli: u64,
    pub kinds: Vec<KindStat>,
    pub timeline: Vec<SecStat>,
    pub over_budget_secs: usize,
    /// Max. Füllstand der virtuellen 6-kB/s-Warteschlange (Bytes).
    pub queue_peak_b: u64,
    pub gaps: Vec<GapInfo>,
    pub down_total_ms: f64,
    /// Client-Sicht: Eingabe-Sendung → nächste sichtbare Änderung.
    pub input_visible_ms: Lat,
    /// Client-Sicht: Eingabe-Sendung → nächster Text (`AddText` & Co.).
    pub input_text_ms: Lat,
    /// Client-Sicht: Eingabe-Sendung → nächste Kachel.
    pub input_tile_ms: Lat,
    /// Server-Sicht: Eingabe-Empfang → nächster Frame mit Änderung.
    pub input_frame_ms: Lat,
}

/// Diskrete Eingabe (Taste, Text, Klick) — `MouseMove` ist ausgenommen:
/// 50–100 Moves/s würden sonst jede wartende Eingabe überschreiben und die
/// Latenz systematisch unterschätzen (Maus-Spam-Falle).
fn is_input(m: &LogRecord) -> bool {
    matches!(
        m,
        LogRecord::Msg {
            dir: Dir::CliToSrv,
            kind: MsgKind::Key | MsgKind::Text | MsgKind::Button,
            ..
        }
    )
}

fn is_visible(m: &LogRecord) -> bool {
    is_visible_text(m) || is_visible_tile(m)
}

/// Sichtbare Textänderung (Echo in ms — das „Tippgefühl" bei 6 kB/s).
fn is_visible_text(m: &LogRecord) -> bool {
    matches!(
        m,
        LogRecord::Msg {
            dir: Dir::SrvToCli,
            kind: MsgKind::ClearText | MsgKind::AddText | MsgKind::RemoveText,
            ..
        }
    )
}

/// Sichtbare Bildänderung (Kachel — braucht 250–850 ms Leitung bei 6 kB/s).
fn is_visible_tile(m: &LogRecord) -> bool {
    matches!(
        m,
        LogRecord::Msg {
            dir: Dir::SrvToCli,
            kind: MsgKind::Tile,
            ..
        }
    )
}

fn sample_ms(now_us: u64, t0: u64) -> f64 {
    now_us.saturating_sub(t0) as f64 / 1000.0
}

pub(crate) fn stamp_of(m: &LogRecord) -> Option<(u64, u64)> {
    match m {
        LogRecord::Msg { stamp, .. }
        | LogRecord::Frame { stamp, .. }
        | LogRecord::Decode { stamp, .. }
        | LogRecord::Inject { stamp, .. }
        | LogRecord::Gap { stamp, .. }
        | LogRecord::End { stamp } => Some((stamp.wall_us, stamp.mono_us)),
        LogRecord::Session { .. } => None,
    }
}

pub(crate) fn app_of(records: &[LogRecord]) -> String {
    records
        .iter()
        .find_map(|r| match r {
            LogRecord::Session { app, .. } => Some(app.clone()),
            _ => None,
        })
        .unwrap_or_else(|| "?".into())
}

/// Wertet eine Datei aus (reine Funktion — gut testbar).
pub fn summarize_file(name: &str, records: &[LogRecord]) -> FileSummary {
    let mut kinds: BTreeMap<(u8, u8), (usize, u64)> = BTreeMap::new();
    let mut timeline: BTreeMap<u64, (u64, u64)> = BTreeMap::new();
    let mut gaps = Vec::new();
    let mut pending_down: Option<String> = None;
    let mut down_total_ms = 0.0;
    let mut bytes_srv = 0;
    let mut bytes_cli = 0;
    let mut msgs = 0;
    let mut vis_samples = Vec::new();
    let mut txt_samples = Vec::new();
    let mut tile_samples = Vec::new();
    let mut frm_samples = Vec::new();
    // FIFO je Echo-Art: jede Eingabe wird genau einmal gegen das nächste
    // passende Echo gematcht (kein Überschreiben der wartenden Eingabe).
    let mut pending_vis: VecDeque<u64> = VecDeque::new();
    let mut pending_txt: VecDeque<u64> = VecDeque::new();
    let mut pending_tile: VecDeque<u64> = VecDeque::new();
    let mut pending_frm: VecDeque<u64> = VecDeque::new();
    let mut first_wall: Option<u64> = None;
    let mut last_wall = 0;
    let mut first_mono: Option<u64> = None;
    let mut last_mono = 0;
    let mut has_end = false;

    for r in records {
        if let Some((w, m)) = stamp_of(r) {
            first_wall.get_or_insert(w);
            last_wall = last_wall.max(w);
            first_mono.get_or_insert(m);
            last_mono = last_mono.max(m);
        }
        match r {
            LogRecord::Session { .. } => {}
            LogRecord::End { .. } => has_end = true,
            LogRecord::Msg {
                stamp,
                dir,
                kind,
                wire_bytes,
                ..
            } => {
                msgs += 1;
                let b = *wire_bytes as u64;
                match dir {
                    Dir::SrvToCli => bytes_srv += b,
                    Dir::CliToSrv => bytes_cli += b,
                }
                let k = kinds.entry((*dir as u8, *kind as u8)).or_default();
                k.0 += 1;
                k.1 += b;
                let sec = stamp.wall_us / 1_000_000;
                let t = timeline.entry(sec).or_default();
                match dir {
                    Dir::SrvToCli => t.0 += b,
                    Dir::CliToSrv => t.1 += b,
                }
                if is_input(r) {
                    for q in [
                        &mut pending_vis,
                        &mut pending_txt,
                        &mut pending_tile,
                        &mut pending_frm,
                    ] {
                        q.push_back(stamp.mono_us);
                    }
                } else {
                    if is_visible(r)
                        && let Some(t0) = pending_vis.pop_front()
                    {
                        vis_samples.push(sample_ms(stamp.mono_us, t0));
                    }
                    if is_visible_text(r)
                        && let Some(t0) = pending_txt.pop_front()
                    {
                        txt_samples.push(sample_ms(stamp.mono_us, t0));
                    }
                    if is_visible_tile(r)
                        && let Some(t0) = pending_tile.pop_front()
                    {
                        tile_samples.push(sample_ms(stamp.mono_us, t0));
                    }
                }
            }
            LogRecord::Frame {
                stamp,
                tile,
                text_changed,
                ..
            } => {
                if (tile.is_some() || *text_changed)
                    && let Some(t0) = pending_frm.pop_front()
                {
                    frm_samples.push(sample_ms(stamp.mono_us, t0));
                }
            }
            LogRecord::Gap { event, .. } => match event {
                GapEvent::Down { reason } => pending_down = Some(reason.clone()),
                GapEvent::Up { peer, down_ms } => {
                    if let Some(d) = down_ms {
                        down_total_ms += f64::from(*d);
                    }
                    gaps.push(GapInfo {
                        peer: peer.clone(),
                        reason: pending_down.take().unwrap_or_else(|| "?".into()),
                        down_ms: *down_ms,
                    });
                }
            },
            LogRecord::Decode { .. } | LogRecord::Inject { .. } => {}
        }
    }
    if let Some(reason) = pending_down {
        gaps.push(GapInfo {
            peer: String::new(),
            reason,
            down_ms: None,
        });
    }
    let kinds = kinds
        .into_iter()
        .map(|((d, k), (count, bytes))| KindStat {
            dir: if d == Dir::SrvToCli as u8 {
                Dir::SrvToCli
            } else {
                Dir::CliToSrv
            },
            kind: msg_kind_of(k),
            count,
            bytes,
        })
        .collect();
    let timeline: Vec<SecStat> = timeline
        .into_iter()
        .map(|(sec, (srv, cli))| SecStat { sec, srv, cli })
        .collect();
    let over_budget_secs = timeline.iter().filter(|s| s.srv > BUDGET_BPS).count();
    let queue_peak_b = queue_peak(&timeline);
    let wall_secs = match (first_wall, last_wall) {
        (Some(a), b) => (b.saturating_sub(a)) as f64 / 1_000_000.0,
        _ => 0.0,
    };
    let mono_secs = match (first_mono, last_mono) {
        (Some(a), b) => (b.saturating_sub(a)) as f64 / 1_000_000.0,
        _ => 0.0,
    };
    FileSummary {
        name: name.into(),
        app: app_of(records),
        records: records.len(),
        wall_secs,
        mono_secs,
        has_end,
        msgs,
        bytes_srv,
        bytes_cli,
        kinds,
        timeline,
        over_budget_secs,
        queue_peak_b,
        gaps,
        down_total_ms,
        input_visible_ms: Lat::of(vis_samples),
        input_text_ms: Lat::of(txt_samples),
        input_tile_ms: Lat::of(tile_samples),
        input_frame_ms: Lat::of(frm_samples),
    }
}

/// Max. Füllstand einer virtuellen Queue mit 6-kB/s-Abfluss über die
/// (sekundengenaue, aufsteigende) Timeline: Bursts tragen Überhang in Folge-
/// Sekunden (12 kB in s1 + 0 kB in s2 = 2 s verstopfter Kanal). Lücken lassen
/// die Queue leerlaufen. Reine Funktion — gut testbar.
pub fn queue_peak(timeline: &[SecStat]) -> u64 {
    let mut q: u64 = 0;
    let mut peak: u64 = 0;
    let mut prev: Option<u64> = None;
    for t in timeline {
        if let Some(p) = prev {
            q = q.saturating_sub(t.sec.saturating_sub(p).saturating_mul(BUDGET_BPS));
        }
        q += t.srv;
        peak = peak.max(q);
        prev = Some(t.sec);
    }
    peak
}

fn msg_kind_of(k: u8) -> MsgKind {
    const ALL: [MsgKind; 10] = [
        MsgKind::SrvHello,
        MsgKind::ClearText,
        MsgKind::AddText,
        MsgKind::Tile,
        MsgKind::CliHello,
        MsgKind::MouseMove,
        MsgKind::Button,
        MsgKind::Text,
        MsgKind::Key,
        MsgKind::RemoveText,
    ];
    ALL.iter()
        .find(|m| **m as u8 == k)
        .copied()
        .unwrap_or(MsgKind::SrvHello)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::record::Stamp;

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
            version: 1,
            args: vec![],
        }
    }

    #[test]
    fn lat_reports_percentiles() {
        let l = Lat::of((1..=100).map(|v| v as f64).collect());
        assert_eq!((l.n, l.avg, l.max), (100, 50.5, 100.0));
        assert_eq!((l.p50, l.p90, l.p99), (51.0, 91.0, 100.0));
        assert_eq!(Lat::of(vec![]).n, 0);
    }

    #[test]
    fn file_summary_counts_timeline_gaps_and_latency() {
        let recs = vec![
            session("lbw-client"),
            LogRecord::Gap {
                stamp: stamp(1_000_000, 0),
                event: GapEvent::Up {
                    peer: "p".into(),
                    down_ms: None,
                },
            },
            // Eingabe bei mono 100 ms, sichtbare Kachel bei 350 ms.
            msg(1_100_000, 100_000, Dir::CliToSrv, MsgKind::Text, 20, 1),
            msg(1_350_000, 350_000, Dir::SrvToCli, MsgKind::Tile, 7000, 2),
            msg(1_360_000, 360_000, Dir::SrvToCli, MsgKind::Tile, 100, 3),
            LogRecord::Gap {
                stamp: stamp(2_000_000, 1_000_000),
                event: GapEvent::Down {
                    reason: "eof".into(),
                },
            },
            LogRecord::Gap {
                stamp: stamp(2_500_000, 1_500_000),
                event: GapEvent::Up {
                    peer: "p".into(),
                    down_ms: Some(500.0),
                },
            },
            LogRecord::End {
                stamp: stamp(2_500_000, 1_500_000),
            },
        ];
        let s = summarize_file("c", &recs);
        assert_eq!(s.app, "lbw-client");
        assert_eq!(s.msgs, 3);
        assert_eq!((s.bytes_srv, s.bytes_cli), (7100, 20));
        assert!(s.has_end);
        // Timeline: Sekunde 1 trägt 7100 Srv-Bytes → über Budget.
        assert_eq!(s.timeline.len(), 1);
        assert_eq!((s.timeline[0].srv, s.timeline[0].cli), (7100, 20));
        assert_eq!(s.over_budget_secs, 1);
        // Gaps: erstes Up (down None) + Reconnect mit 500 ms Down.
        assert_eq!(s.gaps.len(), 2);
        assert_eq!(s.gaps[1].down_ms, Some(500.0));
        assert_eq!(s.gaps[1].reason, "eof");
        assert_eq!(s.down_total_ms, 500.0);
        // Latenz: 350 − 100 = 250 ms.
        assert_eq!(s.input_visible_ms.n, 1);
        assert_eq!(s.input_visible_ms.p50, 250.0);
        assert_eq!(s.input_frame_ms.n, 0);
    }

    #[test]
    fn mouse_moves_neither_trigger_nor_clobber_latency() {
        let recs = vec![
            session("lbw-client"),
            // Taste bei 100 ms, danach Maus-Spam, Kachel bei 350 ms.
            msg(1_100_000, 100_000, Dir::CliToSrv, MsgKind::Key, 20, 1),
            msg(1_150_000, 150_000, Dir::CliToSrv, MsgKind::MouseMove, 12, 2),
            msg(1_250_000, 250_000, Dir::CliToSrv, MsgKind::MouseMove, 12, 3),
            msg(1_350_000, 350_000, Dir::SrvToCli, MsgKind::Tile, 7000, 4),
        ];
        let s = summarize_file("c", &recs);
        // Ab der Taste (nicht ab dem letzten Move): 350 − 100 = 250 ms.
        assert_eq!(s.input_visible_ms.n, 1);
        assert_eq!(s.input_visible_ms.p50, 250.0);
        // Reine Maus ohne Taste: kein Sample.
        let recs = vec![
            session("lbw-client"),
            msg(1_150_000, 150_000, Dir::CliToSrv, MsgKind::MouseMove, 12, 2),
            msg(1_350_000, 350_000, Dir::SrvToCli, MsgKind::Tile, 7000, 4),
        ];
        let s = summarize_file("c", &recs);
        assert_eq!(s.input_visible_ms.n, 0);
        assert_eq!(s.input_tile_ms.n, 0);
    }

    #[test]
    fn queue_peak_tracks_carryover_and_idle_drain() {
        let t = |sec, srv| SecStat { sec, srv, cli: 0 };
        // 12 kB-Burst: Queue steht 2 s (nichts fließt vorher ab).
        assert_eq!(queue_peak(&[t(1, 12_000), t(2, 0)]), 12_000);
        // Überhang trägt weiter: 9000 + (9000 − 6000 Abfluss) = 12000.
        assert_eq!(queue_peak(&[t(1, 9_000), t(2, 9_000)]), 12_000);
        // 4 s Lücke lassen 9000 B vollständig ablaufen.
        assert_eq!(queue_peak(&[t(1, 9_000), t(5, 1_000)]), 9_000);
        assert_eq!(queue_peak(&[]), 0);
    }

    #[test]
    fn latency_matches_fifo_and_splits_text_vs_tile() {
        let recs = vec![
            session("lbw-client"),
            msg(1_100_000, 100_000, Dir::CliToSrv, MsgKind::Key, 20, 1),
            msg(1_150_000, 150_000, Dir::SrvToCli, MsgKind::AddText, 60, 2),
            msg(1_200_000, 200_000, Dir::CliToSrv, MsgKind::Text, 20, 3),
            msg(1_400_000, 400_000, Dir::SrvToCli, MsgKind::Tile, 7000, 4),
            msg(1_500_000, 500_000, Dir::SrvToCli, MsgKind::Tile, 7000, 5),
        ];
        let s = summarize_file("c", &recs);
        // Sichtbar: Taste→AddText (50), Text→Tile (200).
        assert_eq!(s.input_visible_ms.n, 2);
        assert_eq!(s.input_visible_ms.avg, 125.0);
        // Text-Echo nur für die erste Eingabe (50), Kachel-Echo für beide
        // (FIFO: 400−100=300, 500−200=300).
        assert_eq!((s.input_text_ms.n, s.input_text_ms.p50), (1, 50.0));
        assert_eq!((s.input_tile_ms.n, s.input_tile_ms.p50), (2, 300.0));
    }
}
