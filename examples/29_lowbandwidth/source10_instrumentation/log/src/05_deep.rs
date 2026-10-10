//! `05_deep` — Tiefenanalyse für `lbw-logstat --deep`.
//!
//! Reine Funktionen über Records: Verbindungs-Zeitleiste, Dedup-Totals,
//! Text-Churn, Idle-Phasen, Kachel-Verteilung, Erkennungs-Zeiten je Textzahl,
//! Zustell-Verzögerung (Server→Client, Positions-Matching) und Maus-Statistik.

use std::collections::BTreeMap;

use lbw_common::ServerMsg;
use lbw_common::framing::decode_server_logged;

use crate::record::{Dir, GapEvent, LogRecord, MsgKind};
use crate::stats::{FileLog, Lat, SecStat, app_of, stamp_of};

/// Eine Verbindung (Up … Down) mit Zählern je Richtung.
#[derive(Clone, Debug)]
pub struct ConnStat {
    pub index: usize,
    pub peer: String,
    pub srv_msgs: usize,
    pub srv_bytes: u64,
    pub cli_msgs: usize,
    pub cli_bytes: u64,
    pub end: String,
}

/// Teilt die Records an Gap-Markern in Verbindungen (reine Funktion).
pub fn conn_split(records: &[LogRecord]) -> Vec<ConnStat> {
    let mut out = Vec::new();
    let mut cur: Option<ConnStat> = None;
    let mut index = 0;
    for r in records {
        match r {
            LogRecord::Gap {
                event: GapEvent::Up { peer, .. },
                ..
            } => {
                if let Some(c) = cur.take() {
                    out.push(c);
                }
                index += 1;
                cur = Some(ConnStat {
                    index,
                    peer: peer.clone(),
                    srv_msgs: 0,
                    srv_bytes: 0,
                    cli_msgs: 0,
                    cli_bytes: 0,
                    end: "(dateiende)".into(),
                });
            }
            LogRecord::Msg {
                dir, wire_bytes, ..
            } => {
                if let Some(c) = cur.as_mut() {
                    match dir {
                        Dir::SrvToCli => {
                            c.srv_msgs += 1;
                            c.srv_bytes += *wire_bytes as u64;
                        }
                        Dir::CliToSrv => {
                            c.cli_msgs += 1;
                            c.cli_bytes += *wire_bytes as u64;
                        }
                    }
                }
            }
            LogRecord::Gap {
                event: GapEvent::Down { reason },
                ..
            } => {
                if let Some(c) = cur.as_mut() {
                    c.end = reason.clone();
                }
            }
            _ => {}
        }
    }
    if let Some(c) = cur.take() {
        out.push(c);
    }
    // Aufeinanderfolgende Downs (z. B. Refused-Retries) erzeugen keine
    // leeren Verbindungen: nur Conns mit Daten oder Up-Marker zählen schon.
    out
}

/// Ein Gap mit relativer Zeit (s seit erster Wall-Clock der Datei).
#[derive(Clone, Debug)]
pub struct GapStamp {
    pub t_rel_s: f64,
    pub up: bool,
    pub peer: String,
    pub reason: String,
    pub down_ms: Option<f32>,
}

/// Gap-Zeitleiste (relativ, für lange Sessions lesbar).
pub fn gap_timeline(records: &[LogRecord]) -> Vec<GapStamp> {
    let w0 = records
        .iter()
        .filter_map(|r| stamp_of(r).map(|(w, _)| w))
        .min();
    let mut out = Vec::new();
    for r in records {
        if let LogRecord::Gap { stamp, event } = r {
            let t_rel_s = match w0 {
                Some(a) => (stamp.wall_us.saturating_sub(a)) as f64 / 1_000_000.0,
                None => 0.0,
            };
            let (up, peer, reason, down_ms) = match event {
                GapEvent::Up { peer, down_ms } => (true, peer.clone(), String::new(), *down_ms),
                GapEvent::Down { reason } => (false, String::new(), reason.clone(), None),
            };
            out.push(GapStamp {
                t_rel_s,
                up,
                peer,
                reason,
                down_ms,
            });
        }
    }
    out
}

/// Dedup-Totals über einen Nachrichtenstrom (vgl. Top-5 in `04_summary`).
#[derive(Clone, Copy, Debug, Default)]
pub struct DedupTotals {
    pub total_n: usize,
    pub total_b: u64,
    pub uniq_n: usize,
    pub uniq_b: u64,
    pub wasted: u64,
    pub hist_1: usize,
    pub hist_2_10: usize,
    pub hist_11_100: usize,
    pub hist_100p: usize,
}

/// Zählt Wiederholungen einer Nachrichtenart (pro Hash).
/// Einträge tragen die Log-Version ihrer Datei (ungenutzt, aber einheitlich).
pub fn dedup_totals(records: &[(&LogRecord, u16)], kind: MsgKind) -> DedupTotals {
    let mut map: BTreeMap<u64, (usize, usize)> = BTreeMap::new();
    for (r, _) in records {
        if let LogRecord::Msg {
            kind: k,
            hash,
            body,
            ..
        } = r
            && *k == kind
        {
            let e = map.entry(*hash).or_insert((0, body.len()));
            e.0 += 1;
        }
    }
    let mut t = DedupTotals::default();
    for (_, (n, b)) in map {
        t.total_n += n;
        t.total_b += (n * b) as u64;
        t.uniq_n += 1;
        t.uniq_b += b as u64;
        match n {
            1 => t.hist_1 += 1,
            2..=10 => t.hist_2_10 += 1,
            11..=100 => t.hist_11_100 += 1,
            _ => t.hist_100p += 1,
        }
    }
    t.wasted = t.total_b.saturating_sub(t.uniq_b);
    t
}

/// Texte pro `ClearText`-Abschnitt: (Abschnitte, Verteilung der AddText-Zahl).
pub fn adds_per_clear(records: &[LogRecord]) -> (usize, Lat) {
    let mut clears = 0;
    let mut cur = 0;
    let mut v = Vec::new();
    for r in records {
        match r {
            LogRecord::Msg {
                kind: MsgKind::ClearText,
                ..
            } => {
                if cur > 0 {
                    v.push(cur as f64);
                }
                cur = 0;
                clears += 1;
            }
            LogRecord::Msg {
                kind: MsgKind::AddText,
                ..
            } => cur += 1,
            _ => {}
        }
    }
    if cur > 0 {
        v.push(cur as f64);
    }
    (clears, Lat::of(v))
}

/// Top-`n` meistgesendete Texte als (Anzahl, 60-Zeichen-Sample).
/// Einträge tragen die Log-Version ihrer Datei (Body-Dekodierung).
pub fn top_texts(records: &[(&LogRecord, u16)], n: usize) -> Vec<(usize, String)> {
    let mut map: BTreeMap<u64, (usize, String)> = BTreeMap::new();
    for (r, version) in records {
        if let LogRecord::Msg {
            kind: MsgKind::AddText,
            hash,
            body,
            ..
        } = r
        {
            let e = map.entry(*hash).or_insert_with(|| {
                let txt = decode_server_logged(body, *version)
                    .ok()
                    .and_then(|m| match m {
                        ServerMsg::AddText(t) => Some(t.text),
                        _ => None,
                    })
                    .unwrap_or_default();
                (0, txt.chars().take(60).collect())
            });
            e.0 += 1;
        }
    }
    let mut out: Vec<(usize, String)> = map.into_values().collect();
    out.sort_by_key(|e| std::cmp::Reverse(e.0));
    out.truncate(n);
    out
}

/// Top-`n` Sekunden nach Server-Durchsatz.
pub fn top_seconds(timeline: &[SecStat], n: usize) -> Vec<SecStat> {
    let mut out = timeline.to_vec();
    out.sort_by_key(|s| std::cmp::Reverse(s.srv));
    out.truncate(n);
    out
}

/// Stille-Phasen ohne Server→Client-Nachricht: (längste in s, Anzahl > 5 s).
pub fn idle_stats(records: &[LogRecord]) -> (f64, usize) {
    let mut last: Option<u64> = None;
    let mut longest = 0;
    let mut over_5s = 0;
    for r in records {
        if let LogRecord::Msg {
            stamp,
            dir: Dir::SrvToCli,
            ..
        } = r
        {
            if let Some(t) = last {
                let gap = stamp.wall_us.saturating_sub(t);
                longest = longest.max(gap);
                if gap > 5_000_000 {
                    over_5s += 1;
                }
            }
            last = Some(stamp.wall_us);
        }
    }
    (longest as f64 / 1_000_000.0, over_5s)
}

/// Kachelgrößen-Verteilung (Byte, aus Frame-Records).
pub fn tile_sizes(records: &[LogRecord]) -> Lat {
    Lat::of(
        records
            .iter()
            .filter_map(|r| match r {
                LogRecord::Frame { tile: Some(t), .. } => Some(t.bytes as f64),
                _ => None,
            })
            .collect(),
    )
}

/// Kachel-Positionen im 3×4-Raster (1280×720 geviertelt/gedrittelt).
pub fn tile_raster(records: &[LogRecord]) -> [[usize; 4]; 3] {
    let mut grid = [[0usize; 4]; 3];
    for r in records {
        if let LogRecord::Frame { tile: Some(t), .. } = r {
            grid[(t.y as usize / 240).min(2)][(t.x as usize / 320).min(3)] += 1;
        }
    }
    grid
}

/// Erkennungs-Zeit je Textzahl-Bucket (10er-Buckets → Verteilung der rec-ms).
pub fn rec_by_texts(records: &[LogRecord]) -> BTreeMap<usize, Lat> {
    let mut buckets: BTreeMap<usize, Vec<f64>> = BTreeMap::new();
    for r in records {
        if let LogRecord::Frame { texts, ms, .. } = r {
            buckets
                .entry(texts / 10 * 10)
                .or_default()
                .push(f64::from(ms.rec));
        }
    }
    buckets.into_iter().map(|(b, v)| (b, Lat::of(v))).collect()
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
/// (TCP-Reihenfolge, Verluste nur am Verbindungsende). `offset_s` ist der
/// Wall-Offset Server−Client in Sekunden (vgl. `Summary::clock_offset_ms`).
pub fn delivery_delays(srv_segs: &[Vec<u64>], cli_segs: &[Vec<u64>], offset_s: f64) -> Vec<f64> {
    srv_segs
        .iter()
        .zip(cli_segs.iter())
        .flat_map(|(s, c)| {
            s.iter()
                .zip(c.iter())
                .map(|(sw, cw)| (*cw as f64 - *sw as f64) / 1e6 + offset_s)
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

/// Eingaben, die bei getrennter Verbindung ins Leere (in den Kanal) gingen —
/// werden beim Reconnect stale geflusht.
pub fn gap_inputs(records: &[LogRecord]) -> usize {
    let mut connected = false;
    let mut n = 0;
    for r in records {
        match r {
            LogRecord::Gap { event, .. } => connected = matches!(event, GapEvent::Up { .. }),
            LogRecord::Msg {
                dir: Dir::CliToSrv,
                kind,
                ..
            } if !matches!(kind, MsgKind::CliHello) && !connected => n += 1,
            _ => {}
        }
    }
    n
}

/// Maus-Aktivität: (aktive Minuten, Peak Moves/Minute).
pub fn mouse_stats(records: &[LogRecord]) -> (usize, usize) {
    let w0 = match records
        .iter()
        .filter_map(|r| stamp_of(r).map(|(w, _)| w))
        .min()
    {
        Some(w) => w,
        None => return (0, 0),
    };
    let mut per_min: BTreeMap<u64, usize> = BTreeMap::new();
    for r in records {
        if let LogRecord::Msg {
            stamp,
            kind: MsgKind::MouseMove,
            ..
        } = r
        {
            *per_min
                .entry((stamp.wall_us - w0) / 60_000_000)
                .or_default() += 1;
        }
    }
    (per_min.len(), per_min.values().max().copied().unwrap_or(0))
}

/// Wählt Server- und Client-Datei (per App-Namen) für Kreuz-Analysen.
pub fn pick_srv_cli(files: &[FileLog]) -> (Option<&FileLog>, Option<&FileLog>) {
    let srv = files.iter().find(|f| app_of(&f.records).contains("server"));
    let cli = files.iter().find(|f| app_of(&f.records).contains("client"));
    (srv, cli)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::record::{FrameMs, Stamp, TileStat};

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
            body: vec![1, 2, 3],
        }
    }

    fn up(w: u64, peer: &str, down_ms: Option<f32>) -> LogRecord {
        LogRecord::Gap {
            stamp: stamp(w, w),
            event: GapEvent::Up {
                peer: peer.into(),
                down_ms,
            },
        }
    }

    fn down(w: u64, reason: &str) -> LogRecord {
        LogRecord::Gap {
            stamp: stamp(w, w),
            event: GapEvent::Down {
                reason: reason.into(),
            },
        }
    }

    #[test]
    fn conns_split_and_count_per_direction() {
        let recs = vec![
            up(1_000_000, "a", None),
            msg(1_100_000, 0, Dir::SrvToCli, MsgKind::Tile, 100, 1),
            msg(1_200_000, 0, Dir::CliToSrv, MsgKind::Text, 20, 2),
            down(1_300_000, "eof"),
            up(2_000_000, "a", Some(700.0)),
            msg(2_100_000, 0, Dir::SrvToCli, MsgKind::Tile, 50, 3),
        ];
        let c = conn_split(&recs);
        assert_eq!(c.len(), 2);
        assert_eq!((c[0].srv_msgs, c[0].srv_bytes, c[0].cli_msgs), (1, 100, 1));
        assert_eq!(c[0].end, "eof");
        assert_eq!((c[1].srv_msgs, c[1].cli_msgs), (1, 0));
        assert_eq!(c[1].end, "(dateiende)");
        let g = gap_timeline(&recs);
        assert_eq!(g.len(), 3);
        assert_eq!((g[0].t_rel_s, g[0].up), (0.0, true));
        assert_eq!((g[2].t_rel_s, g[2].down_ms), (1.0, Some(700.0)));
    }

    #[test]
    fn totals_and_churn_and_top() {
        let recs = vec![
            msg(1, 1, Dir::SrvToCli, MsgKind::ClearText, 5, 0),
            msg(2, 2, Dir::SrvToCli, MsgKind::AddText, 10, 7),
            msg(3, 3, Dir::SrvToCli, MsgKind::AddText, 10, 7),
            msg(4, 4, Dir::SrvToCli, MsgKind::ClearText, 5, 0),
            msg(5, 5, Dir::SrvToCli, MsgKind::AddText, 10, 7),
            msg(6, 6, Dir::SrvToCli, MsgKind::AddText, 12, 8),
        ];
        let refs: Vec<&LogRecord> = recs.iter().collect();
        let tagged: Vec<(&LogRecord, u16)> = refs
            .iter()
            .map(|r| (*r, crate::record::LOG_VERSION))
            .collect();
        let t = dedup_totals(&tagged, MsgKind::AddText);
        assert_eq!((t.total_n, t.uniq_n), (4, 2));
        assert_eq!((t.total_b, t.uniq_b, t.wasted), (12, 6, 6));
        assert_eq!((t.hist_1, t.hist_2_10), (1, 1));
        let (clears, lat) = adds_per_clear(&recs);
        assert_eq!(clears, 2);
        assert_eq!((lat.n, lat.avg), (2, 2.0));
        // Bodies sind kein valides AddText → Sample leer, Zählung stimmt.
        assert_eq!(top_texts(&tagged, 5), vec![(3, "".into()), (1, "".into())]);
    }

    #[test]
    fn idle_tiles_and_rec_buckets() {
        let frame = |w, texts, rec_ms, tile: Option<TileStat>| LogRecord::Frame {
            stamp: stamp(w, w),
            frame: 0,
            texts,
            text_changed: false,
            text_bytes: 0,
            tile,
            ms: FrameMs {
                rec: rec_ms,
                ..Default::default()
            },
        };
        let tile = |x, y| TileStat {
            x,
            y,
            w: 16,
            h: 16,
            bytes: 100,
            hash: 1,
        };
        let recs = vec![
            msg(1_000_000, 0, Dir::SrvToCli, MsgKind::Tile, 10, 1),
            msg(9_000_000, 0, Dir::SrvToCli, MsgKind::Tile, 10, 2),
            frame(1, 5, 30.0, Some(tile(0, 0))),
            frame(2, 25, 150.0, Some(tile(1000, 700))),
            frame(3, 25, 170.0, None),
        ];
        assert_eq!(idle_stats(&recs), (8.0, 1));
        let sizes = tile_sizes(&recs);
        assert_eq!((sizes.n, sizes.avg), (2, 100.0));
        let grid = tile_raster(&recs);
        assert_eq!((grid[0][0], grid[2][3]), (1, 1));
        let buckets = rec_by_texts(&recs);
        assert_eq!(buckets.len(), 2);
        assert_eq!(buckets[&0].avg, 30.0);
        assert_eq!(buckets[&20].n, 2);
    }

    #[test]
    fn delivery_pairs_by_position_per_segment() {
        // Server 10 s vor (Offset), eine Nachricht verloren am Schluss.
        let srv = vec![vec![100_000_000, 101_000_000, 102_000_000]];
        let cli = vec![vec![90_100_000, 91_100_000]];
        let d = delivery_delays(&srv, &cli, 10.0);
        assert_eq!(d.len(), 2);
        assert!((d[0] - 0.1).abs() < 1e-9);
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

    #[test]
    fn gap_inputs_and_mouse() {
        let recs = vec![
            up(1_000_000, "p", None),
            msg(1_100_000, 0, Dir::CliToSrv, MsgKind::Text, 10, 1),
            down(1_200_000, "eof"),
            msg(1_300_000, 0, Dir::CliToSrv, MsgKind::Text, 10, 2),
            msg(61_300_000, 0, Dir::CliToSrv, MsgKind::MouseMove, 10, 3),
            msg(61_400_000, 0, Dir::CliToSrv, MsgKind::MouseMove, 10, 3),
        ];
        assert_eq!(gap_inputs(&recs), 3);
        assert_eq!(mouse_stats(&recs), (1, 2));
        assert_eq!(gap_inputs(&[]), 0);
    }
}
