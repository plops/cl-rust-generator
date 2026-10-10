//! `04_summary` — Datei-übergreifende Zusammenführung.
//!
//! Baut aus mehreren [`FileLog`](crate::stats::FileLog)s die Gesamt-`Summary`:
//! Dedup über den Referenzstrom, Pipeline-, Decode- und Inject-Statistiken
//! (jeweils Einzel-Ursprung, kein Doppelzählen) sowie die Uhr-Offset-Schätzung.

use std::collections::{BTreeMap, HashMap};

use lbw_common::ServerMsg;
use lbw_common::framing::decode_server_logged;

use crate::record::{Dir, LogRecord, MsgKind};
use crate::stats::{FileLog, FileSummary, Lat, app_of, summarize_file};

/// Wiederholt übertragener Inhalt (Dedup-Kandidat).
#[derive(Clone, Debug)]
pub struct DedupEntry {
    pub hash: u64,
    pub count: usize,
    pub bytes: usize,
    pub wasted: u64,
    pub sample: String,
}

/// Server-Pipeline über alle Frames (ms-Verteilungen + Zähler).
#[derive(Clone, Debug, Default)]
pub struct FrameStats {
    pub n: usize,
    pub capture: Lat,
    pub det: Lat,
    pub rec: Lat,
    pub mask_diff: Lat,
    pub encode: Lat,
    pub send: Lat,
    pub total: Lat,
    pub tiles: usize,
    pub tile_bytes: u64,
    pub text_changes: usize,
}

/// Kachel-Bewegung aufeinanderfolgender Frames (Motion-Comp-Hinweis).
#[derive(Clone, Debug, Default)]
pub struct Motion {
    pub pairs: usize,
    pub same_rect: usize,
    pub avg_abs_dx: f64,
    pub avg_abs_dy: f64,
}

/// Gesamtauswertung über alle Dateien.
#[derive(Clone, Debug)]
pub struct Summary {
    pub files: Vec<FileSummary>,
    pub dedup_text: Vec<DedupEntry>,
    pub dedup_tile: Vec<DedupEntry>,
    pub frames: FrameStats,
    pub decode: Lat,
    pub decode_failed: usize,
    pub inject: Lat,
    pub inject_failed: usize,
    pub motion: Motion,
    /// Geschätzter Wall-Offset Server−Client in ms (Min-Filter, NTP-Prinzip).
    pub clock_offset_ms: Option<f64>,
    pub clock_offset_samples: usize,
}

/// Wählt den Referenzstrom für die Dedup-Analyse: SrvToCli-Nachrichten aus
/// Server-Dateien (das tatsächlich Gesendete); ohne Server-Datei alle.
/// Jeder Eintrag trägt die Log-Version seiner Datei (Body-Dekodierung).
pub fn dedup_stream(files: &[FileLog]) -> Vec<(&LogRecord, u16)> {
    let has_server = files.iter().any(|f| app_of(&f.records).contains("server"));
    files
        .iter()
        .filter(|f| !has_server || app_of(&f.records).contains("server"))
        .flat_map(|f| f.records.iter().map(|r| (r, f.version)))
        .filter(|(r, _)| {
            matches!(
                r,
                LogRecord::Msg {
                    dir: Dir::SrvToCli,
                    ..
                }
            )
        })
        .collect()
}

fn sample_text(body: &[u8], version: u16) -> String {
    let s = decode_server_logged(body, version)
        .ok()
        .and_then(|m| match m {
            ServerMsg::AddText(t) => Some(t.text),
            ServerMsg::Tile { data, x, y } => Some(format!("tile@{x},{y} {}B", data.len())),
            _ => None,
        })
        .unwrap_or_default();
    s.chars().take(40).collect::<String>().replace('\n', "␊")
}

fn dedup(records: &[(&LogRecord, u16)], kind: MsgKind) -> Vec<DedupEntry> {
    let mut map: HashMap<u64, (usize, usize, String)> = HashMap::new();
    for (r, version) in records {
        if let LogRecord::Msg {
            kind: k,
            hash,
            body,
            ..
        } = r
            && *k == kind
        {
            let e = map
                .entry(*hash)
                .or_insert_with(|| (0, body.len(), sample_text(body, *version)));
            e.0 += 1;
        }
    }
    let mut out: Vec<DedupEntry> = map
        .into_iter()
        .filter(|(_, (n, _, _))| *n > 1)
        .map(|(hash, (count, bytes, sample))| DedupEntry {
            hash,
            count,
            bytes,
            wasted: (count.saturating_sub(1) * bytes) as u64,
            sample,
        })
        .collect();
    out.sort_by_key(|d| std::cmp::Reverse(d.wasted));
    out.truncate(5);
    out
}

fn frame_stats(records: &[&LogRecord]) -> FrameStats {
    let mut st = FrameStats::default();
    let (mut cap, mut det, mut rec, mut md, mut enc, mut snd, mut tot) =
        (vec![], vec![], vec![], vec![], vec![], vec![], vec![]);
    for r in records {
        if let LogRecord::Frame {
            tile,
            text_changed,
            ms,
            ..
        } = r
        {
            st.n += 1;
            cap.push(f64::from(ms.capture));
            det.push(f64::from(ms.det));
            rec.push(f64::from(ms.rec));
            md.push(f64::from(ms.mask_diff));
            enc.push(f64::from(ms.encode));
            snd.push(f64::from(ms.send));
            tot.push(f64::from(ms.total()));
            if *text_changed {
                st.text_changes += 1;
            }
            if let Some(t) = tile {
                st.tiles += 1;
                st.tile_bytes += t.bytes as u64;
            }
        }
    }
    st.capture = Lat::of(cap);
    st.det = Lat::of(det);
    st.rec = Lat::of(rec);
    st.mask_diff = Lat::of(md);
    st.encode = Lat::of(enc);
    st.send = Lat::of(snd);
    st.total = Lat::of(tot);
    st
}

/// Baut die Gesamtauswertung (Zusammenführung s. Modul-Doku).
pub fn summarize(files: &[FileLog]) -> Summary {
    let file_sums: Vec<FileSummary> = files
        .iter()
        .map(|f| summarize_file(&f.name, &f.records))
        .collect();
    let all: Vec<&LogRecord> = files.iter().flat_map(|f| &f.records).collect();
    let stream = dedup_stream(files);
    let frames = frame_stats(&all);
    let motion = motion_of(&all);
    let mut dec = Vec::new();
    let mut dec_fail = 0;
    let mut inj = Vec::new();
    let mut inj_fail = 0;
    for r in &all {
        match r {
            LogRecord::Decode { ms, ok, .. } => {
                if *ok {
                    dec.push(f64::from(*ms));
                } else {
                    dec_fail += 1;
                }
            }
            LogRecord::Inject { ms, ok, .. } => {
                if *ok {
                    inj.push(f64::from(*ms));
                } else {
                    inj_fail += 1;
                }
            }
            _ => {}
        }
    }
    let (clock_offset_ms, clock_offset_samples) = clock_offset(files);
    Summary {
        files: file_sums,
        dedup_text: dedup(&stream, MsgKind::AddText),
        dedup_tile: dedup(&stream, MsgKind::Tile),
        frames,
        decode: Lat::of(dec),
        decode_failed: dec_fail,
        inject: Lat::of(inj),
        inject_failed: inj_fail,
        motion,
        clock_offset_ms,
        clock_offset_samples,
    }
}

fn motion_of(records: &[&LogRecord]) -> Motion {
    let mut motion = Motion::default();
    let mut prev: Option<(u16, u16, u16, u16)> = None;
    for r in records {
        if let LogRecord::Frame { tile: Some(t), .. } = r {
            let rect = (t.x, t.y, t.w, t.h);
            if let Some((px, py, pw, ph)) = prev {
                motion.pairs += 1;
                if rect == (px, py, pw, ph) {
                    motion.same_rect += 1;
                }
                let (cx, cy) = (
                    f64::from(t.x) + f64::from(t.w) / 2.0,
                    f64::from(t.y) + f64::from(t.h) / 2.0,
                );
                let (ox, oy) = (
                    f64::from(px) + f64::from(pw) / 2.0,
                    f64::from(py) + f64::from(ph) / 2.0,
                );
                motion.avg_abs_dx += (cx - ox).abs();
                motion.avg_abs_dy += (cy - oy).abs();
            }
            prev = Some(rect);
        }
    }
    if motion.pairs > 0 {
        motion.avg_abs_dx /= motion.pairs as f64;
        motion.avg_abs_dy /= motion.pairs as f64;
    }
    motion
}

/// Wall-Offset Server−Client: gleiche Eingaben (Art+Hash, Auftretens-Reihenfolge)
/// auf beiden Seiten matchen, Minimum der Differenzen (schnellstes Paket ≈
/// reine Uhrdifferenz). `None` ohne passendes Datei-Paar.
fn clock_offset(files: &[FileLog]) -> (Option<f64>, usize) {
    let mut cli: BTreeMap<(u8, u64), Vec<u64>> = BTreeMap::new();
    let mut srv: BTreeMap<(u8, u64), Vec<u64>> = BTreeMap::new();
    for f in files {
        let app = app_of(&f.records);
        let side = if app.contains("client") {
            &mut cli
        } else if app.contains("server") {
            &mut srv
        } else {
            continue;
        };
        let mut walls: Vec<((u8, u64), u64)> = f
            .records
            .iter()
            .filter_map(|r| match r {
                LogRecord::Msg {
                    stamp,
                    dir: Dir::CliToSrv,
                    kind,
                    hash,
                    ..
                } if !matches!(kind, MsgKind::CliHello) => {
                    Some(((*kind as u8, *hash), stamp.wall_us))
                }
                _ => None,
            })
            .collect();
        walls.sort_by_key(|(_, w)| *w);
        for (k, w) in walls {
            side.entry(k).or_default().push(w);
        }
    }
    let mut diffs = Vec::new();
    for (k, c) in &cli {
        if let Some(s) = srv.get(k) {
            for (a, b) in c.iter().zip(s.iter()) {
                diffs.push((*b as f64 - *a as f64) / 1000.0);
            }
        }
    }
    let n = diffs.len();
    let min = diffs.into_iter().reduce(f64::min);
    (min, n)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::record::{FrameMs, Stamp, TileStat, fnv1a64};
    use lbw_common::framing::encode_msg;

    fn stamp(wall_us: u64, mono_us: u64) -> Stamp {
        Stamp { wall_us, mono_us }
    }

    fn session(app: &str) -> LogRecord {
        LogRecord::Session {
            app: app.into(),
            version: 1,
            args: vec![],
        }
    }

    fn add_text_body(text: &str) -> Vec<u8> {
        use lbw_common::{Rect, TextItem};
        encode_msg(&ServerMsg::AddText(TextItem {
            id: 1,
            rect: Rect::new(0, 0, 10, 10),
            fg: [0; 3],
            bg: [255; 3],
            text: text.into(),
        }))
        .unwrap()
    }

    fn file(name: &str, records: Vec<LogRecord>) -> FileLog {
        FileLog {
            name: name.into(),
            records,
            version: crate::record::LOG_VERSION,
        }
    }

    #[test]
    fn dedup_finds_repeated_texts() {
        let body = add_text_body("Hallo");
        let h = fnv1a64(&body);
        let m = |i| LogRecord::Msg {
            stamp: stamp(1_000_000 + i, i),
            dir: Dir::SrvToCli,
            kind: MsgKind::AddText,
            wire_bytes: 4 + body.len(),
            hash: h,
            body: body.clone(),
        };
        let files = vec![file("s", vec![session("lbw-server"), m(1), m(2), m(3)])];
        let s = summarize(&files);
        assert_eq!(s.dedup_text.len(), 1);
        assert_eq!(
            (s.dedup_text[0].count, s.dedup_text[0].bytes),
            (3, body.len())
        );
        assert_eq!(s.dedup_text[0].wasted, (2 * body.len()) as u64);
        assert_eq!(s.dedup_text[0].sample, "Hallo");
    }

    #[test]
    fn clock_offset_is_min_over_matched_inputs() {
        let input = |w, m| LogRecord::Msg {
            stamp: stamp(w, m),
            dir: Dir::CliToSrv,
            kind: MsgKind::Text,
            wire_bytes: 20,
            hash: 42,
            body: vec![],
        };
        let files = vec![
            file(
                "c",
                vec![
                    session("lbw-client"),
                    input(1_000_000, 1),
                    input(2_000_000, 2),
                ],
            ),
            file(
                "s",
                vec![
                    session("lbw-server"),
                    input(1_050_000, 1),
                    input(2_200_000, 2),
                ],
            ),
        ];
        let s = summarize(&files);
        // Differenzen 50 ms und 200 ms → Min 50 ms, 2 Samples.
        assert_eq!(s.clock_offset_samples, 2);
        assert_eq!(s.clock_offset_ms, Some(50.0));
    }

    #[test]
    fn frames_and_motion_aggregate() {
        let tile = |x| TileStat {
            x,
            y: 0,
            w: 16,
            h: 16,
            bytes: 100,
            hash: 1,
        };
        let frame = |i, t: Option<TileStat>| LogRecord::Frame {
            stamp: stamp(1_000_000 + i, i),
            frame: i,
            texts: 0,
            text_changed: false,
            text_bytes: 0,
            tile: t,
            ms: FrameMs {
                capture: 1.0,
                det: 2.0,
                rec: 4.0,
                mask_diff: 8.0,
                encode: 16.0,
                send: 32.0,
            },
        };
        let files = vec![file(
            "s",
            vec![
                session("lbw-server"),
                frame(1, Some(tile(0))),
                frame(2, Some(tile(10))),
                frame(3, None),
            ],
        )];
        let s = summarize(&files);
        assert_eq!(s.frames.n, 3);
        assert_eq!(s.frames.total.p50, 63.0);
        assert_eq!((s.frames.tiles, s.frames.tile_bytes), (2, 200));
        assert_eq!(s.motion.pairs, 1);
        assert_eq!(s.motion.same_rect, 0);
        assert_eq!((s.motion.avg_abs_dx, s.motion.avg_abs_dy), (10.0, 0.0));
    }
}
