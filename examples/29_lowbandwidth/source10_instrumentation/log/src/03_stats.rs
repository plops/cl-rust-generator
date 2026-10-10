//! `03_stats` — Offline-Auswertung von `.lbwlog`-Aufzeichnungen.
//!
//! Reine Funktionen über Records: Durchsatz, Gaps, Dedup, Latenzen,
//! Tile-Bewegung und Uhr-Offset-Schätzung. Wichtig: Server- und Client-Log
//! beschreiben *dieselben* Bytes — Summen werden daher **pro Datei**
//! berichtet (kein Doppelzählen), nur Einzel-Ursprungs-Daten (Frames, Decode,
//! Inject) sowie die Kreuz-Uhr-Schätzung werden zusammengeführt.

use std::collections::{BTreeMap, HashMap};

use lbw_common::ServerMsg;
use lbw_common::framing::decode_msg;

use crate::record::{Dir, GapEvent, LogRecord, MsgKind};

/// Kanal-Budget in Byte/s (stockende 6-kB/s-Strecke).
pub const BUDGET_BPS: u64 = 6000;

/// Eine geladene Logdatei.
pub struct FileLog {
    pub name: String,
    pub records: Vec<LogRecord>,
}

/// Latenz-Verteilung (ms).
#[derive(Clone, Copy, Debug, Default)]
pub struct Lat {
    pub n: usize,
    pub avg: f64,
    pub p50: f64,
    pub max: f64,
}

impl Lat {
    fn of(mut v: Vec<f64>) -> Self {
        if v.is_empty() {
            return Self::default();
        }
        v.sort_by(|a, b| a.total_cmp(b));
        let n = v.len();
        Self {
            n,
            avg: v.iter().sum::<f64>() / n as f64,
            p50: v[n / 2],
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
    pub gaps: Vec<GapInfo>,
    pub down_total_ms: f64,
    /// Client-Sicht: Eingabe-Sendung → nächste sichtbare Änderung.
    pub input_visible_ms: Lat,
    /// Server-Sicht: Eingabe-Empfang → nächster Frame mit Änderung.
    pub input_frame_ms: Lat,
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

fn is_input(m: &LogRecord) -> bool {
    matches!(
        m,
        LogRecord::Msg {
            dir: Dir::CliToSrv,
            kind,
            ..
        } if !matches!(kind, MsgKind::CliHello)
    )
}

fn is_visible(m: &LogRecord) -> bool {
    matches!(
        m,
        LogRecord::Msg {
            dir: Dir::SrvToCli,
            kind: MsgKind::Tile | MsgKind::ClearText | MsgKind::AddText,
            ..
        }
    )
}

fn stamp_of(m: &LogRecord) -> Option<(u64, u64)> {
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

fn app_of(records: &[LogRecord]) -> String {
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
    let mut frm_samples = Vec::new();
    let mut pending_vis: Option<u64> = None;
    let mut pending_frm: Option<u64> = None;
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
                    pending_vis = Some(stamp.mono_us);
                    pending_frm = Some(stamp.mono_us);
                } else if is_visible(r)
                    && let Some(t0) = pending_vis.take()
                {
                    vis_samples.push((stamp.mono_us.saturating_sub(t0)) as f64 / 1000.0);
                }
            }
            LogRecord::Frame {
                stamp,
                tile,
                text_changed,
                ..
            } => {
                if (tile.is_some() || *text_changed)
                    && let Some(t0) = pending_frm.take()
                {
                    frm_samples.push((stamp.mono_us.saturating_sub(t0)) as f64 / 1000.0);
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
        gaps,
        down_total_ms,
        input_visible_ms: Lat::of(vis_samples),
        input_frame_ms: Lat::of(frm_samples),
    }
}

fn msg_kind_of(k: u8) -> MsgKind {
    const ALL: [MsgKind; 9] = [
        MsgKind::SrvHello,
        MsgKind::ClearText,
        MsgKind::AddText,
        MsgKind::Tile,
        MsgKind::CliHello,
        MsgKind::MouseMove,
        MsgKind::Button,
        MsgKind::Text,
        MsgKind::Key,
    ];
    ALL.iter()
        .find(|m| **m as u8 == k)
        .copied()
        .unwrap_or(MsgKind::SrvHello)
}

fn kind_name(k: MsgKind) -> &'static str {
    match k {
        MsgKind::SrvHello => "SrvHello",
        MsgKind::ClearText => "ClearText",
        MsgKind::AddText => "AddText",
        MsgKind::Tile => "Tile",
        MsgKind::CliHello => "CliHello",
        MsgKind::MouseMove => "MouseMove",
        MsgKind::Button => "Button",
        MsgKind::Text => "Text",
        MsgKind::Key => "Key",
    }
}

/// Wählt den Referenzstrom für die Dedup-Analyse: SrvToCli-Nachrichten aus
/// Server-Dateien (das tatsächlich Gesendete); ohne Server-Datei alle.
fn dedup_stream(files: &[FileLog]) -> Vec<&LogRecord> {
    let has_server = files.iter().any(|f| app_of(&f.records).contains("server"));
    files
        .iter()
        .filter(|f| !has_server || app_of(&f.records).contains("server"))
        .flat_map(|f| &f.records)
        .filter(|r| {
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

fn sample_text(body: &[u8]) -> String {
    let s = decode_msg::<ServerMsg>(body)
        .ok()
        .and_then(|m| match m {
            ServerMsg::AddText(t) => Some(t.text),
            ServerMsg::Tile { data, x, y } => Some(format!("tile@{x},{y} {}B", data.len())),
            _ => None,
        })
        .unwrap_or_default();
    s.chars().take(40).collect::<String>().replace('\n', "␊")
}

fn dedup(records: &[&LogRecord], kind: MsgKind) -> Vec<DedupEntry> {
    let mut map: HashMap<u64, (usize, usize, String)> = HashMap::new();
    for r in records {
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
                .or_insert_with(|| (0, body.len(), sample_text(body)));
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

fn esc(s: &str) -> String {
    let mut o = String::with_capacity(s.len() + 2);
    o.push('"');
    for c in s.chars() {
        match c {
            '"' => o.push_str("\\\""),
            '\\' => o.push_str("\\\\"),
            '\n' => o.push_str("\\n"),
            '\r' => o.push_str("\\r"),
            '\t' => o.push_str("\\t"),
            c if (c as u32) < 0x20 => o.push_str(&format!("\\u{:04x}", c as u32)),
            c => o.push(c),
        }
    }
    o.push('"');
    o
}

fn lat_json(l: &Lat) -> String {
    format!(
        "{{\"n\":{},\"avg\":{:.3},\"p50\":{:.3},\"max\":{:.3}}}",
        l.n, l.avg, l.p50, l.max
    )
}

/// Kompakte JSON-Darstellung (Hand-Code — flache Summary, kein Dep wert).
pub fn to_json(s: &Summary) -> String {
    let mut o = String::from("{\"files\":[");
    for (i, f) in s.files.iter().enumerate() {
        if i > 0 {
            o.push(',');
        }
        o.push_str(&format!(
            "{{\"name\":{},\"app\":{},\"records\":{},\"wall_secs\":{:.3},\"mono_secs\":{:.3},\"has_end\":{},\"msgs\":{},\"bytes_srv\":{},\"bytes_cli\":{},\"over_budget_secs\":{},\"down_total_ms\":{:.1},\"input_visible_ms\":{},\"input_frame_ms\":{},\"kinds\":[",
            esc(&f.name),
            esc(&f.app),
            f.records,
            f.wall_secs,
            f.mono_secs,
            f.has_end,
            f.msgs,
            f.bytes_srv,
            f.bytes_cli,
            f.over_budget_secs,
            f.down_total_ms,
            lat_json(&f.input_visible_ms),
            lat_json(&f.input_frame_ms),
        ));
        for (j, k) in f.kinds.iter().enumerate() {
            if j > 0 {
                o.push(',');
            }
            o.push_str(&format!(
                "{{\"dir\":\"{:?}\",\"kind\":\"{}\",\"count\":{},\"bytes\":{}}}",
                k.dir,
                kind_name(k.kind),
                k.count,
                k.bytes
            ));
        }
        o.push_str("],\"timeline\":[");
        for (j, t) in f.timeline.iter().enumerate() {
            if j > 0 {
                o.push(',');
            }
            o.push_str(&format!(
                "{{\"sec\":{},\"srv\":{},\"cli\":{}}}",
                t.sec, t.srv, t.cli
            ));
        }
        o.push_str("],\"gaps\":[");
        for (j, g) in f.gaps.iter().enumerate() {
            if j > 0 {
                o.push(',');
            }
            o.push_str(&format!(
                "{{\"peer\":{},\"reason\":{},\"down_ms\":{}}}",
                esc(&g.peer),
                esc(&g.reason),
                g.down_ms
                    .map(|d| format!("{d:.1}"))
                    .unwrap_or("null".into())
            ));
        }
        o.push_str("]}");
    }
    o.push_str("],\"dedup_text\":[");
    for (i, d) in s.dedup_text.iter().enumerate() {
        if i > 0 {
            o.push(',');
        }
        o.push_str(&format!(
            "{{\"hash\":{},\"count\":{},\"bytes\":{},\"wasted\":{},\"sample\":{}}}",
            d.hash,
            d.count,
            d.bytes,
            d.wasted,
            esc(&d.sample)
        ));
    }
    o.push_str("],\"dedup_tile\":[");
    for (i, d) in s.dedup_tile.iter().enumerate() {
        if i > 0 {
            o.push(',');
        }
        o.push_str(&format!(
            "{{\"hash\":{},\"count\":{},\"bytes\":{},\"wasted\":{},\"sample\":{}}}",
            d.hash,
            d.count,
            d.bytes,
            d.wasted,
            esc(&d.sample)
        ));
    }
    let f = &s.frames;
    o.push_str(&format!(
        "],\"frames\":{{\"n\":{},\"capture\":{},\"det\":{},\"rec\":{},\"mask_diff\":{},\"encode\":{},\"send\":{},\"total\":{},\"tiles\":{},\"tile_bytes\":{},\"text_changes\":{}}}",
        f.n,
        lat_json(&f.capture),
        lat_json(&f.det),
        lat_json(&f.rec),
        lat_json(&f.mask_diff),
        lat_json(&f.encode),
        lat_json(&f.send),
        lat_json(&f.total),
        f.tiles,
        f.tile_bytes,
        f.text_changes
    ));
    o.push_str(&format!(
        ",\"decode\":{},\"decode_failed\":{},\"inject\":{},\"inject_failed\":{}",
        lat_json(&s.decode),
        s.decode_failed,
        lat_json(&s.inject),
        s.inject_failed
    ));
    o.push_str(&format!(
        ",\"motion\":{{\"pairs\":{},\"same_rect\":{},\"avg_abs_dx\":{:.1},\"avg_abs_dy\":{:.1}}}",
        s.motion.pairs, s.motion.same_rect, s.motion.avg_abs_dx, s.motion.avg_abs_dy
    ));
    o.push_str(&format!(
        ",\"clock_offset_ms\":{},\"clock_offset_samples\":{}}}",
        s.clock_offset_ms
            .map(|d| format!("{d:.1}"))
            .unwrap_or("null".into()),
        s.clock_offset_samples
    ));
    o
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::record::{FrameMs, Stamp, TileStat, fnv1a64};
    use lbw_common::framing::encode_msg;

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

    fn add_text_body(text: &str) -> Vec<u8> {
        use lbw_common::{Rect, TextItem};
        encode_msg(&ServerMsg::AddText(TextItem {
            rect: Rect::new(0, 0, 10, 10),
            fg: [0; 3],
            bg: [255; 3],
            text: text.into(),
        }))
        .unwrap()
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
        let files = vec![FileLog {
            name: "s".into(),
            records: vec![session("lbw-server"), m(1), m(2), m(3)],
        }];
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
            FileLog {
                name: "c".into(),
                records: vec![
                    session("lbw-client"),
                    input(1_000_000, 1),
                    input(2_000_000, 2),
                ],
            },
            FileLog {
                name: "s".into(),
                records: vec![
                    session("lbw-server"),
                    input(1_050_000, 1),
                    input(2_200_000, 2),
                ],
            },
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
        let files = vec![FileLog {
            name: "s".into(),
            records: vec![
                session("lbw-server"),
                frame(1, Some(tile(0))),
                frame(2, Some(tile(10))),
                frame(3, None),
            ],
        }];
        let s = summarize(&files);
        assert_eq!(s.frames.n, 3);
        assert_eq!(s.frames.total.p50, 63.0);
        assert_eq!((s.frames.tiles, s.frames.tile_bytes), (2, 200));
        assert_eq!(s.motion.pairs, 1);
        assert_eq!(s.motion.same_rect, 0);
        assert_eq!((s.motion.avg_abs_dx, s.motion.avg_abs_dy), (10.0, 0.0));
    }

    #[test]
    fn json_is_wellformed_for_empty_summary() {
        let s = summarize(&[FileLog {
            name: "x".into(),
            records: vec![session("?")],
        }]);
        let j = to_json(&s);
        assert!(j.starts_with("{\"files\":[") && j.ends_with("}"), "{j}");
        assert!(j.contains("\"clock_offset_ms\":null"));
    }
}
