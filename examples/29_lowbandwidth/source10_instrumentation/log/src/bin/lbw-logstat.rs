//! `lbw-logstat` — Offline-Auswertung von `.lbwlog`-Aufzeichnungen.
//!
//! Liest Server- und/oder Client-Logs, druckt Durchsatz, Gaps, Dedup und
//! Latenzen (Human-Text oder `--json`, Timeline optional als `--csv`).
//! Mit `--deep` folgen Tiefenanalyse-Abschnitte (Verbindungs-Zeitleiste,
//! Dedup-Totals, Idle-Phasen, Zustell-Verzögerung).
//! Zerrissene Dateischlüsse (Absturz) werden als Teilstand mit Warnung
//! ausgewertet, nicht als harter Fehler.

use clap::Parser;
use lbw_log::deep;
use lbw_log::delivery;
use lbw_log::export::to_json;
use lbw_log::io::load_lenient;
use lbw_log::record::MsgKind;
use lbw_log::stats::{BUDGET_BPS, FileLog, Lat, log_version};
use lbw_log::summary::{Summary, dedup_stream, summarize};

#[derive(Parser)]
#[command(
    name = "lbw-logstat",
    version,
    about = "Wertet .lbwlog-Aufzeichnungen aus"
)]
struct Args {
    /// Logdateien (Server- und/oder Client-Aufzeichnungen).
    files: Vec<String>,
    /// JSON statt Human-Text ausgeben.
    #[arg(long)]
    json: bool,
    /// Durchsatz-Timeline zusätzlich als CSV in diese Datei schreiben.
    #[arg(long)]
    csv: Option<String>,
    /// Tiefenanalyse anhängen (nur Human-Text).
    #[arg(long)]
    deep: bool,
}

fn lat(l: &Lat) -> String {
    if l.n == 0 {
        return "—".into();
    }
    format!(
        "n={} avg={:.1} p50={:.1} p90={:.1} p99={:.1} max={:.1} ms",
        l.n, l.avg, l.p50, l.p90, l.p99, l.max
    )
}

fn render(s: &Summary) -> String {
    let mut o = String::new();
    for f in &s.files {
        o.push_str(&format!(
            "== {} ({}, {} Records, {:.1}s wall{}) ==\n",
            f.name,
            f.app,
            f.records,
            f.wall_secs,
            if f.has_end { "" } else { ", KEIN End" }
        ));
        o.push_str(&format!(
            "Nachrichten: {} (Server→Client {} B, Client→Server {} B)\n",
            f.msgs, f.bytes_srv, f.bytes_cli
        ));
        for k in &f.kinds {
            o.push_str(&format!(
                "  {:?}/{:?}: {}x {} B\n",
                k.dir, k.kind, k.count, k.bytes
            ));
        }
        o.push_str(&format!(
            "Durchsatz: {} s mit Daten, {} s über Budget ({} B/s)\n",
            f.timeline.len(),
            f.over_budget_secs,
            BUDGET_BPS
        ));
        let base = f.timeline.first().map(|t| t.sec).unwrap_or(0);
        for t in f.timeline.iter().take(10) {
            o.push_str(&format!(
                "  s+{}: {} B/s srv, {} B/s cli\n",
                t.sec.saturating_sub(base),
                t.srv,
                t.cli
            ));
        }
        if f.timeline.len() > 10 {
            o.push_str(&format!("  … ({} weitere)\n", f.timeline.len() - 10));
        }
        o.push_str(&format!(
            "Warteschlange ({} B/s): max {} B (≈ {:.0} ms)\n",
            BUDGET_BPS,
            f.queue_peak_b,
            f.queue_peak_b as f64 / BUDGET_BPS as f64 * 1000.0
        ));
        o.push_str(&format!(
            "Gaps: {} (Down gesamt {:.0} ms)\n",
            f.gaps.len(),
            f.down_total_ms
        ));
        for g in f.gaps.iter().take(10) {
            let state = match (g.down_ms, g.peer.is_empty()) {
                (Some(_), _) => "reconnect",
                (None, false) => "initial",
                (None, true) => "offen",
            };
            o.push_str(&format!(
                "  {} → {} ({} ms): {state}\n",
                g.reason,
                g.peer,
                g.down_ms
                    .map(|d| format!("{d:.0}"))
                    .unwrap_or_else(|| "?".into()),
            ));
        }
        o.push_str(&format!(
            "Input→sichtbar (Client-Uhr): {}\n",
            lat(&f.input_visible_ms)
        ));
        o.push_str(&format!(
            "Input→Text (Client-Uhr): {}\n",
            lat(&f.input_text_ms)
        ));
        o.push_str(&format!(
            "Input→Kachel (Client-Uhr): {}\n",
            lat(&f.input_tile_ms)
        ));
        o.push_str(&format!(
            "Input→Frame (Server-Uhr): {}\n",
            lat(&f.input_frame_ms)
        ));
    }
    o.push_str("== Dedup (Referenzstrom) ==\n");
    for d in &s.dedup_text {
        o.push_str(&format!(
            "  Text {}x {} B ({} B doppelt): {:?}\n",
            d.count, d.bytes, d.wasted, d.sample
        ));
    }
    for d in &s.dedup_tile {
        o.push_str(&format!(
            "  Tile {}x {} B ({} B doppelt): {}\n",
            d.count, d.bytes, d.wasted, d.sample
        ));
    }
    if s.dedup_text.is_empty() && s.dedup_tile.is_empty() {
        o.push_str("  keine Wiederholungen\n");
    }
    let fr = &s.frames;
    o.push_str(&format!(
        "== Frames: {} ({} Tiles, {} B, {} Textwechsel) ==\n",
        fr.n, fr.tiles, fr.tile_bytes, fr.text_changes
    ));
    for (name, l) in [
        ("capture", &fr.capture),
        ("det", &fr.det),
        ("rec", &fr.rec),
        ("mask+diff", &fr.mask_diff),
        ("encode", &fr.encode),
        ("send", &fr.send),
        ("gesamt", &fr.total),
    ] {
        o.push_str(&format!("  {name}: {}\n", lat(l)));
    }
    o.push_str(&format!("Decode (Client): {}\n", lat(&s.decode)));
    o.push_str(&format!("Inject (Server): {}\n", lat(&s.inject)));
    if s.decode_failed > 0 || s.inject_failed > 0 {
        o.push_str(&format!(
            "  FEHLER: {} Decode-, {} Inject-Fehlschläge\n",
            s.decode_failed, s.inject_failed
        ));
    }
    o.push_str(&format!(
        "Motion: {} Paare, {} gleiche Box, |dx|={:.1} |dy|={:.1} px\n",
        s.motion.pairs, s.motion.same_rect, s.motion.avg_abs_dx, s.motion.avg_abs_dy
    ));
    match s.clock_offsets.as_slice() {
        [] => o.push_str("Uhr-Offset: — (kein Client/Server-Paar mit gleichen Inputs)\n"),
        [w] => o.push_str(&format!(
            "Uhr-Offset Server−Client: {:.1} ms ({} Samples, Min-Filter)\n",
            w.ms, w.samples
        )),
        wins => {
            let n: usize = wins.iter().map(|w| w.samples).sum();
            o.push_str(&format!(
                "Uhr-Offset Server−Client: {:.1}→{:.1} ms ({} Fenster à 5 min, {} Samples, Min-Filter)\n",
                wins.first().map(|w| w.ms).unwrap_or(0.0),
                wins.last().map(|w| w.ms).unwrap_or(0.0),
                wins.len(),
                n
            ));
        }
    }
    o
}

fn render_deep(files: &[FileLog], s: &Summary) -> String {
    let mut o = String::from("== Tiefenanalyse ==\n");
    for (fi, f) in files.iter().enumerate() {
        let fs = &s.files[fi];
        o.push_str(&format!("-- {} --\n", f.name));
        for c in deep::conn_split(&f.records) {
            o.push_str(&format!(
                "  conn{} {}: SrvToCli {}x {} B, CliToSrv {}x {} B, Ende: {}\n",
                c.index, c.peer, c.srv_msgs, c.srv_bytes, c.cli_msgs, c.cli_bytes, c.end
            ));
        }
        let gaps = deep::gap_timeline(&f.records);
        o.push_str(&format!("  {} Gaps, erste:\n", gaps.len()));
        for g in gaps.iter().take(5) {
            o.push_str(&format!(
                "    t={:8.1}s {} {} {} {:?}\n",
                g.t_rel_s,
                if g.up { "Up  " } else { "Down" },
                g.peer,
                g.reason,
                g.down_ms
            ));
        }
        let top = deep::top_seconds(&fs.timeline, 5);
        let base = fs.timeline.first().map(|t| t.sec).unwrap_or(0);
        o.push_str(&format!("  Top-{} Sekunden:\n", top.len()));
        for t in &top {
            o.push_str(&format!(
                "    s+{}: {} B/s srv, {} B/s cli\n",
                t.sec.saturating_sub(base),
                t.srv,
                t.cli
            ));
        }
        let (longest, over5) = deep::idle_stats(&f.records);
        o.push_str(&format!(
            "  längste Srv-Stille: {longest:.1}s, Stillstände>5s: {over5}\n"
        ));
        let sizes = deep::tile_sizes(&f.records);
        o.push_str(&format!(
            "  Kachelgrößen: n={} avg={:.0} p50={:.0} p90={:.0} max={:.0} B\n",
            sizes.n, sizes.avg, sizes.p50, sizes.p90, sizes.max
        ));
        o.push_str("  Kachelraster (3x4):\n");
        for row in deep::tile_raster(&f.records) {
            o.push_str(&format!("    {row:?}\n"));
        }
        o.push_str("  rec-ms je Textzahl:\n");
        for (b, l) in deep::rec_by_texts(&f.records) {
            o.push_str(&format!(
                "    {b:3}-{:<3}: n={:4} avg={:6.1}ms\n",
                b + 9,
                l.n,
                l.avg
            ));
        }
        let (clears, adds) = deep::adds_per_clear(&f.records);
        o.push_str(&format!(
            "  ClearText: {clears}x, Adds/Clear: n={} avg={:.1} p50={:.0} max={:.0}\n",
            adds.n, adds.avg, adds.p50, adds.max
        ));
        o.push_str(&format!(
            "  stale Inputs (in Lücke): {}\n",
            deep::gap_inputs(&f.records)
        ));
        let (mins, peak) = deep::mouse_stats(&f.records);
        o.push_str(&format!("  Maus: {mins} aktive Minuten, Peak {peak}/min\n"));
    }
    // Referenzstrom-Totals + Top-Texte (Server-Datei bevorzugt).
    let stream = dedup_stream(files);
    for (name, kind) in [("Text", MsgKind::AddText), ("Tile", MsgKind::Tile)] {
        let t = deep::dedup_totals(&stream, kind);
        if t.total_n == 0 {
            continue;
        }
        o.push_str(&format!(
            "  Dedup-{name}-Total: {total}x ({total_b} B), {uniq} unique, verschwendet {w} B ({p:.1}%), hist 1x:{h1} 2-10x:{h10} 11-100x:{h100} >100x:{hbig}\n",
            total = t.total_n,
            total_b = t.total_b,
            uniq = t.uniq_n,
            w = t.wasted,
            p = 100.0 * t.wasted as f64 / t.total_b.max(1) as f64,
            h1 = t.hist_1,
            h10 = t.hist_2_10,
            h100 = t.hist_11_100,
            hbig = t.hist_100p,
        ));
    }
    o.push_str("  Top-Texte:\n");
    for (n, sample) in deep::top_texts(&stream, 8) {
        o.push_str(&format!("    {n:5}x {sample:?}\n"));
    }
    // Zustell-Verzögerung über Server/Client-Paar.
    match delivery::pick_srv_cli(files) {
        (Some(srv), Some(cli)) if !s.clock_offsets.is_empty() => {
            let d = delivery::delivery_delays(
                &delivery::srv_segments(&srv.records),
                &delivery::srv_segments(&cli.records),
                &s.clock_offsets,
            );
            if d.is_empty() {
                o.push_str("  Zustell-Verzögerung: — (keine Paare)\n");
            } else {
                let tenths = delivery::tenths(&d);
                let ts: Vec<String> = tenths.iter().map(|v| format!("{v:.1}")).collect();
                let mut tail = d[d.len().saturating_sub(500)..].to_vec();
                tail.sort_by(|a, b| a.total_cmp(b));
                o.push_str(&format!(
                    "  Zustell-Verzögerung (n={}, s): Zehntel-Mediane [{}], letzte 500: min={:.1} p50={:.1} max={:.1}\n",
                    d.len(),
                    ts.join(" "),
                    tail[0],
                    tail[tail.len() / 2],
                    tail[tail.len() - 1]
                ));
            }
        }
        _ => o.push_str("  Zustell-Verzögerung: — (braucht Server+Client-Datei mit Offset)\n"),
    }
    o
}

fn main() {
    let args = Args::parse();
    if args.files.is_empty() {
        eprintln!("lbw-logstat: keine Datei angegeben");
        std::process::exit(2);
    }
    let mut files = Vec::new();
    for p in &args.files {
        match load_lenient(p) {
            Ok((records, torn)) => {
                if torn {
                    eprintln!("{p}: WARNUNG — zerrissener Schluss, Teilstand");
                }
                files.push(FileLog {
                    name: p.clone(),
                    version: log_version(&records),
                    records,
                });
            }
            Err(e) => {
                eprintln!("lbw-logstat: {e}");
                std::process::exit(1);
            }
        }
    }
    let s = summarize(&files);
    if args.json {
        println!("{}", to_json(&s));
    } else {
        print!("{}", render(&s));
        if args.deep {
            print!("{}", render_deep(&files, &s));
        }
    }
    if let Some(csv) = args.csv {
        let mut o = String::from("file,sec,srv_bps,cli_bps\n");
        for f in &s.files {
            for t in &f.timeline {
                o.push_str(&format!("{},{},{},{}\n", f.name, t.sec, t.srv, t.cli));
            }
        }
        if let Err(e) = std::fs::write(&csv, o) {
            eprintln!("lbw-logstat: {csv}: {e}");
            std::process::exit(1);
        }
    }
}
