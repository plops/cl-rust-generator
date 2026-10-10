//! `lbw-logstat` — Offline-Auswertung von `.lbwlog`-Aufzeichnungen.
//!
//! Liest Server- und/oder Client-Logs, druckt Durchsatz, Gaps, Dedup und
//! Latenzen (Human-Text oder `--json`, Timeline optional als `--csv`).
//! Zerrissene Dateischlüsse (Absturz) werden als Teilstand mit Warnung
//! ausgewertet, nicht als harter Fehler.

use clap::Parser;
use lbw_log::io::load_lenient;
use lbw_log::stats::{BUDGET_BPS, FileLog, Lat, Summary, summarize, to_json};

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
}

fn lat(l: &Lat) -> String {
    if l.n == 0 {
        return "—".into();
    }
    format!(
        "n={} avg={:.1} p50={:.1} max={:.1} ms",
        l.n, l.avg, l.p50, l.max
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
    match s.clock_offset_ms {
        Some(d) => o.push_str(&format!(
            "Uhr-Offset Server−Client: {d:.1} ms ({} Samples, Min-Filter)\n",
            s.clock_offset_samples
        )),
        None => o.push_str("Uhr-Offset: — (kein Client/Server-Paar mit gleichen Inputs)\n"),
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
