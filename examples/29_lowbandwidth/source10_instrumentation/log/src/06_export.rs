//! `06_export` — JSON-Darstellung der `Summary` (Hand-Code).
//!
//! Flache Struktur, kein `serde_json` wert. Tiefenanalyse (`05_deep`) wird
//! nur als Human-Text gerendert (`--deep`), nicht exportiert.

use crate::record::MsgKind;
use crate::stats::Lat;
use crate::summary::Summary;

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
        MsgKind::RemoveText => "RemoveText",
    }
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
        "{{\"n\":{},\"avg\":{:.3},\"p50\":{:.3},\"p90\":{:.3},\"p99\":{:.3},\"max\":{:.3}}}",
        l.n, l.avg, l.p50, l.p90, l.p99, l.max
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
    use crate::stats::FileLog;
    use crate::summary::summarize;

    fn session(app: &str) -> crate::record::LogRecord {
        crate::record::LogRecord::Session {
            app: app.into(),
            version: 1,
            args: vec![],
        }
    }

    #[test]
    fn json_is_wellformed_for_empty_summary() {
        let s = summarize(&[FileLog {
            name: "x".into(),
            records: vec![session("?")],
            version: crate::record::LOG_VERSION,
        }]);
        let j = to_json(&s);
        assert!(j.starts_with("{\"files\":[") && j.ends_with("}"), "{j}");
        assert!(j.contains("\"clock_offset_ms\":null"));
        assert!(j.contains("\"p90\":0.000"));
    }
}
