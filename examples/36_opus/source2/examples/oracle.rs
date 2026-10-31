//! Final-Range-Orakel auf beliebigen Dateien: `cargo run --release --example oracle -- a.opus ...`
//!
//! Druckt je Datei: Pakete, verglichen, Treffer, erster Fehlschlag (Index, Modus).
fn main() {
    let mut bad_total = 0usize;
    for path in std::env::args().skip(1) {
        let bytes = std::fs::read(&path).expect("lesen");
        let ps = opus_vis::analyze::read_packets(&bytes, bytes.len() as u64).expect("ogg");
        let dec = opus_vis::decode::decode_packets(&ps).expect("decode");
        let mut w = opus_vis::walk::Walker::new();
        let (mut cmp, mut ok, mut first_bad) = (0usize, 0usize, None);
        let mut modes = [0usize; 3];
        let (mut red, mut lbrr, mut switches, mut plc, mut last) =
            (0usize, 0usize, 0usize, 0usize, None);
        for (i, p) in ps.packets.iter().enumerate() {
            let pw = match w.packet(&p.data) {
                Ok(pw) => pw,
                Err(e) => {
                    first_bad.get_or_insert(format!("#{i} walk-fehler {e}"));
                    continue;
                }
            };
            modes[pw.toc.mode as usize] += 1;
            if last.is_some_and(|m| m != pw.toc.mode) {
                switches += 1;
            }
            last = Some(pw.toc.mode);
            for f in &pw.frames {
                red += usize::from(f.redundancy.is_some());
                lbrr += f.silk.as_ref().map_or(0, |s| s.lbrr.len());
                plc += usize::from(f.plc);
                let b = f.budget();
                if b.total() != f.bytes as i32 * 64 {
                    first_bad.get_or_insert(format!(
                        "#{i} budget {} != {} {:?}",
                        b.total(),
                        f.bytes * 64,
                        b
                    ));
                }
            }
            if let Some(r) = pw.final_range() {
                cmp += 1;
                if r == dec.final_range[i] {
                    ok += 1;
                } else {
                    first_bad.get_or_insert(format!(
                        "#{i} {:?} cfg {} code {} frames {} red {:?}",
                        pw.toc.mode,
                        pw.toc.config,
                        p.data[0] & 3,
                        pw.frames.len(),
                        pw.frames
                            .iter()
                            .map(|f| f.redundancy.as_ref().map(|r| (r.celt_to_silk, r.bytes)))
                            .collect::<Vec<_>>()
                    ));
                }
            }
        }
        bad_total += cmp - ok;
        println!(
            "{path}: pakete {} (silk/hyb/celt {:?}, wechsel {switches}, redundanz {red}, lbrr {lbrr}, plc {plc}) verglichen {cmp} gleich {ok} {}",
            ps.packets.len(),
            modes,
            first_bad.unwrap_or_default()
        );
    }
    std::process::exit(i32::from(bad_total > 0));
}
