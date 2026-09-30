//! `14_bench` — headless Benchmark-Schleife mit TSV-Log.
//!
//! Für jede (Sprache, Generator, Sample) läuft `Engine::run`; die
//! `Stats` sammeln, das TSV protokolliert jede Zeile (GT + OCR zum
//! Ansehen), der Markdown-Report geht nach stdout.

use crate::cli::BenchArgs;
use crate::engine::Engine;
use crate::stats::Stats;

/// TSV-Spalten (eine Zeile pro Sample).
pub const TSV_HEADER: &str = "sample\tlang\tgen\tmodel\tseed\tpx\tcer\trecall\tfp\tiou\tconf\trender_ms\tdet_ms\trec_ms\tgt\tocr";

/// Führt den Benchmark aus; gibt den Markdown-Report zurück.
pub fn run(args: &BenchArgs) -> Result<String, String> {
    let mut eng = Engine::open(
        &args.models_dir,
        args.font.as_deref(),
        Some(&args.corpus_dir),
    )?;
    let mut stats = Stats::new();
    let mut tsv = args.tsv.is_some().then(|| format!("{TSV_HEADER}\n"));
    let mut n = 0u64;
    for &li in &args.langs {
        for &mode in &args.gens {
            for s in 0..args.samples {
                let seed = args.seed.wrapping_add(s as u64);
                let sample = eng.run(&args.settings(li, mode), seed)?;
                stats.add(&sample.lang, &sample.model, &sample.eval, sample.times);
                if let Some(t) = tsv.as_mut() {
                    n += 1;
                    t.push_str(&row(n, &sample, seed));
                    t.push('\n');
                }
            }
        }
    }
    if let (Some(path), Some(t)) = (&args.tsv, tsv) {
        std::fs::write(path, t).map_err(|e| format!("{}: {e}", path.display()))?;
    }
    Ok(stats.markdown())
}

fn row(n: u64, sample: &crate::engine::Sample, seed: u64) -> String {
    let e = &sample.eval;
    let lines = e.lines.len().max(1) as f32;
    let iou = e.lines.iter().map(|l| l.iou).sum::<f32>() / lines;
    let matched: Vec<f32> = e
        .lines
        .iter()
        .filter(|l| l.matched > 0)
        .map(|l| l.conf)
        .collect();
    let conf = if matched.is_empty() {
        0.0
    } else {
        matched.iter().sum::<f32>() / matched.len() as f32
    };
    let gt = sample
        .gt
        .iter()
        .map(|l| clean(l))
        .collect::<Vec<_>>()
        .join(" | ");
    let ocr = e
        .lines
        .iter()
        .map(|l| clean(&l.ocr))
        .collect::<Vec<_>>()
        .join(" | ");
    format!(
        "{n}\t{}\t{}\t{}\t{seed}\t{}\t{:.4}\t{:.2}\t{}\t{iou:.2}\t{conf:.2}\t{:.1}\t{:.1}\t{:.1}\t{gt}\t{ocr}",
        sample.lang,
        sample.mode.name(),
        sample.model,
        sample.px,
        e.mean_cer(),
        e.recall(),
        e.fp_boxes,
        sample.times.render_ms,
        sample.times.det_ms,
        sample.times.rec_ms,
    )
}

/// Entfernt TSV-Zeilenbrecher aus Text.
fn clean(s: &str) -> String {
    s.chars()
        .map(|c| {
            if c == '\t' || c == '\n' || c == '\r' {
                ' '
            } else {
                c
            }
        })
        .collect()
}
