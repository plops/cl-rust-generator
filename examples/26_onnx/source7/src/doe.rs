//! doe.rs — Headless-DoE-Treiber: findet die beste UMAP-Cluster-Dimension.
//!
//! Lädt `faces_db.bin`, sweept die Zieldimension `d` (und `min_cluster_size`),
//! clustert je Punkt im `d`-D-UMAP-Subraum und bewertet im originalen 512D-Raum
//! mit `adjusted = Silhouette_orig · (1 − Rauschen)` (Methodik aus
//! `174_autocluster`). Gibt eine Tabelle + optional CSV aus und nennt die beste
//! Dimension. Kein Fenster, keine Modelle nötig.
//!
//! Bedienung: `doe [--db PATH] [--dims 2,4,8,11,16,24] [--mcs 5,10,20]`
//!            `[--neighbors 15] [--rep 3] [--max-points 1200] [--csv PATH]`

// Teilt sich die Kernmodule mit den anderen Binaries; ungenutzte Re-ID-Teile
// sind hier erwartbar tot.
#![allow(dead_code)]

#[path = "06_face_database.rs"]
mod db;
#[path = "09_doe.rs"]
mod doe;
#[path = "08_latent.rs"]
mod latent;
#[path = "01_types.rs"]
mod types;

use db::FaceDatabase;
use doe::{Sweep, best_by_mean, run_sweep};
use latent::LatentData;

struct Args {
    db_path: String,
    sweep: Sweep,
    csv: Option<String>,
}

fn parse_list(s: &str) -> Result<Vec<usize>, String> {
    s.split(',')
        .map(|t| t.trim().parse::<usize>().map_err(|_| format!("Zahl? {t}")))
        .collect()
}

fn parse_args() -> Result<Args, String> {
    let mut a = Args {
        db_path: "faces_db.bin".into(),
        sweep: Sweep::default(),
        csv: None,
    };
    let mut it = std::env::args().skip(1);
    while let Some(f) = it.next() {
        let mut next = || it.next().ok_or(format!("{f} braucht Wert"));
        match f.as_str() {
            "--db" => a.db_path = next()?,
            "--dims" => a.sweep.dims = parse_list(&next()?)?,
            "--mcs" => a.sweep.mcs = parse_list(&next()?)?,
            "--min-samples" => {
                // 0 bedeutet „an min_cluster_size gekoppelt" (HDBSCAN-Default).
                a.sweep.min_samples = parse_list(&next()?)?
                    .into_iter()
                    .map(|v| if v == 0 { None } else { Some(v) })
                    .collect();
            }
            "--neighbors" => a.sweep.n_neighbors = next()?.parse().map_err(|_| "neighbors?")?,
            "--rep" => a.sweep.n_rep = next()?.parse().map_err(|_| "rep?")?,
            "--max-points" => a.sweep.max_points = next()?.parse().map_err(|_| "max-points?")?,
            "--csv" => a.csv = Some(next()?),
            "--help" | "-h" => return Err("help".into()),
            x => return Err(format!("unbekannt: {x}")),
        }
    }
    Ok(a)
}

fn usage() -> &'static str {
    "doe [--db PATH] [--dims 2,4,8,11,16,24] [--mcs 5,10,20] \
     [--min-samples 0,5,10] [--neighbors 15] [--rep 3] [--max-points 1200] \
     [--csv PATH]   (min-samples 0 = an mcs gekoppelt)"
}

fn ms_str(ms: Option<usize>) -> String {
    ms.map_or_else(|| "=mcs".to_string(), |v| v.to_string())
}

fn main() {
    let args = parse_args().unwrap_or_else(|e| {
        if e == "help" {
            println!("{}", usage());
            std::process::exit(0);
        }
        eprintln!("{e}\n{}", usage());
        std::process::exit(2);
    });

    let database = FaceDatabase::load(&args.db_path);
    let ld = LatentData::from_database(&database);
    println!(
        "geladen: {} Exemplare, {} Personen aus {}",
        ld.len(),
        ld.n_persons,
        args.db_path
    );
    if ld.len() < 8 {
        eprintln!("zu wenige Exemplare ({}) für einen DoE", ld.len());
        std::process::exit(2);
    }
    println!(
        "sweep: dims={:?} mcs={:?} min_samples={:?} neighbors={} rep={} max_points={}",
        args.sweep.dims,
        args.sweep.mcs,
        args.sweep
            .min_samples
            .iter()
            .map(|m| ms_str(*m))
            .collect::<Vec<_>>(),
        args.sweep.n_neighbors,
        args.sweep.n_rep,
        args.sweep.max_points
    );

    let t0 = std::time::Instant::now();
    let points = run_sweep(&ld.data, &args.sweep);
    let secs = t0.elapsed().as_secs_f64();

    // Tabelle.
    println!(
        "\n{:>3}  {:>4}  {:>5}  {:>5}  {:>8}  {:>8}  {:>8}  {:>9}  {:>7}",
        "d", "mcs", "ms", "n_rep", "mean_adj", "std_adj", "sn", "clusters", "noise%"
    );
    println!("{}", "-".repeat(74));
    for p in &points {
        println!(
            "{:>3}  {:>4}  {:>5}  {:>5}  {:>8.4}  {:>8.4}  {:>8.3}  {:>9.1}  {:>6.1}%",
            p.d,
            p.min_cluster_size,
            ms_str(p.min_samples),
            p.n_rep,
            p.mean_adj,
            p.std_adj,
            p.sn_ratio,
            p.mean_clusters,
            p.mean_noise * 100.0
        );
    }

    if let Some(csv) = &args.csv {
        write_csv(csv, &points).unwrap_or_else(|e| eprintln!("CSV-Fehler: {e}"));
        println!("CSV geschrieben: {csv}");
    }

    match best_by_mean(&points) {
        Some(b) => {
            println!(
                "\nBESTE KONFIG: d={} (mcs={}, ms={}), mean_adj={:.4} ± {:.4}, \
                 {:.0} Cluster, {:.1}% Rauschen",
                b.d,
                b.min_cluster_size,
                ms_str(b.min_samples),
                b.mean_adj,
                b.std_adj,
                b.mean_clusters,
                b.mean_noise * 100.0
            );
            println!("sweep fertig in {secs:.1}s");
            println!(
                "stats best_d={} best_mcs={} best_ms={} best_adj={:.4} points={}",
                b.d,
                b.min_cluster_size,
                ms_str(b.min_samples),
                b.mean_adj,
                points.len()
            );
        }
        None => {
            println!(
                "\nKein Design-Punkt wurde akzeptiert (alle > 40% Rauschen \
                      oder < 2 Cluster)."
            );
            println!("stats best_d=none points={}", points.len());
        }
    }
}

fn write_csv(path: &str, points: &[doe::DoePoint]) -> std::io::Result<()> {
    use std::io::Write;
    let mut f = std::fs::File::create(path)?;
    writeln!(
        f,
        "d,min_cluster_size,min_samples,n_rep,mean_adj,std_adj,sn_ratio,mean_clusters,mean_noise"
    )?;
    for p in points {
        writeln!(
            f,
            "{},{},{},{},{:.6},{:.6},{:.6},{:.3},{:.6}",
            p.d,
            p.min_cluster_size,
            p.min_samples.unwrap_or(0),
            p.n_rep,
            p.mean_adj,
            p.std_adj,
            p.sn_ratio,
            p.mean_clusters,
            p.mean_noise
        )?;
    }
    Ok(())
}
