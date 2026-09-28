//! `09_doe` — Design of Experiments: welche UMAP-Zieldimension clustert am
//! besten?
//!
//! Methodik übernommen aus `cl-py-generator/example/174_autocluster`: Wir
//! **clustern im UMAP-`d`-D-Subraum**, aber **bewerten im originalen
//! L2-normierten 512D-Raum** mit dem Kriterium
//! `adjusted = Silhouette_orig · (1 − Rauschanteil)`. Configs mit über 40 %
//! Rauschen oder unter 2 Clustern werden verworfen. Da `umap-rs` stochastisch
//! ist (Hogwild-SGD, kein Seed), wiederholen wir jeden Design-Punkt mehrfach
//! und berichten Mittelwert, Streuung und die Taguchi-S/N-Ratio
//! (larger-is-better) als Robustheitsmaß — genau wie die Störgrößen-Behandlung
//! im Python-DoE.
//!
//! Reine CPU-Mathematik über [`crate::latent`]; keine ONNX-/X11-Abhängigkeit.

use crate::latent::{ClusterResult, hdbscan_cluster_ms, umap_embed};
use ndarray::Array2;

/// Maximal tolerierter Rauschanteil, sonst Config verworfen (wie Python).
pub const MAX_NOISE: f32 = 0.40;

/// Bewertung einer einzelnen (d, mcs)-Wiederholung.
#[derive(Debug, Clone, Copy)]
pub struct RunScore {
    /// Rejektionsfilter bestanden (Rauschen ≤ 40 %, ≥ 2 Cluster).
    pub accepted: bool,
    /// Silhouette im Originalraum (NaN wenn nicht bewertbar).
    pub silhouette: f32,
    /// `silhouette · (1 − noise)` (NaN wenn verworfen).
    pub adjusted: f32,
    /// Cluster-Zahl.
    pub n_clusters: usize,
    /// Rauschanteil.
    pub noise_ratio: f32,
}

/// Aggregat eines Design-Punkts (d, mcs, ms) über mehrere Wiederholungen.
#[derive(Debug, Clone)]
pub struct DoePoint {
    /// UMAP-Zieldimension.
    pub d: usize,
    /// HDBSCAN `min_cluster_size`.
    pub min_cluster_size: usize,
    /// HDBSCAN `min_samples` (`None` = defaultet auf `min_cluster_size`).
    pub min_samples: Option<usize>,
    /// Zahl akzeptierter Wiederholungen.
    pub n_rep: usize,
    /// Mittelwert `adjusted` über akzeptierte Läufe.
    pub mean_adj: f32,
    /// Streuung `adjusted`.
    pub std_adj: f32,
    /// Taguchi-S/N (larger-is-better); NaN bei < 2 Läufen.
    pub sn_ratio: f32,
    /// Mittlere Cluster-Zahl.
    pub mean_clusters: f32,
    /// Mittlerer Rauschanteil.
    pub mean_noise: f32,
}

/// Silhouette-Koeffizient im gegebenen (Original-)Raum, Rauschen ausgeschlossen.
///
/// Klassische Definition `s = (b − a) / max(a, b)` mit `a` = mittlere
/// Intra-Cluster-Distanz, `b` = kleinste mittlere Distanz zu einem fremden
/// Cluster. Euklidische Distanz auf L2-normierten Vektoren ist monoton zur
/// Kosinus-Distanz. Bei > `max_points` Punkten wird deterministisch
/// (Strided-Sampling) auf `max_points` reduziert, damit `O(m²·dim)` handhabbar
/// bleibt. Rückgabe `NaN`, wenn < 2 Cluster übrig bleiben.
#[must_use]
pub fn silhouette(space: &Array2<f32>, labels: &[i32], max_points: usize) -> f32 {
    // Nicht-Rausch-Punkte sammeln.
    let members: Vec<usize> = labels
        .iter()
        .enumerate()
        .filter(|&(_, &l)| l >= 0)
        .map(|(i, _)| i)
        .collect();
    if members.len() < 2 {
        return f32::NAN;
    }
    // Deterministisches Sampling per Stride.
    let stride = members.len().div_ceil(max_points).max(1);
    let sample: Vec<usize> = members.iter().copied().step_by(stride).collect();
    let lbl: Vec<i32> = sample.iter().map(|&i| labels[i]).collect();
    let distinct: std::collections::HashSet<i32> = lbl.iter().copied().collect();
    if distinct.len() < 2 {
        return f32::NAN;
    }
    let rows: Vec<&[f32]> = sample
        .iter()
        .map(|&i| space.row(i).to_slice().unwrap())
        .collect();
    let m = rows.len();
    let dist = |a: &[f32], b: &[f32]| -> f32 {
        a.iter()
            .zip(b.iter())
            .map(|(x, y)| (x - y) * (x - y))
            .sum::<f32>()
            .sqrt()
    };
    let mut sil_sum = 0.0f64;
    for i in 0..m {
        // a_i: mittlere Distanz zu gleichem Cluster; b_i: min über Fremdcluster.
        let mut same_sum = 0.0f32;
        let mut same_cnt = 0usize;
        // Fremd-Cluster: Label -> (Summe, Anzahl).
        let mut other: std::collections::HashMap<i32, (f32, usize)> =
            std::collections::HashMap::new();
        for j in 0..m {
            if i == j {
                continue;
            }
            let dij = dist(rows[i], rows[j]);
            if lbl[j] == lbl[i] {
                same_sum += dij;
                same_cnt += 1;
            } else {
                let e = other.entry(lbl[j]).or_insert((0.0, 0));
                e.0 += dij;
                e.1 += 1;
            }
        }
        if same_cnt == 0 || other.is_empty() {
            continue; // Singleton-Cluster steuert 0 bei.
        }
        let a = same_sum / same_cnt as f32;
        let b = other
            .values()
            .map(|(s, c)| s / *c as f32)
            .fold(f32::INFINITY, f32::min);
        let s = (b - a) / a.max(b).max(1e-12);
        sil_sum += s as f64;
    }
    (sil_sum / m as f64) as f32
}

/// Bewertet eine Cluster-Zuordnung im Originalraum nach dem Python-Kriterium.
#[must_use]
pub fn score_run(orig_space: &Array2<f32>, cluster: &ClusterResult, max_points: usize) -> RunScore {
    if cluster.noise_ratio > MAX_NOISE || cluster.n_clusters < 2 {
        return RunScore {
            accepted: false,
            silhouette: f32::NAN,
            adjusted: f32::NAN,
            n_clusters: cluster.n_clusters,
            noise_ratio: cluster.noise_ratio,
        };
    }
    let sil = silhouette(orig_space, &cluster.labels, max_points);
    let adjusted = sil * (1.0 - cluster.noise_ratio);
    RunScore {
        accepted: sil.is_finite(),
        silhouette: sil,
        adjusted,
        n_clusters: cluster.n_clusters,
        noise_ratio: cluster.noise_ratio,
    }
}

/// Taguchi-S/N-Ratio „larger-is-better": `−10·log10(mean(1/s²))`.
///
/// Belohnt hohen Mittelwert UND kleine Streuung. Erwartet positive Scores.
#[must_use]
pub fn taguchi_sn(scores: &[f32]) -> f32 {
    if scores.len() < 2 {
        return f32::NAN;
    }
    let mean_inv_sq =
        scores.iter().map(|s| 1.0 / (s * s + 1e-9)).sum::<f32>() / scores.len() as f32;
    -10.0 * mean_inv_sq.log10()
}

/// Führt einen Design-Punkt (d, mcs, ms) mit `n_rep` Wiederholungen aus.
///
/// `orig` ist der originale L2-normierte 512D-Raum (Bewertung), `n_neighbors`
/// steuert UMAP/KNN, `max_points` deckelt die Silhouette-Stichprobe.
#[must_use]
pub fn run_point(
    orig: &Array2<f32>,
    d: usize,
    min_cluster_size: usize,
    min_samples: Option<usize>,
    n_neighbors: usize,
    n_rep: usize,
    max_points: usize,
) -> DoePoint {
    let mut adj = Vec::new();
    let mut clusters = Vec::new();
    let mut noises = Vec::new();
    for _ in 0..n_rep.max(1) {
        let reduced = umap_embed(orig, n_neighbors, d);
        let cl = hdbscan_cluster_ms(&reduced, min_cluster_size, min_samples);
        let sc = score_run(orig, &cl, max_points);
        if sc.accepted {
            adj.push(sc.adjusted);
            clusters.push(sc.n_clusters as f32);
            noises.push(sc.noise_ratio);
        }
    }
    let n = adj.len();
    let mean_adj = if n == 0 {
        f32::NAN
    } else {
        adj.iter().sum::<f32>() / n as f32
    };
    let std_adj = if n < 2 {
        f32::NAN
    } else {
        (adj.iter().map(|x| (x - mean_adj).powi(2)).sum::<f32>() / n as f32).sqrt()
    };
    DoePoint {
        d,
        min_cluster_size,
        min_samples,
        n_rep: n,
        mean_adj,
        std_adj,
        sn_ratio: taguchi_sn(&adj),
        mean_clusters: mean_or_nan(&clusters),
        mean_noise: mean_or_nan(&noises),
    }
}

fn mean_or_nan(v: &[f32]) -> f32 {
    if v.is_empty() {
        f32::NAN
    } else {
        v.iter().sum::<f32>() / v.len() as f32
    }
}

/// Sweep-Konfiguration: Faktorstufen für `d`, `min_cluster_size`, `min_samples`.
pub struct Sweep {
    /// UMAP-Zieldimensionen (Faktor 1).
    pub dims: Vec<usize>,
    /// HDBSCAN `min_cluster_size`-Stufen (Faktor 2).
    pub mcs: Vec<usize>,
    /// HDBSCAN `min_samples`-Stufen (Faktor 3); `None` = an `mcs` gekoppelt.
    pub min_samples: Vec<Option<usize>>,
    /// UMAP/KNN-Nachbarn.
    pub n_neighbors: usize,
    /// Wiederholungen je Design-Punkt (Störgröße UMAP-Jitter).
    pub n_rep: usize,
    /// Silhouette-Stichprobengröße.
    pub max_points: usize,
}

impl Default for Sweep {
    fn default() -> Self {
        Self {
            dims: vec![2, 4, 8, 11, 16, 24],
            mcs: vec![5, 10, 20],
            min_samples: vec![None],
            n_neighbors: 15,
            n_rep: 3,
            max_points: 1200,
        }
    }
}

/// Läuft den vollen Faktor-Sweep und liefert alle aggregierten Punkte.
#[must_use]
pub fn run_sweep(orig: &Array2<f32>, sweep: &Sweep) -> Vec<DoePoint> {
    let mut out = Vec::new();
    for &d in &sweep.dims {
        for &mcs in &sweep.mcs {
            for &ms in &sweep.min_samples {
                out.push(run_point(
                    orig,
                    d,
                    mcs,
                    ms,
                    sweep.n_neighbors,
                    sweep.n_rep,
                    sweep.max_points,
                ));
            }
        }
    }
    out
}

/// Bester Punkt nach mittlerem `adjusted` (ignoriert NaN/leere Punkte).
#[must_use]
pub fn best_by_mean(points: &[DoePoint]) -> Option<&DoePoint> {
    points
        .iter()
        .filter(|p| p.mean_adj.is_finite())
        .max_by(|a, b| a.mean_adj.partial_cmp(&b.mean_adj).unwrap())
}

#[cfg(test)]
mod tests {
    use super::*;
    use ndarray::arr2;

    #[test]
    fn silhouette_two_tight_clusters_high() {
        // Zwei weit getrennte, dichte 2D-Cluster → Silhouette nahe 1.
        let space = arr2(&[
            [0.0, 0.0],
            [0.1, 0.0],
            [0.0, 0.1],
            [10.0, 10.0],
            [10.1, 10.0],
            [10.0, 10.1],
        ]);
        let labels = [0, 0, 0, 1, 1, 1];
        let s = silhouette(&space, &labels, 100);
        assert!(s > 0.9, "erwartete hohe Silhouette, war {s}");
    }

    #[test]
    fn silhouette_needs_two_clusters() {
        let space = arr2(&[[0.0, 0.0], [1.0, 1.0]]);
        // Alles ein Cluster → NaN.
        assert!(silhouette(&space, &[0, 0], 100).is_nan());
        // Alles Rauschen → NaN.
        assert!(silhouette(&space, &[-1, -1], 100).is_nan());
    }

    #[test]
    fn score_run_rejects_high_noise() {
        let space = arr2(&[[0.0, 0.0], [1.0, 0.0], [0.0, 1.0]]);
        let cl = ClusterResult {
            labels: vec![0, 0, 1],
            n_clusters: 2,
            noise_ratio: 0.5, // > MAX_NOISE
        };
        let sc = score_run(&space, &cl, 100);
        assert!(!sc.accepted);
        assert!(sc.adjusted.is_nan());
    }

    #[test]
    fn taguchi_prefers_low_variance() {
        // Gleicher Mittelwert, kleinere Streuung → höhere S/N.
        let tight = taguchi_sn(&[0.50, 0.50, 0.50]);
        let loose = taguchi_sn(&[0.30, 0.50, 0.70]);
        assert!(tight > loose, "tight={tight} loose={loose}");
    }

    #[test]
    fn taguchi_needs_two_samples() {
        assert!(taguchi_sn(&[0.5]).is_nan());
    }

    #[test]
    fn best_by_mean_picks_highest() {
        let pts = vec![
            DoePoint {
                d: 2,
                min_cluster_size: 5,
                min_samples: None,
                n_rep: 3,
                mean_adj: 0.20,
                std_adj: 0.01,
                sn_ratio: 1.0,
                mean_clusters: 10.0,
                mean_noise: 0.1,
            },
            DoePoint {
                d: 11,
                min_cluster_size: 5,
                min_samples: None,
                n_rep: 3,
                mean_adj: 0.35,
                std_adj: 0.02,
                sn_ratio: 2.0,
                mean_clusters: 12.0,
                mean_noise: 0.15,
            },
        ];
        assert_eq!(best_by_mean(&pts).unwrap().d, 11);
    }
}
