//! `08_latent` — Datenaufbereitung für die UMAP-Latent-Space-Visualisierung.
//!
//! Liest die Exemplar-Bank aus `faces_db.bin`, baut die 512D-Matrix samt
//! Personen-Labels und Thumbnails, und liefert die von `umap-rs` verlangten
//! Vorstufen: exakte Kosinus-KNN (Brute-Force; Vektoren sind L2-normiert,
//! also Distanz = `1 − dot`) und eine deterministische PCA-2-Initialisierung.
//!
//! Keine ONNX-/X11-Abhängigkeit — reine CPU-Mathematik, host-testbar.

use crate::db::FaceDatabase;
use crate::types::{CROP_SIZE, EMBED_DIM};
use hdbscan::{DistanceMetric, Hdbscan, HdbscanHyperParams, NnAlgorithm};
use ndarray::Array2;
use umap_rs::{GraphParams, Umap, UmapConfig};

/// Ein Punkt im Latent-Space: Herkunft (Person/Exemplar) + Thumbnail-Referenz.
#[derive(Debug, Clone)]
pub struct PointMeta {
    /// Personen-ID (Farbe/Legende).
    pub person_id: u32,
    /// Index des Exemplars innerhalb der Person.
    pub exemplar_idx: usize,
}

/// Aufbereiteter Datensatz für UMAP + Rendering.
pub struct LatentData {
    /// Zeilen-normierte Embeddings, Form `(n, 512)`.
    pub data: Array2<f32>,
    /// Metadaten pro Zeile (gleiche Reihenfolge wie `data`).
    pub meta: Vec<PointMeta>,
    /// Thumbnails pro Zeile (`112*112*3` RGB), gleiche Reihenfolge.
    pub thumbs: Vec<Vec<u8>>,
    /// Anzahl distinkter Personen (für Farbpalette).
    pub n_persons: usize,
}

impl LatentData {
    /// Anzahl Punkte (Exemplare).
    #[must_use]
    pub fn len(&self) -> usize {
        self.meta.len()
    }

    /// Ob keine Punkte vorhanden sind.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.meta.is_empty()
    }

    /// Lädt und flacht die DB in eine Matrix + Metadaten ab.
    #[must_use]
    pub fn from_database(db: &FaceDatabase) -> Self {
        let mut rows: Vec<f32> = Vec::new();
        let mut meta = Vec::new();
        let mut thumbs = Vec::new();
        for p in db.persons() {
            for (k, ex) in p.exemplars.iter().enumerate() {
                rows.extend_from_slice(&ex.embedding.v);
                meta.push(PointMeta {
                    person_id: p.id,
                    exemplar_idx: k,
                });
                thumbs.push(ex.thumbnail.clone());
            }
        }
        let n = meta.len();
        let data = Array2::from_shape_vec((n, EMBED_DIM), rows)
            .expect("Zeilenlänge == EMBED_DIM je Punkt");
        Self {
            data,
            meta,
            thumbs,
            n_persons: db.persons().len(),
        }
    }
}

/// Skaliert ein `CROP_SIZE`-Thumbnail (RGB) per Nearest auf `size` (RGBA).
#[must_use]
pub fn thumb_scaled_rgba(thumb: &[u8], size: usize) -> Vec<u8> {
    let mut out = vec![0u8; size * size * 4];
    for y in 0..size {
        for x in 0..size {
            let si = ((y * CROP_SIZE / size) * CROP_SIZE + (x * CROP_SIZE / size)) * 3;
            let di = (y * size + x) * 4;
            if si + 3 <= thumb.len() {
                out[di..di + 3].copy_from_slice(&thumb[si..si + 3]);
            }
            out[di + 3] = 255;
        }
    }
    out
}

/// Exakte k-nächste-Nachbarn unter Kosinus-Distanz (`1 − dot`, da normiert).
///
/// Brute-Force `O(n²·d)` — für einige tausend Exemplare vollkommen genügend
/// und exakt (kein ANN-Approximationsfehler). Der Punkt selbst wird nicht als
/// eigener Nachbar aufgenommen. `k` wird auf `n-1` gedeckelt.
///
/// Rückgabe: `(indices (n,k) u32, dists (n,k) f32)`, aufsteigend nach Distanz.
#[must_use]
pub fn cosine_knn(data: &Array2<f32>, k: usize) -> (Array2<u32>, Array2<f32>) {
    let n = data.nrows();
    let k = k.min(n.saturating_sub(1)).max(1);
    let mut idx = Array2::<u32>::zeros((n, k));
    let mut dist = Array2::<f32>::zeros((n, k));
    let rows: Vec<&[f32]> = (0..n).map(|i| data.row(i).to_slice().unwrap()).collect();
    for i in 0..n {
        // (Distanz, Nachbar-Index) über alle j != i, dann Teilsortierung.
        let mut cand: Vec<(f32, u32)> = Vec::with_capacity(n - 1);
        let ri = rows[i];
        for (j, rj) in rows.iter().enumerate() {
            if j == i {
                continue;
            }
            let dot: f32 = ri.iter().zip(rj.iter()).map(|(a, b)| a * b).sum();
            cand.push((1.0 - dot, j as u32));
        }
        cand.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap_or(std::cmp::Ordering::Equal));
        for (c, (d, j)) in cand.into_iter().take(k).enumerate() {
            dist[[i, c]] = d.max(0.0);
            idx[[i, c]] = j;
        }
    }
    (idx, dist)
}

/// Projiziert ein einzelnes neues 512D-Embedding in ein bereits berechnetes
/// 2D-Layout — ohne UMAP neu zu rechnen (out-of-sample / Landmark-Embedding).
///
/// Idee: Ein Live-Gesicht liegt dort im Latent-Space, wo die ihm ähnlichsten
/// gespeicherten Exemplare liegen. Wir nehmen die `k` nächsten Exemplare unter
/// Kosinus-Ähnlichkeit (`= dot`, da L2-normiert), gewichten ihre bekannten
/// 2D-Positionen mit einem scharfen Similarity-Kernel und mitteln sie
/// baryzentrisch. Das ist deterministisch, O(n·d) pro Punkt (Millisekunden für
/// einige tausend Exemplare) und platziert Live-Punkte konsistent im selben
/// Layout wie die statische Wolke — Voraussetzung für eine sinnvolle
/// Trajektorie.
///
/// `data` (n×512) und `layout` (n×2) müssen dieselbe Zeilenreihenfolge haben.
/// `query` ist das L2-normierte Live-Embedding. Rückgabe: `[x, y]` im
/// Layout-Koordinatensystem plus die beste erreichte Similarity `S_max`
/// (für Färbung/HUD). Bei leerer Referenz → `([0,0], -inf)`.
#[must_use]
pub fn project_into_2d(
    data: &Array2<f32>,
    layout: &[[f32; 2]],
    query: &[f32],
    k: usize,
) -> ([f32; 2], f32) {
    let n = data.nrows();
    if n == 0 || layout.len() != n {
        return ([0.0, 0.0], f32::NEG_INFINITY);
    }
    // Ähnlichkeit (dot) zu jedem Exemplar; Index behalten.
    let mut sims: Vec<(f32, usize)> = (0..n)
        .map(|i| {
            let row = data.row(i);
            let dot: f32 = row.iter().zip(query.iter()).map(|(a, b)| a * b).sum();
            (dot, i)
        })
        .collect();
    // Absteigend nach Similarity; die k besten behalten.
    sims.sort_by(|a, b| b.0.partial_cmp(&a.0).unwrap_or(std::cmp::Ordering::Equal));
    let k = k.clamp(1, n);
    let s_max = sims[0].0;
    // Scharfer Kernel: Gewicht = exp(beta·(sim − s_max)); relativ zu s_max
    // stabilisiert es numerisch. beta groß → Punkt klebt am Nachbarn.
    let beta = 12.0f32;
    let mut wsum = 0.0f32;
    let mut acc = [0.0f32; 2];
    for &(sim, i) in sims.iter().take(k) {
        let w = (beta * (sim - s_max)).exp();
        acc[0] += w * layout[i][0];
        acc[1] += w * layout[i][1];
        wsum += w;
    }
    if wsum > 1e-12 {
        acc[0] /= wsum;
        acc[1] /= wsum;
    } else {
        acc = layout[sims[0].1];
    }
    (acc, s_max)
}

/// Reduziert `data` per UMAP auf `n_components` Dimensionen.
///
/// Nutzt Kosinus-KNN (exakt) und PCA-Init in derselben Zieldimension. Für
/// `n ≤ n_components` oder sehr kleine `n` wird direkt die PCA-Projektion
/// zurückgegeben (UMAP braucht genug Nachbarn). `n_neighbors` wird auf
/// `n-1` gedeckelt. UMAP selbst ist stochastisch (Hogwild-SGD, kein Seed) —
/// Läufe variieren leicht; die DoE misst diese Streuung als Störgröße.
#[must_use]
pub fn umap_embed(data: &Array2<f32>, n_neighbors: usize, n_components: usize) -> Array2<f32> {
    let n = data.nrows();
    let nc = n_components.max(1);
    if n == 0 {
        return Array2::zeros((0, nc));
    }
    // Zu wenige Punkte für sinnvolle KNN/UMAP → PCA-Projektion direkt.
    if n <= nc + 1 || n < 4 {
        return pca_init(data, nc);
    }
    let k = n_neighbors.min(n - 1).max(2);
    let (idx, dists) = cosine_knn(data, k);
    let init = pca_init(data, nc);
    let config = UmapConfig {
        n_components: nc,
        graph: GraphParams {
            n_neighbors: k,
            ..Default::default()
        },
        ..Default::default()
    };
    let umap = Umap::new(config);
    let model = umap.fit(data.view(), idx.view(), dists.view(), init.view());
    model.embedding().to_owned()
}
pub struct ClusterResult {
    /// Label je Punkt (gleiche Reihenfolge wie `LatentData`); `-1` = Rauschen.
    pub labels: Vec<i32>,
    /// Anzahl echter Cluster (ohne Rauschen).
    pub n_clusters: usize,
    /// Anteil Rausch-Punkte in `[0, 1]`.
    pub noise_ratio: f32,
}

/// Clustert die 512D-Embeddings dichtebasiert per HDBSCAN.
///
/// Die Vektoren sind L2-normiert, daher ist die euklidische Distanz auf der
/// Einheitskugel streng monoton zur Kosinus-Distanz
/// (`‖a−b‖² = 2 − 2·cos`) — euklidisches Clustering entspricht also dem
/// Kosinus-Clustering, das auch die Re-ID-Schwellen nutzen. HDBSCAN braucht
/// weder eine Cluster-Zahl noch eine feste Dichte und markiert Ausreißer als
/// Rauschen (`-1`).
///
/// `min_cluster_size` wird auf `≥2` gedeckelt; `min_samples` (Kerndistanz-k)
/// ist optional und defaultet in HDBSCAN auf `min_cluster_size`. Bei `<3`
/// Punkten wird gar nicht geclustert (alles Rauschen).
#[must_use]
pub fn hdbscan_cluster_ms(
    data: &Array2<f32>,
    min_cluster_size: usize,
    min_samples: Option<usize>,
) -> ClusterResult {
    let n = data.nrows();
    if n < 3 {
        return ClusterResult {
            labels: vec![-1; n],
            n_clusters: 0,
            noise_ratio: if n == 0 { 0.0 } else { 1.0 },
        };
    }
    let rows: Vec<Vec<f32>> = (0..n).map(|i| data.row(i).to_vec()).collect();
    let mut builder = HdbscanHyperParams::builder()
        .min_cluster_size(min_cluster_size.max(2))
        .dist_metric(DistanceMetric::Euclidean)
        .nn_algorithm(NnAlgorithm::KdTree);
    if let Some(ms) = min_samples {
        builder = builder.min_samples(ms.max(1));
    }
    let hp = builder.build();
    let labels = Hdbscan::new(&rows, hp)
        .cluster_par()
        .unwrap_or_else(|_| vec![-1; n]);
    summarize_labels(labels)
}

/// Wie [`hdbscan_cluster_ms`], aber mit `min_samples = min_cluster_size`.
#[must_use]
pub fn hdbscan_cluster(data: &Array2<f32>, min_cluster_size: usize) -> ClusterResult {
    hdbscan_cluster_ms(data, min_cluster_size, None)
}

/// Fasst Roh-Labels zu `ClusterResult` zusammen (Cluster-Zahl + Rauschanteil).
fn summarize_labels(labels: Vec<i32>) -> ClusterResult {
    let n = labels.len();
    let noise = labels.iter().filter(|&&l| l < 0).count();
    let mut seen = std::collections::HashSet::new();
    for &l in &labels {
        if l >= 0 {
            seen.insert(l);
        }
    }
    ClusterResult {
        n_clusters: seen.len(),
        noise_ratio: if n == 0 { 0.0 } else { noise as f32 / n as f32 },
        labels,
    }
}

/// Deterministische 2D-PCA-Initialisierung (Spezialfall von [`pca_init`]).
#[must_use]
pub fn pca_init_2d(data: &Array2<f32>) -> Array2<f32> {
    pca_init(data, 2)
}

/// Deterministische `out_dim`-D-PCA-Initialisierung via Power-Iteration.
///
/// Zentriert die Daten, schätzt die `out_dim` dominanten Eigenvektoren der
/// Kovarianz per Power-Iteration mit Deflation (ohne externe LA-Bibliothek)
/// und projiziert. Jede Achse wird auf Standardabweichung ~10 skaliert
/// (UMAP-üblicher Startraum). `out_dim` wird auf `d` gedeckelt.
#[must_use]
pub fn pca_init(data: &Array2<f32>, out_dim: usize) -> Array2<f32> {
    let n = data.nrows();
    let d = data.ncols();
    let out_dim = out_dim.min(d).max(1);
    let mut init = Array2::<f32>::zeros((n, out_dim));
    if n == 0 {
        return init;
    }
    // Spaltenmittel abziehen.
    let mut mean = vec![0.0f32; d];
    for row in data.rows() {
        for (m, &x) in mean.iter_mut().zip(row.iter()) {
            *m += x;
        }
    }
    for m in &mut mean {
        *m /= n as f32;
    }
    let centered: Vec<Vec<f32>> = data
        .rows()
        .into_iter()
        .map(|r| r.iter().zip(mean.iter()).map(|(x, m)| x - m).collect())
        .collect();

    let comps = power_iteration_k(&centered, d, out_dim);
    for (i, row) in centered.iter().enumerate() {
        for (c, comp) in comps.iter().enumerate() {
            init[[i, c]] = row.iter().zip(comp.iter()).map(|(a, b)| a * b).sum();
        }
    }
    // Jede Achse auf sinnvolle Startskala bringen (Std-Abw. je Achse ~ 10).
    for c in 0..out_dim {
        let col: Vec<f32> = (0..n).map(|i| init[[i, c]]).collect();
        let mean_c = col.iter().sum::<f32>() / n as f32;
        let var = col.iter().map(|v| (v - mean_c).powi(2)).sum::<f32>() / n.max(1) as f32;
        let std = var.sqrt().max(1e-6);
        for i in 0..n {
            init[[i, c]] = (init[[i, c]] - mean_c) / std * 10.0;
        }
    }
    init
}

/// `k` dominante Eigenvektoren der Kovarianz per Power-Iteration + Deflation.
fn power_iteration_k(centered: &[Vec<f32>], d: usize, k: usize) -> Vec<Vec<f32>> {
    let mut comps: Vec<Vec<f32>> = Vec::with_capacity(k);
    for _ in 0..k {
        let mut v = dominant_eigenvector(centered, d, &comps);
        normalize_vec(&mut v);
        comps.push(v);
    }
    comps
}

/// Ein dominanter Eigenvektor, orthogonal zu allen bereits gefundenen `deflate`.
fn dominant_eigenvector(centered: &[Vec<f32>], d: usize, deflate: &[Vec<f32>]) -> Vec<f32> {
    // Startvektor deterministisch (nicht kollinear zu Achse 0), abhängig vom
    // bisherigen Komponenten-Index für unterschiedliche Startrichtungen.
    let phase = deflate.len();
    let mut v = vec![0.0f32; d];
    for (i, vi) in v.iter_mut().enumerate() {
        *vi = 1.0 + ((i + phase) % 7) as f32 * 0.13;
    }
    normalize_vec(&mut v);
    for _ in 0..64 {
        // w = C·v = Σ_x x·(xᵀv), ohne die d×d-Matrix zu materialisieren.
        let mut w = vec![0.0f32; d];
        for x in centered {
            let proj: f32 = x.iter().zip(v.iter()).map(|(a, b)| a * b).sum();
            for (wi, xi) in w.iter_mut().zip(x.iter()) {
                *wi += xi * proj;
            }
        }
        // Gram-Schmidt gegen alle bereits gefundenen Komponenten.
        for u in deflate {
            let dot: f32 = w.iter().zip(u.iter()).map(|(a, b)| a * b).sum();
            for (wi, ui) in w.iter_mut().zip(u.iter()) {
                *wi -= dot * ui;
            }
        }
        normalize_vec(&mut w);
        v = w;
    }
    v
}

/// L2-Normierung in-place (Null-Vektor bleibt Null).
fn normalize_vec(v: &mut [f32]) {
    let n = v.iter().map(|x| x * x).sum::<f32>().sqrt();
    if n > 1e-12 {
        for x in v {
            *x /= n;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{Embedding512, Exemplar, PersonRecord};

    fn unit(idx: usize) -> Embedding512 {
        let mut v = [0.0f32; EMBED_DIM];
        v[idx] = 1.0;
        Embedding512 { v }
    }

    fn db_with(persons: &[(u32, &[usize])]) -> FaceDatabase {
        let mut db = FaceDatabase::new();
        let recs: Vec<PersonRecord> = persons
            .iter()
            .map(|(id, dims)| PersonRecord {
                id: *id,
                exemplars: dims
                    .iter()
                    .map(|&dz| Exemplar {
                        embedding: unit(dz),
                        thumbnail: vec![
                            (*id as u8).wrapping_add(dz as u8);
                            CROP_SIZE * CROP_SIZE * 3
                        ],
                    })
                    .collect(),
            })
            .collect();
        db.replace_persons(recs, persons.len() as u32);
        db
    }

    #[test]
    fn flatten_counts_and_shapes() {
        let db = db_with(&[(0, &[0, 1]), (1, &[2])]);
        let ld = LatentData::from_database(&db);
        assert_eq!(ld.len(), 3);
        assert_eq!(ld.data.dim(), (3, EMBED_DIM));
        assert_eq!(ld.thumbs.len(), 3);
        assert_eq!(ld.n_persons, 2);
        assert_eq!(ld.meta[0].person_id, 0);
        assert_eq!(ld.meta[2].person_id, 1);
    }

    #[test]
    fn knn_orthonormal_all_equidistant() {
        // Drei orthonormale Vektoren: paarweise dot=0 → Distanz=1.
        let db = db_with(&[(0, &[0]), (1, &[1]), (2, &[2])]);
        let ld = LatentData::from_database(&db);
        let (idx, dist) = cosine_knn(&ld.data, 2);
        assert_eq!(idx.dim(), (3, 2));
        for d in dist.iter() {
            assert!((d - 1.0).abs() < 1e-5, "orthonormal → Distanz 1, war {d}");
        }
    }

    #[test]
    fn knn_identical_neighbor_zero_distance() {
        // Zwei identische + ein orthogonaler: der nächste Nachbar hat Distanz 0.
        let db = db_with(&[(0, &[0]), (1, &[0]), (2, &[1])]);
        let ld = LatentData::from_database(&db);
        let (idx, dist) = cosine_knn(&ld.data, 1);
        // Punkt 0 und 1 sind identisch → gegenseitig nächster Nachbar, Distanz 0.
        assert!((dist[[0, 0]]).abs() < 1e-5);
        assert_eq!(idx[[0, 0]], 1);
        assert!((dist[[1, 0]]).abs() < 1e-5);
        assert_eq!(idx[[1, 0]], 0);
    }

    #[test]
    fn pca_init_separates_two_clusters() {
        // Zwei klar getrennte Richtungen → PCA-Achse 0 trennt sie im Vorzeichen.
        let db = db_with(&[(0, &[0]), (0, &[0]), (1, &[1]), (1, &[1])]);
        let ld = LatentData::from_database(&db);
        let init = pca_init_2d(&ld.data);
        assert_eq!(init.dim(), (4, 2));
        let s0 = init[[0, 0]].signum();
        // Punkte gleicher Gruppe auf gleicher Seite, andere Gruppe gegenüber.
        assert_eq!(init[[1, 0]].signum(), s0);
        assert_eq!(init[[2, 0]].signum(), -s0);
        assert_eq!(init[[3, 0]].signum(), -s0);
    }

    #[test]
    fn project_into_2d_lands_on_identical_exemplar() {
        // Drei orthonormale Exemplare mit bekannten 2D-Positionen. Eine Query
        // identisch zu Exemplar 1 muss (nahezu) auf dessen Position landen.
        let db = db_with(&[(0, &[0]), (1, &[1]), (2, &[2])]);
        let ld = LatentData::from_database(&db);
        let layout = [[10.0, 0.0], [0.0, 10.0], [-10.0, 0.0]];
        let mut q = [0.0f32; EMBED_DIM];
        q[1] = 1.0; // identisch zu Exemplar 1
        let (p, s) = project_into_2d(&ld.data, &layout, &q, 3);
        assert!((s - 1.0).abs() < 1e-5, "S_max sollte 1 sein, war {s}");
        assert!(
            (p[0] - 0.0).abs() < 0.5 && (p[1] - 10.0).abs() < 0.5,
            "erwartete ~[0,10], war {p:?}"
        );
    }

    #[test]
    fn project_into_2d_interpolates_between_neighbors() {
        // Query zwischen zwei gleich ähnlichen Exemplaren → Position dazwischen.
        let mut e0 = [0.0f32; EMBED_DIM];
        e0[0] = 1.0;
        let mut e1 = [0.0f32; EMBED_DIM];
        e1[1] = 1.0;
        let data = Array2::from_shape_vec(
            (2, EMBED_DIM),
            e0.iter().chain(e1.iter()).copied().collect(),
        )
        .unwrap();
        let layout = [[0.0, 0.0], [10.0, 0.0]];
        // 45°-Query: gleich ähnlich zu beiden.
        let mut q = [0.0f32; EMBED_DIM];
        q[0] = std::f32::consts::FRAC_1_SQRT_2;
        q[1] = std::f32::consts::FRAC_1_SQRT_2;
        let (p, _) = project_into_2d(&data, &layout, &q, 2);
        assert!((p[0] - 5.0).abs() < 0.5, "erwartete Mitte ~5, war {}", p[0]);
    }

    #[test]
    fn project_into_2d_empty_reference_is_safe() {
        let empty = Array2::<f32>::zeros((0, EMBED_DIM));
        let (p, s) = project_into_2d(&empty, &[], &[0.0; EMBED_DIM], 3);
        assert_eq!(p, [0.0, 0.0]);
        assert!(s.is_infinite() && s < 0.0);
    }

    #[test]
    fn thumb_scaled_rgba_dims_and_alpha() {
        let t = vec![7u8; CROP_SIZE * CROP_SIZE * 3];
        let out = thumb_scaled_rgba(&t, 32);
        assert_eq!(out.len(), 32 * 32 * 4);
        assert_eq!(out[3], 255);
        assert_eq!(out[0], 7);
    }

    /// Baut ein normiertes Embedding nahe an Basisrichtung `axis` mit kleinem
    /// Jitter auf `axis+1`, sodass Punkte gleicher `axis` einen dichten Cluster
    /// bilden und Punkte anderer `axis` klar getrennt liegen.
    fn near(axis: usize, jitter: f32) -> Embedding512 {
        let mut v = [0.0f32; EMBED_DIM];
        v[axis] = 1.0;
        v[(axis + 1) % EMBED_DIM] = jitter;
        let mut e = Embedding512 { v };
        e.normalize();
        e
    }

    fn db_clusters() -> FaceDatabase {
        // Zwei dichte Cluster (Achse 0 und Achse 5) + ein Ausreißer (Achse 200).
        let mut recs = Vec::new();
        for (id, axis) in [(0u32, 0usize), (1, 5)] {
            let exemplars = (0..6)
                .map(|j| Exemplar {
                    embedding: near(axis, 0.01 * j as f32),
                    thumbnail: vec![id as u8; CROP_SIZE * CROP_SIZE * 3],
                })
                .collect();
            recs.push(PersonRecord { id, exemplars });
        }
        recs.push(PersonRecord {
            id: 2,
            exemplars: vec![Exemplar {
                embedding: near(200, 0.0),
                thumbnail: vec![2u8; CROP_SIZE * CROP_SIZE * 3],
            }],
        });
        let mut db = FaceDatabase::new();
        db.replace_persons(recs, 3);
        db
    }

    #[test]
    fn hdbscan_finds_two_clusters_and_noise() {
        let db = db_clusters();
        let ld = LatentData::from_database(&db);
        let res = hdbscan_cluster(&ld.data, 3);
        assert_eq!(res.labels.len(), ld.len());
        // Zwei dichte Gruppen à 6 Punkte werden gefunden.
        assert_eq!(res.n_clusters, 2, "labels={:?}", res.labels);
        // Der einzelne Ausreißer (letzter Punkt) ist Rauschen.
        assert_eq!(*res.labels.last().unwrap(), -1);
        // Die beiden 6er-Gruppen tragen je ein einheitliches, echtes Label.
        assert!(
            res.labels[..6]
                .iter()
                .all(|&l| l >= 0 && l == res.labels[0])
        );
        assert!(
            res.labels[6..12]
                .iter()
                .all(|&l| l >= 0 && l == res.labels[6])
        );
        assert_ne!(res.labels[0], res.labels[6]);
        assert!(res.noise_ratio > 0.0 && res.noise_ratio < 0.2);
    }

    #[test]
    fn hdbscan_too_few_points_all_noise() {
        let db = db_with(&[(0, &[0]), (1, &[1])]);
        let ld = LatentData::from_database(&db);
        let res = hdbscan_cluster(&ld.data, 3);
        assert_eq!(res.n_clusters, 0);
        assert_eq!(res.labels, vec![-1, -1]);
    }

    #[test]
    fn umap_embed_returns_requested_dims() {
        let db = db_clusters();
        let ld = LatentData::from_database(&db);
        for nc in [2usize, 4, 8] {
            let e = umap_embed(&ld.data, 10, nc);
            assert_eq!(e.dim(), (ld.len(), nc), "n_components={nc}");
            assert!(e.iter().all(|v| v.is_finite()));
        }
    }

    #[test]
    fn pca_init_multidim_axes_are_orthogonal() {
        let db = db_clusters();
        let ld = LatentData::from_database(&db);
        let init = pca_init(&ld.data, 4);
        assert_eq!(init.dim(), (ld.len(), 4));
        // Achsen 0 und 1 sind (näherungsweise) unkorreliert über die Punkte.
        let n = ld.len();
        let col = |c: usize| -> Vec<f32> { (0..n).map(|i| init[[i, c]]).collect() };
        let (a, b) = (col(0), col(1));
        let dot: f32 = a.iter().zip(b.iter()).map(|(x, y)| x * y).sum();
        assert!(
            dot.abs() < 5.0,
            "Achsen sollten fast orthogonal sein, dot={dot}"
        );
    }
}
