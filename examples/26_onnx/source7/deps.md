# deps.md — 20260926_01_face

Registry für DeepWiki-Abfragen: GitHub-Notation `<organization>/<projekt>`.
Nur direkt genutzte Deps. Stand: 2026-09-26.

| Crate/System | Org/Projekt bzw. Paket | Version (Stand) | Zweck hier |
|---|---|---|---|
| ort (+ `cuda`-Feature optional) | pykeio/ort | 2.0.0-rc.13 | SCRFD- + ArcFace-Sessions, EP CUDA→CPU |
| x11rb | psychon/x11rb | 0.14.0 | Root-Grab 640×640 Z-Pixmap |
| macroquad (no-default) | not-fl3/macroquad | 0.4.16 | Fenster 820×640, Feed + Sidebar/HUD |
| serde | serde-rs/serde | 1.0.x | DB-Schemata (`Exemplar`, `PersonRecord`) |
| bincode | bincode-org/bincode | 2.0.x | `faces_db.bin`-Persistenz |
| umap-rs (import `umap_rs`) | wilsonzlin/umap-rs | 0.4.5 | 512D→2D Latent-Space-Reduktion (Binary `latent_viz`) |
| ndarray | rust-ndarray/ndarray | 0.17 | Matrizen für UMAP (`data`/`knn`/`init`) |
| hdbscan (Feature `parallel`) | tom-whitehead/hdbscan | 0.12.0 | dichtebasiertes Clustering der Embeddings (euklidisch == Kosinus auf Einheitskugel) |
| Xvfb | system (apt) | system | Headless-Tests via `xvfb-run` |
| det_500m.onnx / w600k_mbf.onnx | yakhyo/face-reidentification (Release v0.0.1) | — | Modelle per `download_models.sh` (curl, nicht im Git) |

Referenz-Repos (keine Build-Deps, nur Algorithmus-Vorbild):
`deepinsight/insightface` (ArcFace-Template, Umeyama),
`yakhyo/face-reidentification` (SCRFD-Decode, ArcFace-Pipeline).
Repo-Kontext: `plops/cl-rust-generator`.

DeepWiki-Abfrage-Muster: `yakhyo/face-reidentification` (SCRFD-Anker/NMS),
`deepinsight/insightface` (ArcFace-Template/Embedding).

NICHT eingeführt (bewusst): FAISS/C++-VDB (Prompt-Verbot),
`nalgebra`/`image` (Alignment/NMS ist Handcode; KNN/PCA für UMAP ebenfalls
handgeschrieben in `08_latent.rs`), `clap` (CLI ist `std::env`-Handparse),
Audio/ALSA (macroquad no-default). `ndarray` kam allein für `umap-rs` hinzu.
