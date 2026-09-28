//! `10_live` — Live-Zufuhr für die Latent-Space-Trajektorie.
//!
//! Greift denselben Pfad wie `x11_face_reid` ab, aber ohne DB-Update: X11-Grab
//! → SCRFD-Detektion → Umeyama-Alignment → ArcFace-Embedding. Pro Frame liefert
//! [`LiveFeed::poll`] die aktuell sichtbaren Gesichter als
//! (L2-normiertes 512D-Embedding, 112×112-RGB-Crop). Der Aufrufer projiziert
//! diese Embeddings per [`crate::latent::project_into_2d`] in das statische
//! 2D-UMAP-Layout und hängt sie an die Trajektorie an.
//!
//! Dieses Modul braucht Kamera (X11) und beide ONNX-Modelle und wird nur im
//! `--live`-Pfad instanziiert; der statische Viewer bleibt davon unberührt
//! (und damit headless ohne Modelle lauffähig).

use crate::align::align_face;
use crate::arcface::ArcfaceEmbed;
use crate::capture::{CAPTURE_SIZE, ScreenCapture};
use crate::scrfd::ScrfdDetector;
use crate::types::{BBox, Embedding512, Landmarks5};

/// Ein live erfasstes Gesicht eines Frames (noch nicht projiziert).
pub struct LiveFace {
    /// L2-normiertes ArcFace-Embedding.
    pub embedding: Embedding512,
    /// Alignter 112×112-RGB-Crop (für die Live-Vorschau).
    pub crop: Vec<u8>,
    /// Bounding Box im 640er-Feed (optionale Zusatzanzeige).
    pub bbox: BBox,
    /// Landmarks im 640er-Feed.
    pub landmarks: Landmarks5,
}

/// Kapselt Capture + Detektor + Embedder für den Live-Pfad.
pub struct LiveFeed {
    capture: ScreenCapture,
    detector: ScrfdDetector,
    embedder: ArcfaceEmbed,
    /// Aktiver Execution Provider (fürs HUD).
    pub provider: &'static str,
}

impl LiveFeed {
    /// Baut den Live-Pfad: X11-Verbindung + beide ONNX-Sessions (EP-Fallback).
    ///
    /// `models_dir` enthält `det_500m.onnx` und `w600k_mbf.onnx`. Fehler beim
    /// X11-Connect oder fehlende Modelle liefert der Aufrufer als klaren Abbruch.
    pub fn open(models_dir: &str, conf: f32) -> Result<Self, String> {
        let det_path = format!("{models_dir}/det_500m.onnx");
        let emb_path = format!("{models_dir}/w600k_mbf.onnx");
        for p in [&det_path, &emb_path] {
            if !std::path::Path::new(p).exists() {
                return Err(format!("Modell fehlt: {p} — erst ./download_models.sh"));
            }
        }
        let capture = ScreenCapture::connect().map_err(|e| format!("X11-Fehler: {e}"))?;
        let mut detector = ScrfdDetector::open(&det_path);
        detector.conf_thres = conf;
        let provider = detector.provider;
        let embedder = ArcfaceEmbed::open(&emb_path);
        Ok(Self {
            capture,
            detector,
            embedder,
            provider,
        })
    }

    /// Erfasst einen Frame und liefert die aktuell sichtbaren Gesichter.
    ///
    /// Reine Read-Only-Inferenz — kein DB-Zugriff, kein State außer den
    /// Sessions. Leerer Vektor, wenn kein Gesicht sichtbar ist.
    pub fn poll(&mut self) -> Vec<LiveFace> {
        let rgb = self.capture.capture_rgb();
        let mut out = Vec::new();
        for det in self.detector.detect(&rgb) {
            let crop = align_face(&rgb, CAPTURE_SIZE, CAPTURE_SIZE, &det.landmarks);
            let embedding = self.embedder.embed(&crop);
            out.push(LiveFace {
                embedding,
                crop,
                bbox: det.bbox,
                landmarks: det.landmarks,
            });
        }
        out
    }
}
