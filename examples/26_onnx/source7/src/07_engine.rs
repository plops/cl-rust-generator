//! `07_engine` — Pipeline: Detect → Align → Embed → Match → DB-Update.
//!
//! Generisch über `Detector`/`Embedder`-Traits, damit Tests ohne ONNX-Modelle
//! mit Fakes laufen. Der aktive Provider-Name dient dem HUD.

use crate::align::align_face;
use crate::db::{FaceDatabase, MatchResult};
use crate::types::{Embedding512, FaceDetection};

/// Gesichtserkennung pro Frame (Echt: [`crate::scrfd::ScrfdDetector`]).
pub trait Detector {
    /// Detektiert Gesichter im RGB-Frame.
    fn detect(&mut self, rgb: &[u8]) -> Vec<FaceDetection>;
}

/// Embedding-Extraktion pro Crop (Echt: [`crate::arcface::ArcfaceEmbed`]).
pub trait Embedder {
    /// Extrahiert das L2-normierte Embedding eines 112-Crops.
    fn embed(&mut self, crop112_rgb: &[u8]) -> Embedding512;
}

impl Detector for crate::scrfd::ScrfdDetector {
    fn detect(&mut self, rgb: &[u8]) -> Vec<FaceDetection> {
        self.detect(rgb)
    }
}

impl Embedder for crate::arcface::ArcfaceEmbed {
    fn embed(&mut self, crop: &[u8]) -> Embedding512 {
        self.embed(crop)
    }
}

/// Gesicht mit Re-ID-Zuordnung und Align-Crop für die UI.
pub struct TrackedFace {
    /// Detektion (Box + Landmarks).
    pub detection: FaceDetection,
    /// Personen-ID (`None` = ambig, nur getrackt).
    pub person_id: Option<u32>,
    /// Beste DB-Similarity (−inf bei erster Person).
    pub sim: f32,
    /// Alignter 112×112-RGB-Crop.
    pub crop: Vec<u8>,
}

/// Pipeline-Orchestrierung mit DB und Provider-Name.
pub struct Engine<D, E> {
    detector: D,
    embedder: E,
    db: FaceDatabase,
    provider: &'static str,
}

impl<D: Detector, E: Embedder> Engine<D, E> {
    /// Baut die Engine (DB ggf. aus Datei laden übernimmt der Caller).
    pub fn new(detector: D, embedder: E, db: FaceDatabase, provider: &'static str) -> Self {
        Self {
            detector,
            embedder,
            db,
            provider,
        }
    }

    /// Aktiver Execution Provider fürs HUD.
    #[must_use]
    pub fn provider(&self) -> &'static str {
        self.provider
    }

    /// Datenbank (Galerie/HUD/Speichern).
    #[must_use]
    pub fn db(&self) -> &FaceDatabase {
        &self.db
    }

    /// Verarbeitet einen RGB-Frame (`w*h*3`) vollständig.
    pub fn process_frame(&mut self, rgb: &[u8], w: usize, h: usize) -> Vec<TrackedFace> {
        let mut out = Vec::new();
        for det in self.detector.detect(rgb) {
            let crop = align_face(rgb, w, h, &det.landmarks);
            let emb = self.embedder.embed(&crop);
            let res = self.db.update(&emb, crop.clone());
            let person_id = res.person_id();
            let sim = match res {
                MatchResult::Known { sim, .. } | MatchResult::Ambiguous { sim } => sim,
                MatchResult::New { .. } => f32::NEG_INFINITY,
            };
            out.push(TrackedFace {
                detection: det,
                person_id,
                sim,
                crop,
            });
        }
        out
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{BBox, Landmarks5};

    struct FakeDetector(Vec<FaceDetection>);
    impl Detector for FakeDetector {
        fn detect(&mut self, _rgb: &[u8]) -> Vec<FaceDetection> {
            self.0.clone()
        }
    }

    struct FakeEmbedder(Embedding512);
    impl Embedder for FakeEmbedder {
        fn embed(&mut self, _crop: &[u8]) -> Embedding512 {
            self.0
        }
    }

    fn det() -> FaceDetection {
        FaceDetection {
            bbox: BBox {
                x1: 10.0,
                y1: 10.0,
                x2: 100.0,
                y2: 100.0,
                score: 0.9,
            },
            landmarks: Landmarks5 {
                points: [
                    [30.0, 40.0],
                    [70.0, 40.0],
                    [50.0, 60.0],
                    [35.0, 80.0],
                    [65.0, 80.0],
                ],
            },
        }
    }

    fn emb(first: f32) -> Embedding512 {
        let mut v = [0.0; 512];
        v[0] = first;
        v[1] = (1.0 - first * first).max(0.0).sqrt();
        Embedding512 { v }
    }

    #[test]
    fn engine_assigns_new_then_known_id() {
        let rgb = vec![128u8; 640 * 640 * 3];
        let mut eng = Engine::new(
            FakeDetector(vec![det()]),
            FakeEmbedder(emb(1.0)),
            FaceDatabase::new(),
            "CPU",
        );
        let t1 = eng.process_frame(&rgb, 640, 640);
        assert_eq!(t1[0].person_id, Some(0));
        // Gleiches Embedding → wiedererkannt, redundant (>0.88).
        let t2 = eng.process_frame(&rgb, 640, 640);
        assert_eq!(t2[0].person_id, Some(0));
        assert_eq!(eng.db().exemplar_count(), 1);
        assert_eq!(eng.provider(), "CPU");
    }

    /// Lädt P6-PPM (PIL-Format `P6\nW H\nMAX\n`, ohne Kommentare).
    fn load_ppm6(path: &str) -> Vec<u8> {
        let b = std::fs::read(path).unwrap();
        let hdr = b"P6\n640 640\n255\n";
        assert!(b.starts_with(hdr), "unerwarteter PPM-Header in {path}");
        b[hdr.len()..].to_vec()
    }

    #[test]
    #[ignore = "braucht models/*.onnx + tests/assets/face640.ppm"]
    fn real_models_detect_and_reidentify_face() {
        // E2E über echte Sessions: Ross-Porträt (yakhyo/assets/faces).
        let base = env!("CARGO_MANIFEST_DIR");
        let rgb = load_ppm6(&format!("{base}/tests/assets/face640.ppm"));
        let det = crate::scrfd::ScrfdDetector::open(&format!("{base}/models/det_500m.onnx"));
        let prov = det.provider;
        let emb = crate::arcface::ArcfaceEmbed::open(&format!("{base}/models/w600k_mbf.onnx"));
        let mut eng = Engine::new(det, emb, FaceDatabase::new(), prov);
        let tracked = eng.process_frame(&rgb, 640, 640);
        // Genau ein Gesicht (doppelte Boxen würden auf NMS-Fehler deuten).
        assert_eq!(tracked.len(), 1);
        assert_eq!(tracked[0].person_id, Some(0));
        assert_eq!(eng.db().persons().len(), 1);
        // Zweiter Frame: gleiche ID, redundant (sim 1.0), kein Exemplar mehr.
        let again = eng.process_frame(&rgb, 640, 640);
        assert_eq!(again.len(), 1);
        assert_eq!(again[0].person_id, Some(0));
        assert_eq!(eng.db().exemplar_count(), 1);
        // Optionaler Benchmark: FACE_BENCH_FRAMES=N misst N Gesichts-Frames
        // (Detect+Align+Embed+DB, ohne X11/Render) und meldet ms/Frame.
        let bench: usize = std::env::var("FACE_BENCH_FRAMES")
            .ok()
            .and_then(|s| s.parse().ok())
            .unwrap_or(2);
        let t0 = std::time::Instant::now();
        for _ in 2..bench.max(2) {
            eng.process_frame(&rgb, 640, 640);
        }
        if bench > 2 {
            let ms = t0.elapsed().as_secs_f64() * 1000.0 / (bench - 2) as f64;
            eprintln!("bench: {bench} Gesichts-Frames, {ms:.2} ms/Frame (Provider: {prov})");
        }
    }

    #[test]
    fn engine_leaves_ambiguous_untracked_in_db() {
        let rgb = vec![128u8; 640 * 640 * 3];
        let mut eng = Engine::new(
            FakeDetector(vec![det()]),
            FakeEmbedder(emb(1.0)),
            FaceDatabase::new(),
            "CPU",
        );
        eng.process_frame(&rgb, 640, 640);
        // sim 0.5 → ambig: getrackt (Crop da), aber keine ID, kein Exemplar.
        eng.embedder = FakeEmbedder(emb(0.5));
        let t = eng.process_frame(&rgb, 640, 640);
        assert_eq!(t[0].person_id, None);
        assert_eq!(t[0].crop.len(), 112 * 112 * 3);
        assert_eq!(eng.db().exemplar_count(), 1);
    }
}
