//! `01_types` — Schemata: BBox, Landmarks5, Embedding512, Exemplar, PersonRecord.
//!
//! Reine Typen + Serde-Schemata für die DB. Keine X11-/ONNX-Abhängigkeit,
//! daher ohne Hardware/Modell testbar.

use serde::{Deserialize, Serialize};

/// Kantenlänge des alignten Gesichts-Crops (ArcFace-Standard).
pub const CROP_SIZE: usize = 112;
/// Bytes pro Thumbnail (`112*112*3`, RGB interleaved).
pub const THUMB_BYTES: usize = CROP_SIZE * CROP_SIZE * 3;
/// Dimension des ArcFace-Embeddings.
pub const EMBED_DIM: usize = 512;
/// Max. Exemplare pro Person (FIFO bei Überlauf).
pub const MAX_EXEMPLARS: usize = 5;

/// Achsenparallele Box mit Konfidenz im Detektor-Koordinatensystem.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BBox {
    /// Linke Kante in px.
    pub x1: f32,
    /// Obere Kante in px.
    pub y1: f32,
    /// Rechte Kante in px.
    pub x2: f32,
    /// Untere Kante in px.
    pub y2: f32,
    /// Detektor-Konfidenz in `[0, 1]`.
    pub score: f32,
}

impl BBox {
    /// Breite in px (kann ≤0 sein bei degenerierten Boxen).
    #[must_use]
    pub fn width(&self) -> f32 {
        self.x2 - self.x1
    }

    /// Höhe in px.
    #[must_use]
    pub fn height(&self) -> f32 {
        self.y2 - self.y1
    }
}

/// Fünf Gesichts-Landmarks: Augen links/rechts, Nase, Mundwinkel links/rechts.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Landmarks5 {
    /// Punkte als `(x, y)` im Detektor-Koordinatensystem.
    pub points: [[f32; 2]; 5],
}

/// L2-normalisierter 512D-Feature-Vektor (Cosine = Dot-Product).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Embedding512 {
    /// Komponenten, nach `normalize` mit Norm 1 (außer Null-Vektor).
    pub v: [f32; EMBED_DIM],
}

// Manuelle Serde-Impls: serde implementiert Arrays nur bis Länge 32.
impl Serialize for Embedding512 {
    fn serialize<S: serde::Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        use serde::ser::SerializeSeq;
        let mut seq = s.serialize_seq(Some(EMBED_DIM))?;
        for x in &self.v {
            seq.serialize_element(x)?;
        }
        seq.end()
    }
}

impl<'de> Deserialize<'de> for Embedding512 {
    fn deserialize<D: serde::Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        use serde::de::{SeqAccess, Visitor};
        struct V;
        impl<'v> Visitor<'v> for V {
            type Value = Embedding512;
            fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "512 f32-Komponenten")
            }
            fn visit_seq<A: SeqAccess<'v>>(self, mut a: A) -> Result<Embedding512, A::Error> {
                use serde::de::Error;
                let mut v = [0.0f32; EMBED_DIM];
                for x in &mut v {
                    *x = a
                        .next_element()?
                        .ok_or_else(|| Error::invalid_length(EMBED_DIM, &self))?;
                }
                Ok(Embedding512 { v })
            }
        }
        d.deserialize_seq(V)
    }
}

impl Embedding512 {
    /// Null-Vektor (nicht normiert; `normalize` lässt ihn unverändert).
    #[must_use]
    pub fn zeros() -> Self {
        Self {
            v: [0.0; EMBED_DIM],
        }
    }

    /// L2-Norm des Vektors.
    #[must_use]
    pub fn norm(&self) -> f32 {
        self.v.iter().map(|x| x * x).sum::<f32>().sqrt()
    }

    /// Skaliert auf Norm 1; Null-Vektor bleibt Null (kein NaN).
    pub fn normalize(&mut self) {
        let n = self.norm();
        if n > 1e-12 {
            for x in &mut self.v {
                *x /= n;
            }
        }
    }

    /// Dot-Product; bei normierten Vektoren = Cosine-Similarity.
    #[must_use]
    pub fn dot(&self, other: &Self) -> f32 {
        self.v.iter().zip(other.v.iter()).map(|(a, b)| a * b).sum()
    }
}

/// Ein Repräsentant einer Person: Embedding + zugehöriger RGB-Thumbnail.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Exemplar {
    /// L2-normalisiertes Embedding.
    pub embedding: Embedding512,
    /// `112*112*3` RGB-Bytes des alignten Crops.
    pub thumbnail: Vec<u8>,
}

/// Alle Exemplare einer Person unter stabiler ID.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct PersonRecord {
    /// Laufende Personen-ID (ab 0).
    pub id: u32,
    /// Repräsentanten, älteste zuerst (max. [`MAX_EXEMPLARS`]).
    pub exemplars: Vec<Exemplar>,
}

/// Eine Detektion: Box plus 5 Landmarks im gleichen Koordinatensystem.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct FaceDetection {
    /// Bounding Box mit Score.
    pub bbox: BBox,
    /// Zugehörige Landmarks.
    pub landmarks: Landmarks5,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bbox_width_height() {
        let b = BBox {
            x1: 1.0,
            y1: 2.0,
            x2: 5.0,
            y2: 5.0,
            score: 0.9,
        };
        assert_eq!((b.width(), b.height()), (4.0, 3.0));
    }

    #[test]
    fn embedding_normalize_unit_and_zero_safe() {
        let mut e = Embedding512 {
            v: [1.0; EMBED_DIM],
        };
        e.normalize();
        assert!((e.norm() - 1.0).abs() < 1e-5);
        let mut z = Embedding512::zeros();
        z.normalize();
        assert_eq!(z, Embedding512::zeros());
        assert!(z.dot(&z).is_finite());
    }

    #[test]
    fn dot_of_orthogonal_is_zero() {
        let mut a = Embedding512::zeros();
        let mut b = Embedding512::zeros();
        a.v[0] = 1.0;
        b.v[1] = 1.0;
        assert_eq!(a.dot(&b), 0.0);
        assert_eq!(a.dot(&a), 1.0);
    }

    #[test]
    fn person_record_serde_roundtrip() {
        let rec = PersonRecord {
            id: 7,
            exemplars: vec![Exemplar {
                embedding: Embedding512 {
                    v: [0.5; EMBED_DIM],
                },
                thumbnail: vec![3u8; THUMB_BYTES],
            }],
        };
        let bytes = bincode::serde::encode_to_vec(&rec, bincode::config::standard()).unwrap();
        let (back, _): (PersonRecord, usize) =
            bincode::serde::decode_from_slice(&bytes, bincode::config::standard()).unwrap();
        assert_eq!(rec, back);
    }
}
