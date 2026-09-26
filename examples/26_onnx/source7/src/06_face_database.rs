//! `06_face_database` — Exemplar-Store, Dot-Similarity, Clustering, Bincode-I/O.
//!
//! Schwellen aus dem Prompt: ≥0.65 bekannt (0.65–0.88 Exemplar anhängen,
//! >0.88 redundant), <0.45 neu, 0.45–0.65 ambig (nur tracken, kein DB-Eintrag).

use crate::types::{Embedding512, Exemplar, MAX_EXEMPLARS, PersonRecord};
use serde::{Deserialize, Serialize};

/// Ab hier gilt ein Match als bekannte Person.
pub const THRESH_KNOWN: f32 = 0.65;
/// Darunter wird eine neue Person angelegt.
pub const THRESH_NEW: f32 = 0.45;
/// Darüber ist das Bild redundant (kein neues Exemplar).
pub const THRESH_REDUNDANT: f32 = 0.88;

/// Ergebnis eines DB-Updates für ein Embedding.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MatchResult {
    /// Bekannte Person; `added` ob ein Exemplar angehängt wurde.
    Known {
        /// Personen-ID.
        id: u32,
        /// Beste Similarity.
        sim: f32,
        /// Neues Exemplar gespeichert.
        added: bool,
    },
    /// Neue Person angelegt.
    New {
        /// Vergebene ID.
        id: u32,
    },
    /// Ambivalenz-Bereich: nur tracken, nichts speichern.
    Ambiguous {
        /// Beste Similarity.
        sim: f32,
    },
}

impl MatchResult {
    /// Zugeordnete Personen-ID (`None` nur bei Ambiguität).
    #[must_use]
    pub fn person_id(&self) -> Option<u32> {
        match *self {
            Self::Known { id, .. } | Self::New { id } => Some(id),
            Self::Ambiguous { .. } => None,
        }
    }
}

/// Datei-Schema für `faces_db.bin`.
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
struct DbFile {
    persons: Vec<PersonRecord>,
    next_id: u32,
}

/// Exemplar-Bank mit Online-Clustering und Persistenz.
#[derive(Debug, Clone, Default)]
pub struct FaceDatabase {
    persons: Vec<PersonRecord>,
    next_id: u32,
}

impl FaceDatabase {
    /// Leere Datenbank.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Alle Personen (für Galerie/HUD).
    #[must_use]
    pub fn persons(&self) -> &[PersonRecord] {
        &self.persons
    }

    /// Anzahl gespeicherter Exemplare insgesamt.
    #[must_use]
    pub fn exemplar_count(&self) -> usize {
        self.persons.iter().map(|p| p.exemplars.len()).sum()
    }

    /// Beste Similarity über alle Exemplare (`None` bei leerer DB).
    pub fn best_match(&self, emb: &Embedding512) -> Option<(usize, f32)> {
        let mut best: Option<(usize, f32)> = None;
        for (pi, p) in self.persons.iter().enumerate() {
            for ex in &p.exemplars {
                let s = emb.dot(&ex.embedding);
                if best.is_none_or(|(_, b)| s > b) {
                    best = Some((pi, s));
                }
            }
        }
        best
    }

    /// Ordnet ein Embedding zu und pflegt ggf. ein Exemplar ein.
    pub fn update(&mut self, emb: &Embedding512, thumbnail: Vec<u8>) -> MatchResult {
        let (pi, sim) = self.best_match(emb).unwrap_or((0, f32::NEG_INFINITY));
        if sim >= THRESH_KNOWN {
            let id = self.persons[pi].id;
            let added = if sim <= THRESH_REDUNDANT {
                let ex = &mut self.persons[pi].exemplars;
                if ex.len() >= MAX_EXEMPLARS {
                    ex.remove(0); // FIFO: ältestes Exemplar verdrängen
                }
                ex.push(Exemplar {
                    embedding: *emb,
                    thumbnail,
                });
                true
            } else {
                false
            };
            MatchResult::Known { id, sim, added }
        } else if sim < THRESH_NEW {
            let id = self.next_id;
            self.next_id += 1;
            self.persons.push(PersonRecord {
                id,
                exemplars: vec![Exemplar {
                    embedding: *emb,
                    thumbnail,
                }],
            });
            MatchResult::New { id }
        } else {
            MatchResult::Ambiguous { sim }
        }
    }

    /// Lädt die DB (fehlende Datei → leere DB).
    pub fn load(path: &str) -> Self {
        std::fs::read(path).map_or_else(
            |_| Self::new(),
            |bytes| {
                bincode::serde::decode_from_slice::<DbFile, _>(&bytes, bincode::config::standard())
                    .map_or_else(
                        |_| Self::new(),
                        |(f, _)| Self {
                            persons: f.persons,
                            next_id: f.next_id,
                        },
                    )
            },
        )
    }

    /// Speichert die DB atomar-via-Tempfile (einfach: direkt schreiben).
    pub fn save(&self, path: &str) -> std::io::Result<()> {
        let file = DbFile {
            persons: self.persons.clone(),
            next_id: self.next_id,
        };
        let bytes = bincode::serde::encode_to_vec(&file, bincode::config::standard())
            .map_err(std::io::Error::other)?;
        std::fs::write(path, bytes)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::THUMB_BYTES;

    /// Testvektor mit expliziten ersten 4 Komponenten (Rest 0).
    fn v4(a: f32, b: f32, c: f32, d: f32) -> Embedding512 {
        let mut v = [0.0; 512];
        v[0] = a;
        v[1] = b;
        v[2] = c;
        v[3] = d;
        Embedding512 { v }
    }

    #[test]
    fn thresholds_new_ambiguous_known() {
        let mut db = FaceDatabase::new();
        let thumb = || vec![1u8; THUMB_BYTES];
        // Leer → sim −inf < 0.45 → neu.
        assert!(matches!(
            db.update(&v4(1.0, 0.0, 0.0, 0.0), thumb()),
            MatchResult::New { id: 0 }
        ));
        // Fast identisch (sim 1.0 > 0.88) → bekannt, redundant.
        let r = db.update(&v4(1.0, 0.0, 0.0, 0.0), thumb());
        assert!(matches!(r, MatchResult::Known { added: false, .. }));
        assert_eq!(db.persons()[0].exemplars.len(), 1);
        // sim 0.7 zu a → bekannt + Exemplar.
        let r = db.update(&v4(0.7, 0.0, 0.714, 0.0), thumb());
        assert!(matches!(r, MatchResult::Known { added: true, .. }));
        assert_eq!(db.persons()[0].exemplars.len(), 2);
        // sim 0.5 zu a, 0.35 zu b → ambig, nichts gespeichert.
        let r = db.update(&v4(0.5, 0.0, 0.0, 0.866), thumb());
        assert!(matches!(r, MatchResult::Ambiguous { .. }));
        assert_eq!(db.exemplar_count(), 2);
        // sim 0.0 zu allen → neu.
        let r = db.update(&v4(0.0, 1.0, 0.0, 0.0), thumb());
        assert!(matches!(r, MatchResult::New { id: 1 }));
    }

    #[test]
    fn fifo_caps_at_five_exemplars() {
        let mut db = FaceDatabase::new();
        db.update(&v4(1.0, 0.0, 0.0, 0.0), vec![0u8; THUMB_BYTES]);
        for k in 2..9 {
            // sim 0.7 zu a, 0.49 untereinander → jeweils neues Exemplar.
            let mut v = [0.0; 512];
            v[0] = 0.7;
            v[k] = 0.714;
            db.update(&Embedding512 { v }, vec![k as u8; THUMB_BYTES]);
        }
        assert_eq!(db.persons()[0].exemplars.len(), MAX_EXEMPLARS);
    }

    #[test]
    fn bincode_roundtrip_preserves_thumbnails() {
        let mut db = FaceDatabase::new();
        db.update(&v4(1.0, 0.0, 0.0, 0.0), vec![9u8; THUMB_BYTES]);
        let dir = std::env::temp_dir().join("face_db_test.bin");
        db.save(dir.to_str().unwrap()).unwrap();
        let back = FaceDatabase::load(dir.to_str().unwrap());
        assert_eq!(back.persons(), db.persons());
        std::fs::remove_file(&dir).unwrap();
    }
}
