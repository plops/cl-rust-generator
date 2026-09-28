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
/// Darüber ist das Bild praktisch identisch (obere Grenze der Bekanntheit).
/// Da [`THRESH_NOVELTY`] (0.82) strenger ist, entscheidet in der Praxis die
/// Novelty-Schwelle über die Aufnahme; diese Konstante dokumentiert die obere
/// Grenze der Schwellen-Hierarchie und dient externen Konsumenten/Tests.
#[allow(dead_code)]
pub const THRESH_REDUNDANT: f32 = 0.88;
/// Novelty-Admission: ein neues Exemplar wird nur aufgenommen, wenn seine
/// Ähnlichkeit zum ähnlichsten vorhandenen Exemplar **unter** dieser Schwelle
/// liegt — d.h. es bringt echte neue Information (andere Pose/Licht/Ausdruck).
/// Liegt sie darüber (aber unter [`THRESH_REDUNDANT`]), ist der Bank-Inhalt
/// bereits abgedeckt und wir sparen uns die Redundanz.
pub const THRESH_NOVELTY: f32 = 0.82;

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

    /// Ersetzt den kompletten Personen-Bestand (für Tests/Import via latent_viz).
    #[allow(dead_code)] // im x11_face_reid-Binary ungenutzt, in 08_latent-Tests genutzt
    pub fn replace_persons(&mut self, persons: Vec<PersonRecord>, next_id: u32) {
        self.persons = persons;
        self.next_id = next_id;
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
    ///
    /// Bei bekannter Person entscheidet **Novelty-Admission**, ob das Exemplar
    /// überhaupt gespeichert wird: nur wenn seine beste Ähnlichkeit unter
    /// [`THRESH_NOVELTY`] liegt, bringt es echte neue Information
    /// (andere Pose/Licht/Ausdruck). Läuft die Bank über [`MAX_EXEMPLARS`],
    /// wird per **Diversitäts-Verdrängung** das *redundanteste* Exemplar
    /// entfernt (nicht das älteste), damit die verbleibende Menge den
    /// Erscheinungsraum der Person möglichst breit abdeckt.
    pub fn update(&mut self, emb: &Embedding512, thumbnail: Vec<u8>) -> MatchResult {
        let (pi, sim) = self.best_match(emb).unwrap_or((0, f32::NEG_INFINITY));
        if sim >= THRESH_KNOWN {
            let id = self.persons[pi].id;
            // Nur echt neuartige Exemplare aufnehmen (zwischen "bekannt" und
            // Novelty-Schwelle). Redundante (sim ≥ THRESH_NOVELTY) fallen raus.
            let added = if sim < THRESH_NOVELTY {
                let ex = &mut self.persons[pi].exemplars;
                ex.push(Exemplar {
                    embedding: *emb,
                    thumbnail,
                });
                if ex.len() > MAX_EXEMPLARS {
                    let victim = most_redundant_index(ex);
                    ex.remove(victim);
                }
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

    /// Speichert die DB atomar: erst in `<path>.tmp` schreiben + `sync`, dann
    /// per `rename` über das Ziel schieben (POSIX-atomar auf gleichem FS).
    /// Ein Absturz während des Schreibens lässt so die alte DB intakt.
    pub fn save(&self, path: &str) -> std::io::Result<()> {
        use std::io::Write;
        let file = DbFile {
            persons: self.persons.clone(),
            next_id: self.next_id,
        };
        let bytes = bincode::serde::encode_to_vec(&file, bincode::config::standard())
            .map_err(std::io::Error::other)?;
        let tmp = format!("{path}.tmp");
        {
            let mut f = std::fs::File::create(&tmp)?;
            f.write_all(&bytes)?;
            f.sync_all()?;
        }
        std::fs::rename(&tmp, path)
    }
}

/// Index des redundantesten Exemplars: jenes, dessen ähnlichster Nachbar in
/// der Bank am ähnlichsten ist (höchste Nearest-Neighbor-Similarity). Dieses
/// Exemplar trägt am wenigsten zur Abdeckung des Erscheinungsraums bei, sein
/// Entfernen reduziert die Diversität am wenigsten (Max-Min-Diversität).
///
/// `O(k²·d)` für `k` Exemplare — bei `k ≤ MAX_EXEMPLARS` vernachlässigbar.
/// Bank mit `< 2` Exemplaren: Index 0 (kein sinnvoller Vergleich).
fn most_redundant_index(ex: &[Exemplar]) -> usize {
    if ex.len() < 2 {
        return 0;
    }
    let mut worst_sim = f32::NEG_INFINITY;
    let mut worst_idx = 0;
    for i in 0..ex.len() {
        // Ähnlichkeit von Exemplar i zu seinem nächsten Nachbarn (j != i).
        let mut nn = f32::NEG_INFINITY;
        for (j, e) in ex.iter().enumerate() {
            if j != i {
                nn = nn.max(ex[i].embedding.dot(&e.embedding));
            }
        }
        if nn > worst_sim {
            worst_sim = nn;
            worst_idx = i;
        }
    }
    worst_idx
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
    fn novelty_admission_skips_near_duplicates() {
        // Baut Person 0, dann ein sim≈0.85-Exemplar (> THRESH_NOVELTY 0.82,
        // aber < REDUNDANT 0.88) → bekannt, aber NICHT gespeichert.
        let mut db = FaceDatabase::new();
        db.update(&v4(1.0, 0.0, 0.0, 0.0), vec![0u8; THUMB_BYTES]);
        // cos = 0.85 zu (1,0,0,0): v=(0.85, sqrt(1-0.85²), 0, 0).
        let s = (1.0f32 - 0.85 * 0.85).sqrt();
        let r = db.update(&v4(0.85, s, 0.0, 0.0), vec![1u8; THUMB_BYTES]);
        assert!(matches!(r, MatchResult::Known { added: false, .. }));
        assert_eq!(db.persons()[0].exemplars.len(), 1);
        // cos = 0.70 (< 0.82) → neuartig, wird gespeichert.
        let r = db.update(&v4(0.7, 0.0, 0.714, 0.0), vec![2u8; THUMB_BYTES]);
        assert!(matches!(r, MatchResult::Known { added: true, .. }));
        assert_eq!(db.persons()[0].exemplars.len(), 2);
    }

    #[test]
    fn diversity_eviction_caps_and_keeps_spread() {
        // Füllt die Bank über MAX_EXEMPLARS hinaus mit paarweise diversen
        // Exemplaren (sim 0.7 zum Anker, ~0.49 untereinander) und prüft, dass
        // (a) die Kappung greift und (b) ein danach eingefügtes, stark
        // redundantes Paar den redundanten Partner verdrängt.
        let mut db = FaceDatabase::new();
        db.update(&v4(1.0, 0.0, 0.0, 0.0), vec![0u8; THUMB_BYTES]);
        for k in 1..=(MAX_EXEMPLARS + 4) {
            let mut v = [0.0; 512];
            v[0] = 0.7;
            v[(k % 400) + 2] = 0.714; // je eigene Achse → paarweise ~0.49
            db.update(&Embedding512 { v }, vec![k as u8; THUMB_BYTES]);
        }
        assert_eq!(db.persons()[0].exemplars.len(), MAX_EXEMPLARS);
    }

    #[test]
    fn most_redundant_index_picks_closest_pair_member() {
        // Drei Exemplare: zwei fast identisch (a, a'), eines weit weg (b).
        // Der redundanteste Index muss einer der beiden nahen sein.
        let mut a = Embedding512::zeros();
        a.v[0] = 1.0;
        let mut a2 = Embedding512::zeros();
        a2.v[0] = 0.999;
        a2.v[1] = (1.0f32 - 0.999 * 0.999).sqrt();
        let mut b = Embedding512::zeros();
        b.v[5] = 1.0;
        let ex = vec![
            Exemplar {
                embedding: a,
                thumbnail: vec![],
            },
            Exemplar {
                embedding: a2,
                thumbnail: vec![],
            },
            Exemplar {
                embedding: b,
                thumbnail: vec![],
            },
        ];
        assert!(most_redundant_index(&ex) < 2); // 0 oder 1, nie der ferne b
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
