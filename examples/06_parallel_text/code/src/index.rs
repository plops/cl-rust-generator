use byteorder::{LittleEndian, WriteBytesExt};
#[allow(unused_parens)]
use chrono::{DateTime, Utc};
use std::collections::HashMap;
fn tokenize(text: &str) -> Vec<&str> {
    return text
        .split(|ch: char| {
            return !(ch.is_alphanumeric());
        })
        .filter(|word| {
            return !(word.is_empty());
        })
        .collect();
}
pub struct InMemoryIndex {
    pub word_count: usize,
    pub map: HashMap<String, Vec<Hit>>,
}
pub type Hit = Vec<u8>;
impl InMemoryIndex {
    pub fn new() -> InMemoryIndex {
        return InMemoryIndex {
            word_count: 0,
            map: HashMap::new(),
        };
    }
    pub fn from_single_document(document_id: usize, text: String) -> InMemoryIndex {
        let document_id = document_id as u32;
        let mut index = InMemoryIndex::new();
        let text = text.to_lowercase();
        let tokens = tokenize(&text);
        for (i, token) in tokens.iter().enumerate() {
            let hits = index.map.entry(token.to_string()).or_insert_with(|| {
                let mut hits = Vec::with_capacity(4 + 4);
                hits.write_u32::<LittleEndian>(document_id).unwrap();
                return vec![hits];
            });
            hits[0].write_u32::<LittleEndian>(document_id).unwrap();
            index.word_count += 1;
        }
        if (0) == (document_id % 100) {
            println!(
                "indexed document {}, {} bytes, {} words",
                document_id,
                text.len(),
                index.word_count
            );
        };
        return index;
    }
    pub fn merge(&mut self, other: InMemoryIndex) {
        for (term, hits) in other.map {
            self.map
                .entry(term)
                .or_insert_with(|| {
                    return vec![];
                })
                .extend(hits);
        }
        self.word_count += other.word_count;
    }
    pub fn is_empty(&self) -> bool {
        return (0) == (self.word_count);
    }
    pub fn is_large(&self) -> bool {
        return 100_000_000 < self.word_count;
    }
}
