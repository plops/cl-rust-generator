//! `06_font` — Font-Stapel mit Glyphen-Fallback (Plan §2.5).
//!
//! macroquad zeichnet mit genau **einem** Font; fehlt ein Zeichen darin,
//! gibt es Tofu statt Ausweichen. Deshalb lädt der Client bis zu drei
//! Fonts gleichzeitig (Noto CJK → Noto → Unifont → Default) und zerlegt
//! jeden Text in Läufe gleicher Abdeckung: Jedes Zeichen kommt aus dem
//! ersten Font, dessen Cmap es enthält (`ttf-parser`, pro Zeichen einmal
//! geprüft und dann gecacht). `--font PATH` ersetzt nur die erste Stufe.

use std::collections::HashMap;

use macroquad::prelude::*;

/// Kandidaten-Dateien je Stufe (erste existierende gewinnt).
const NOTO_CJK: &[&str] = &[
    "/usr/share/fonts/opentype/noto/NotoSansCJK-Regular.otf",
    "/usr/share/fonts/truetype/noto/NotoSansCJK-Regular.otf",
];
const NOTO: &[&str] = &[
    "/usr/share/fonts/truetype/noto/NotoSans-Regular.ttf",
    "/usr/share/fonts/opentype/noto/NotoSans-Regular.otf",
];
const UNIFONT: &[&str] = &[
    "/usr/share/fonts/opentype/unifont/unifont.otf",
    "/usr/share/fonts/unifont/unifont.otf",
    "/usr/share/fonts/truetype/unifont/unifont.ttf",
];

/// Eine Stufe: macroquad-Font plus Abdeckungsnachweis.
struct Entry {
    name: String,
    font: Option<Font>,
    /// Explizite Abdeckung (nur Tests); sonst Cmap der `bytes`.
    listed: Option<Vec<char>>,
    bytes: Vec<u8>,
}

/// Font-Stapel; Slot `None` = macroquad-Default (letzte Rettung).
pub struct FontStack {
    entries: Vec<Entry>,
    cache: HashMap<char, Option<usize>>,
}

impl FontStack {
    /// Lädt den Stapel (`--font` zuerst, dann System-Fonts).
    #[must_use]
    pub fn load(path: Option<&str>) -> Self {
        let mut entries = Vec::new();
        let mut add = |name: &str, bytes: Vec<u8>| {
            let font = load_ttf_font_from_bytes(&bytes).ok();
            if font.is_none() {
                eprintln!("[font] {name}: lädt nicht, Stufe übersprungen");
            }
            entries.push(Entry {
                name: name.into(),
                font,
                listed: None,
                bytes,
            });
        };
        if let Some(p) = path {
            match std::fs::read(p) {
                Ok(b) => add(p, b),
                Err(e) => eprintln!("[font] {p}: {e} — nutze System-Fonts"),
            }
        }
        for (name, cands) in [("Noto-CJK", NOTO_CJK), ("Noto", NOTO), ("Unifont", UNIFONT)] {
            if let Some((p, b)) = cands
                .iter()
                .find_map(|p| std::fs::read(p).ok().map(|b| (*p, b)))
            {
                add(&format!("{name} ({p})"), b);
            }
        }
        if entries.is_empty() {
            eprintln!("[font] keine Fonts gefunden — nur Standardschrift");
        }
        Self {
            entries,
            cache: HashMap::new(),
        }
    }

    /// Test-Stapel mit expliziter Abdeckung (keine Dateien nötig).
    #[cfg(test)]
    fn for_test(covers: Vec<(&str, Vec<char>)>) -> Self {
        let entries = covers
            .into_iter()
            .map(|(name, listed)| Entry {
                name: name.into(),
                font: None,
                listed: Some(listed),
                bytes: Vec::new(),
            })
            .collect();
        Self {
            entries,
            cache: HashMap::new(),
        }
    }

    /// Enthält Stufe `i` das Zeichen?
    fn covers(&self, i: usize, c: char) -> bool {
        let e = &self.entries[i];
        if let Some(listed) = &e.listed {
            return listed.contains(&c);
        }
        ttf_parser::Face::parse(&e.bytes, 0)
            .ok()
            .and_then(|f| f.glyph_index(c))
            .is_some()
    }

    /// Slot für ein Zeichen: erste abdeckende Stufe, sonst Default (`None`).
    pub fn slot_for(&mut self, c: char) -> Option<usize> {
        if let Some(hit) = self.cache.get(&c) {
            return *hit;
        }
        let mut hit = None;
        for i in 0..self.entries.len() {
            // Echte Stufe: Font geladen UND Zeichen enthalten. Test-Stufen
            // tragen explizite Abdeckung ohne Font-Datei.
            let usable = self.entries[i].font.is_some() || self.entries[i].listed.is_some();
            if usable && self.covers(i, c) {
                hit = Some(i);
                break;
            }
        }
        self.cache.insert(c, hit);
        hit
    }

    /// Zerlegt Text in Läufe `(Slot, Teilstring)` gleicher Abdeckung.
    pub fn runs(&mut self, text: &str) -> Vec<(Option<usize>, String)> {
        let mut out: Vec<(Option<usize>, String)> = Vec::new();
        for c in text.chars() {
            let slot = self.slot_for(c);
            match out.last_mut() {
                Some((s, buf)) if *s == slot => buf.push(c),
                _ => out.push((slot, c.to_string())),
            }
        }
        out
    }

    /// macroquad-Font eines Slots (`None` = Default).
    #[must_use]
    pub fn font(&self, slot: Option<usize>) -> Option<&Font> {
        slot.and_then(|i| self.entries[i].font.as_ref())
    }

    /// Geladene Stufen (HUD/Debug).
    #[must_use]
    pub fn names(&self) -> Vec<&str> {
        self.entries.iter().map(|e| e.name.as_str()).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn stack() -> FontStack {
        FontStack::for_test(vec![
            ("noto", vec!['a', 'b', 'h', 'n', ' ']),
            ("unifont", vec!['a', 'b', 'h', 'n', ' ', '中', '한']),
        ])
    }

    #[test]
    fn runs_split_by_coverage() {
        let mut f = stack();
        // "a" aus Stufe 0, "中" erst aus Stufe 1, "?" aus keiner.
        let runs = f.runs("a中?");
        assert_eq!(
            runs.iter()
                .map(|(s, t)| (*s, t.as_str()))
                .collect::<Vec<_>>(),
            vec![(Some(0), "a"), (Some(1), "中"), (None, "?")]
        );
    }

    #[test]
    fn mixed_cjk_latin_splits_minimally() {
        let mut f = stack();
        let runs = f.runs("ab中h?n");
        // "ab" Lauf 0, "中" Lauf 1, "h" Lauf 0, "?" Default, "n" Lauf 0.
        assert_eq!(
            runs,
            vec![
                (Some(0), "ab".into()),
                (Some(1), "中".into()),
                (Some(0), "h".into()),
                (None, "?".into()),
                (Some(0), "n".into()),
            ]
        );
    }

    #[test]
    fn coverage_is_cached() {
        let mut f = stack();
        assert_eq!(f.cache.len(), 0);
        f.runs("a中a中");
        assert!(f.cache.len() >= 2);
        assert_eq!(f.cache[&'a'], Some(0));
        assert_eq!(f.cache[&'中'], Some(1));
    }
}
