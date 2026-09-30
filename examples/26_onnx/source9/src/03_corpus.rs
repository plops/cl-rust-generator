//! `03_corpus` — Zeichensatz-Schnitt und (T6) Korpus-Laden.
//!
//! Der `Charset` schneidet Schrift der Sprache ∩ Wörterbuch des Modells ∩
//! Unifont: Generatoren erzeugen nur darstellbare Zeichen, die das Modell
//! kennen kann. Fehler messen dann das *Modell*, nicht Wörterbuchlücken
//! (z. B. `ẞ` fehlt im Universal-Wörterbuch und würde sonst immer
//! falsch gezählt). Leerzeichen ist implizit erlaubt (`use_space_char`).

use std::collections::HashSet;

use crate::lang::{COMMON, Lang};
use crate::render::Raster;

/// Erlaubte Zeichen für (Sprache, Modell, Schrift).
#[derive(Clone, Debug)]
pub struct Charset {
    set: HashSet<char>,
}

impl Charset {
    /// Schnitt aus `Lang::in_script` ∩ Ein-Zeichen-Wörterbuch ∩ Font.
    #[must_use]
    pub fn build(lang: &Lang, dict: &[String], font: &Raster) -> Self {
        let in_dict: HashSet<char> = dict
            .iter()
            .filter_map(|s| {
                let mut it = s.chars();
                let c = it.next()?;
                it.next().is_none().then_some(c)
            })
            .collect();
        let mut set = HashSet::new();
        let mut consider = |c: char| {
            if lang.in_script(c) && (c == ' ' || in_dict.contains(&c)) && font.has(c) {
                set.insert(c);
            }
        };
        for &(lo, hi) in lang.ranges {
            let (mut lo, hi) = (lo as u32, hi as u32);
            while lo <= hi {
                if let Some(c) = char::from_u32(lo) {
                    consider(c);
                }
                lo += 1;
            }
        }
        for c in COMMON.chars().chain(lang.extra.chars()) {
            consider(c);
        }
        consider(' ');
        Self { set }
    }

    /// Aus expliziter Zeichenliste (für Tests ohne Font/Wörterbuch).
    #[must_use]
    pub fn from_chars(chars: impl IntoIterator<Item = char>) -> Self {
        Self {
            set: chars.into_iter().collect(),
        }
    }

    /// Darf das Zeichen erzeugt werden?
    #[must_use]
    pub fn contains(&self, c: char) -> bool {
        self.set.contains(&c)
    }

    /// Entfernt alle nicht erlaubten Zeichen.
    #[must_use]
    pub fn filter(&self, s: &str) -> String {
        s.chars().filter(|c| self.contains(*c)).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lang::{LANGS, by_code};

    #[test]
    fn filter_keeps_only_allowed() {
        let cs = Charset::from_chars("ab ".chars());
        assert_eq!(cs.filter("a c b!"), "a  b");
        assert!(cs.contains(' '));
        assert!(!cs.contains('!'));
    }

    #[test]
    fn real_charset_drops_dict_gaps_but_keeps_umlauts() {
        let font = Raster::load(None).expect("GNU Unifont required");
        let de = &LANGS[by_code("de").unwrap()];
        // Mini-Wörterbuch ohne ẞ (wie das echte Universal-Wörterbuch).
        let dict: Vec<String> = "äöüÄÖÜßabc ".chars().map(|c| c.to_string()).collect();
        let cs = Charset::build(de, &dict, &font);
        for c in "äöüÄÖÜß ".chars() {
            assert!(cs.contains(c), "{c}");
        }
        assert!(!cs.contains('ẞ')); // nicht im Wörterbuch
        assert!(!cs.contains('ж')); // nicht in der Schrift
        assert_eq!(cs.filter("ß ẞ"), "ß ");
    }
}
