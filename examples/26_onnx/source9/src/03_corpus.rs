//! `03_corpus` — Zeichensatz-Schnitt und Korpus-Laden.
//!
//! Der `Charset` schneidet Schrift der Sprache ∩ Wörterbuch des Modells ∩
//! Unifont: Generatoren erzeugen nur darstellbare Zeichen, die das Modell
//! kennen kann. Fehler messen dann das *Modell*, nicht Wörterbuchlücken
//! (z. B. `ẞ` fehlt im Universal-Wörterbuch und würde sonst immer
//! falsch gezählt). Leerzeichen ist implizit erlaubt (`use_space_char`).
//!
//! Der `Corpus` lädt `corpus/<lang>.txt` (Wikipedia-Extrakte) und filtert
//! Tokens: nur Tokens aus Charset-Zeichen überleben (Häufigkeit bleibt
//! erhalten → gleichverteiltes Ziehen ist häufigkeitsgewichtet). Fehlt
//! die Datei, ist der Korpus leer und die Generatoren fallen auf
//! Pangramme zurück.

use std::collections::HashSet;
use std::path::Path;

use crate::lang::{COMMON, Lang};
use crate::render::Raster;

/// Erlaubte Zeichen für (Sprache, Modell, Schrift).
#[derive(Clone, Debug)]
pub struct Charset {
    set: HashSet<char>,
    /// Ziehbare Zeichen (sortiert, ohne Whitespace) für `Chars`.
    draw: Vec<char>,
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
        Self::from_set(set)
    }

    /// Aus expliziter Zeichenliste (für Tests ohne Font/Wörterbuch).
    #[must_use]
    pub fn from_chars(chars: impl IntoIterator<Item = char>) -> Self {
        Self::from_set(chars.into_iter().collect())
    }

    fn from_set(set: HashSet<char>) -> Self {
        let mut draw: Vec<char> = set.iter().copied().filter(|c| !c.is_whitespace()).collect();
        draw.sort();
        Self { set, draw }
    }

    /// Darf das Zeichen erzeugt werden?
    #[must_use]
    pub fn contains(&self, c: char) -> bool {
        self.set.contains(&c)
    }

    /// Gleichverteilte Ziehliste (sortiert, ohne Whitespace).
    #[must_use]
    pub fn drawable(&self) -> &[char] {
        &self.draw
    }

    /// Entfernt alle nicht erlaubten Zeichen.
    #[must_use]
    pub fn filter(&self, s: &str) -> String {
        s.chars().filter(|c| self.contains(*c)).collect()
    }
}

/// Gefilterte Korpus-Tokens einer Sprache.
#[derive(Clone, Debug, Default)]
pub struct Corpus {
    tokens: Vec<String>,
}

impl Corpus {
    /// Lädt `<dir>/<lang>.txt`; fehlt Datei/Verzeichnis → leerer Korpus.
    ///
    /// Dekodiert verlustbehaftet (`from_utf8_lossy`): die Wikipedia-
    /// Extrakte können abgeschnittene UTF-8-Sequenzen enthalten.
    #[must_use]
    pub fn load(dir: Option<&Path>, lang: &Lang, charset: &Charset) -> Self {
        let text = dir
            .map(|d| d.join(format!("{}.txt", lang.code)))
            .and_then(|p| std::fs::read(&p).ok())
            .map(|b| String::from_utf8_lossy(&b).into_owned())
            .unwrap_or_default();
        Self::parse(&text, charset)
    }

    /// Leerer Korpus (kein Verzeichnis konfiguriert).
    #[must_use]
    pub fn empty() -> Self {
        Self::default()
    }

    /// Filtert Tokens aus Rohtext (reine Funktion, testbar ohne Datei).
    #[must_use]
    pub fn parse(text: &str, charset: &Charset) -> Self {
        let tokens = text
            .split_whitespace()
            .filter(|t| !t.is_empty() && t.chars().all(|c| charset.contains(c)))
            .map(str::to_string)
            .collect();
        Self { tokens }
    }

    /// Gefilterte Tokens (mit Duplikaten = Häufigkeitsgewichtung).
    #[must_use]
    pub fn tokens(&self) -> &[String] {
        &self.tokens
    }

    /// Keine Tokens (→ Generatoren fallen auf Pangramme zurück).
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.tokens.is_empty()
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
    fn drawable_is_sorted_without_whitespace() {
        let cs = Charset::from_chars("b a\t".chars());
        assert_eq!(cs.drawable(), &['a', 'b']);
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

    #[test]
    fn parse_keeps_only_allowed_tokens() {
        let cs = Charset::from_chars("abc. ".chars());
        let c = Corpus::parse("abc ab.c a!c  d", &cs);
        assert_eq!(c.tokens(), &["abc".to_string(), "ab.c".to_string()]);
        assert!(!c.is_empty());
        assert!(Corpus::parse("", &cs).is_empty());
    }

    #[test]
    fn missing_dir_gives_empty_corpus() {
        let de = &LANGS[by_code("de").unwrap()];
        let cs = Charset::from_chars("abc ".chars());
        assert!(Corpus::load(None, de, &cs).is_empty());
        assert!(Corpus::load(Some(Path::new("/nonexistent-corpus-xyz")), de, &cs).is_empty());
        assert!(Corpus::empty().is_empty());
    }

    #[test]
    fn real_german_corpus_loads_thousands_of_tokens() {
        let dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("corpus");
        if !dir.join("de.txt").exists() {
            println!("SKIP: corpus missing (uv run scripts/fetch_corpus.py)");
            return;
        }
        let font = Raster::load(None).expect("GNU Unifont required");
        let de = &LANGS[by_code("de").unwrap()];
        // Durchlässiger Charset: alles aus Schrift + Font.
        let dict: Vec<String> = ('\u{20}'..='\u{33FF}').map(|c| c.to_string()).collect();
        let cs = Charset::build(de, &dict, &font);
        let c = Corpus::load(Some(&dir), de, &cs);
        assert!(c.tokens().len() > 5000, "tokens: {}", c.tokens().len());
        assert!(
            c.tokens()
                .iter()
                .all(|t| t.chars().all(|ch| cs.contains(ch)))
        );
    }
}
