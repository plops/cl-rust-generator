//! `05_generate` — Textzeilen je Generatormodus erzeugen.
//!
//! Alle Modi liefern Zeilen, die in die Canvas-Breite passen (Umbruch per
//! `fits`-Callback, das die echte Unifont-Breite misst). `Pangram` ist
//! implementiert; `Words`/`Markov`/`Chars` fallen bis T6–T8 darauf zurück.

use crate::corpus::Charset;
use crate::lang::Lang;
use crate::rng::Rng;

/// Textgenerator-Modus.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum GenMode {
    /// Kuratierte Beispielsätze (Sonderzeichen-lastig).
    #[default]
    Pangram,
    /// Zufallswörter aus dem Wikipedia-Korpus (T6).
    Words,
    /// Markov-Ketten auf dem Korpus (T7).
    Markov,
    /// Gleichverteilte Charset-Zeichen (T8).
    Chars,
}

impl GenMode {
    /// Alle Modi in Schaltreihenfolge.
    pub const ALL: [GenMode; 4] = [
        GenMode::Pangram,
        GenMode::Words,
        GenMode::Markov,
        GenMode::Chars,
    ];

    /// Kurzname (CLI, HUD).
    #[must_use]
    pub fn name(&self) -> &'static str {
        match self {
            GenMode::Pangram => "pangram",
            GenMode::Words => "words",
            GenMode::Markov => "markov",
            GenMode::Chars => "chars",
        }
    }
}

/// Mindestlänge einer Zeile (ohne Leerzeichen).
///
/// Ein-Zeichen-Zeilen (Umbruch-Artefakte wie ein einsames „…“) tragen
/// 0 % oder 100 % Zeilen-CER bei und verzerren den Mittelwert; die
/// Detektion misst `recall`/`FP` ohnehin separat.
pub const MIN_LINE_CHARS: usize = 2;

/// Erzeugt bis zu `n` Zeilen (ggf. weniger, nie leer bei `n > 0`).
///
/// Nur Zeichen aus `charset` (Schrift ∩ Wörterbuch ∩ Font) überleben;
/// Zeilen unter `MIN_LINE_CHARS` werden übersprungen.
pub fn generate(
    mode: GenMode,
    lang: &Lang,
    rng: &mut Rng,
    n: usize,
    charset: &Charset,
    fits: &mut impl FnMut(&str) -> bool,
) -> Vec<String> {
    match mode {
        GenMode::Pangram | GenMode::Words | GenMode::Markov | GenMode::Chars => {
            generate_pangram(lang, rng, n, charset, fits)
        }
    }
}

fn generate_pangram(
    lang: &Lang,
    rng: &mut Rng,
    n: usize,
    charset: &Charset,
    fits: &mut impl FnMut(&str) -> bool,
) -> Vec<String> {
    let mut out = Vec::new();
    let mut guard = 0;
    while out.len() < n && guard < n * 16 + 16 {
        guard += 1;
        let p = rng.pick(lang.pangrams);
        for line in wrap(p, fits) {
            let kept = charset.filter(&line);
            if kept.chars().filter(|c| !c.is_whitespace()).count() >= MIN_LINE_CHARS {
                out.push(kept);
                if out.len() >= n {
                    break;
                }
            }
        }
    }
    out
}

/// Bricht `text` so um, dass jede Zeile `fits` erfüllt.
///
/// Wortweise, wenn Leerzeichen vorkommen (sonst zeichenweise für
/// CJK/Thai); ein einzelnes zu langes Wort wird zeichenweise geteilt.
pub fn wrap(text: &str, fits: &mut impl FnMut(&str) -> bool) -> Vec<String> {
    if fits(text) {
        return vec![text.to_string()];
    }
    let tokens: Vec<&str> = if text.contains(' ') {
        text.split(' ').collect()
    } else {
        let mut v = Vec::new();
        let mut start = 0;
        for (i, _) in text.char_indices().skip(1) {
            v.push(&text[start..i]);
            start = i;
        }
        v.push(&text[start..]);
        v
    };
    let mut lines = Vec::new();
    let mut cur = String::new();
    let joiner = if text.contains(' ') { " " } else { "" };
    for tok in tokens {
        let cand = if cur.is_empty() {
            tok.to_string()
        } else {
            format!("{cur}{joiner}{tok}")
        };
        if fits(&cand) {
            cand.clone_into(&mut cur);
        } else {
            if !cur.is_empty() {
                lines.push(std::mem::take(&mut cur));
            }
            if fits(tok) {
                tok.clone_into(&mut cur);
            } else {
                // Einzelnes Token zu lang: zeichenweise füllen.
                for c in tok.chars() {
                    let cand2 = format!("{cur}{c}");
                    if cur.is_empty() || fits(&cand2) {
                        cand2.clone_into(&mut cur);
                    } else {
                        lines.push(std::mem::take(&mut cur));
                        cur.push(c);
                    }
                }
            }
        }
    }
    if !cur.is_empty() {
        lines.push(cur);
    }
    if lines.is_empty() {
        lines.push(text.to_string());
    }
    lines
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lang::{LANGS, by_code};

    fn max10(s: &str) -> bool {
        s.chars().count() <= 10
    }

    #[test]
    fn mode_names_cycle() {
        assert_eq!(GenMode::ALL.len(), 4);
        assert_eq!(GenMode::Pangram.name(), "pangram");
        assert_eq!(GenMode::default(), GenMode::Pangram);
    }

    #[test]
    fn wrap_keeps_words_and_width() {
        assert_eq!(wrap("aa bb cc", &mut max10), vec!["aa bb cc"]);
        assert_eq!(wrap("aaa bbb ccc", &mut max10), vec!["aaa bbb", "ccc"]);
        // Wort zu lang → Zeichenumbruch.
        assert_eq!(wrap("abcdefghijk", &mut max10), vec!["abcdefghij", "k"]);
        // Ohne Leerzeichen (CJK-Stil) zeichenweise.
        assert_eq!(
            wrap("一二三四五六七八九十十一", &mut max10),
            vec!["一二三四五六七八九十", "十一"]
        );
    }

    #[test]
    fn pangram_lines_fit_width_and_charset() {
        // Durchlässiger Charset: alle Pangramm-Zeichen aller Sprachen.
        let all: String = LANGS
            .iter()
            .flat_map(|l| l.pangrams.iter().copied())
            .collect();
        let cs = Charset::from_chars(all.chars());
        for l in LANGS {
            let mut rng = Rng::new(7);
            let lines = generate(GenMode::Pangram, l, &mut rng, 6, &cs, &mut max10);
            assert!(!lines.is_empty(), "{}", l.code);
            for line in &lines {
                assert!(max10(line), "{}: {line}", l.code);
                for c in line.chars() {
                    assert!(cs.contains(c), "{}: {c:?}", l.code);
                    assert!(l.in_script(c), "{}: {c:?}", l.code);
                }
            }
        }
        // Deterministisch.
        let de = &LANGS[by_code("de").unwrap()];
        let a = generate(GenMode::Pangram, de, &mut Rng::new(3), 4, &cs, &mut max10);
        let b = generate(GenMode::Pangram, de, &mut Rng::new(3), 4, &cs, &mut max10);
        assert_eq!(a, b);
    }

    #[test]
    fn pangram_filtering_drops_disallowed_chars() {
        let de = &LANGS[by_code("de").unwrap()];
        let cs = Charset::from_chars("abc ".chars());
        let lines = generate(GenMode::Pangram, de, &mut Rng::new(1), 4, &cs, &mut max10);
        assert!(!lines.is_empty());
        for line in &lines {
            let n = line.chars().filter(|c| !c.is_whitespace()).count();
            assert!(n >= MIN_LINE_CHARS, "{line}");
            for c in line.chars() {
                assert!(cs.contains(c), "{c:?} in {line}");
            }
        }
    }
}
