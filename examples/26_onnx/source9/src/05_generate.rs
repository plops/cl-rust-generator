//! `05_generate` — Textzeilen je Generatormodus erzeugen.
//!
//! Alle Modi liefern Zeilen, die in die Canvas-Breite passen (Umbruch per
//! `fits`-Callback, das die echte Unifont-Breite misst). `Pangram` ist
//! implementiert; `Words`/`Markov`/`Chars` fallen bis T6–T8 darauf zurück.

use crate::corpus::{Charset, Corpus};
use crate::lang::Lang;
use crate::markov::Markov;
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

/// Eingaben für die Generatoren (alles in `Engine` gecacht).
pub struct GenInput<'a> {
    /// Erlaubte Zeichen.
    pub charset: &'a Charset,
    /// Korpus-Tokens (für `Words`, Training für `Markov`).
    pub corpus: &'a Corpus,
    /// Trainiertes n-Gramm (für `Markov`).
    pub markov: &'a Markov,
}

/// Erzeugt bis zu `n` Zeilen (ggf. weniger, nie leer bei `n > 0`).
///
/// Nur Zeichen aus dem Charset überleben; Zeilen unter `MIN_LINE_CHARS`
/// werden übersprungen. Leerer Korpus → Pangramm-Fallback.
pub fn generate(
    mode: GenMode,
    lang: &Lang,
    rng: &mut Rng,
    n: usize,
    input: &GenInput,
    fits: &mut impl FnMut(&str) -> bool,
) -> Vec<String> {
    match mode {
        GenMode::Pangram => generate_pangram(lang, rng, n, input.charset, fits),
        GenMode::Words => generate_words(lang, rng, n, input.charset, input.corpus, fits),
        GenMode::Markov => generate_markov(lang, rng, n, input.charset, input.markov, fits),
        GenMode::Chars => generate_chars(lang, rng, n, input.charset, fits),
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
        push_lines(&mut out, p, n, charset, fits);
    }
    out
}

fn generate_words(
    lang: &Lang,
    rng: &mut Rng,
    n: usize,
    charset: &Charset,
    corpus: &Corpus,
    fits: &mut impl FnMut(&str) -> bool,
) -> Vec<String> {
    if corpus.is_empty() {
        return generate_pangram(lang, rng, n, charset, fits);
    }
    let mut out = Vec::new();
    let mut guard = 0;
    while out.len() < n && guard < n * 4 + 4 {
        guard += 1;
        let text: Vec<&str> = (0..12)
            .map(|_| rng.pick(corpus.tokens()).as_str())
            .collect();
        push_lines(&mut out, &text.join(" "), n, charset, fits);
    }
    if out.is_empty() {
        generate_pangram(lang, rng, n, charset, fits)
    } else {
        out
    }
}

fn generate_markov(
    lang: &Lang,
    rng: &mut Rng,
    n: usize,
    charset: &Charset,
    markov: &Markov,
    fits: &mut impl FnMut(&str) -> bool,
) -> Vec<String> {
    if markov.is_empty() {
        return generate_pangram(lang, rng, n, charset, fits);
    }
    let mut out = Vec::new();
    let mut guard = 0;
    while out.len() < n && guard < n * 4 + 4 {
        guard += 1;
        push_lines(&mut out, &markov.sample(rng, 120), n, charset, fits);
    }
    if out.is_empty() {
        generate_pangram(lang, rng, n, charset, fits)
    } else {
        out
    }
}

/// Pseudowörter (2–8 Zeichen) aus gleichverteilten Charset-Zeichen.
///
/// Jedes Sonderzeichen kommt gleich oft vor (keine Häufigkeits-
/// Gewichtung wie bei `Words`/`Markov`).
fn generate_chars(
    lang: &Lang,
    rng: &mut Rng,
    n: usize,
    charset: &Charset,
    fits: &mut impl FnMut(&str) -> bool,
) -> Vec<String> {
    let draw = charset.drawable();
    if draw.is_empty() {
        return generate_pangram(lang, rng, n, charset, fits);
    }
    let mut out = Vec::new();
    let mut guard = 0;
    while out.len() < n && guard < n * 4 + 4 {
        guard += 1;
        let words: Vec<String> = (0..12)
            .map(|_| (0..rng.range(2, 8)).map(|_| rng.pick(draw)).collect())
            .collect();
        push_lines(&mut out, &words.join(" "), n, charset, fits);
    }
    if out.is_empty() {
        generate_pangram(lang, rng, n, charset, fits)
    } else {
        out
    }
}

/// Bricht `text` um, filtert und hängt gültige Zeilen an (bis `n`).
fn push_lines(
    out: &mut Vec<String>,
    text: &str,
    n: usize,
    charset: &Charset,
    fits: &mut impl FnMut(&str) -> bool,
) {
    for line in wrap(text, fits) {
        let kept = charset.filter(&line);
        if kept.chars().filter(|c| !c.is_whitespace()).count() >= MIN_LINE_CHARS {
            out.push(kept);
            if out.len() >= n {
                break;
            }
        }
    }
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

    fn gen_input<'a>(cs: &'a Charset, corpus: &'a Corpus, markov: &'a Markov) -> GenInput<'a> {
        GenInput {
            charset: cs,
            corpus,
            markov,
        }
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
        let corpus = Corpus::empty();
        let markov = Markov::train(&[]);
        let input = gen_input(&cs, &corpus, &markov);
        for l in LANGS {
            let mut rng = Rng::new(7);
            let lines = generate(GenMode::Pangram, l, &mut rng, 6, &input, &mut max10);
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
        let a = generate(
            GenMode::Pangram,
            de,
            &mut Rng::new(3),
            4,
            &input,
            &mut max10,
        );
        let b = generate(
            GenMode::Pangram,
            de,
            &mut Rng::new(3),
            4,
            &input,
            &mut max10,
        );
        assert_eq!(a, b);
    }

    #[test]
    fn pangram_filtering_drops_disallowed_chars() {
        let de = &LANGS[by_code("de").unwrap()];
        let cs = Charset::from_chars("abc ".chars());
        let corpus = Corpus::empty();
        let markov = Markov::train(&[]);
        let input = gen_input(&cs, &corpus, &markov);
        let lines = generate(
            GenMode::Pangram,
            de,
            &mut Rng::new(1),
            4,
            &input,
            &mut max10,
        );
        assert!(!lines.is_empty());
        for line in &lines {
            let n = line.chars().filter(|c| !c.is_whitespace()).count();
            assert!(n >= MIN_LINE_CHARS, "{line}");
            for c in line.chars() {
                assert!(cs.contains(c), "{c:?} in {line}");
            }
        }
    }

    #[test]
    fn words_uses_only_corpus_tokens() {
        let de = &LANGS[by_code("de").unwrap()];
        let cs = Charset::from_chars("abc ".chars());
        let corpus = Corpus::parse("ab bc ab", &cs);
        let markov = Markov::train(&[]);
        let input = gen_input(&cs, &corpus, &markov);
        let lines = generate(GenMode::Words, de, &mut Rng::new(5), 4, &input, &mut max10);
        assert_eq!(lines.len(), 4);
        for line in &lines {
            assert!(max10(line), "{line}");
            for word in line.split_whitespace() {
                assert!(["ab", "bc"].contains(&word), "{word} in {line}");
            }
        }
    }

    #[test]
    fn words_with_empty_corpus_falls_back_to_pangram() {
        let de = &LANGS[by_code("de").unwrap()];
        let all: String = de.pangrams.iter().copied().collect();
        let cs = Charset::from_chars(all.chars());
        let corpus = Corpus::empty();
        let markov = Markov::train(&[]);
        let input = gen_input(&cs, &corpus, &markov);
        let words = generate(GenMode::Words, de, &mut Rng::new(9), 4, &input, &mut max10);
        let pang = generate(
            GenMode::Pangram,
            de,
            &mut Rng::new(9),
            4,
            &input,
            &mut max10,
        );
        assert_eq!(words, pang);
        assert!(!words.is_empty());
    }

    #[test]
    fn markov_lines_come_from_model_and_are_deterministic() {
        let de = &LANGS[by_code("de").unwrap()];
        let cs = Charset::from_chars("abcdef ".chars());
        let corpus = Corpus::parse("abc abd abe", &cs);
        let markov = Markov::train(corpus.tokens());
        let input = gen_input(&cs, &corpus, &markov);
        let lines = generate(GenMode::Markov, de, &mut Rng::new(2), 4, &input, &mut max10);
        assert_eq!(lines.len(), 4);
        for line in &lines {
            assert!(max10(line), "{line}");
            for c in line.chars() {
                assert!(cs.contains(c), "{c:?} in {line}");
            }
        }
        let again = generate(GenMode::Markov, de, &mut Rng::new(2), 4, &input, &mut max10);
        assert_eq!(lines, again);
    }

    #[test]
    fn chars_draws_only_charset_chars_in_pseudowords_2_to_8() {
        let de = &LANGS[by_code("de").unwrap()];
        let cs = Charset::from_chars("abcd ".chars());
        let corpus = Corpus::empty();
        let markov = Markov::train(&[]);
        let input = gen_input(&cs, &corpus, &markov);
        let lines = generate(GenMode::Chars, de, &mut Rng::new(4), 6, &input, &mut max10);
        assert_eq!(lines.len(), 6);
        for line in &lines {
            assert!(max10(line), "{line}");
            for word in line.split_whitespace() {
                let len = word.chars().count();
                assert!((2..=8).contains(&len), "{word} in {line}");
                for c in word.chars() {
                    assert!(cs.contains(c), "{c:?} in {line}");
                }
            }
        }
        let again = generate(GenMode::Chars, de, &mut Rng::new(4), 6, &input, &mut max10);
        assert_eq!(lines, again);
    }

    #[test]
    fn markov_with_empty_model_falls_back_to_pangram() {
        let de = &LANGS[by_code("de").unwrap()];
        let all: String = de.pangrams.iter().copied().collect();
        let cs = Charset::from_chars(all.chars());
        let corpus = Corpus::empty();
        let markov = Markov::train(&[]);
        let input = gen_input(&cs, &corpus, &markov);
        let mark = generate(GenMode::Markov, de, &mut Rng::new(9), 4, &input, &mut max10);
        let pang = generate(
            GenMode::Pangram,
            de,
            &mut Rng::new(9),
            4,
            &input,
            &mut max10,
        );
        assert_eq!(mark, pang);
        assert!(!mark.is_empty());
    }
}
