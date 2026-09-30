//! `04_markov` — Zeichen-n-Gramm (Ordnung 3) für realistische Texte.
//!
//! Trainiert auf den Korpus-Tokens (mit Leerzeichen verbunden, damit
//! Wortgrenzen gelernt werden). Sampling mit Backoff: unbekannter
//! 2-Zeichen-Kontext → 1-Zeichen-Kontext → Gesamtverteilung. Alle
//! Tabellen sind nach Zeichen sortiert → deterministisch bei festem Seed.

use std::collections::HashMap;

use crate::rng::Rng;

/// Ordnung des Modells (Trigramm: 2 Zeichen Kontext → 1 Zeichen).
pub const ORDER: usize = 3;
/// Kontextlänge.
const CTX: usize = ORDER - 1;

/// Zeichen-Trigramm-Modell mit Backoff.
#[derive(Clone, Debug, Default)]
pub struct Markov {
    /// 2-Zeichen-Kontext → (Zeichen, Anzahl), nach Zeichen sortiert.
    tri: HashMap<[char; CTX], Vec<(char, u32)>>,
    /// 1-Zeichen-Kontext → (Zeichen, Anzahl).
    bi: HashMap<[char; 1], Vec<(char, u32)>>,
    /// Gesamtverteilung.
    uni: Vec<(char, u32)>,
    /// Beobachtete Start-Kontexte (sortiert).
    starts: Vec<[char; CTX]>,
}

impl Markov {
    /// Trainiert auf Tokens (werden mit Leerzeichen verbunden).
    #[must_use]
    pub fn train(tokens: &[String]) -> Self {
        let text = tokens.join(" ");
        let chars: Vec<char> = text.chars().collect();
        let mut tri: HashMap<[char; CTX], HashMap<char, u32>> = HashMap::new();
        let mut bi: HashMap<[char; 1], HashMap<char, u32>> = HashMap::new();
        let mut uni: HashMap<char, u32> = HashMap::new();
        for c in &chars {
            *uni.entry(*c).or_default() += 1;
        }
        for w in chars.windows(2) {
            *bi.entry([w[0]]).or_default().entry(w[1]).or_default() += 1;
        }
        for w in chars.windows(3) {
            *tri.entry([w[0], w[1]])
                .or_default()
                .entry(w[2])
                .or_default() += 1;
        }
        let mut starts: Vec<[char; CTX]> = tri.keys().copied().collect();
        starts.sort();
        let uni = sorted(uni);
        if starts.is_empty() {
            // Winziger Trainings-Text (< 3 Zeichen): Kontext erfinden,
            // `step` fällt dann auf Bi-/Unigramm zurück.
            if let Some(&(c, _)) = uni.first() {
                starts.push([c, c]);
            }
        }
        Self {
            tri: tri.into_iter().map(|(k, v)| (k, sorted(v))).collect(),
            bi: bi.into_iter().map(|(k, v)| (k, sorted(v))).collect(),
            uni,
            starts,
        }
    }

    /// Keine Trainingsdaten (→ Pangramm-Fallback).
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.uni.is_empty()
    }

    /// Ein Zeichen im Kontext (mit Backoff); `None` nur bei leerem Modell.
    pub fn step(&self, rng: &mut Rng, ctx: &[char; CTX]) -> Option<char> {
        if let Some(v) = self.tri.get(ctx).filter(|v| !v.is_empty()) {
            return Some(pick_weighted(rng, v));
        }
        if let Some(v) = self.bi.get(&[ctx[1]]).filter(|v| !v.is_empty()) {
            return Some(pick_weighted(rng, v));
        }
        (!self.uni.is_empty()).then(|| pick_weighted(rng, &self.uni))
    }

    /// Erzeugt bis zu `max_chars` Zeichen (weiche Grenze).
    ///
    /// Angebrochene Randwörter werden abgeschnitten (nur wenn der Text
    /// Leerzeichen enthält; CJK bleibt unbeschnitten).
    pub fn sample(&self, rng: &mut Rng, max_chars: usize) -> String {
        if self.is_empty() || max_chars == 0 {
            return String::new();
        }
        let mut ctx = *rng.pick(&self.starts);
        let mut out = vec![ctx[0], ctx[1]];
        while out.len() < max_chars.max(CTX) {
            match self.step(rng, &ctx) {
                Some(c) => {
                    out.push(c);
                    ctx = [ctx[1], c];
                }
                None => break,
            }
        }
        let t = out.into_iter().collect::<String>();
        let t = t.trim();
        match (t.find(' '), t.rfind(' ')) {
            (Some(a), Some(b)) if a < b => t[a + 1..b].to_string(),
            _ => t.to_string(),
        }
    }
}

fn sorted(m: HashMap<char, u32>) -> Vec<(char, u32)> {
    let mut v: Vec<(char, u32)> = m.into_iter().collect();
    v.sort_by_key(|&(c, _)| c);
    v
}

/// Häufigkeitsgewichtete Ziehung (`v` nicht leer).
fn pick_weighted(rng: &mut Rng, v: &[(char, u32)]) -> char {
    let total: u32 = v.iter().map(|(_, w)| w).sum();
    let mut r = rng.below(total.max(1) as usize);
    for (c, w) in v {
        if r < *w as usize {
            return *c;
        }
        r -= *w as usize;
    }
    v.last().expect("non-empty").0
}

#[cfg(test)]
mod tests {
    use super::*;

    fn toks(words: &[&str]) -> Vec<String> {
        words.iter().map(|s| s.to_string()).collect()
    }

    #[test]
    fn train_and_sample_are_deterministic() {
        let m1 = Markov::train(&toks(&["die", "katze", "sitzt", "auf", "der", "matte"]));
        let m2 = Markov::train(&toks(&["die", "katze", "sitzt", "auf", "der", "matte"]));
        let a = m1.sample(&mut Rng::new(11), 60);
        let b = m2.sample(&mut Rng::new(11), 60);
        assert_eq!(a, b);
        assert!(!a.is_empty());
        // Nur gelerntes Alphabet.
        for c in a.chars() {
            assert!("diekatzsumf r".contains(c), "{c}");
        }
    }

    #[test]
    fn step_prefers_trigram_then_backs_off() {
        let m = Markov::train(&toks(&["ab", "cd"])); // Text: "ab cd"
        let mut rng = Rng::new(1);
        // Gelernt: "ab"→' ', "b "→'c', " c"→'d'.
        assert_eq!(m.step(&mut rng, &['a', 'b']), Some(' '));
        // Unbekannter Kontext mit bekanntem 2. Zeichen → Bigramm.
        assert_eq!(m.step(&mut rng, &['x', 'b']), Some(' '));
        // Alles unbekannt → Unigramm (irgendein gelerntes Zeichen).
        let c = m.step(&mut rng, &['x', 'y']).unwrap();
        assert!("ab cd".contains(c));
        assert_eq!(Markov::train(&[]).step(&mut rng, &['a', 'b']), None);
    }

    #[test]
    fn sample_trims_partial_edge_words() {
        let m = Markov::train(&toks(&["aaa", "bbb", "ccc"]));
        for seed in 0..20 {
            let s = m.sample(&mut Rng::new(seed), 30);
            assert!(!s.starts_with(' ') && !s.ends_with(' '), "{s:?}");
        }
    }

    #[test]
    fn empty_and_tiny_models_dont_panic() {
        assert!(Markov::train(&[]).is_empty());
        assert_eq!(Markov::train(&[]).sample(&mut Rng::new(1), 50), "");
        // Ein Zeichen: kein Tri-/Bigramm, nur Unigramm.
        let m = Markov::train(&toks(&["x"]));
        assert!(!m.is_empty());
        assert_eq!(m.sample(&mut Rng::new(1), 5), "xxxxx");
    }
}
