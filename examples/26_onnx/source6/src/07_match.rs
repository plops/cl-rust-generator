//! `07_match` — Fuzzy-OCR-Matcher (S4).
//!
//! Faltet optisch verwechselbare Glyphen (`I`/`l`/`1`, `O`/`0`),
//! ignoriert Cursor-Artefakte und bröckelnde Satzzeichen und erlaubt
//! längenproportional 0–3 Edits (Sliding-Levenshtein). Rein, ohne X11.

/// OCR-Text normalisieren: Cursor/Satzzeichen weg, Glyphen falten.
#[must_use]
pub fn norm_ocr_str(s: &str) -> String {
    let trimmed = s.trim().trim_end_matches(['|', '¦', '_', '.', '…']);
    let mut out = String::with_capacity(trimmed.len());
    let mut prev_space = true;

    for ch in trimmed.chars() {
        let mapped = match ch {
            // Optisch fast identisch: I, l, 1, |, !, i → 'l'.
            'I' | 'l' | '1' | '|' | '!' | '¦' | 'i' | ']' | '[' => 'l',
            // O, o, 0 → 'o'.
            '0' | 'O' | 'o' => 'o',
            // Whitespace falten.
            ' ' | '\t' | '\n' | '\r' => ' ',
            // Kleine/bröckelnde Satzzeichen ignorieren.
            '.' | '…' | ',' | ':' | ';' | '\'' | '"' | '`' | '-' | '_' | '~' => continue,
            other => other.to_ascii_lowercase(),
        };

        if mapped == ' ' {
            if !prev_space {
                out.push(' ');
                prev_space = true;
            }
        } else {
            out.push(mapped);
            prev_space = false;
        }
    }
    if prev_space && !out.is_empty() {
        out.pop();
    }
    out
}

/// Kleinste Levenshtein-Distanz von `needle` gegen ein Substring-Fenster
/// von `haystack` (1D-DP; leeres Pattern kostet 0).
fn levenshtein_substring(needle: &str, haystack: &str) -> usize {
    let n_chars: Vec<char> = needle.chars().collect();
    let h_chars: Vec<char> = haystack.chars().collect();
    if n_chars.is_empty() {
        return 0;
    }
    if h_chars.is_empty() {
        return n_chars.len();
    }

    // Substring-Match: Distanz zum leeren Anfang ist 0 an jeder Position.
    let mut prev = vec![0usize; h_chars.len() + 1];
    let mut curr = vec![0usize; h_chars.len() + 1];

    for (i, &nc) in n_chars.iter().enumerate() {
        curr[0] = i + 1;
        for (j, &hc) in h_chars.iter().enumerate() {
            let cost = usize::from(nc != hc);
            curr[j + 1] = (prev[j] + cost).min(prev[j + 1] + 1).min(curr[j] + 1);
        }
        std::mem::swap(&mut prev, &mut curr);
    }
    *prev.iter().skip(1).min().unwrap_or(&n_chars.len())
}

/// Wahr, wenn `pattern` in `candidate` steckt — trotz Glyph-Konfusion,
/// Cursor-Artefakten und fehlender Punkte (Toleranz nach Pattern-Länge).
#[must_use]
pub fn fuzzy_ocr_match(pattern: &str, candidate: &str) -> bool {
    let p = norm_ocr_str(pattern);
    let c = norm_ocr_str(candidate);
    if p.is_empty() {
        return false;
    }
    // 1. Direkter gefalteter Substring (deckt I/l/1, Punkte, Cursor ab).
    if c.contains(&p) {
        return true;
    }
    // 2. Toleranz für fehlende/überzählige Kleinstzeichen (längenproportional).
    let max_edits = match p.len() {
        0..=3 => 0,
        4..=7 => 1,
        8..=15 => 2,
        _ => 3,
    };
    if max_edits == 0 {
        return false;
    }
    levenshtein_substring(&p, &c) <= max_edits
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fuzzy_ocr_meta_ai() {
        // Exaktes Nutzer-Problem: großes I als kleines l, Punkte fehlen.
        assert!(fuzzy_ocr_match("Ask Meta AI...", "Ask Meta Al.."));
        assert!(fuzzy_ocr_match("Ask Meta AI...", "Ask Meta AI |"));
        assert!(fuzzy_ocr_match("Ask Meta AI...", "Ask Meta Al"));
        assert!(fuzzy_ocr_match("Ask Meta AI...", "Ask Meta AI..."));
    }

    #[test]
    fn test_fuzzy_ocr_common_confusions() {
        assert!(fuzzy_ocr_match("Sign In", "Sign ln"));
        assert!(fuzzy_ocr_match("Google Search", "GoogIe Search"));
        assert!(fuzzy_ocr_match(
            "Ask anything privately",
            "Ask anything privately |"
        ));
        assert!(!fuzzy_ocr_match("Cancel", "Confirm"));
    }
}
