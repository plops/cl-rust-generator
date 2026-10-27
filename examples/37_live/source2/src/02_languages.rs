//! 02 — Sprachen des Live-Uebersetzungsmodells.
//!
//! Quelle: ai.google.dev/gemini-api/docs/live-api/live-translate,
//! Abschnitt „Supported languages" (Stand 2026-09-16, 78 Sprachen).
//! Gemini erkennt die Quellsprache selbst; konfiguriert wird nur die
//! Zielsprache (`translationConfig.targetLanguageCode`, BCP-47).
//! Die Tabelle geht ueber `/api/languages` ans Frontend.

/// Eine Sprache: BCP-47-Code und deutscher Anzeigename.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize)]
pub struct Language {
    pub code: &'static str,
    pub name: &'static str,
}

/// Default „Ich spreche".
pub const DEFAULT_LANG: &str = "de";
/// Default „Partner hoert".
pub const DEFAULT_TARGET: &str = "zh-Hans";

const fn l(code: &'static str, name: &'static str) -> Language {
    Language { code, name }
}

/// Alle unterstuetzten Sprachen, nach deutschem Namen sortiert.
pub const LANGUAGES: [Language; 78] = [
    l("af", "Afrikaans"),
    l("ak", "Akan"),
    l("sq", "Albanisch"),
    l("am", "Amharisch"),
    l("ar", "Arabisch"),
    l("hy", "Armenisch"),
    l("az", "Aserbaidschanisch"),
    l("eu", "Baskisch"),
    l("be", "Belarussisch"),
    l("bn", "Bengalisch"),
    l("my", "Birmanisch"),
    l("bg", "Bulgarisch"),
    l("zh-Hans", "Chinesisch (vereinfacht)"),
    l("zh-Hant", "Chinesisch (traditionell)"),
    l("da", "Dänisch"),
    l("de", "Deutsch"),
    l("en", "Englisch"),
    l("et", "Estnisch"),
    l("fil", "Filipino"),
    l("fi", "Finnisch"),
    l("fr", "Französisch"),
    l("gl", "Galicisch"),
    l("ka", "Georgisch"),
    l("el", "Griechisch"),
    l("gu", "Gujarati"),
    l("ha", "Hausa"),
    l("he", "Hebräisch"),
    l("hi", "Hindi"),
    l("id", "Indonesisch"),
    l("is", "Isländisch"),
    l("it", "Italienisch"),
    l("ja", "Japanisch"),
    l("jv", "Javanisch"),
    l("kn", "Kannada"),
    l("kk", "Kasachisch"),
    l("ca", "Katalanisch"),
    l("km", "Khmer"),
    l("rw", "Kinyarwanda"),
    l("ko", "Koreanisch"),
    l("hr", "Kroatisch"),
    l("lo", "Laotisch"),
    l("lv", "Lettisch"),
    l("lt", "Litauisch"),
    l("ms", "Malaiisch"),
    l("ml", "Malayalam"),
    l("mr", "Marathi"),
    l("mk", "Mazedonisch"),
    l("mn", "Mongolisch"),
    l("ne", "Nepali"),
    l("nl", "Niederländisch"),
    l("no", "Norwegisch"),
    l("pa", "Panjabi"),
    l("fa", "Persisch"),
    l("pl", "Polnisch"),
    l("pt-BR", "Portugiesisch (Brasilien)"),
    l("pt-PT", "Portugiesisch (Portugal)"),
    l("ro", "Rumänisch"),
    l("ru", "Russisch"),
    l("sv", "Schwedisch"),
    l("sr", "Serbisch"),
    l("sd", "Sindhi"),
    l("si", "Singhalesisch"),
    l("sk", "Slowakisch"),
    l("sl", "Slowenisch"),
    l("es", "Spanisch"),
    l("su", "Sundanesisch"),
    l("sw", "Suaheli"),
    l("ta", "Tamil"),
    l("te", "Telugu"),
    l("th", "Thai"),
    l("cs", "Tschechisch"),
    l("tr", "Türkisch"),
    l("uk", "Ukrainisch"),
    l("hu", "Ungarisch"),
    l("ur", "Urdu"),
    l("uz", "Usbekisch"),
    l("vi", "Vietnamesisch"),
    l("zu", "Zulu"),
];

/// Sprache per Code suchen (exakt, Gross-/Kleinschreibung egal).
pub fn find(code: &str) -> Option<Language> {
    LANGUAGES
        .iter()
        .copied()
        .find(|l| l.code.eq_ignore_ascii_case(code))
}

/// Code normalisieren: bekannte Codes in kanonischer Schreibweise,
/// unbekannte/leere → `fallback`.
pub fn canonical<'a>(code: &str, fallback: &'a str) -> &'a str
where
    'static: 'a,
{
    find(code).map_or(fallback, |l| l.code)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    #[test]
    fn tabelle_vollstaendig_und_eindeutig() {
        let codes: HashSet<_> = LANGUAGES.iter().map(|l| l.code).collect();
        assert_eq!(codes.len(), 78, "doppelter code");
        let names: HashSet<_> = LANGUAGES.iter().map(|l| l.name).collect();
        assert_eq!(names.len(), 78, "doppelter name");
    }

    #[test]
    fn defaults_sind_deutsch_und_chinesisch() {
        assert_eq!(find(DEFAULT_LANG).unwrap().name, "Deutsch");
        assert_eq!(
            find(DEFAULT_TARGET).unwrap().name,
            "Chinesisch (vereinfacht)"
        );
    }

    #[test]
    fn canonical_normalisiert() {
        assert_eq!(canonical("ZH-hans", "en"), "zh-Hans");
        assert_eq!(canonical("pt-br", "en"), "pt-BR");
        assert_eq!(canonical("xx", DEFAULT_TARGET), "zh-Hans");
        assert_eq!(canonical("", "de"), "de");
    }
}
