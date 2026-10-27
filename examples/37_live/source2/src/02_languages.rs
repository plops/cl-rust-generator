//! 02 — Sprachen beider Gemini-Live-Modelle.
//!
//! Zwei Modelle, zwei Sprachmengen:
//! - Uebersetzungsmodell `gemini-3.5-live-translate-preview`: 78 Sprachen
//!   (ai.google.dev/gemini-api/docs/live-api/live-translate, „Supported
//!   languages", Stand 2026-09-16) → Feld `translate`.
//! - Konversationsmodell `gemini-3.8-live`: 99 Sprachen (offizielle
//!   Live-API-Tabelle, ai.google.dev/gemini-api/docs/live-api/capabilities
//!   #supported-languages) → Feld `live`. Das sind alle 78 Translate-Sprachen
//!   AUSSER `jv` (Javanisch) und `su` (Sundanesisch), PLUS 23 nur-Live-
//!   Sprachen.
//!
//! Vereinigung = 101 Eintraege. Jeder Eintrag traegt den deutschen und den
//! englischen Namen (`en`), weil die Dolmetsch-Systemanweisung des
//! Live-Modells englische Sprachnamen fuer Quelle und Ziel braucht.
//!
//! Gemini erkennt die Quellsprache selbst; konfiguriert wird nur die
//! Zielsprache. Die Tabelle geht ueber `/api/languages` ans Frontend.

/// Eine Sprache: Code, deutscher und englischer Anzeigename, Modell-Flags.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize)]
pub struct Language {
    /// BCP-47-Code, z. B. `de` oder `zh-Hans`.
    pub code: &'static str,
    /// Deutscher Anzeigename (Frontend, nach diesem sortiert).
    pub name: &'static str,
    /// Englischer Name fuer die Dolmetsch-Systemanweisung des Live-Modells.
    pub en: &'static str,
    /// Vom Uebersetzungsmodell unterstuetzt (78 Sprachen).
    pub translate: bool,
    /// Vom Konversationsmodell `gemini-3.8-live` unterstuetzt (99 Sprachen).
    pub live: bool,
}

/// Default „Ich spreche".
pub const DEFAULT_LANG: &str = "de";
/// Default „Partner hoert" (nur noch fuer `/api/languages`-Anzeige relevant).
pub const DEFAULT_TARGET: &str = "zh-Hans";

const fn l(
    code: &'static str,
    name: &'static str,
    en: &'static str,
    translate: bool,
    live: bool,
) -> Language {
    Language {
        code,
        name,
        en,
        translate,
        live,
    }
}

/// Alle unterstuetzten Sprachen, nach deutschem Namen sortiert.
///
/// `translate`/`live` zeigen, welches Modell die Sprache kann. `jv`/`su`
/// koennen nur `translate`, die 23 nur-Live-Sprachen nur `live`; alle anderen
/// 76 koennen beides.
pub const LANGUAGES: [Language; 101] = [
    l("af", "Afrikaans", "Afrikaans", true, true),
    l("ak", "Akan", "Akan", true, true),
    l("sq", "Albanisch", "Albanian", true, true),
    l("am", "Amharisch", "Amharic", true, true),
    l("ar", "Arabisch", "Arabic", true, true),
    l("hy", "Armenisch", "Armenian", true, true),
    l("az", "Aserbaidschanisch", "Azerbaijani", true, true),
    l("as", "Assamesisch", "Assamese", false, true),
    l("eu", "Baskisch", "Basque", true, true),
    l("be", "Belarussisch", "Belarusian", true, true),
    l("bn", "Bengalisch", "Bengali", true, true),
    l("my", "Birmanisch", "Burmese", true, true),
    l("bs", "Bosnisch", "Bosnian", false, true),
    l("bg", "Bulgarisch", "Bulgarian", true, true),
    l("ceb", "Cebuano", "Cebuano", false, true),
    l(
        "zh-Hant",
        "Chinesisch (traditionell)",
        "Traditional Chinese",
        true,
        true,
    ),
    l(
        "zh-Hans",
        "Chinesisch (vereinfacht)",
        "Simplified Chinese",
        true,
        true,
    ),
    l("da", "Dänisch", "Danish", true, true),
    l("de", "Deutsch", "German", true, true),
    l("en", "Englisch", "English", true, true),
    l("et", "Estnisch", "Estonian", true, true),
    l("fo", "Färöisch", "Faroese", false, true),
    l("fil", "Filipino", "Filipino", true, true),
    l("fi", "Finnisch", "Finnish", true, true),
    l("fr", "Französisch", "French", true, true),
    l("gl", "Galicisch", "Galician", true, true),
    l("ka", "Georgisch", "Georgian", true, true),
    l("el", "Griechisch", "Greek", true, true),
    l("gu", "Gujarati", "Gujarati", true, true),
    l("ha", "Hausa", "Hausa", true, true),
    l("he", "Hebräisch", "Hebrew", true, true),
    l("hi", "Hindi", "Hindi", true, true),
    l("id", "Indonesisch", "Indonesian", true, true),
    l("ga", "Irisch", "Irish", false, true),
    l("is", "Isländisch", "Icelandic", true, true),
    l("it", "Italienisch", "Italian", true, true),
    l("ja", "Japanisch", "Japanese", true, true),
    l("jv", "Javanisch", "Javanese", true, false),
    l("kn", "Kannada", "Kannada", true, true),
    l("kk", "Kasachisch", "Kazakh", true, true),
    l("ca", "Katalanisch", "Catalan", true, true),
    l("km", "Khmer", "Khmer", true, true),
    l("rw", "Kinyarwanda", "Kinyarwanda", true, true),
    l("ky", "Kirgisisch", "Kyrgyz", false, true),
    l("ko", "Koreanisch", "Korean", true, true),
    l("hr", "Kroatisch", "Croatian", true, true),
    l("ku", "Kurdisch", "Kurdish", false, true),
    l("lo", "Laotisch", "Lao", true, true),
    l("lv", "Lettisch", "Latvian", true, true),
    l("lt", "Litauisch", "Lithuanian", true, true),
    l("ms", "Malaiisch", "Malay", true, true),
    l("ml", "Malayalam", "Malayalam", true, true),
    l("mt", "Maltesisch", "Maltese", false, true),
    l("mi", "Maori", "Maori", false, true),
    l("mr", "Marathi", "Marathi", true, true),
    l("mk", "Mazedonisch", "Macedonian", true, true),
    l("mn", "Mongolisch", "Mongolian", true, true),
    l("ne", "Nepali", "Nepali", true, true),
    l("nl", "Niederländisch", "Dutch", true, true),
    l("no", "Norwegisch", "Norwegian", true, true),
    l("or", "Oriya", "Odia", false, true),
    l("om", "Oromo", "Oromo", false, true),
    l("pa", "Panjabi", "Punjabi", true, true),
    l("ps", "Paschtu", "Pashto", false, true),
    l("fa", "Persisch", "Persian", true, true),
    l("pl", "Polnisch", "Polish", true, true),
    l(
        "pt-BR",
        "Portugiesisch (Brasilien)",
        "Brazilian Portuguese",
        true,
        true,
    ),
    l(
        "pt-PT",
        "Portugiesisch (Portugal)",
        "European Portuguese",
        true,
        true,
    ),
    l("qu", "Quechua", "Quechua", false, true),
    l("rm", "Rätoromanisch", "Romansh", false, true),
    l("ro", "Rumänisch", "Romanian", true, true),
    l("ru", "Russisch", "Russian", true, true),
    l("sv", "Schwedisch", "Swedish", true, true),
    l("sr", "Serbisch", "Serbian", true, true),
    l("st", "Sesotho", "Southern Sotho", false, true),
    l("tn", "Setswana", "Tswana", false, true),
    l("sd", "Sindhi", "Sindhi", true, true),
    l("si", "Singhalesisch", "Sinhala", true, true),
    l("sk", "Slowakisch", "Slovak", true, true),
    l("sl", "Slowenisch", "Slovenian", true, true),
    l("so", "Somali", "Somali", false, true),
    l("es", "Spanisch", "Spanish", true, true),
    l("sw", "Suaheli", "Swahili", true, true),
    l("su", "Sundanesisch", "Sundanese", true, false),
    l("tg", "Tadschikisch", "Tajik", false, true),
    l("ta", "Tamil", "Tamil", true, true),
    l("te", "Telugu", "Telugu", true, true),
    l("th", "Thai", "Thai", true, true),
    l("cs", "Tschechisch", "Czech", true, true),
    l("tr", "Türkisch", "Turkish", true, true),
    l("tk", "Turkmenisch", "Turkmen", false, true),
    l("uk", "Ukrainisch", "Ukrainian", true, true),
    l("hu", "Ungarisch", "Hungarian", true, true),
    l("ur", "Urdu", "Urdu", true, true),
    l("uz", "Usbekisch", "Uzbek", true, true),
    l("vi", "Vietnamesisch", "Vietnamese", true, true),
    l("cy", "Walisisch", "Welsh", false, true),
    l("fy", "Westfriesisch", "Western Frisian", false, true),
    l("wo", "Wolof", "Wolof", false, true),
    l("yo", "Yoruba", "Yoruba", false, true),
    l("zu", "Zulu", "Zulu", true, true),
];

/// Sprache per Code suchen (exakt, Gross-/Kleinschreibung egal).
pub fn find(code: &str) -> Option<Language> {
    LANGUAGES
        .iter()
        .copied()
        .find(|l| l.code.eq_ignore_ascii_case(code))
}

/// Englischer Name einer Sprache (fuer die Live-Systemanweisung); unbekannt →
/// der Code selbst.
pub fn english_name(code: &str) -> &str {
    find(code).map_or(code, |l| l.en)
}

/// Kann die Sprache vom Uebersetzungsmodell uebersetzt werden?
pub fn is_translate(code: &str) -> bool {
    find(code).is_some_and(|l| l.translate)
}

/// Kann die Sprache vom Live-Konversationsmodell gesprochen werden?
pub fn is_live(code: &str) -> bool {
    find(code).is_some_and(|l| l.live)
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
        assert_eq!(LANGUAGES.len(), 101, "101 eintraege");
        let codes: HashSet<_> = LANGUAGES.iter().map(|l| l.code).collect();
        assert_eq!(codes.len(), 101, "doppelter code");
        let names: HashSet<_> = LANGUAGES.iter().map(|l| l.name).collect();
        assert_eq!(names.len(), 101, "doppelter deutscher name");
        let en: HashSet<_> = LANGUAGES.iter().map(|l| l.en).collect();
        assert_eq!(en.len(), 101, "doppelter englischer name");
    }

    #[test]
    fn nach_deutschem_namen_sortiert() {
        // Deutsche Sortierung: Gross-/Kleinschreibung egal, Umlaute wie ihr
        // Grundvokal (ä→a, ö→o, ü→u, ß→ss).
        fn key(n: &str) -> String {
            n.to_lowercase()
                .replace('ä', "a")
                .replace('ö', "o")
                .replace('ü', "u")
                .replace('ß', "ss")
        }
        let mut sortiert = LANGUAGES.to_vec();
        sortiert.sort_by_key(|a| key(a.name));
        let orig: Vec<_> = LANGUAGES.iter().map(|l| l.name).collect();
        let soll: Vec<_> = sortiert.iter().map(|l| l.name).collect();
        assert_eq!(orig, soll, "tabelle ist nach deutschem Namen sortiert");
    }

    #[test]
    fn mengen_78_translate_99_live() {
        let translate = LANGUAGES.iter().filter(|l| l.translate).count();
        let live = LANGUAGES.iter().filter(|l| l.live).count();
        assert_eq!(translate, 78, "78 translate-sprachen");
        assert_eq!(live, 99, "99 live-sprachen");
    }

    #[test]
    fn jv_und_su_nur_translate() {
        for code in ["jv", "su"] {
            let x = find(code).unwrap();
            assert!(x.translate, "{code} ist translate");
            assert!(!x.live, "{code} ist NICHT live");
        }
    }

    #[test]
    fn die_23_nur_live() {
        // Genau diese 23 Codes sind nur-Live (translate=false, live=true).
        let nur_live = [
            "as", "bs", "ceb", "cy", "fo", "fy", "ga", "ku", "ky", "mi", "mt", "om", "or", "ps",
            "qu", "rm", "so", "st", "tg", "tk", "tn", "wo", "yo",
        ];
        assert_eq!(nur_live.len(), 23);
        for code in nur_live {
            let x = find(code).unwrap_or_else(|| panic!("{code} fehlt"));
            assert!(!x.translate, "{code} ist NICHT translate");
            assert!(x.live, "{code} ist live");
        }
        // Es gibt genau 23 nur-Live-Sprachen in der Tabelle.
        let count = LANGUAGES.iter().filter(|l| !l.translate && l.live).count();
        assert_eq!(count, 23, "genau 23 nur-live");
    }

    #[test]
    fn defaults_sind_deutsch_und_chinesisch() {
        let de = find(DEFAULT_LANG).unwrap();
        assert_eq!(de.name, "Deutsch");
        assert_eq!(de.en, "German");
        assert!(de.translate && de.live);
        assert_eq!(
            find(DEFAULT_TARGET).unwrap().name,
            "Chinesisch (vereinfacht)"
        );
    }

    #[test]
    fn englische_namen_und_flags() {
        assert_eq!(english_name("zh-Hans"), "Simplified Chinese");
        assert_eq!(english_name("de"), "German");
        assert_eq!(english_name("xx"), "xx"); // unbekannt → Code
        assert!(is_translate("de") && is_live("de"));
        assert!(is_translate("jv") && !is_live("jv"));
        assert!(!is_translate("cy") && is_live("cy"));
        assert!(!is_translate("xx") && !is_live("xx"));
    }

    #[test]
    fn canonical_normalisiert() {
        assert_eq!(canonical("ZH-hans", "en"), "zh-Hans");
        assert_eq!(canonical("pt-br", "en"), "pt-BR");
        assert_eq!(canonical("xx", DEFAULT_TARGET), "zh-Hans");
        assert_eq!(canonical("", "de"), "de");
    }
}
