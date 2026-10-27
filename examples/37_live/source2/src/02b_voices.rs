//! 02b — Stimmenwahl und Modell-Entscheidung (Option 3).
//!
//! Jeder Nutzer waehlt SEINE Sprecherstimme — die Stimme, mit der der Partner
//! die Uebersetzung hoert. Daraus (zusammen mit beiden Sprachen) folgt, welches
//! Gemini-Modell die Sitzung fuehrt:
//!
//! - „Automatisch" (`AUTO`): das Uebersetzungsmodell
//!   `gemini-3.5-live-translate-preview` bildet die eigene Stimme nach (Voice
//!   Replication, fortlaufend, schnell). Geht nur, wenn BEIDE Sprachen
//!   `translate` koennen.
//! - Eine feste Stimme: das Konversationsmodell `gemini-3.8-live` mit
//!   Dolmetsch-Systemanweisung spricht in genau dieser Stimme (antwortet aber
//!   erst nach einer Sprechpause). Geht nur, wenn BEIDE Sprachen `live` koennen.
//!
//! Fallbacks, wenn das Wunschmodell eine der beiden Sprachen nicht kann:
//! - `AUTO`, aber eine Sprache ist NICHT `translate` (nur-Live wie `cy`): auf
//!   das Live-Modell ausweichen und eine Stimme per `default_voice` bzw.
//!   ersatzweise `Zephyr` waehlen.
//! - Feste Stimme, aber eine Sprache ist NICHT `live` (`jv`/`su`): auf das
//!   Translate-Modell ausweichen (die Stimme wird dort ignoriert).
//! - Keins von beidem moeglich: Fehler.
//!
//! Default-Stimmen: Chinesisch (`zh-*`) → weiblich `Kore`, Deutsch (`de`) →
//! maennlich `Charon`, sonst `AUTO`.

use crate::languages;

/// Marker fuer „Automatisch (eigene Stimme nachbilden)".
pub const AUTO: &str = "auto";

/// Ersatzstimme, wenn `AUTO` aufs Live-Modell ausweichen muss und die
/// eigene Sprache keine Default-Stimme hat.
pub const FALLBACK_VOICE: &str = "Zephyr";

/// Geschlecht einer Prebuilt-Stimme (fuer die Frontend-Gruppierung).
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize)]
#[serde(rename_all = "lowercase")]
pub enum Gender {
    Female,
    Male,
}

/// Eine Prebuilt-Stimme: Name und Geschlecht.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize)]
pub struct Voice {
    pub name: &'static str,
    pub gender: Gender,
}

const fn v(name: &'static str, gender: Gender) -> Voice {
    Voice { name, gender }
}

/// Die 30 Prebuilt-Stimmen (Geschlecht laut Chirp3-HD-Tabelle / GET
/// `/v1beta/voices`), weiblich zuerst, dann maennlich — je alphabetisch.
pub const VOICES: [Voice; 30] = [
    v("Achernar", Gender::Female),
    v("Aoede", Gender::Female),
    v("Autonoe", Gender::Female),
    v("Callirrhoe", Gender::Female),
    v("Despina", Gender::Female),
    v("Erinome", Gender::Female),
    v("Gacrux", Gender::Female),
    v("Kore", Gender::Female),
    v("Laomedeia", Gender::Female),
    v("Leda", Gender::Female),
    v("Pulcherrima", Gender::Female),
    v("Sulafat", Gender::Female),
    v("Vindemiatrix", Gender::Female),
    v("Zephyr", Gender::Female),
    v("Achird", Gender::Male),
    v("Algenib", Gender::Male),
    v("Algieba", Gender::Male),
    v("Alnilam", Gender::Male),
    v("Charon", Gender::Male),
    v("Enceladus", Gender::Male),
    v("Fenrir", Gender::Male),
    v("Iapetus", Gender::Male),
    v("Orus", Gender::Male),
    v("Puck", Gender::Male),
    v("Rasalgethi", Gender::Male),
    v("Sadachbia", Gender::Male),
    v("Sadaltager", Gender::Male),
    v("Schedar", Gender::Male),
    v("Umbriel", Gender::Male),
    v("Zubenelgenubi", Gender::Male),
];

/// Stimme per Name suchen (exakt, Gross-/Kleinschreibung egal).
pub fn find(name: &str) -> Option<Voice> {
    VOICES
        .iter()
        .copied()
        .find(|v| v.name.eq_ignore_ascii_case(name))
}

/// Default-Stimme fuer eine Sprache: immer `AUTO` (Uebersetzungsmodell).
///
/// Nutzer-Entscheidung nach dem HIL (2026-10-27): feste Stimmen laufen ueber
/// `gemini-3.8-live` und sind dort unzuverlaessig (Saetze fallen weg,
/// Latenz 5–25 s), daher ist „Automatisch" fuer alle der Default; feste
/// Stimmen bleiben als experimentelle Option waehlbar.
pub fn default_voice(_lang: &str) -> &'static str {
    AUTO
}

/// Bevorzugte feste Stimme, falls das Live-Modell ohnehin noetig ist
/// (AUTO, aber eine Sprache nur live): Chinesisch (`zh-*`) → `Kore`
/// (weiblich), Deutsch (`de`) → `Charon` (maennlich), sonst `AUTO`.
pub fn preferred_fixed_voice(lang: &str) -> &'static str {
    let l = lang.to_ascii_lowercase();
    if l.starts_with("zh") {
        "Kore"
    } else if l == "de" {
        "Charon"
    } else {
        AUTO
    }
}

/// Welches Gemini-Modell fuehrt die Sitzung?
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Model {
    /// Uebersetzungsmodell (Voice Replication, fortlaufend).
    Translate,
    /// Konversationsmodell `gemini-3.8-live` mit fester Stimme.
    Live,
}

/// Gewuenschtes Modell samt Stimme aus Sprachpaar und Stimmenwunsch bestimmen.
///
/// `lang` = eigene Sprache (Quelle), `partner_lang` = Ziel (Partner-Sprache),
/// `voice` = `AUTO`/leer/unbekannt → Automatisch, sonst ein Stimmenname.
///
/// Rueckgabe: das Modell und — nur bei `Live` — die zu nutzende Stimme.
/// `Err`, wenn weder Translate noch Live beide Sprachen abdecken.
pub fn choose(
    lang: &str,
    partner_lang: &str,
    voice: &str,
) -> Result<(Model, Option<&'static str>), String> {
    let both_translate = languages::is_translate(lang) && languages::is_translate(partner_lang);
    let both_live = languages::is_live(lang) && languages::is_live(partner_lang);

    // Unbekannte Stimme wie AUTO behandeln.
    let fixed = (!voice.eq_ignore_ascii_case(AUTO) && !voice.is_empty())
        .then(|| find(voice))
        .flatten();

    match fixed {
        // Automatisch (kein/auto/unbekannter Stimmenwunsch).
        None => {
            if both_translate {
                Ok((Model::Translate, None))
            } else if both_live {
                // Translate geht nicht (z. B. nur-Live-Sprache): aufs
                // Live-Modell ausweichen und eine konsistente Stimme waehlen.
                Ok((Model::Live, Some(fallback_voice(lang))))
            } else {
                Err(format!(
                    "kein gemeinsames modell fuer {lang} ↔ {partner_lang}"
                ))
            }
        }
        // Feste Stimme gewuenscht.
        Some(chosen) => {
            if both_live {
                Ok((Model::Live, Some(chosen.name)))
            } else if both_translate {
                // Live geht nicht (z. B. jv/su): aufs Translate-Modell
                // ausweichen; die feste Stimme wird dort ignoriert.
                Ok((Model::Translate, None))
            } else {
                Err(format!(
                    "kein gemeinsames modell fuer {lang} ↔ {partner_lang}"
                ))
            }
        }
    }
}

/// Stimme fuer den AUTO→Live-Fallback: die bevorzugte feste Stimme der
/// eigenen Sprache, sonst `FALLBACK_VOICE`.
fn fallback_voice(lang: &str) -> &'static str {
    let dv = preferred_fixed_voice(lang);
    if dv == AUTO { FALLBACK_VOICE } else { dv }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    #[test]
    fn dreissig_stimmen_eindeutig_14_weiblich_16_maennlich() {
        assert_eq!(VOICES.len(), 30);
        let namen: HashSet<_> = VOICES.iter().map(|v| v.name).collect();
        assert_eq!(namen.len(), 30, "doppelte stimme");
        let weiblich = VOICES.iter().filter(|v| v.gender == Gender::Female).count();
        let maennlich = VOICES.iter().filter(|v| v.gender == Gender::Male).count();
        assert_eq!(weiblich, 14);
        assert_eq!(maennlich, 16);
    }

    #[test]
    fn default_voice_regeln() {
        // Default ist fuer alle Sprachen Automatisch (Uebersetzungsmodell).
        for l in ["zh-Hans", "zh-Hant", "de", "en", "cy"] {
            assert_eq!(default_voice(l), AUTO, "{l}");
        }
        // Bevorzugte feste Stimme nur fuer den Live-Fallback.
        assert_eq!(preferred_fixed_voice("zh-Hans"), "Kore");
        assert_eq!(preferred_fixed_voice("zh-Hant"), "Kore");
        assert_eq!(preferred_fixed_voice("ZH-HANS"), "Kore");
        assert_eq!(preferred_fixed_voice("de"), "Charon");
        assert_eq!(preferred_fixed_voice("en"), AUTO);
    }

    #[test]
    fn auto_beide_translate_ergibt_translate() {
        // de ↔ en: beide translate → Translate, keine Stimme.
        assert_eq!(choose("de", "en", AUTO).unwrap(), (Model::Translate, None));
        assert_eq!(choose("de", "en", "").unwrap(), (Model::Translate, None));
        // Unbekannte Stimme → wie AUTO.
        assert_eq!(
            choose("de", "en", "GibtsNicht").unwrap(),
            (Model::Translate, None)
        );
    }

    #[test]
    fn feste_stimme_beide_live_ergibt_live() {
        assert_eq!(
            choose("de", "en", "Charon").unwrap(),
            (Model::Live, Some("Charon"))
        );
        assert_eq!(
            choose("zh-Hans", "de", "Kore").unwrap(),
            (Model::Live, Some("Kore"))
        );
        // Gross-/Kleinschreibung der Stimme egal, kanonischer Name zurueck.
        assert_eq!(
            choose("de", "en", "kore").unwrap(),
            (Model::Live, Some("Kore"))
        );
    }

    #[test]
    fn auto_aber_nur_live_sprache_weicht_auf_live_aus() {
        // cy (Walisisch) ist nur-Live → Translate unmoeglich, Live-Fallback.
        // Eigene Sprache de hat Default-Stimme Charon.
        assert_eq!(
            choose("de", "cy", AUTO).unwrap(),
            (Model::Live, Some("Charon"))
        );
        // Eigene Sprache ohne Default-Stimme → FALLBACK_VOICE (Zephyr).
        assert_eq!(
            choose("en", "cy", AUTO).unwrap(),
            (Model::Live, Some(FALLBACK_VOICE))
        );
        // Eigene Sprache zh-Hans → Kore.
        assert_eq!(
            choose("zh-Hans", "cy", AUTO).unwrap(),
            (Model::Live, Some("Kore"))
        );
    }

    #[test]
    fn feste_stimme_aber_nur_translate_sprache_weicht_auf_translate_aus() {
        // jv (Javanisch) ist nur-Translate → Live unmoeglich, Translate-
        // Fallback; die feste Stimme wird ignoriert (None).
        assert_eq!(
            choose("de", "jv", "Kore").unwrap(),
            (Model::Translate, None)
        );
        assert_eq!(
            choose("jv", "de", "Charon").unwrap(),
            (Model::Translate, None)
        );
    }

    #[test]
    fn kein_gemeinsames_modell_ist_fehler() {
        // jv (nur translate) ↔ cy (nur live): kein gemeinsames Modell.
        assert!(choose("jv", "cy", AUTO).is_err());
        assert!(choose("jv", "cy", "Kore").is_err());
        assert!(choose("cy", "jv", AUTO).is_err());
    }
}
