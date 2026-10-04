//! 04_scoring: reiner Weighted-Scoring-Algorithmus (0–100 %).
//!
//! Absichtlich DB-frei (reine Funktionen über `ScoreProfile`), damit der
//! Algorithmus exhaustiv per Unit-Test prüfbar ist.
//!
//! * Harte Filter (Dealbreaker): Altersfenster, gegenseitige
//!   Geschlechtspräferenz, Familienplanung → sonst Score 0.
//! * MBTI-Kompatibilität: max. 35 Punkte (Temperament-Gruppen).
//! * Hobby-Schnittmenge (Jaccard): max. 35 Punkte.
//! * Hidden-Passung (Einkommen/Erwartung + Vermögen): max. 30 Punkte.
//!
//! Der Score ist bewusst RICHTUNGSABHÄNGIG (Erwartung des Suchenden), daher
//! nicht symmetrisch. Intime Freitext-Präferenzen fließen in MVP v1 NICHT in
//! den Score ein (nur verschlüsselt gespeichert) — dokumentierte
//! Privacy-Entscheidung.

/// Max. Altersdifferenz für harte Filter (MVP-Vereinfachung, konfigurierbar).
pub const MAX_AGE_GAP: i32 = 10;

/// Entschlüsselte Sicht eines Profils für den Algorithmus.
#[derive(Debug, Clone)]
pub struct ScoreProfile {
    pub age: i32,
    pub gender: String,
    pub looking_for: String,
    pub family_plan: String,
    pub mbti: String,
    pub hobbies: Vec<String>,
    pub income_band: String,
    pub wealth_band: String,
    pub income_expectation: String,
}

/// Gesamt-Score aus Sicht von `seeker` für `candidate` (0–100).
pub fn compatibility_score(seeker: &ScoreProfile, candidate: &ScoreProfile) -> u8 {
    if !hard_filters_pass(seeker, candidate) {
        return 0;
    }
    let total = mbti_points(&seeker.mbti, &candidate.mbti)
        + hobby_points(&seeker.hobbies, &candidate.hobbies)
        + hidden_points(seeker, candidate);
    total.min(100) as u8
}

pub fn hard_filters_pass(seeker: &ScoreProfile, candidate: &ScoreProfile) -> bool {
    if (seeker.age - candidate.age).abs() > MAX_AGE_GAP {
        return false;
    }
    // Gegenseitige Geschlechtspräferenz (MVP: exakte Übereinstimmung).
    if candidate.gender != seeker.looking_for || seeker.gender != candidate.looking_for {
        return false;
    }
    if !family_compatible(&seeker.family_plan, &candidate.family_plan) {
        return false;
    }
    true
}

pub fn family_compatible(a: &str, b: &str) -> bool {
    a == b || a == "egal" || b == "egal"
}

/// MBTI-Temperament-Gruppe: 0 Analysten (NT), 1 Diplomaten (NF),
/// 2 Sentinels (SJ), 3 Explorers (SP). Unbekannt → None (neutral).
pub fn mbti_group(mbti: &str) -> Option<u8> {
    match mbti {
        "INTJ" | "INTP" | "ENTJ" | "ENTP" => Some(0),
        "INFJ" | "INFP" | "ENFJ" | "ENFP" => Some(1),
        "ISTJ" | "ISFJ" | "ESTJ" | "ESFJ" => Some(2),
        "ISTP" | "ISFP" | "ESTP" | "ESFP" => Some(3),
        _ => None,
    }
}

/// MBTI-Punkte (0–35): identisch 35, gleiche Gruppe 28, verwandte
/// Gruppen (NT↔NF, SJ↔SP) 20, sonst 12, unbekannt 12.
pub fn mbti_points(a: &str, b: &str) -> u32 {
    if a == b && mbti_group(a).is_some() {
        return 35;
    }
    match (mbti_group(a), mbti_group(b)) {
        (Some(x), Some(y)) if x == y => 28,
        (Some(0), Some(1)) | (Some(1), Some(0)) | (Some(2), Some(3)) | (Some(3), Some(2)) => 20,
        (Some(_), Some(_)) => 12,
        _ => 12,
    }
}

/// Hobby-Punkte (0–35) via Jaccard-Ähnlichkeit (case-insensitiv).
pub fn hobby_points(a: &[String], b: &[String]) -> u32 {
    use std::collections::HashSet;
    let norm = |xs: &[String]| {
        xs.iter()
            .map(|s| s.trim().to_lowercase())
            .collect::<HashSet<_>>()
    };
    let (sa, sb) = (norm(a), norm(b));
    if sa.is_empty() && sb.is_empty() {
        return 12; // neutral, kein Malus für leere Listen
    }
    if sa.is_empty() || sb.is_empty() {
        return 5;
    }
    let inter = sa.intersection(&sb).count() as f64;
    let union = sa.union(&sb).count() as f64;
    ((inter / union) * 35.0).round() as u32
}

fn band_rank(band: &str) -> Option<i32> {
    match band {
        "low" => Some(0),
        "medium" => Some(1),
        "high" => Some(2),
        _ => None,
    }
}

/// Hidden-Punkte (0–30): Einkommens-Fit (max 18) + Vermögens-Nähe (max 12).
pub fn hidden_points(seeker: &ScoreProfile, candidate: &ScoreProfile) -> u32 {
    let income = if seeker.income_expectation == "any" {
        12
    } else {
        match (
            band_rank(&seeker.income_expectation),
            band_rank(&candidate.income_band),
        ) {
            (Some(e), Some(c)) if e == c => 18,
            (Some(e), Some(c)) if (e - c).abs() == 1 => 12,
            (Some(_), Some(_)) => 4,
            _ => 8, // unbekannte Stufe: neutral
        }
    };
    let wealth = match (
        band_rank(&seeker.wealth_band),
        band_rank(&candidate.wealth_band),
    ) {
        (Some(a), Some(b)) if a == b => 12,
        (Some(a), Some(b)) if (a - b).abs() == 1 => 8,
        (Some(_), Some(_)) => 3,
        _ => 6,
    };
    income + wealth
}

#[cfg(test)]
mod tests {
    use super::*;

    fn profile() -> ScoreProfile {
        ScoreProfile {
            age: 30,
            gender: "w".into(),
            looking_for: "m".into(),
            family_plan: "kinderwunsch".into(),
            mbti: "INFJ".into(),
            hobbies: vec!["klettern".into(), "kochen".into()],
            income_band: "medium".into(),
            wealth_band: "medium".into(),
            income_expectation: "any".into(),
        }
    }

    fn candidate_for(seeker: &ScoreProfile) -> ScoreProfile {
        let mut c = seeker.clone();
        c.gender = seeker.looking_for.clone();
        c.looking_for = seeker.gender.clone();
        c
    }

    #[test]
    fn identical_compatible_profiles_score_very_high() {
        let seeker = profile();
        let candidate = candidate_for(&seeker);
        let score = compatibility_score(&seeker, &candidate);
        assert!(score >= 90, "expected >= 90, got {score}");
    }

    #[test]
    fn age_gap_dealbreaker_yields_zero() {
        let seeker = profile();
        let mut candidate = candidate_for(&seeker);
        candidate.age = seeker.age + MAX_AGE_GAP + 1;
        assert_eq!(compatibility_score(&seeker, &candidate), 0);
    }

    #[test]
    fn age_gap_boundary_passes() {
        let seeker = profile();
        let mut candidate = candidate_for(&seeker);
        candidate.age = seeker.age + MAX_AGE_GAP;
        assert!(compatibility_score(&seeker, &candidate) > 0);
    }

    #[test]
    fn gender_mismatch_yields_zero() {
        let seeker = profile();
        let mut candidate = candidate_for(&seeker);
        candidate.gender = "w".into(); // Suchende sucht "m"
        assert_eq!(compatibility_score(&seeker, &candidate), 0);
    }

    #[test]
    fn one_sided_preference_yields_zero() {
        let seeker = profile(); // w, sucht m
        let mut candidate = candidate_for(&seeker); // m, sucht w
        candidate.looking_for = "m".into(); // sucht Männer, also nicht die Suchende
        assert_eq!(compatibility_score(&seeker, &candidate), 0);
    }

    #[test]
    fn family_plan_conflict_yields_zero() {
        let seeker = profile();
        let mut candidate = candidate_for(&seeker);
        candidate.family_plan = "keine_kinder".into();
        assert_eq!(compatibility_score(&seeker, &candidate), 0);
    }

    #[test]
    fn family_plan_egal_is_compatible() {
        assert!(family_compatible("egal", "kinderwunsch"));
        assert!(family_compatible("keine_kinder", "egal"));
        assert!(!family_compatible("kinderwunsch", "keine_kinder"));
    }

    #[test]
    fn mbti_matrix_spots() {
        assert_eq!(mbti_points("INFJ", "INFJ"), 35);
        assert_eq!(mbti_points("INFJ", "ENFP"), 28); // gleiche Gruppe NF
        assert_eq!(mbti_points("INTJ", "INFJ"), 20); // NT <-> NF verwandt
        assert_eq!(mbti_points("ISTJ", "ESTP"), 20); // SJ <-> SP verwandt
        assert_eq!(mbti_points("INTJ", "ISTJ"), 12); // fremde Gruppen
        assert_eq!(mbti_points("XXXX", "INFJ"), 12); // unbekannt neutral
    }

    #[test]
    fn disjoint_hobbies_score_lower_than_shared() {
        let seeker = profile();
        let mut shared = candidate_for(&seeker);
        shared.hobbies = vec!["klettern".into(), "kochen".into(), "lesen".into()];
        let mut disjoint = candidate_for(&seeker);
        disjoint.hobbies = vec!["gaming".into(), "angeln".into()];
        assert_eq!(hobby_points(&seeker.hobbies, &shared.hobbies), 23); // 2/3*35
        assert_eq!(hobby_points(&seeker.hobbies, &disjoint.hobbies), 0);
        assert!(compatibility_score(&seeker, &shared) > compatibility_score(&seeker, &disjoint));
    }

    #[test]
    fn income_expectation_fit_beats_mismatch() {
        let mut seeker = profile();
        seeker.income_expectation = "high".into();
        let mut rich = candidate_for(&seeker);
        rich.income_band = "high".into();
        let mut poor = candidate_for(&seeker);
        poor.income_band = "low".into();
        assert!(
            hidden_points(&seeker, &rich) > hidden_points(&seeker, &poor),
            "rich={} poor={}",
            hidden_points(&seeker, &rich),
            hidden_points(&seeker, &poor)
        );
    }

    #[test]
    fn score_is_deterministic() {
        let seeker = profile();
        let candidate = candidate_for(&seeker);
        assert_eq!(
            compatibility_score(&seeker, &candidate),
            compatibility_score(&seeker, &candidate)
        );
    }

    #[test]
    fn score_always_in_range_property() {
        // Kleiner deterministischer Property-Sweep über Eingabekombinationen.
        let genders = ["w", "m", "d"];
        let plans = ["kinderwunsch", "keine_kinder", "egal", "???"];
        let mbtis = ["INTJ", "ENFP", "ISTJ", "ESTP", "??"];
        let bands = ["low", "medium", "high", "", "???"];
        let hobby_sets: &[Vec<String>] = &[vec![], vec!["a".into()], vec!["a".into(), "b".into()]];
        let mut count = 0;
        for (i, g) in genders.iter().enumerate() {
            for plan in &plans {
                for mbti in &mbtis {
                    for band in &bands {
                        for hobbies in hobby_sets {
                            let seeker = ScoreProfile {
                                age: 20 + (i as i32 * 7),
                                gender: (*g).into(),
                                looking_for: "m".into(),
                                family_plan: (*plan).into(),
                                mbti: (*mbti).into(),
                                hobbies: hobbies.clone(),
                                income_band: (*band).into(),
                                wealth_band: (*band).into(),
                                income_expectation: (*band).into(),
                            };
                            let candidate = ScoreProfile {
                                age: 25 + (count % 30),
                                gender: "m".into(),
                                looking_for: (*g).into(),
                                family_plan: "egal".into(),
                                mbti: "ENFJ".into(),
                                hobbies: vec!["b".into(), "c".into()],
                                income_band: "medium".into(),
                                wealth_band: "high".into(),
                                income_expectation: "any".into(),
                            };
                            let s = compatibility_score(&seeker, &candidate);
                            assert!(s <= 100, "score out of range: {s}");
                            count += 1;
                        }
                    }
                }
            }
        }
        // 3 genders × 4 plans × 5 mbtis × 5 bands × 3 hobby-sets = 900.
        assert!(count >= 900, "sweep covered {count} combos");
    }
}
