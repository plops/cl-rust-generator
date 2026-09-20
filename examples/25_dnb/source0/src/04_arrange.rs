//! 04_arrange: Sektions-State-Machine (nur an Bar-Grenzen schalten).
//! Form: Intro 4 / A 16 / Bridge 8 / Riser 4 / Tearout 16 / Outro 4 = 52 Bars.
//! `ARR_VER = 1` (s. plan.md Kap. 5).

/// Arrangement-Format-Version.
pub const ARR_VER: u32 = 1;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Section {
    Intro,
    Rolling,
    Bridge,
    Riser,
    Tearout,
    Outro,
}

/// Sektion fuer 0-basierten Takt-Index.
pub fn section_at(bar: u32) -> Section {
    match bar {
        0..=3 => Section::Intro,
        4..=19 => Section::Rolling,
        20..=27 => Section::Bridge,
        28..=31 => Section::Riser,
        32..=47 => Section::Tearout,
        _ => Section::Outro,
    }
}

/// Relative Sektions-Lautheit (Pegel-Automatisierung, linear).
pub fn section_gain(section: Section) -> f32 {
    match section {
        Section::Intro => 0.5,
        Section::Rolling => 0.9,
        Section::Bridge => 0.6,
        Section::Riser => 0.75,
        Section::Tearout => 1.0,
        Section::Outro => 0.4,
    }
}

/// Half-time-Feel in der Bridge: nur jeder zweite 16th triggert Drums.
pub fn drum_active(section: Section, step: usize) -> bool {
    match section {
        Section::Bridge => step.is_multiple_of(2),
        Section::Intro | Section::Outro => step.is_multiple_of(4),
        _ => true,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn boundaries_match_form() {
        assert_eq!(section_at(0), Section::Intro);
        assert_eq!(section_at(4), Section::Rolling);
        assert_eq!(section_at(20), Section::Bridge);
        assert_eq!(section_at(28), Section::Riser);
        assert_eq!(section_at(32), Section::Tearout);
        assert_eq!(section_at(48), Section::Outro);
    }

    #[test]
    fn bridge_is_half_density() {
        let full = (0..16)
            .filter(|s| drum_active(Section::Rolling, *s))
            .count();
        let half = (0..16).filter(|s| drum_active(Section::Bridge, *s)).count();
        assert_eq!(full, 16);
        assert_eq!(half, 8);
    }

    #[test]
    fn tearout_is_loudest() {
        for s in [
            Section::Intro,
            Section::Rolling,
            Section::Bridge,
            Section::Riser,
            Section::Outro,
        ] {
            assert!(section_gain(Section::Tearout) >= section_gain(s));
        }
    }
}
