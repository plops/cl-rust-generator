//! `15_ui_state` — reine Tasten-Logik (ohne Fenster, testbar).
//!
//! `UiState` = `Engine`-Einstellung + Anzeigezustand; `apply` führt eine
//! `Action` aus und meldet Kanten-Effekte (`quit`/`step`/`clear`), die
//! die Schleife sofort verbraucht.

use crate::engine::Settings;
use crate::generate::GenMode;
use crate::lang::LANGS;
use crate::models::ModelChoice;

/// Schriftgrößen-Stufen (Unifont: 16-px-Bitmap → Vielfache scharf).
pub const SIZES: &[u32] = &[16, 24, 32, 48];

/// Anzeige-Modus (Taste `V`).
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum ViewMode {
    /// Nur HUD (kein Bild).
    Hidden,
    /// Gerendertes Bild.
    #[default]
    Text,
    /// Bild + Detektions-Boxen.
    Boxes,
}

/// Tasten-Aktion.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Action {
    /// `→`: nächste Sprache.
    NextLang,
    /// `←`: vorige Sprache.
    PrevLang,
    /// `R`: Zufallssprache an/aus.
    ToggleRandom,
    /// `G`: nächster Generator.
    NextGen,
    /// `V`: nächster Anzeige-Modus.
    NextView,
    /// `↑`: größer.
    Bigger,
    /// `↓`: kleiner.
    Smaller,
    /// `M`: auto ↔ universal.
    ToggleModel,
    /// `Space`: Pause an/aus.
    TogglePause,
    /// `N`: ein Sample (auch in Pause).
    Step,
    /// `C`: Statistik löschen.
    Clear,
    /// `Esc`/`Q`: Ende.
    Quit,
}

/// Kanten-Effekte einer Aktion.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Effects {
    /// Beenden + Report.
    pub quit: bool,
    /// Ein Sample ausführen.
    pub step: bool,
    /// Statistik löschen.
    pub clear: bool,
}

/// UI-Zustand.
#[derive(Clone, Debug)]
pub struct UiState {
    /// Engine-Einstellung.
    pub settings: Settings,
    /// Anzeige-Modus.
    pub view: ViewMode,
    /// Zufallssprache pro Sample.
    pub random_lang: bool,
    /// Angehalten.
    pub paused: bool,
}

impl Default for UiState {
    fn default() -> Self {
        Self {
            settings: Settings::default(),
            view: ViewMode::Text,
            random_lang: false,
            paused: false,
        }
    }
}

/// Führt `action` aus; gibt Kanten-Effekte zurück.
pub fn apply(state: &mut UiState, action: Action) -> Effects {
    let mut fx = Effects::default();
    match action {
        Action::NextLang => state.settings.lang = (state.settings.lang + 1) % LANGS.len(),
        Action::PrevLang => {
            state.settings.lang = (state.settings.lang + LANGS.len() - 1) % LANGS.len();
        }
        Action::ToggleRandom => state.random_lang = !state.random_lang,
        Action::NextGen => {
            let i = GenMode::ALL
                .iter()
                .position(|&m| m == state.settings.mode)
                .unwrap_or(0);
            state.settings.mode = GenMode::ALL[(i + 1) % GenMode::ALL.len()];
        }
        Action::NextView => {
            state.view = match state.view {
                ViewMode::Hidden => ViewMode::Text,
                ViewMode::Text => ViewMode::Boxes,
                ViewMode::Boxes => ViewMode::Hidden,
            };
        }
        Action::Bigger => {
            state.settings.px = SIZES
                .iter()
                .copied()
                .find(|&s| s > state.settings.px)
                .unwrap_or(48);
        }
        Action::Smaller => {
            state.settings.px = SIZES
                .iter()
                .copied()
                .rev()
                .find(|&s| s < state.settings.px)
                .unwrap_or(16);
        }
        Action::ToggleModel => {
            state.settings.model = match state.settings.model {
                ModelChoice::Auto => ModelChoice::Universal,
                ModelChoice::Universal => ModelChoice::Auto,
            };
        }
        Action::TogglePause => state.paused = !state.paused,
        Action::Step => fx.step = true,
        Action::Clear => fx.clear = true,
        Action::Quit => fx.quit = true,
    }
    fx
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn language_cycles_both_ways() {
        let mut s = UiState::default();
        s.settings.lang = LANGS.len() - 1;
        apply(&mut s, Action::NextLang);
        assert_eq!(s.settings.lang, 0);
        apply(&mut s, Action::PrevLang);
        assert_eq!(s.settings.lang, LANGS.len() - 1);
    }

    #[test]
    fn sizes_clamp_at_ends() {
        let mut s = UiState::default();
        s.settings.px = 48;
        apply(&mut s, Action::Bigger);
        assert_eq!(s.settings.px, 48);
        s.settings.px = 16;
        apply(&mut s, Action::Smaller);
        assert_eq!(s.settings.px, 16);
        s.settings.px = 32;
        apply(&mut s, Action::Bigger);
        assert_eq!(s.settings.px, 48);
        apply(&mut s, Action::Smaller);
        assert_eq!(s.settings.px, 32);
    }

    #[test]
    fn generator_and_view_cycle() {
        let mut s = UiState::default();
        for _ in 0..4 {
            apply(&mut s, Action::NextGen);
        }
        assert_eq!(s.settings.mode, GenMode::Pangram);
        assert_eq!(s.view, ViewMode::Text);
        apply(&mut s, Action::NextView);
        assert_eq!(s.view, ViewMode::Boxes);
        apply(&mut s, Action::NextView);
        assert_eq!(s.view, ViewMode::Hidden);
        apply(&mut s, Action::NextView);
        assert_eq!(s.view, ViewMode::Text);
    }

    #[test]
    fn toggles_and_effects() {
        let mut s = UiState::default();
        assert!(!s.paused && !s.random_lang);
        apply(&mut s, Action::TogglePause);
        apply(&mut s, Action::ToggleRandom);
        apply(&mut s, Action::ToggleModel);
        assert!(s.paused && s.random_lang);
        assert_eq!(s.settings.model, ModelChoice::Universal);
        assert_eq!(
            apply(&mut s, Action::Step),
            Effects {
                step: true,
                ..Effects::default()
            }
        );
        assert!(apply(&mut s, Action::Clear).clear);
        assert!(apply(&mut s, Action::Quit).quit);
    }
}
