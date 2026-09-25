//! `08_rules` — Regel-Engine: Match, Cooldown, Log (S4).
//!
//! Die Engine matcht Box-Texte per Fuzzy-OCR (`07_match`) und feuert
//! genau eine Aktion pro `evaluate`-Aufruf (Cooldown pro Regel, Log-Deckel).
//! Aktionen laufen über das `Sink`-Trait — ohne X11 testbar (Fake-Sink),
//! produktiv implementiert von `X11Input` (Verdrahtung in S6).
//! Typen und TOML-Laden stehen in `06_config`.

use std::time::Instant;

use crate::config::{Action, Config, Rule};
use crate::input::InputError;
use crate::mtch::fuzzy_ocr_match;

/// Deckel für das Aktions-Log (Einträge).
pub const LOG_CAP: usize = 10;

/// Getroffene Box: erkannter Text + Screen-Rechteck aus `View`.
#[derive(Debug, Clone)]
pub struct BoxHit {
    /// Erkannter Text.
    pub text: String,
    /// Screen-Rechteck `(x, y, w, h)`.
    pub rect: (i32, i32, u32, u32),
}

/// Aktions-Ziel: produktiv `X11Input`, im Test ein Fake.
/// Fehler werden im Log sichtbar (kein stilles Nichts-Tun).
pub trait Sink {
    /// Klick auf absolute Pixel.
    fn click(&mut self, x: i16, y: i16) -> Result<(), InputError>;
    /// Text tippen (+ optional Enter).
    fn type_text(&mut self, text: &str, press_enter: bool) -> Result<(), InputError>;

    /// Klick, dann Fokus-Wechsel abwarten, dann tippen.
    /// Die `FOCUS_SETTLE_MS`-Pause liegt garantiert DAZWISCHEN.
    fn click_and_type(
        &mut self,
        x: i16,
        y: i16,
        text: &str,
        press_enter: bool,
    ) -> Result<(), InputError> {
        self.click(x, y)?;
        std::thread::sleep(std::time::Duration::from_millis(
            crate::input::FOCUS_SETTLE_MS,
        ));
        self.type_text(text, press_enter)
    }

    /// Übersprungene Zeichen (nur echte Eingaben zählen; Default 0).
    fn skipped(&self) -> u64 {
        0
    }
}

/// Anstehende Aktion nach einem Regel-Treffer (Borrow-sicher kopiert).
struct PendingFire {
    name: String,
    htext: String,
    rect: (i32, i32, u32, u32),
    action: Action,
}

/// Regel-Engine: matcht Box-Texte, feuert mit Cooldown, schreibt Log.
pub struct Automation {
    enabled: bool,
    rules: Vec<Rule>,
    log: Vec<String>,
}

impl Automation {
    /// Neue Engine aus Regeln (startet AUS — erst `set_enabled(true)`
    /// schaltet scharf).
    #[must_use]
    pub fn from_config(cfg: &Config) -> Self {
        Self {
            enabled: false,
            rules: cfg.rules.clone(),
            log: Vec::new(),
        }
    }

    /// Scharf/unscharf schalten (Taste `a`).
    pub fn set_enabled(&mut self, on: bool) {
        self.enabled = on;
    }

    /// Wahr, wenn scharf.
    #[must_use]
    pub fn is_enabled(&self) -> bool {
        self.enabled
    }

    /// Aktions-Log (neueste höchstens `LOG_CAP` Einträge).
    #[must_use]
    pub fn log(&self) -> &[String] {
        &self.log
    }

    /// Prüft alle Boxen gegen alle Regeln; feuert höchstens eine Aktion.
    pub fn evaluate<S: Sink + ?Sized>(
        &mut self,
        hits: &[BoxHit],
        screen: (i32, i32),
        sink: &mut S,
    ) {
        if !self.enabled {
            return;
        }
        for i in 0..self.rules.len() {
            // Einmal-Regel, die schon feuerte: nie wieder (Cooldown egal).
            if self.rules[i].once && self.rules[i].last_fired.is_some() {
                continue;
            }
            let ready = self.rules[i]
                .last_fired
                .is_none_or(|t| t.elapsed() >= self.rules[i].cooldown);
            if !ready {
                continue;
            }
            // Treffer suchen (nur lesen); erst danach feuern (schreiben).
            let mut fire: Option<PendingFire> = None;
            for hit in hits {
                if fuzzy_ocr_match(&self.rules[i].pattern, &hit.text) {
                    fire = Some(PendingFire {
                        name: self.rules[i].name.clone(),
                        htext: hit.text.clone(),
                        rect: hit.rect,
                        action: self.rules[i].action.clone(),
                    });
                    break;
                }
            }
            let Some(f) = fire else { continue };
            let name = f.name;
            let htext = f.htext;
            let action = f.action;
            let (sx, sy, sw, sh) = f.rect;
            let cx = (sx + sw as i32 / 2).clamp(0, screen.0.max(0)) as i16;
            let cy = (sy + sh as i32 / 2).clamp(0, screen.1.max(0)) as i16;
            self.rules[i].last_fired = Some(Instant::now());
            let done = match &action {
                Action::Click => sink
                    .click(cx, cy)
                    .map(|()| format!("[{name}] Klick auf '{htext}' @ ({cx},{cy})")),
                Action::ClickAndType { text, press_enter } => sink
                    .click_and_type(cx, cy, text, *press_enter)
                    .map(|()| format!("[{name}] Klick + Eingabe in '{htext}'")),
            };
            match done {
                Ok(line) => self.push_log(line),
                Err(e) => self.push_log(format!("[{name}] FEHLER bei '{htext}': {e}")),
            }
            return; // genau eine Aktion pro Zyklus
        }
    }

    fn push_log(&mut self, entry: String) {
        self.log.push(entry);
        if self.log.len() > LOG_CAP {
            self.log.remove(0);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE: &str = include_str!("../rules.example.toml");

    fn hit(text: &str) -> BoxHit {
        BoxHit {
            text: text.into(),
            rect: (10, 20, 100, 30),
        }
    }

    #[derive(Default)]
    struct FakeSink {
        clicks: Vec<(i16, i16)>,
        typed: Vec<(String, bool)>,
    }

    impl Sink for FakeSink {
        fn click(&mut self, x: i16, y: i16) -> Result<(), InputError> {
            self.clicks.push((x, y));
            Ok(())
        }
        fn type_text(&mut self, text: &str, press_enter: bool) -> Result<(), InputError> {
            self.typed.push((text.into(), press_enter));
            Ok(())
        }
    }

    #[test]
    fn disabled_fires_nothing() {
        let cfg = Config::parse(EXAMPLE).unwrap();
        let mut auto = Automation::from_config(&cfg);
        let mut sink = FakeSink::default();
        auto.evaluate(&[hit("Google Search here")], (1920, 1080), &mut sink);
        assert!(sink.clicks.is_empty());
        assert!(auto.log().is_empty());
    }

    #[test]
    fn match_fires_click_and_type() {
        let cfg = Config::parse(EXAMPLE).unwrap();
        let mut auto = Automation::from_config(&cfg);
        auto.set_enabled(true);
        let mut sink = FakeSink::default();
        auto.evaluate(&[hit("Please Sign In now")], (1920, 1080), &mut sink);
        assert_eq!(sink.clicks.len(), 1);
        assert_eq!(sink.typed.len(), 1);
        assert!(sink.typed[0].1); // press_enter aus der TOML
        assert_eq!(auto.log().len(), 1);
    }

    #[test]
    fn cooldown_blocks_second_fire() {
        let cfg = Config::parse(
            "schema_version = 1\n[[rule]]\nname = \"r\"\npattern = \"go\"\n\
             action = \"click\"\ncooldown_secs = 3600",
        )
        .unwrap();
        let mut auto = Automation::from_config(&cfg);
        auto.set_enabled(true);
        let mut sink = FakeSink::default();
        let hits = [hit("go button")];
        auto.evaluate(&hits, (800, 600), &mut sink);
        auto.evaluate(&hits, (800, 600), &mut sink);
        assert_eq!(sink.clicks.len(), 1);
    }

    #[test]
    fn once_rule_fires_exactly_once() {
        // Meta.ai-Fall: Platzhalter kehrt nach dem Absenden zurück —
        // ohne `once` würde jeder abgelaufene Cooldown erneut feuern.
        let cfg = Config::parse(
            "schema_version = 1\n[[rule]]\nname = \"r\"\npattern = \"go\"\n\
             action = \"click\"\ncooldown_secs = 0\nonce = true",
        )
        .unwrap();
        let mut auto = Automation::from_config(&cfg);
        auto.set_enabled(true);
        let mut sink = FakeSink::default();
        let hits = [hit("go button")];
        auto.evaluate(&hits, (800, 600), &mut sink);
        auto.evaluate(&hits, (800, 600), &mut sink);
        auto.evaluate(&hits, (800, 600), &mut sink);
        assert_eq!(sink.clicks.len(), 1);
        assert_eq!(auto.log().len(), 1);
    }

    #[test]
    fn log_is_capped() {
        let cfg = Config::parse(
            "schema_version = 1\n[[rule]]\nname = \"r\"\npattern = \"go\"\n\
             action = \"click\"\ncooldown_secs = 0",
        )
        .unwrap();
        let mut auto = Automation::from_config(&cfg);
        auto.set_enabled(true);
        let mut sink = FakeSink::default();
        for _ in 0..LOG_CAP + 5 {
            auto.evaluate(&[hit("go")], (800, 600), &mut sink);
        }
        assert_eq!(auto.log().len(), LOG_CAP);
    }

    #[test]
    fn click_clamps_to_screen() {
        let cfg = Config::parse(
            "schema_version = 1\n[[rule]]\nname = \"r\"\npattern = \"go\"\n\
             action = \"click\"\ncooldown_secs = 0",
        )
        .unwrap();
        let mut auto = Automation::from_config(&cfg);
        auto.set_enabled(true);
        let mut sink = FakeSink::default();
        let far = BoxHit {
            text: "go".into(),
            rect: (5000, 5000, 100, 30),
        };
        auto.evaluate(&[far], (800, 600), &mut sink);
        assert_eq!(sink.clicks[0], (800, 600));
    }
}
