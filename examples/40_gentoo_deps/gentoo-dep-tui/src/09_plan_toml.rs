//! 09_plan_toml — TOML-Plan: Baseline, Pakete, AI-Alternativen, `||`-Entscheidungen.
//!
//! Genau eine Zustaendigkeit: `world` ↔ `plan.toml` ohne Verlust von
//! `pros`/`cons`. Pros/Cons sind echte Datenfelder (keine Kommentare), damit
//! die TUI sie anzeigen und speichern kann.

use std::collections::HashMap;
use std::path::Path;

use serde::{Deserialize, Serialize};

use crate::mod_01_types::{EngineError, EngineResult};
use crate::mod_02_profile::read_world;

/// Wurzel des Plan-Formats.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Plan {
    /// Basisdaten (Profil, Welt).
    #[serde(default)]
    pub baseline: Baseline,
    /// Pakete mit expliziten USE-Overrides.
    #[serde(default)]
    pub package: Vec<PlanPackage>,
    /// AI-generierte Alternativen (`[[choice]]`).
    #[serde(default)]
    pub choice: Vec<PlanChoice>,
    /// Persistierte `||`-Entscheidungen.
    #[serde(default)]
    pub or_decision: Vec<OrDecision>,
}

/// `[baseline]`: Profil + Welt-Atome.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Baseline {
    /// Profilname (informativ, z. B. aus `make.profile`).
    #[serde(default)]
    pub profile: String,
    /// Welt-Atome.
    #[serde(default)]
    pub world: Vec<String>,
}

/// `[[package]]`: explizite USE-Overrides eines Pakets.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct PlanPackage {
    /// `cat/pkg`.
    pub atom: String,
    /// Aktive Flags (Negation via `-flag`).
    #[serde(default)]
    pub use_flags: Vec<String>,
}

/// `[[choice]]`: eine AI-generierte Alternative.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct PlanChoice {
    /// Stabile ID (z. B. `web-server`).
    #[serde(default)]
    pub id: String,
    /// Ob die Entscheidung Pflicht ist.
    #[serde(default)]
    pub required: bool,
    /// Beschreibung (z. B. `HTTP Reverse Proxy`).
    #[serde(default)]
    pub description: String,
    /// Optionsliste.
    #[serde(default)]
    pub option: Vec<ChoiceOption>,
    /// Gewaehlter Optionsindex.
    #[serde(default)]
    pub selected: usize,
}

/// `[[choice.option]]`: eine konkrete Alternative mit Pro/Contra.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ChoiceOption {
    /// Paket-Atom (z. B. `www-servers/nginx`).
    #[serde(default)]
    pub atom: String,
    /// Vorteile (AI-generiert).
    #[serde(default)]
    pub pros: String,
    /// Nachteile (AI-generiert).
    #[serde(default)]
    pub cons: String,
    /// Empfohlene USE-Flags.
    #[serde(default)]
    pub suggested_use: Vec<String>,
}

/// `[[or_decision]]`: persistierte `||`-Entscheidung aus dem Graphen.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct OrDecision {
    /// Gruppen-ID (wie in `AlternativeChoice`).
    pub group_id: String,
    /// Gewaehlter Kandidaten-Index.
    pub selected: usize,
}

impl Plan {
    /// `world`-Datei → saubere `plan.toml`-Struktur.
    pub fn init_from_world(world_path: &Path, profile: &str) -> EngineResult<Self> {
        Ok(Self {
            baseline: Baseline {
                profile: profile.to_string(),
                world: read_world(world_path)?,
            },
            package: Vec::new(),
            choice: Vec::new(),
            or_decision: Vec::new(),
        })
    }

    /// Laden (fehlende Datei → leerer Plan mit Hinweis im Caller).
    pub fn load(path: &Path) -> EngineResult<Self> {
        let text = std::fs::read_to_string(path).map_err(|e| EngineError::Io {
            path: path.display().to_string(),
            source: e,
        })?;
        toml::from_str(&text).map_err(|e| EngineError::Parse {
            context: path.display().to_string(),
            message: e.to_string(),
        })
    }

    /// Speichern (menschenlesbar, `pros`/`cons` bleiben erhalten).
    pub fn save(&self, path: &Path) -> EngineResult<()> {
        let text = toml::to_string_pretty(self).map_err(|e| EngineError::Parse {
            context: path.display().to_string(),
            message: e.to_string(),
        })?;
        std::fs::write(path, text).map_err(|e| EngineError::Io {
            path: path.display().to_string(),
            source: e,
        })
    }

    /// Welt-Atome inkl. gewaehlter `[[choice]]`-Optionen.
    pub fn effective_world(&self) -> Vec<String> {
        let mut world = self.baseline.world.clone();
        for choice in &self.choice {
            if let Some(opt) = choice.option.get(choice.selected)
                && !world.iter().any(|w| w == &opt.atom)
            {
                world.push(opt.atom.clone());
            }
        }
        world
    }

    /// `||`-Entscheidungen als Map (fuer `Engine.or_decisions`).
    pub fn or_decisions(&self) -> HashMap<String, usize> {
        self.or_decision
            .iter()
            .map(|d| (d.group_id.clone(), d.selected))
            .collect()
    }

    /// Paket-Overrides als explizite Intents (fuer `Engine.overrides`).
    ///
    /// Nur Tokens aus `use_flags`/`suggested_use` (`flag` → an,
    /// `-flag` → aus); keine Defaults — sonst wuerde jeder Default zum
    /// scheinbaren User-Intent.
    pub fn package_overrides(&self) -> HashMap<String, std::collections::HashMap<String, bool>> {
        fn apply(
            out: &mut HashMap<String, std::collections::HashMap<String, bool>>,
            atom: &str,
            tokens: &[String],
        ) {
            let entry = out.entry(atom.to_string()).or_default();
            for token in tokens {
                if let Some(flag) = token.strip_prefix('-') {
                    entry.insert(flag.to_string(), false);
                } else {
                    entry.insert(token.clone(), true);
                }
            }
        }
        let mut out = HashMap::new();
        // `suggested_use` gewaehlter Optionen zuerst (Defaults), ...
        for choice in &self.choice {
            if let Some(opt) = choice.option.get(choice.selected) {
                apply(&mut out, &opt.atom, &opt.suggested_use);
            }
        }
        // ... explizite `[[package]]`-Toggles danach (User gewinnt immer).
        for pkg in &self.package {
            apply(&mut out, &pkg.atom, &pkg.use_flags);
        }
        out
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;

    #[test]
    fn package_overrides_beat_suggested_use() {
        let mut plan = Plan::default();
        plan.choice.push(PlanChoice {
            id: "web".into(),
            required: false,
            description: "proxy".into(),
            selected: 0,
            option: vec![ChoiceOption {
                atom: "www-servers/nginx".into(),
                pros: String::new(),
                cons: String::new(),
                suggested_use: vec!["ssl".into()],
            }],
        });
        plan.package.push(PlanPackage {
            atom: "www-servers/nginx".into(),
            use_flags: vec!["-ssl".into()],
        });
        let ov = plan.package_overrides();
        assert_eq!(
            ov.get("www-servers/nginx").unwrap().get("ssl"),
            Some(&false)
        );
    }

    #[test]
    fn roundtrip_preserves_pros_cons() {
        let d = tempfile::tempdir().unwrap();
        let world = d.path().join("world");
        let mut f = std::fs::File::create(&world).unwrap();
        writeln!(f, "# c\napp-misc/jq").unwrap();
        let mut plan = Plan::init_from_world(&world, "default/linux/amd64/23.0").unwrap();
        plan.choice.push(PlanChoice {
            id: "web".into(),
            required: true,
            description: "proxy".into(),
            selected: 1,
            option: vec![
                ChoiceOption {
                    atom: "www-servers/nginx".into(),
                    pros: "modular".into(),
                    cons: "c-code".into(),
                    suggested_use: vec!["ssl".into()],
                },
                ChoiceOption {
                    atom: "www-servers/caddy".into(),
                    pros: "https auto".into(),
                    cons: "gross".into(),
                    suggested_use: vec![],
                },
            ],
        });
        let path = d.path().join("plan.toml");
        plan.save(&path).unwrap();
        let back = Plan::load(&path).unwrap();
        assert_eq!(back.choice[0].option[1].pros, "https auto");
        assert_eq!(back.choice[0].option[0].cons, "c-code");
        let world_atoms = back.effective_world();
        assert!(world_atoms.contains(&"app-misc/jq".to_string()));
        assert!(world_atoms.contains(&"www-servers/caddy".to_string()));
    }
}
