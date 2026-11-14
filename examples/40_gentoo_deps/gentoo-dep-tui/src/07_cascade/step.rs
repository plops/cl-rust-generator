//! 07_cascade/step — Kaskaden-Fixpunkt: ein Durchgang.
//!
//! Genau eine Zustaendigkeit: User-Intents gegen Profil/Graphen
//! abgleichen, Forwards/Flips setzen, Konflikte sammeln.

use std::collections::HashSet;

use crate::mod_01_types::{ConflictInfo, QuickSwitch};

use super::required_use::ForceAction;
use super::{Engine, ResolvedGraph};

/// Ein Kaskaden-Durchgang: sammelt Aenderungen + Konflikte, schreibt Overrides.
pub(super) struct CascadeStep<'e, 'a, 'u, 'f> {
    pub(super) engine: &'e mut Engine<'a>,
    /// User-Overrides bei Eintritt (alles andere ist Kaskaden-Werk).
    pub(super) user: &'u crate::mod_06_graph::UseOverrides,
    /// Bereits Kaskaden-gesetzte (Paket, Flag)-Paare (rundenuebergreifend).
    pub(super) forced: &'f mut HashSet<(String, String)>,
    pub(super) changed: bool,
    pub(super) conflicts: Vec<ConflictInfo>,
}

impl<'e, 'a, 'u, 'f> CascadeStep<'e, 'a, 'u, 'f> {
    pub(super) fn new(
        engine: &'e mut Engine<'a>,
        user: &'u crate::mod_06_graph::UseOverrides,
        forced: &'f mut HashSet<(String, String)>,
    ) -> Self {
        Self {
            engine,
            user,
            forced,
            changed: false,
            conflicts: Vec::new(),
        }
    }

    pub(super) fn enforce_graph(&mut self, graph: &ResolvedGraph) {
        // Snapshot der Knoten (cpn, cpv, aktive Flags), sortiert
        // (deterministische Reihenfolge). `cpv` waehlt die exakt
        // aufgeloeste Version statt der neuesten (Task 2.3).
        let mut nodes: Vec<(String, String, std::collections::HashSet<String>)> = graph
            .nodes
            .iter()
            .map(|(k, n)| (k.clone(), n.cpv.clone(), n.use_flags.clone()))
            .collect();
        nodes.sort_by(|a, b| a.0.cmp(&b.0));
        for (cpn, cpv, active) in &nodes {
            self.enforce_required_use(cpn, cpv, active, graph);
        }
        // Paketgrenzen: `cat/pkg[flag]`-Constraints aus allen Atomen.
        for (parent, pcpv, active) in &nodes {
            self.enforce_use_deps(parent, pcpv, active, graph);
        }
    }

    /// User-Intent aus dem Eintritts-Snapshot (`None` = nicht gesetzt).
    pub(super) fn user_value(&self, cpn: &str, flag: &str) -> Option<bool> {
        self.user.get(cpn).and_then(|set| set.get(flag)).copied()
    }

    /// Aktuelle effektive Flags (Defaults + explizite Overrides).
    pub(super) fn current_flags(
        &self,
        cpn: &str,
        cpv: &str,
        fallback: &HashSet<String>,
    ) -> HashSet<String> {
        match self.engine.index.select_cpv(cpn, cpv) {
            Some(pkg) => self.engine.effective_use(pkg),
            None => fallback.clone(),
        }
    }

    /// Flag erzwingen; Widerspruch → Konflikt mit zwei Optionen.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn force_flag(
        &mut self,
        cpn: &str,
        cpv: &str,
        flag: &str,
        want_on: bool,
        reason: String,
        caused_by: Vec<String>,
        fallback: &HashSet<String>,
    ) {
        if self.engine.profile.is_masked(flag) && want_on {
            self.conflicts.push(ConflictInfo {
                package: cpn.to_string(),
                message: format!("{flag} von {reason} verlangt, aber profil-maskiert"),
                caused_by,
                options: vec![],
            });
            return;
        }
        if !want_on && self.engine.profile.is_forced(flag) {
            self.conflicts.push(ConflictInfo {
                package: cpn.to_string(),
                message: format!("{flag} von {reason} verboten, aber profil-erzwungen"),
                caused_by,
                options: vec![],
            });
            return;
        }
        // Echter User-Intent gegen die Kaskade: nie still ueberschreiben,
        // sondern als Konflikt mit Uebernahme-Option melden (Task 2.4).
        if self.user_value(cpn, flag) == Some(!want_on) {
            self.conflicts.push(ConflictInfo {
                package: cpn.to_string(),
                message: format!(
                    "{flag} soll laut {reason} {}, User-Intent ist das Gegenteil",
                    if want_on { "an" } else { "aus" }
                ),
                caused_by,
                options: vec![QuickSwitch {
                    label: format!(
                        "Kaskade uebernehmen: {flag} {}",
                        if want_on { "an" } else { "aus" }
                    ),
                    changes: vec![(cpn.to_string(), flag.to_string(), want_on)],
                }],
            });
            return;
        }
        let current = self.current_flags(cpn, cpv, fallback);
        if current.contains(flag) != want_on {
            self.engine
                .overrides
                .entry(cpn.to_string())
                .or_default()
                .insert(flag.to_string(), want_on);
            self.forced.insert((cpn.to_string(), flag.to_string()));
            self.changed = true;
        }
        let _ = reason;
    }

    /// `REQUIRED_USE` eines Pakets gegen seine Flags pruefen/erzwingen.
    fn enforce_required_use(
        &mut self,
        cpn: &str,
        cpv: &str,
        active: &HashSet<String>,
        graph: &ResolvedGraph,
    ) {
        let Some(pkg) = self.engine.index.select_cpv(cpn, cpv) else {
            return;
        };
        let Some(expr) = &pkg.entry.metadata.required_use else {
            return;
        };
        let current = self.current_flags(cpn, cpv, active);
        let mut actions = Vec::new();
        self.enforce_expr(expr, &current, &mut actions, cpn, None);
        for action in actions {
            match action {
                ForceAction::Set(flag, want_on) => {
                    self.force_flag(
                        cpn,
                        cpv,
                        &flag,
                        want_on,
                        format!("REQUIRED_USE von {cpn}"),
                        vec![cpn.to_string()],
                        active,
                    );
                }
                ForceAction::Conflict(msg, options) => {
                    self.conflicts.push(ConflictInfo {
                        package: cpn.to_string(),
                        message: msg,
                        caused_by: vec![cpn.to_string()],
                        options,
                    });
                }
            }
        }
        let _ = graph;
    }
}
