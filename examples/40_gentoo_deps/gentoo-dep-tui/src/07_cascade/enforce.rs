//! 07_cascade/enforce — Paketgrenzen: `pkg[flag]`-Constraints.
//!
//! Genau eine Zustaendigkeit: Use-Dep-Abgleich zwischen Eltern und
//! Kindern (IUSE-Gate, Parent-Flips, Konflikt-Optionen).

use std::collections::HashSet;

use portage_atom::{UseDefault, UseDepKind};

use crate::mod_01_types::{ConflictInfo, QuickSwitch};
use crate::mod_06_graph::ResolvedGraph;

use super::step::CascadeStep;

impl<'e, 'a, 'u, 'f> CascadeStep<'e, 'a, 'u, 'f> {
    /// `pkg[flag]`-Constraints aller Eltern gegen Kinder pruefen/erzwingen.
    pub(super) fn enforce_use_deps(
        &mut self,
        parent: &str,
        parent_cpv: &str,
        parent_flags: &HashSet<String>,
        graph: &ResolvedGraph,
    ) {
        let Some(atoms) = graph.out_edges.get(parent).cloned() else {
            return;
        };
        let active = self.current_flags(parent, parent_cpv, parent_flags);
        for atom in &atoms {
            let Some(use_deps) = &atom.use_deps else {
                continue;
            };
            if use_deps.is_empty() {
                continue;
            }
            let target = atom.cpn.to_string();
            let Some(node) = graph.nodes.get(&target) else {
                continue;
            };
            // Blocker (`!dep[...]`) verbieten die gematchte Konfiguration:
            // ihre Use-Deps gelten invertiert (z. B. `!gnupg[-alternatives]`
            // verlangt alternatives=AN). Keine Parent-Flips fuer Blocker.
            let invert = atom.blocker.is_some();
            for ud in use_deps {
                let flag = ud.flag.to_string();
                let parent_has = active.contains(&flag);
                // Parent-Flip bei weichen Constraints: steht das Ziel durch
                // User-Intent fest, weicht (wenn moeglich) das Eltern-Flag.
                match ud.kind {
                    UseDepKind::Enabled => {
                        let flip: Option<(String, String, bool)> = None;
                        self.force_target(
                            &target,
                            &node.cpv,
                            &flag,
                            !invert,
                            parent,
                            parent_cpv,
                            &node.use_flags,
                            flip,
                            ud.default,
                        )
                    }
                    UseDepKind::Disabled => {
                        let flip: Option<(String, String, bool)> = None;
                        self.force_target(
                            &target,
                            &node.cpv,
                            &flag,
                            invert,
                            parent,
                            parent_cpv,
                            &node.use_flags,
                            flip,
                            ud.default,
                        )
                    }
                    UseDepKind::Conditional => {
                        if parent_has {
                            // Ziel aus → Eltern-Flag duerfte ausgehen.
                            let flip = if invert {
                                None
                            } else {
                                Some((parent.to_string(), flag.clone(), false))
                            };
                            self.force_target(
                                &target,
                                &node.cpv,
                                &flag,
                                !invert,
                                parent,
                                parent_cpv,
                                &node.use_flags,
                                flip,
                                ud.default,
                            );
                        }
                    }
                    UseDepKind::ConditionalInverse => {
                        if !parent_has {
                            // Ziel aus → Eltern-Flag duerfte angehen.
                            let flip = if invert {
                                None
                            } else {
                                Some((parent.to_string(), flag.clone(), true))
                            };
                            self.force_target(
                                &target,
                                &node.cpv,
                                &flag,
                                !invert,
                                parent,
                                parent_cpv,
                                &node.use_flags,
                                flip,
                                ud.default,
                            );
                        }
                    }
                    UseDepKind::Equal => {
                        // Ziel fest → Eltern-Flag angleichen (oder umgekehrt).
                        let flip = if invert {
                            None
                        } else {
                            Some((
                                parent.to_string(),
                                flag.clone(),
                                self.dep_fixed(&target, &node.cpv, &flag),
                            ))
                        };
                        self.force_target(
                            &target,
                            &node.cpv,
                            &flag,
                            parent_has != invert,
                            parent,
                            parent_cpv,
                            &node.use_flags,
                            flip,
                            ud.default,
                        );
                    }
                    UseDepKind::EqualInverse => {
                        let flip = if invert {
                            None
                        } else {
                            Some((
                                parent.to_string(),
                                flag.clone(),
                                !self.dep_fixed(&target, &node.cpv, &flag),
                            ))
                        };
                        self.force_target(
                            &target,
                            &node.cpv,
                            &flag,
                            parent_has == invert,
                            parent,
                            parent_cpv,
                            &node.use_flags,
                            flip,
                            ud.default,
                        );
                    }
                }
            }
        }
    }

    /// Aktuell gespeicherter Ziel-Zustand (Override oder Fallback).
    fn dep_fixed(&mut self, target: &str, target_cpv: &str, flag: &str) -> bool {
        self.cached_effective(target, target_cpv)
            .is_some_and(|active| active.contains(flag))
    }

    /// Ob das Ziel das Flag in IUSE kennt (exakte aufgeloeste Version).
    fn target_has_flag(&self, target: &str, target_cpv: &str, flag: &str) -> bool {
        self.engine
            .index
            .select_cpv(target, target_cpv)
            .is_some_and(|pkg| pkg.iuse_defaults.contains_key(flag))
    }

    /// Ziel-Flag aus Eltern-Constraint erzwingen (mit Konflikt-Optionen).
    ///
    /// `flip`: Parent-Ausweg bei weichen Constraints
    /// `(eltern-paket, eltern-flag, soll-an)` — steht das Ziel durch
    /// User-Intent fest, wird stattdessen das Eltern-Flag umgelegt
    /// (Portage-Backtracking light). Harte Constraints (`None`) melden
    /// echte Konflikte.
    #[allow(clippy::too_many_arguments)]
    fn force_target(
        &mut self,
        target: &str,
        target_cpv: &str,
        flag: &str,
        want_on: bool,
        parent: &str,
        parent_cpv: &str,
        fallback: &HashSet<String>,
        flip: Option<(String, String, bool)>,
        default: Option<UseDefault>,
    ) {
        // IUSE-Gate (Task 2.4): kennt das Ziel das Flag nicht, gilt die
        // `[+]/[-]`-Default-Semantik — kein invalides Flag injizieren.
        if !self.target_has_flag(target, target_cpv, flag) {
            match default {
                Some(UseDefault::Enabled) if want_on => return,
                Some(UseDefault::Disabled) if !want_on => return,
                _ => {}
            }
            let detail = match default {
                Some(_) => "Default widerspricht der Forderung",
                None => "ohne Default in IUSE",
            };
            self.conflicts.push(ConflictInfo {
                package: target.to_string(),
                message: format!(
                    "{parent} verlangt {target}[{flag}] = {}, aber {flag} fehlt in IUSE ({detail})",
                    if want_on { "an" } else { "aus" }
                ),
                caused_by: vec![parent.to_string(), target.to_string()],
                options: {
                    let mut opts = vec![QuickSwitch {
                        label: format!(
                            "trotzdem setzen: {target}[{flag}] {} (User-Override)",
                            if want_on { "an" } else { "aus" }
                        ),
                        changes: vec![(target.to_string(), flag.to_string(), want_on)],
                    }];
                    if let Some((pcpn, pflag, need_on)) = flip.clone() {
                        opts.push(QuickSwitch {
                            label: format!(
                                "{parent}-Constraint lockern ({pflag} {})",
                                if need_on { "an" } else { "aus" }
                            ),
                            changes: vec![(pcpn, pflag, need_on)],
                        });
                    }
                    opts
                },
            });
            return;
        }
        let current = self.current_flags(target, target_cpv, fallback);
        let current_on = current.contains(flag);
        if current_on == want_on {
            return;
        }
        match self.user_value(target, flag) {
            // User-Intent ist der gespeicherte Wert: Parent-Flip oder Konflikt.
            Some(u) if u == current_on => {
                if let Some((pcpn, pflag, need_on)) = flip.clone()
                    && self.try_parent_flip(&pcpn, parent_cpv, &pflag, need_on, target, flag)
                {
                    return;
                }
                self.push_use_conflict(target, flag, want_on, parent, flip);
            }
            // Kaskade hat User-Intent ueberschrieben: restaurieren.
            Some(_) => {
                self.force_flag(
                    target,
                    target_cpv,
                    flag,
                    want_on,
                    format!("{parent}-Abhaengigkeit (User-Restore)"),
                    vec![parent.to_string(), target.to_string()],
                    fallback,
                );
            }
            // Kein User-Intent: Widerspruch nur gegen fruehere Kaskaden-Werte
            // (Flip/Konflikt, Dep bleibt → Konvergenz), sonst uebernehmen.
            None if self
                .forced
                .contains(&(target.to_string(), flag.to_string())) =>
            {
                if let Some((pcpn, pflag, need_on)) = flip.clone()
                    && self.try_parent_flip(&pcpn, parent_cpv, &pflag, need_on, target, flag)
                {
                    return;
                }
                self.push_use_conflict(target, flag, want_on, parent, flip);
            }
            None => {
                self.force_flag(
                    target,
                    target_cpv,
                    flag,
                    want_on,
                    format!("{parent}-Abhaengigkeit"),
                    vec![parent.to_string(), target.to_string()],
                    fallback,
                );
            }
        }
    }

    /// Use-Dep-Konflikt mit Quick-Switch-Optionen ablegen.
    ///
    /// Die zweite Option traegt echte Aenderungen (Parent-Flip) oder
    /// entfaellt bei harten Constraints — nie `changes: vec![]` (Task 2.4).
    fn push_use_conflict(
        &mut self,
        target: &str,
        flag: &str,
        want_on: bool,
        parent: &str,
        flip: Option<(String, String, bool)>,
    ) {
        let mut options = vec![QuickSwitch {
            label: format!("{target}[{flag}] uebernehmen"),
            changes: vec![(target.to_string(), flag.to_string(), want_on)],
        }];
        if let Some((pcpn, pflag, need_on)) = flip {
            options.push(QuickSwitch {
                label: format!(
                    "{parent}-Constraint lockern ({pflag} {})",
                    if need_on { "an" } else { "aus" }
                ),
                changes: vec![(pcpn, pflag, need_on)],
            });
        }
        self.conflicts.push(ConflictInfo {
            package: target.to_string(),
            message: format!(
                "{parent} verlangt {target}[{flag}] = {}, gesetzt ist das Gegenteil",
                if want_on { "an" } else { "aus" }
            ),
            caused_by: vec![parent.to_string(), target.to_string()],
            options,
        });
    }

    /// Eltern-Flag umlegen, wenn es nicht User-gelockt/profil-fixiert ist.
    fn try_parent_flip(
        &mut self,
        parent: &str,
        parent_cpv: &str,
        flag: &str,
        need_on: bool,
        target: &str,
        target_flag: &str,
    ) -> bool {
        if parent == target && flag == target_flag {
            return false; // Selbst-Constraint: kein Ausweg.
        }
        let current_on = self
            .cached_effective(parent, parent_cpv)
            .is_some_and(|active| active.contains(flag));
        if current_on == need_on {
            return true; // Bereits passend (Fixpunkt klaert den Rest).
        }
        if self.user_value(parent, flag) == Some(current_on) {
            return false; // Echter User-Intent am Eltern-Flag.
        }
        if self.engine.profile.is_masked(flag) && need_on {
            return false;
        }
        if !need_on && self.engine.profile.is_forced(flag) {
            return false;
        }
        self.force_flag(
            parent,
            parent_cpv,
            flag,
            need_on,
            format!("Rueckwaerts-Abgleich mit {target}"),
            vec![parent.to_string(), target.to_string()],
            &HashSet::new(),
        );
        true
    }
}
