//! 07_cascade — `REQUIRED_USE`- und `pkg[flag]`-Kaskaden mit Konflikterkennung.
//!
//! Genau eine Zustaendigkeit: erzwungene USE-Flags per Fixpunkt fortpflanzen,
//! Widersprueche als `ConflictInfo` mit Quick-Switch-Kandidaten melden.
//! Kaskaden aendern die Aufloesung → Rueckkopplung in `resolve` (max. Runden).

use std::collections::HashSet;

use portage_atom::UseDepKind;
use portage_metadata::RequiredUseExpr;

use crate::mod_01_types::{ConflictInfo, EngineResult, QuickSwitch};
use crate::mod_06_graph::{Engine, ResolvedGraph, resolve};

/// Maximalrunden des Kaskaden-Fixpunkts (danach: Konflikt statt Endlosschleife).
const MAX_ROUNDS: u8 = 8;

/// Ergebnis einer Kaskaden-Aufloesung.
#[derive(Debug, Clone, Default)]
pub struct CascadeOutcome {
    /// Finaler Graph nach Fixpunkt.
    pub graph: ResolvedGraph,
    /// Erkannte Konflikte (leere Liste = konsistent).
    pub conflicts: Vec<ConflictInfo>,
    /// Benoetigte Fixpunkt-Runden.
    pub rounds: u8,
}

/// Welt + Kaskaden vollstaendig aufloesen (Fixpunkt aus Resolve ↔ Enforce).
pub fn resolve_with_cascade(
    engine: &mut Engine<'_>,
    world: &[String],
) -> EngineResult<CascadeOutcome> {
    // Snapshot der User-Overrides: alles, was danach in `overrides` landet,
    // stammt aus der Kaskade (kein User-Intent) und darf ueberschrieben oder
    // per Parent-Flip aufgeloest werden.
    let user = engine.overrides.clone();
    let mut forced: HashSet<(String, String)> = HashSet::new();
    let mut conflicts: Vec<ConflictInfo> = Vec::new();
    let mut rounds = 0u8;
    loop {
        let graph = resolve(engine, world)?;
        // Schritt-Scope: `step` leiht `engine` mutabel, danach ist es frei.
        // `forced` ueberlebt Runden (Ping-Pong-Schutz bei harten Widerspruechen).
        let (changed, step_conflicts) = {
            let mut step = CascadeStep::new(engine, &user, &mut forced);
            step.enforce_graph(&graph);
            (step.changed, step.conflicts)
        };
        rounds += 1;
        if step_conflicts.is_empty() && !changed {
            return Ok(CascadeOutcome {
                graph,
                conflicts,
                rounds,
            });
        }
        for conflict in step_conflicts {
            if !conflicts.contains(&conflict) {
                conflicts.push(conflict);
            }
        }
        if !changed || rounds >= MAX_ROUNDS {
            let graph = resolve(engine, world)?;
            if changed {
                conflicts.push(ConflictInfo {
                    package: "(kaskade)".into(),
                    message: format!(
                        "Fixpunkt konvergiert nicht nach {rounds} Runden (oszillierende Constraints)"
                    ),
                    caused_by: Vec::new(),
                    options: Vec::new(),
                });
            }
            return Ok(CascadeOutcome {
                graph,
                conflicts,
                rounds,
            });
        }
    }
}

/// Ein Kaskaden-Durchgang: sammelt Aenderungen + Konflikte, schreibt Overrides.
struct CascadeStep<'e, 'a, 'u, 'f> {
    engine: &'e mut Engine<'a>,
    /// User-Overrides bei Eintritt (alles andere ist Kaskaden-Werk).
    user: &'u crate::mod_06_graph::UseOverrides,
    /// Bereits Kaskaden-gesetzte (Paket, Flag)-Paare (rundenuebergreifend).
    forced: &'f mut HashSet<(String, String)>,
    changed: bool,
    conflicts: Vec<ConflictInfo>,
}

impl<'e, 'a, 'u, 'f> CascadeStep<'e, 'a, 'u, 'f> {
    fn new(
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

    fn enforce_graph(&mut self, graph: &ResolvedGraph) {
        // Snapshot der Knoten, sortiert (deterministische Reihenfolge).
        let mut nodes: Vec<(String, std::collections::HashSet<String>)> = graph
            .nodes
            .iter()
            .map(|(k, n)| (k.clone(), n.use_flags.clone()))
            .collect();
        nodes.sort_by(|a, b| a.0.cmp(&b.0));
        for (cpn, active) in &nodes {
            self.enforce_required_use(cpn, active, graph);
        }
        // Paketgrenzen: `cat/pkg[flag]`-Constraints aus allen Atomen.
        for (parent, active) in &nodes {
            self.enforce_use_deps(parent, active, graph);
        }
    }

    /// User-Intent aus dem Eintritts-Snapshot (`None` = nicht gesetzt).
    fn user_value(&self, cpn: &str, flag: &str) -> Option<bool> {
        self.user.get(cpn).and_then(|set| set.get(flag)).copied()
    }

    /// Aktuelle effektive Flags (Defaults + explizite Overrides).
    fn current_flags(&self, cpn: &str, fallback: &HashSet<String>) -> HashSet<String> {
        match self.engine.index.select(cpn) {
            Some(pkg) => self.engine.effective_use(pkg),
            None => fallback.clone(),
        }
    }

    /// Flag erzwingen; Widerspruch → Konflikt mit zwei Optionen.
    fn force_flag(
        &mut self,
        cpn: &str,
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
        let current = self.current_flags(cpn, fallback);
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
    fn enforce_required_use(&mut self, cpn: &str, active: &HashSet<String>, graph: &ResolvedGraph) {
        let Some(pkg) = self.engine.index.select(cpn) else {
            return;
        };
        let Some(expr) = &pkg.entry.metadata.required_use else {
            return;
        };
        let current = self.current_flags(cpn, active);
        let mut actions = Vec::new();
        enforce_expr(expr, &current, &mut actions);
        for action in actions {
            match action {
                ForceAction::On(flag) => {
                    self.force_flag(
                        cpn,
                        &flag,
                        true,
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

    /// `pkg[flag]`-Constraints aller Eltern gegen Kinder pruefen/erzwingen.
    fn enforce_use_deps(
        &mut self,
        parent: &str,
        parent_flags: &HashSet<String>,
        graph: &ResolvedGraph,
    ) {
        let Some(atoms) = graph.out_edges.get(parent).cloned() else {
            return;
        };
        let active = self.current_flags(parent, parent_flags);
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
                        self.force_target(&target, &flag, !invert, parent, &node.use_flags, flip)
                    }
                    UseDepKind::Disabled => {
                        let flip: Option<(String, String, bool)> = None;
                        self.force_target(&target, &flag, invert, parent, &node.use_flags, flip)
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
                                &flag,
                                !invert,
                                parent,
                                &node.use_flags,
                                flip,
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
                                &flag,
                                !invert,
                                parent,
                                &node.use_flags,
                                flip,
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
                                self.dep_fixed(&target, &flag),
                            ))
                        };
                        self.force_target(
                            &target,
                            &flag,
                            parent_has != invert,
                            parent,
                            &node.use_flags,
                            flip,
                        );
                    }
                    UseDepKind::EqualInverse => {
                        let flip = if invert {
                            None
                        } else {
                            Some((
                                parent.to_string(),
                                flag.clone(),
                                !self.dep_fixed(&target, &flag),
                            ))
                        };
                        self.force_target(
                            &target,
                            &flag,
                            parent_has == invert,
                            parent,
                            &node.use_flags,
                            flip,
                        );
                    }
                }
            }
        }
    }

    /// Aktuell gespeicherter Ziel-Zustand (Override oder Fallback).
    fn dep_fixed(&self, target: &str, flag: &str) -> bool {
        self.engine
            .index
            .select(target)
            .map(|pkg| self.engine.effective_use(pkg).contains(flag))
            .unwrap_or(false)
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
        flag: &str,
        want_on: bool,
        parent: &str,
        fallback: &HashSet<String>,
        flip: Option<(String, String, bool)>,
    ) {
        let current = self.current_flags(target, fallback);
        let current_on = current.contains(flag);
        if current_on == want_on {
            return;
        }
        match self.user_value(target, flag) {
            // User-Intent ist der gespeicherte Wert: Parent-Flip oder Konflikt.
            Some(u) if u == current_on => {
                if let Some((pcpn, pflag, need_on)) = flip.clone()
                    && self.try_parent_flip(&pcpn, &pflag, need_on, target, flag)
                {
                    return;
                }
                self.push_use_conflict(target, flag, want_on, parent);
            }
            // Kaskade hat User-Intent ueberschrieben: restaurieren.
            Some(_) => {
                self.force_flag(
                    target,
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
                    && self.try_parent_flip(&pcpn, &pflag, need_on, target, flag)
                {
                    return;
                }
                self.push_use_conflict(target, flag, want_on, parent);
            }
            None => {
                self.force_flag(
                    target,
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
    fn push_use_conflict(&mut self, target: &str, flag: &str, want_on: bool, parent: &str) {
        self.conflicts.push(ConflictInfo {
            package: target.to_string(),
            message: format!(
                "{parent} verlangt {target}[{flag}] = {}, gesetzt ist das Gegenteil",
                if want_on { "an" } else { "aus" }
            ),
            caused_by: vec![parent.to_string(), target.to_string()],
            options: vec![
                QuickSwitch {
                    label: format!("{target}[{flag}] uebernehmen"),
                    changes: vec![(target.to_string(), flag.to_string(), want_on)],
                },
                QuickSwitch {
                    label: format!("{parent}-Constraint lockern"),
                    changes: vec![],
                },
            ],
        });
    }

    /// Eltern-Flag umlegen, wenn es nicht User-gelockt/profil-fixiert ist.
    fn try_parent_flip(
        &mut self,
        parent: &str,
        flag: &str,
        need_on: bool,
        target: &str,
        target_flag: &str,
    ) -> bool {
        if parent == target && flag == target_flag {
            return false; // Selbst-Constraint: kein Ausweg.
        }
        let current_on = self
            .engine
            .index
            .select(parent)
            .map(|pkg| self.engine.effective_use(pkg).contains(flag))
            .unwrap_or(false);
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
            flag,
            need_on,
            format!("Rueckwaerts-Abgleich mit {target}"),
            vec![parent.to_string(), target.to_string()],
            &HashSet::new(),
        );
        true
    }
}

/// Erzwungene Einzelaktion aus `REQUIRED_USE`.
enum ForceAction {
    /// Flag einschalten.
    On(String),
    /// Unaufloesbar: Nachricht + Quick-Switch-Optionen.
    Conflict(String, Vec<QuickSwitch>),
}

/// `REQUIRED_USE`-Baum gegen aktive Flags auswerten (sammelt Aktionen).
fn enforce_expr(expr: &RequiredUseExpr, active: &HashSet<String>, out: &mut Vec<ForceAction>) {
    match expr {
        RequiredUseExpr::Flag { name, negated } => {
            let has = active.contains(name);
            if *negated {
                if has {
                    out.push(ForceAction::Conflict(
                        format!("REQUIRED_USE verbietet {name}, ist aber an"),
                        vec![QuickSwitch {
                            label: format!("{name} abschalten"),
                            changes: vec![],
                        }],
                    ));
                }
            } else if !has {
                out.push(ForceAction::On(name.clone()));
            }
        }
        RequiredUseExpr::UseConditional {
            flag,
            negated,
            entries,
        } => {
            if active.contains(flag) != *negated {
                for child in entries {
                    enforce_expr(child, active, out);
                }
            }
        }
        RequiredUseExpr::All(entries) => {
            for child in entries {
                enforce_expr(child, active, out);
            }
        }
        RequiredUseExpr::AnyOf(children) => {
            if !children.iter().any(|c| satisfied(c, active)) {
                let candidates: Vec<String> = children
                    .iter()
                    .filter_map(|c| enablable_flag(c, active))
                    .collect();
                if let Some(first) = candidates.first() {
                    out.push(ForceAction::On(first.clone()));
                } else {
                    out.push(ForceAction::Conflict(
                        "REQUIRED_USE-|| unerfuellt, keine Option einschaltbar".into(),
                        children
                            .iter()
                            .map(describe)
                            .map(|d| QuickSwitch {
                                label: format!("erfuellen: {d}"),
                                changes: vec![],
                            })
                            .collect(),
                    ));
                }
            }
        }
        RequiredUseExpr::ExactlyOne(children) => {
            let n = children.iter().filter(|c| satisfied(c, active)).count();
            if n == 0 {
                enforce_expr(&RequiredUseExpr::AnyOf(children.clone()), active, out);
            } else if n > 1 {
                out.push(ForceAction::Conflict(
                    "REQUIRED_USE-^^ mehrfach erfuellt (genau eines erlaubt)".into(),
                    children
                        .iter()
                        .filter(|c| satisfied(c, active))
                        .map(describe)
                        .map(|d| QuickSwitch {
                            label: format!("nur behalten: {d}"),
                            changes: vec![],
                        })
                        .collect(),
                ));
            }
        }
        RequiredUseExpr::AtMostOne(children) => {
            let n = children.iter().filter(|c| satisfied(c, active)).count();
            if n > 1 {
                out.push(ForceAction::Conflict(
                    "REQUIRED_USE-?? mehrfach erfuellt (hoechstens eines erlaubt)".into(),
                    children
                        .iter()
                        .filter(|c| satisfied(c, active))
                        .map(describe)
                        .map(|d| QuickSwitch {
                            label: format!("nur behalten: {d}"),
                            changes: vec![],
                        })
                        .collect(),
                ));
            }
        }
    }
}

/// Ob ein `REQUIRED_USE`-Teilbaum aktuell erfuellt ist.
fn satisfied(expr: &RequiredUseExpr, active: &HashSet<String>) -> bool {
    match expr {
        RequiredUseExpr::Flag { name, negated } => active.contains(name) != *negated,
        RequiredUseExpr::UseConditional {
            flag,
            negated,
            entries,
        } => {
            if active.contains(flag) != *negated {
                entries.iter().all(|c| satisfied(c, active))
            } else {
                true
            }
        }
        RequiredUseExpr::All(entries) => entries.iter().all(|c| satisfied(c, active)),
        RequiredUseExpr::AnyOf(children) => children.iter().any(|c| satisfied(c, active)),
        RequiredUseExpr::ExactlyOne(children) => {
            children.iter().filter(|c| satisfied(c, active)).count() == 1
        }
        RequiredUseExpr::AtMostOne(children) => {
            children.iter().filter(|c| satisfied(c, active)).count() <= 1
        }
    }
}

/// Ein einzelnes per Default einschaltbares Flag extrahieren.
fn enablable_flag(expr: &RequiredUseExpr, active: &HashSet<String>) -> Option<String> {
    match expr {
        RequiredUseExpr::Flag { name, negated } => {
            if !negated && !active.contains(name) {
                Some(name.clone())
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Kurzbeschreibung eines Ausdrucks (fuer Optionen).
fn describe(expr: &RequiredUseExpr) -> String {
    match expr {
        RequiredUseExpr::Flag { name, negated } => {
            format!("{}{}", if *negated { "!" } else { "" }, name)
        }
        RequiredUseExpr::AnyOf(_) => "|| (...)".into(),
        RequiredUseExpr::ExactlyOne(_) => "^^ (...)".into(),
        RequiredUseExpr::AtMostOne(_) => "?? (...)".into(),
        RequiredUseExpr::UseConditional { flag, negated, .. } => {
            format!("{}{}? (...)", if *negated { "!" } else { "" }, flag)
        }
        RequiredUseExpr::All(_) => "(...)".into(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mod_02_profile::Profile;
    use crate::mod_03_manifest::parse_manifest;
    use crate::mod_05_repo_index::{PkgVersion, RepoIndex};
    use portage_metadata::CacheEntry;
    use std::collections::HashMap;

    fn pkg(cpn: &str, version: &str, iuse: &str, rdepend: &str, required_use: &str) -> PkgVersion {
        let text = format!(
            "EAPI=8\nDESCRIPTION=t\nSLOT=0\nKEYWORDS=amd64\nIUSE={iuse}\n\
             RDEPEND={rdepend}\nREQUIRED_USE={required_use}\nSRC_URI=http://x/a.tar.gz\n"
        );
        let entry = CacheEntry::parse(&text).unwrap();
        let mut iuse_defaults = HashMap::new();
        for flag in &entry.metadata.iuse {
            iuse_defaults.insert(flag.name().to_string(), flag.is_enabled_default());
        }
        PkgVersion {
            cpn: cpn.into(),
            cpv: format!("{cpn}-{version}"),
            version_str: version.into(),
            version: portage_atom::Version::parse(version).unwrap(),
            slot: "0".into(),
            masked: false,
            live: false,
            iuse_defaults,
            entry,
            manifest: parse_manifest("DIST a.tar.gz 100\n"),
            binhost_size: Some(10),
        }
    }

    fn index_with(pkgs: Vec<PkgVersion>) -> RepoIndex {
        let mut map = HashMap::new();
        for p in pkgs {
            map.entry(p.cpn.clone()).or_insert_with(Vec::new).push(p);
        }
        RepoIndex {
            versions: map,
            cache_files: 0,
            load_ms: 0,
        }
    }

    #[test]
    fn required_use_forces_flag() {
        // wayland? ( egl ): wayland an → egl wird erzwungen.
        let index = index_with(vec![pkg(
            "gui/app",
            "1.0",
            "wayland egl",
            "",
            "wayland? ( egl )",
        )]);
        let profile = Profile::test_default();
        let mut engine = Engine::new(&index, &profile);
        engine.overrides.insert(
            "gui/app".into(),
            [("wayland".to_string(), true)].into_iter().collect(),
        );
        let outcome = resolve_with_cascade(&mut engine, &["gui/app".into()]).unwrap();
        assert!(outcome.conflicts.is_empty());
        let node = &outcome.graph.nodes["gui/app"];
        assert!(node.use_flags.contains("egl"));
    }

    #[test]
    fn use_dep_conflict_detected_with_options() {
        // A braucht libX[foo], C braucht libX[-foo] → Konflikt.
        let index = index_with(vec![
            pkg("app/a", "1.0", "bar", "dev-libs/libx[foo]", ""),
            pkg("app/c", "1.0", "baz", "dev-libs/libx[-foo]", ""),
            pkg("dev-libs/libx", "1.0", "foo", "", ""),
        ]);
        let profile = Profile::test_default();
        let mut engine = Engine::new(&index, &profile);
        engine
            .overrides
            .insert("app/a".into(), [("bar".into(), true)].into_iter().collect());
        engine
            .overrides
            .insert("app/c".into(), [("baz".into(), true)].into_iter().collect());
        let outcome = resolve_with_cascade(&mut engine, &["app/a".into(), "app/c".into()]).unwrap();
        assert!(!outcome.conflicts.is_empty());
        assert!(outcome.conflicts.iter().all(|c| !c.options.is_empty()));
    }

    #[test]
    fn shared_dep_delta_zero_documented() {
        // Vorstufe fuer T10: beide brauchen openssl — Abschalten bei nur
        // einem Paket spart nichts (wird in 08_delta exakt gerechnet).
        let index = index_with(vec![
            pkg("app/a", "1.0", "ssl", "dev-libs/openssl", ""),
            pkg("app/b", "1.0", "ssl", "dev-libs/openssl", ""),
            pkg("dev-libs/openssl", "1.0", "ssl", "", ""),
        ]);
        let profile = Profile::test_default();
        let mut engine = Engine::new(&index, &profile);
        let outcome = resolve_with_cascade(&mut engine, &["app/a".into(), "app/b".into()]).unwrap();
        assert!(outcome.conflicts.is_empty());
        assert_eq!(outcome.graph.refcounts.get("dev-libs/openssl"), Some(&2));
    }
}
