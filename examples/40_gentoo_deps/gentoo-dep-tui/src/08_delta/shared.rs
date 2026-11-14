//! 08_delta/shared — Shared-Blocker- und Voll-Ersparnis-Analyse.
//!
//! Genau eine Zustaendigkeit: beantworten, wie viel ein Flag-Toggle *wirklich*
//! spart, wenn mitgehaltene Abhaengigkeiten woanders ebenfalls abgeschaltet
//! (Flag-Blocker) oder Welt-Pakete entfernt werden (World-Blocker).
//!
//! On-Demand-Analyse nur fuer das aktuell selektierte Flag (Ergebnis im
//! `AppState` gecacht) — kein Einfluss auf `package_deltas`/`big_levers`.

use std::collections::{BTreeSet, HashSet};

use crate::mod_01_types::{EngineResult, SizeMetrics, atom_cpn};
use crate::mod_06_graph::Engine;
use crate::mod_07_cascade::{CascadeOutcome, resolve_with_cascade_opts};

/// Ersparnis einer Entfernungssimulation (Outcome minus Baseline).
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct RemovalSavings {
    /// Groessenaenderung (negativ = Ersparnis, wie `FlagDelta::delta`).
    pub metrics: SizeMetrics,
    /// Entfallende Pakete (`cat/pkg`, sortiert).
    pub removed_packages: Vec<String>,
}

/// Voll-Ersparnis-Analyse fuer ein Flag auf einem Paket.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct SharedRemovalImpact {
    /// Untersuchtes Paket (`cat/pkg`).
    pub package: String,
    /// Untersuchtes Flag.
    pub flag: String,
    /// Gezogene Abhaengigkeiten, die andere Pakete weiter halten (sortiert).
    pub shared_deps: Vec<String>,
    /// `(Paket, Flag`-Paare, die ebenfalls abgeschaltet werden muessten.
    pub flag_blockers: Vec<(String, String)>,
    /// Welt-Atome, die unbedingt brauchen und entfernt werden muessten.
    pub world_blockers: Vec<String>,
    /// Ersparnis, wenn `package[flag]` + alle Flag-Blocker aus sind.
    pub without_flag_blockers: RemovalSavings,
    /// Maximale Ersparnis zusaetzlich ohne World-Blocker.
    pub without_all_blockers: RemovalSavings,
}

/// Shared-Blocker-Analyse fuer `package[flag]` (aus → Einsparnis).
///
/// Gibt `None` zurueck, wenn es nichts zu analysieren gibt: Flag bereits aus,
/// Paket nicht im Graphen, oder keine gezogene Abhaengigkeit wird von
/// anderen gehalten (dann erzaehlt das Marginal-Delta schon alles).
///
/// Voraussetzung: `base` enthaelt Blame-Daten (`reverse`), also z. B.
/// `AppState::last_outcome` aus `refresh()` — ohne Rueckwaertskanten gibt
/// es keine Halteranalyse (dann `None`).
///
/// Ablauf (jeweils exakte Simulationen, keine Schaetzungen):
/// 1. Toggle allein simulieren (`outcome_alone`).
/// 2. Direkt gezogene Deps (Rueckwaertskanten mit `via == flag`) plus deren
///    transitive Deps bestimmen; Ueberlebende mit fremden Haltern sind
///    `shared_deps`, deren Halter werden in Flag-/World-Blocker getrennt.
/// 3. Einmal exakt mit allen Flag-Blockern aus, einmal zusaetzlich ohne
///    World-Blocker simulieren.
pub fn shared_removal_impact(
    engine: &Engine<'_>,
    world: &[String],
    base: &CascadeOutcome,
    package: &str,
    flag: &str,
) -> EngineResult<Option<SharedRemovalImpact>> {
    // Nur die Aus-Richtung ist eine Ersparnis-Analyse.
    let node = match base.graph.nodes.get(package) {
        Some(n) => n,
        None => return Ok(None),
    };
    if !node.use_flags.contains(flag) {
        return Ok(None);
    }
    // 1. Allein abschalten simulieren.
    let mut alone = engine.fork();
    alone
        .overrides
        .entry(package.to_string())
        .or_default()
        .insert(flag.to_string(), false);
    let outcome_alone = resolve_with_cascade_opts(&mut alone, world, false, false)?;

    // 2. Gezogene Deps: direkte Flag-Kinder + deren transitive Deps.
    let pulled = pulled_deps(&base.graph, package, flag);
    let world_cpns: HashSet<String> = world.iter().map(|w| atom_cpn(w)).collect();
    let mut shared: Vec<String> = pulled
        .into_iter()
        .filter(|d| d != package)
        .filter(|d| outcome_alone.graph.nodes.contains_key(d))
        .filter(|d| {
            base.graph
                .reverse
                .get(d)
                .is_some_and(|ps| ps.iter().any(|(p, _)| p != package))
        })
        .collect();
    shared.sort();
    if shared.is_empty() {
        return Ok(None);
    }

    // Halter in Flag-/World-Blocker trennen.
    let mut flag_blockers: BTreeSet<(String, String)> = BTreeSet::new();
    let mut world_blockers: BTreeSet<String> = BTreeSet::new();
    for dep in &shared {
        let mut visited = HashSet::new();
        collect_blockers(
            &base.graph,
            package,
            &world_cpns,
            dep,
            &mut flag_blockers,
            &mut world_blockers,
            &mut visited,
        );
    }
    let flag_blockers: Vec<(String, String)> = flag_blockers.into_iter().collect();
    let world_blockers: Vec<String> = world_blockers.into_iter().collect();

    // 3a. Mit allen Flag-Blockern aus simulieren.
    let mut sim1 = engine.fork();
    sim1.overrides
        .entry(package.to_string())
        .or_default()
        .insert(flag.to_string(), false);
    for (cpn, f) in &flag_blockers {
        sim1.overrides
            .entry(cpn.clone())
            .or_default()
            .insert(f.clone(), false);
    }
    let outcome1 = resolve_with_cascade_opts(&mut sim1, world, false, false)?;
    let without_flag_blockers = savings_vs(&base.graph, &outcome1.graph);

    // 3b. Zusaetzlich ohne World-Blocker simulieren.
    let world2: Vec<String> = world
        .iter()
        .filter(|w| !world_blockers.contains(&atom_cpn(w)))
        .cloned()
        .collect();
    let mut sim2 = engine.fork();
    sim2.overrides
        .entry(package.to_string())
        .or_default()
        .insert(flag.to_string(), false);
    for (cpn, f) in &flag_blockers {
        sim2.overrides
            .entry(cpn.clone())
            .or_default()
            .insert(f.clone(), false);
    }
    let outcome2 = resolve_with_cascade_opts(&mut sim2, &world2, false, false)?;
    let without_all_blockers = savings_vs(&base.graph, &outcome2.graph);

    Ok(Some(SharedRemovalImpact {
        package: package.to_string(),
        flag: flag.to_string(),
        shared_deps: shared,
        flag_blockers,
        world_blockers,
        without_flag_blockers,
        without_all_blockers,
    }))
}

/// Ersparnis Outcome-gegen-Baseline (Metrik + entfernte Pakete).
fn savings_vs(
    before: &crate::mod_06_graph::ResolvedGraph,
    after: &crate::mod_06_graph::ResolvedGraph,
) -> RemovalSavings {
    let before_totals = before.totals();
    let after_totals = after.totals();
    let mut removed: Vec<String> = before
        .nodes
        .keys()
        .filter(|k| !after.nodes.contains_key(*k))
        .cloned()
        .collect();
    removed.sort();
    RemovalSavings {
        metrics: SizeMetrics {
            binpkg_bytes: after_totals.binpkg_bytes - before_totals.binpkg_bytes,
            download_bytes: after_totals.download_bytes - before_totals.download_bytes,
        },
        removed_packages: removed,
    }
}

/// Durch `package[flag]` gezogene Abhaengigkeiten (transitive Huelle).
///
/// Start: direkte Kinder mit Rueckwaertskante `(package, Some(flag))`;
/// danach allen `out_edges`-Kanten in Abhaengigkeitsrichtung folgen
/// (nur Knoten des Basisgraphen).
fn pulled_deps(
    base: &crate::mod_06_graph::ResolvedGraph,
    package: &str,
    flag: &str,
) -> HashSet<String> {
    let mut out = HashSet::new();
    // Direkte Flag-Kinder: Rueckwaertskante `(package, Some(flag))`.
    let mut stack: Vec<String> = base
        .reverse
        .iter()
        .filter(|(_, parents)| {
            parents
                .iter()
                .any(|(p, via)| p == package && via.as_deref() == Some(flag))
        })
        .map(|(child, _)| child.clone())
        .collect();
    while let Some(cpn) = stack.pop() {
        if !out.insert(cpn.clone()) {
            continue;
        }
        if let Some(atoms) = base.out_edges.get(&cpn) {
            for atom in atoms {
                let target = atom.cpn.to_string();
                if base.nodes.contains_key(&target) && !out.contains(&target) {
                    stack.push(target);
                }
            }
        }
    }
    out
}

/// Halter eines Shared-Deps in Flag-/World-Blocker trennen (Tiefensuche
/// aufwaerts ueber Rueckwaertskanten, zyklenfest).
///
/// - Bedingte Kante `(parent, Some(f))` → Flag-Blocker (Ast endet hier: mit
///   abgeschaltetem Flag bricht dieser Haltepfad weg).
/// - Unbedingte Kante zu Welt-Atom → World-Blocker.
/// - Unbedingte Kante zu Transitivem → weiter aufsteigen.
/// - Das untersuchte Paket selbst wird uebersprungen (es bleibt im Graphen).
#[allow(clippy::too_many_arguments)]
fn collect_blockers(
    base: &crate::mod_06_graph::ResolvedGraph,
    package: &str,
    world_cpns: &HashSet<String>,
    dep: &str,
    flag_blockers: &mut BTreeSet<(String, String)>,
    world_blockers: &mut BTreeSet<String>,
    visited: &mut HashSet<String>,
) {
    if !visited.insert(dep.to_string()) {
        return;
    }
    let Some(parents) = base.reverse.get(dep) else {
        return;
    };
    for (parent, via) in parents {
        if parent == package {
            continue;
        }
        match via {
            Some(f) => {
                flag_blockers.insert((parent.clone(), f.clone()));
            }
            None => {
                if world_cpns.contains(parent) {
                    world_blockers.insert(parent.clone());
                } else {
                    collect_blockers(
                        base,
                        package,
                        world_cpns,
                        parent,
                        flag_blockers,
                        world_blockers,
                        visited,
                    );
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mod_02_profile::Profile;
    use crate::mod_08_delta::flag_delta_with_base;
    use crate::test_helpers::{PkgFixture, repo_index};

    /// Diamond: `app/a[ssl]` + `app/b[ssl]` + `app/c` (unbedingt) → openssl.
    /// Baseline MIT Blame-Daten (`reverse`): wie `AppState::last_outcome`
    /// aus `refresh()` — Shared-Analyse braucht Rueckwaertskanten.
    fn diamond_base(
        engine: &Engine<'_>,
        world: &[String],
    ) -> crate::mod_07_cascade::CascadeOutcome {
        resolve_with_cascade_opts(&mut engine.fork(), world, false, true).unwrap()
    }

    fn diamond() -> (crate::mod_05_repo_index::RepoIndex, Profile, Vec<String>) {
        let index = repo_index(vec![
            PkgFixture::new("app/a")
                .iuse("+ssl")
                .rdepend("ssl? ( dev-libs/openssl )")
                .binpkg(Some(100))
                .build(),
            PkgFixture::new("app/b")
                .iuse("+ssl")
                .rdepend("ssl? ( dev-libs/openssl )")
                .binpkg(Some(100))
                .build(),
            PkgFixture::new("app/c")
                .rdepend("dev-libs/openssl")
                .binpkg(Some(100))
                .build(),
            PkgFixture::new("dev-libs/openssl")
                .src_uri("http://x/o.tar.gz")
                .manifest("DIST o.tar.gz 7000\n")
                .binpkg(Some(800))
                .build(),
        ]);
        let world = vec![
            "app/a".to_string(),
            "app/b".to_string(),
            "app/c".to_string(),
        ];
        (index, Profile::test_default(), world)
    }

    #[test]
    fn diamond_names_flag_and_world_blockers() {
        let (index, profile, world) = diamond();
        let engine = Engine::new(&index, &profile);
        let base = diamond_base(&engine, &world);
        // Marginal bei app/a ist exakt 0 B (openssl bleibt ueber b/c).
        let marginal = flag_delta_with_base(&engine, &world, "app/a", "ssl", &base).unwrap();
        assert_eq!(marginal.delta.binpkg_bytes, 0);
        assert_eq!(marginal.delta.download_bytes, 0);
        let impact = shared_removal_impact(&engine, &world, &base, "app/a", "ssl")
            .unwrap()
            .expect("Shared-Dep openssl erwartet");
        assert_eq!(
            impact.flag_blockers,
            vec![("app/b".to_string(), "ssl".to_string())]
        );
        assert_eq!(impact.world_blockers, vec!["app/c".to_string()]);
        // Nur mit Flag-Blockern aus bleibt openssl (ueber app/c).
        assert_eq!(impact.without_flag_blockers.metrics.binpkg_bytes, 0);
        assert!(impact.without_flag_blockers.removed_packages.is_empty());
        // Maximal-Ersparnis = volles openssl (800 bin / 7000 dl) plus das
        // entfernte Welt-Paket app/c selbst (100 bin / 100 dl).
        assert_eq!(impact.without_all_blockers.metrics.binpkg_bytes, -900);
        assert_eq!(impact.without_all_blockers.metrics.download_bytes, -7100);
        assert!(
            impact
                .without_all_blockers
                .removed_packages
                .contains(&"dev-libs/openssl".to_string())
        );
    }

    #[test]
    fn single_holder_returns_none() {
        // Nur app/a zieht openssl → alleiniges Abschalten reicht.
        let index = repo_index(vec![
            PkgFixture::new("app/a")
                .iuse("+ssl")
                .rdepend("ssl? ( dev-libs/openssl )")
                .binpkg(Some(100))
                .build(),
            PkgFixture::new("dev-libs/openssl")
                .binpkg(Some(800))
                .build(),
        ]);
        let profile = Profile::test_default();
        let engine = Engine::new(&index, &profile);
        let world = vec!["app/a".to_string()];
        let base = diamond_base(&engine, &world);
        assert!(
            shared_removal_impact(&engine, &world, &base, "app/a", "ssl")
                .unwrap()
                .is_none()
        );
    }
}
