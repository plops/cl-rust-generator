//! 08_delta/shared — Shared-Blocker- und Cut-Analyse (exakt, simuliert).

use std::collections::{BTreeSet, HashMap, HashSet};
use serde::Serialize;

use crate::mod_01_types::{EngineResult, SizeMetrics, atom_cpn};
use crate::mod_06_graph::{Engine, ResolvedGraph};
use crate::mod_07_cascade::{CascadeOutcome, resolve_with_cascade_opts};
use crate::mod_08_delta::{is_hidden_expand_flag, is_platform_flag};

/// Ersparnis einer Entfernungssimulation.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub struct RemovalSavings {
    /// Groessenaenderung (negativ = Ersparnis).
    pub metrics: SizeMetrics,
    /// Entfallende Pakete (`cat/pkg`, sortiert).
    pub removed_packages: Vec<String>,
    /// Hinzukommende Pakete (`cat/pkg`, sortiert).
    #[serde(default)]
    pub added_packages: Vec<String>,
    /// Konflikte nachher.
    #[serde(default)]
    pub conflicts: usize,
}

/// Ein schaltbarer Flag-Blocker.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize)]
pub struct FlagBlocker {
    /// Haltendes Paket.
    pub package: String,
    /// Flag auf dem Paket.
    pub flag: String,
    /// `true` = einschalten (bei `!flag?`), `false` = abschalten (bei `flag?`).
    pub want_on: bool,
}

/// Paket-Eliminierungs-Analyse.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub struct CutPlan {
    /// Untersuchtes Ziel (`cat/pkg`).
    pub target: String,
    /// Schaltbare Flag-Blocker.
    pub flag_blockers: Vec<FlagBlocker>,
    /// Welt-Blocker.
    pub world_blockers: Vec<String>,
    /// Ersparnis nur mit Flag-Blockern.
    pub without_flag_blockers: RemovalSavings,
    /// Ersparnis inkl. World-Entfernung.
    pub without_all_blockers: RemovalSavings,
}

/// Voll-Ersparnis-Analyse fuer ein Flag auf einem Paket.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub struct SharedRemovalImpact {
    /// Untersuchtes Paket.
    pub package: String,
    /// Untersuchtes Flag.
    pub flag: String,
    /// Geteilte Deps mit fremden Haltern.
    pub shared_deps: Vec<String>,
    /// Flag-Blocker.
    pub flag_blockers: Vec<FlagBlocker>,
    /// Welt-Blocker.
    pub world_blockers: Vec<String>,
    /// Ersparnis mit allen Flag-Blockern aus.
    pub without_flag_blockers: RemovalSavings,
    /// Maximale Ersparnis zusaetzlich ohne World-Blocker.
    pub without_all_blockers: RemovalSavings,
}

/// Parst ein `via_flag` (`"gtk"` oder `"!minimal"`) in `(flag, want_on)`.
fn parse_breakable_flag(
    engine: &Engine<'_>,
    graph: &ResolvedGraph,
    parent: &str,
    via: &str,
) -> Option<(String, bool)> {
    let (flag, want_on) = match via.strip_prefix('!') {
        Some(f) => (f, true),
        None => (via, false),
    };
    if is_platform_flag(engine.profile, flag) || is_hidden_expand_flag(engine.profile, flag) {
        return None;
    }
    if want_on && engine.profile.is_masked(flag) {
        return None;
    }
    if !want_on && engine.profile.is_forced(flag) {
        return None;
    }
    let node = graph.nodes.get(parent)?;
    let pkg = engine.index.select_cpv(parent, &node.cpv)?;
    if !pkg.iuse_defaults.contains_key(flag) {
        return None;
    }
    Some((flag.to_string(), want_on))
}

/// Steigt ab `dep` aufwaerts ueber `reverse`-Kanten bis zu schaltbaren
/// Flag-Blockern oder `world`-Blockern.
#[allow(clippy::too_many_arguments)]
fn collect_blockers_up(
    graph: &ResolvedGraph,
    engine: &Engine<'_>,
    world_cpns: &HashSet<String>,
    skip_pkg: Option<&str>,
    dep: &str,
    flag_blockers: &mut BTreeSet<FlagBlocker>,
    world_blockers: &mut BTreeSet<String>,
    visited: &mut HashSet<String>,
) {
    if !visited.insert(dep.to_string()) {
        return;
    }
    if skip_pkg != Some(dep) && world_cpns.contains(dep) {
        world_blockers.insert(dep.to_string());
    }
    let Some(parents) = graph.reverse.get(dep) else {
        return;
    };
    for (parent, via) in parents {
        if skip_pkg == Some(parent.as_str()) {
            continue;
        }
        if let Some(via_str) = via
            && let Some((flag, want_on)) = parse_breakable_flag(engine, graph, parent, via_str)
        {
            flag_blockers.insert(FlagBlocker {
                package: parent.clone(),
                flag,
                want_on,
            });
            continue;
        }
        collect_blockers_up(
            graph,
            engine,
            world_cpns,
            skip_pkg,
            parent,
            flag_blockers,
            world_blockers,
            visited,
        );
    }
}

/// Paket-Eliminierungs-Analyse: wie bekommt man `target` aus dem Graphen?
pub fn cut_package(
    engine: &Engine<'_>,
    world: &[String],
    base: &CascadeOutcome,
    target: &str,
) -> EngineResult<Option<CutPlan>> {
    if !base.graph.nodes.contains_key(target) {
        return Ok(None);
    }
    let world_cpns: HashSet<String> = world.iter().map(|w| atom_cpn(w)).collect();
    let mut flag_blockers = BTreeSet::new();
    let mut world_blockers = BTreeSet::new();
    let mut visited = HashSet::new();
    collect_blockers_up(
        &base.graph,
        engine,
        &world_cpns,
        None,
        target,
        &mut flag_blockers,
        &mut world_blockers,
        &mut visited,
    );
    let flag_blockers: Vec<FlagBlocker> = flag_blockers.into_iter().collect();
    let world_blockers: Vec<String> = world_blockers.into_iter().collect();

    let without_flag_blockers = simulate_cut(engine, world, &base.graph, &flag_blockers, &[])?;
    let without_all_blockers =
        simulate_cut(engine, world, &base.graph, &flag_blockers, &world_blockers)?;

    Ok(Some(CutPlan {
        target: target.to_string(),
        flag_blockers,
        world_blockers,
        without_flag_blockers,
        without_all_blockers,
    }))
}

/// Shared-Blocker-Analyse fuer `package[flag]` (nur wenn Flag aktuell aktiv).
pub fn shared_removal_impact(
    engine: &Engine<'_>,
    world: &[String],
    base: &CascadeOutcome,
    package: &str,
    flag: &str,
) -> EngineResult<Option<SharedRemovalImpact>> {
    let Some(node) = base.graph.nodes.get(package) else {
        return Ok(None);
    };
    if !node.use_flags.contains(flag) {
        return Ok(None);
    }
    let mut alone = engine.fork();
    alone
        .overrides
        .entry(package.to_string())
        .or_default()
        .insert(flag.to_string(), false);
    let outcome_alone = resolve_with_cascade_opts(&mut alone, world, false, false)?;

    let mut shared: Vec<String> = base
        .graph
        .reverse
        .iter()
        .filter(|(child, _)| child.as_str() != package)
        .filter(|(_, parents)| {
            parents
                .iter()
                .any(|(p, via)| p == package && via.as_deref() == Some(flag))
        })
        .filter(|(child, parents)| {
            outcome_alone.graph.nodes.contains_key(*child)
                && parents.iter().any(|(p, _)| p != package)
        })
        .map(|(child, _)| child.clone())
        .collect();
    shared.sort();
    if shared.is_empty() {
        return Ok(None);
    }

    let world_cpns: HashSet<String> = world.iter().map(|w| atom_cpn(w)).collect();
    let mut flag_blockers = BTreeSet::new();
    let mut world_blockers = BTreeSet::new();
    for dep in &shared {
        let mut visited = HashSet::new();
        collect_blockers_up(
            &base.graph,
            engine,
            &world_cpns,
            Some(package),
            dep,
            &mut flag_blockers,
            &mut world_blockers,
            &mut visited,
        );
    }
    let flag_blockers: Vec<FlagBlocker> = flag_blockers.into_iter().collect();
    let world_blockers: Vec<String> = world_blockers.into_iter().collect();

    let self_off = FlagBlocker {
        package: package.to_string(),
        flag: flag.to_string(),
        want_on: false,
    };
    let mut all_flags = vec![self_off];
    all_flags.extend(flag_blockers.iter().cloned());

    let without_flag_blockers = simulate_cut(engine, world, &base.graph, &all_flags, &[])?;
    let without_all_blockers =
        simulate_cut(engine, world, &base.graph, &all_flags, &world_blockers)?;

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

fn simulate_cut(
    engine: &Engine<'_>,
    world: &[String],
    before: &ResolvedGraph,
    flags: &[FlagBlocker],
    drop_world: &[String],
) -> EngineResult<RemovalSavings> {
    let mut sim = engine.fork();
    for fb in flags {
        sim.overrides
            .entry(fb.package.clone())
            .or_default()
            .insert(fb.flag.clone(), fb.want_on);
    }
    let filtered_world: Vec<String> = if drop_world.is_empty() {
        world.to_vec()
    } else {
        world
            .iter()
            .filter(|w| !drop_world.contains(&atom_cpn(w)))
            .cloned()
            .collect()
    };
    let outcome = resolve_with_cascade_opts(&mut sim, &filtered_world, false, false)?;
    Ok(savings_vs(before, &outcome))
}

fn savings_vs(before: &ResolvedGraph, after: &CascadeOutcome) -> RemovalSavings {
    let before_totals = before.totals();
    let after_totals = after.graph.totals();
    let mut removed: Vec<String> = before
        .nodes
        .keys()
        .filter(|k| !after.graph.nodes.contains_key(*k))
        .cloned()
        .collect();
    let mut added: Vec<String> = after
        .graph
        .nodes
        .keys()
        .filter(|k| !before.nodes.contains_key(*k))
        .cloned()
        .collect();
    removed.sort();
    added.sort();
    RemovalSavings {
        metrics: SizeMetrics {
            binpkg_bytes: after_totals.binpkg_bytes - before_totals.binpkg_bytes,
            download_bytes: after_totals.download_bytes - before_totals.download_bytes,
        },
        removed_packages: removed,
        added_packages: added,
        conflicts: after.conflicts.len(),
    }
}

/// Test-Hilfen: simuliere Cut ueber rohe Welt (fuer schnelle Unit-Tests).
#[allow(dead_code)]
pub fn simulate_cut_world(
    engine: &Engine<'_>,
    world: &[String],
    flags: &HashMap<String, HashMap<String, bool>>,
    drop_world: &[String],
) -> EngineResult<RemovalSavings> {
    let mut sim = engine.fork();
    for (cpn, map) in flags {
        for (f, on) in map {
            sim.overrides
                .entry(cpn.clone())
                .or_default()
                .insert(f.clone(), *on);
        }
    }
    let filtered: Vec<String> = if drop_world.is_empty() {
        world.to_vec()
    } else {
        world
            .iter()
            .filter(|w| !drop_world.contains(&atom_cpn(w)))
            .cloned()
            .collect()
    };
    let before = resolve_with_cascade_opts(&mut engine.fork(), world, false, false)?;
    let after = resolve_with_cascade_opts(&mut sim, &filtered, false, false)?;
    Ok(savings_vs(&before.graph, &after))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mod_02_profile::Profile;
    use crate::mod_08_delta::flag_delta_with_base;
    use crate::test_helpers::{PkgFixture, repo_index};

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
        let marginal = flag_delta_with_base(&engine, &world, "app/a", "ssl", &base).unwrap();
        assert_eq!(marginal.delta.binpkg_bytes, 0);
        assert_eq!(marginal.delta.download_bytes, 0);
        let impact = shared_removal_impact(&engine, &world, &base, "app/a", "ssl")
            .unwrap()
            .expect("Shared-Dep openssl erwartet");
        let tuples: Vec<(String, String)> = impact
            .flag_blockers
            .iter()
            .map(|b| (b.package.clone(), b.flag.clone()))
            .collect();
        assert_eq!(tuples, vec![("app/b".to_string(), "ssl".to_string())]);
        assert_eq!(impact.world_blockers, vec!["app/c".to_string()]);
        assert_eq!(impact.without_flag_blockers.metrics.binpkg_bytes, 0);
        assert!(impact.without_flag_blockers.removed_packages.is_empty());
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

    #[test]
    fn negated_flag_wants_on() {
        // `!minimal? ( dev-libs/heavy )`: Blocker will EIN-Schalten von minimal.
        let index = repo_index(vec![
            PkgFixture::new("app/a")
                .iuse("-minimal")
                .rdepend("!minimal? ( dev-libs/heavy )")
                .binpkg(Some(100))
                .build(),
            PkgFixture::new("app/b")
                .iuse("-minimal")
                .rdepend("!minimal? ( dev-libs/heavy )")
                .binpkg(Some(100))
                .build(),
            PkgFixture::new("dev-libs/heavy")
                .binpkg(Some(500))
                .build(),
        ]);
        let profile = Profile::test_default();
        let engine = Engine::new(&index, &profile);
        let world = vec!["app/a".to_string(), "app/b".to_string()];
        let base = diamond_base(&engine, &world);
        let plan = cut_package(&engine, &world, &base, "dev-libs/heavy")
            .unwrap()
            .expect("Cut-Plan erwartet");
        assert!(
            plan.flag_blockers
                .iter()
                .all(|b| b.want_on),
            "negierte Kante muss want_on=true liefern: {:?}",
            plan.flag_blockers
        );
    }

    #[test]
    fn transitive_leaf_bug_absent() {
        // `readline`-artige Basis darf kein Blocker fuer app/entfernt sein.
        let index = repo_index(vec![
            PkgFixture::new("app/top")
                .iuse("+js")
                .rdepend("js? ( dev-libs/mid )")
                .binpkg(Some(100))
                .build(),
            PkgFixture::new("dev-libs/mid")
                .iuse("+extra")
                .rdepend("extra? ( sys-libs/base )")
                .binpkg(Some(100))
                .build(),
            PkgFixture::new("app/other")
                .rdepend("sys-libs/base")
                .binpkg(Some(100))
                .build(),
            PkgFixture::new("sys-libs/base")
                .binpkg(Some(50))
                .build(),
        ]);
        let profile = Profile::test_default();
        let engine = Engine::new(&index, &profile);
        let world = vec!["app/top".to_string(), "app/other".to_string()];
        let base = diamond_base(&engine, &world);
        let impact = shared_removal_impact(&engine, &world, &base, "app/top", "js").unwrap();
        // mid wird allein entfernt -> keine Shared-Analyse noetig.
        assert!(impact.is_none());
    }
}
