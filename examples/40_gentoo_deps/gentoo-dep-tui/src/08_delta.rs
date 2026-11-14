//! 08_delta — Marginal-Delta-Engine und Big-Levers-Aggregation.
//!
//! Genau eine Zustaendigkeit: Was aendert ein Flag-Toggle *wirklich* an der
//! Gesamtgroesse (Reference-Counting steckt implizit in der Neuaufloesung)?
//! Jedes Delta wird durch Simulation (Toggle → Kaskade → Re-Resolve) exakt
//! berechnet, nicht geschaetzt. Bewiesen wirkungslose Toggles liefern per
//! exaktem No-Op-Skip (`flag_can_affect_graph`) sofort `0 B` —
//! bit-identisch zur vollen Simulation. Shared-Blocker-Analyse siehe
//! `08_delta/shared.rs`.

use std::collections::{BTreeSet, HashMap, HashSet};

use rayon::prelude::*;

use crate::mod_00_perf::{
    ScopeTimer, perf_count_big_levers, perf_count_big_levers_noop_skip, perf_count_delta,
    perf_count_delta_noop_skip,
};
use crate::mod_01_types::{BigLever, EngineResult, FlagDelta, SizeMetrics};
use crate::mod_06_graph::{Engine, ResolvedGraph};
use crate::mod_07_cascade::{CascadeOutcome, resolve_with_cascade_opts};

#[path = "08_delta/shared.rs"]
mod shared;
pub use shared::{
    CutPlan, FlagBlocker, RemovalSavings, SharedRemovalImpact, cut_package,
    shared_removal_impact,
};

/// Marginal-Delta eines einzelnen USE-Flag-Toggles auf einem Paket.
///
/// Simuliert den Toggle (inkl. Kaskaden aus `07_cascade`) und vergleicht die
/// Gesamtgroessen vorher/nachher. Shared Dependencies ergeben exakt `0 B`,
/// solange ein anderes Paket sie weiterhin braucht.
pub fn flag_delta(
    engine: &Engine<'_>,
    world: &[String],
    package: &str,
    flag: &str,
) -> EngineResult<FlagDelta> {
    let before_outcome = resolve_with_cascade_opts(&mut engine.fork(), world, false, false)?;
    flag_delta_impl(engine, world, package, flag, &before_outcome, true)
}

/// Wie `flag_delta`, nutzt aber eine vorberechnete Baseline (T04: kein Doppel-Resolve).
pub fn flag_delta_with_base(
    engine: &Engine<'_>,
    world: &[String],
    package: &str,
    flag: &str,
    before: &CascadeOutcome,
) -> EngineResult<FlagDelta> {
    flag_delta_impl(engine, world, package, flag, before, true)
}

/// Delta-Kern mit schaltbarem No-Op-Skip (`allow_skip = false` erzwingt die
/// volle Simulation — fuer Aequivalenz-Tests, review3 Task 3.4).
fn flag_delta_impl(
    engine: &Engine<'_>,
    world: &[String],
    package: &str,
    flag: &str,
    before: &CascadeOutcome,
    allow_skip: bool,
) -> EngineResult<FlagDelta> {
    perf_count_delta();
    let _timer = ScopeTimer::delta();
    if allow_skip && !flag_can_affect_graph(&before.graph, engine, package, flag) {
        perf_count_delta_noop_skip();
        return Ok(noop_delta(engine, package, flag, before));
    }
    // Toggle gegen Post-Kaskaden-Stand (nicht gegen reine Defaults).
    let toggled_on = !current_from_base(before, engine, package).contains(flag);
    let mut sim = engine.fork();
    sim.overrides
        .entry(package.to_string())
        .or_default()
        .insert(flag.to_string(), toggled_on);
    let outcome = resolve_with_cascade_opts(&mut sim, world, false, false)?;
    Ok(build_delta(engine, package, flag, before, outcome))
}

/// Exakter No-Op-Skip: Das Flag beeinflusst garantiert nichts (siehe
/// `flag_can_affect_graph`) — die volle Simulation wuerde bit-identisch
/// `0 B / 0 B / keine Paket-Aenderung` liefern (review3 Task 3.4).
fn noop_delta(
    engine: &Engine<'_>,
    package: &str,
    flag: &str,
    before: &CascadeOutcome,
) -> FlagDelta {
    let current = current_from_base(before, engine, package);
    FlagDelta {
        package: package.to_string(),
        flag: flag.to_string(),
        current: current.contains(flag),
        delta: SizeMetrics::zero(),
        cascade_note: cascade_note(engine, package, flag, !current.contains(flag)),
        // Identische Graphen → identische Konfliktlage wie die Baseline.
        conflict: before.conflicts.first().cloned(),
        added_packages: Vec::new(),
        removed_packages: Vec::new(),
        has_binhost_data: before.graph.nodes.values().any(|n| n.binpkg > 0),
    }
}

/// Ob ein Flag-Toggle auf `package` ueberhaupt etwas aendern kann (exakter
/// No-Op-Check, review3 Task 3.4).
///
/// `false` nur wenn das Flag (a) in keiner eigenen Liste des Pakets
/// (`rdepend`/`pdepend`-Conditionals und eingehende `use_deps`,
/// `src_uri`-Conditionals, `required_use`) und (b) in keinem eingehenden
/// `use_dep` (`base.graph.out_edges`) vorkommt. Ein Toggle aendert dann nur
/// `node.use_flags` — Graph, Groessen und Konflikte bleiben bit-identisch.
fn flag_can_affect_graph(
    base: &ResolvedGraph,
    engine: &Engine<'_>,
    package: &str,
    flag: &str,
) -> bool {
    let node = match base.nodes.get(package) {
        Some(n) => n,
        None => return true,
    };
    let pkg = match engine.index.select_cpv(package, &node.cpv) {
        Some(p) => p,
        None => return true,
    };
    let md = &pkg.entry.metadata;
    if dep_entries_mention_flag(md.rdepend.list(), flag)
        || dep_entries_mention_flag(md.pdepend.list(), flag)
        || src_uri_entries_mention_flag(md.src_uri.list(), flag)
    {
        return true;
    }
    if let Some(required_use) = &md.required_use
        && required_use_mentions_flag(required_use, flag)
    {
        return true;
    }
    // Eingehende `use_deps`: Atome anderer Pakete, die `package` mit
    // Flag-Constraint hereinziehen.
    for atoms in base.out_edges.values() {
        for atom in atoms {
            if atom.cpn.to_string() != package {
                continue;
            }
            if let Some(use_deps) = &atom.use_deps
                && use_deps.iter().any(|u| u.flag.as_str() == flag)
            {
                return true;
            }
        }
    }
    false
}

/// Ob ein Flag in `*DEPEND`-Listen vorkommt: `flag?`-Conditionals oder
/// `pkg[flag...]`-Constraints (konservativ ueber alle Use-Dep-Arten).
fn dep_entries_mention_flag(entries: &[portage_atom::DepEntry], flag: &str) -> bool {
    entries.iter().any(|entry| match entry {
        portage_atom::DepEntry::Atom(dep) => dep
            .use_deps
            .as_ref()
            .is_some_and(|uds| uds.iter().any(|u| u.flag.as_str() == flag)),
        portage_atom::DepEntry::UseConditional {
            flag: cond,
            children,
            ..
        } => cond.as_str() == flag || dep_entries_mention_flag(children, flag),
        portage_atom::DepEntry::AllOf(children)
        | portage_atom::DepEntry::AnyOf(children)
        | portage_atom::DepEntry::ExactlyOneOf(children)
        | portage_atom::DepEntry::AtMostOneOf(children) => dep_entries_mention_flag(children, flag),
    })
}

/// Ob ein Flag in `SRC_URI`-Conditionals vorkommt.
fn src_uri_entries_mention_flag(entries: &[portage_metadata::SrcUriEntry], flag: &str) -> bool {
    entries.iter().any(|entry| match entry {
        portage_metadata::SrcUriEntry::UseConditional {
            flag: cond,
            entries: children,
            ..
        } => cond == flag || src_uri_entries_mention_flag(children, flag),
        portage_metadata::SrcUriEntry::Group(children) => {
            src_uri_entries_mention_flag(children, flag)
        }
        portage_metadata::SrcUriEntry::Uri { .. }
        | portage_metadata::SrcUriEntry::Renamed { .. } => false,
    })
}

/// Ob ein Flag in `REQUIRED_USE` vorkommt.
fn required_use_mentions_flag(expr: &portage_metadata::RequiredUseExpr, flag: &str) -> bool {
    match expr {
        portage_metadata::RequiredUseExpr::Flag { name, .. } => name == flag,
        portage_metadata::RequiredUseExpr::UseConditional {
            flag: cond,
            entries,
            ..
        } => cond == flag || entries.iter().any(|e| required_use_mentions_flag(e, flag)),
        portage_metadata::RequiredUseExpr::AnyOf(exprs)
        | portage_metadata::RequiredUseExpr::ExactlyOne(exprs)
        | portage_metadata::RequiredUseExpr::AtMostOne(exprs)
        | portage_metadata::RequiredUseExpr::All(exprs) => {
            exprs.iter().any(|e| required_use_mentions_flag(e, flag))
        }
    }
}

/// Delta aus Baseline + Simulation bauen (inkl. Paket-Diff, T08).
fn build_delta(
    engine: &Engine<'_>,
    package: &str,
    flag: &str,
    before: &CascadeOutcome,
    outcome: CascadeOutcome,
) -> FlagDelta {
    let current = current_from_base(before, engine, package);
    let before_totals = before.graph.totals();
    let new_totals = outcome.graph.totals();
    let (added, removed) = diff_packages(&before.graph, &outcome.graph);
    FlagDelta {
        package: package.to_string(),
        flag: flag.to_string(),
        current: current.contains(flag),
        delta: SizeMetrics {
            binpkg_bytes: new_totals.binpkg_bytes - before_totals.binpkg_bytes,
            download_bytes: new_totals.download_bytes - before_totals.download_bytes,
        },
        cascade_note: cascade_note(engine, package, flag, !current.contains(flag)),
        conflict: outcome.conflicts.into_iter().next(),
        added_packages: added,
        removed_packages: removed,
        has_binhost_data: outcome.graph.nodes.values().any(|n| n.binpkg > 0)
            || before.graph.nodes.values().any(|n| n.binpkg > 0),
    }
}

/// Post-Kaskaden-Stand aus der Baseline (fällt auf Defaults + Overrides
/// zurueck, wenn das Paket fehlt). Kaskaden-erzwungene Flags zeigen hier
/// `[x]` statt fälschlich `[ ]` (Task 3.1).
fn current_from_base(
    before: &CascadeOutcome,
    engine: &Engine<'_>,
    package: &str,
) -> HashSet<String> {
    match before.graph.nodes.get(package) {
        Some(node) => node.use_flags.clone(),
        None => current_flags(engine, package),
    }
}

/// Paket-Differenz zwischen zwei Graphen (sortiert, stabil).
fn diff_packages(before: &ResolvedGraph, after: &ResolvedGraph) -> (Vec<String>, Vec<String>) {
    let mut added: Vec<String> = after
        .nodes
        .keys()
        .filter(|k| !before.nodes.contains_key(*k))
        .cloned()
        .collect();
    let mut removed: Vec<String> = before
        .nodes
        .keys()
        .filter(|k| !after.nodes.contains_key(*k))
        .cloned()
        .collect();
    added.sort();
    removed.sort();
    (added, removed)
}

/// Ob ein Flag per `USE_EXPAND_HIDDEN` versteckt ist (z. B. `abi_x86_64` bei
/// `USE_EXPAND_HIDDEN="ABI_X86 ..."`) und daher kein schaltbarer Hebel ist
/// (review3 Task 1.1).
pub(crate) fn is_hidden_expand_flag(profile: &crate::mod_02_profile::Profile, flag: &str) -> bool {
    let lower = flag.to_lowercase();
    profile.use_expand_hidden.iter().any(|h| {
        let prefix = h.to_lowercase();
        let prefix = prefix.trim_end_matches('*');
        lower == prefix || lower.starts_with(&format!("{prefix}_"))
    })
}

/// Ob ein Flag ein Plattform-Fakt ist (ARCH/ELIBC/KERNEL, nicht toggelbar).
pub(crate) fn is_platform_flag(profile: &crate::mod_02_profile::Profile, flag: &str) -> bool {
    flag == profile.arch.to_lowercase()
        || (!profile.elibc.is_empty() && flag == format!("elibc_{}", profile.elibc.to_lowercase()))
        || (!profile.kernel.is_empty()
            && flag == format!("kernel_{}", profile.kernel.to_lowercase()))
}

/// Aktuelle effektive Flags (Defaults + explizite Overrides).
fn current_flags(engine: &Engine<'_>, package: &str) -> HashSet<String> {
    match engine.index.select(package) {
        Some(pkg) => engine.effective_use(pkg),
        None => HashSet::new(),
    }
}

/// Kurzer Kaskaden-Hinweis (erzwungene weitere Flags am selben Paket).
fn cascade_note(
    engine: &Engine<'_>,
    package: &str,
    flag: &str,
    toggled_on: bool,
) -> Option<String> {
    let pkg = engine.index.select(package)?;
    let required = pkg.entry.metadata.required_use.as_ref()?;
    let text = format!("{required:?}");
    if text.contains(flag) {
        Some(format!(
            "REQUIRED_USE beruehrt {flag} (Toggle {}), Kaskade moeglich",
            if toggled_on { "an" } else { "aus" }
        ))
    } else {
        None
    }
}

/// Globale Big Levers: jedes aktive USE-Flag global abschalten.
///
/// Standard-Fokus ist ein minimales System (`turn_on == false`); mit
/// `include_enable` werden auch wieder einschaltbare Flags geliefert.
pub fn big_levers(engine: &Engine<'_>, world: &[String]) -> EngineResult<Vec<BigLever>> {
    let base_outcome = resolve_with_cascade_opts(&mut engine.fork(), world, false, false)?;
    big_levers_with_base(engine, world, &base_outcome)
}

/// Wie `big_levers`, nutzt aber eine vorberechnete Baseline (kein
/// Doppel-Resolve; `AppState` reicht sein `CascadeOutcome` durch).
pub fn big_levers_with_base(
    engine: &Engine<'_>,
    world: &[String],
    base: &CascadeOutcome,
) -> EngineResult<Vec<BigLever>> {
    big_levers_with_opts(engine, world, base, false)
}

/// Big Levers mit vorberechneter Baseline und optionalen Enable-Hebeln.
pub fn big_levers_with_opts(
    engine: &Engine<'_>,
    world: &[String],
    base: &CascadeOutcome,
    include_enable: bool,
) -> EngineResult<Vec<BigLever>> {
    big_levers_impl(engine, world, base, true, include_enable)
}

/// Big-Levers-Kern mit schaltbarem No-Op-Skip (`allow_skip = false` erzwingt
/// volle Simulationen — fuer Aequivalenz-Tests).
fn big_levers_impl(
    engine: &Engine<'_>,
    world: &[String],
    base: &CascadeOutcome,
    allow_skip: bool,
    include_enable: bool,
) -> EngineResult<Vec<BigLever>> {
    perf_count_big_levers();
    let _timer = ScopeTimer::big_levers();
    let base_outcome = base;
    let base_totals = base_outcome.graph.totals();

    // Alle (Paket, Flag-an)-Paare einsammeln; dazu Flags, die irgendwo
    // bekannt, aber ueberall aus sind (T07: global wieder einschaltbar).
    // Zwei Durchgaenge (erst alle aktiven sammeln!), sonst waere die
    // Aufteilung von der HashMap-Iteration (nicht-deterministisch) abhaengig.
    // Plattform-Fakten (ARCH/ELIBC/KERNEL) sowie profil-maskierte und
    // -erzwungene Flags sind keine toggelbaren Hebel (Task 3.1).
    let toggleable = |flag: &str| {
        !is_platform_flag(engine.profile, flag)
            && !engine.profile.is_masked(flag)
            && !engine.profile.is_forced(flag)
            && !is_hidden_expand_flag(engine.profile, flag)
    };
    let mut flag_packages: HashMap<String, Vec<String>> = HashMap::new();
    for (cpn, node) in &base_outcome.graph.nodes {
        for flag in &node.use_flags {
            if !toggleable(flag) {
                continue;
            }
            flag_packages
                .entry(flag.clone())
                .or_default()
                .push(cpn.clone());
        }
    }
    let mut known_off: HashMap<String, Vec<String>> = HashMap::new();
    for (cpn, node) in &base_outcome.graph.nodes {
        if let Some(pkg) = engine.index.select_cpv(cpn, &node.cpv) {
            for flag in pkg.iuse_defaults.keys() {
                if !toggleable(flag) {
                    continue;
                }
                if !node.use_flags.contains(flag) && !flag_packages.contains_key(flag) {
                    known_off.entry(flag.clone()).or_default().push(cpn.clone());
                }
            }
        }
    }

    let mut flags: Vec<String> = flag_packages.keys().cloned().collect();
    flags.sort();
    let mut off_flags: Vec<String> = known_off.keys().cloned().collect();
    off_flags.sort();
    let levers: EngineResult<Vec<BigLever>> = flags
        .into_par_iter()
        .map(|flag| {
            big_lever_for_flag(
                engine,
                world,
                base_outcome,
                base_totals,
                &flag_packages[&flag],
                &flag,
                false,
                allow_skip,
            )
        })
        .collect();
    let mut levers = levers?;
    if include_enable {
        let mut off_levers: Vec<BigLever> = off_flags
            .into_par_iter()
            .map(|flag| {
                big_lever_for_flag(
                    engine,
                    world,
                    base_outcome,
                    base_totals,
                    &known_off[&flag],
                    &flag,
                    true,
                    allow_skip,
                )
            })
            .collect::<EngineResult<Vec<BigLever>>>()?;
        levers.append(&mut off_levers);
    }
    let has_bin = engine.index.has_any_binhost();
    levers.sort_by(|a, b| {
        if has_bin {
            a.total
                .binpkg_bytes
                .cmp(&b.total.binpkg_bytes)
                .then_with(|| a.net_pkg_delta.cmp(&b.net_pkg_delta))
        } else {
            a.net_pkg_delta
                .cmp(&b.net_pkg_delta)
        }
        .then_with(|| a.total.download_bytes.cmp(&b.total.download_bytes))
        .then_with(|| a.flag.cmp(&b.flag))
    });
    Ok(levers)
}

/// Ein einzelner Big-Lever-Lauf (an- oder abschalten, je `turn_on`).
///
/// Trifft das Flag garantiert keine der betroffenen Paketlisten
/// (`flag_can_affect_graph` auf allen Paketen negativ), entfaellt die volle
/// Kaskaden-Simulation: Ergebnis bit-identisch `0 / 0 / Baseline-Konflikte`
/// (review3 Task 3.4).
#[allow(clippy::too_many_arguments)]
fn big_lever_for_flag(
    engine: &Engine<'_>,
    world: &[String],
    base: &CascadeOutcome,
    base_totals: SizeMetrics,
    packages: &[String],
    flag: &str,
    turn_on: bool,
    allow_skip: bool,
) -> EngineResult<BigLever> {
    let mut affected_packages: Vec<String> = packages.to_vec();
    affected_packages.sort();
    if allow_skip
        && packages
            .iter()
            .all(|cpn| !flag_can_affect_graph(&base.graph, engine, cpn, flag))
    {
        perf_count_big_levers_noop_skip();
        return Ok(BigLever {
            flag: flag.to_string(),
            turn_on,
            packages: packages.len(),
            affected_packages,
            total: SizeMetrics::zero(),
            net_pkg_delta: 0,
            conflicts: base.conflicts.len(),
            added_packages: Vec::new(),
            removed_packages: Vec::new(),
        });
    }
    let mut sim = engine.fork();
    for cpn in packages {
        sim.overrides
            .entry(cpn.clone())
            .or_default()
            .insert(flag.to_string(), turn_on);
    }
    let outcome = resolve_with_cascade_opts(&mut sim, world, false, false)?;
    let totals = outcome.graph.totals();
    let (added_packages, removed_packages) = diff_packages(&base.graph, &outcome.graph);
    let net_pkg_delta = added_packages.len() as i64 - removed_packages.len() as i64;
    Ok(BigLever {
        flag: flag.to_string(),
        turn_on,
        packages: packages.len(),
        affected_packages,
        total: SizeMetrics {
            binpkg_bytes: totals.binpkg_bytes - base_totals.binpkg_bytes,
            download_bytes: totals.download_bytes - base_totals.download_bytes,
        },
        net_pkg_delta,
        conflicts: outcome.conflicts.len(),
        added_packages,
        removed_packages,
    })
}

/// Alle Flags eines Pakets mit Deltas (fuer `deltas <cat/pkg>`).
pub fn package_deltas(
    engine: &Engine<'_>,
    world: &[String],
    package: &str,
) -> EngineResult<Vec<FlagDelta>> {
    // Baseline genau einmal (T04), Simulationen parallel via rayon.
    let base = resolve_with_cascade_opts(&mut engine.fork(), world, false, false)?;
    package_deltas_with_base(engine, world, package, &base)
}

/// Wie `package_deltas`, nutzt aber eine vorberechnete Baseline (kein
/// Doppel-Resolve; `AppState` reicht sein `last_outcome` durch, review3 3.4).
pub fn package_deltas_with_base(
    engine: &Engine<'_>,
    world: &[String],
    package: &str,
    base: &CascadeOutcome,
) -> EngineResult<Vec<FlagDelta>> {
    // Kandidaten aus der exakt aufgeloesten Version (IUSE) + Post-Kaskaden-
    // Stand (aktive Flags) — sortiert, stabil (Task 3.1).
    let mut candidates: BTreeSet<String> = BTreeSet::new();
    if let Some(node) = base.graph.nodes.get(package)
        && let Some(pkg) = engine.index.select_cpv(package, &node.cpv)
    {
        candidates.extend(pkg.iuse_defaults.keys().cloned());
    }
    candidates.extend(current_from_base(base, engine, package));
    // Profil-maskierte Flags sind nie schaltbar.
    candidates.retain(|f| !engine.profile.is_masked(f));
    // Plattform-Fakten (ARCH/ELIBC/KERNEL) sind keine schaltbaren USE-Flags.
    candidates.retain(|f| !is_platform_flag(engine.profile, f));
    // Versteckte Expand-Flags (`USE_EXPAND_HIDDEN`) ebenfalls nicht.
    candidates.retain(|f| !is_hidden_expand_flag(engine.profile, f));

    let flags: Vec<String> = candidates.into_iter().collect();
    let deltas: EngineResult<Vec<FlagDelta>> = flags
        .into_par_iter()
        .map(|flag| flag_delta_impl(engine, world, package, &flag, base, true))
        .collect();
    let mut out = deltas?;
    out.sort_by(|a, b| a.flag.cmp(&b.flag));
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mod_02_profile::Profile;
    use crate::mod_05_repo_index::{PkgVersion, RepoIndex};
    use crate::test_helpers::{PkgFixture, repo_index};

    fn ssl_pkg(cpn: &str, rdepend: &str, binpkg: u64) -> PkgVersion {
        PkgFixture::new(cpn)
            .iuse("+ssl")
            .rdepend(rdepend)
            .manifest("DIST a.tar.gz 1000\n")
            .binpkg(Some(binpkg))
            .build()
    }

    fn fixture() -> (RepoIndex, Profile) {
        let index = repo_index(vec![
            ssl_pkg("app/a", "ssl? ( dev-libs/openssl )", 100),
            ssl_pkg("app/b", "ssl? ( dev-libs/openssl )", 100),
            ssl_pkg("dev-libs/openssl", "", 800),
        ]);
        (index, Profile::test_default())
    }

    #[test]
    fn cascade_enabled_flag_shows_current() {
        // `wayland? ( egl )`, wayland an → egl ist Post-Kaskade an und
        // meldet `current == true` (kein `0 B`-Geistertoggle).
        let mut map: HashMap<String, Vec<PkgVersion>> = HashMap::new();
        let p = PkgFixture::new("gui/app")
            .version("1.0")
            .iuse("wayland egl")
            .required_use("wayland? ( egl )")
            .manifest("DIST a.tar.gz 1000\n")
            .binpkg(Some(100))
            .build();
        map.entry(p.cpn.clone()).or_default().push(p);
        let index = RepoIndex {
            versions: map,
            cache_files: 1,
            load_ms: 0,
        };
        let profile = Profile::test_default();
        let mut engine = Engine::new(&index, &profile);
        engine.overrides.insert(
            "gui/app".into(),
            [("wayland".to_string(), true)].into_iter().collect(),
        );
        let world = vec!["gui/app".to_string()];
        let deltas = package_deltas(&engine, &world, "gui/app").unwrap();
        let egl = deltas.iter().find(|d| d.flag == "egl").unwrap();
        assert!(egl.current);
    }

    #[test]
    fn big_levers_skip_platform_mask_force() {
        // Kein Hebel fuer Plattform-Fakten, Masken oder Forces.
        let index = repo_index(vec![
            PkgFixture::new("app/a")
                .version("1.0")
                .iuse("+ssl masked forced elibc_glibc kernel_linux")
                .manifest("DIST a.tar.gz 1000\n")
                .binpkg(Some(100))
                .build(),
        ]);
        let mut profile = Profile::test_default();
        profile.elibc = "glibc".into();
        profile.kernel = "linux".into();
        profile.use_mask.insert("masked".into());
        profile.use_force.insert("forced".into());
        let engine = Engine::new(&index, &profile);
        let world = vec!["app/a".to_string()];
        let levers = big_levers(&engine, &world).unwrap();
        assert!(!levers.is_empty());
        for lever in &levers {
            assert!(
                !["amd64", "elibc_glibc", "kernel_linux", "masked", "forced"]
                    .contains(&lever.flag.as_str()),
                "kein Hebel erwartet: {}",
                lever.flag
            );
        }
        assert!(levers.iter().any(|l| l.flag == "ssl"));
    }

    #[test]
    fn shared_dep_toggle_saves_zero_until_last_user() {
        let (index, profile) = fixture();
        let engine = Engine::new(&index, &profile);
        let world = vec!["app/a".to_string(), "app/b".to_string()];
        // Nur bei A abschalten: openssl bleibt ueber B → exakt 0 B Binpkg.
        let d = flag_delta(&engine, &world, "app/a", "ssl").unwrap();
        assert_eq!(d.delta.binpkg_bytes, 0);
        // Global-Hebel ssl: beide aus → -800 B.
        let levers = big_levers(&engine, &world).unwrap();
        let ssl = levers.iter().find(|l| l.flag == "ssl").unwrap();
        assert_eq!(ssl.total.binpkg_bytes, -800);
        assert_eq!(ssl.packages, 3);
    }

    #[test]
    fn package_deltas_lists_all_flags() {
        let (index, profile) = fixture();
        let engine = Engine::new(&index, &profile);
        let world = vec!["app/a".to_string(), "app/b".to_string()];
        let deltas = package_deltas(&engine, &world, "app/a").unwrap();
        assert_eq!(deltas.len(), 1);
        assert!(deltas[0].current);
    }

    #[test]
    fn phantom_flags_from_global_use_are_gone() {
        // `openmp` global gesetzt, aber in keinem IUSE deklariert: weder in
        // Knoten-Flags noch in Deltas noch in Big Levers (review3 Task 1.1).
        let index = repo_index(vec![
            ssl_pkg("app/a", "ssl? ( dev-libs/openssl )", 100),
            ssl_pkg("dev-libs/openssl", "", 800),
        ]);
        let mut profile = Profile::test_default();
        profile.global_use.insert("openmp".into());
        profile.global_use.insert("cet".into());
        let engine = Engine::new(&index, &profile);
        let world = vec!["app/a".to_string()];
        let base = resolve_with_cascade_opts(&mut engine.fork(), &world, false, false).unwrap();
        assert!(!base.graph.nodes["app/a"].use_flags.contains("openmp"));
        assert!(!base.graph.nodes["app/a"].use_flags.contains("cet"));
        let deltas = package_deltas(&engine, &world, "app/a").unwrap();
        assert!(deltas.iter().all(|d| d.flag != "openmp"));
        assert!(deltas.iter().all(|d| d.flag != "cet"));
        let levers = big_levers(&engine, &world).unwrap();
        assert!(levers.iter().all(|l| l.flag != "openmp"));
        assert!(levers.iter().all(|l| l.flag != "cet"));
    }

    #[test]
    fn hidden_expand_flags_are_not_levers() {
        // `abi_x86_64` ist deklariert und aktiv, aber per USE_EXPAND_HIDDEN
        // versteckt: keine Delta-/Lever-Kandidatur (review3 Task 1.1).
        let index = repo_index(vec![
            PkgFixture::new("app/a")
                .iuse("+abi_x86_64 +ssl")
                .rdepend("")
                .binpkg(Some(100))
                .build(),
        ]);
        let mut profile = Profile::test_default();
        profile.use_expand_hidden.insert("ABI_X86".into());
        let engine = Engine::new(&index, &profile);
        let world = vec!["app/a".to_string()];
        let deltas = package_deltas(&engine, &world, "app/a").unwrap();
        assert!(deltas.iter().all(|d| d.flag != "abi_x86_64"));
        assert!(deltas.iter().any(|d| d.flag == "ssl"));
        let levers = big_levers(&engine, &world).unwrap();
        assert!(levers.iter().all(|l| l.flag != "abi_x86_64"));
    }

    #[test]
    fn noop_skip_matches_full_simulation() {
        // `doc` steht in IUSE, aber in keiner Liste: Skip muss bit-identisch
        // zur vollen Simulation sein; `ssl` zieht wirklich (kein Skip).
        let index = repo_index(vec![
            PkgFixture::new("app/a")
                .iuse("+doc +ssl")
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
        let base = resolve_with_cascade_opts(&mut engine.fork(), &world, false, false).unwrap();
        assert!(!flag_can_affect_graph(&base.graph, &engine, "app/a", "doc"));
        assert!(flag_can_affect_graph(&base.graph, &engine, "app/a", "ssl"));
        for flag in ["doc", "ssl"] {
            let full = flag_delta_impl(&engine, &world, "app/a", flag, &base, false).unwrap();
            let skipped = flag_delta_impl(&engine, &world, "app/a", flag, &base, true).unwrap();
            assert_eq!(full, skipped, "Flag {flag}");
        }
        let full_levers = big_levers_impl(&engine, &world, &base, false, false).unwrap();
        let skipped_levers = big_levers_impl(&engine, &world, &base, true, false).unwrap();
        assert_eq!(full_levers, skipped_levers);
    }

    #[test]
    fn flag_delta_reports_added_packages() {
        // ffmpeg-Simulator: svt-av1 zieht svt-av1-Lib rein.
        let index = repo_index(vec![
            PkgFixture::new("media-video/ffmpeg")
                .iuse("svt-av1")
                .rdepend("svt-av1? ( media-libs/svt-av1 )")
                .manifest("DIST a.tar.gz 1000\n")
                .binpkg(Some(100))
                .build(),
            PkgFixture::new("media-libs/svt-av1")
                .manifest("DIST a.tar.gz 1000\n")
                .binpkg(Some(500))
                .build(),
        ]);
        let profile = Profile::test_default();
        let engine = Engine::new(&index, &profile);
        let world = vec!["media-video/ffmpeg".to_string()];
        let d = flag_delta(&engine, &world, "media-video/ffmpeg", "svt-av1").unwrap();
        assert_eq!(d.added_packages, vec!["media-libs/svt-av1".to_string()]);
        assert!(d.has_binhost_data);
    }
}
