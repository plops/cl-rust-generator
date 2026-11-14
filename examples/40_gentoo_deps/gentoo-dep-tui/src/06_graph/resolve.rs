//! 06_graph/resolve — BFS-Resolver: Welt-Atome → Laufzeitgraph.
//!
//! Genau eine Zustaendigkeit: Version-/Slot-treue Auswahl und Abstieg
//! ueber `RDEPEND`/`PDEPEND` (inkl. `||`-Alternativen und Via-Flags).

use std::collections::{HashMap, HashSet, VecDeque};

use portage_atom::{Dep, DepEntry};

use crate::mod_01_types::{AlternativeChoice, EngineError, EngineResult, SkippedDep};
use crate::mod_05_repo_index::PkgVersion;

use super::build_extra::{download_bytes, resolve_build_extra};
use super::{Engine, ResolvedGraph, ResolvedNode};

/// Welt-Atome → `ResolvedGraph` (ohne Kaskaden, siehe `07_cascade`).
pub fn resolve(engine: &Engine<'_>, world: &[String]) -> EngineResult<ResolvedGraph> {
    resolve_with_options(engine, world, true, true)
}

/// Wie `resolve`, aber `BDEPEND`-Extra und Blame-Sammlung steuerbar.
///
/// `collect_blame = false` (Delta-/Big-Lever-Simulationen): `reverse`,
/// `refcounts`, `alternatives` und `via_map` entfallen — `out_edges`,
/// Knoten und Diagnosen bleiben vollstaendig (review3 Task 3.3).
pub fn resolve_with_options(
    engine: &Engine<'_>,
    world: &[String],
    include_build: bool,
    collect_blame: bool,
) -> EngineResult<ResolvedGraph> {
    crate::mod_00_perf::perf_count_resolve();
    let _timer = crate::mod_00_perf::ScopeTimer::resolve();
    let mut graph = ResolvedGraph {
        world: world.to_vec(),
        ..ResolvedGraph::default()
    };
    // Queue: (cpn, geparstes waehlendes Atom). Das Atom transportiert die
    // exakte Versions-/Slot-Restriktion, sodass `resolve_target` dieselbe
    // Version waehlt wie `select_for_dep` am Fundort (kein stiller Fallback
    // auf die neueste Version, Task 2.1). Geparst statt String: kein
    // `to_string()` + `Dep::parse()` pro BFS-Knoten (review3 Task 3.3).
    let mut queue: VecDeque<(String, Dep)> = VecDeque::new();

    for atom in world {
        let dep = Dep::parse(atom).map_err(|e| EngineError::Parse {
            context: atom.clone(),
            message: e.to_string(),
        })?;
        if dep.blocker.is_some() {
            continue;
        }
        match engine.index.select_for_dep(&dep) {
            Some(pkg) => queue.push_back((pkg.cpn.clone(), dep)),
            None => {
                // Z. B. veralteter Slot-Pin in world: warnen, Rest aufloesen.
                graph.skipped.push(SkippedDep {
                    atom: atom.clone(),
                    required_by: "world".into(),
                    reason: "keine passende Version im Index".into(),
                });
            }
        }
    }

    while let Some((cpn, required_by)) = queue.pop_front() {
        if graph.nodes.contains_key(&cpn) {
            continue;
        }
        // Versionswahl: Atom-Restriktion bevorzugen, sonst Top-Version.
        let pkg = resolve_target(engine, &cpn, &required_by)?;
        let active = engine.effective_use(pkg);
        let download = download_bytes(pkg, &active);
        graph.nodes.insert(
            cpn.clone(),
            ResolvedNode {
                cpn: cpn.clone(),
                cpv: pkg.cpv.clone(),
                slot: pkg.slot.clone(),
                binpkg: pkg.binhost_size.unwrap_or(0),
                download,
                use_flags: active.clone(),
                has_binpkg: pkg.binhost_size.is_some(),
            },
        );

        // Via-Flags aus dem *rohen* Baum einsammeln (evaluate_use loescht sie).
        // `RDEPEND` + `PDEPEND` bilden gemeinsam die Runtime (Task 2.1).
        // Ohne Blame-Bedarf entfaellt das (Simulationen, review3 Task 3.3).
        let mut via_map: HashMap<String, (usize, Option<String>)> = HashMap::new();
        if collect_blame {
            collect_via_flags(
                pkg.entry.metadata.rdepend.list(),
                &active,
                &mut via_map,
                None,
                0,
            );
            collect_via_flags(
                pkg.entry.metadata.pdepend.list(),
                &active,
                &mut via_map,
                None,
                0,
            );
        }

        let mut evaluated = DepEntry::evaluate_use(pkg.entry.metadata.rdepend.list(), &active);
        evaluated.extend(DepEntry::evaluate_use(
            pkg.entry.metadata.pdepend.list(),
            &active,
        ));
        descend_runtime(
            engine,
            &evaluated,
            &cpn,
            &mut graph,
            &mut queue,
            &via_map,
            collect_blame,
        )?;
    }

    // Reference-Counts aus Rueckwaertskanten (nur mit Blame-Daten).
    if collect_blame {
        for (child, parents) in &graph.reverse {
            let mut distinct = HashSet::new();
            for (parent, _) in parents {
                distinct.insert(parent.clone());
            }
            graph.refcounts.insert(child.clone(), distinct.len());
        }
    }

    if include_build {
        resolve_build_extra(engine, &mut graph)?;
    }
    Ok(graph)
}

/// Zielversion fuer einen Knoten (world-Atom oder Top-Version).
fn resolve_target<'b>(
    engine: &'b Engine<'b>,
    cpn: &str,
    required_by: &Dep,
) -> EngineResult<&'b PkgVersion> {
    if required_by.cpn.to_string() == cpn
        && required_by.version.is_some()
        && let Some(pkg) = engine.index.select_for_dep(required_by)
    {
        return Ok(pkg);
    }
    engine
        .index
        .select(cpn)
        .ok_or_else(|| EngineError::Unresolvable {
            atom: cpn.to_string(),
            required_by: required_by.to_string(),
        })
}

/// Evaluierte Runtime-Eintraege absteigen (rekursiv, zyklenfest via `nodes`).
#[allow(clippy::too_many_arguments)]
fn descend_runtime(
    engine: &Engine<'_>,
    entries: &[DepEntry],
    owner: &str,
    graph: &mut ResolvedGraph,
    queue: &mut VecDeque<(String, Dep)>,
    via_map: &HashMap<String, (usize, Option<String>)>,
    collect_blame: bool,
) -> EngineResult<()> {
    for entry in entries {
        match entry {
            DepEntry::Atom(dep) => {
                // Auch Blocker vermerken (invertierte Constraints in 07),
                // aber keine Kanten/Knoten fuer sie anlegen.
                push_edge(graph, owner, dep);
                if dep.blocker.is_some() {
                    continue;
                }
                let Some(pkg) = engine.index.select_for_dep(dep) else {
                    graph.skipped.push(SkippedDep {
                        atom: dep.to_string(),
                        required_by: owner.to_string(),
                        reason: "keine passende Version im Index".into(),
                    });
                    continue;
                };
                if collect_blame {
                    let via = via_map.get(&pkg.cpn).and_then(|(_, v)| v.clone());
                    push_reverse(graph, &pkg.cpn, owner, via);
                }
                if !graph.nodes.contains_key(&pkg.cpn) {
                    // Das waehlende Atom weiterreichen (Version/Slot bleiben
                    // erhalten, Task 2.1) statt nacktem cpn.
                    queue.push_back((pkg.cpn.clone(), dep.clone()));
                }
            }
            DepEntry::AllOf(children) => {
                descend_runtime(
                    engine,
                    children,
                    owner,
                    graph,
                    queue,
                    via_map,
                    collect_blame,
                )?;
            }
            DepEntry::AnyOf(children) => {
                descend_any_of(
                    engine,
                    children,
                    owner,
                    graph,
                    queue,
                    via_map,
                    collect_blame,
                )?;
            }
            DepEntry::ExactlyOneOf(children) | DepEntry::AtMostOneOf(children) => {
                // Aufloesung wie `||` (Validierung in `07_cascade`); keine
                // Alternative im engeren Sinne (`||`-Task T06).
                pick_first(
                    engine,
                    children,
                    owner,
                    graph,
                    queue,
                    via_map,
                    collect_blame,
                )?;
            }
            DepEntry::UseConditional { .. } => {
                // Nach evaluate_use unerwartet: defensiv ignorieren.
            }
        }
    }
    Ok(())
}

/// `||`-Gruppe: erste gueltige Option (oder gespeicherte Entscheidung).
#[allow(clippy::too_many_arguments)]
fn descend_any_of(
    engine: &Engine<'_>,
    children: &[DepEntry],
    owner: &str,
    graph: &mut ResolvedGraph,
    queue: &mut VecDeque<(String, Dep)>,
    via_map: &HashMap<String, (usize, Option<String>)>,
    collect_blame: bool,
) -> EngineResult<()> {
    let mut candidates: Vec<String> = Vec::new();
    let mut selectable: Vec<(usize, String, Dep)> = Vec::new();
    for child in children {
        let mut atoms = Vec::new();
        DepEntry::walk_atoms(std::slice::from_ref(child), &mut |d| atoms.push(d.clone()));
        // Nur der GEWAEHLTE Zweig wird vermerkt (Blocker in `||` ignoriert —
        // in der Praxis top-level; siehe descend_runtime).
        for atom in atoms {
            if atom.blocker.is_some() {
                continue;
            }
            candidates.push(atom.to_string());
            if let Some(pkg) = engine.index.select_for_dep(&atom) {
                selectable.push((candidates.len() - 1, pkg.cpn.clone(), atom));
            }
        }
    }
    if selectable.is_empty() {
        graph.skipped.push(SkippedDep {
            atom: format!("|| ( {} )", candidates.join(" ")),
            required_by: owner.to_string(),
            reason: "keine Alternative im Index".into(),
        });
        return Ok(());
    }
    let group_id = format!("{owner}||{}", candidates.join(","));
    let want = engine.or_decisions.get(&group_id).copied().unwrap_or(0);
    let (sel_idx, sel_cpn, sel_atom) = selectable
        .iter()
        .find(|(i, _, _)| *i == want)
        .or(selectable.first())
        .cloned()
        .unwrap();
    push_edge(graph, owner, &sel_atom);
    if collect_blame {
        graph.alternatives.push(AlternativeChoice {
            group_id,
            owner: owner.to_string(),
            candidates,
            selected: sel_idx,
        });
        let via = via_map.get(&sel_cpn).and_then(|(_, v)| v.clone());
        push_reverse(graph, &sel_cpn, owner, via);
    }
    if !graph.nodes.contains_key(&sel_cpn) {
        // Waehlendes `||`-Atom weiterreichen (Version/Slot, Task 2.1).
        queue.push_back((sel_cpn, sel_atom));
    }
    Ok(())
}

/// Erste gueltige Option ohne Alternativen-Eintrag.
#[allow(clippy::too_many_arguments)]
fn pick_first(
    engine: &Engine<'_>,
    children: &[DepEntry],
    owner: &str,
    graph: &mut ResolvedGraph,
    queue: &mut VecDeque<(String, Dep)>,
    via_map: &HashMap<String, (usize, Option<String>)>,
    collect_blame: bool,
) -> EngineResult<()> {
    let mut atoms = Vec::new();
    DepEntry::walk_atoms(children, &mut |d| atoms.push(d.clone()));
    // Nur der GEWAEHLTE Zweig wird vermerkt (siehe descend_any_of).
    for atom in atoms {
        if atom.blocker.is_some() {
            continue;
        }
        if let Some(pkg) = engine.index.select_for_dep(&atom) {
            push_edge(graph, owner, &atom);
            if collect_blame {
                let via = via_map.get(&pkg.cpn).and_then(|(_, v)| v.clone());
                push_reverse(graph, &pkg.cpn, owner, via);
            }
            if !graph.nodes.contains_key(&pkg.cpn) {
                queue.push_back((pkg.cpn.clone(), atom));
            }
            return Ok(());
        }
    }
    graph.skipped.push(SkippedDep {
        atom: "^^/?? ( ... )".into(),
        required_by: owner.to_string(),
        reason: "keine Alternative im Index".into(),
    });
    Ok(())
}

/// Aufloesungs-Kante vermerken (doppelte vermeiden).
fn push_edge(graph: &mut ResolvedGraph, owner: &str, atom: &Dep) {
    let entry = graph.out_edges.entry(owner.to_string()).or_default();
    if !entry.contains(atom) {
        entry.push(atom.clone());
    }
}

/// Rueckwaertskante Kind → Eltern (doppelte vermeiden).
fn push_reverse(graph: &mut ResolvedGraph, child: &str, parent: &str, via: Option<String>) {
    let entry = graph.reverse.entry(child.to_string()).or_default();
    if !entry.iter().any(|(p, v)| p == parent && *v == via) {
        entry.push((parent.to_string(), via));
    }
}

/// Via-Flags aus dem rohen Baum: bedingte Kinder → ausloesendes Flag.
///
/// `cond`: innerste aktive Bedingung (`"foo"` bzw. `"!foo"` bei Negation),
/// `depth`: Verschachtelungstiefe. Unbedingte Atome tragen `None` ein und
/// gewinnen immer — unabhaengig von der `RDEPEND`-Reihenfolge. Bei
/// verschachtelten Bedingungen gewinnt die tiefste (spezifischste, Ties:
/// erste); `None` bleibt davon unberuehrt (Task 2.2).
fn collect_via_flags(
    entries: &[DepEntry],
    active: &HashSet<String>,
    out: &mut HashMap<String, (usize, Option<String>)>,
    cond: Option<String>,
    depth: usize,
) {
    for entry in entries {
        match entry {
            DepEntry::Atom(dep) => {
                match cond.clone() {
                    // Unbedingt: gewinnt immer (ueberschreibt Bedingtes).
                    None => {
                        out.insert(dep.cpn.to_string(), (0, None));
                    }
                    // Bedingt: tiefere Bedingung ersetzt flachere, Ties
                    // behalten die erste; `None` (unbedingt) bleibt kleben.
                    Some(f) => {
                        out.entry(dep.cpn.to_string())
                            .and_modify(|(d, v)| {
                                if v.is_some() && depth >= *d {
                                    *d = depth;
                                    *v = Some(f.clone());
                                }
                            })
                            .or_insert((depth, Some(f)));
                    }
                }
            }
            DepEntry::UseConditional {
                flag,
                negate,
                children,
            } => {
                let holds = active.contains(flag.as_str()) != *negate;
                if holds {
                    let inner = if *negate {
                        format!("!{flag}")
                    } else {
                        flag.to_string()
                    };
                    collect_via_flags(children, active, out, Some(inner), depth + 1);
                }
            }
            DepEntry::AllOf(children)
            | DepEntry::AnyOf(children)
            | DepEntry::ExactlyOneOf(children)
            | DepEntry::AtMostOneOf(children) => {
                collect_via_flags(children, active, out, cond.clone(), depth);
            }
        }
    }
}
