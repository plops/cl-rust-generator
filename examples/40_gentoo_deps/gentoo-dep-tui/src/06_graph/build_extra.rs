//! 06_graph/build_extra — Buildtime-Extra und SRC_URI-Groessen.
//!
//! Genau eine Zustaendigkeit: `DEPEND`/`BDEPEND`/`IDEPEND`-Schluss
//! minus Runtime plus Manifest-DIST-Summen.

use std::collections::{HashMap, HashSet, VecDeque};

use portage_atom::DepEntry;
use portage_metadata::SrcUriEntry;

use crate::mod_01_types::{EngineResult, SizeMetrics};
use crate::mod_05_repo_index::PkgVersion;

use super::{Engine, ResolvedGraph};

/// Download-Bytes: aktives `SRC_URI` × `Manifest`-Groessen.
///
/// Dateinamen deduplizieren: mehrere Spiegel-URLs derselben Datei zaehlen
/// nur einmal (Task 2.1).
pub(super) fn download_bytes(pkg: &PkgVersion, active: &HashSet<String>) -> u64 {
    let mut files = Vec::new();
    collect_dist_files(pkg.entry.metadata.src_uri.list(), active, &mut files);
    files.sort();
    files.dedup();
    pkg.manifest.sum_files(&files).0
}

/// DIST-Dateinamen aus `SRC_URI` (nur aktive Bedingungen).
fn collect_dist_files(entries: &[SrcUriEntry], active: &HashSet<String>, out: &mut Vec<String>) {
    for entry in entries {
        match entry {
            SrcUriEntry::Uri { filename, .. } => out.push(filename.clone()),
            SrcUriEntry::Renamed { target, .. } => out.push(target.clone()),
            SrcUriEntry::UseConditional {
                flag,
                negated,
                entries,
            } => {
                if active.contains(flag.as_str()) != *negated {
                    collect_dist_files(entries, active, out);
                }
            }
            SrcUriEntry::Group(entries) => collect_dist_files(entries, active, out),
        }
    }
}

/// Buildtime-Extra: `DEPEND`/`BDEPEND`/`IDEPEND`-Schluss minus Runtime.
///
/// Versionen/Slots werden exakt aufgeloest (`select_cpv`), kein stiller
/// Fallback auf die neueste Version (Task 2.1).
pub(super) fn resolve_build_extra(
    engine: &Engine<'_>,
    graph: &mut ResolvedGraph,
) -> EngineResult<()> {
    crate::mod_01_types::perf_count_build_extra();
    let runtime: HashSet<String> = graph.nodes.keys().cloned().collect();
    // cpn → (cpv, Binpkg-Groesse).
    let mut build: HashMap<String, (String, u64)> = HashMap::new();
    let mut queue: VecDeque<(String, String)> = VecDeque::new();

    for (cpn, node) in &graph.nodes {
        let Some(pkg) = engine.index.select_cpv(cpn, &node.cpv) else {
            continue;
        };
        let active = &node.use_flags;
        for list in [
            &pkg.entry.metadata.depend.list(),
            &pkg.entry.metadata.bdepend.list(),
            &pkg.entry.metadata.idepend.list(),
        ] {
            let evaluated = DepEntry::evaluate_use(list, active);
            let mut atoms = Vec::new();
            DepEntry::walk_atoms(&evaluated, &mut |d| atoms.push(d.clone()));
            for atom in atoms {
                if atom.blocker.is_some() {
                    continue;
                }
                if let Some(target) = engine.index.select_for_dep(&atom)
                    && !runtime.contains(&target.cpn)
                    && !build.contains_key(&target.cpn)
                {
                    build.insert(
                        target.cpn.clone(),
                        (target.cpv.clone(), target.binhost_size.unwrap_or(0)),
                    );
                    queue.push_back((target.cpn.clone(), target.cpv.clone()));
                }
            }
        }
    }
    // Eine Ebene Runtime-Transitiva der Build-Tools (Header etc.,
    // `RDEPEND` + `PDEPEND` wie im Runtime-Graphen).
    while let Some((cpn, cpv)) = queue.pop_front() {
        let Some(pkg) = engine.index.select_cpv(&cpn, &cpv) else {
            continue;
        };
        let active = engine.effective_use(pkg);
        let mut evaluated = DepEntry::evaluate_use(pkg.entry.metadata.rdepend.list(), &active);
        evaluated.extend(DepEntry::evaluate_use(
            pkg.entry.metadata.pdepend.list(),
            &active,
        ));
        let mut atoms = Vec::new();
        DepEntry::walk_atoms(&evaluated, &mut |d| atoms.push(d.clone()));
        for atom in atoms {
            if atom.blocker.is_some() {
                continue;
            }
            if let Some(target) = engine.index.select_for_dep(&atom)
                && !runtime.contains(&target.cpn)
                && !build.contains_key(&target.cpn)
            {
                build.insert(
                    target.cpn.clone(),
                    (target.cpv.clone(), target.binhost_size.unwrap_or(0)),
                );
                queue.push_back((target.cpn.clone(), target.cpv.clone()));
            }
        }
    }

    let mut extra = SizeMetrics::zero();
    let mut names: Vec<String> = build.keys().cloned().collect();
    names.sort();
    for cpn in &names {
        let (cpv, _) = &build[cpn];
        if let Some(pkg) = engine.index.select_cpv(cpn, cpv) {
            let active = engine.effective_use(pkg);
            extra.binpkg_bytes = extra
                .binpkg_bytes
                .saturating_add(pkg.binhost_size.unwrap_or(0) as i64);
            extra.download_bytes = extra
                .download_bytes
                .saturating_add(download_bytes(pkg, &active) as i64);
            graph.build_packages.insert(pkg.cpv.clone());
        }
    }
    graph.build_extra = extra;
    Ok(())
}
