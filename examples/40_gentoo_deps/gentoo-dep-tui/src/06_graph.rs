//! 06_graph — `RDEPEND`-Resolver mit Reference-Counting und `||`-Alternativen.
//!
//! Genau eine Zustaendigkeit: Welt-Atome → aufgeloester Laufzeitgraph.
//! Strikte Trennung `RDEPEND`/`PDEPEND` (Runtime) vs. `DEPEND`/`BDEPEND`/
//! `IDEPEND` (Buildtime). Integer-freundlich: Schluessel sind `cat/pkg`-Strings,
//! Zaehler laufen ueber kompakte Maps (Mikrosekunden-Reachability).

use std::collections::{HashMap, HashSet, VecDeque};

use portage_atom::{Dep, DepEntry};
use portage_metadata::SrcUriEntry;

use crate::mod_01_types::{
    AlternativeChoice, BlameLink, EngineError, EngineResult, SizeMetrics, SkippedDep,
};
use crate::mod_02_profile::Profile;
use crate::mod_05_repo_index::{PkgVersion, RepoIndex};

/// Volle USE-Overrides: `cat/pkg` → komplette aktive Flag-Menge.
///
/// `None` pro Paket heisst: Profil-Defaults (`02_profile::apply_use_defaults`).
/// Explizite USE-Intents: `cat/pkg` → Flag → an/aus.
///
/// Nur explizit gesetzte Flags stehen hier (keine Defaults!): Abwesenheit
/// heisst "kein Intent", sodass die Kaskade frei entscheiden kann.
pub type UseOverrides = HashMap<String, HashMap<String, bool>>;

/// Engine-Bundle: Index + Profil + Overrides + `||`-Entscheidungen.
pub struct Engine<'a> {
    /// Paketindex.
    pub index: &'a RepoIndex,
    /// Profil- und make.conf-Konfiguration.
    pub profile: &'a Profile,
    /// Explizite USE-Sets pro Paket.
    pub overrides: UseOverrides,
    /// Gewaehlte `||`-Optionen: Gruppen-ID → Kandidaten-Index.
    pub or_decisions: HashMap<String, usize>,
}

impl<'a> Engine<'a> {
    /// Neues Bundle ohne Overrides.
    pub fn new(index: &'a RepoIndex, profile: &'a Profile) -> Self {
        Self {
            index,
            profile,
            overrides: HashMap::new(),
            or_decisions: HashMap::new(),
        }
    }

    /// Abzweig fuer Simulationen (gleicher Index + Profil, eigene Overrides).
    pub fn fork(&self) -> Engine<'_> {
        Engine {
            index: self.index,
            profile: self.profile,
            overrides: self.overrides.clone(),
            or_decisions: self.or_decisions.clone(),
        }
    }

    /// Effektive USE-Flags: Profil-Defaults, explizite Overrides obendrauf.
    pub fn effective_use(&self, pkg: &PkgVersion) -> HashSet<String> {
        let mut active = self
            .profile
            .apply_use_defaults(&pkg.iuse_defaults, &pkg.cpn);
        if let Some(explicit) = self.overrides.get(&pkg.cpn) {
            for (flag, on) in explicit {
                if *on {
                    active.insert(flag.clone());
                } else {
                    active.remove(flag);
                }
            }
        }
        active
    }
}

/// Ein aufgeloestes Laufzeitpaket.
#[derive(Debug, Clone)]
pub struct ResolvedNode {
    /// `cat/pkg`.
    pub cpn: String,
    /// `cat/pkg-version`.
    pub cpv: String,
    /// Slot.
    pub slot: String,
    /// Binhost-`SIZE` (0 ohne Binpkg).
    pub binpkg: u64,
    /// Download-Summe (`Manifest` × aktives `SRC_URI`).
    pub download: u64,
    /// Effektive USE-Flags bei Aufloesung.
    pub use_flags: HashSet<String>,
}

/// Aufgeloester Graph inkl. Diagnose.
#[derive(Debug, Clone, Default)]
pub struct ResolvedGraph {
    /// Laufzeitknoten: `cat/pkg` → Knoten.
    pub nodes: HashMap<String, ResolvedNode>,
    /// Reference-Counts ( distinct Eltern pro Knoten).
    pub refcounts: HashMap<String, usize>,
    /// Entdeckte `||`-Alternativen (Default: erste gueltige Option).
    pub alternatives: Vec<AlternativeChoice>,
    /// Rueckwaertskanten: Kind → `(Eltern, ausloesendes Flag?)`.
    pub reverse: HashMap<String, Vec<(String, Option<String>)>>,
    /// Welt-Atome (fuer Blame-Startpunkte).
    pub world: Vec<String>,
    /// Buildtime-Extra (BDEPEND-nur Pakete, nicht im Container-Runtime).
    pub build_extra: SizeMetrics,
    /// Buildtime-Pakete (`cat/pkg-version`).
    pub build_packages: HashSet<String>,
    /// Uebersprungene Abhaengigkeiten (Warnungen, kein Abbruch).
    pub skipped: Vec<SkippedDep>,
    /// Aufloesungs-Kanten: Eltern-CPN → traversierte (selektierte) Atome.
    ///
    /// Nur tatsächlich gewählte `||`-Zweige sind enthalten. Blocker-Atome
    /// sind enthalten (fuer invertierte Constraints in `07_cascade`), legen
    /// aber keine Graph-Knoten an.
    pub out_edges: HashMap<String, Vec<Dep>>,
}

impl ResolvedGraph {
    /// Gesamtgroessen des Runtime-Graphen.
    pub fn totals(&self) -> SizeMetrics {
        let mut total = SizeMetrics::zero();
        for node in self.nodes.values() {
            total.binpkg_bytes = total.binpkg_bytes.saturating_add(node.binpkg as i64);
            total.download_bytes = total.download_bytes.saturating_add(node.download as i64);
        }
        total
    }

    /// Alle Blame-Ketten von `world` bis `target` (Tiefensuche).
    ///
    /// Pfad-Konvention: `path[0]` ist das Ziel, das Ende ein Welt-Paket.
    /// Glied `i` bedeutet: `path[i]` wird von `path[i+1]` gebraucht.
    pub fn blame(&self, target: &str) -> Vec<Vec<BlameLink>> {
        let mut chains = Vec::new();
        let mut path = vec![target.to_string()];
        self.blame_dfs(target, &mut path, &mut chains, 0);
        chains
    }

    fn blame_dfs(
        &self,
        node: &str,
        path: &mut Vec<String>,
        chains: &mut Vec<Vec<BlameLink>>,
        depth: usize,
    ) {
        if depth > 32 || chains.len() >= 16 {
            return;
        }
        match self.reverse.get(node) {
            None => {
                // Wurzel erreicht (world-Eintrag): Kette schliessen.
                let mut links = Vec::new();
                for w in path.windows(2) {
                    let via = self
                        .reverse
                        .get(&w[0])
                        .and_then(|ps| ps.iter().find(|(p, _)| *p == w[1]))
                        .and_then(|(_, v)| v.clone());
                    links.push(BlameLink {
                        package: w[0].clone(),
                        required_by: w[1].clone(),
                        via_flag: via.filter(|v| !v.is_empty()),
                    });
                }
                chains.push(links);
            }
            Some(parents) => {
                for (parent, _) in parents {
                    if path.contains(parent) {
                        continue; // Zyklenfest.
                    }
                    path.push(parent.clone());
                    self.blame_dfs(parent, path, chains, depth + 1);
                    path.pop();
                }
            }
        }
    }
}

/// Welt-Atome → `ResolvedGraph` (ohne Kaskaden, siehe `07_cascade`).
pub fn resolve(engine: &Engine<'_>, world: &[String]) -> EngineResult<ResolvedGraph> {
    let mut graph = ResolvedGraph {
        world: world.to_vec(),
        ..ResolvedGraph::default()
    };
    let mut queue: VecDeque<(String, String, Option<String>)> = VecDeque::new();

    for atom in world {
        let dep = Dep::parse(atom).map_err(|e| EngineError::Parse {
            context: atom.clone(),
            message: e.to_string(),
        })?;
        if dep.blocker.is_some() {
            continue;
        }
        match engine.index.select_for_dep(&dep) {
            Some(pkg) => queue.push_back((pkg.cpn.clone(), atom.clone(), None)),
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

    let mut or_counter: HashMap<String, usize> = HashMap::new();
    while let Some((cpn, required_by, _via)) = queue.pop_front() {
        if graph.nodes.contains_key(&cpn) {
            continue;
        }
        // Versionswahl: exakter CPV aus world bevorzugen, sonst Top-Version.
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
            },
        );

        // Via-Flags aus dem *rohen* Baum einsammeln (evaluate_use loescht sie).
        let mut via_map: HashMap<String, String> = HashMap::new();
        collect_via_flags(pkg.entry.metadata.rdepend.list(), &active, &mut via_map);

        let evaluated = DepEntry::evaluate_use(pkg.entry.metadata.rdepend.list(), &active);
        descend_runtime(
            engine,
            &evaluated,
            &cpn,
            &mut graph,
            &mut queue,
            &via_map,
            &mut or_counter,
        )?;
    }

    // Reference-Counts aus Rueckwaertskanten.
    for (child, parents) in &graph.reverse {
        let mut distinct = HashSet::new();
        for (parent, _) in parents {
            distinct.insert(parent.clone());
        }
        graph.refcounts.insert(child.clone(), distinct.len());
    }

    resolve_build_extra(engine, &mut graph)?;
    Ok(graph)
}

/// Zielversion fuer einen Knoten (world-Atom oder Top-Version).
fn resolve_target<'b>(
    engine: &'b Engine<'b>,
    cpn: &str,
    required_by: &str,
) -> EngineResult<&'b PkgVersion> {
    if let Ok(dep) = Dep::parse(required_by)
        && dep.cpn.to_string() == cpn
        && dep.version.is_some()
        && let Some(pkg) = engine.index.select_for_dep(&dep)
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

/// Evaluierte `RDEPEND`-Eintraege absteigen (rekursiv, zyklenfest via `nodes`).
#[allow(clippy::too_many_arguments)]
fn descend_runtime(
    engine: &Engine<'_>,
    entries: &[DepEntry],
    owner: &str,
    graph: &mut ResolvedGraph,
    queue: &mut VecDeque<(String, String, Option<String>)>,
    via_map: &HashMap<String, String>,
    or_counter: &mut HashMap<String, usize>,
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
                let via = via_map.get(&pkg.cpn).cloned();
                push_reverse(graph, &pkg.cpn, owner, via);
                if !graph.nodes.contains_key(&pkg.cpn) {
                    queue.push_back((pkg.cpn.clone(), pkg.cpn.clone(), None));
                }
            }
            DepEntry::AllOf(children) => {
                descend_runtime(engine, children, owner, graph, queue, via_map, or_counter)?;
            }
            DepEntry::AnyOf(children) => {
                descend_any_of(engine, children, owner, graph, queue, via_map, or_counter)?;
            }
            DepEntry::ExactlyOneOf(children) | DepEntry::AtMostOneOf(children) => {
                // Aufloesung wie `||` (Validierung in `07_cascade`); keine
                // Alternative im engeren Sinne (`||`-Task T06).
                pick_first(engine, children, owner, graph, queue, via_map)?;
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
    queue: &mut VecDeque<(String, String, Option<String>)>,
    via_map: &HashMap<String, String>,
    or_counter: &mut HashMap<String, usize>,
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
    let n = or_counter.entry(owner.to_string()).or_insert(0);
    *n += 1;
    let group_id = format!("{owner}||{}", candidates.join(","));
    let want = engine.or_decisions.get(&group_id).copied().unwrap_or(0);
    let (sel_idx, sel_cpn, sel_atom) = selectable
        .iter()
        .find(|(i, _, _)| *i == want)
        .or(selectable.first())
        .cloned()
        .unwrap();
    push_edge(graph, owner, &sel_atom);
    graph.alternatives.push(AlternativeChoice {
        group_id,
        owner: owner.to_string(),
        candidates,
        selected: sel_idx,
    });
    let via = via_map.get(&sel_cpn).cloned();
    push_reverse(graph, &sel_cpn, owner, via);
    if !graph.nodes.contains_key(&sel_cpn) {
        queue.push_back((sel_cpn, owner.to_string(), None));
    }
    Ok(())
}

/// Erste gueltige Option ohne Alternativen-Eintrag.
fn pick_first(
    engine: &Engine<'_>,
    children: &[DepEntry],
    owner: &str,
    graph: &mut ResolvedGraph,
    queue: &mut VecDeque<(String, String, Option<String>)>,
    via_map: &HashMap<String, String>,
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
            let via = via_map.get(&pkg.cpn).cloned();
            push_reverse(graph, &pkg.cpn, owner, via);
            if !graph.nodes.contains_key(&pkg.cpn) {
                queue.push_back((pkg.cpn.clone(), owner.to_string(), None));
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
fn collect_via_flags(
    entries: &[DepEntry],
    active: &HashSet<String>,
    out: &mut HashMap<String, String>,
) {
    for entry in entries {
        match entry {
            DepEntry::Atom(dep) => {
                out.entry(dep.cpn.to_string()).or_default();
            }
            DepEntry::UseConditional {
                flag,
                negate,
                children,
            } => {
                let holds = active.contains(flag.as_str()) != *negate;
                if holds {
                    let mut sub = HashMap::new();
                    collect_via_flags(children, active, &mut sub);
                    for (k, _) in sub {
                        out.entry(k).or_insert_with(|| flag.to_string());
                    }
                }
            }
            DepEntry::AllOf(children)
            | DepEntry::AnyOf(children)
            | DepEntry::ExactlyOneOf(children)
            | DepEntry::AtMostOneOf(children) => {
                collect_via_flags(children, active, out);
            }
        }
    }
}

/// Download-Bytes: aktives `SRC_URI` × `Manifest`-Groessen.
fn download_bytes(pkg: &PkgVersion, active: &HashSet<String>) -> u64 {
    let mut files = Vec::new();
    collect_dist_files(pkg.entry.metadata.src_uri.list(), active, &mut files);
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
fn resolve_build_extra(engine: &Engine<'_>, graph: &mut ResolvedGraph) -> EngineResult<()> {
    let runtime: HashSet<String> = graph.nodes.keys().cloned().collect();
    let mut build: HashMap<String, u64> = HashMap::new();
    let mut queue: VecDeque<String> = VecDeque::new();

    for (cpn, node) in &graph.nodes {
        let Some(pkg) = engine.index.select(cpn) else {
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
                    build.insert(target.cpn.clone(), target.binhost_size.unwrap_or(0));
                    queue.push_back(target.cpn.clone());
                }
            }
        }
    }
    // Eine Ebene Runtime-Transitiva der Build-Tools (Header etc.).
    while let Some(cpn) = queue.pop_front() {
        let Some(pkg) = engine.index.select(&cpn) else {
            continue;
        };
        let active = engine.effective_use(pkg);
        let evaluated = DepEntry::evaluate_use(pkg.entry.metadata.rdepend.list(), &active);
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
                build.insert(target.cpn.clone(), target.binhost_size.unwrap_or(0));
                queue.push_back(target.cpn.clone());
            }
        }
    }

    let mut extra = SizeMetrics::zero();
    let mut names: Vec<String> = build.keys().cloned().collect();
    names.sort();
    for cpn in &names {
        if let Some(pkg) = engine.index.select(cpn) {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mod_02_profile::Profile;
    use crate::mod_03_manifest::parse_manifest;
    use portage_metadata::CacheEntry;
    use std::collections::HashMap;

    #[allow(clippy::too_many_arguments)]
    fn pkg(
        cpn: &str,
        version: &str,
        keywords: &str,
        iuse: &str,
        rdepend: &str,
        bdepend: &str,
        src_uri: &str,
        binpkg: Option<u64>,
    ) -> PkgVersion {
        let text = format!(
            "EAPI=8\nDESCRIPTION=t\nSLOT=0\nKEYWORDS={keywords}\nIUSE={iuse}\n\
             RDEPEND={rdepend}\nBDEPEND={bdepend}\nSRC_URI={src_uri}\n"
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
            // Passt zu `http://x/a.tar.gz` in den Test-`SRC_URI`s oben.
            manifest: parse_manifest("DIST a.tar.gz 1000\n"),
            binhost_size: binpkg,
        }
    }

    fn test_index() -> RepoIndex {
        // Diamond: app -> {libb, libc}, beide -> libshared.
        // app[ssl?] -> openssl ; app hat BDEPEND auf build-tool.
        let versions = vec![
            pkg(
                "app-misc/app",
                "1.0",
                "amd64",
                "ssl",
                "dev-libs/libb dev-libs/libc ssl? ( dev-libs/openssl )",
                "dev-build/tool",
                "http://x/a.tar.gz",
                Some(100),
            ),
            pkg(
                "dev-libs/libb",
                "1.0",
                "amd64",
                "",
                "dev-libs/libshared",
                "",
                "http://x/a.tar.gz",
                Some(50),
            ),
            pkg(
                "dev-libs/libc",
                "1.0",
                "amd64",
                "",
                "dev-libs/libshared",
                "",
                "http://x/a.tar.gz",
                Some(60),
            ),
            pkg(
                "dev-libs/libshared",
                "1.0",
                "amd64",
                "",
                "",
                "",
                "http://x/a.tar.gz",
                Some(70),
            ),
            pkg(
                "dev-libs/openssl",
                "3.0",
                "amd64",
                "",
                "",
                "",
                "http://x/a.tar.gz",
                Some(80),
            ),
            pkg(
                "dev-build/tool",
                "1.0",
                "amd64",
                "",
                "",
                "",
                "http://x/a.tar.gz",
                Some(900),
            ),
        ];
        let mut map: HashMap<String, Vec<PkgVersion>> = HashMap::new();
        for p in versions {
            map.entry(p.cpn.clone()).or_default().push(p);
        }
        RepoIndex {
            versions: map,
            cache_files: 6,
            load_ms: 0,
        }
    }

    #[test]
    fn shared_dep_counted_once_with_refcount_two() {
        let index = test_index();
        let profile = Profile::test_default();
        let engine = Engine::new(&index, &profile);
        let graph = resolve(&engine, &["app-misc/app".into()]).unwrap();
        // app, libb, libc, libshared (ssl aus → kein openssl).
        assert_eq!(graph.nodes.len(), 4);
        assert_eq!(graph.refcounts.get("dev-libs/libshared"), Some(&2));
        let total = graph.totals();
        assert_eq!(total.binpkg_bytes, 100 + 50 + 60 + 70);
        // Build-Tool landet im Extra, nicht im Runtime-Total.
        assert_eq!(graph.build_extra.binpkg_bytes, 900);
        assert!(!graph.nodes.contains_key("dev-build/tool"));
    }

    #[test]
    fn use_toggle_pulls_optional_dep() {
        let index = test_index();
        let profile = Profile::test_default();
        let mut engine = Engine::new(&index, &profile);
        engine.overrides.insert(
            "app-misc/app".into(),
            [("ssl".to_string(), true)].into_iter().collect(),
        );
        let graph = resolve(&engine, &["app-misc/app".into()]).unwrap();
        assert!(graph.nodes.contains_key("dev-libs/openssl"));
        assert_eq!(graph.refcounts.get("dev-libs/openssl"), Some(&1));
    }

    #[test]
    fn cycle_terminates() {
        let versions = vec![
            pkg(
                "cat/a",
                "1.0",
                "amd64",
                "",
                "cat/b",
                "",
                "http://x/a.tar.gz",
                Some(10),
            ),
            pkg(
                "cat/b",
                "1.0",
                "amd64",
                "",
                "cat/a",
                "",
                "http://x/a.tar.gz",
                Some(20),
            ),
        ];
        let mut map: HashMap<String, Vec<PkgVersion>> = HashMap::new();
        for p in versions {
            map.entry(p.cpn.clone()).or_default().push(p);
        }
        let index = RepoIndex {
            versions: map,
            cache_files: 2,
            load_ms: 0,
        };
        let profile = Profile::test_default();
        let engine = Engine::new(&index, &profile);
        let graph = resolve(&engine, &["cat/a".into()]).unwrap();
        assert_eq!(graph.nodes.len(), 2);
    }

    #[test]
    fn any_of_records_alternative() {
        let versions = vec![
            pkg(
                "app-misc/app",
                "1.0",
                "amd64",
                "",
                "|| ( net-misc/curl net-misc/wget )",
                "",
                "http://x/a.tar.gz",
                Some(100),
            ),
            pkg(
                "net-misc/curl",
                "1.0",
                "amd64",
                "",
                "",
                "",
                "http://x/a.tar.gz",
                Some(30),
            ),
            pkg(
                "net-misc/wget",
                "1.0",
                "amd64",
                "",
                "",
                "",
                "http://x/a.tar.gz",
                Some(40),
            ),
        ];
        let mut map: HashMap<String, Vec<PkgVersion>> = HashMap::new();
        for p in versions {
            map.entry(p.cpn.clone()).or_default().push(p);
        }
        let index = RepoIndex {
            versions: map,
            cache_files: 3,
            load_ms: 0,
        };
        let profile = Profile::test_default();
        let engine = Engine::new(&index, &profile);
        let graph = resolve(&engine, &["app-misc/app".into()]).unwrap();
        assert!(graph.nodes.contains_key("net-misc/curl"));
        assert_eq!(graph.alternatives.len(), 1);
        assert_eq!(graph.alternatives[0].candidates.len(), 2);
    }
}
