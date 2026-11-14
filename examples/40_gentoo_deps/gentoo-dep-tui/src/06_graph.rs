//! 06_graph — `RDEPEND`-Resolver mit Reference-Counting und `||`-Alternativen.
//!
//! Genau eine Zustaendigkeit: Welt-Atome → aufgeloester Laufzeitgraph.
//! Strikte Trennung `RDEPEND`/`PDEPEND` (Runtime) vs. `DEPEND`/`BDEPEND`/
//! `IDEPEND` (Buildtime). Integer-freundlich: Schluessel sind `cat/pkg`-Strings,
//! Zaehler laufen ueber kompakte Maps (Mikrosekunden-Reachability).

use std::collections::{HashMap, HashSet};

use portage_atom::Dep;

use crate::mod_01_types::{AlternativeChoice, SizeMetrics, SkippedDep};
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

    /// Effektive USE-Flags: Profil-Baseline (gecacht) + explizite Overrides.
    ///
    /// Maske und Force behalten Vorrang vor Overrides: ein Override kann
    /// kein maskiertes Flag einschalten und kein erzwungenes (deklariertes)
    /// Flag abschalten.
    pub fn effective_use(&self, pkg: &PkgVersion) -> HashSet<String> {
        let base = pkg.base_use.get_or_init(|| {
            self.profile.effective_defaults(
                &pkg.iuse_defaults,
                &pkg.cpn,
                &pkg.version_str,
                &pkg.slot,
            )
        });
        let Some(explicit) = self.overrides.get(&pkg.cpn) else {
            return base.clone();
        };
        let mut active = base.clone();
        for (flag, on) in explicit {
            if *on {
                active.insert(flag.clone());
            } else {
                active.remove(flag);
            }
        }
        active.retain(|f| !self.profile.use_mask.contains(f));
        for flag in &self.profile.use_force {
            if pkg.iuse_defaults.contains_key(flag) {
                active.insert(flag.clone());
            }
        }
        active
    }
}

/// Ein aufgeloestes Laufzeitpaket.
#[derive(Debug, Clone, serde::Serialize)]
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
    /// Ob ein Binpkg existiert (`binhost_size.is_some()`).
    #[serde(default)]
    pub has_binpkg: bool,
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

    /// Exklusive Kosten je Paket: was faellt weg, wenn das Paket aus dem
    /// Graphen faellt (Erreichbarkeit ab `world` ohne diesen Knoten).
    pub fn node_costs(&self) -> HashMap<String, NodeCost> {
        let mut names: Vec<&String> = self.nodes.keys().collect();
        names.sort();
        let idx_of: HashMap<&str, usize> = names
            .iter()
            .enumerate()
            .map(|(i, s)| (s.as_str(), i))
            .collect();
        let n = names.len();
        let mut adj: Vec<Vec<usize>> = vec![Vec::new(); n];
        for (i, name) in names.iter().enumerate() {
            if let Some(edges) = self.out_edges.get(name.as_str()) {
                for dep in edges {
                    if dep.blocker.is_none()
                        && let Some(&j) = idx_of.get(dep.cpn.to_string().as_str())
                    {
                        adj[i].push(j);
                    }
                }
            }
        }
        let world_idx: Vec<usize> = self
            .world
            .iter()
            .filter_map(|w| idx_of.get(crate::mod_01_types::atom_cpn(w).as_str()).copied())
            .collect();
        let mut seen = vec![0u32; n];
        let mut gen_id = 0u32;
        let mut queue = std::collections::VecDeque::new();
        let mut out = HashMap::with_capacity(n);
        for cut in 0..n {
            gen_id += 1;
            queue.clear();
            for &w in &world_idx {
                if w != cut && seen[w] != gen_id {
                    seen[w] = gen_id;
                    queue.push_back(w);
                }
            }
            while let Some(u) = queue.pop_front() {
                for &v in &adj[u] {
                    if v != cut && seen[v] != gen_id {
                        seen[v] = gen_id;
                        queue.push_back(v);
                    }
                }
            }
            let mut pkgs = 0usize;
            let mut metrics = SizeMetrics::zero();
            for (i, name) in names.iter().enumerate() {
                if seen[i] != gen_id {
                    let node = &self.nodes[name.as_str()];
                    pkgs += 1;
                    metrics.binpkg_bytes = metrics.binpkg_bytes.saturating_add(node.binpkg as i64);
                    metrics.download_bytes =
                        metrics.download_bytes.saturating_add(node.download as i64);
                }
            }
            out.insert(
                names[cut].clone(),
                NodeCost {
                    exclusive_packages: pkgs,
                    exclusive_metrics: metrics,
                },
            );
        }
        out
    }
}

/// Exklusive Subgraph-Kosten eines Pakets.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, serde::Serialize)]
pub struct NodeCost {
    /// Anzahl Pakete (inkl. Paket selbst), die ohne dieses Paket unerreichbar sind.
    pub exclusive_packages: usize,
    /// Deren Groessen (inkl. Paket selbst).
    pub exclusive_metrics: SizeMetrics,
}

#[path = "06_graph/blame.rs"]
mod blame;
#[path = "06_graph/build_extra.rs"]
mod build_extra;
#[path = "06_graph/resolve.rs"]
mod resolve;
#[cfg(test)]
#[path = "06_graph/tests.rs"]
mod tests;

pub use resolve::{resolve, resolve_with_options};
