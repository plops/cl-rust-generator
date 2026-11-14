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

    /// Effektive USE-Flags: Profil-Defaults, explizite Overrides obendrauf.
    pub fn effective_use(&self, pkg: &PkgVersion) -> HashSet<String> {
        let mut active = self.profile.effective_defaults(
            &pkg.iuse_defaults,
            &pkg.cpn,
            &pkg.version_str,
            &pkg.slot,
        );
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
