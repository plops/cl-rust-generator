//! 12_tui_state — TUI-AppState und alle State-Uebergaenge.
//!
//! Genau eine Zustaendigkeit: Tabs, Fokus, Selektion, Toggle-/Quick-Switch-
//! Logik (inkl. Live-Neuberechnung ueber `06` + `07` + `08`). Kein Rendering
//! (siehe `13_tui_view`), kein Terminal-Lifecycle (siehe `14_tui_runner`).

use std::collections::{HashMap, HashSet};

use ratatui::widgets::{ListState, TableState};

use crate::mod_01_types::{BigLever, ConflictInfo, EngineResult, FlagDelta, atom_cpn};
use crate::mod_02_profile::{Profile, UseDescriptions};
use crate::mod_05_repo_index::RepoIndex;
use crate::mod_06_graph::{Engine, NodeCost, ResolvedGraph, UseOverrides};
use crate::mod_07_cascade::CascadeOutcome;
use crate::mod_07_cascade::resolve_with_cascade;
use crate::mod_08_delta::{
    big_levers, big_levers_with_base, package_deltas, package_deltas_with_base,
};
use crate::mod_08_delta::CutPlan;

/// Haupttab.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Tab {
    /// `[1]` Pakete, USE-Flags, Blame.
    #[default]
    Packages,
    /// `[2]` Globale Big Levers.
    BigLevers,
}

/// Fokus-Pane.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Pane {
    /// Links: Pakete / `||`-Entscheidungen.
    #[default]
    Left,
    /// Mitte: USE-Flags / Kandidaten.
    Middle,
    /// Rechts: Blame / Pros-Cons (nur Anzeige, kein Fokus-Cycling noetig).
    Right,
}

/// Sortiermodus der linken Liste (`o`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum LeftSort {
    /// World → `||` → Transitive (wie bisher).
    #[default]
    Default,
    /// Absteigend nach exklusiver Paketanzahl.
    ExclusivePackages,
    /// Absteigend nach exklusiver Groesse.
    ExclusiveSize,
}

/// Eintrag der linken Pane (einheitlich navigierbar).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LeftEntry {
    /// Welt-Paket.
    WorldPkg(String),
    /// Transitive `||`-Entscheidung (Gruppen-ID).
    OrDecision(String),
    /// Transitives Paket.
    Transitive(String),
}

/// Geoeffnetes Modal.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Modal {
    /// Konflikt-Quick-Switch: Konflikt-Index + Options-Selektion.
    Conflict {
        /// Index in `conflicts`.
        conflict: usize,
        /// Selektierte Option.
        selected: usize,
    },
    /// Simulierter Konflikt eines Toggles (noch nicht in `conflicts`).
    SimulatedConflict {
        /// Konfliktinfo der Simulation.
        conflict: ConflictInfo,
        /// Selektierte Option.
        selected: usize,
    },
    /// Hilfe-Overlay.
    Help,
}

/// Applikationszustand (Engine-Bundle + UI-Selektion).
pub struct AppState<'a> {
    /// Resolver-Bundle (Overrides werden hier mutiert).
    pub engine: Engine<'a>,
    /// Welt-Atome.
    pub world: Vec<String>,
    /// Aktueller Graph nach Kaskaden.
    pub graph: ResolvedGraph,
    /// Aktuelle Konflikte.
    pub conflicts: Vec<ConflictInfo>,
    /// Letzte Kaskaden-Baseline.
    pub last_outcome: Option<CascadeOutcome>,
    /// Paket-Ziele in der rechten Pane (Blame / Bewirkt / Geteilte Deps).
    pub right_targets: Vec<String>,
    /// Aktuell ausgewaehltes Paket-Ziel in der rechten Pane.
    pub right_target_selected: Option<usize>,
    /// Cache fuer Cut-Analysen (`target_cpn -> CutPlan`), geleert bei `refresh()`.
    pub cut_cache: HashMap<String, CutPlan>,
    /// Cut-Analyse fuer `right_target_selected` ausstehend.
    pub cut_loading: bool,
    /// Terminal-Hoehe fuer `PageUp`/`PageDown` (Default 24).
    pub viewport_height: u16,
    /// Persistenter Scroll-Offset der mittleren Pane (Viewport-Erhalt).
    pub middle_scroll: std::cell::Cell<u16>,
    /// Exklusive Kosten je Paket (einmal pro `refresh()`).
    pub node_costs: HashMap<String, NodeCost>,
    /// Sortiermodus links.
    pub left_sort: LeftSort,
    /// Cut-Analyse zum selektierten Paket (Taste `x`).
    pub cut_plan: Option<CutPlan>,
    /// Undo-Stapel fuer Overrides (max. 32).
    pub undo_stack: Vec<UseOverrides>,
    /// Offline-USE-Flag-Beschreibungen.
    pub use_descriptions: UseDescriptions,
    /// Aktiver Tab / Fokus.
    pub tab: Tab,
    /// Aktive Pane.
    pub pane: Pane,
    /// Linke Eintraege (berechnet).
    pub left: Vec<LeftEntry>,
    /// Selektion links.
    pub left_selected: usize,
    /// Deltas des selektierten Pakets (Mitte).
    pub flag_deltas: Vec<FlagDelta>,
    /// Selektion Mitte (Flag oder Kandidat).
    pub middle_selected: usize,
    /// Big Levers (Tab 2).
    pub levers: Vec<BigLever>,
    /// Selektion Big-Levers-Tabelle.
    pub lever_selected: usize,
    /// Suchfilter (`/`-Modus).
    pub filter: String,
    /// Ob Filter-Eingabe aktiv ist.
    pub filtering: bool,
    /// Geoeffnetes Modal.
    pub modal: Option<Modal>,
    /// Ungespeicherte Aenderungen.
    pub dirty: bool,
    /// Statuszeile.
    pub status: String,
    /// Scroll-Zustand links.
    pub left_list_state: ListState,
    /// Scroll-Zustand Big-Levers-Tabelle.
    pub lever_table_state: TableState,
    /// Vertikaler Scroll-Offset rechts.
    pub right_scroll: u16,
    /// `levers` veraltet (lazy Neuberechnung erst bei Tab-2-Besuch).
    pub levers_dirty: bool,
    /// Deltas werden gerade nachgeladen.
    pub deltas_loading: bool,
    /// Delta-Cache: `(cpn, overrides-Fingerprint)` → Deltas.
    pub delta_cache: HashMap<(String, String), Vec<FlagDelta>>,
}

impl<'a> AppState<'a> {
    /// Neuer State: Engine aufbauen, einmal aufloesen.
    pub fn new(index: &'a RepoIndex, profile: &'a Profile, world: Vec<String>) -> EngineResult<Self> {
        let engine = Engine::new(index, profile);
        let mut state = Self {
            engine,
            world,
            graph: ResolvedGraph::default(),
            conflicts: Vec::new(),
            last_outcome: None,
            right_targets: Vec::new(),
            right_target_selected: None,
            cut_cache: HashMap::new(),
            cut_loading: false,
            viewport_height: 24,
            middle_scroll: std::cell::Cell::new(0),
            node_costs: HashMap::new(),
            left_sort: LeftSort::Default,
            cut_plan: None,
            undo_stack: Vec::new(),
            use_descriptions: UseDescriptions::load(&profile.profiles_dir),
            tab: Tab::Packages,
            pane: Pane::Left,
            left: Vec::new(),
            left_selected: 0,
            flag_deltas: Vec::new(),
            middle_selected: 0,
            levers: Vec::new(),
            lever_selected: 0,
            filter: String::new(),
            filtering: false,
            modal: None,
            dirty: false,
            status: String::new(),
            left_list_state: ListState::default(),
            lever_table_state: TableState::default(),
            right_scroll: 0,
            levers_dirty: true,
            deltas_loading: false,
            delta_cache: HashMap::new(),
        };
        state.refresh()?;
        Ok(state)
    }

    /// Overrides sichern (max. 32 Eintraege).
    pub fn push_undo(&mut self) {
        self.undo_stack.push(self.engine.overrides.clone());
        if self.undo_stack.len() > 32 {
            self.undo_stack.remove(0);
        }
    }

    /// Letzten Stand zurueckrollen.
    pub fn pop_undo(&mut self) -> EngineResult<bool> {
        let Some(prev) = self.undo_stack.pop() else {
            return Ok(false);
        };
        self.engine.overrides = prev;
        self.dirty = true;
        self.refresh()?;
        Ok(true)
    }

    /// Neu aufloesen + Listen/Deltas aktualisieren (Levers lazy).
    pub fn refresh(&mut self) -> EngineResult<()> {
        let prev_entry = self.left.get(self.left_selected).cloned();
        let outcome = resolve_with_cascade(&mut self.engine, &self.world)?;
        let levers = if self.tab == Tab::BigLevers {
            Some(big_levers_with_base(&self.engine, &self.world, &outcome).unwrap_or_default())
        } else {
            None
        };
        self.last_outcome = Some(outcome.clone());
        self.graph = outcome.graph;
        self.conflicts = outcome.conflicts;
        self.node_costs = self.graph.node_costs();
        self.cut_plan = None;
        self.cut_cache.clear();
        self.cut_loading = false;
        self.delta_cache.clear();
        if self.status.starts_with("package.use geschrieben:") {
            self.status.clear();
        }
        self.rebuild_left_preserving(prev_entry.as_ref());
        self.rebuild_middle()?;
        self.rebuild_right_targets();
        match levers {
            Some(levers) => {
                self.levers = levers;
                self.levers_dirty = false;
                self.sync_table_states();
            }
            None => self.levers_dirty = true,
        }
        Ok(())
    }

    /// Big Levers bei Bedarf (nach-)berechnen (lazy beim Tab-2-Besuch).
    pub(super) fn ensure_levers(&mut self) -> EngineResult<()> {
        if self.levers_dirty {
            self.levers = match &self.last_outcome {
                Some(base) => {
                    big_levers_with_base(&self.engine, &self.world, base).unwrap_or_default()
                }
                None => big_levers(&self.engine, &self.world).unwrap_or_default(),
            };
            self.levers_dirty = false;
            self.sync_table_states();
        }
        Ok(())
    }

    /// Listen-/Tabellen-States mit Selektion synchronisieren.
    fn sync_table_states(&mut self) {
        self.left_list_state.select(Some(self.left_selected));
        self.lever_table_state.select(Some(self.lever_selected));
    }

    /// Linke Liste aufbauen (Sortiermodus beachten, kein Resolve!).
    fn rebuild_left(&mut self) {
        let prev = self.left.get(self.left_selected).cloned();
        self.rebuild_left_preserving(prev.as_ref());
    }

    fn rebuild_left_preserving(&mut self, prev_entry: Option<&LeftEntry>) {
        let mut world_entries: Vec<LeftEntry> = Vec::new();
        let mut seen = HashSet::new();
        for atom in &self.world {
            let cpn = atom_cpn(atom);
            if self.graph.nodes.contains_key(&cpn) && seen.insert(cpn.clone()) {
                world_entries.push(LeftEntry::WorldPkg(cpn));
            }
        }
        let mut or_entries: Vec<LeftEntry> = self
            .graph
            .alternatives
            .iter()
            .map(|a| LeftEntry::OrDecision(a.group_id.clone()))
            .collect();
        let mut transitive: Vec<String> = self
            .graph
            .nodes
            .keys()
            .filter(|cpn| !seen.contains(*cpn))
            .cloned()
            .collect();
        match self.left_sort {
            LeftSort::Default => {
                transitive.sort_by_key(|cpn| {
                    std::cmp::Reverse(self.graph.nodes.get(cpn).map(|n| n.binpkg).unwrap_or(0))
                });
            }
            LeftSort::ExclusivePackages => {
                transitive.sort_by(|a, b| {
                    let ca = self.node_costs.get(a);
                    let cb = self.node_costs.get(b);
                    cb.map(|c| c.exclusive_packages)
                        .cmp(&ca.map(|c| c.exclusive_packages))
                        .then_with(|| {
                            cb.map(|c| c.exclusive_metrics.download_bytes).cmp(
                                &ca.map(|c| c.exclusive_metrics.download_bytes),
                            )
                        })
                        .then_with(|| a.cmp(b))
                });
                world_entries.sort_by(|a, b| {
                    let ca = self.node_costs.get(match a {
                        LeftEntry::WorldPkg(c) => c.as_str(),
                        _ => "",
                    });
                    let cb = self.node_costs.get(match b {
                        LeftEntry::WorldPkg(c) => c.as_str(),
                        _ => "",
                    });
                    cb.map(|c| c.exclusive_packages)
                        .cmp(&ca.map(|c| c.exclusive_packages))
                        .then_with(|| {
                            cb.map(|c| c.exclusive_metrics.download_bytes).cmp(
                                &ca.map(|c| c.exclusive_metrics.download_bytes),
                            )
                        })
                        .then_with(|| left_label(a).cmp(&left_label(b)))
                });
            }
            LeftSort::ExclusiveSize => {
                let has_bin = self.engine.index.has_any_binhost();
                let key = |cpn: &str| -> (i64, String) {
                    match self.node_costs.get(cpn) {
                        Some(c) if has_bin => (
                            c.exclusive_metrics.binpkg_bytes,
                            cpn.to_string(),
                        ),
                        Some(c) => (c.exclusive_metrics.download_bytes, cpn.to_string()),
                        None => (0, cpn.to_string()),
                    }
                };
                transitive.sort_by_key(|a| std::cmp::Reverse(key(a)));
                world_entries.sort_by(|a, b| {
                    let ka = key(match a {
                        LeftEntry::WorldPkg(c) => c.as_str(),
                        _ => "",
                    });
                    let kb = key(match b {
                        LeftEntry::WorldPkg(c) => c.as_str(),
                        _ => "",
                    });
                    kb.cmp(&ka)
                });
            }
        }
        let mut left = Vec::new();
        left.extend(world_entries);
        left.append(&mut or_entries);
        for cpn in transitive {
            left.push(LeftEntry::Transitive(cpn));
        }
        if !self.filter.is_empty() {
            let f = self.filter.to_lowercase();
            left.retain(|e| left_label(e).to_lowercase().contains(&f));
        }
        self.left = left;
        if let Some(prev) = prev_entry
            && let Some(pos) = self.left.iter().position(|e| e == prev)
        {
            self.left_selected = pos;
        } else {
            self.left_selected = self.left_selected.min(self.left.len().saturating_sub(1));
        }
        self.left_list_state.select(Some(self.left_selected));
    }

    /// Sortiermodus wechseln (`o`): zyklisch, kein Resolve.
    pub fn cycle_left_sort(&mut self) {
        self.left_sort = match self.left_sort {
            LeftSort::Default => LeftSort::ExclusivePackages,
            LeftSort::ExclusivePackages => LeftSort::ExclusiveSize,
            LeftSort::ExclusiveSize => LeftSort::Default,
        };
        self.rebuild_left();
    }

    /// Nur Filter anwenden: kein Resolve, keine Deltas, kein Big-Levers.
    ///
    /// Der Filter aendert die sichtbare Menge: Restore per Index (Clamp),
    /// nicht per Paket — `refresh()`/Sortierung restaurieren per Paket.
    pub fn apply_filter_only(&mut self) {
        let prev = self.selected_package();
        self.rebuild_left_preserving(None);
        if self.selected_package() != prev {
            self.select_left_fast(self.left_selected);
            return;
        }
        self.middle_selected = self
            .middle_selected
            .min(self.middle_len().saturating_sub(1));
    }

    /// Mitte neu berechnen (Flags des Pakets oder `||`-Kandidaten).
    pub(super) fn rebuild_middle(&mut self) -> EngineResult<()> {
        let prev_pkg = self.selected_package();
        let prev_flag = self
            .flag_deltas
            .get(self.middle_selected)
            .map(|d| d.flag.clone());
        self.flag_deltas.clear();
        match self.left.get(self.left_selected).cloned() {
            Some(LeftEntry::WorldPkg(cpn)) | Some(LeftEntry::Transitive(cpn)) => {
                let key = (cpn.clone(), overrides_fingerprint(&self.engine));
                if let Some(cached) = self.delta_cache.get(&key) {
                    self.flag_deltas = cached.clone();
                    self.deltas_loading = false;
                    crate::mod_00_perf::perf_count_cache_hit();
                } else {
                    self.deltas_loading = true;
                    crate::mod_00_perf::perf_count_cache_miss();
                    let deltas = match &self.last_outcome {
                        Some(base) => {
                            package_deltas_with_base(&self.engine, &self.world, &cpn, base)?
                        }
                        None => package_deltas(&self.engine, &self.world, &cpn)?,
                    };
                    self.delta_cache.insert(key, deltas.clone());
                    self.flag_deltas = deltas;
                    self.deltas_loading = false;
                }
            }
            _ => {
                self.deltas_loading = false;
            }
        }
        if self.selected_package() == prev_pkg
            && let Some(f) = prev_flag
            && let Some(idx) = self.flag_deltas.iter().position(|d| d.flag == f)
        {
            self.middle_selected = idx;
        } else {
            self.middle_selected = 0;
        }
        if self.selected_package() != prev_pkg {
            self.right_scroll = 0;
            self.middle_scroll.set(0);
        }
        self.rebuild_right_targets();
        Ok(())
    }

    /// Sammelt alle in der rechten Pane sichtbaren Zielpakete in Anzeigereihenfolge.
    pub fn rebuild_right_targets(&mut self) {
        self.right_targets.clear();
        self.right_target_selected = None;
        self.cut_loading = false;
        let mut seen = HashSet::new();
        for chain in self.selected_blame().iter().take(4) {
            for link in chain {
                if seen.insert(link.required_by.clone()) {
                    self.right_targets.push(link.required_by.clone());
                }
            }
        }
        if let Some(delta) = self.flag_deltas.get(self.middle_selected) {
            for p in delta.added_packages.iter().take(8) {
                if seen.insert(p.clone()) {
                    self.right_targets.push(p.clone());
                }
            }
            for p in delta.removed_packages.iter().take(8) {
                if seen.insert(p.clone()) {
                    self.right_targets.push(p.clone());
                }
            }
            if let Some(pkg) = self.selected_package() {
                for (dep_cpn, others) in
                    crate::mod_08_delta::shared_flag_deps(&self.engine, &self.graph, &pkg, &delta.flag)
                        .into_iter()
                        .take(8)
                {
                    if seen.insert(dep_cpn.clone()) {
                        self.right_targets.push(dep_cpn);
                    }
                    for o in others.into_iter().take(4) {
                        if seen.insert(o.clone()) {
                            self.right_targets.push(o);
                        }
                    }
                }
            }
        }
    }

    pub fn selected_right_target(&self) -> Option<&str> {
        self.right_target_selected
            .and_then(|i| self.right_targets.get(i))
            .map(String::as_str)
    }

    /// Ausstehende Deltas nachladen (Idle-Recompute nach Cache-Miss).
    pub fn ensure_deltas(&mut self) -> EngineResult<()> {
        if self.deltas_loading {
            self.rebuild_middle()?;
        }
        if self.cut_loading {
            self.cut_loading = false;
            if let Some(target) = self.selected_right_target().map(str::to_string) {
                self.load_cut_for_target(&target)?;
            }
        }
        Ok(())
    }

    pub fn load_cut_for_target(&mut self, target: &str) -> EngineResult<bool> {
        if let Some(cached) = self.cut_cache.get(target) {
            self.cut_plan = Some(cached.clone());
            return Ok(true);
        }
        let Some(base) = &self.last_outcome else {
            return Ok(false);
        };
        if let Some(plan) =
            crate::mod_08_delta::cut_package(&self.engine, &self.world, base, target)?
        {
            self.cut_cache.insert(target.to_string(), plan.clone());
            self.cut_plan = Some(plan);
            return Ok(true);
        }
        Ok(false)
    }

    /// Aktuell selektiertes Middle-Flag.
    pub fn selected_flag(&self) -> Option<String> {
        self.flag_deltas
            .get(self.middle_selected)
            .map(|d| d.flag.clone())
    }

    /// Cut-Analyse fuers selektierte Paket berechnen (`x`).
    pub fn compute_cut_plan(&mut self) -> EngineResult<bool> {
        let target = if self.pane == Pane::Right {
            self.selected_right_target()
                .map(str::to_string)
                .or_else(|| self.selected_package())
        } else {
            self.selected_package()
        };
        let Some(pkg) = target else {
            return Ok(false);
        };
        self.right_scroll = 0;
        self.load_cut_for_target(&pkg)
    }

    /// Alle Flag-Blocker des Cut-Plans anwenden (`a`).
    pub fn apply_cut_blockers(&mut self) -> EngineResult<bool> {
        let Some(plan) = self.cut_plan.clone() else {
            return Ok(false);
        };
        if plan.flag_blockers.is_empty() {
            return Ok(false);
        }
        self.push_undo();
        for fb in &plan.flag_blockers {
            self.engine
                .overrides
                .entry(fb.package.clone())
                .or_default()
                .insert(fb.flag.clone(), fb.want_on);
        }
        self.dirty = true;
        self.refresh()?;
        Ok(true)
    }

    /// package.use-Export der geaenderten Flags (`[S]`).
    pub fn export_package_use(&mut self, path: &std::path::Path) -> EngineResult<()> {
        let mut cpns: Vec<&String> = self.engine.overrides.keys().collect();
        cpns.sort();
        let mut lines = Vec::new();
        for cpn in cpns {
            let pkg = self
                .graph
                .nodes
                .get(cpn)
                .and_then(|n| self.engine.index.select_cpv(cpn, &n.cpv))
                .or_else(|| self.engine.index.select(cpn));
            let Some(pkg) = pkg else {
                continue;
            };
            let profile_only = self
                .engine
                .profile
                .effective_defaults_without_package_use(&pkg.iuse_defaults);
            let final_active = self.engine.effective_use(pkg);
            let mut flags: Vec<&String> = pkg.iuse_defaults.keys().collect();
            flags.sort();
            let mut tokens = Vec::new();
            for flag in flags {
                if self.engine.profile.is_masked(flag)
                    || self.engine.profile.is_forced(flag)
                    || crate::mod_08_delta::is_platform_flag(self.engine.profile, flag)
                    || crate::mod_08_delta::is_hidden_expand_flag(self.engine.profile, flag)
                {
                    continue;
                }
                let want = final_active.contains(flag);
                let base = profile_only.contains(flag);
                if want != base {
                    tokens.push(if want {
                        (*flag).clone()
                    } else {
                        format!("-{flag}")
                    });
                }
            }
            if !tokens.is_empty() {
                lines.push(format!("{cpn} {}", tokens.join(" ")));
            }
        }
        let body = if lines.is_empty() {
            String::new()
        } else {
            format!("{}\n", lines.join("\n"))
        };
        std::fs::write(path, body).map_err(|e| crate::mod_01_types::EngineError::Io {
            path: path.display().to_string(),
            source: e,
        })?;
        self.dirty = false;
        self.status = format!("package.use geschrieben: {}", path.display());
        Ok(())
    }
}

/// Aktuelle effektive Flags (Override oder Profil-Default).
pub(super) fn current_set(engine: &Engine<'_>, package: &str) -> HashSet<String> {
    match engine.index.select(package) {
        Some(pkg) => engine.effective_use(pkg),
        None => HashSet::new(),
    }
}

/// Overrides-Fingerabdruck fuer den Delta-Cache.
pub(super) fn overrides_fingerprint(engine: &Engine<'_>) -> String {
    let mut parts: Vec<String> = Vec::new();
    let mut cpns: Vec<&String> = engine.overrides.keys().collect();
    cpns.sort();
    for cpn in cpns {
        let mut flags: Vec<String> = engine.overrides[cpn]
            .iter()
            .map(|(f, on)| format!("{f}={on}"))
            .collect();
        flags.sort();
        parts.push(format!("{cpn}:{}", flags.join(",")));
    }
    let mut ors: Vec<String> = engine
        .or_decisions
        .iter()
        .map(|(g, s)| format!("{g}={s}"))
        .collect();
    ors.sort();
    parts.push(format!("or:{}", ors.join(",")));
    parts.join("|")
}

/// Anzeigename eines linken Eintrags (kurz, keine endlosen `||`-Listen).
pub fn left_label(entry: &LeftEntry) -> String {
    match entry {
        LeftEntry::WorldPkg(cpn) => cpn.clone(),
        LeftEntry::OrDecision(group) => {
            let short = group.split("||").next().unwrap_or(group);
            format!("|| {short}")
        }
        LeftEntry::Transitive(cpn) => format!("  {cpn}"),
    }
}

/// Konflikte eines Pakets (fuer View-Markierung).
pub fn package_conflicts<'c>(
    conflicts: &'c [ConflictInfo],
    package: &str,
) -> Vec<&'c ConflictInfo> {
    conflicts.iter().filter(|c| c.package == package).collect()
}

#[path = "12_tui_state/actions.rs"]
mod actions;
#[cfg(test)]
#[path = "12_tui_state/tests.rs"]
mod tests;
