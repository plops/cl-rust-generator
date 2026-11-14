//! 12_tui_state — TUI-AppState und alle State-Uebergaenge.
//!
//! Genau eine Zustaendigkeit: Tabs, Fokus, Selektion, Toggle-/Quick-Switch-
//! Logik (inkl. Live-Neuberechnung ueber `06` + `07` + `08`). Kein Rendering
//! (siehe `13_tui_view`), kein Terminal-Lifecycle (siehe `14_tui_runner`).

use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

use ratatui::widgets::{ListState, TableState};

use crate::mod_01_types::{BigLever, ConflictInfo, EngineResult, FlagDelta, atom_cpn};
use crate::mod_02_profile::{Profile, UseDescriptions};
use crate::mod_05_repo_index::RepoIndex;
use crate::mod_06_graph::{Engine, ResolvedGraph};
use crate::mod_07_cascade::CascadeOutcome;
use crate::mod_07_cascade::resolve_with_cascade;
use crate::mod_08_delta::SharedRemovalImpact;
use crate::mod_08_delta::{
    big_levers, big_levers_with_base, package_deltas, package_deltas_with_base,
};
use crate::mod_09_plan_toml::Plan;

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

/// Eintrag der linken Pane (einheitlich navigierbar).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LeftEntry {
    /// Welt-/Plan-Paket.
    WorldPkg(String),
    /// Transitive `||`-Entscheidung (Gruppen-ID).
    OrDecision(String),
    /// Transitives Paket (nach Binpkg-Groesse sortiert).
    Transitive(String),
    /// `[[choice]]`-Gruppe aus `plan.toml` (Choice-ID, T07).
    PlanChoice(String),
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
    /// Simulierter Konflikt eines Toggles (noch nicht in `conflicts`):
    /// direkt anwendbar oder per `Esc` verwerfbar (Task 4.1).
    SimulatedConflict {
        /// Konfliktinfo der Simulation.
        conflict: ConflictInfo,
        /// Selektierte Option.
        selected: usize,
    },
    /// Hilfe-Overlay (`?`/`F1` oeffnet, `Esc`/`?`/`q` schliesst, review3 5.2).
    Help,
}

/// Applikationszustand (Engine-Bundle + UI-Selektion).
pub struct AppState<'a> {
    /// Resolver-Bundle (Overrides werden hier mutiert).
    pub engine: Engine<'a>,
    /// Welt-Atome.
    pub world: Vec<String>,
    /// Zugehoeriger Plan (fuer Save/Export).
    pub plan: Plan,
    /// Plan-Pfad (`None` = noch nie gespeichert).
    pub plan_path: Option<PathBuf>,
    /// Aktueller Graph nach Kaskaden.
    pub graph: ResolvedGraph,
    /// Aktuelle Konflikte.
    pub conflicts: Vec<ConflictInfo>,
    /// Letzte Kaskaden-Baseline (fuer Delta-/Lever-Simulationen ohne
    /// Doppel-Resolve, review3 Task 3.4).
    pub last_outcome: Option<CascadeOutcome>,
    /// Voll-Ersparnis-Analyse zum aktuell selektierten Flag (on demand,
    /// review3 Task 4.1).
    pub shared_impact: Option<SharedRemovalImpact>,
    /// Cache-Schluessel zu `shared_impact` (Paket, Flag, Overrides-Fingerprint).
    pub shared_impact_key: Option<(String, String, String)>,
    /// Offline-USE-Flag-Beschreibungen (`use.desc` & Co., review3 Task 4.2).
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
    /// Statuszeile (z. B. Verify-Ergebnis).
    pub status: String,
    /// Scroll-Zustand links (T06, `render_stateful_widget`).
    pub left_list_state: ListState,
    /// Scroll-Zustand Big-Levers-Tabelle (T06).
    pub lever_table_state: TableState,
    /// Vertikaler Scroll-Offset Mitte (T06, `Paragraph::scroll`).
    pub middle_scroll: u16,
    /// Vertikaler Scroll-Offset rechts (T06, `j`/`k` auf `Pane::Right`).
    pub right_scroll: u16,
    /// `levers` veraltet (T05: lazy Neuberechnung erst bei Tab-2-Besuch).
    pub levers_dirty: bool,
    /// Deltas werden gerade nachgeladen (T05: Cache-Miss/Idle-Recompute).
    pub deltas_loading: bool,
    /// Delta-Cache: `(cpn, overrides-Fingerprint)` → Deltas (T05).
    pub delta_cache: HashMap<(String, String), Vec<FlagDelta>>,
}

impl<'a> AppState<'a> {
    /// Neuer State: Engine aufbauen, einmal aufloesen.
    pub fn new(
        index: &'a RepoIndex,
        profile: &'a Profile,
        world: Vec<String>,
        plan: Plan,
        plan_path: Option<PathBuf>,
    ) -> EngineResult<Self> {
        let mut engine = Engine::new(index, profile);
        engine.overrides = plan.package_overrides();
        engine.or_decisions = plan.or_decisions();
        // Gewaehlte [[choice]]-Optionen als Welt-Erzaenzung sind bereits in
        // `effective_world` enthalten — der Aufrufer uebergibt sie in `world`.
        let mut state = Self {
            engine,
            world,
            plan,
            plan_path,
            graph: ResolvedGraph::default(),
            conflicts: Vec::new(),
            last_outcome: None,
            shared_impact: None,
            shared_impact_key: None,
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
            middle_scroll: 0,
            right_scroll: 0,
            levers_dirty: true,
            deltas_loading: false,
            delta_cache: HashMap::new(),
        };
        state.refresh()?;
        Ok(state)
    }

    /// Neu aufloesen + Listen/Deltas aktualisieren (Levers lazy, T05).
    pub fn refresh(&mut self) -> EngineResult<()> {
        let outcome = resolve_with_cascade(&mut self.engine, &self.world)?;
        // Big Levers nur in Tab 2 frisch halten; sonst als dirty markieren.
        // Bereits berechnete Baseline wiederverwenden (kein Doppel-Resolve).
        let levers = if self.tab == Tab::BigLevers {
            Some(big_levers_with_base(&self.engine, &self.world, &outcome).unwrap_or_default())
        } else {
            None
        };
        // Baseline fuer Delta-/Lever-Simulationen behalten (ein Klon pro
        // Refresh spart je Paketbesuch einen vollen Kaskadenlauf, 3.4).
        self.last_outcome = Some(outcome.clone());
        self.graph = outcome.graph;
        self.conflicts = outcome.conflicts;
        self.delta_cache.clear();
        self.rebuild_left();
        self.rebuild_middle()?;
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

    /// Big Levers bei Bedarf (nach-)berechnen (T05: lazy beim Tab-2-Besuch).
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

    /// Listen-/Tabellen-States mit Selektion synchronisieren (T06).
    fn sync_table_states(&mut self) {
        self.left_list_state.select(Some(self.left_selected));
        self.lever_table_state.select(Some(self.lever_selected));
    }

    /// Linke Liste aufbauen: Welt → Choices → `||` → Transitive (nach Binpkg).
    fn rebuild_left(&mut self) {
        let mut left = Vec::new();
        let mut seen = HashSet::new();
        for atom in &self.world {
            // Welt-Atom auf cpn kuerzen (Version/Slot-Anteile abstreifen).
            let cpn = atom_cpn(atom);
            if self.graph.nodes.contains_key(&cpn) {
                left.push(LeftEntry::WorldPkg(cpn.clone()));
                seen.insert(cpn);
            }
        }
        // `[[choice]]`-Gruppen als eigene Eintraege (T07).
        for choice in &self.plan.choice {
            left.push(LeftEntry::PlanChoice(choice.id.clone()));
        }
        for alt in &self.graph.alternatives {
            left.push(LeftEntry::OrDecision(alt.group_id.clone()));
        }
        let mut transitive: Vec<(&String, &crate::mod_06_graph::ResolvedNode)> = self
            .graph
            .nodes
            .iter()
            .filter(|(cpn, _)| !seen.contains(*cpn))
            .collect();
        transitive.sort_by_key(|(_, n)| std::cmp::Reverse(n.binpkg));
        for (cpn, _) in transitive {
            left.push(LeftEntry::Transitive(cpn.clone()));
        }
        if !self.filter.is_empty() {
            let f = self.filter.to_lowercase();
            left.retain(|e| left_label(e).to_lowercase().contains(&f));
        }
        self.left = left;
        self.left_selected = self.left_selected.min(self.left.len().saturating_sub(1));
        self.left_list_state.select(Some(self.left_selected));
    }

    /// Nur Filter anwenden (T05): kein Resolve, keine Deltas, kein Big-Levers.
    pub fn apply_filter_only(&mut self) {
        let prev = self.selected_package();
        self.rebuild_left();
        // Hat der Filter die Selektion geaendert (klassischer Fall: vor dem
        // Tippen war ein anderes Paket selektiert), Mitte sofort auf das neue
        // Paket umstellen statt stale Flags zu zeigen (review3 Task 1.2).
        if self.selected_package() != prev {
            self.select_left_fast(self.left_selected);
            return;
        }
        // Selektion erhalten, falls Paket noch sichtbar; Mitte unangetastet.
        self.middle_selected = self
            .middle_selected
            .min(self.middle_len().saturating_sub(1));
        self.middle_scroll = 0;
    }

    /// Mitte neu berechnen (Flags des Pakets oder `||`-Kandidaten).
    ///
    /// Cursor bleibt auf demselben Flag, solange dasselbe Paket selektiert
    /// ist (kein Sprung auf 0 je Toggle, Task 4.1).
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
                    // Baseline wiederverwenden statt neu aufloesen (3.4).
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
        // Gleiches Paket: Flag-Position wiederherstellen, sonst auf 0.
        if self.selected_package() == prev_pkg
            && let Some(f) = prev_flag
            && let Some(idx) = self.flag_deltas.iter().position(|d| d.flag == f)
        {
            self.middle_selected = idx;
        } else {
            self.middle_selected = 0;
        }
        self.middle_scroll = 0;
        // Paketwechsel: rechte Pane von vorn (review3 Task 1.2).
        if self.selected_package() != prev_pkg {
            self.right_scroll = 0;
        }
        Ok(())
    }

    /// Ausstehende Deltas nachladen (T05: Idle-Recompute nach Cache-Miss).
    pub fn ensure_deltas(&mut self) -> EngineResult<()> {
        if self.deltas_loading {
            self.rebuild_middle()?;
        }
        self.ensure_shared_impact()?;
        Ok(())
    }

    /// Aktuell selektiertes Middle-Flag (fuer Doku + Shared-Analyse).
    pub fn selected_flag(&self) -> Option<String> {
        self.flag_deltas
            .get(self.middle_selected)
            .map(|d| d.flag.clone())
    }

    /// Voll-Ersparnis-Analyse zum aktuellen Flag nachladen (on demand,
    /// Ergebnis im State gecacht, review3 Task 4.1).
    pub fn ensure_shared_impact(&mut self) -> EngineResult<()> {
        let (package, flag) = match (self.selected_package(), self.selected_flag()) {
            (Some(p), Some(f)) => (p, f),
            _ => {
                self.shared_impact = None;
                self.shared_impact_key = None;
                return Ok(());
            }
        };
        if self.tab != Tab::Packages {
            return Ok(());
        }
        let key = (
            package.clone(),
            flag.clone(),
            overrides_fingerprint(&self.engine),
        );
        if self.shared_impact_key.as_ref() == Some(&key) {
            return Ok(());
        }
        let impact = match &self.last_outcome {
            Some(outcome) => crate::mod_08_delta::shared_removal_impact(
                &self.engine,
                &self.world,
                outcome,
                &package,
                &flag,
            )?,
            None => None,
        };
        self.shared_impact = impact;
        self.shared_impact_key = Some(key);
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

// Hinweis: Welt-Atome werden zentral via `crate::mod_01_types::atom_cpn`
// gekuerzt (review3 Task 1.3); kein lokales Duplikat mehr.

/// Overrides-Fingerabdruck fuer den Delta-Cache (T05).
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

/// Plan aus State synchronisieren (Overrides, `||`-Entscheidungen).
///
/// `baseline.world` wird NICHT mit gewaehlten Choice-Atomen ueberschrieben
/// (T07): Choice-Atome gehoeren zur Option, nicht zur Baseline.
pub fn sync_plan_from_state(plan: &mut Plan, engine: &Engine<'_>, world: &[String]) {
    use std::collections::BTreeMap;
    let mut by_pkg: BTreeMap<&String, &HashMap<String, bool>> = BTreeMap::new();
    for (cpn, map) in &engine.overrides {
        by_pkg.insert(cpn, map);
    }
    // Bestehende Eintraege aktualisieren, neue anhaengen (sortiert, stabil).
    // `false` → `-flag`-Token (Portage-Syntax).
    for (cpn, map) in &by_pkg {
        let mut tokens: Vec<String> = map
            .iter()
            .map(|(flag, on)| {
                if *on {
                    flag.clone()
                } else {
                    format!("-{flag}")
                }
            })
            .collect();
        tokens.sort();
        match plan.package.iter_mut().find(|p| &p.atom == *cpn) {
            Some(entry) => entry.use_flags = tokens,
            None => plan.package.push(crate::mod_09_plan_toml::PlanPackage {
                atom: (*cpn).clone(),
                use_flags: tokens,
            }),
        }
    }
    plan.package.sort_by(|a, b| a.atom.cmp(&b.atom));
    // Choice-Atome duerfen `baseline.world` nicht verfuettern (T07).
    let choice_atoms: std::collections::HashSet<&String> = plan
        .choice
        .iter()
        .flat_map(|c| c.option.iter().map(|o| &o.atom))
        .collect();
    plan.baseline.world = world
        .iter()
        .filter(|w| !choice_atoms.contains(w))
        .cloned()
        .collect();
    let mut decisions: Vec<_> = engine.or_decisions.iter().collect();
    decisions.sort_by(|a, b| a.0.cmp(b.0));
    plan.or_decision = decisions
        .into_iter()
        .map(|(group_id, selected)| crate::mod_09_plan_toml::OrDecision {
            group_id: group_id.clone(),
            selected: *selected,
        })
        .collect();
}

/// Anzeigename eines linken Eintrags.
pub fn left_label(entry: &LeftEntry) -> String {
    match entry {
        LeftEntry::WorldPkg(cpn) => cpn.clone(),
        LeftEntry::OrDecision(group) => format!("|| {group}"),
        LeftEntry::Transitive(cpn) => format!("  {cpn}"),
        LeftEntry::PlanChoice(id) => format!("choice: {id}"),
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
