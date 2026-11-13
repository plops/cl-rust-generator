//! 12_tui_state — TUI-AppState und alle State-Uebergaenge.
//!
//! Genau eine Zustaendigkeit: Tabs, Fokus, Selektion, Toggle-/Quick-Switch-
//! Logik (inkl. Live-Neuberechnung ueber `06` + `07` + `08`). Kein Rendering
//! (siehe `13_tui_view`), kein Terminal-Lifecycle (siehe `14_tui_runner`).

use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

use ratatui::widgets::{ListState, TableState};

use crate::mod_01_types::{BigLever, ConflictInfo, EngineResult, FlagDelta, QuickSwitch};
use crate::mod_02_profile::Profile;
use crate::mod_05_repo_index::RepoIndex;
use crate::mod_06_graph::{Engine, ResolvedGraph};
use crate::mod_07_cascade::resolve_with_cascade;
use crate::mod_08_delta::{big_levers, package_deltas};
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
        self.graph = outcome.graph;
        self.conflicts = outcome.conflicts;
        self.delta_cache.clear();
        self.rebuild_left();
        self.rebuild_middle()?;
        // Big Levers nur in Tab 2 frisch halten; sonst als dirty markieren.
        if self.tab == Tab::BigLevers {
            self.levers = big_levers(&self.engine, &self.world).unwrap_or_default();
            self.levers_dirty = false;
            self.sync_table_states();
        } else {
            self.levers_dirty = true;
        }
        Ok(())
    }

    /// Big Levers bei Bedarf (nach-)berechnen (T05: lazy beim Tab-2-Besuch).
    pub fn ensure_levers(&mut self) -> EngineResult<()> {
        if self.levers_dirty {
            self.levers = big_levers(&self.engine, &self.world).unwrap_or_default();
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
            let cpn = world_atom_cpn(atom);
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
        self.rebuild_left();
        // Selektion erhalten, falls Paket noch sichtbar; Mitte unangetastet.
        self.middle_selected = self
            .middle_selected
            .min(self.middle_len().saturating_sub(1));
        self.middle_scroll = 0;
    }

    /// Mitte neu berechnen (Flags des Pakets oder `||`-Kandidaten).
    fn rebuild_middle(&mut self) -> EngineResult<()> {
        self.flag_deltas.clear();
        match self.left.get(self.left_selected).cloned() {
            Some(LeftEntry::WorldPkg(cpn)) | Some(LeftEntry::Transitive(cpn)) => {
                let key = (cpn.clone(), overrides_fingerprint(&self.engine));
                if let Some(cached) = self.delta_cache.get(&key) {
                    self.flag_deltas = cached.clone();
                    self.deltas_loading = false;
                } else {
                    self.deltas_loading = true;
                    let deltas = package_deltas(&self.engine, &self.world, &cpn)?;
                    self.delta_cache.insert(key, deltas.clone());
                    self.flag_deltas = deltas;
                    self.deltas_loading = false;
                }
            }
            _ => {
                self.deltas_loading = false;
            }
        }
        self.middle_selected = 0;
        self.middle_scroll = 0;
        Ok(())
    }

    /// Ausstehende Deltas nachladen (T05: Idle-Recompute nach Cache-Miss).
    pub fn ensure_deltas(&mut self) -> EngineResult<()> {
        if self.deltas_loading {
            self.rebuild_middle()?;
        }
        Ok(())
    }

    /// Selektiertes Paket links (`None` bei `||`-Eintrag).
    pub fn selected_package(&self) -> Option<String> {
        match self.left.get(self.left_selected)? {
            LeftEntry::WorldPkg(cpn) | LeftEntry::Transitive(cpn) => Some(cpn.clone()),
            LeftEntry::OrDecision(_) | LeftEntry::PlanChoice(_) => None,
        }
    }

    /// Selektierte `||`-Gruppen-ID (`None` bei Paket-Eintrag).
    pub fn selected_or_group(&self) -> Option<String> {
        match self.left.get(self.left_selected)? {
            LeftEntry::OrDecision(id) => Some(id.clone()),
            _ => None,
        }
    }

    /// Selektierte `[[choice]]`-ID (`None` bei Paket-/`||`-Eintrag, T07).
    pub fn selected_choice(&self) -> Option<String> {
        match self.left.get(self.left_selected)? {
            LeftEntry::PlanChoice(id) => Some(id.clone()),
            _ => None,
        }
    }

    /// Tab wechseln (Big Levers werden lazy nachgeladen, T05).
    pub fn set_tab(&mut self, tab: Tab) -> EngineResult<()> {
        self.tab = tab;
        if tab == Tab::BigLevers {
            self.ensure_levers()?;
        }
        Ok(())
    }

    /// Pane weiter schalten (`Tab`-Taste).
    pub fn cycle_pane(&mut self) {
        self.pane = match self.pane {
            Pane::Left => Pane::Middle,
            Pane::Middle => Pane::Right,
            Pane::Right => Pane::Left,
        };
    }

    /// Linken Eintrag direkt waehlen (Mitte wird neu berechnet).
    pub fn select_left(&mut self, idx: usize) -> EngineResult<()> {
        if self.left.is_empty() {
            return Ok(());
        }
        self.left_selected = idx.min(self.left.len() - 1);
        self.rebuild_middle()
    }

    /// Selektion bewegen (Listen-Laenge beachten, Wrap-around).
    ///
    /// Links wird die gecachte Variante genutzt (T05): Cache-Hit ist sofort
    /// da, Cache-Miss setzt `deltas_loading` und wird per `ensure_deltas`
    /// (Idle im Event-Loop) nachgeladen. Rechts scrollt `right_scroll` (T06).
    pub fn move_selection(&mut self, delta: i32) {
        if self.tab == Tab::BigLevers {
            if self.levers.is_empty() {
                return;
            }
            let len = self.levers.len() as i32;
            self.lever_selected = (self.lever_selected as i32 + delta).rem_euclid(len) as usize;
            self.lever_table_state.select(Some(self.lever_selected));
            return;
        }
        match self.pane {
            Pane::Left => {
                if self.left.is_empty() {
                    return;
                }
                let len = self.left.len() as i32;
                self.left_selected = (self.left_selected as i32 + delta).rem_euclid(len) as usize;
                self.left_list_state.select(Some(self.left_selected));
                self.select_left_fast(self.left_selected);
            }
            Pane::Middle => {
                let len = self.middle_len();
                if len == 0 {
                    return;
                }
                self.middle_selected =
                    (self.middle_selected as i32 + delta).rem_euclid(len as i32) as usize;
            }
            Pane::Right => {
                // Blame-/Pros-Cons-Text scrollen (T06).
                if delta < 0 {
                    self.right_scroll = self.right_scroll.saturating_sub((-delta) as u16);
                } else {
                    self.right_scroll = self.right_scroll.saturating_add(delta as u16);
                }
            }
        }
    }

    /// Schnellauswahl links (T05): Selektion + Cache-Hit sofort, Miss → `deltas_loading`.
    pub fn select_left_fast(&mut self, idx: usize) {
        if self.left.is_empty() {
            return;
        }
        self.left_selected = idx.min(self.left.len() - 1);
        self.left_list_state.select(Some(self.left_selected));
        match self.left.get(self.left_selected).cloned() {
            Some(LeftEntry::WorldPkg(cpn)) | Some(LeftEntry::Transitive(cpn)) => {
                let key = (cpn, overrides_fingerprint(&self.engine));
                if let Some(cached) = self.delta_cache.get(&key) {
                    self.flag_deltas = cached.clone();
                    self.deltas_loading = false;
                } else {
                    // Miss: alte Flags nicht weiter anzeigen (Idle-Recompute folgt).
                    self.flag_deltas.clear();
                    self.deltas_loading = true;
                }
                self.middle_selected = 0;
                self.middle_scroll = 0;
            }
            _ => {
                self.flag_deltas.clear();
                self.deltas_loading = false;
                self.middle_selected = 0;
                self.middle_scroll = 0;
            }
        }
    }

    /// Laenge der mittleren Liste (Flags, Kandidaten oder Choice-Optionen).
    pub fn middle_len(&self) -> usize {
        match self.left.get(self.left_selected) {
            Some(LeftEntry::OrDecision(group)) => self
                .graph
                .alternatives
                .iter()
                .find(|a| &a.group_id == group)
                .map(|a| a.candidates.len())
                .unwrap_or(0),
            Some(LeftEntry::WorldPkg(_)) | Some(LeftEntry::Transitive(_)) => self.flag_deltas.len(),
            Some(LeftEntry::PlanChoice(id)) => self
                .plan
                .choice
                .iter()
                .find(|c| &c.id == id)
                .map(|c| c.option.len())
                .unwrap_or(0),
            None => 0,
        }
    }

    /// `Space`: USE-Flag toggeln (nur bei Paket + Flag-Selektion).
    pub fn toggle_selected_flag(&mut self) -> EngineResult<bool> {
        let package = match self.selected_package() {
            Some(p) => p,
            None => return Ok(false),
        };
        let flag = match self.flag_deltas.get(self.middle_selected) {
            Some(d) => d.flag.clone(),
            None => return Ok(false),
        };
        // Conflict-Flags sind nicht direkt schaltbar → Modal anbieten.
        if self.flag_deltas[self.middle_selected].conflict.is_some() {
            self.open_conflict_for_package(&package);
            return Ok(false);
        }
        // Nur der Toggle ist expliziter Intent (keine Defaults kopieren).
        let toggled_on = !current_set(&self.engine, &package).contains(&flag);
        self.engine
            .overrides
            .entry(package)
            .or_default()
            .insert(flag, toggled_on);
        self.dirty = true;
        self.refresh()?;
        Ok(true)
    }

    /// `Enter` auf `||`-Eintrag: naechste Option waehlen (zyklisch).
    pub fn cycle_or_decision(&mut self) -> EngineResult<bool> {
        let group = match self.selected_or_group() {
            Some(g) => g,
            None => return Ok(false),
        };
        let (candidates, selected) = match self
            .graph
            .alternatives
            .iter()
            .find(|a| a.group_id == group)
            .map(|a| (a.candidates.len(), a.selected))
        {
            Some(v) => v,
            None => return Ok(false),
        };
        if candidates == 0 {
            return Ok(false);
        }
        let next = (selected + 1) % candidates;
        self.engine.or_decisions.insert(group, next);
        self.dirty = true;
        self.refresh()?;
        Ok(true)
    }

    /// `Space` auf Tab 2 (T07): selektiertes Big-Lever-Flag global umschalten.
    ///
    /// Ist das Flag irgendwo an, wird es ueberall ausgeschaltet; ist es
    /// ueberall aus, wird es fuer alle Pakete eingeschaltet, die es kennen.
    pub fn toggle_big_lever(&mut self) -> EngineResult<bool> {
        let lever = match self.levers.get(self.lever_selected) {
            Some(l) => l.clone(),
            None => return Ok(false),
        };
        let flag = lever.flag;
        let holders: Vec<String> = self
            .graph
            .nodes
            .iter()
            .filter(|(_, n)| n.use_flags.contains(&flag))
            .map(|(cpn, _)| cpn.clone())
            .collect();
        if holders.is_empty() {
            // Ueberall aus → fuer alle Pakete mit diesem IUSE-Flag einschalten.
            let mut any = false;
            let cpns: Vec<String> = self.graph.nodes.keys().cloned().collect();
            for cpn in cpns {
                let knows = self
                    .engine
                    .index
                    .select(&cpn)
                    .is_some_and(|p| p.iuse_defaults.contains_key(&flag));
                if knows {
                    self.engine
                        .overrides
                        .entry(cpn)
                        .or_default()
                        .insert(flag.clone(), true);
                    any = true;
                }
            }
            if !any {
                return Ok(false);
            }
        } else {
            for cpn in holders {
                self.engine
                    .overrides
                    .entry(cpn)
                    .or_default()
                    .insert(flag.clone(), false);
            }
        }
        self.dirty = true;
        self.refresh()?;
        Ok(true)
    }

    /// `Enter`/`Space` auf `[[choice]]` (T07): naechste Option waehlen.
    pub fn cycle_choice_option(&mut self) -> EngineResult<bool> {
        let id = match self.selected_choice() {
            Some(id) => id,
            None => return Ok(false),
        };
        let next = match self.plan.choice.iter().find(|c| c.id == id) {
            Some(c) if !c.option.is_empty() => (c.selected + 1) % c.option.len(),
            _ => return Ok(false),
        };
        if let Some(choice) = self.plan.choice.iter_mut().find(|c| c.id == id) {
            choice.selected = next;
        }
        self.sync_world_from_plan();
        // `suggested_use` der neu gewaehlten Option einmischen (manuelle
        // Toggles bleiben erhalten — kein Voll-Reset der Overrides!).
        if let Some(choice) = self.plan.choice.iter().find(|c| c.id == id)
            && let Some(opt) = choice.option.get(choice.selected)
        {
            for token in &opt.suggested_use {
                if let Some(flag) = token.strip_prefix('-') {
                    self.engine
                        .overrides
                        .entry(opt.atom.clone())
                        .or_default()
                        .insert(flag.to_string(), false);
                } else {
                    self.engine
                        .overrides
                        .entry(opt.atom.clone())
                        .or_default()
                        .insert(token.clone(), true);
                }
            }
        }
        self.dirty = true;
        self.refresh()?;
        Ok(true)
    }

    /// Welt aus `plan.baseline.world` + gewaehlten Choice-Optionen ableiten.
    fn sync_world_from_plan(&mut self) {
        use std::collections::HashSet;
        let mut choice_atoms: HashSet<&String> = HashSet::new();
        for choice in &self.plan.choice {
            for opt in &choice.option {
                choice_atoms.insert(&opt.atom);
            }
        }
        let mut world = self.plan.baseline.world.clone();
        if world.is_empty() {
            // Ohne Baseline: bisherige Welt ohne alte Choice-Atome behalten.
            world = self
                .world
                .iter()
                .filter(|w| !choice_atoms.contains(w))
                .cloned()
                .collect();
        }
        for atom in self.plan.effective_world() {
            if !world.contains(&atom) {
                world.push(atom);
            }
        }
        self.world = world;
    }

    /// Konflikt-Modal fuer Paket oeffnen (erster passender Konflikt).
    pub fn open_conflict_for_package(&mut self, package: &str) {
        if let Some(idx) = self.conflicts.iter().position(|c| c.package == package) {
            self.modal = Some(Modal::Conflict {
                conflict: idx,
                selected: 0,
            });
        }
    }

    /// Quick-Switch-Option anwenden (Modal schliessen, neu aufloesen).
    pub fn apply_quick_switch(&mut self, option: &QuickSwitch) -> EngineResult<()> {
        for (package, flag, want_on) in &option.changes {
            self.engine
                .overrides
                .entry(package.clone())
                .or_default()
                .insert(flag.clone(), *want_on);
        }
        self.modal = None;
        self.dirty = true;
        self.refresh()
    }

    /// Modal-Selektion bewegen.
    pub fn move_modal(&mut self, delta: i32) {
        if let Some(Modal::Conflict { conflict, selected }) = &mut self.modal {
            let len = self
                .conflicts
                .get(*conflict)
                .map(|c| c.options.len().max(1))
                .unwrap_or(1);
            *selected = (*selected as i32 + delta).rem_euclid(len as i32) as usize;
        }
    }

    /// In `plan.toml` speichern (Dirty-Flag zuruecksetzen).
    pub fn save(&mut self) -> EngineResult<PathBuf> {
        let path = self
            .plan_path
            .clone()
            .unwrap_or_else(|| PathBuf::from("plan.toml"));
        sync_plan_from_state(&mut self.plan, &self.engine, &self.world);
        self.plan.save(&path)?;
        self.plan_path = Some(path.clone());
        self.dirty = false;
        self.status = format!("Gespeichert: {}", path.display());
        Ok(path)
    }

    /// Blame-Ketten des selektierten Pakets.
    pub fn selected_blame(&self) -> Vec<Vec<crate::mod_01_types::BlameLink>> {
        match self.selected_package() {
            Some(p) => self.graph.blame(&p),
            None => Vec::new(),
        }
    }
}

/// Aktuelle effektive Flags (Override oder Profil-Default).
fn current_set(engine: &Engine<'_>, package: &str) -> HashSet<String> {
    match engine.index.select(package) {
        Some(pkg) => engine.effective_use(pkg),
        None => HashSet::new(),
    }
}

/// Welt-Atom auf `cat/pkg` kuerzen (Version nach letztem `-` mit Ziffer).
fn world_atom_cpn(atom: &str) -> String {
    let atom = atom.split_whitespace().next().unwrap_or(atom);
    let atom = atom.trim_start_matches(['=', '<', '>', '~']);
    match atom.find('/') {
        Some(slash) => {
            let (cat, rest) = atom.split_at(slash);
            let pkg = &rest[1..];
            // `name-version`: letzter `-`, wonach eine Ziffer folgt.
            let mut cut = None;
            for (i, c) in pkg.char_indices() {
                if c == '-'
                    && pkg[i + 1..]
                        .chars()
                        .next()
                        .is_some_and(|d| d.is_ascii_digit())
                {
                    cut = Some(i);
                }
            }
            match cut {
                Some(i) => format!("{cat}/{}", &pkg[..i]),
                None => atom.to_string(),
            }
        }
        None => atom.to_string(),
    }
}

/// Overrides-Fingerabdruck fuer den Delta-Cache (T05).
fn overrides_fingerprint(engine: &Engine<'_>) -> String {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mod_03_manifest::parse_manifest;
    use portage_metadata::CacheEntry;
    use std::collections::HashMap;

    fn pkg(
        cpn: &str,
        iuse: &str,
        rdepend: &str,
        binpkg: u64,
    ) -> crate::mod_05_repo_index::PkgVersion {
        let text = format!(
            "EAPI=8\nDESCRIPTION=t\nSLOT=0\nKEYWORDS=amd64\nIUSE={iuse}\n\
             RDEPEND={rdepend}\nSRC_URI=http://x/a.tar.gz\n"
        );
        let entry = CacheEntry::parse(&text).unwrap();
        let mut iuse_defaults = HashMap::new();
        for flag in &entry.metadata.iuse {
            iuse_defaults.insert(flag.name().to_string(), flag.is_enabled_default());
        }
        crate::mod_05_repo_index::PkgVersion {
            cpn: cpn.into(),
            cpv: format!("{cpn}-1.0"),
            version_str: "1.0".into(),
            version: portage_atom::Version::parse("1.0").unwrap(),
            slot: "0".into(),
            masked: false,
            live: false,
            iuse_defaults,
            entry,
            manifest: parse_manifest("DIST a.tar.gz 100\n"),
            binhost_size: Some(binpkg),
        }
    }

    fn fixture<'x>(index: &'x RepoIndex, profile: &'x Profile) -> AppState<'x> {
        AppState::new(
            index,
            profile,
            vec!["app/a".to_string()],
            Plan::default(),
            None,
        )
        .unwrap()
    }

    fn test_index() -> RepoIndex {
        let mut map = HashMap::new();
        for p in [
            pkg("app/a", "+ssl", "ssl? ( dev-libs/openssl )", 100),
            pkg("dev-libs/openssl", "+ssl", "", 800),
        ] {
            map.entry(p.cpn.clone()).or_insert_with(Vec::new).push(p);
        }
        RepoIndex {
            versions: map,
            cache_files: 2,
            load_ms: 0,
        }
    }

    #[test]
    fn tab_switch_and_pane_cycle() {
        let index = test_index();
        let profile = Profile::test_default();
        let mut st = fixture(&index, &profile);
        assert_eq!(st.tab, Tab::Packages);
        st.set_tab(Tab::BigLevers).unwrap();
        assert_eq!(st.tab, Tab::BigLevers);
        st.cycle_pane();
        assert_eq!(st.pane, Pane::Middle);
        st.cycle_pane();
        assert_eq!(st.pane, Pane::Right);
    }

    #[test]
    fn toggle_updates_deltas_and_dirty() {
        let index = test_index();
        let profile = Profile::test_default();
        let mut st = fixture(&index, &profile);
        assert!(st.graph.nodes.contains_key("dev-libs/openssl"));
        st.pane = Pane::Middle;
        assert!(st.toggle_selected_flag().unwrap());
        assert!(st.dirty);
        assert!(!st.graph.nodes.contains_key("dev-libs/openssl"));
        // Zurueckschalten stellt wieder her.
        assert!(st.toggle_selected_flag().unwrap());
        assert!(st.graph.nodes.contains_key("dev-libs/openssl"));
    }

    #[test]
    fn quick_switch_resolves_conflict() {
        let mut map = HashMap::new();
        for p in [
            pkg("app/a", "bar", "dev-libs/libx[foo]", 10),
            pkg("app/c", "baz", "dev-libs/libx[-foo]", 10),
            pkg("dev-libs/libx", "foo", "", 50),
        ] {
            map.entry(p.cpn.clone()).or_insert_with(Vec::new).push(p);
        }
        let index = RepoIndex {
            versions: map,
            cache_files: 3,
            load_ms: 0,
        };
        let profile = Profile::test_default();
        let mut st = AppState::new(
            &index,
            &profile,
            vec!["app/a".to_string(), "app/c".to_string()],
            Plan::default(),
            None,
        )
        .unwrap();
        // Overrides fuer bar/baz setzen (sonst keine use-dep-Constraints aktiv).
        st.engine
            .overrides
            .insert("app/a".into(), [("bar".into(), true)].into_iter().collect());
        st.engine
            .overrides
            .insert("app/c".into(), [("baz".into(), true)].into_iter().collect());
        st.refresh().unwrap();
        assert!(!st.conflicts.is_empty());
        let option = st.conflicts[0].options[0].clone();
        st.apply_quick_switch(&option).unwrap();
        assert!(st.modal.is_none());
    }

    #[test]
    fn save_resets_dirty() {
        let index = test_index();
        let profile = Profile::test_default();
        let mut st = fixture(&index, &profile);
        let d = tempfile::tempdir().unwrap();
        st.plan_path = Some(d.path().join("plan.toml"));
        st.pane = Pane::Middle;
        st.toggle_selected_flag().unwrap();
        assert!(st.dirty);
        st.save().unwrap();
        assert!(!st.dirty);
        let back = Plan::load(&d.path().join("plan.toml")).unwrap();
        assert!(!back.package.is_empty());
    }

    #[test]
    fn world_atom_cpn_strips_version() {
        assert_eq!(world_atom_cpn(">=dev-libs/openssl-3.0"), "dev-libs/openssl");
        assert_eq!(world_atom_cpn("app-misc/jq"), "app-misc/jq");
    }

    #[test]
    fn big_lever_toggle_disables_globally() {
        let index = test_index();
        let profile = Profile::test_default();
        let mut st = fixture(&index, &profile);
        st.set_tab(Tab::BigLevers).unwrap();
        assert!(!st.levers.is_empty());
        let pos = st.levers.iter().position(|l| l.flag == "ssl").unwrap();
        st.lever_selected = pos;
        st.lever_table_state.select(Some(pos));
        assert!(st.toggle_big_lever().unwrap());
        // ssl ueberall aus → openssl faellt aus dem Graphen.
        assert!(!st.graph.nodes.contains_key("dev-libs/openssl"));
        // Zurueck: wieder einschalten stellt openssl wieder her.
        st.set_tab(Tab::BigLevers).unwrap();
        let pos = st.levers.iter().position(|l| l.flag == "ssl").unwrap();
        st.lever_selected = pos;
        assert!(st.toggle_big_lever().unwrap());
        assert!(st.graph.nodes.contains_key("dev-libs/openssl"));
    }

    #[test]
    fn choice_switch_updates_world_without_baseline() {
        use crate::mod_09_plan_toml::{ChoiceOption, PlanChoice};
        let mut map = HashMap::new();
        for p in [
            pkg("app/a", "+ssl", "dev-libs/openssl", 100),
            pkg("net-misc/openssh", "", "", 200),
            pkg("net-misc/dropbear", "", "", 50),
            pkg("dev-libs/openssl", "+ssl", "", 800),
        ] {
            map.entry(p.cpn.clone()).or_insert_with(Vec::new).push(p);
        }
        let index = RepoIndex {
            versions: map,
            cache_files: 4,
            load_ms: 0,
        };
        let profile = Profile::test_default();
        let mut plan = Plan::default();
        // Baseline ohne Choice-Atome (T07-Konvention): Optionen kommen aus `choice`.
        plan.baseline.world = vec!["app/a".to_string()];
        plan.choice.push(PlanChoice {
            id: "ssh".into(),
            required: true,
            description: "ssh".into(),
            selected: 0,
            option: vec![
                ChoiceOption {
                    atom: "net-misc/openssh".into(),
                    pros: "voll".into(),
                    cons: "gross".into(),
                    suggested_use: vec![],
                },
                ChoiceOption {
                    atom: "net-misc/dropbear".into(),
                    pros: "klein".into(),
                    cons: "weniger".into(),
                    suggested_use: vec![],
                },
            ],
        });
        let mut st = AppState::new(&index, &profile, plan.effective_world(), plan, None).unwrap();
        assert!(
            st.left
                .iter()
                .any(|e| *e == LeftEntry::PlanChoice("ssh".into()))
        );
        let idx = st
            .left
            .iter()
            .position(|e| *e == LeftEntry::PlanChoice("ssh".into()))
            .unwrap();
        st.left_selected = idx;
        assert!(st.cycle_choice_option().unwrap());
        assert!(st.world.contains(&"net-misc/dropbear".to_string()));
        assert!(!st.world.contains(&"net-misc/openssh".to_string()));
        // Baseline darf das Choice-Atom nicht enthalten (T07).
        sync_plan_from_state(&mut st.plan, &st.engine, &st.world);
        assert!(
            !st.plan
                .baseline
                .world
                .contains(&"net-misc/dropbear".to_string())
        );
        assert!(st.plan.baseline.world.contains(&"app/a".to_string()));
    }

    #[test]
    fn filter_only_does_not_resolve() {
        let index = test_index();
        let profile = Profile::test_default();
        let mut st = fixture(&index, &profile);
        let before = st.graph.nodes.len();
        st.filtering = true;
        st.filter.push('a');
        st.apply_filter_only();
        // Kein Resolve: Graph unveraendert, Levers unangetastet.
        assert_eq!(st.graph.nodes.len(), before);
        assert!(!st.filter.is_empty());
    }
}
