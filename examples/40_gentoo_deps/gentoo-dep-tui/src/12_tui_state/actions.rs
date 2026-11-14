//! 12_tui_state/actions — AppState-Mutationen und Selektion.
//!
//! Genau eine Zustaendigkeit: Navigation, Toggle-/Quick-Switch-Aktionen,
//! Choice-Entscheidungen, Speichern (alles was `engine.overrides` aendert).

use std::path::PathBuf;

use crate::mod_01_types::{EngineResult, QuickSwitch};

use super::{
    AppState, LeftEntry, Modal, Pane, Tab, current_set, overrides_fingerprint, sync_plan_from_state,
};

impl<'a> AppState<'a> {
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
    ///
    /// Der Toggle misst gegen den Post-Kaskaden-Stand aus dem Graphen.
    /// Traegt die Simulation einen Konflikt, oeffnet `Space` stattdessen ein
    /// Modal (echter Konflikt bevorzugt, sonst simulierter, Task 4.1).
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
        if let Some(sim_conflict) = self.flag_deltas[self.middle_selected].conflict.clone() {
            if let Some(idx) = self
                .conflicts
                .iter()
                .position(|c| c.package == package || c.caused_by.contains(&package))
            {
                self.modal = Some(Modal::Conflict {
                    conflict: idx,
                    selected: 0,
                });
            } else {
                self.modal = Some(Modal::SimulatedConflict {
                    conflict: sim_conflict,
                    selected: 0,
                });
            }
            return Ok(false);
        }
        // Nur der Toggle ist expliziter Intent (keine Defaults kopieren).
        // Post-Kaskaden-Stand aus dem Graphen, Fallback auf Defaults.
        let current_on = match self.graph.nodes.get(&package) {
            Some(n) => n.use_flags.contains(&flag),
            None => current_set(&self.engine, &package).contains(&flag),
        };
        let toggled_on = !current_on;
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
    /// Plattform-Fakten sowie profil-maskierte/-erzwungene Flags sind keine
    /// Hebel und werden abgewiesen (Task 4.1).
    pub fn toggle_big_lever(&mut self) -> EngineResult<bool> {
        let lever = match self.levers.get(self.lever_selected) {
            Some(l) => l.clone(),
            None => return Ok(false),
        };
        let flag = lever.flag;
        if crate::mod_08_delta::is_platform_flag(self.engine.profile, &flag)
            || self.engine.profile.is_masked(&flag)
            || self.engine.profile.is_forced(&flag)
        {
            self.status = format!("{flag} ist kein schaltbarer Hebel");
            return Ok(false);
        }
        let holders: Vec<String> = self
            .graph
            .nodes
            .iter()
            .filter(|(_, n)| n.use_flags.contains(&flag))
            .map(|(cpn, _)| cpn.clone())
            .collect();
        if holders.is_empty() {
            // Ueberall aus → fuer alle Pakete mit diesem IUSE-Flag einschalten
            // (maskierte ausgenommen — dort waere AN wirkungslos).
            let mut any = false;
            let cpns: Vec<String> = self.graph.nodes.keys().cloned().collect();
            for cpn in cpns {
                let knows = self
                    .engine
                    .index
                    .select_cpv(&cpn, &self.graph.nodes[&cpn].cpv)
                    .is_some_and(|p| p.iuse_defaults.contains_key(&flag));
                if knows && !self.engine.profile.is_masked(&flag) {
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
        if let Some(idx) = self
            .conflicts
            .iter()
            .position(|c| c.package == package || c.caused_by.contains(&package.to_string()))
        {
            self.modal = Some(Modal::Conflict {
                conflict: idx,
                selected: 0,
            });
        }
    }

    /// Selektierte Modal-Option (echter oder simulierter Konflikt).
    pub fn modal_selected_option(&self) -> Option<QuickSwitch> {
        match &self.modal {
            Some(Modal::Conflict { conflict, selected }) => self
                .conflicts
                .get(*conflict)
                .and_then(|c| c.options.get(*selected))
                .cloned(),
            Some(Modal::SimulatedConflict { conflict, selected }) => {
                conflict.options.get(*selected).cloned()
            }
            None => None,
        }
    }

    /// Quick-Switch-Option anwenden (Modal schliessen, neu aufloesen).
    ///
    /// Leere `changes` sind keine Aktion: Status-Hinweis statt Dirty-Flag
    /// und ohne Re-Resolve (Task 4.1).
    pub fn apply_quick_switch(&mut self, option: &QuickSwitch) -> EngineResult<()> {
        if option.changes.is_empty() {
            self.status = "Keine automatische Aenderung verfuegbar".into();
            self.modal = None;
            return Ok(());
        }
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
        let len = match &self.modal {
            Some(Modal::Conflict { conflict, .. }) => self
                .conflicts
                .get(*conflict)
                .map(|c| c.options.len().max(1))
                .unwrap_or(1),
            Some(Modal::SimulatedConflict { conflict, .. }) => conflict.options.len().max(1),
            None => return,
        };
        let selected = match &mut self.modal {
            Some(Modal::Conflict { selected, .. })
            | Some(Modal::SimulatedConflict { selected, .. }) => selected,
            None => return,
        };
        *selected = (*selected as i32 + delta).rem_euclid(len as i32) as usize;
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
