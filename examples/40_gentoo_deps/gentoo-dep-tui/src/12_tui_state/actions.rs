//! 12_tui_state/actions — AppState-Mutationen und Selektion.

use crate::mod_01_types::{EngineResult, QuickSwitch};

use super::{AppState, LeftEntry, Modal, Pane, Tab, current_set, overrides_fingerprint};

impl<'a> AppState<'a> {
    pub fn selected_package(&self) -> Option<String> {
        match self.left.get(self.left_selected)? {
            LeftEntry::WorldPkg(cpn) | LeftEntry::Transitive(cpn) => Some(cpn.clone()),
            LeftEntry::OrDecision(_) => None,
        }
    }

    /// Selektierte `||`-Gruppen-ID (`None` bei Paket-Eintrag).
    pub fn selected_or_group(&self) -> Option<String> {
        match self.left.get(self.left_selected)? {
            LeftEntry::OrDecision(id) => Some(id.clone()),
            _ => None,
        }
    }

    /// Tab wechseln (Big Levers werden lazy nachgeladen).
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
                if delta < 0 {
                    self.right_scroll = self.right_scroll.saturating_sub((-delta) as u16);
                } else {
                    self.right_scroll = self.right_scroll.saturating_add(delta as u16);
                }
            }
        }
    }

    /// Schnellauswahl links: Selektion + Cache-Hit sofort, Miss → `deltas_loading`.
    pub fn select_left_fast(&mut self, idx: usize) {
        if self.left.is_empty() {
            self.flag_deltas.clear();
            self.deltas_loading = false;
            self.middle_selected = 0;
            self.right_scroll = 0;
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
                    crate::mod_00_perf::perf_count_cache_hit();
                } else {
                    self.flag_deltas.clear();
                    self.deltas_loading = true;
                    crate::mod_00_perf::perf_count_cache_miss();
                }
            }
            _ => {
                self.flag_deltas.clear();
                self.deltas_loading = false;
            }
        }
        self.middle_selected = 0;
        self.right_scroll = 0;
    }

    /// Laenge der mittleren Liste (Flags oder `||`-Kandidaten).
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
        let current_on = match self.graph.nodes.get(&package) {
            Some(n) => n.use_flags.contains(&flag),
            None => current_set(&self.engine, &package).contains(&flag),
        };
        let toggled_on = !current_on;
        self.push_undo();
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

    /// `Space` auf Tab 2: selektiertes Big-Lever-Flag global umschalten.
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
        self.push_undo();
        if lever.turn_on {
            // Enable-Hebel: fuer alle Pakete mit diesem IUSE-Flag einschalten.
            let cpns: Vec<String> = self.graph.nodes.keys().cloned().collect();
            let mut any = false;
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
                self.undo_stack.pop();
                return Ok(false);
            }
        } else {
            let holders: Vec<String> = self
                .graph
                .nodes
                .iter()
                .filter(|(_, n)| n.use_flags.contains(&flag))
                .map(|(cpn, _)| cpn.clone())
                .collect();
            if holders.is_empty() {
                self.undo_stack.pop();
                return Ok(false);
            }
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
            Some(Modal::Help) | None => None,
        }
    }

    /// Quick-Switch-Option anwenden (Modal schliessen, neu aufloesen).
    pub fn apply_quick_switch(&mut self, option: &QuickSwitch) -> EngineResult<()> {
        if option.changes.is_empty() {
            self.status = "Keine automatische Aenderung verfuegbar".into();
            self.modal = None;
            return Ok(());
        }
        self.push_undo();
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
            Some(Modal::Help) | None => return,
        };
        let selected = match &mut self.modal {
            Some(Modal::Conflict { selected, .. })
            | Some(Modal::SimulatedConflict { selected, .. }) => selected,
            Some(Modal::Help) | None => return,
        };
        *selected = (*selected as i32 + delta).rem_euclid(len as i32) as usize;
    }

    /// Blame-Ketten des selektierten Pakets.
    pub fn selected_blame(&self) -> Vec<Vec<crate::mod_01_types::BlameLink>> {
        match self.selected_package() {
            Some(p) => self.graph.blame(&p),
            None => Vec::new(),
        }
    }
}
