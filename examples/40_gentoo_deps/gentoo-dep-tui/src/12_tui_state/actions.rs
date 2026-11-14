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

    fn move_clamped(cur: usize, delta: i32, len: usize) -> usize {
        if len == 0 {
            return 0;
        }
        let max = (len - 1) as i32;
        (cur as i32 + delta).clamp(0, max) as usize
    }

    pub fn page_step(&self) -> i32 {
        (self.viewport_height as i32 - 9).max(3)
    }

    /// Pane weiter schalten (`Tab`-Taste).
    pub fn cycle_pane(&mut self) {
        self.pane = match self.pane {
            Pane::Left => Pane::Middle,
            Pane::Middle => Pane::Right,
            Pane::Right => Pane::Left,
        };
        if self.pane == Pane::Right {
            self.activate_right_target();
        }
    }

    pub fn activate_right_target(&mut self) {
        if self.right_target_selected.is_none() && !self.right_targets.is_empty() {
            self.right_target_selected = Some(0);
        }
        self.preview_cut_for_selected_right_target();
    }

    pub fn preview_cut_for_selected_right_target(&mut self) {
        let Some(target) = self.selected_right_target().map(str::to_string) else {
            return;
        };
        if let Some(cached) = self.cut_cache.get(&target) {
            self.cut_plan = Some(cached.clone());
            self.cut_loading = false;
        } else {
            self.cut_loading = true;
        }
    }

    /// Linken Eintrag direkt waehlen (Mitte wird neu berechnet).
    pub fn select_left(&mut self, idx: usize) -> EngineResult<()> {
        if self.left.is_empty() {
            return Ok(());
        }
        self.left_selected = idx.min(self.left.len() - 1);
        self.rebuild_middle()
    }

    /// Selektion bewegen (Listen-Laenge beachten, Clamping).
    pub fn move_selection(&mut self, delta: i32) {
        if self.tab == Tab::BigLevers {
            if self.levers.is_empty() {
                return;
            }
            self.lever_selected = Self::move_clamped(self.lever_selected, delta, self.levers.len());
            self.lever_table_state.select(Some(self.lever_selected));
            return;
        }
        match self.pane {
            Pane::Left => {
                if self.left.is_empty() {
                    return;
                }
                let next = Self::move_clamped(self.left_selected, delta, self.left.len());
                if next != self.left_selected || self.flag_deltas.is_empty() {
                    self.select_left_fast(next);
                }
            }
            Pane::Middle => {
                let len = self.middle_len();
                if len == 0 {
                    return;
                }
                self.middle_selected =
                    Self::move_clamped(self.middle_selected, delta, len);
                self.rebuild_right_targets();
                self.cut_plan = None;
                self.right_scroll = 0;
            }
            Pane::Right => {
                if !self.right_targets.is_empty() && delta.abs() == 1 {
                    let cur = self.right_target_selected.unwrap_or(0);
                    let next = Self::move_clamped(cur, delta, self.right_targets.len());
                    self.right_target_selected = Some(next);
                    self.preview_cut_for_selected_right_target();
                } else if delta < 0 {
                    self.right_scroll = self.right_scroll.saturating_sub((-delta) as u16);
                } else {
                    self.right_scroll = self.right_scroll.saturating_add(delta as u16);
                }
            }
        }
    }

    pub fn move_to_edge(&mut self, to_end: bool) {
        if self.tab == Tab::BigLevers {
            if self.levers.is_empty() {
                return;
            }
            self.lever_selected = if to_end { self.levers.len() - 1 } else { 0 };
            self.lever_table_state.select(Some(self.lever_selected));
            return;
        }
        match self.pane {
            Pane::Left => {
                if self.left.is_empty() {
                    return;
                }
                let target_idx = if to_end { self.left.len() - 1 } else { 0 };
                self.select_left_fast(target_idx);
            }
            Pane::Middle => {
                let len = self.middle_len();
                if len == 0 {
                    return;
                }
                self.middle_selected = if to_end { len - 1 } else { 0 };
                self.rebuild_right_targets();
                self.cut_plan = None;
                self.right_scroll = 0;
            }
            Pane::Right => {
                if !self.right_targets.is_empty() {
                    let target_idx = if to_end {
                        self.right_targets.len() - 1
                    } else {
                        0
                    };
                    self.right_target_selected = Some(target_idx);
                    self.preview_cut_for_selected_right_target();
                } else if to_end {
                    self.right_scroll = u16::MAX / 2;
                } else {
                    self.right_scroll = 0;
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
            self.middle_scroll.set(0);
            self.cut_plan = None;
            self.rebuild_right_targets();
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
        self.middle_scroll.set(0);
        self.cut_plan = None;
        self.rebuild_right_targets();
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
        *selected = Self::move_clamped(*selected, delta, len);
    }

    /// Blame-Ketten des selektierten Pakets.
    pub fn selected_blame(&self) -> Vec<Vec<crate::mod_01_types::BlameLink>> {
        match self.selected_package() {
            Some(p) => self.graph.blame(&p),
            None => Vec::new(),
        }
    }
}
