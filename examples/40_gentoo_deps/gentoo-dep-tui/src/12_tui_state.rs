//! 12_tui_state — TUI-AppState und alle State-Uebergaenge.
//!
//! Genau eine Zustaendigkeit: Tabs, Fokus, Selektion, Toggle-/Quick-Switch-
//! Logik (inkl. Live-Neuberechnung ueber `06` + `07` + `08`). Kein Rendering
//! (siehe `13_tui_view`), kein Terminal-Lifecycle (siehe `14_tui_runner`).

use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

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
        };
        state.refresh()?;
        Ok(state)
    }

    /// Neu aufloesen + Listen/Deltas/Levers aktualisieren.
    pub fn refresh(&mut self) -> EngineResult<()> {
        let outcome = resolve_with_cascade(&mut self.engine, &self.world)?;
        self.graph = outcome.graph;
        self.conflicts = outcome.conflicts;
        self.rebuild_left();
        self.rebuild_middle()?;
        self.levers = big_levers(&self.engine, &self.world).unwrap_or_default();
        Ok(())
    }

    /// Linke Liste aufbauen: Welt → `||` → Transitive (nach Binpkg).
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
    }

    /// Mitte neu berechnen (Flags des Pakets oder `||`-Kandidaten).
    fn rebuild_middle(&mut self) -> EngineResult<()> {
        self.flag_deltas.clear();
        match self.left.get(self.left_selected).cloned() {
            Some(LeftEntry::WorldPkg(cpn)) | Some(LeftEntry::Transitive(cpn)) => {
                self.flag_deltas = package_deltas(&self.engine, &self.world, &cpn)?;
            }
            _ => {}
        }
        self.middle_selected = 0;
        Ok(())
    }

    /// Selektiertes Paket links (`None` bei `||`-Eintrag).
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

    /// Tab wechseln.
    pub fn set_tab(&mut self, tab: Tab) {
        self.tab = tab;
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
        let len = match (self.tab, self.pane) {
            (Tab::BigLevers, _) => self.levers.len(),
            (_, Pane::Left) => self.left.len(),
            (_, Pane::Middle) => self.middle_len(),
            (_, Pane::Right) => 0,
        };
        if len == 0 {
            return;
        }
        let cur = match (self.tab, self.pane) {
            (Tab::BigLevers, _) => self.lever_selected,
            (_, Pane::Left) => self.left_selected,
            _ => self.middle_selected,
        } as i32;
        let next = (cur + delta).rem_euclid(len as i32) as usize;
        match (self.tab, self.pane) {
            (Tab::BigLevers, _) => self.lever_selected = next,
            (_, Pane::Left) => {
                self.left_selected = next;
                let _ = self.rebuild_middle();
            }
            _ => self.middle_selected = next,
        }
    }

    /// Laenge der mittleren Liste (Flags oder Kandidaten).
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

/// Plan aus State synchronisieren (Overrides, `||`-Entscheidungen).
fn sync_plan_from_state(plan: &mut Plan, engine: &Engine<'_>, world: &[String]) {
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
    plan.baseline.world = world.to_vec();
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
        st.set_tab(Tab::BigLevers);
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
}
