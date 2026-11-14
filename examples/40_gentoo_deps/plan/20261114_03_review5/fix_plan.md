Super, deine Antworten klären alle offenen Designfragen eindeutig. Nachfolgend bekommst du:

1. **Das verbindliche Architektur-Zielbild** (inkl. Einordnung zum Thema Cucumber / GUI-Tests), und
2. **Den fertigen Umsetzungs- und Test-Auftrag für deinen AI-Agenten** — mit exakten Dateipfaden, explizitem Rust-Code, Docker/SSH/`deepwiki`-Hinweisen und einem vollständigen Test-Katalog, damit es kein Hin-und-Her mehr gibt.

---

## 1. Architektur-Zielbild & Test-Strategie (kurz vorweg)

* **Datenfluss-Architektur (`00_perf` → `14_tui_runner`) bleibt strikt erhalten** — es kommen keine neuen Schichten dazu, stattdessen schrumpft der Code um ca. 200 Zeilen:
  * **`shared_removal_impact` + `deltas --deep` fliegen komplett raus.**
  * **Ersatz im UI (kostenlos & gezielt):** Ein reiner AST/Graph-Lookup (`shared_flag_deps`, 0 ms, keine Simulation) listet rechts bei jedem Flag ggf. geteilte Abhängigkeiten auf. Wenn der Nutzer in `Pane::Right` (`Tab`) navigiert, kann er dort direkt über alle sichtbaren Pakete (Blame-Eltern, `bewirkt:`-Pakete, geteilte Abhängigkeiten) wandern — und die **Cut-Analyse (`cut_package`) wird automatisch** für das gerade fokussierte Paket im unteren Split der rechten Pane angezeigt (`cut_cache` + Idle-Update).
* **Zur GUI-Test-Frage (Cucumber vs. Rust-Harness):**
  `cucumber-rs` bringt für ein schlankes TUI-Tool unnötig viel Makro-/Async-Ballast. **Das Repo besitzt in `tests/tui_screencaps.rs` bereits ein eigenes Scenario-Harness (`Driver` mit `Step::Type`, `Step::Key`, `Step::Idle`, `Step::Capture`) gegen `ratatui::TestBackend` und exakt denselben `dispatch_key` wie die echte TUI.** Wir erweitern genau dieses Harness um BDD-ähnliche Szenarien für *alle* deine manuellen Testfälle plus echte System-Tests per SSH auf dem Gentoo-Host.

---

## 2. Direkter Arbeitsauftrag für den KI-Agenten

Den folgenden Block kannst du 1:1 an den Agenten übergeben:

````markdown
# Auftrag: Bugfixes, UX-Verbesserungen & Vereinfachung in `gentoo-dep-tui`

## Arbeitsumgebung & Werkzeuge
1. **Dependencies nachschlagen (`deepwiki` MCP):**
   Rate bei Crate-APIs (`ratatui` 0.30, `crossterm` 0.29, `portage-atom` 0.12, `portage-metadata` 0.11) **nicht**, sondern nutze den `deepwiki` MCP-Server, falls du Methodennamen oder Signaturen nachsehen musst.
2. **Gentoo-Host für System-Tests (SSH aus dem Docker-Container):**
   Du läufst in einem Docker-Container. Auf dem Host läuft ein echtes Gentoo-System (gebaut über `/workspace/src/cl-py-generator/example/110_gentoo/openrc/Dockerfile`).
   - SSH-Identity-Key: `cl-rust-generator/id_lenovo_to_hpz6`
   - SSH-Benutzer: `kiel`
   - Host-IP: Ermittle die `eno1`-IP des Hosts dynamisch im Container über `ip a` bzw. `ip route` (z. B. `10.60.x.y`). **Schreibe die echte IP niemals ins Git-Repository!** Nutze bei Doku/Logs immer `<HOST_IP>`.
   - Beispielaufruf: `ssh -i cl-rust-generator/id_lenovo_to_hpz6 -o BatchMode=yes -o StrictHostKeyChecking=accept-new kiel@<HOST_IP> "uname -a"`
   - Nutze den Gentoo-Host am Ende für die System-/HIL-Tests (siehe Phase 3).
3. **Architektur-Regel:**
   Halte die bestehende Schichtenordnung `01_types` → `02_profile` → ... → `08_delta` → `11_cli` → `12_tui_state` → `13_tui_view` → `14_tui_runner` strikt ein. Keine UI-Logik in `08_delta`, keine Solver-Logik in `13_tui_view`.

---

## Phase 1: Code-Änderungen (exakt wie vorgegeben umsetzen)

### Task 1: `package.use`-Präzedenz & Profil-Diff-Export (`src/02_profile.rs`, `src/02_profile/stack.rs`, `src/11_cli/context.rs`)

**Problem:**
1. `Profile::sync_pattern_caches()` sortiert `package_use`-Keys alphabetisch (`>` vor `a`), was Portage-Zeilenreihenfolge bricht.
2. Für den sauberen `[S]`-Export (nur vom reinen Profil abweichende, lebende Flags) braucht `Profile` eine Methode `effective_defaults_without_package_use`.

**Änderungen:**
1. In `src/02_profile/stack.rs` ergänze `read_package_use_lines` und implementiere `read_package_use` darüber:
```rust
/// Liest `package.use` in exakter Datei- und Zeilenreihenfolge.
pub fn read_package_use_lines(path: &Path) -> EngineResult<Vec<(String, Vec<String>)>> {
    let mut out: Vec<(String, Vec<String>)> = Vec::new();
    let mut files = Vec::new();
    if path.is_dir() {
        let mut entries: Vec<PathBuf> = std::fs::read_dir(path)
            .map_err(|e| EngineError::Io {
                path: path.display().to_string(),
                source: e,
            })?
            .filter_map(|e| e.ok().map(|e| e.path()))
            .filter(|p| p.is_file())
            .collect();
        entries.sort();
        files = entries;
    } else if path.is_file() {
        files.push(path.to_path_buf());
    } else {
        return Ok(out);
    }
    for file in files {
        let text = read_to_string(&file)?;
        for line in text.lines() {
            let line = strip_comment(line).trim();
            if line.is_empty() {
                continue;
            }
            let mut parts = line.split_whitespace();
            if let Some(atom) = parts.next() {
                let flags: Vec<String> = parts.map(str::to_string).collect();
                out.push((atom.to_string(), flags));
            }
        }
    }
    Ok(out)
}

/// Liest `package.use` (Datei oder Verzeichnis): `atom flag1 -flag2`.
pub fn read_package_use(path: &Path) -> EngineResult<HashMap<String, Vec<String>>> {
    let mut out: HashMap<String, Vec<String>> = HashMap::new();
    for (atom, flags) in read_package_use_lines(path)? {
        out.entry(atom).or_default().extend(flags);
    }
    Ok(out)
}
```
2. In `src/02_profile.rs`:
   - Feld in `Profile` ergänzen:
     ```rust
     /// `package.use`-Regeln in Datei-/Zeilenreihenfolge (hat Vorrang vor `package_use`-Map).
     pub package_use_lines: Vec<(String, Vec<String>)>,
     ```
   - `pub use stack::{ ..., read_package_use_lines };` in `src/02_profile.rs` exportieren.
   - In `sync_pattern_caches(&mut self)` den `package_use_index`-Block ersetzen durch:
     ```rust
     let rules_source: Vec<(String, Vec<String>)> = if !self.package_use_lines.is_empty() {
         self.package_use_lines.clone()
     } else {
         let mut atoms: Vec<String> = self.package_use.keys().cloned().collect();
         atoms.sort();
         atoms
             .into_iter()
             .map(|a| (a.clone(), self.package_use[&a].clone()))
             .collect()
     };
     let mut index: HashMap<String, Vec<UseRule>> = HashMap::new();
     for (atom, tokens) in rules_source {
         crate::mod_00_perf::perf_count_dep_parse();
         let (key, dep) = match Dep::parse(&atom) {
             Ok(dep) => (dep.cpn.to_string(), Some(dep)),
             Err(_) => (atom, None),
         };
         index.entry(key).or_default().push(UseRule { dep, tokens });
     }
     self.package_use_index = index;
     ```
   - `effective_defaults` in `effective_defaults_inner(..., include_package_use: bool)` auslagern und ergänzen:
     ```rust
     /// Profil-Defaults ohne `package.use` (fuer Diff-Berechnung beim Export).
     pub fn effective_defaults_without_package_use(
         &self,
         iuse_defaults: &HashMap<String, bool>,
     ) -> HashSet<String> {
         self.effective_defaults_inner(iuse_defaults, "", "", "", false)
     }
     ```
3. In `src/11_cli/context.rs` (`load_profile`):
   ```rust
   profile.package_use_lines =
       mod_02_profile::read_package_use_lines(&args.eff_package_use()).unwrap_or_default();
   profile.package_use.clear();
   for (atom, flags) in &profile.package_use_lines {
       profile
           .package_use
           .entry(atom.clone())
           .or_default()
           .extend(flags.iter().cloned());
   }
   ```

---

### Task 2: `shared_removal_impact` + `--deep` entfernen, `shared_flag_deps` & `CONFLICT`-Fix (`src/08_delta.rs`, `src/08_delta/shared.rs`, `src/11_cli/args.rs`, `src/11_cli/commands.rs`)

**Problem:**
1. `shared_removal_impact` („Voll-Ersparnis“) ist schwer lesbar, führt bei z. B. `pipewire[extra]` zu `firefox-bin`-Löschvorschlägen und kostet 3 unnötige Simulationen pro Flag.
2. `noop_delta` und `build_delta` markieren fälschlich fremde `before.conflicts` als `CONFLICT` auf unbeteiligten Flags.

**Änderungen:**
1. In `src/08_delta/shared.rs`:
   - Entferne `SharedRemovalImpact`, `shared_removal_impact` und die 3 dazugehörigen Unit-Tests (`diamond_names_flag_and_world_blockers`, `single_holder_returns_none`, `transitive_leaf_bug_absent`). Behalte `negated_flag_wants_on` und füge einen direkten `cut_package`-Test auf dem Diamond hinzu.
   - Ergänze folgenden reinen 0-ms-Graph-Lookup (ohne Simulation):
     ```rust
     /// Direkte/transitive Abhaengigkeiten, die unter `flag?` auf `package` stehen
     /// und im Graphen auch von mindestens einem anderen Paket gehalten werden.
     pub fn shared_flag_deps(
         engine: &Engine<'_>,
         graph: &ResolvedGraph,
         package: &str,
         flag: &str,
     ) -> Vec<(String, Vec<String>)> {
         let Some(node) = graph.nodes.get(package) else {
             return Vec::new();
         };
         let Some(pkg) = engine.index.select_cpv(package, &node.cpv) else {
             return Vec::new();
         };
         let active = &node.use_flags;
         let mut targets = BTreeSet::new();
         collect_conditional_targets(pkg.entry.metadata.rdepend.list(), active, flag, false, &mut targets);
         collect_conditional_targets(pkg.entry.metadata.pdepend.list(), active, flag, false, &mut targets);

         let mut out = Vec::new();
         for dep_cpn in targets {
             if dep_cpn == package || !graph.nodes.contains_key(&dep_cpn) {
                 continue;
             }
             if let Some(parents) = graph.reverse.get(&dep_cpn) {
                 let mut others: Vec<String> = parents
                     .iter()
                     .filter(|(p, _)| p != package)
                     .map(|(p, _)| p.clone())
                     .collect();
                 others.sort();
                 others.dedup();
                 if !others.is_empty() {
                     out.push((dep_cpn, others));
                 }
             }
         }
         out
     }

     fn collect_conditional_targets(
         entries: &[portage_atom::DepEntry],
         active: &HashSet<String>,
         target_flag: &str,
         inside_target: bool,
         out: &mut BTreeSet<String>,
     ) {
         for entry in entries {
             match entry {
                 portage_atom::DepEntry::Atom(dep) => {
                     if inside_target && dep.blocker.is_none() {
                         out.insert(dep.cpn.to_string());
                     }
                 }
                 portage_atom::DepEntry::UseConditional { flag, negate, children } => {
                     if active.contains(flag.as_str()) != *negate {
                         let now_inside = inside_target || flag.as_str() == target_flag;
                         collect_conditional_targets(children, active, target_flag, now_inside, out);
                     }
                 }
                 portage_atom::DepEntry::AllOf(c)
                 | portage_atom::DepEntry::AnyOf(c)
                 | portage_atom::DepEntry::ExactlyOneOf(c)
                 | portage_atom::DepEntry::AtMostOneOf(c) => {
                     collect_conditional_targets(c, active, target_flag, inside_target, out);
                 }
             }
         }
     }
     ```
2. In `src/08_delta.rs`:
   - `pub use shared::{CutPlan, FlagBlocker, RemovalSavings, cut_package, shared_flag_deps};`
   - In `noop_delta`: ändere `conflict: before.conflicts.first().cloned(),` zu:
     ```rust
     conflict: None,
     ```
   - In `build_delta`: ändere `conflict: outcome.conflicts.into_iter().next(),` zu:
     ```rust
     let conflict = outcome.conflicts.into_iter().find(|c| {
         !before
             .conflicts
             .iter()
             .any(|b| b.package == c.package && b.message == c.message)
     });
     ```
     und übergebe `conflict` an `FlagDelta`.
3. In `src/11_cli/args.rs`: entferne `pub deep: bool` aus `DeltasArgs`.
4. In `src/11_cli/commands.rs`: entferne `shared_removal_impact` aus Imports, den `if args.deep`-Aufruf und die komplette Funktion `cmd_deltas_deep`.

---

### Task 3: `AppState` Fixes (`src/12_tui_state.rs` & `src/12_tui_state/actions.rs`)

**Alle zu behebenden Fehler:**
1. Doppelte Pakete in `world_entries` (`gentoo-sources` 2×).
2. Selektions-Sprung bei `refresh()` / `cycle_left_sort()` (wenn `tlp` nach Toggle umsortiert wird).
3. `*ungespeichert*` nicht gelöscht + verlorene `package.use`-Flags bei `export_package_use`.
4. Entfernen von `shared_impact`/`ensure_shared_impact`; neu: `right_targets`, `right_target_selected`, `cut_cache`, `cut_loading`, `viewport_height`, `middle_scroll`.
5. Clamping statt `rem_euclid` in `move_selection`, `page_step()`, `move_to_edge()`, sowie Auto-Cut beim Navigieren in `Pane::Right`.

**Änderungen in `src/12_tui_state.rs`:**
1. Imports anpassen: `SharedRemovalImpact` entfernen, `CutPlan` behalten.
2. Felder in `AppState`:
   - Entferne `shared_impact`, `shared_impact_key`.
   - Ergänze:
     ```rust
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
     ```
3. `refresh(&mut self)`:
   ```rust
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
   ```
4. `rebuild_left` & `rebuild_left_preserving` & `cycle_left_sort` & `apply_filter_only`:
   ```rust
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
       // ... Rest wie bisher (or_entries, transitive, sort, filter) ...
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
   ```
   In `apply_filter_only(&mut self)`:
   ```rust
   pub fn apply_filter_only(&mut self) {
       let prev = self.selected_package();
       self.rebuild_left();
       if self.selected_package() != prev {
           self.select_left_fast(self.left_selected);
           return;
       }
       self.middle_selected = self
           .middle_selected
           .min(self.middle_len().saturating_sub(1));
   }
   ```
5. `rebuild_middle`: am Ende `self.middle_scroll.set(0);` wenn `self.selected_package() != prev_pkg`, und `self.rebuild_right_targets();` aufrufen.
6. `rebuild_right_targets` und `selected_right_target` ergänzen:
   ```rust
   pub fn selected_right_target(&self) -> Option<&str> {
       self.right_target_selected
           .and_then(|i| self.right_targets.get(i))
           .map(String::as_str)
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
   ```
7. `ensure_deltas`:
   ```rust
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
   ```
   In `compute_cut_plan`:
   ```rust
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
   ```
8. `export_package_use(&mut self, path: &std::path::Path)`:
   ```rust
   pub fn export_package_use(&mut self, path: &std::path::Path) -> EngineResult<()> {
       let mut cpns: Vec<&String> = self.engine.overrides.keys().collect();
       cpns.sort();
       let mut lines = Vec::new();
       for cpn in cpns {
           let pkg = self
               .graph
               .nodes
               .get(*cpn)
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
                       flag.clone()
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
   ```

**Änderungen in `src/12_tui_state/actions.rs`:**
1. Helper `move_clamped(cur: usize, delta: i32, len: usize) -> usize` und `pub fn page_step(&self) -> i32`:
   ```rust
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
   ```
2. `cycle_pane`: wenn auf `Pane::Right` gewechselt wird und `!self.right_targets.is_empty()`:
   ```rust
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
   ```
3. `move_selection(&mut self, delta: i32)`:
   - Tab `BigLevers`: `self.lever_selected = move_clamped(self.lever_selected, delta, self.levers.len());` + `self.lever_table_state.select(Some(self.lever_selected));`
   - `Pane::Left`: `let next = move_clamped(self.left_selected, delta, self.left.len());` -> falls `next != self.left_selected` (oder `flag_deltas.is_empty()`), `self.select_left_fast(next);`
   - `Pane::Middle`: `self.middle_selected = move_clamped(self.middle_selected, delta, self.middle_len());` -> `self.rebuild_right_targets(); self.cut_plan = None; self.right_scroll = 0;`
   - `Pane::Right`:
     ```rust
     if !self.right_targets.is_empty() && delta.abs() == 1 {
         let cur = self.right_target_selected.unwrap_or(0);
         let next = move_clamped(cur, delta, self.right_targets.len());
         self.right_target_selected = Some(next);
         self.preview_cut_for_selected_right_target();
     } else if delta < 0 {
         self.right_scroll = self.right_scroll.saturating_sub((-delta) as u16);
     } else {
         self.right_scroll = self.right_scroll.saturating_add(delta as u16);
     }
     ```
4. `pub fn move_to_edge(&mut self, to_end: bool)` für `Home`/`End`:
   - `BigLevers`: `0` oder `len - 1`
   - `Pane::Left`: `select_left_fast(target_idx)`
   - `Pane::Middle`: `middle_selected = target_idx; rebuild_right_targets(); cut_plan = None; right_scroll = 0;`
   - `Pane::Right`: bei `!right_targets.is_empty()`: `right_target_selected = Some(target_idx); preview_cut_for_selected_right_target();`, sonst `right_scroll = 0` / `u16::MAX / 2`.
5. In `select_left_fast`: zusätzlich `self.middle_scroll.set(0); self.cut_plan = None; self.rebuild_right_targets();` (bzw. bei Cache-Hit sofort `self.rebuild_right_targets()`).
6. In `move_modal`: auch `move_clamped` nutzen statt `rem_euclid` (oder Clamping).

---

### Task 4: TUI-Runner: Filter-Navigation, `PageUp`/`PageDown`/`Home`/`End`, `Space` auf `Left` (`src/14_tui_runner.rs`)

In `dispatch_key`:
1. Filter-Modus (`if state.filtering`):
   ```rust
   if state.filtering {
       match key.code {
           KeyCode::Esc => {
               state.filtering = false;
               state.filter.clear();
               state.apply_filter_only();
           }
           KeyCode::Enter => {
               state.filtering = false;
           }
           KeyCode::Tab => {
               state.filtering = false;
               state.pane = Pane::Middle;
               state.ensure_deltas()?;
           }
           KeyCode::Up => {
               state.pane = Pane::Left;
               state.move_selection(-1);
           }
           KeyCode::Down => {
               state.pane = Pane::Left;
               state.move_selection(1);
           }
           KeyCode::PageUp => {
               state.pane = Pane::Left;
               let step = state.page_step();
               state.move_selection(-step);
           }
           KeyCode::PageDown => {
               state.pane = Pane::Left;
               let step = state.page_step();
               state.move_selection(step);
           }
           KeyCode::Home => {
               state.pane = Pane::Left;
               state.move_to_edge(false);
           }
           KeyCode::End => {
               state.pane = Pane::Left;
               state.move_to_edge(true);
           }
           KeyCode::Backspace => {
               state.filter.pop();
               state.apply_filter_only();
           }
           KeyCode::Char(c) => {
               state.filter.push(c);
               state.apply_filter_only();
           }
           _ => {}
       }
       return Ok(LoopAction::Continue);
   }
   ```
2. Normaler Modus:
   - `KeyCode::PageUp`: `let step = state.page_step(); state.move_selection(-step);`
   - `KeyCode::PageDown`: `let step = state.page_step(); state.move_selection(step);`
   - `KeyCode::Home`: `state.move_to_edge(false);`
   - `KeyCode::End`: `state.move_to_edge(true);`
   - `KeyCode::Char(' ')`:
     ```rust
     if state.tab == Tab::BigLevers {
         state.toggle_big_lever()?;
     } else if state.pane == Pane::Left {
         state.pane = Pane::Middle;
         state.ensure_deltas()?;
     } else if state.pane == Pane::Middle {
         state.toggle_selected_flag()?;
     }
     ```
3. In `event_loop`: vor `terminal.draw(...)`:
   ```rust
   if let Ok(size) = terminal.size() {
       state.viewport_height = size.height;
   }
   ```

---

### Task 5: View-Updates: ruhiges Scrollen Mitte, konsistente Farben Rechts, Auto-Cut-Split (`src/13_tui_view/middle.rs`, `src/13_tui_view/right.rs`, `src/13_tui_view/bottom.rs`)

1. **`src/13_tui_view/middle.rs`:**
   Ersetze die harte `offset`-Berechnung durch das Viewport-erhaltende Scrollen mit `state.middle_scroll`:
   ```rust
   let inner_height = area.height.saturating_sub(2) as usize;
   let header = 2 + usize::from(state.deltas_loading);
   let selected_line = state.middle_selected + header;
   let mut offset = state.middle_scroll.get() as usize;
   if inner_height > 0 {
       if selected_line < offset {
           offset = selected_line;
       } else if selected_line >= offset + inner_height {
           offset = selected_line - inner_height + 1;
       }
   } else {
       offset = 0;
   }
   state.middle_scroll.set(offset as u16);
   ```
2. **`src/13_tui_view/right.rs`:**
   - Entferne `render_shared_impact` und `SharedRemovalImpact`.
   - Korrigiere die Farben bei `bewirkt:`:
     - `added_packages` (`+ {p}`): `Color::Red` (Mehrkosten)
     - `removed_packages` (`- {p}`): `Color::Green` (Ersparnis)
   - Zeige unter `bewirkt:` bei Bedarf `Geteilte Abhaengigkeiten:` über `crate::mod_08_delta::shared_flag_deps(&state.engine, &state.graph, &pkg, &delta.flag)`.
   - Hebe in `Pane::Right` das aktuell selektierte Zielpaket aus `state.selected_right_target()` visuell hervor (Prefix `> ` + `Modifier::BOLD` / `Color::DarkGray` auf der Paketzeile) und passe `right_scroll` automatisch so an, dass die markierte Zeile im oberen Viewport sichtbar bleibt.
   - Teile `area` vertikal auf, wenn `state.cut_plan.is_some()` (z. B. oben `Constraint::Min(6)` mit Blame/Bewirkt/Geteilt/Doku, unten `Constraint::Length(cut_height)` als eigene Box `"Cut-Analyse"`), und entferne die alte Bedingung `if plan.target != pkg { return; }`, damit der Auto-Cut auch für jedes in `Pane::Right` angesteuerte Paket sichtbar ist!
3. **`src/13_tui_view/bottom.rs`:**
   - Hilfe-Modal (`render_help_modal`): Zeile `Voll-Ersparnis: ...` entfernen und durch `Rechte Pane: j/k waehlt Pakete + Auto-Cut [x]; PgUp/PgDn blaettert` ersetzen.

---

## Phase 2: Pflicht-Tests (lokal im Repo)

Alle Tests müssen ohne Fehler durchlaufen (`cargo test` und `cargo test --release`):

1. **Bestehende Tests anpassen:**
   - `src/12_tui_state/tests.rs`: In `package_use_export_writes_overrides` zusätzlich `assert!(!st.dirty);` prüfen.
   - `src/13_tui_view/tests.rs`: `renders_shared_impact_and_flag_docs` aktualisieren auf Flag-Doku + `Geteilte Abhaengigkeiten` (ohne `Voll-Ersparnis`).
   - `tests/tui_screencaps.rs`: `03_emacs_shared_blocker` auf die neuen Labels (`Geteilte Abhaengigkeiten`, Tab in `Pane::Right` -> `Cut-Analyse`) aktualisieren.

2. **Neue Regressionstests (alle in `tests/tui_screencaps.rs` und Unit-Tests ergänzen):**
   - `sort_toggle_keeps_selection_on_same_package`: Sortierung `o` (`ExclusivePackages`), Flag toggeln sodass das Paket in der Sortierung nach unten fällt -> Cursor bleibt auf demselben Paket!
   - `duplicate_world_atoms_are_deduplicated`: `world` enthält `sys-kernel/gentoo-sources:1.0` und `sys-kernel/gentoo-sources:2.0` -> erscheint in `state.left` exakt einmal!
   - `export_merges_valid_package_use_cleans_dead_and_clears_dirty`: Paket hat `alsa dbus sound-server udev` in `package.use` (`dbus` vom Profil an, `alsa`/`udev` tot), TUI schaltet `extra` an -> `[S]` schreibt exakt `extra sound-server`, setzt `dirty == false`, nächster Toggle löscht `package.use geschrieben:`-Status!
   - `filter_allows_navigation_tab_and_esc_clear`: Während `filtering == true` bewegen `Down`/`Up`/`PageDown` die linke Selektion, `Tab` springt direkt in `Pane::Middle`, `Esc` im Filter-Modus leert den Filter komplett.
   - `space_on_left_focuses_middle_without_toggling`: `Space` in `Pane::Left` wechselt zu `Pane::Middle`, toggelt aber nichts.
   - `page_up_down_clamps_without_wrap`: `PageDown` am Ende bleibt am letzten Element, `PageUp` am Anfang bleibt bei `0`.
   - `right_pane_hover_updates_cut_plan_automatically`: `Tab` in `Pane::Right` + `Down`/`Idle` aktualisiert `cut_plan` automatisch für das fokussierte Paket.
   - `unrelated_conflict_does_not_poison_other_flags`: Wenn Paket A einen Konflikt hat, zeigen No-Op-/normale Flags auf Paket B **kein** `CONFLICT`.
   - `package_use_file_order_precedence`: `>=app/foo-2.0 -ssl` gefolgt von `app/foo ssl` vs. umgekehrt befolgt Datei-/Zeilenreihenfolge (`>` ASCII-Problem behoben).

---

## Phase 3: System-Tests auf dem echten Gentoo-Host via SSH

Führe erst nach erfolgreichem `cargo test --release` die Echtdaten-Prüfung auf dem Gentoo-Host aus:

1. Host-IP dynamisch im Container ermitteln (`ip a` / `ip route`), Key `cl-rust-generator/id_lenovo_to_hpz6`, User `kiel`. **IP nie in Dateien schreiben.**
2. Release-Binary bauen (`cargo build --release`) und auf den Host kopieren (oder dort bauen, falls Repo gemountet/synchronisiert).
3. Auf dem Host gegen das echte System (`/var/db/repos/gentoo`, `/var/lib/portage/world`, `/etc/portage`, `/tmp/Packages`) testen:
   - `gentoo-dep-tui deltas media-video/ffmpeg ...` (läuft sauber ohne `--deep`).
   - `gentoo-dep-tui graph --json ...` und `gentoo-dep-tui cut x11-libs/gtk+ --json ...` (valides JSON, Exit 0).
   - `cargo test --release --test tui_screencaps --test hil_regression` (oder HIL-Snapshot-Lauf gegen `/var/db/repos/gentoo` + echte `world` + `/etc/portage`), um die erzeugten Screencaps in `target/screencaps/hil_screencaps.md` auf Plausibilität zu prüfen (keine doppelten `gentoo-sources`, kein `Voll-Ersparnis`-Rauschen bei `pipewire[extra]`, funktionierender Auto-Cut in rechter Pane).
4. Berichte am Ende die exakten Testergebnisse (Unit/Screencap-Tests + SSH-Systemtest-Befunde) ohne Platzhalter.
````

---

Falls du vor dem Absenden an den Agenten noch eine Kleinigkeit am Verhalten anpassen möchtest (z. B. ob `cut` in `Pane::Right` eine einzelne Zeile pro Paket oder direkt Blame+Bewirkt-Einträge ansteuern soll), sag einfach kurz Bescheid — ansonsten kannst du den Block direkt so ausführen lassen.
