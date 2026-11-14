Below is a concrete `plan.md` and `tasks.md` based on a full review of `gentoo-dep-tui` (`src/01_types.rs`–`src/14_tui_runner.rs`, `tests/*`, and `walkthrough.md`).

---

# `plan.md`

## 1. Review Summary & Core Issues

The codebase has a solid modular pipeline (`types → profile → manifest → binhost → repo_index → graph → cascade → delta → plan → export/verify → cli → tui`), strong test coverage, and good initial `rayon` parallelization. However, the review uncovered **critical correctness bugs**, **Portage semantic mismatches**, **TUI UX glitches**, **dead/duplicated code**, and **structural bloat (>600 LOC files)** that need targeted fixes:

### A. Critical Correctness & Portage Semantics
1. **Cascade Pollution of User Overrides (`07_cascade.rs`, `12_tui_state.rs`, `08_delta.rs`)**:
   - `AppState::refresh()` passes `&mut self.engine` directly into `resolve_with_cascade()`. Cascade-forced flags are written into `engine.overrides`. On the next `refresh()`, `07_cascade.rs` snapshots `engine.overrides` as `user` intent—permanently locking cascade side-effects as user choices and exporting them to `plan.toml`/`package.use`.
   - Conversely, in CLI (`08_delta.rs`), `engine` is *not* mutated by cascade before `package_deltas`, so `current_flags(engine, package)` disagrees with `before.graph.nodes[package].use_flags`, causing cascade-enabled flags to render as `[ ]` and produce bogus `0 B` deltas when toggled.
2. **Version & Slot Loss in Graph/Cascade/Delta (`05_repo_index.rs`, `06_graph.rs`, `07_cascade.rs`, `08_delta.rs`)**:
   - `descend_runtime`, `descend_any_of`, and `pick_first` match a `Dep` via `select_for_dep(dep)`, then push `(pkg.cpn, pkg.cpn, None)` (or `owner`) onto `queue`, discarding the version/slot constraint. `resolve_target()` falls back to `engine.index.select(cpn)` (always newest version overall).
   - `resolve_build_extra`, `07_cascade.rs`, `08_delta.rs`, and `12_tui_state.rs` also call `engine.index.select(cpn)` instead of looking up the exact `cpv` stored in `ResolvedNode.cpv`.
   - `PDEPEND` is documented in `01_types.rs` as part of runtime (`RDEPEND (+ PDEPEND)`) but completely ignored in `06_graph.rs`.
3. **Profile & `make.conf` Precedence/Parsing (`02_profile.rs`, `05_repo_index.rs`, `11_cli.rs`, `tests/hil_regression.rs`)**:
   - `Engine::effective_use` runs `package_use_for` *after* `apply_use_defaults`, allowing `package.use` (including exact `cat/pkg` keys applied twice) to override `use.mask` and `use.force`.
   - `parse_key_value` fails on multiline quoted assignments (`USE="\n foo\n bar\n"`) and `\` continuations.
   - `11_cli.rs::load_profile` overwrites profile `make.defaults` `USE` if `${USE}` is absent from `make.conf`, and stores `-flag` inside a `HashSet` alongside `flag` (non-deterministic iteration order).
   - `11_cli.rs::load_profile` only updates `expand_defaults` keys that already had defaults, ignoring `USE_EXPAND` variables set only in `make.conf` (e.g. `VIDEO_CARDS`, `L10N`).
   - `11_cli.rs::resolve_make_profile` fails on relative symlinks (`../../var/db/repos/gentoo/profiles/...`) because `..` segments are not normalized before prefix comparison.
   - `05_repo_index.rs::keywords_accepted` returns `true` when `entry.metadata.keywords.is_empty()`, accepting unkeyworded ebuilds (`KEYWORDS=""`).
4. **Cascade `REQUIRED_USE` & `QuickSwitch` Gaps (`07_cascade.rs`)**:
   - `enforce_required_use` calls `force_flag` without checking `user_value`, silently overwriting explicit user flags.
   - `RequiredUseExpr::AnyOf` picks the first `enablable_flag` without checking `is_masked` or `user_value == Some(false)`, and ignores negated options.
   - `resolve_with_cascade_opts` accumulates transient conflicts from intermediate rounds instead of reporting only the final unresolved conflicts.
   - `enforce_use_deps` ignores target `IUSE` membership and `[flag(+)]` / `[flag(-)]` defaults, injecting nonexistent flags into `overrides`.
   - Multiple `QuickSwitch` options (`AnyOf`, `ExactlyOne`, `AtMostOne`, `"{parent}-Constraint lockern"`) have `changes: vec![]`, making them no-ops in the TUI.
5. **Graph, Delta & Plan Edge Cases (`06_graph.rs`, `08_delta.rs`, `09_plan_toml.rs`, `11_cli.rs`)**:
   - `download_bytes` double-counts distfiles when `SRC_URI` lists multiple mirrors for the same filename.
   - `collect_via_flags` loses nested conditional flags (`foo? ( bar? ( dep ) )`), is order-dependent when a dep appears both conditionally and unconditionally, and drops `!` negation in blame.
   - `ResolvedGraph::blame` returns `[]` if a `world` package is part of a dependency cycle (never reaches `reverse == None`).
   - `big_levers` does not filter out platform flags (`amd64`, `elibc_*`, `kernel_*`), `use.mask`, or `use.force`.
   - `Plan::package_overrides` applies `suggested_use` after `plan.package`, overwriting saved user overrides.
   - `cmd_blame` calls `resolve()` instead of `resolve_with_cascade()`.

### B. TUI & Export/Verify Issues
1. **Export vs. Verify Root Layout (`10_export_verify.rs`, `11_cli.rs`, `14_tui_runner.rs`)**:
   - `export_plan` writes `out_dir/world` and `out_dir/package.use`, whereas `VerifyRunner::against_configroot` executes `emerge --root out_dir --config-root out_dir @world`, which expects Portage's `var/lib/portage/world` and `etc/portage/package.use` (+ `make.profile`) layout.
2. **TUI State & View Glitches (`12_tui_state.rs`, `13_tui_view.rs`, `14_tui_runner.rs`)**:
   - `toggle_selected_flag` on a `CONFLICT`-marked flag calls `open_conflict_for_package` on *current* `self.conflicts`, which is empty/mismatched for pre-toggle simulated conflicts—so `Space` silently does nothing.
   - `rebuild_middle` resets `middle_selected = 0` on every `refresh()`, losing cursor position when toggling a flag in a long list.
   - `14_tui_runner.rs` calls `state.ensure_deltas()?` synchronously on every `Up`/`Down`/`j`/`k` keypress, defeating T05's idle-poll recomputation.
   - `render_right` omits `.border_style(border_style(state.pane == Pane::Right))`; `render_middle` uses hardcoded `middle_selected + 2` scroll offset ignoring `deltas_loading` and `middle_scroll`; `render_footer` lacks a live `/` filter indicator and clips on 80-col terminals.
   - `run_tui` errors if `--world` does not exist on disk even when `--plan` supplies `baseline.world`.

### C. Cleanup, Dead Code & Module Structure
1. **Dead/Unused Items**:
   - `01_types.rs`: `PkgId`, `FlagId`, `IdTable`, `UseState`, `PkgKey`, `DepClass` are unused (either leverage `PkgId`/`FlagId` in a follow-up optimization pass or remove truly unused types like `UseState`/`PkgKey`/`DepClass` until wired in).
   - `05_repo_index.rs`: `PkgVersion.masked` and `PkgVersion.live` are always `false`; `parse_cache_file` conflates filtering with `Err` and swallows real I/O/parse errors via `.ok()`.
   - `06_graph.rs`: `or_counter` is incremented and never read.
   - `11_cli.rs`: redundant leaf `use.mask`/`use.force` load after `load_profile_stack`; `Box::leak` in `load_engine`.
   - Duplicate `version_matches`/`slot_matches` logic in `02_profile.rs` and `05_repo_index.rs`.
   - Duplicate `export_portage` (`10_export_verify.rs`) vs `export_plan` (`11_cli.rs`).
   - Duplicate `pkg(...)` test helpers across 6 files.
2. **File Size Split (>600 LOC)**:
   - Split `12_tui_state.rs` (~809 lines), `06_graph.rs` (~663 lines), and `07_cascade.rs` (~656 lines) into cohesive submodules (keeping `src/lib.rs` public module paths intact via `mod` submodules or helper files so external tests keep compiling without breakage).

---

## 2. Execution Phases & Ordering

1. **Phase 1 — Profile, RepoIndex & Shared Atom Matching (`02`, `05`, `11`)**: Fix config parsing, keyword/mask/force precedence, error propagation, and symlink normalization first, because all downstream graph/cascade behavior depends on accurate `Profile` and `RepoIndex` state.
2. **Phase 2 — Graph Resolution & Cascade Separation (`06`, `07`, `09`)**: Separate `user_overrides` from cascaded overrides, propagate exact matched `PkgVersion`/`cpv` (plus `PDEPEND`, distfile dedup, blame cycle/via fixes), and fix `REQUIRED_USE` / `IUSE` defaults / `QuickSwitch` generation.
3. **Phase 3 — Delta Engine & CLI (`08`, `10`, `11`)**: Align `08_delta.rs` with post-cascade `ResolvedNode` state, filter non-toggleable flags from `big_levers`, unify `export_portage`/`export_plan` with `VerifyRunner` layout, and fix `cmd_blame`/`--plan` handling.
4. **Phase 4 — TUI State, View & Event Loop (`12`, `13`, `14`)**: Fix `user_overrides` integration, conflict modal opening, cursor preservation, idle delta loading, right-pane border/scroll, and filter prompt.
5. **Phase 5 — Structural Cleanup & Perf Pass (`01`, `05`–`08`, `12`, `tests`)**: Remove dead code, consolidate test fixtures, split >600-line files into clean submodules, and verify all unit/integration/perf tests pass.

---

# `tasks.md`

Execute tasks in order. After each phase, run `cargo test` and `cargo clippy --all-targets -- -D warnings`.

---

## Phase 1: Profile, RepoIndex & Atom Matching

### Task 1.1: Unify PMS Version/Slot Matching Helper
- **Files**: `src/02_profile.rs`, `src/05_repo_index.rs`
- **Instructions**:
  1. In `src/02_profile.rs`, extract `pub(crate) fn dep_matches_version_and_slot(dep: &portage_atom::Dep, version: &portage_atom::Version, version_str: &str, slot: &str) -> bool` (sharing `version_matches`, `slot_matches`, `strip_revision`).
  2. Check PMS glob semantics (`Op::Equal` with `dep.glob`): verify against `portage_atom` (or ensure prefix glob `=cat/pkg-1.0*` compares against `version_str` without matching e.g. `10` for `1*`—actually in PMS 8.3.1 `=*` is pure string prefix on the version string, e.g. `=1*` matches `10`; keep exact PMS behavior and add a clarifying unit test for `=1.8*` vs `1.8.3` and `1.8.0-r1`).
  3. Update `02_profile.rs::Profile::package_use_for`, `05_repo_index.rs::dep_matches`, and `05_repo_index.rs::mask_dep_matches` to call this shared helper instead of duplicating `version_matches` / `slot_matches`.
- **Acceptance**: Zero duplicate version/slot matching functions; existing unit tests pass.

### Task 1.2: Fix Multiline `make.conf`/`make.defaults`, Inline Comments & `USE_EXPAND` Tracking
- **Files**: `src/02_profile.rs`, `src/11_cli.rs`, `tests/hil_regression.rs`
- **Instructions**:
  1. In `02_profile.rs::parse_key_value`, support multiline quoted assignments (`KEY="...\n..."` and `KEY='...\n...'`) and trailing `\` line continuations before splitting `KEY=VALUE`. Strip `#` comments outside quotes on each line.
  2. In `02_profile.rs::apply_flag_list`, `extend_atom_lines`, and `read_world`, strip inline `#` comments (`strip_comment(line).trim()`) before parsing tokens.
  3. Add `pub use_expand: HashSet<String>` to `Profile` in `02_profile.rs`, populated in `load_profile_stack` from `USE_EXPAND` lines in `make.defaults`. Also accumulate `USE` across `make.defaults` even without `${USE}` (Portage incremental behavior: replace `"${USE}"` with `""` and apply tokens sequentially via `apply_use_token_set`, handling `-*` to clear prior tokens).
  4. Add `pub fn apply_use_tokens(&mut self, tokens: &str)` on `Profile` that processes `-*` (clear `global_use`) and `-flag`/`flag` via `apply_use_token_set`, so `global_use` never contains both `"foo"` and `"-foo"` simultaneously.
  5. Update `11_cli.rs::load_profile` and `tests/hil_regression.rs::hil_full_setup`:
     - When `make.conf` defines `USE`, call `profile.apply_use_tokens(use_line)` (expanding `${USE}` to `""` or incremental application) instead of overwriting `profile.global_use` with `parse_use_tokens`.
     - Iterate over `&profile.use_expand` (not just `profile.expand_defaults.keys()`) when reading `make.conf` `USE_EXPAND` variables.
     - Remove the redundant `read_flag_list(.../use.mask)` and `.../use.force` calls at the end of `11_cli.rs::load_profile` (`load_profile_stack` already loads the entire stack).
- **Acceptance**: Unit tests for multiline `USE="\n a\n -b\n"`, `-flag` overriding parent `flag` deterministically, `make.conf` preserving profile `USE`, and `VIDEO_CARDS` in `make.conf` without default in `make.defaults`.

### Task 1.3: Fix `package.use` vs `use.mask`/`use.force` Precedence
- **Files**: `src/02_profile.rs`, `src/06_graph.rs`
- **Instructions**:
  1. Merge `package_use_for(cpn, version_str, slot)` into `Profile::effective_defaults(&self, iuse_defaults: &HashMap<String, bool>, cpn: &str, version_str: &str, slot: &str) -> HashSet<String>` (keep `apply_use_defaults` delegating to it with empty version/slot or update all callers) so the exact Portage order is strictly enforced:
     1) IUSE defaults (`+flag`)
     2) `ARCH`, `ELIBC`, `KERNEL`
     3) `USE_EXPAND` defaults (for flags in `iuse_defaults`)
     4) `global_use`
     5) `package_use_for(cpn, version_str, slot)` (both exact and versioned/slotted, in order)
     6) `use_force`
     7) `use_mask` (mask always wins last)
  2. Update `06_graph.rs::Engine::effective_use` to call `self.profile.effective_defaults(...)` and then apply `self.overrides.get(&pkg.cpn)` (still respecting `use_mask`/`use_force` or leaving conflict detection to `07_cascade.rs`).
- **Acceptance**: Unit test proving `package.use` with a masked flag does NOT enable it against `use.mask`, and `package.use` with `-forced_flag` does NOT disable a `use.force` flag.

### Task 1.4: Fix `resolve_make_profile` Relative Symlink Normalization
- **Files**: `src/11_cli.rs`, `src/02_profile.rs`
- **Instructions**:
  1. Make `normalize_join` in `02_profile.rs` `pub(crate)` (and also normalize `profiles_dir` components lexically).
  2. In `11_cli.rs::resolve_make_profile`, normalize `target` via `normalize_join` (or `std::fs::canonicalize` fallback on both `abs` and `profiles_dir`) before comparing path components against `profiles_dir`.
- **Acceptance**: Unit test with a `tempfile` relative symlink `../../repo/profiles/default/linux/amd64/23.0` verifying `resolve_make_profile` extracts `"default/linux/amd64/23.0"`.

### Task 1.5: Fix `RepoIndex::load` Error Propagation, `KEYWORDS=""`, and Dead Fields
- **Files**: `src/05_repo_index.rs`
- **Instructions**:
  1. In `05_repo_index.rs::keywords_accepted`, return `false` when `entry.metadata.keywords.is_empty()` (unkeyworded ebuilds are not accepted by default; also handle `accept_keywords.contains("*")` / `"~*"` if present).
  2. Remove dead fields `masked: bool` and `live: bool` from `PkgVersion` (and update struct literals across `05_repo_index.rs`, `06_graph.rs`, `07_cascade.rs`, `08_delta.rs`, `12_tui_state.rs`, `13_tui_view.rs`).
  3. Change `parse_cache_file` return type to `EngineResult<Option<(String, PkgVersion)>>`: return `Ok(None)` for filtered versions (`live || masked || !keywords_ok`), and log/propagate real I/O or parse errors instead of conflating them with `.ok()`.
  4. Add `pub fn select_cpv(&self, cpn: &str, cpv: &str) -> Option<&PkgVersion>` on `RepoIndex` (falls back to `self.select(cpn)` only if `cpv` is empty).
- **Acceptance**: Unit test verifying an ebuild with `KEYWORDS=` (empty) is filtered out; `cargo test` passes.

---

## Phase 2: Graph Resolution, Blame & Cascade Separation

### Task 2.1: Preserve Exact Selected `PkgVersion` (`cpv`) and Add `PDEPEND` + Distfile Dedup
- **Files**: `src/06_graph.rs`
- **Instructions**:
  1. Change the BFS queue in `06_graph.rs::resolve_with_options` to store `(PkgVersion ref or cpv: String, required_by: String)` directly from `engine.index.select_for_dep(&dep)` so version/slot constraints from `world`, `descend_runtime`, `descend_any_of`, and `pick_first` are never dropped and re-evaluated with `select(cpn)`.
  2. Include `pkg.entry.metadata.pdepend.list()` alongside `pkg.entry.metadata.rdepend.list()` in runtime dependency evaluation (`collect_via_flags` + `DepEntry::evaluate_use` + `descend_runtime`), matching `01_types.rs` documentation.
  3. In `resolve_build_extra`, look up each runtime node's `PkgVersion` via `engine.index.select_cpv(&node.cpn, &node.cpv)` instead of `engine.index.select(cpn)`.
  4. In `download_bytes` / `collect_dist_files`, deduplicate filenames (e.g. `HashSet<String>` or `sort` + `dedup`) so multiple mirror URLs in `SRC_URI` do not double-count file sizes.
  5. Remove unused `or_counter` from `resolve_with_options` and `descend_any_of`.
- **Acceptance**: Unit tests for: (a) transitive versioned dep `<cat/lib-2.0` selecting `1.0` instead of `2.0` in index; (b) `PDEPEND` inclusion in runtime graph; (c) duplicate mirror URLs in `SRC_URI` counted once.

### Task 2.2: Fix `collect_via_flags` & `ResolvedGraph::blame` Cycle/World Handling
- **Files**: `src/06_graph.rs`
- **Instructions**:
  1. In `collect_via_flags`, pass the active flag condition stack down (or keep deepest/most-specific flag, and format negated conditions as `"!flag"`). If a package is reached *both* unconditionally (`None`) and conditionally (`Some(flag)`), prefer `None` (unconditional) deterministically regardless of `RDEPEND` order.
  2. In `ResolvedGraph::blame` / `blame_dfs`: compute `world_cpns: HashSet<String>` from `self.world` (using `world_atom_cpn` or exact match). Treat reaching a `world` node as a valid root *in addition* to `reverse.get(node) == None`, so a cycle between two `world` packages (`a <-> b`) still yields valid blame chains instead of `[]`.
- **Acceptance**: Unit tests for nested `foo? ( bar? ( dep ) )`, unconditional+conditional order independence, and `world` cycle blame returning non-empty chains.

### Task 2.3: Separate User Overrides from Cascade Side-Effects
- **Files**: `src/07_cascade.rs`, `src/08_delta.rs`, `src/12_tui_state.rs`
- **Instructions**:
  1. Add `pub effective_overrides: UseOverrides` (or `pub final_engine_overrides` / `resolved_use`) to `CascadeOutcome` in `07_cascade.rs`, OR change `resolve_with_cascade` to run on `let mut sim = engine.fork();` and store the resulting `effective_overrides` on `CascadeOutcome` **without mutating the caller's `engine.overrides`**!
     - Keep a helper or clear contract: `engine.overrides` holds **only explicit user intents**; `CascadeOutcome.effective_overrides` holds user intents + cascade-forced flags; `CascadeOutcome.graph.nodes[cpn].use_flags` holds the true final active USE flags.
  2. Update `CascadeStep` in `07_cascade.rs` to look up `PkgVersion` via `self.engine.index.select_cpv(&node.cpn, &node.cpv)` (fallback `select`) so `REQUIRED_USE` and `effective_use` match the resolved version.
  3. Update `07_cascade.rs::resolve_with_cascade_opts` so `conflicts` reflects **only the final round's unresolved `step_conflicts`** (not stale conflicts from earlier rounds that converged).
- **Acceptance**: Unit test: user enables `app[svt-av1]` -> cascade forces `aom`; user disables `app[svt-av1]` -> `aom` is no longer in `effective_overrides` or active flags, and `user_overrides` never contains `aom`.

### Task 2.4: Fix `REQUIRED_USE`, `use_deps` `[+]/[-]` Defaults & Broken `QuickSwitch` Options
- **Files**: `src/07_cascade.rs`
- **Instructions**:
  1. In `enforce_required_use`:
     - Check `self.user_value(cpn, flag)` inside `force_flag` (or pass a `respect_user: bool` flag): if the user explicitly set `flag` to `!want_on`, emit a `ConflictInfo` with `QuickSwitch` options (e.g. accept `want_on` OR disable the triggering condition if inside `UseConditional`) instead of silently overwriting `user_value`.
     - For `RequiredUseExpr::AnyOf(children)`: filter candidates skipping `profile.is_masked(f)` and `user_value(cpn, f) == Some(false)` (and support negated `!flag` branches via `offable_flag`). If no non-conflicting candidate exists, populate each `QuickSwitch.changes` with concrete `(cpn, flag, !negated)` changes.
     - For `RequiredUseExpr::ExactlyOne` and `AtMostOne` when `n > 1`: populate `QuickSwitch.changes` for each option by turning off the *other* currently satisfied simple flags (e.g., for `^^ ( a b )` with both on, option `"nur behalten: a"` has `changes: vec![(cpn, "b", false)]`).
     - For negated `Flag { name, negated: true }` conflict, if triggered inside `UseConditional { flag: cond, negated: false, .. }`, also include a `QuickSwitch` to turn `cond` off.
  2. In `enforce_use_deps`:
     - Check `pkg.iuse_defaults.contains_key(&flag)` on the target `PkgVersion` (`select_cpv`).
     - If `flag` is NOT in target's `IUSE`: inspect `ud.default` (`Option<bool>` / `UseDepDefault` in `portage_atom::UseDep`):
       - `[flag(+)]` / `[-flag(+)]`: evaluate as `true` without mutating `overrides`! Emit conflict only if the constraint requires `false`.
       - `[flag(-)]` / `[-flag(-)]`: evaluate as `false` without mutating `overrides`! Emit conflict only if the constraint requires `true`.
       - No default (`None`): emit conflict (`missing from IUSE`) instead of injecting an invalid flag into `overrides`.
  3. In `push_use_conflict`: populate the second `QuickSwitch` option with real `changes` (e.g. toggling the parent's conditional flag if applicable, or omit the second option when hard/unconditional instead of emitting `changes: vec![]`).
- **Acceptance**: Unit tests for: (a) `|| ( masked_flag valid_flag )` picking `valid_flag`; (b) `^^ ( a b )` QuickSwitch having non-empty `changes` that actually resolve the conflict; (c) `dep[missing_flag(+)]` not injecting `missing_flag` into `overrides`.

### Task 2.5: Fix `Plan::package_overrides` Precedence
- **Files**: `src/09_plan_toml.rs`
- **Instructions**:
  1. In `Plan::package_overrides`, apply `choice.option[choice.selected].suggested_use` **first**, and `plan.package` **second**, so user-saved toggles in `[[package]]` always override default `suggested_use`.
- **Acceptance**: Unit test where `choice.option[0].suggested_use = ["ssl"]` and `plan.package` has `use_flags = ["-ssl"]` results in `ssl = false`.

---

## Phase 3: Delta Engine, Export/Verify & CLI

### Task 3.1: Fix `08_delta.rs` Post-Cascade `current` State & `big_levers` Filtering
- **Files**: `src/08_delta.rs`
- **Instructions**:
  1. In `build_delta` and `flag_delta_with_base`, determine `current` from `before.graph.nodes.get(package).map(|n| n.use_flags.contains(flag))` (falling back to `current_flags(engine, package)` only if `package` is absent from the graph). Same in `flag_delta` (`before_outcome`). This ensures cascade-enabled flags show `[x]` and toggle to `false`.
  2. In `package_deltas`, look up the `PkgVersion` via `engine.index.select_cpv(package, &node.cpv)` from `base.graph.nodes.get(package)` so `iuse_defaults` matches the actually resolved version.
  3. In `big_levers`:
     - Filter out `!is_platform_flag(engine.profile, &flag)`, `!engine.profile.is_masked(&flag)`, and `!engine.profile.is_forced(&flag)` from both `flag_packages` and `known_off`.
     - Add `pub fn big_levers_with_base(engine: &Engine<'_>, world: &[String], base: &CascadeOutcome) -> EngineResult<Vec<BigLever>>` so `AppState` can reuse its already-computed `CascadeOutcome` without resolving the baseline a second time.
  4. Pass clean `user_overrides` (from `Engine`) into `flag_delta` / `package_deltas` / `big_levers` (since `resolve_with_cascade_opts` no longer mutates `engine.overrides`, this works automatically!).
- **Acceptance**: Unit tests: (a) `REQUIRED_USE="wayland? ( egl )"` with `wayland` on shows `egl` as `current == true` in `package_deltas`; (b) `big_levers` never includes `amd64`, `elibc_glibc`, `kernel_linux`, masked, or forced flags.

### Task 3.2: Align `export_portage` / `export_plan` with `VerifyRunner` Configroot Layout & CLI Fixes
- **Files**: `src/10_export_verify.rs`, `src/11_cli.rs`
- **Instructions**:
  1. Unify `export_plan` and `export_portage` (move `export_plan` into `10_export_verify.rs` or have it call shared write logic) and ensure `VerifyRunner::against_configroot(out_dir)` matches the exported directory layout:
     - Either write Portage-standard layout (`out_dir/var/lib/portage/world` and `out_dir/etc/portage/package.use`, plus optional `etc/portage/make.profile` symlink if known) **or** keep top-level `world`/`package.use` for human inspection while also writing `var/lib/portage/world` and `etc/portage/package.use` (or updating `VerifyRunner` docs/command). Recommend: write `var/lib/portage/world` & `etc/portage/package.use` (and document/keep top-level compatibility if any test expects `out.join("world")` — check `host_integration.rs` and `hil_regression.rs` which assert `out.join("world").is_file()` and `out.join("package.use").is_file()`! So if `export_plan` writes *both* or uses a helper, keep existing tests green and make `VerifyRunner` actually work with `emerge --root ... --config-root ...`!).
  2. In `11_cli.rs::cmd_blame`, use `resolve_with_cascade(&mut engine, &world)?` instead of `resolve(&engine, &world)?` so cascade-induced dependencies are included in blame output.
  3. In `11_cli.rs::cmd_big_levers`, also print `DL-Delta` column alongside `Bin-Delta` so CLI output is useful without `--binhost`.
  4. In `11_cli.rs::load_engine`, return `Engine` cleanly without `Box::leak` where feasible (e.g., return owned `(RepoIndex, Profile, Vec<String>)` in a `LoadedContext` struct and construct `Engine<'_>` by borrowing it in callers!).
- **Acceptance**: `cargo test` passes; `cmd_blame` shows cascade-triggered deps; zero `Box::leak` in `11_cli.rs` (only `'static` if needed by `ratatui` closures, or borrow from local `LoadedContext` in `run_tui`!).

---

## Phase 4: TUI State, View & Event Loop Fixes

### Task 4.1: Fix `AppState` Cascade Separation, Conflict Toggle, and Cursor Preservation
- **Files**: `src/12_tui_state.rs`
- **Instructions**:
  1. Keep `self.engine.overrides` strictly as **user overrides** (never overwritten by `resolve_with_cascade`!). Store `self.effective_use: HashMap<String, HashSet<String>>` (from `outcome.graph.nodes`) or `self.last_outcome` if needed.
  2. In `rebuild_middle`: remember the currently selected flag name (`let prev_flag = self.flag_deltas.get(self.middle_selected).map(|d| d.flag.clone())`) when staying on the same package, and restore `self.middle_selected` to that flag's new index instead of resetting to `0`. Reset to `0` only when switching to a different left entry.
  3. In `toggle_selected_flag`:
     - Use `self.graph.nodes.get(&package).map(|n| n.use_flags.contains(&flag))` to compute `toggled_on = !current`.
     - If `self.flag_deltas[self.middle_selected].conflict` is `Some(sim_conflict)`: do NOT silently drop the action! Either apply the toggle and open `self.modal = Some(Modal::Conflict { conflict: idx, selected: 0 })` on the matching conflict in `self.conflicts` (prefer matching `c.package == package || c.caused_by.contains(&package)`), or show `sim_conflict` directly via a modal variant `Modal::SimulatedConflict(ConflictInfo)` with options to either apply a `QuickSwitch` or cancel (`Esc`).
  4. In `toggle_big_lever`: skip `is_platform_flag`, `is_masked`, and `is_forced` flags; when turning back on, only re-enable on packages where it isn't masked.
  5. In `apply_quick_switch`: if `option.changes.is_empty()`, set `self.status = "Keine automatische Aenderung verfuegbar".into()` instead of marking `dirty = true`.
- **Acceptance**: Unit tests for: (a) toggling a flag preserves `middle_selected` on the same flag; (b) toggling a trigger flag ON then OFF cleanly removes cascade side-effects from `user_overrides` and `plan.toml`; (c) pressing `Space` on a `CONFLICT` flag opens a modal instead of silently doing nothing.

### Task 4.2: Fix `13_tui_view.rs` Rendering, Focus & Scrolling
- **Files**: `src/13_tui_view.rs`
- **Instructions**:
  1. Add `.border_style(border_style(state.pane == Pane::Right))` to the `Block` in `render_right`.
  2. In `render_right`, if `state.conflicts` contains conflicts involving `selected_package()` (`c.package == pkg || c.caused_by.contains(&pkg)`), render a red `"Konflikte:"` section with `c.message`.
  3. In `render_middle`:
     - Account for the actual header lines (package summary line + blank line + optional `deltas_loading` line) when computing scroll offset, and synchronize `middle_scroll` properly.
     - Render `delta.cascade_note` (when `Some`) or conflict hint cleanly.
  4. In `render_left` / `render_footer`:
     - Show `Filter: /<filter>_` clearly when `state.filtering` is true (even when `filter` is still empty).
     - Keep `render_footer` concise enough (or increase footer height to `Constraint::Length(4)` when needed) so status + totals + keyhints don't clip at 80×24.
- **Acceptance**: `TestBackend` tests at `80x24` and `120x40` verifying: Right pane border changes style when `Pane::Right` is active; filter mode shows `/` prompt when `filtering == true` and `filter == ""`; conflict messages appear in Right pane.

### Task 4.3: Fix `14_tui_runner.rs` Async/Idle Delta Loading & `--plan` Without `--world`
- **Files**: `src/14_tui_runner.rs`, `src/11_cli.rs`
- **Instructions**:
  1. In `event_loop` (`14_tui_runner.rs`): remove synchronous `state.ensure_deltas()?` from `KeyCode::Up` / `KeyCode::Char('k')` and `KeyCode::Down` / `KeyCode::Char('j')`. Instead, use a shorter `event::poll` timeout (e.g. `Duration::from_millis(25)` when `state.deltas_loading` is true, `100ms` otherwise) so `(rechne Deltas...)` renders immediately while scrolling and computes as soon as the user pauses.
  2. In `run_tui` / `run_tui_with`: if `args.plan` is `Some(path)` and `plan.baseline.world` is non-empty, allow running even if `--world` (default `/var/lib/portage/world`) does not exist on disk.
  3. Borrow `RepoIndex` and `Profile` from local `LoadedContext` in `run_tui` instead of `Box::leak`.
- **Acceptance**: `cargo test` passes; `run_tui` can start with `--plan plan.toml` without `/var/lib/portage/world` existing; rapid `move_selection` does not trigger delta calculation until idle.

---

## Phase 5: Cleanup, Module Splitting & Verification

### Task 5.1: Split Oversized Modules (>600 LOC)
- **Files**: `src/06_graph.rs`, `src/07_cascade.rs`, `src/12_tui_state.rs`
- **Instructions**:
  Split each file >600 lines into a module directory or sibling helper files while preserving all existing public re-exports in `src/lib.rs` (`mod_06_graph`, `mod_07_cascade`, `mod_12_tui_state`):
  1. `06_graph`: separate core BFS/types (`resolve`, `ResolvedGraph`, `Engine`) from blame/build-extra/src-uri helpers (`06_graph_blame.rs` or `06_graph/mod.rs` + `blame.rs` + `build_extra.rs` + `tests.rs`).
  2. `07_cascade`: separate fixpoint driver (`CascadeStep`, `resolve_with_cascade`) from `REQUIRED_USE` expression evaluator (`enforce_expr`, `satisfied`, `QuickSwitch` builders) and tests.
  3. `12_tui_state`: separate `AppState` navigation/filter/cache logic from mutation actions (`toggle_selected_flag`, `toggle_big_lever`, `cycle_choice_option`, `apply_quick_switch`, `sync_plan_from_state`) and tests.
- **Acceptance**: Every `.rs` file in `src/` is under ~450 lines; no public API signatures break; all tests pass.

### Task 5.2: Consolidate Duplicate Test Fixtures & Run Full Verification Suite
- **Files**: `src/*`, `tests/host_integration.rs`, `tests/perf_system.rs`, `tests/hil_regression.rs`
- **Instructions**:
  1. Create a shared `#[cfg(test)]` test-helper module (e.g. `src/test_helpers.rs` or `pub mod test_support` under `#[cfg(test)]`) for building synthetic `PkgVersion` and `RepoIndex` fixtures, replacing the 5+ duplicated `fn pkg(...)` helpers in `src/`.
  2. Run full verification:
     - `cargo fmt --check`
     - `cargo clippy --all-targets -- -D warnings`
     - `cargo test`
     - `cargo test --release --test perf_system -- --nocapture`
     - (If `GENTOO_HIL_REPO` is available) `GENTOO_HIL_REPO=... cargo test --release --test hil_regression -- --nocapture`
- **Acceptance**: Zero clippy warnings, 100% tests green, `sys_perf_*` budgets satisfied and no regression in resolve/delta counts.
