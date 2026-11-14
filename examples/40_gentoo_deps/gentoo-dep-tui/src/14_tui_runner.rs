//! 14_tui_runner — Terminal-Lifecycle und Crossterm-Event-Loop.
//!
//! Genau eine Zustaendigkeit: `ratatui::run` (init/restore inkl. Panic-Hook),
//! Tastatur-Events → `AppState`-Aktionen via `dispatch_key` (dieselbe
//! Funktion steuert Test-/Script-Driver). Kein Rendering, keine Logik.

use std::path::PathBuf;
use std::time::Duration;

use anyhow::Context;
use crossterm::event::{self, Event, KeyCode, KeyModifiers};
use ratatui::DefaultTerminal;

use crate::mod_05_repo_index::RepoIndex;
use crate::mod_09_plan_toml::Plan;
use crate::mod_10_export_verify::VerifyRunner;
use crate::mod_11_cli::{CommonArgs, TuiArgs, export_plan, load_profile};
use crate::mod_12_tui_state::{AppState, Modal, Pane, Tab, sync_plan_from_state};
use crate::mod_13_tui_view::render;

/// TUI starten (aus CLI-`tui`): Engine laden, Loop bis `q`.
///
/// Fehlt `--world` auf Platte, aber `--plan` liefert `baseline.world`,
/// startet die TUI trotzdem mit der Plan-Welt (Task 4.3).
pub fn run_tui(args: &TuiArgs) -> anyhow::Result<()> {
    let profile = load_profile(&args.common)?;
    let index = RepoIndex::load(&args.common.repo, args.common.binhost.as_deref(), &profile)
        .with_context(|| "Repo-Index laden")?;
    let world = resolve_world(&args.common, args.plan.as_ref())?;
    let engine = crate::mod_06_graph::Engine::new(&index, &profile);
    run_tui_with(engine, world, args)
}

/// Welt bestimmen: `--world`-Datei, notfalls `baseline.world` aus `--plan`
/// (Test-Haelle fuer den Task-4.3-Fallback ohne Terminal).
fn resolve_world(common: &CommonArgs, plan: Option<&PathBuf>) -> anyhow::Result<Vec<String>> {
    match crate::mod_11_cli::load_world(common) {
        Ok(world) => Ok(world),
        Err(e) => {
            let Some(path) = plan else {
                return Err(e);
            };
            let plan = Plan::load(path)?;
            if plan.baseline.world.is_empty() {
                return Err(e);
            }
            Ok(plan.baseline.world.clone())
        }
    }
}

/// Eigentliche Loop (Engine + Welt + Args).
fn run_tui_with(
    engine: crate::mod_06_graph::Engine<'_>,
    world: Vec<String>,
    args: &TuiArgs,
) -> anyhow::Result<()> {
    let (plan, plan_path) = match &args.plan {
        Some(path) => (Plan::load(path)?, Some(path.clone())),
        None => (Plan::default(), None),
    };
    // Welt um gewaehlte Choice-Atome erweitern, Index/Profil aus Engine leihen.
    let world = {
        let mut w = world;
        for atom in plan.effective_world() {
            if !w.contains(&atom) {
                w.push(atom);
            }
        }
        w
    };
    // Safety: Engine enthaelt nur &'static-Referenzen (geleakte Boxen).
    let state = AppState::new(engine.index, engine.profile, world, plan, plan_path)?;
    let export_default = PathBuf::from("/tmp/gentoo-export");
    ratatui::run(|terminal| event_loop(terminal, state, args, &export_default))
        .with_context(|| "TUI-Loop")
}

/// Ergebnis eines Tastatur-Events (Loop fortsetzen oder beenden).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LoopAction {
    /// Weiterlaufen.
    Continue,
    /// Beenden (`q` oder `Esc` ohne Modal/Filter).
    Quit,
}

/// Kontext fuer `dispatch_key` (TUI-Loop und Test-/Script-Driver teilen ihn).
pub struct RunnerContext<'a> {
    /// CLI-Argumente (fuer `plan`-Pfad beim Speichern).
    pub args: &'a TuiArgs,
    /// Standard-Exportverzeichnis (`[E]`/auto-Export bei `[V]`).
    pub export_default: &'a std::path::Path,
}

/// Synchroner Idle-Schritt fuer Tests/Scripting (wie der Idle-Pfad im Loop).
pub fn step_idle(state: &mut AppState<'_>) -> anyhow::Result<()> {
    state.ensure_deltas()?;
    Ok(())
}

/// Tastatur-Event verarbeiten (reine State-Transition, terminal-unabhaengig).
///
/// Dieselbe Funktion steuert die echte TUI (`event_loop`) und den
/// Test-/Script-Driver (`tests/tui_screencaps.rs`), sodass Integrationstests
/// exakt das Tastenverhalten der TUI pruefen (review3 Task 6.1).
pub fn dispatch_key(
    state: &mut AppState<'_>,
    key: crossterm::event::KeyEvent,
    ctx: &mut RunnerContext<'_>,
) -> anyhow::Result<LoopAction> {
    let args = ctx.args;
    let export_default = ctx.export_default;
    if state.filtering {
        match key.code {
            KeyCode::Esc | KeyCode::Enter => state.filtering = false,
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
    if state.modal.is_some() {
        // Hilfe zusaetzlich per `?`/`q` schliessen (review3 Task 5.2).
        if matches!(state.modal, Some(Modal::Help))
            && matches!(
                key.code,
                KeyCode::Char('?') | KeyCode::Char('q') | KeyCode::Char('Q')
            )
        {
            state.modal = None;
            return Ok(LoopAction::Continue);
        }
        match key.code {
            KeyCode::Esc => state.modal = None,
            KeyCode::Up | KeyCode::Char('k') => state.move_modal(-1),
            KeyCode::Down | KeyCode::Char('j') => state.move_modal(1),
            KeyCode::Enter => {
                // Echte und simulierte Konflikte gleich behandeln.
                if let Some(opt) = state.modal_selected_option() {
                    state.apply_quick_switch(&opt)?;
                } else {
                    state.modal = None;
                }
            }
            _ => {}
        }
        return Ok(LoopAction::Continue);
    }
    match key.code {
        KeyCode::Char('q') => return Ok(LoopAction::Quit),
        KeyCode::Char('1') => state.set_tab(Tab::Packages)?,
        KeyCode::Char('2') => state.set_tab(Tab::BigLevers)?,
        KeyCode::Tab => state.cycle_pane(),
        KeyCode::Up | KeyCode::Char('k') => {
            state.move_selection(-1);
        }
        KeyCode::Down | KeyCode::Char('j') => {
            state.move_selection(1);
        }
        KeyCode::Char(' ') => {
            // Tab 2: Big-Lever global toggeln (T07).
            if state.tab == Tab::BigLevers {
                state.toggle_big_lever()?;
            } else if state.selected_choice().is_some() {
                state.cycle_choice_option()?;
            } else {
                state.pane = Pane::Middle;
                state.toggle_selected_flag()?;
            }
        }
        KeyCode::Enter => {
            // Auf Paket mit Konflikt: Modal; auf ||: umschalten; auf Choice: Option wechseln.
            if state.selected_or_group().is_some() {
                state.cycle_or_decision()?;
            } else if state.selected_choice().is_some() {
                state.cycle_choice_option()?;
            } else if let Some(pkg) = state.selected_package() {
                state.open_conflict_for_package(&pkg);
            }
        }
        KeyCode::Char('/') => {
            state.filtering = true;
            state.filter.clear();
        }
        // Hilfe-Overlay (review3 Task 5.2).
        KeyCode::Char('?') | KeyCode::F(1) => {
            state.modal = Some(Modal::Help);
        }
        KeyCode::Char('s' | 'S') => {
            let path = state.plan_path.clone().unwrap_or_else(|| {
                args.plan
                    .clone()
                    .unwrap_or_else(|| PathBuf::from("plan.toml"))
            });
            state.plan_path = Some(path);
            state.save()?;
        }
        KeyCode::Char('e' | 'E') => {
            // Vor Export synchronisieren (T07): ungespeicherte Toggles landen im Export.
            sync_plan_from_state(&mut state.plan, &state.engine, &state.world);
            export_plan(&state.plan, export_default)?;
            state.dirty = false;
            state.status = format!("Exportiert: {}", export_default.display());
        }
        KeyCode::Char('v' | 'V') => {
            // Export synchron halten (dirty oder noch nie exportiert),
            // dann verifizieren (review3 Task 5.3).
            let exported_use = export_default.join("etc/portage/package.use");
            if state.dirty || !exported_use.is_file() {
                sync_plan_from_state(&mut state.plan, &state.engine, &state.world);
                match export_plan(&state.plan, export_default) {
                    Ok(_) => {
                        state.dirty = false;
                        state.status = format!("Exportiert: {}", export_default.display());
                    }
                    Err(e) => {
                        state.status = format!("Export-Fehler: {e:#}");
                        return Ok(LoopAction::Continue);
                    }
                }
            }
            let runner = VerifyRunner::against_configroot(export_default);
            match runner.run() {
                Ok(o) => {
                    state.status = if o.success {
                        format!("Verify OK: {} Pakete", o.packages.len())
                    } else {
                        format!("Verify FEHLER: {} Probleme", o.problems.len())
                    };
                }
                Err(e) => state.status = format!("Verify-Fehler: {e:#}"),
            }
        }
        KeyCode::Esc => {
            // Erst Filter loeschen, dann Pane, dann beenden (review3 1.2).
            if !state.filter.is_empty() {
                state.filter.clear();
                state.apply_filter_only();
            } else if state.pane != Pane::Left {
                state.pane = Pane::Left;
            } else {
                return Ok(LoopAction::Quit);
            }
        }
        _ => {}
    }
    Ok(LoopAction::Continue)
}

/// Event-Loop bis Quit (`q` oder `Esc` ohne Modal/Filter).
fn event_loop(
    terminal: &mut DefaultTerminal,
    mut state: AppState<'_>,
    args: &TuiArgs,
    export_default: &std::path::Path,
) -> anyhow::Result<()> {
    let mut ctx = RunnerContext {
        args,
        export_default,
    };
    loop {
        terminal.draw(|frame| render(frame, &state))?;
        // Idle-Recompute (T05): Bei ausstehenden Deltas schneller pollen,
        // damit `(rechne Deltas...)` sofort rendert und erst in der Pause
        // gerechnet wird — kein synchrones Nachladen beim Scrollen (4.3).
        let timeout = if state.deltas_loading {
            Duration::from_millis(25)
        } else {
            Duration::from_millis(100)
        };
        if !event::poll(timeout)? {
            // Idle: ausstehende Deltas nach Cache-Miss nachladen (T05).
            state.ensure_deltas()?;
            continue;
        }
        let Event::Key(key) = event::read()? else {
            continue;
        };
        // Ctrl-C beendet immer (Terminal wird via run() restauriert).
        if key.modifiers.contains(KeyModifiers::CONTROL) && key.code == KeyCode::Char('c') {
            return Ok(());
        }
        match dispatch_key(&mut state, key, &mut ctx)? {
            LoopAction::Continue => {}
            LoopAction::Quit => return Ok(()),
        }
    }
}

/// Smoke-Einstieg fuer Tests (ohne Terminal): State aufbauen.
pub fn build_state_for_smoke(
    index: &'static RepoIndex,
    profile: &'static crate::mod_02_profile::Profile,
    world: Vec<String>,
) -> anyhow::Result<AppState<'static>> {
    Ok(AppState::new(index, profile, world, Plan::default(), None)?)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mod_11_cli::CommonArgs;

    #[test]
    fn plan_world_fallback_without_world_file() {
        // Fehlende `--world`-Datei + `--plan` mit Baseline → Plan-Welt;
        // ohne Plan (oder leere Baseline) → Fehler (Task 4.3).
        let d = tempfile::tempdir().unwrap();
        let missing = d.path().join("kein-world");
        let plan_path = d.path().join("plan.toml");
        let mut plan = Plan::default();
        plan.baseline.world = vec!["app-misc/jq".to_string()];
        plan.save(&plan_path).unwrap();
        let common = CommonArgs {
            repo: d.path().join("repo"),
            profile_dir: None,
            profile: None,
            world: missing.clone(),
            binhost: None,
            make_conf: d.path().join("make.conf"),
            package_use: d.path().join("package.use"),
            perf_report: false,
        };
        let world = resolve_world(&common, Some(&plan_path)).unwrap();
        assert_eq!(world, vec!["app-misc/jq".to_string()]);
        assert!(resolve_world(&common, None).is_err());
        let empty_plan = d.path().join("leer.toml");
        Plan::default().save(&empty_plan).unwrap();
        assert!(resolve_world(&common, Some(&empty_plan)).is_err());
    }

    #[test]
    fn tui_args_parse_plan_flag() {
        use clap::Parser;
        let cli = crate::mod_11_cli::Cli::try_parse_from([
            "gentoo-dep-tui",
            "tui",
            "--plan",
            "/tmp/p.toml",
        ])
        .unwrap();
        match cli.command {
            Some(crate::mod_11_cli::Commands::Tui(a)) => {
                assert_eq!(a.plan, Some(PathBuf::from("/tmp/p.toml")));
            }
            _ => panic!("falsches Subcommand"),
        }
    }
}
