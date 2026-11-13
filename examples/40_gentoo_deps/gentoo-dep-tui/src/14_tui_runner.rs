//! 14_tui_runner — Terminal-Lifecycle und Crossterm-Event-Loop.
//!
//! Genau eine Zustaendigkeit: `ratatui::run` (init/restore inkl. Panic-Hook),
//! Tastatur-Events → `AppState`-Aktionen. Kein Rendering, keine Logik.

use std::path::PathBuf;
use std::time::Duration;

use anyhow::Context;
use crossterm::event::{self, Event, KeyCode, KeyModifiers};
use ratatui::DefaultTerminal;

use crate::mod_05_repo_index::RepoIndex;
use crate::mod_09_plan_toml::Plan;
use crate::mod_10_export_verify::VerifyRunner;
use crate::mod_11_cli::{TuiArgs, export_plan, load_engine};
use crate::mod_12_tui_state::{AppState, Modal, Pane, Tab, sync_plan_from_state};
use crate::mod_13_tui_view::render;

/// TUI starten (aus CLI-`tui`): Engine laden, Loop bis `q`.
pub fn run_tui(args: &TuiArgs) -> anyhow::Result<()> {
    let (engine, world, _lease) = load_engine(&args.common)?;
    // HIL-Ersatz: Boxen sind 'static-geleakt (siehe 11_cli) — hier direkt nutzen.
    run_tui_with(engine, world, args)
}

/// Eigentliche Loop (Engine + Welt + Args).
fn run_tui_with(
    engine: crate::mod_06_graph::Engine<'static>,
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

/// Event-Loop bis Quit (`q` oder `Esc` ohne Modal/Filter).
fn event_loop(
    terminal: &mut DefaultTerminal,
    mut state: AppState<'static>,
    args: &TuiArgs,
    export_default: &std::path::Path,
) -> anyhow::Result<()> {
    loop {
        terminal.draw(|frame| render(frame, &state))?;
        if !event::poll(Duration::from_millis(100))? {
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
            continue;
        }
        if state.modal.is_some() {
            match key.code {
                KeyCode::Esc => state.modal = None,
                KeyCode::Up | KeyCode::Char('k') => state.move_modal(-1),
                KeyCode::Down | KeyCode::Char('j') => state.move_modal(1),
                KeyCode::Enter => {
                    if let Some(Modal::Conflict { conflict, selected }) = &state.modal {
                        if let Some(opt) = state
                            .conflicts
                            .get(*conflict)
                            .and_then(|c| c.options.get(*selected))
                            .cloned()
                        {
                            state.apply_quick_switch(&opt)?;
                        } else {
                            state.modal = None;
                        }
                    }
                }
                _ => {}
            }
            continue;
        }
        match key.code {
            KeyCode::Char('q') => return Ok(()),
            KeyCode::Char('1') => state.set_tab(Tab::Packages)?,
            KeyCode::Char('2') => state.set_tab(Tab::BigLevers)?,
            KeyCode::Tab => state.cycle_pane(),
            KeyCode::Up | KeyCode::Char('k') => {
                state.move_selection(-1);
                state.ensure_deltas()?;
            }
            KeyCode::Down | KeyCode::Char('j') => {
                state.move_selection(1);
                // Erster Besuch: sofort nachladen (Folge-Besuche aus Cache).
                state.ensure_deltas()?;
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
                if state.pane != Pane::Left {
                    state.pane = Pane::Left;
                } else {
                    return Ok(());
                }
            }
            _ => {}
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
