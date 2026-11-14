//! 14_tui_runner — Terminal-Lifecycle und Crossterm-Event-Loop.

use std::time::Duration;

use anyhow::Context;
use crossterm::event::{self, Event, KeyCode, KeyModifiers};
use ratatui::DefaultTerminal;

use crate::mod_05_repo_index::RepoIndex;
use crate::mod_11_cli::{TuiArgs, load_profile, load_world};
use crate::mod_12_tui_state::{AppState, Modal, Pane, Tab};
use crate::mod_13_tui_view::render;

/// TUI starten (aus CLI-`tui`): Engine laden, Loop bis `q`.
pub fn run_tui(args: &TuiArgs) -> anyhow::Result<()> {
    let profile = load_profile(&args.common)?;
    let index = RepoIndex::load(&args.common.eff_repo(), args.common.binhost.as_deref(), &profile)
        .with_context(|| "Repo-Index laden")?;
    let world = load_world(&args.common)?;
    run_tui_with(index, profile, world, args)
}

/// Eigentliche Loop (owned Index + Profil + Welt + Args).
fn run_tui_with(
    index: RepoIndex,
    profile: crate::mod_02_profile::Profile,
    world: Vec<String>,
    args: &TuiArgs,
) -> anyhow::Result<()> {
    // Engine enthaelt nur Referenzen: auf statische Leaks umziehen.
    let index: &'static RepoIndex = Box::leak(Box::new(index));
    let profile: &'static crate::mod_02_profile::Profile = Box::leak(Box::new(profile));
    let state = AppState::new(index, profile, world)?;
    ratatui::run(|terminal| event_loop(terminal, state, args))
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
    /// CLI-Argumente (fuer `out-use`-Pfad beim Speichern).
    pub args: &'a TuiArgs,
}

/// Synchroner Idle-Schritt fuer Tests/Scripting (wie der Idle-Pfad im Loop).
pub fn step_idle(state: &mut AppState<'_>) -> anyhow::Result<()> {
    state.ensure_deltas()?;
    Ok(())
}

/// Tastatur-Event verarbeiten (reine State-Transition, terminal-unabhaengig).
pub fn dispatch_key(
    state: &mut AppState<'_>,
    key: crossterm::event::KeyEvent,
    ctx: &mut RunnerContext<'_>,
) -> anyhow::Result<LoopAction> {
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
            if state.tab == Tab::BigLevers {
                state.toggle_big_lever()?;
            } else {
                state.pane = Pane::Middle;
                state.toggle_selected_flag()?;
            }
        }
        KeyCode::Enter => {
            // Auf Paket mit Konflikt: Modal; auf ||: umschalten.
            if state.selected_or_group().is_some() {
                state.cycle_or_decision()?;
            } else if let Some(pkg) = state.selected_package() {
                state.open_conflict_for_package(&pkg);
            }
        }
        KeyCode::Char('/') => {
            state.filtering = true;
            state.filter.clear();
        }
        KeyCode::Char('?') | KeyCode::F(1) => {
            state.modal = Some(Modal::Help);
        }
        KeyCode::Char('o' | 'O') => {
            state.cycle_left_sort();
        }
        KeyCode::Char('x' | 'X') => {
            state.compute_cut_plan()?;
        }
        KeyCode::Char('a' | 'A') => {
            if state.apply_cut_blockers()? {
                state.status = "Cut-Blocker angewendet".to_string();
            }
        }
        KeyCode::Char('u' | 'U') => {
            if state.pop_undo()? {
                state.status = "Undo".to_string();
            } else {
                state.status = "Nichts zum Zurueckrollen".to_string();
            }
        }
        KeyCode::Char('s' | 'S') => {
            match state.export_package_use(&ctx.args.out_use) {
                Ok(()) => {}
                Err(e) => state.status = format!("Export-Fehler: {e:#}"),
            }
        }
        KeyCode::Esc => {
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
) -> anyhow::Result<()> {
    let mut ctx = RunnerContext { args };
    loop {
        terminal.draw(|frame| render(frame, &state))?;
        // Idle-Entprellung: erst rechnen, wenn kein weiteres Event ansteht
        // (~80 ms), damit schnelles j/k-Scrollen fluessig bleibt.
        let timeout = Duration::from_millis(80);
        if !event::poll(timeout)? {
            // Idle: ausstehende Deltas nachladen.
            state.ensure_deltas()?;
            continue;
        }
        let Event::Key(key) = event::read()? else {
            continue;
        };
        if key.modifiers.contains(KeyModifiers::CONTROL) && key.code == KeyCode::Char('c') {
            return Ok(());
        }
        match dispatch_key(&mut state, key, &mut ctx)? {
            LoopAction::Continue => {}
            LoopAction::Quit => return Ok(()),
        }
        // Nach Tastendruck kein sofortiges Rechnen: naechster Poll (80 ms)
        // zeigt erst, ob der Nutzer pausiert (Idle-Pfad oben).
    }
}

/// Smoke-Einstieg fuer Tests (ohne Terminal): State aufbauen.
pub fn build_state_for_smoke(
    index: &'static RepoIndex,
    profile: &'static crate::mod_02_profile::Profile,
    world: Vec<String>,
) -> anyhow::Result<AppState<'static>> {
    Ok(AppState::new(index, profile, world)?)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mod_11_cli::CommonArgs;

    fn test_common(world: std::path::PathBuf) -> CommonArgs {
        CommonArgs {
            repo: world.parent().unwrap().join("repo"),
            profile_dir: None,
            profile: None,
            world,
            binhost: None,
            make_conf: None,
            package_use: None,
            portage_dir: std::path::PathBuf::from("/nonexistent-portage"),
            snapshot: None,
            json: false,
            perf_report: false,
        }
    }

    #[test]
    fn missing_world_is_an_error_with_hint() {
        let d = tempfile::tempdir().unwrap();
        let missing = d.path().join("kein-world");
        let common = test_common(missing.clone());
        let err = load_world(&common).unwrap_err();
        let msg = format!("{err:#}");
        assert!(msg.contains("--world"), "Hinweis fehlt: {msg}");
    }

    #[test]
    fn tui_args_parse_out_use_flag() {
        use clap::Parser;
        let cli = crate::mod_11_cli::Cli::try_parse_from([
            "gentoo-dep-tui",
            "tui",
            "--out-use",
            "/tmp/x.use",
        ])
        .unwrap();
        match cli.command {
            Some(crate::mod_11_cli::Commands::Tui(a)) => {
                assert_eq!(a.out_use, std::path::PathBuf::from("/tmp/x.use"));
            }
            _ => panic!("falsches Subcommand"),
        }
    }

    #[test]
    fn common_args_defaults_to_portage_dir() {
        use clap::Parser;
        let cli = crate::mod_11_cli::Cli::try_parse_from(["gentoo-dep-tui", "graph"]).unwrap();
        match cli.command {
            Some(crate::mod_11_cli::Commands::Graph(a)) => {
                assert_eq!(
                    a.common.portage_dir,
                    std::path::PathBuf::from("/etc/portage")
                );
                assert!(!a.common.json);
            }
            _ => panic!("falsches Subcommand"),
        }
    }
}
