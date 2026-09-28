//! `ratatui` telemetry: variable inspector, loss graph, status line.
//!
//! [`render`] is pure over [`AppState`] and tested headless via
//! `TestBackend` (no display needed). Only [`run_tui`] touches a real
//! terminal, and it restores cooked mode plus the main screen on exit
//! (also on initialization, draw, and event errors).

use ratatui::backend::CrosstermBackend;
use ratatui::layout::{Constraint, Direction, Layout};
use ratatui::style::{Color, Style};
use ratatui::symbols;
use ratatui::text::Line;
use ratatui::widgets::{
    Axis, Block, Borders, Chart, Dataset, GraphType, List, ListItem, Paragraph,
};
use ratatui::{Frame, Terminal};
use std::io;

/// Telemetry snapshot rendered by [`render`].
#[derive(Debug, Clone, Default)]
pub struct AppState {
    /// `(name, value)` optimized variables.
    pub vars: Vec<(String, f64)>,
    /// Loss history, oldest first.
    pub loss_history: Vec<f64>,
    /// One-line status.
    pub status: String,
}

/// Render inspector (left), loss chart (top right), status (bottom right).
pub fn render(frame: &mut Frame, state: &AppState) {
    let cols = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Percentage(30), Constraint::Percentage(70)])
        .split(frame.area());
    let rows = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Percentage(60), Constraint::Percentage(40)])
        .split(cols[1]);

    let items: Vec<ListItem> = state
        .vars
        .iter()
        .map(|(k, v)| ListItem::new(Line::from(format!("{k}: {v:.4}"))))
        .collect();
    let inspector =
        List::new(items).block(Block::default().title("Variables").borders(Borders::ALL));
    frame.render_widget(inspector, cols[0]);

    let points: Vec<(f64, f64)> = state
        .loss_history
        .iter()
        .enumerate()
        .map(|(i, y)| (i as f64, *y))
        .collect();
    let (ymin, ymax) = points
        .iter()
        .map(|p| p.1)
        .fold((f64::INFINITY, f64::NEG_INFINITY), |(a, b), y| {
            (a.min(y), b.max(y))
        });
    let (ymin, ymax) = if ymin.is_finite() && ymax.is_finite() && (ymax - ymin) > 1e-12 {
        (ymin, ymax)
    } else if ymin.is_finite() {
        (ymin - 1.0, ymin + 1.0)
    } else {
        (0.0, 1.0)
    };
    let dataset = Dataset::default()
        .marker(symbols::Marker::Dot)
        .graph_type(GraphType::Line)
        .style(Style::default().fg(Color::Cyan))
        .data(&points);
    let xmax = state.loss_history.len().saturating_sub(1).max(1) as f64;
    let chart = Chart::new(vec![dataset])
        .block(Block::default().title("Loss").borders(Borders::ALL))
        .x_axis(Axis::default().bounds([0.0, xmax]))
        .y_axis(Axis::default().bounds([ymin, ymax]));
    frame.render_widget(chart, rows[0]);

    let status = Paragraph::new(state.status.clone())
        .block(Block::default().title("Status").borders(Borders::ALL));
    frame.render_widget(status, rows[1]);
}

/// Run the interactive TUI until `q`/Esc/Enter; restores the terminal.
pub fn run_tui(state: &AppState) -> io::Result<()> {
    use crossterm::ExecutableCommand;
    use crossterm::event::{self, Event, KeyCode};
    use crossterm::terminal::{self, EnterAlternateScreen, LeaveAlternateScreen};

    terminal::enable_raw_mode()?;
    restore_terminal_after(
        || {
            let mut out = io::stdout();
            out.execute(EnterAlternateScreen)?;
            let mut term = Terminal::new(CrosstermBackend::new(io::stdout()))?;
            loop {
                term.draw(|f| render(f, state))?;
                if let Event::Key(k) = event::read()?
                    && matches!(k.code, KeyCode::Char('q') | KeyCode::Esc | KeyCode::Enter)
                {
                    break;
                }
            }
            Ok(())
        },
        terminal::disable_raw_mode,
        || io::stdout().execute(LeaveAlternateScreen).map(|_| ()),
    )
}

fn restore_terminal_after(
    run: impl FnOnce() -> io::Result<()>,
    disable_raw_mode: impl FnOnce() -> io::Result<()>,
    leave_screen: impl FnOnce() -> io::Result<()>,
) -> io::Result<()> {
    let result = run();
    // Attempt both restorations, retaining the original failure when present.
    let raw_result = disable_raw_mode();
    let screen_result = leave_screen();
    result.and(raw_result).and(screen_result)
}

#[cfg(test)]
mod tests {
    use super::*;
    use ratatui::backend::TestBackend;

    fn screen_text(term: &Terminal<TestBackend>) -> String {
        let buf = term.backend().buffer();
        let mut text = String::new();
        for y in 0..buf.area.height {
            for x in 0..buf.area.width {
                text.push_str(buf[(x, y)].symbol());
            }
            text.push('\n');
        }
        text
    }

    #[test]
    fn renders_panels_headless() {
        let backend = TestBackend::new(80, 24);
        let mut term = Terminal::new(backend).expect("terminal");
        let state = AppState {
            vars: vec![("Front Element.radius".into(), 50.0)],
            loss_history: vec![3.0, 2.0, 1.0],
            status: "iter 3 ok".into(),
        };
        term.draw(|f| render(f, &state)).expect("draw");
        let text = screen_text(&term);
        assert!(text.contains("Variables"), "missing inspector");
        assert!(text.contains("Front Element.radius"), "missing var");
        assert!(text.contains("Loss"), "missing chart");
        assert!(text.contains("iter 3 ok"), "missing status");
    }

    #[test]
    fn renders_empty_state_without_panic() {
        let backend = TestBackend::new(80, 24);
        let mut term = Terminal::new(backend).expect("terminal");
        term.draw(|f| render(f, &AppState::default()))
            .expect("draw");
        let text = screen_text(&term);
        assert!(text.contains("Variables") && text.contains("Status"));
    }

    #[test]
    fn restores_terminal_on_startup_or_event_error() {
        use std::cell::RefCell;

        for fails in [false, true] {
            let calls = RefCell::new(Vec::new());
            let result = restore_terminal_after(
                || {
                    calls.borrow_mut().push("run");
                    if fails {
                        Err(io::Error::other("terminal failed"))
                    } else {
                        Ok(())
                    }
                },
                || {
                    calls.borrow_mut().push("disable");
                    Ok(())
                },
                || {
                    calls.borrow_mut().push("leave");
                    Ok(())
                },
            );
            assert_eq!(result.is_err(), fails);
            assert_eq!(*calls.borrow(), ["run", "disable", "leave"]);
        }
    }

    #[test]
    fn attempts_screen_cleanup_even_when_raw_cleanup_fails() {
        use std::cell::Cell;

        for fails in [false, true] {
            let left_screen = Cell::new(false);
            let result = restore_terminal_after(
                || {
                    if fails {
                        Err(io::Error::other("original error"))
                    } else {
                        Ok(())
                    }
                },
                || Err(io::Error::other("raw cleanup error")),
                || {
                    left_screen.set(true);
                    Err(io::Error::other("screen cleanup error"))
                },
            );
            assert!(left_screen.get());
            assert_eq!(
                result.unwrap_err().to_string(),
                if fails { "original error" } else { "raw cleanup error" }
            );
        }
    }
}
