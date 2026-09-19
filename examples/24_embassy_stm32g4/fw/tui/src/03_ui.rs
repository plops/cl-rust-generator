//! Rendering (T1): header tabs, per-screen content, shared log pane.

use crate::app_02::{App, Screen};
use ratatui::layout::{Constraint, Direction, Layout};
use ratatui::style::{Color, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Axis, Block, Borders, Chart, Dataset, GraphType, List, Paragraph};
use ratatui::Frame;

const SCREENS: [Screen; 6] = [
    Screen::Status,
    Screen::Freq,
    Screen::Scope,
    Screen::Awg,
    Screen::Vna,
    Screen::Cap,
];

pub fn draw(f: &mut Frame, app: &App) {
    let rows = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Length(3),
            Constraint::Min(8),
            Constraint::Length(9),
            Constraint::Length(3),
        ])
        .split(f.area());

    // Tabs.
    let tabs: Vec<Span> = SCREENS
        .iter()
        .enumerate()
        .flat_map(|(i, s)| {
            let label = format!(" {}:{} ", i + 1, s.title());
            let span = if *s == app.screen {
                Span::styled(label, Style::default().bg(Color::Blue))
            } else {
                Span::raw(label)
            };
            [span, Span::raw(" ")]
        })
        .collect();
    f.render_widget(
        Paragraph::new(Line::from(tabs)).block(Block::default().borders(Borders::ALL)),
        rows[0],
    );

    // Content.
    match app.screen {
        Screen::Scope if !app.scope.is_empty() => {
            let max = app.scope.iter().copied().max().unwrap_or(4095) as f64;
            let pts: Vec<(f64, f64)> = app
                .scope
                .iter()
                .enumerate()
                .map(|(i, v)| (i as f64, *v as f64))
                .collect();
            let chart = Chart::new(vec![Dataset::default()
                .graph_type(GraphType::Line)
                .data(&pts)])
            .block(Block::default().borders(Borders::ALL).title("scope"))
            .x_axis(Axis::default().bounds([0.0, pts.len() as f64]))
            .y_axis(Axis::default().bounds([0.0, max.max(1.0)]));
            f.render_widget(chart, rows[1]);
        }
        Screen::Vna if !app.vna.is_empty() => {
            let lines: Vec<Line> = app
                .vna
                .iter()
                .map(|(fr, mv)| Line::raw(format!("{:>8} Hz  {:>5} mVpp", fr, mv)))
                .collect();
            f.render_widget(
                List::new(lines).block(Block::default().borders(Borders::ALL).title("vna")),
                rows[1],
            );
        }
        _ => {
            let body = format!("{}\n\n{}", app.screen.title(), app.screen.hint());
            f.render_widget(
                Paragraph::new(body).block(Block::default().borders(Borders::ALL)),
                rows[1],
            );
        }
    }

    // Log.
    let n = app.log.len();
    let start = n.saturating_sub(7);
    let items: Vec<Line> = app.log[start..]
        .iter()
        .map(|l| Line::raw(l.clone()))
        .collect();
    f.render_widget(
        List::new(items).block(Block::default().borders(Borders::ALL).title("log")),
        rows[2],
    );

    // Footer.
    f.render_widget(Paragraph::new("1-6 screen · space run · q quit"), rows[3]);
}
