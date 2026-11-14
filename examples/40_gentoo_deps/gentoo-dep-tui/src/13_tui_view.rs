//! 13_tui_view — Ratatui-3-Pane-Layout, Blame-View, Big-Levers, Modal, Footer.
//!
//! Genau eine Zustaendigkeit: `AppState` → Widgets. Keine Logik, keine
//! Events (siehe `12_tui_state` / `14_tui_runner`).

use ratatui::Frame;
use ratatui::layout::{Constraint, Direction, Layout, Rect};
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, List, ListItem, Paragraph, Tabs};

use crate::mod_12_tui_state::{AppState, LeftEntry, Modal, Pane, Tab, left_label};

/// Kompletten Frame zeichnen (Tabs oben, Footer unten).
pub fn render(frame: &mut Frame, state: &AppState) {
    let area = frame.area();
    if area.width < 20 || area.height < 8 {
        frame.render_widget(Paragraph::new("Terminal zu klein"), area);
        return;
    }
    // Footer-Hoehe aus Inhalt und Breite (kein Clipping bei 80x24, review3 5.1).
    let footer_len = footer_height(state, frame.area().width);
    let rows = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Length(3),
            Constraint::Min(1),
            Constraint::Length(footer_len),
        ])
        .split(area);
    render_tabs(frame, state, rows[0]);
    match state.tab {
        Tab::Packages => render_packages(frame, state, rows[1]),
        Tab::BigLevers => render_levers(frame, state, rows[1]),
    }
    render_footer(frame, state, rows[2]);
    match &state.modal {
        Some(Modal::Conflict { conflict, selected }) => {
            if let Some(info) = state.conflicts.get(*conflict) {
                render_conflict_modal(frame, area, info, *selected);
            }
        }
        Some(Modal::SimulatedConflict { conflict, selected }) => {
            render_conflict_modal(frame, area, conflict, *selected);
        }
        Some(Modal::Help) => {
            render_help_modal(frame, area);
        }
        None => {}
    }
}

/// Tab-Leiste `[1] Pakete & Alternativen  [2] Global Big Levers`.
fn render_tabs(frame: &mut Frame, state: &AppState, area: Rect) {
    let titles = vec!["[1] Pakete & Alternativen", "[2] Global Big Levers"];
    let tabs = Tabs::new(titles)
        .select(match state.tab {
            Tab::Packages => 0,
            Tab::BigLevers => 1,
        })
        .block(Block::default().borders(Borders::BOTTOM))
        .highlight_style(
            Style::default()
                .fg(Color::Yellow)
                .add_modifier(Modifier::BOLD),
        );
    frame.render_widget(tabs, area);
}

/// Drei Panes: links Liste, mitte Flags, rechts Blame/Pros-Cons.
fn render_packages(frame: &mut Frame, state: &AppState, area: Rect) {
    let cols = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([
            Constraint::Percentage(30),
            Constraint::Percentage(40),
            Constraint::Percentage(30),
        ])
        .split(area);
    render_left(frame, state, cols[0]);
    render_middle(frame, state, cols[1]);
    render_right(frame, state, cols[2]);
}

/// Links: Welt/Choices/`||`/Transitive, Konflikt-Markierung (T06 stateful).
fn render_left(frame: &mut Frame, state: &AppState, area: Rect) {
    let items: Vec<ListItem> = state
        .left
        .iter()
        .enumerate()
        .map(|(i, entry)| {
            let mut spans = vec![Span::raw(left_label(entry))];
            if let LeftEntry::WorldPkg(cpn) | LeftEntry::Transitive(cpn) = entry
                && state.conflicts.iter().any(|c| &c.package == cpn)
            {
                spans.push(Span::styled(" CONFLICT", Style::default().fg(Color::Red)));
            }
            let mut item = ListItem::new(Line::from(spans));
            // Fokus links behalten, auch wenn Mitte/Rechts aktiv (T06).
            if i == state.left_selected {
                let bg = if state.pane == Pane::Left {
                    Color::DarkGray
                } else {
                    Color::Blue
                };
                item = item.style(Style::default().bg(bg));
            }
            item
        })
        .collect();
    let mut title = format!("Pakete ({})", state.left.len());
    if state.filtering {
        // Aktive Filtereingabe immer zeigen — auch noch leer (`/_`).
        title.push_str(&format!(" [Filter: /{}_]", state.filter));
    } else if !state.filter.is_empty() {
        title.push_str(&format!(" [Filter: {}]", state.filter));
    }
    let list = List::new(items).highlight_symbol("> ").block(
        Block::default()
            .borders(Borders::ALL)
            .title(title)
            .border_style(border_style(state.pane == Pane::Left)),
    );
    let mut list_state = state.left_list_state;
    frame.render_stateful_widget(list, area, &mut list_state);
}

#[path = "13_tui_view/bottom.rs"]
mod bottom;
#[path = "13_tui_view/middle.rs"]
mod middle;
#[path = "13_tui_view/right.rs"]
mod right;
#[cfg(test)]
#[path = "13_tui_view/tests.rs"]
mod tests;

use bottom::border_style;
use bottom::{
    footer_height, render_conflict_modal, render_footer, render_help_modal, render_levers,
};
use middle::render_middle;
use right::render_right;
