//! 13_tui_view/bottom — Hebel-Tabelle, Footer, Konflikt-Modal, Formathelfer.

use ratatui::Frame;
use ratatui::layout::{Constraint, Rect};
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span, Text};
use ratatui::widgets::{Block, Borders, Cell, Clear, Paragraph, Row, Table, Wrap};

use crate::mod_01_types::ConflictInfo;
use crate::mod_12_tui_state::AppState;

pub(super) fn render_levers(frame: &mut Frame, state: &AppState, area: Rect) {
    let header = Row::new(vec!["Flag", "Pakete", "Bin-Delta", "DL-Delta", "Konflikte"])
        .style(Style::default().add_modifier(Modifier::BOLD));
    let rows: Vec<Row> = state
        .levers
        .iter()
        .map(|lever| {
            Row::new(vec![
                Cell::from(lever.flag.clone()),
                Cell::from(lever.packages.to_string()),
                Cell::from(fmt_bytes(lever.total.binpkg_bytes)),
                Cell::from(fmt_bytes(lever.total.download_bytes)),
                Cell::from(lever.conflicts.to_string()),
            ])
        })
        .collect();
    let widths = [
        Constraint::Min(12),
        Constraint::Length(8),
        Constraint::Length(12),
        Constraint::Length(12),
        Constraint::Length(10),
    ];
    let mut title = "Global Big Levers [Space toggelt]".to_string();
    if state.levers_dirty {
        title.push_str(" (veraltet — wird neu gerechnet)");
    }
    let table = Table::new(rows, widths)
        .header(header)
        .row_highlight_style(Style::default().bg(Color::DarkGray))
        .highlight_symbol("> ")
        .block(Block::default().borders(Borders::ALL).title(title));
    let mut table_state = state.lever_table_state;
    frame.render_stateful_widget(table, area, &mut table_state);
}

/// Footer: Totals, Paket-/Konflikt-Zaehler, Tastenbelegung.
pub(super) fn render_footer(frame: &mut Frame, state: &AppState, area: Rect) {
    let totals = state.graph.totals();
    let mut spans = vec![
        Span::styled(
            format!(
                "Binpkg {}  Download {}  BDEPEND {}  Pakete {}  Konflikte {}",
                fmt_bytes(totals.binpkg_bytes),
                fmt_bytes(totals.download_bytes),
                fmt_bytes(state.graph.build_extra.binpkg_bytes),
                state.graph.nodes.len(),
                state.conflicts.len()
            ),
            Style::default().add_modifier(Modifier::BOLD),
        ),
        Span::raw(
            "  [Space] Toggle  [Tab] Pane  [1/2] View  [S] Save  [E] Export  [V] Verify  [q] Quit",
        ),
    ];
    if state.dirty {
        spans.push(Span::styled(
            "  *ungespeichert*",
            Style::default().fg(Color::Yellow),
        ));
    }
    if !state.status.is_empty() {
        spans.push(Span::raw(format!("  {}", state.status)));
    }
    let para = Paragraph::new(Line::from(spans))
        .block(Block::default().borders(Borders::TOP))
        .wrap(Wrap { trim: true });
    frame.render_widget(para, area);
}

/// Konflikt-Quick-Switch-Modal (zentriert, mit Aufloesungen).
pub(super) fn render_conflict_modal(
    frame: &mut Frame,
    area: Rect,
    info: &ConflictInfo,
    selected: usize,
) {
    let width = (area.width.saturating_sub(10)).clamp(40, 100);
    let height = (info.options.len() as u16 + 6)
        .max(8)
        .min(area.height.saturating_sub(4));
    let popup = centered_rect(width, height, area);
    frame.render_widget(Clear, popup);
    let mut lines = vec![
        Line::styled(
            format!("Konflikt: {}", info.message),
            Style::default().fg(Color::Red),
        ),
        Line::from(format!("Verursacher: {}", info.caused_by.join(", "))),
        Line::from(""),
    ];
    for (i, opt) in info.options.iter().enumerate() {
        let marker = if i == selected { ">" } else { " " };
        lines.push(Line::from(format!("{marker} {}", opt.label)));
    }
    if info.options.is_empty() {
        lines.push(Line::from(
            "(keine automatische Aufloesung — manuell toggeln)",
        ));
    }
    lines.push(Line::from(""));
    lines.push(Line::from("[Enter] uebernehmen  [Esc] abbrechen"));
    let para = Paragraph::new(Text::from(lines))
        .block(Block::default().borders(Borders::ALL).title("Quick-Switch"));
    frame.render_widget(para, popup);
}

/// Zentriertes Rechteck (fuer Modals).
fn centered_rect(width: u16, height: u16, area: Rect) -> Rect {
    let x = area.x + area.width.saturating_sub(width) / 2;
    let y = area.y + area.height.saturating_sub(height) / 2;
    Rect::new(x, y, width.min(area.width), height.min(area.height))
}

/// Rahmenstil (aktive Pane gelb).
pub(super) fn border_style(active: bool) -> Style {
    if active {
        Style::default().fg(Color::Yellow)
    } else {
        Style::default()
    }
}

/// Bytes menschenlesbar (`-4.2 MB`, `+0 B`).
pub fn fmt_bytes(bytes: i64) -> String {
    let abs = bytes.unsigned_abs();
    if abs >= 1_048_576 {
        format!("{:.1} MB", bytes as f64 / 1_048_576.0)
    } else if abs >= 1024 {
        format!("{:.1} kB", bytes as f64 / 1024.0)
    } else {
        format!("{bytes} B")
    }
}

/// Delta mit Vorzeichen (fuer Binaergroessen).
pub(super) fn fmt_delta(bytes: i64, unit: &str) -> String {
    if bytes > 0 {
        format!("+{} {unit}", fmt_bytes(bytes))
    } else {
        format!("{} {unit}", fmt_bytes(bytes))
    }
}

/// Farbe nach Delta (Ersparnis gruen, Kosten rot).
pub(super) fn delta_color(bytes: i64) -> Color {
    if bytes < 0 {
        Color::Green
    } else if bytes > 0 {
        Color::Red
    } else {
        Color::DarkGray
    }
}
