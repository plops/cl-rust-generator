//! 13_tui_view/bottom — Hebel-Tabelle, Footer, Konflikt-Modal, Formathelfer.

use ratatui::Frame;
use ratatui::layout::{Constraint, Direction, Layout, Rect};
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span, Text};
use ratatui::widgets::{Block, Borders, Cell, Clear, Paragraph, Row, Table, Wrap};

use crate::mod_01_types::ConflictInfo;
use crate::mod_12_tui_state::AppState;

pub(super) fn render_levers(frame: &mut Frame, state: &AppState, area: Rect) {
    // Detailbox zum selektierten Hebel (Beschreibung, betroffene Pakete,
    // hinzu/entfernt) — nur wenn genug Hoehe bleibt (review3 Task 4.3).
    let detail = lever_detail_lines(state);
    if detail.is_empty() || area.height < 14 {
        render_lever_table(frame, state, area);
        return;
    }
    let detail_h = ((detail.len() as u16) + 2).clamp(4, 9);
    let rows = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Min(5), Constraint::Length(detail_h)])
        .split(area);
    render_lever_table(frame, state, rows[0]);
    let para = Paragraph::new(Text::from(detail)).block(
        Block::default()
            .borders(Borders::ALL)
            .title("Hebel-Details"),
    );
    frame.render_widget(para, rows[1]);
}

/// Detailzeilen zum selektierten Big Lever (leer = keine Box).
fn lever_detail_lines(state: &AppState) -> Vec<Line<'static>> {
    let Some(lever) = state.levers.get(state.lever_selected) else {
        return Vec::new();
    };
    let mut lines = vec![Line::styled(
        format!("Flag {}", lever.flag),
        Style::default().add_modifier(Modifier::BOLD),
    )];
    if let Some(text) = state.use_descriptions.describe(None, &lever.flag) {
        lines.push(Line::from(text.to_string()));
    }
    let mut affected = lever.affected_packages.clone();
    affected.sort();
    let shown: Vec<&str> = affected.iter().take(6).map(String::as_str).collect();
    let mut summary = format!(
        "Betroffene Pakete ({}): {}",
        lever.packages,
        shown.join(", ")
    );
    if affected.len() > 6 {
        summary.push_str(", ...");
    }
    lines.push(Line::from(summary));
    if !lever.removed_packages.is_empty() {
        let shown: Vec<&str> = lever
            .removed_packages
            .iter()
            .take(6)
            .map(String::as_str)
            .collect();
        let mut s = format!(
            "Entfernt: {} ({})",
            shown.join(", "),
            fmt_bytes(lever.total.binpkg_bytes)
        );
        if lever.removed_packages.len() > 6 {
            s.push_str(", ...");
        }
        lines.push(Line::styled(s, Style::default().fg(Color::Green)));
    }
    if !lever.added_packages.is_empty() {
        let shown: Vec<&str> = lever
            .added_packages
            .iter()
            .take(6)
            .map(String::as_str)
            .collect();
        let mut s = format!("Hinzu: {}", shown.join(", "));
        if lever.added_packages.len() > 6 {
            s.push_str(", ...");
        }
        lines.push(Line::styled(s, Style::default().fg(Color::Red)));
    }
    lines
}

fn render_lever_table(frame: &mut Frame, state: &AppState, area: Rect) {
    let header = Row::new(vec![
        "Flag",
        "An?",
        "Pakete",
        "Pkg-Delta",
        "Bin-Delta",
        "DL-Delta",
        "Konflikte",
    ])
    .style(Style::default().add_modifier(Modifier::BOLD));
    let rows: Vec<Row> = state
        .levers
        .iter()
        .map(|lever| {
            Row::new(vec![
                Cell::from(lever.flag.clone()),
                Cell::from(if lever.turn_on { "[AN]" } else { "[AUS]" }.to_string()),
                Cell::from(lever.packages.to_string()),
                Cell::from(format!("{:+}", lever.net_pkg_delta)),
                Cell::from(fmt_bytes(lever.total.binpkg_bytes)),
                Cell::from(fmt_bytes(lever.total.download_bytes)),
                Cell::from(lever.conflicts.to_string()),
            ])
        })
        .collect();
    let widths = [
        Constraint::Min(10),
        Constraint::Length(6),
        Constraint::Length(8),
        Constraint::Length(10),
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

/// Footer-Hoehe fuer die Breite (Rahmen oben + umbrochene Zeilen, review3 5.1).
pub(super) fn footer_height(state: &AppState, width: u16) -> u16 {
    let w = (width as usize).max(20);
    let rows = totals_text(state).len().div_ceil(w) + keys_text(state).len().div_ceil(w);
    (rows.max(2) as u16 + 1).clamp(3, 7)
}

/// Totals-Zeile: eindeutige Metriken (review3 Task 5.1).
fn totals_text(state: &AppState) -> String {
    let totals = state.graph.totals();
    let bin = if state.engine.index.has_any_binhost() {
        let hits = state.graph.nodes.values().filter(|n| n.binpkg > 0).count();
        format!(
            "Binpkg {} ({}/{} Hits)",
            fmt_bytes(totals.binpkg_bytes),
            hits,
            state.graph.nodes.len()
        )
    } else {
        "Binpkg n/a (ohne --binhost)".to_string()
    };
    format!(
        "{bin}  Quell-DL {}  Build-Only {} bin / {} dl  Pakete {}  Konflikte {}",
        fmt_bytes(totals.download_bytes),
        fmt_bytes(state.graph.build_extra.binpkg_bytes),
        fmt_bytes(state.graph.build_extra.download_bytes),
        state.graph.nodes.len(),
        state.conflicts.len()
    )
}

/// Tasten-Zeile inkl. Dirty-Markierung und Status.
fn keys_text(state: &AppState) -> String {
    let mut s = String::from(
        "[Space] Toggle  [Tab] Pane  [1/2] View  [/] Suche  [o] Sort  [x] Cut  [a] Apply  [u] Undo  [S] package.use  [?] Hilfe  [q] Quit",
    );
    if state.dirty {
        s.push_str("  *ungespeichert*");
    }
    if !state.status.is_empty() {
        s.push_str(&format!("  {}", state.status));
    }
    s
}

/// Footer: Totals, Paket-/Konflikt-Zaehler, Tastenbelegung.
pub(super) fn render_footer(frame: &mut Frame, state: &AppState, area: Rect) {
    let top = Line::styled(
        totals_text(state),
        Style::default().add_modifier(Modifier::BOLD),
    );
    let mut key_spans = vec![Span::raw(
        "  [Space] Toggle  [Tab] Pane  [1/2] View  [/] Suche  [o] Sort  [x] Cut  [a] Apply  [u] Undo  [S] package.use  [?] Hilfe  [q] Quit",
    )];
    if state.dirty {
        key_spans.push(Span::styled(
            "  *ungespeichert*",
            Style::default().fg(Color::Yellow),
        ));
    }
    if !state.status.is_empty() {
        key_spans.push(Span::raw(format!("  {}", state.status)));
    }
    let para = Paragraph::new(Text::from(vec![top, Line::from(key_spans)]))
        .block(Block::default().borders(Borders::TOP))
        .wrap(Wrap { trim: true });
    frame.render_widget(para, area);
}

/// Hilfe-Overlay (`?`/`F1`, review3 Task 5.2).
pub(super) fn render_help_modal(frame: &mut Frame, area: Rect) {
    let lines = vec![
        Line::styled(
            "Metriken lesen:",
            Style::default().add_modifier(Modifier::BOLD),
        ),
        Line::from("Binpkg (Binhost, Trefferquote) / Quell-DL (Tarballs)"),
        Line::from("Build-Only (BDEPEND, bin+src) / Pkg-Quell-DL (nur Paket)"),
        Line::from("0 B-Delta: geteilte Abh. oder Compile-only-Flag"),
        Line::from("Voll-Ersparnis: rechte Pane (Flag-Blocker/world-Entfernung)"),
        Line::from(""),
        Line::styled("Navigation:", Style::default().add_modifier(Modifier::BOLD)),
        Line::from("/ Suche (Esc loescht Filter), Tab Pane, Space Toggle"),
        Line::from("Enter Auswahl, 1/2 Tabs, j/k Scrollen, o Sort, x Cut, a Apply, u Undo"),
        Line::from(""),
        Line::styled("Ablauf:", Style::default().add_modifier(Modifier::BOLD)),
        Line::from("[S] package.use schreiben"),
        Line::from(""),
        Line::from("[Esc]/[?]/[q] schliesst Hilfe"),
    ];
    let width = (lines.iter().map(|l| l.width()).max().unwrap_or(40) as u16 + 4)
        .clamp(40, area.width.saturating_sub(4).max(40));
    let height = (lines.len() as u16 + 2).min(area.height.saturating_sub(2).max(4));
    let popup = centered_rect(width, height, area);
    frame.render_widget(Clear, popup);
    let para = Paragraph::new(Text::from(lines))
        .block(Block::default().borders(Borders::ALL).title("Hilfe"))
        .wrap(Wrap { trim: true });
    frame.render_widget(para, popup);
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
pub fn fmt_delta(bytes: i64, unit: &str) -> String {
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
