//! 13_tui_view/middle — mittlere Pane: Paketdetails und USE-Flag-Deltas.

use ratatui::Frame;
use ratatui::layout::Rect;
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span, Text};
use ratatui::widgets::{Block, Borders, Paragraph};

use crate::mod_12_tui_state::{AppState, LeftEntry, Pane};

use super::bottom::{border_style, delta_color, fmt_bytes, fmt_delta};

pub(super) fn render_middle(frame: &mut Frame, state: &AppState, area: Rect) {
    let mut lines: Vec<Line> = Vec::new();
    match state.left.get(state.left_selected) {
        Some(LeftEntry::WorldPkg(cpn)) | Some(LeftEntry::Transitive(cpn)) => {
            if let Some(node) = state.graph.nodes.get(cpn) {
                lines.push(Line::from(vec![
                    Span::styled(cpn.clone(), Style::default().add_modifier(Modifier::BOLD)),
                    Span::raw(format!(
                        "  Binpkg: {}  Pkg-Quell-DL: {}",
                        fmt_bytes(node.binpkg as i64),
                        fmt_bytes(node.download as i64)
                    )),
                ]));
                lines.push(Line::from(""));
            }
            if state.deltas_loading {
                lines.push(Line::from(vec![Span::styled(
                    "(rechne Deltas...)",
                    Style::default().fg(Color::Yellow),
                )]));
            }
            for (i, delta) in state.flag_deltas.iter().enumerate() {
                let checkbox = if delta.current { "[x]" } else { "[ ]" };
                let bin = if delta.has_binhost_data {
                    fmt_delta(delta.delta.binpkg_bytes, "bin")
                } else {
                    "n/a (ohne --binhost)".to_string()
                };
                let mut spans = vec![
                    Span::raw(format!("{checkbox} {} ", delta.flag)),
                    Span::styled(
                        bin,
                        Style::default().fg(delta_color(delta.delta.binpkg_bytes)),
                    ),
                    Span::raw(format!(
                        " / {}",
                        fmt_delta(delta.delta.download_bytes, "dl")
                    )),
                ];
                // Paket-Differenz kurz anzeigen (T08).
                if !delta.added_packages.is_empty() || !delta.removed_packages.is_empty() {
                    spans.push(Span::raw(format!(
                        " (+{}/-{} pkg)",
                        delta.added_packages.len(),
                        delta.removed_packages.len()
                    )));
                }
                if delta.conflict.is_some() {
                    spans.push(Span::styled(" CONFLICT", Style::default().fg(Color::Red)));
                }
                if i == state.middle_selected && state.pane == Pane::Middle {
                    lines.push(Line::from(spans).style(Style::default().bg(Color::DarkGray)));
                } else {
                    lines.push(Line::from(spans));
                }
            }
            if state.flag_deltas.is_empty() && !state.deltas_loading {
                lines.push(Line::from("(keine schaltbaren Flags)"));
            }
        }
        Some(LeftEntry::OrDecision(group)) => {
            if let Some(alt) = state
                .graph
                .alternatives
                .iter()
                .find(|a| &a.group_id == group)
            {
                lines.push(Line::from(vec![Span::styled(
                    format!("|| Entscheidung: {}", alt.owner),
                    Style::default().add_modifier(Modifier::BOLD),
                )]));
                for (i, cand) in alt.candidates.iter().enumerate() {
                    let marker = if i == alt.selected { ">" } else { " " };
                    let line = Line::from(format!("{marker} {cand}"));
                    if i == state.middle_selected && state.pane == Pane::Middle {
                        lines.push(line.style(Style::default().bg(Color::DarkGray)));
                    } else {
                        lines.push(line);
                    }
                }
            }
        }
        Some(LeftEntry::PlanChoice(id)) => {
            if let Some(choice) = state.plan.choice.iter().find(|c| &c.id == id) {
                lines.push(Line::from(vec![Span::styled(
                    format!("Choice: {} — {}", choice.id, choice.description),
                    Style::default().add_modifier(Modifier::BOLD),
                )]));
                lines.push(Line::from("[Enter] Option wechseln"));
                for (i, opt) in choice.option.iter().enumerate() {
                    let marker = if i == choice.selected { ">" } else { " " };
                    let line = Line::from(format!(
                        "{marker} {}  (Pros: {} / Cons: {})",
                        opt.atom, opt.pros, opt.cons
                    ));
                    if i == state.middle_selected && state.pane == Pane::Middle {
                        lines.push(line.style(Style::default().bg(Color::DarkGray)));
                    } else {
                        lines.push(line);
                    }
                }
            }
        }
        None => lines.push(Line::from("(leer)")),
    }
    // Scroll-Offset: selektierte Zeile immer sichtbar (T06).
    // Kopfzeilen: Paket-Zusammenfassung + Leerzeile + optional
    // `(rechne Deltas...)` — sonst scrollt die Selektion weg (Task 4.2).
    let inner_height = area.height.saturating_sub(2) as usize;
    let header = 2 + usize::from(state.deltas_loading);
    let selected_line = state.middle_selected + header;
    let offset = if inner_height == 0 {
        0
    } else {
        selected_line.saturating_sub(inner_height.saturating_sub(1))
    } as u16;
    let para = Paragraph::new(Text::from(lines))
        .block(
            Block::default()
                .borders(Borders::ALL)
                .title("Details / USE-Flags")
                .border_style(border_style(state.pane == Pane::Middle)),
        )
        .scroll((offset, 0));
    // Absichtlich kein Wrap: jede Zeile = ein Flag, Scroll-Offset bleibt exakt.
    frame.render_widget(para, area);
}
