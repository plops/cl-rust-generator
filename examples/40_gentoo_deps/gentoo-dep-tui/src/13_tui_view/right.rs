//! 13_tui_view/right — rechte Pane: Blame, Konflikte, Pros/Cons.

use ratatui::Frame;
use ratatui::layout::Rect;
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Text};
use ratatui::widgets::{Block, Borders, Paragraph, Wrap};

use crate::mod_01_types::ConflictInfo;
use crate::mod_12_tui_state::{AppState, Pane};

use super::bottom::border_style;

pub(super) fn render_right(frame: &mut Frame, state: &AppState, area: Rect) {
    let mut lines: Vec<Line> = Vec::new();
    lines.push(Line::styled(
        "Warum ist das hier?",
        Style::default().add_modifier(Modifier::BOLD),
    ));
    // Konflikte des selektierten Pakets direkt in der rechten Pane
    // (Task 4.2) — sonst faellt ein rotes Flag im Blame nicht auf.
    if let Some(pkg) = state.selected_package() {
        let pkg_conflicts: Vec<&ConflictInfo> = state
            .conflicts
            .iter()
            .filter(|c| c.package == pkg || c.caused_by.contains(&pkg))
            .collect();
        if !pkg_conflicts.is_empty() {
            lines.push(Line::from(""));
            lines.push(Line::styled(
                "Konflikte:",
                Style::default().add_modifier(Modifier::BOLD).fg(Color::Red),
            ));
            for c in pkg_conflicts.iter().take(3) {
                lines.push(Line::styled(
                    format!("! {}", c.message),
                    Style::default().fg(Color::Red),
                ));
            }
        }
    }
    let chains = state.selected_blame();
    if chains.is_empty() {
        lines.push(Line::from("(kein Pfad — Welt-Paket oder leer)"));
    }
    for (i, chain) in chains.iter().take(4).enumerate() {
        lines.push(Line::from(format!("Kette {}:", i + 1)));
        for link in chain {
            match &link.via_flag {
                Some(f) => lines.push(Line::from(format!(
                    "  {} -> {} [+{}]",
                    link.required_by, link.package, f
                ))),
                None => lines.push(Line::from(format!(
                    "  {} -> {}",
                    link.required_by, link.package
                ))),
            }
        }
    }
    // Pros/Cons: Choice suchen, deren gewaehlte Option das Paket ist.
    if let Some(pkg) = state.selected_package() {
        for choice in &state.plan.choice {
            if let Some(opt) = choice.option.get(choice.selected)
                && opt.atom == pkg
            {
                lines.push(Line::from(""));
                lines.push(Line::styled(
                    format!("Choice: {}", choice.id),
                    Style::default().add_modifier(Modifier::BOLD),
                ));
                lines.push(Line::from(format!("Pros: {}", opt.pros)));
                lines.push(Line::from(format!("Cons: {}", opt.cons)));
            }
        }
        // Paket-Diff des selektierten Flags (T08).
        if let Some(delta) = state.flag_deltas.get(state.middle_selected) {
            if !delta.added_packages.is_empty() || !delta.removed_packages.is_empty() {
                lines.push(Line::from(""));
                lines.push(Line::styled(
                    format!("Flag {} zieht:", delta.flag),
                    Style::default().add_modifier(Modifier::BOLD),
                ));
                for p in delta.added_packages.iter().take(8) {
                    lines.push(Line::styled(
                        format!("  + {p}"),
                        Style::default().fg(Color::Green),
                    ));
                }
                for p in delta.removed_packages.iter().take(8) {
                    lines.push(Line::styled(
                        format!("  - {p}"),
                        Style::default().fg(Color::Red),
                    ));
                }
                if delta.added_packages.len() + delta.removed_packages.len() > 8 {
                    lines.push(Line::from("  (...)"));
                }
            }
            if !delta.has_binhost_data {
                lines.push(Line::from("bin: n/a (ohne --binhost)"));
            }
        }
    }
    let para = Paragraph::new(Text::from(lines))
        .block(
            Block::default()
                .borders(Borders::ALL)
                .title("Blame / Pros-Cons")
                .border_style(border_style(state.pane == Pane::Right)),
        )
        .scroll((state.right_scroll, 0))
        .wrap(Wrap { trim: true });
    frame.render_widget(para, area);
}
