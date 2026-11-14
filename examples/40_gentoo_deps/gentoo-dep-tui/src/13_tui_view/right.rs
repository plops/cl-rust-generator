//! 13_tui_view/right — rechte Pane: Blame, Konflikte, Deltas, Cut.

use ratatui::Frame;
use ratatui::layout::{Constraint, Direction, Layout, Rect};
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Text};
use ratatui::widgets::{Block, Borders, Paragraph, Wrap};

use crate::mod_01_types::ConflictInfo;
use crate::mod_12_tui_state::{AppState, Pane};

use super::bottom::{border_style, fmt_delta};


pub(super) fn render_right(frame: &mut Frame, state: &AppState, area: Rect) {
    if state.cut_plan.is_some() {
        let cut_lines = cut_plan_lines(state);
        let mut cut_h = (cut_lines.len() as u16 + 2).clamp(5, 14);
        if area.height > 8 {
            cut_h = cut_h.min(area.height - 6).max(3);
        }
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([Constraint::Min(6), Constraint::Length(cut_h)])
            .split(area);
        let top = Paragraph::new(Text::from(top_lines(state)))
            .block(
                Block::default()
                    .borders(Borders::ALL)
                    .title("Blame / Pros-Cons")
                    .border_style(border_style(state.pane == Pane::Right)),
            )
            .scroll((effective_right_scroll(state, chunks[0]), 0))
            .wrap(Wrap { trim: true });
        frame.render_widget(top, chunks[0]);
        let bottom = Paragraph::new(Text::from(cut_lines))
            .block(
                Block::default()
                    .borders(Borders::ALL)
                    .title("Cut-Analyse")
                    .border_style(border_style(state.pane == Pane::Right)),
            )
            .wrap(Wrap { trim: true });
        frame.render_widget(bottom, chunks[1]);
        return;
    }
    let para = Paragraph::new(Text::from(top_lines(state)))
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

/// Obere Box: Blame, Konflikte, Deltas, Doku, geteilte Abhaengigkeiten.
fn top_lines(state: &AppState) -> Vec<Line<'static>> {
    let mut lines: Vec<Line> = Vec::new();
    let selected = state.selected_right_target();
    lines.push(Line::styled(
        "Warum ist das hier?",
        Style::default().add_modifier(Modifier::BOLD),
    ));
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
            let hl = selected == Some(link.required_by.as_str());
            let text = match &link.via_flag {
                Some(f) => format!(
                    "  {} -> {} [+{}]",
                    link.required_by, link.package, f
                ),
                None => format!("  {} -> {}", link.required_by, link.package),
            };
            lines.push(highlight_line(text, hl));
        }
    }
    if let Some(pkg) = state.selected_package() {
        if let Some(delta) = state.flag_deltas.get(state.middle_selected) {
            let action = if delta.current {
                "Ausschalten von"
            } else {
                "Einschalten von"
            };
            if !delta.added_packages.is_empty() || !delta.removed_packages.is_empty() {
                lines.push(Line::from(""));
                lines.push(Line::styled(
                    format!("{action} {} bewirkt:", delta.flag),
                    Style::default().add_modifier(Modifier::BOLD),
                ));
                for p in delta.added_packages.iter().take(8) {
                    lines.push(pkg_delta_line(
                        format!("  + {p}"),
                        selected == Some(p.as_str()),
                        Color::Red,
                    ));
                }
                for p in delta.removed_packages.iter().take(8) {
                    lines.push(pkg_delta_line(
                        format!("  - {p}"),
                        selected == Some(p.as_str()),
                        Color::Green,
                    ));
                }
                if delta.added_packages.len() + delta.removed_packages.len() > 8 {
                    lines.push(Line::from("  (...)"));
                }
            }
            let shared = crate::mod_08_delta::shared_flag_deps(
                &state.engine,
                &state.graph,
                &pkg,
                &delta.flag,
            );
            if !shared.is_empty() {
                lines.push(Line::from(""));
                lines.push(Line::styled(
                    "Geteilte Abhaengigkeiten:",
                    Style::default().add_modifier(Modifier::BOLD),
                ));
                for (dep, others) in shared.iter().take(8) {
                    lines.push(highlight_line(
                        format!("  {dep}"),
                        selected == Some(dep.as_str()),
                    ));
                    for o in others.iter().take(4) {
                        lines.push(highlight_line(
                            format!("    gehalten von {o}"),
                            selected == Some(o.as_str()),
                        ));
                    }
                }
            }
            if let Some(c) = &delta.conflict {
                lines.push(Line::from(""));
                lines.push(Line::styled(
                    format!("Toggle-Konflikt ({}):", delta.flag),
                    Style::default().add_modifier(Modifier::BOLD).fg(Color::Red),
                ));
                lines.push(Line::styled(
                    format!("! {}", c.message),
                    Style::default().fg(Color::Red),
                ));
                for opt in c.options.iter().take(3) {
                    lines.push(Line::from(format!("  Loesung: {}", opt.label)));
                }
            }
            if !delta.has_binhost_data {
                lines.push(Line::from("bin: n/a (ohne --binhost)"));
            }
        }
        if let (Some(pkg), Some(delta)) = (
            state.selected_package(),
            state.flag_deltas.get(state.middle_selected),
        ) && let Some(text) = state.use_descriptions.describe(Some(&pkg), &delta.flag)
        {
            lines.push(Line::from(""));
            lines.push(Line::from(format!("{}: {text}", delta.flag)));
        }
    }
    lines
}

/// Delta-Paketzelle: markiert (`> ` + fett/dunkelgrau) oder farbig.
fn pkg_delta_line(text: String, marked: bool, color: Color) -> Line<'static> {
    if marked {
        let body = text.strip_prefix("  ").unwrap_or(&text);
        return Line::styled(
            format!("> {body}"),
            Style::default()
                .add_modifier(Modifier::BOLD)
                .fg(Color::DarkGray),
        );
    }
    Line::styled(text, Style::default().fg(color))
}

/// Paketzeile, markiert wenn selektiertes Rechts-Ziel (`> ` + fett).
fn highlight_line(text: String, marked: bool) -> Line<'static> {
    if marked {
        let body = text.strip_prefix("  ").unwrap_or(&text);
        Line::styled(
            format!("> {body}"),
            Style::default()
                .add_modifier(Modifier::BOLD)
                .fg(Color::DarkGray),
        )
    } else {
        Line::from(text)
    }
}

/// Scroll so anpassen, dass die markierte Zeile im oberen Viewport bleibt.
fn effective_right_scroll(state: &AppState, area: Rect) -> u16 {
    let inner = area.height.saturating_sub(2) as usize;
    if inner == 0 {
        return 0;
    }
    let Some(sel) = state.selected_right_target() else {
        return state.right_scroll;
    };
    let lines = top_lines(state);
    let marked = lines.iter().position(|l| {
        let s: String = l
            .spans
            .iter()
            .map(|sp| sp.content.as_ref().to_string())
            .collect();
        s.starts_with("> ") && s.contains(sel)
    });
    let Some(idx) = marked else {
        return state.right_scroll;
    };
    let scroll = state.right_scroll as usize;
    if idx < scroll || idx >= scroll + inner {
        idx as u16
    } else {
        state.right_scroll
    }
}

/// Untere Box: Cut-Analyse (gilt fuer jedes angesteuerte Ziel, kein
/// `target != pkg`-Filter — Auto-Cut in `Pane::Right`).
fn cut_plan_lines(state: &AppState) -> Vec<Line<'static>> {
    let mut lines: Vec<Line> = Vec::new();
    let Some(plan) = &state.cut_plan else {
        return lines;
    };
    lines.push(Line::styled(
        format!("Cut-Analyse fuer {}:", plan.target),
        Style::default().add_modifier(Modifier::BOLD),
    ));
    if plan.flag_blockers.is_empty() {
        lines.push(Line::from("keine Flag-Blocker"));
    } else {
        lines.push(Line::from("Flag-Blocker (Taste [a] wendet alle an):"));
        for b in plan.flag_blockers.iter().take(10) {
            lines.push(Line::from(format!(
                "  {} [{}]",
                b.package,
                if b.want_on {
                    format!("+{}", b.flag)
                } else {
                    format!("-{}", b.flag)
                }
            )));
        }
    }
    if plan.world_blockers.is_empty() {
        lines.push(Line::from("keine harten world-Blocker"));
    } else {
        lines.push(Line::styled(
            "Harte world-Blocker:",
            Style::default().add_modifier(Modifier::BOLD),
        ));
        for w in plan.world_blockers.iter().take(10) {
            lines.push(Line::from(format!("  {w}")));
        }
    }
    let wfb = &plan.without_flag_blockers;
    lines.push(Line::from(format!(
        "Ohne Flag-Blocker: {} / {} (-{}/+{} pkg)",
        fmt_delta(wfb.metrics.binpkg_bytes, "bin"),
        fmt_delta(wfb.metrics.download_bytes, "dl"),
        wfb.removed_packages.len(),
        wfb.added_packages.len(),
    )));
    let wab = &plan.without_all_blockers;
    lines.push(Line::from(format!(
        "Ohne alle Blocker: {} / {} (-{}/+{} pkg)",
        fmt_delta(wab.metrics.binpkg_bytes, "bin"),
        fmt_delta(wab.metrics.download_bytes, "dl"),
        wab.removed_packages.len(),
        wab.added_packages.len(),
    )));
    lines
}
