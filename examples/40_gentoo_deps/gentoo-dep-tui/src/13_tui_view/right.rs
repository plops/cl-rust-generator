//! 13_tui_view/right — rechte Pane: Blame, Konflikte, Deltas, Cut.

use ratatui::Frame;
use ratatui::layout::Rect;
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Text};
use ratatui::widgets::{Block, Borders, Paragraph, Wrap};

use crate::mod_01_types::ConflictInfo;
use crate::mod_08_delta::SharedRemovalImpact;
use crate::mod_12_tui_state::{AppState, Pane};

use super::bottom::{border_style, fmt_bytes, fmt_delta};

pub(super) fn render_right(frame: &mut Frame, state: &AppState, area: Rect) {
    let mut lines: Vec<Line> = Vec::new();
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
        if let Some(impact) = &state.shared_impact
            && state.selected_package().as_deref() == Some(impact.package.as_str())
            && state.selected_flag().as_deref() == Some(impact.flag.as_str())
        {
            render_shared_impact(&mut lines, impact);
        }
        render_cut_plan(&mut lines, state, &pkg);
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

/// Voll-Ersparnis-Abschnitt.
fn render_shared_impact(lines: &mut Vec<Line>, impact: &SharedRemovalImpact) {
    lines.push(Line::from(""));
    lines.push(Line::styled(
        format!("Voll-Ersparnis {} [{}]:", impact.flag, impact.package),
        Style::default().add_modifier(Modifier::BOLD),
    ));
    let wfb = &impact.without_flag_blockers;
    lines.push(Line::from(format!(
        "ohne Flag-Blocker: {} / {} (-{} pkg)",
        fmt_delta(wfb.metrics.binpkg_bytes, "bin"),
        fmt_delta(wfb.metrics.download_bytes, "dl"),
        wfb.removed_packages.len()
    )));
    if !impact.flag_blockers.is_empty() {
        lines.push(Line::from("Flag aus bei:"));
        for b in impact.flag_blockers.iter().take(8) {
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
        if impact.flag_blockers.len() > 8 {
            lines.push(Line::from("  (...)"));
        }
    }
    let wab = &impact.without_all_blockers;
    lines.push(Line::from(format!(
        "inkl. world-Entfernung: {} (-{} pkg)",
        fmt_delta(wab.metrics.binpkg_bytes, "bin"),
        wab.removed_packages.len()
    )));
    if !impact.world_blockers.is_empty() {
        lines.push(Line::from("world entfernen:"));
        for w in impact.world_blockers.iter().take(8) {
            lines.push(Line::from(format!("  {w}")));
        }
        if impact.world_blockers.len() > 8 {
            lines.push(Line::from("  (...)"));
        }
    }
}

/// Cut-Analyse (`x`) zum selektierten Paket.
fn render_cut_plan(lines: &mut Vec<Line>, state: &AppState, pkg: &str) {
    let Some(plan) = &state.cut_plan else {
        return;
    };
    if plan.target != pkg {
        return;
    }
    lines.push(Line::from(""));
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
    let _ = fmt_bytes(0);
}
