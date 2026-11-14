//! 13_tui_view/right — rechte Pane: Blame, Konflikte, Pros/Cons.

use ratatui::Frame;
use ratatui::layout::Rect;
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Text};
use ratatui::widgets::{Block, Borders, Paragraph, Wrap};

use crate::mod_01_types::ConflictInfo;
use crate::mod_08_delta::SharedRemovalImpact;
use crate::mod_12_tui_state::{AppState, Pane};

use super::bottom::{border_style, fmt_bytes};

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
        // Offline-USE-Flag-Doku (`use.desc`/`use.local.desc`, lokal zuerst).
        if let (Some(pkg), Some(delta)) = (
            state.selected_package(),
            state.flag_deltas.get(state.middle_selected),
        ) && let Some(text) = state.use_descriptions.describe(Some(&pkg), &delta.flag)
        {
            lines.push(Line::from(""));
            lines.push(Line::from(format!("{}: {text}", delta.flag)));
        }
        // Shared-Blocker-/Voll-Ersparnis-Analyse (nur passend zur Selektion).
        if let Some(impact) = &state.shared_impact
            && state.selected_package().as_deref() == Some(impact.package.as_str())
            && state.selected_flag().as_deref() == Some(impact.flag.as_str())
        {
            render_shared_impact(&mut lines, impact);
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

/// Voll-Ersparnis-Abschnitt: was spart das Flag, wenn Mit-Blocker
/// ebenfalls abgeschaltet bzw. Welt-Pakete entfernt werden.
fn render_shared_impact(lines: &mut Vec<Line>, impact: &SharedRemovalImpact) {
    lines.push(Line::from(""));
    lines.push(Line::styled(
        format!("Voll-Ersparnis {} [{}]:", impact.flag, impact.package),
        Style::default().add_modifier(Modifier::BOLD),
    ));
    let wfb = &impact.without_flag_blockers;
    lines.push(Line::from(format!(
        "ohne Flag-Blocker: {} bin / {} dl (-{} pkg)",
        fmt_bytes(wfb.metrics.binpkg_bytes),
        fmt_bytes(wfb.metrics.download_bytes),
        wfb.removed_packages.len()
    )));
    if !impact.flag_blockers.is_empty() {
        lines.push(Line::from("Flag aus bei:"));
        for (cpn, flag) in impact.flag_blockers.iter().take(8) {
            lines.push(Line::from(format!("  {cpn} [{flag}]")));
        }
        if impact.flag_blockers.len() > 8 {
            lines.push(Line::from("  (...)"));
        }
    }
    let wab = &impact.without_all_blockers;
    lines.push(Line::from(format!(
        "inkl. world-Entfernung: {} bin (-{} pkg)",
        fmt_bytes(wab.metrics.binpkg_bytes),
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
