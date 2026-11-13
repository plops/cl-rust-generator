//! 13_tui_view — Ratatui-3-Pane-Layout, Blame-View, Big-Levers, Modal, Footer.
//!
//! Genau eine Zustaendigkeit: `AppState` → Widgets. Keine Logik, keine
//! Events (siehe `12_tui_state` / `14_tui_runner`).

use ratatui::Frame;
use ratatui::layout::{Constraint, Direction, Layout, Rect};
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span, Text};
use ratatui::widgets::{
    Block, Borders, Cell, Clear, List, ListItem, Paragraph, Row, Table, Tabs, Wrap,
};

use crate::mod_12_tui_state::{AppState, LeftEntry, Modal, Pane, Tab, left_label};

/// Kompletten Frame zeichnen (Tabs oben, Footer unten).
pub fn render(frame: &mut Frame, state: &AppState) {
    let area = frame.area();
    if area.width < 20 || area.height < 8 {
        frame.render_widget(Paragraph::new("Terminal zu klein"), area);
        return;
    }
    let rows = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Length(3),
            Constraint::Min(1),
            Constraint::Length(3),
        ])
        .split(area);
    render_tabs(frame, state, rows[0]);
    match state.tab {
        Tab::Packages => render_packages(frame, state, rows[1]),
        Tab::BigLevers => render_levers(frame, state, rows[1]),
    }
    render_footer(frame, state, rows[2]);
    if let Some(Modal::Conflict { conflict, selected }) = &state.modal {
        render_conflict_modal(frame, state, area, *conflict, *selected);
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
    if !state.filter.is_empty() {
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

/// Mitte: Paketdetails + USE-Flags mit Deltas, `||`-Kandidaten oder Choice-Optionen.
fn render_middle(frame: &mut Frame, state: &AppState, area: Rect) {
    let mut lines: Vec<Line> = Vec::new();
    match state.left.get(state.left_selected) {
        Some(LeftEntry::WorldPkg(cpn)) | Some(LeftEntry::Transitive(cpn)) => {
            if let Some(node) = state.graph.nodes.get(cpn) {
                lines.push(Line::from(vec![
                    Span::styled(cpn.clone(), Style::default().add_modifier(Modifier::BOLD)),
                    Span::raw(format!(
                        "  Binpkg: {}  Subgraph-DL: {}",
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
                    Span::raw(format!(" / {} dl", fmt_bytes(delta.delta.download_bytes))),
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
    let inner_height = area.height.saturating_sub(2) as usize;
    let selected_line = state.middle_selected + 2;
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

/// Rechts: Blame-View + Pros/Cons der gewaehlten Choice (falls vorhanden).
fn render_right(frame: &mut Frame, state: &AppState, area: Rect) {
    let mut lines: Vec<Line> = Vec::new();
    lines.push(Line::styled(
        "Warum ist das hier?",
        Style::default().add_modifier(Modifier::BOLD),
    ));
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
                .title("Blame / Pros-Cons"),
        )
        .scroll((state.right_scroll, 0))
        .wrap(Wrap { trim: true });
    frame.render_widget(para, area);
}

/// Tab 2: globale Hebel als Tabelle (T06 stateful, T05 dirty-Hinweis).
fn render_levers(frame: &mut Frame, state: &AppState, area: Rect) {
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
fn render_footer(frame: &mut Frame, state: &AppState, area: Rect) {
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
fn render_conflict_modal(
    frame: &mut Frame,
    state: &AppState,
    area: Rect,
    conflict: usize,
    selected: usize,
) {
    let Some(info) = state.conflicts.get(conflict) else {
        return;
    };
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
fn border_style(active: bool) -> Style {
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
fn fmt_delta(bytes: i64, unit: &str) -> String {
    if bytes > 0 {
        format!("+{} {unit}", fmt_bytes(bytes))
    } else {
        format!("{} {unit}", fmt_bytes(bytes))
    }
}

/// Farbe nach Delta (Ersparnis gruen, Kosten rot).
fn delta_color(bytes: i64) -> Color {
    if bytes < 0 {
        Color::Green
    } else if bytes > 0 {
        Color::Red
    } else {
        Color::DarkGray
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mod_02_profile::Profile;
    use crate::mod_03_manifest::parse_manifest;
    use crate::mod_05_repo_index::{PkgVersion, RepoIndex};
    use crate::mod_09_plan_toml::{ChoiceOption, Plan, PlanChoice};
    use crate::mod_12_tui_state::AppState;
    use portage_metadata::CacheEntry;
    use ratatui::Terminal;
    use ratatui::backend::TestBackend;
    use std::collections::HashMap;

    fn pkg(cpn: &str, iuse: &str, rdepend: &str, binpkg: u64) -> PkgVersion {
        let text = format!(
            "EAPI=8\nDESCRIPTION=t\nSLOT=0\nKEYWORDS=amd64\nIUSE={iuse}\n\
             RDEPEND={rdepend}\nSRC_URI=http://x/a.tar.gz\n"
        );
        let entry = CacheEntry::parse(&text).unwrap();
        let mut iuse_defaults = HashMap::new();
        for flag in &entry.metadata.iuse {
            iuse_defaults.insert(flag.name().to_string(), flag.is_enabled_default());
        }
        PkgVersion {
            cpn: cpn.into(),
            cpv: format!("{cpn}-1.0"),
            version_str: "1.0".into(),
            version: portage_atom::Version::parse("1.0").unwrap(),
            slot: "0".into(),
            masked: false,
            live: false,
            iuse_defaults,
            entry,
            manifest: parse_manifest("DIST a.tar.gz 100\n"),
            binhost_size: Some(binpkg),
        }
    }

    fn test_state<'x>(index: &'x RepoIndex, profile: &'x Profile, plan: Plan) -> AppState<'x> {
        AppState::new(index, profile, vec!["app/a".to_string()], plan, None).unwrap()
    }

    fn test_index() -> RepoIndex {
        let mut map = HashMap::new();
        for p in [
            pkg("app/a", "+ssl", "dev-libs/openssl", 100),
            pkg("dev-libs/openssl", "+ssl", "", 800),
        ] {
            map.entry(p.cpn.clone()).or_insert_with(Vec::new).push(p);
        }
        RepoIndex {
            versions: map,
            cache_files: 2,
            load_ms: 0,
        }
    }

    /// Scrolling (T06): Selektion 75 von 100 bleibt sichtbar, Eintrag 0 scrollt raus.
    #[test]
    fn scrolls_left_list_with_selection() {
        let mut map = HashMap::new();
        for i in 0..100 {
            let cpn = format!("app-misc/pkg{i:03}");
            map.entry(cpn.clone())
                .or_insert_with(Vec::new)
                .push(pkg(&cpn, "", "", 100));
        }
        let index = RepoIndex {
            versions: map,
            cache_files: 100,
            load_ms: 0,
        };
        let profile = Profile::test_default();
        let world: Vec<String> = (0..100).map(|i| format!("app-misc/pkg{i:03}")).collect();
        let mut state = AppState::new(&index, &profile, world, Plan::default(), None).unwrap();
        state.left_selected = 75;
        state.left_list_state.select(Some(75));
        let text = render_to(80, 24, &state).join("\n");
        assert!(text.contains("app-misc/pkg075"), "Selektion 75 sichtbar");
        assert!(
            !text.contains("app-misc/pkg000"),
            "Eintrag 0 herausgescrollt"
        );
    }

    /// Scrolling (T06): Flag 75 von 80 in der Mitte bleibt sichtbar.
    #[test]
    fn scrolls_middle_flags_with_selection() {
        let flags: Vec<String> = (0..80).map(|i| format!("f{i:02}")).collect();
        let mut map = HashMap::new();
        map.entry("app-misc/big".into())
            .or_insert_with(Vec::new)
            .push(pkg("app-misc/big", &flags.join(" "), "", 100));
        let index = RepoIndex {
            versions: map,
            cache_files: 1,
            load_ms: 0,
        };
        let profile = Profile::test_default();
        let mut state = AppState::new(
            &index,
            &profile,
            vec!["app-misc/big".to_string()],
            Plan::default(),
            None,
        )
        .unwrap();
        state.pane = crate::mod_12_tui_state::Pane::Middle;
        state.middle_selected = 75;
        let text = render_to(80, 24, &state).join("\n");
        assert!(text.contains("f75"), "Flag 75 sichtbar");
    }

    /// Buffer als Textzeilen (fuer `contains`-Pruefungen).
    fn buffer_text(backend: &TestBackend) -> Vec<String> {
        let buf = backend.buffer();
        let area = buf.area;
        (0..area.height)
            .map(|y| {
                (0..area.width)
                    .map(|x| buf[(x, y)].symbol().to_string())
                    .collect::<String>()
                    .trim_end()
                    .to_string()
            })
            .collect()
    }

    fn render_to(width: u16, height: u16, state: &AppState) -> Vec<String> {
        let backend = TestBackend::new(width, height);
        let mut terminal = Terminal::new(backend).unwrap();
        terminal.draw(|frame| render(frame, state)).unwrap();
        buffer_text(terminal.backend())
    }

    #[test]
    fn renders_three_panes_and_footer() {
        let index = test_index();
        let profile = Profile::test_default();
        let state = test_state(&index, &profile, Plan::default());
        for (w, h) in [(120, 40), (80, 24)] {
            let text = render_to(w, h, &state).join("\n");
            assert!(text.contains("app/a"), "Paket fehlt ({w}x{h})");
            assert!(text.contains("ssl"), "Flag fehlt ({w}x{h})");
            assert!(
                text.contains("Warum ist das hier?"),
                "Blame fehlt ({w}x{h})"
            );
            assert!(text.contains("Toggle"), "Footer fehlt ({w}x{h})");
        }
    }

    #[test]
    fn renders_pros_cons_and_blame_chain() {
        let index = test_index();
        let profile = Profile::test_default();
        let mut plan = Plan::default();
        plan.choice.push(PlanChoice {
            id: "crypto".into(),
            required: true,
            description: "tls".into(),
            selected: 0,
            option: vec![ChoiceOption {
                atom: "dev-libs/openssl".into(),
                pros: "ueberall verfuegbar".into(),
                cons: "grosses Binary".into(),
                suggested_use: vec![],
            }],
        });
        let mut state = test_state(&index, &profile, plan);
        // Transitives Paket selektieren (hinter Welt + Choice-Eintraegen).
        state
            .select_left(
                state
                    .left
                    .iter()
                    .position(|e| *e == LeftEntry::Transitive("dev-libs/openssl".into()))
                    .unwrap(),
            )
            .unwrap();
        let text = render_to(120, 40, &state).join("\n");
        assert!(text.contains("dev-libs/openssl"));
        assert!(text.contains("Pros: ueberall verfuegbar"));
        assert!(text.contains("Cons: grosses Binary"));
        assert!(text.contains("->"), "Blame-Pfeil fehlt");
    }

    #[test]
    fn renders_big_levers_and_conflict_modal() {
        let mut map = HashMap::new();
        for p in [
            pkg("app/a", "bar", "dev-libs/libx[foo]", 10),
            pkg("app/c", "baz", "dev-libs/libx[-foo]", 10),
            pkg("dev-libs/libx", "foo", "", 50),
        ] {
            map.entry(p.cpn.clone()).or_insert_with(Vec::new).push(p);
        }
        let index = RepoIndex {
            versions: map,
            cache_files: 3,
            load_ms: 0,
        };
        let profile = Profile::test_default();
        let mut state = AppState::new(
            &index,
            &profile,
            vec!["app/a".to_string(), "app/c".to_string()],
            Plan::default(),
            None,
        )
        .unwrap();
        state
            .engine
            .overrides
            .insert("app/a".into(), [("bar".into(), true)].into_iter().collect());
        state
            .engine
            .overrides
            .insert("app/c".into(), [("baz".into(), true)].into_iter().collect());
        state.refresh().unwrap();
        state.set_tab(Tab::BigLevers).unwrap();
        let text = render_to(120, 40, &state).join("\n");
        assert!(text.contains("Big Levers"));
        assert!(text.contains("Pakete"));
        // Konflikt-Modal rendern.
        state.set_tab(Tab::Packages).unwrap();
        if !state.conflicts.is_empty() {
            state.modal = Some(Modal::Conflict {
                conflict: 0,
                selected: 0,
            });
            let text = render_to(120, 40, &state).join("\n");
            assert!(text.contains("Quick-Switch"));
        }
    }
}
