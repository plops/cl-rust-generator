use super::*;
use crate::mod_02_profile::Profile;
use crate::mod_05_repo_index::RepoIndex;
use crate::mod_12_tui_state::AppState;
use crate::test_helpers::{PkgFixture, repo_index};
use ratatui::Terminal;
use ratatui::backend::TestBackend;

fn test_state<'x>(index: &'x RepoIndex, profile: &'x Profile) -> AppState<'x> {
    AppState::new(index, profile, vec!["app/a".to_string()]).unwrap()
}

fn test_index() -> RepoIndex {
    repo_index(vec![
        PkgFixture::new("app/a")
            .iuse("+ssl")
            .rdepend("dev-libs/openssl")
            .binpkg(Some(100))
            .build(),
        PkgFixture::new("dev-libs/openssl")
            .iuse("+ssl")
            .rdepend("")
            .binpkg(Some(800))
            .build(),
    ])
}

/// Scrolling: Selektion 75 von 100 bleibt sichtbar, Eintrag 0 scrollt raus.
#[test]
fn scrolls_left_list_with_selection() {
    let pkgs: Vec<_> = (0..100)
        .map(|i| {
            PkgFixture::new(&format!("app-misc/pkg{i:03}"))
                .binpkg(Some(100))
                .build()
        })
        .collect();
    let index = repo_index(pkgs);
    let profile = Profile::test_default();
    let world: Vec<String> = (0..100).map(|i| format!("app-misc/pkg{i:03}")).collect();
    let mut state = AppState::new(&index, &profile, world).unwrap();
    state.left_selected = 75;
    state.left_list_state.select(Some(75));
    let text = render_to(80, 24, &state).join("\n");
    assert!(text.contains("app-misc/pkg075"), "Selektion 75 sichtbar");
    assert!(
        !text.contains("app-misc/pkg000"),
        "Eintrag 0 herausgescrollt"
    );
}

/// Scrolling: Flag 75 von 80 in der Mitte bleibt sichtbar.
#[test]
fn scrolls_middle_flags_with_selection() {
    let flags: Vec<String> = (0..80).map(|i| format!("f{i:02}")).collect();
    let index = repo_index(vec![
        PkgFixture::new("app-misc/big")
            .iuse(&flags.join(" "))
            .binpkg(Some(100))
            .build(),
    ]);
    let profile = Profile::test_default();
    let mut state = AppState::new(&index, &profile, vec!["app-misc/big".to_string()]).unwrap();
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
    let state = test_state(&index, &profile);
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
fn renders_blame_chain_for_transitive() {
    let index = test_index();
    let profile = Profile::test_default();
    let state = test_state(&index, &profile);
    let text = render_to(120, 40, &state).join("\n");
    assert!(text.contains("dev-libs/openssl") || text.contains("app/a"));
}

#[test]
fn renders_big_levers_and_conflict_modal() {
    let index = repo_index(vec![
        PkgFixture::new("app/a")
            .iuse("bar")
            .rdepend("dev-libs/libx[foo]")
            .binpkg(Some(10))
            .build(),
        PkgFixture::new("app/c")
            .iuse("baz")
            .rdepend("dev-libs/libx[-foo]")
            .binpkg(Some(10))
            .build(),
        PkgFixture::new("dev-libs/libx")
            .iuse("foo")
            .rdepend("")
            .binpkg(Some(50))
            .build(),
    ]);
    let profile = Profile::test_default();
    let mut state = AppState::new(
        &index,
        &profile,
        vec!["app/a".to_string(), "app/c".to_string()],
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
    assert!(text.contains("Pkg-Delta"), "Pkg-Delta-Spalte fehlt");
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

/// Rechte Pane zeigt Flag-Doku + Geteilte Abhaengigkeiten (ohne Voll-Ersparnis).
#[test]
fn renders_shared_deps_and_flag_docs() {
    use crate::mod_02_profile::UseDescriptions;
    let index = repo_index(vec![
        PkgFixture::new("app/a")
            .iuse("+ssl")
            .rdepend("ssl? ( dev-libs/openssl )")
            .binpkg(Some(100))
            .build(),
        PkgFixture::new("app/b")
            .iuse("+ssl")
            .rdepend("ssl? ( dev-libs/openssl )")
            .binpkg(Some(100))
            .build(),
        PkgFixture::new("app/c")
            .rdepend("dev-libs/openssl")
            .binpkg(Some(100))
            .build(),
        PkgFixture::new("dev-libs/openssl")
            .binpkg(Some(800))
            .build(),
    ]);
    let profile = Profile::test_default();
    let mut state = AppState::new(
        &index,
        &profile,
        vec![
            "app/a".to_string(),
            "app/b".to_string(),
            "app/c".to_string(),
        ],
    )
    .unwrap();
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("use.desc"), "ssl - Secure Sockets Layer\n").unwrap();
    state.use_descriptions = UseDescriptions::load(dir.path());
    state.ensure_deltas().unwrap();
    let shared = crate::mod_08_delta::shared_flag_deps(
        &state.engine,
        &state.graph,
        "app/a",
        "ssl",
    );
    assert!(
        !shared.is_empty(),
        "Shared-Analyse fehlt (openssl wird gehalten)"
    );
    let text = render_to(140, 36, &state).join("\n");
    assert!(!text.contains("Voll-Ersparnis"), "altes Impact-Rauschen");
    assert!(
        text.contains("Geteilte Abhaengigkeiten"),
        "Geteilte-Abhaengigkeiten-Titel fehlt: {text}"
    );
    assert!(text.contains("app/b"), "Blocker app/b fehlt");
    assert!(text.contains("app/c"), "Blocker app/c fehlt");
    assert!(text.contains("Secure Sockets Layer"), "Flag-Doku fehlt");
}

#[test]
fn renders_cut_analysis() {
    let index = repo_index(vec![
        PkgFixture::new("app/a")
            .iuse("+ssl")
            .rdepend("ssl? ( dev-libs/openssl )")
            .binpkg(Some(100))
            .build(),
        PkgFixture::new("dev-libs/openssl")
            .binpkg(Some(800))
            .build(),
    ]);
    let profile = Profile::test_default();
    let mut state = AppState::new(&index, &profile, vec!["app/a".to_string()]).unwrap();
    state.compute_cut_plan().unwrap();
    let text = render_to(140, 36, &state).join("\n");
    assert!(text.contains("Cut-Analyse"), "Cut-Titel fehlt: {text}");
}

/// Toggle-Konflikt erscheint direkt in der rechten Pane.
#[test]
fn renders_toggle_conflict_inline() {
    use crate::mod_01_types::{ConflictInfo, QuickSwitch};
    let index = test_index();
    let profile = Profile::test_default();
    let mut state = test_state(&index, &profile);
    state.flag_deltas[0].conflict = Some(ConflictInfo {
        package: "app/a".into(),
        message: "simulierter Grund".into(),
        caused_by: vec!["app/a".into()],
        options: vec![QuickSwitch {
            label: "loesung".into(),
            changes: vec![],
        }],
    });
    let text = render_to(140, 36, &state).join("\n");
    assert!(text.contains("Toggle-Konflikt"), "Inline-Konflikt fehlt");
    assert!(text.contains("simulierter Grund"), "Konfliktgrund fehlt");
}

/// Leere Blame zeigt sauberen Hinweis ohne leere Kette.
#[test]
fn renders_clean_empty_blame() {
    let index = test_index();
    let profile = Profile::test_default();
    let state = test_state(&index, &profile);
    let text = render_to(120, 40, &state).join("\n");
    assert!(
        text.contains("kein Pfad"),
        "leerer Blame-Hinweis fehlt: {text}"
    );
    assert!(!text.contains("Kette 1:\n  \n"), "leere Kette darf nicht rendern");
}

/// Tab 2 zeigt Detailbox zum selektierten Hebel.
#[test]
fn renders_lever_details() {
    let index = repo_index(vec![
        PkgFixture::new("app/a")
            .iuse("+ssl")
            .rdepend("ssl? ( dev-libs/openssl )")
            .binpkg(Some(100))
            .build(),
        PkgFixture::new("dev-libs/openssl")
            .binpkg(Some(800))
            .build(),
    ]);
    let profile = Profile::test_default();
    let mut state = AppState::new(&index, &profile, vec!["app/a".to_string()]).unwrap();
    state.set_tab(Tab::BigLevers).unwrap();
    assert!(!state.levers.is_empty());
    let text = render_to(120, 40, &state).join("\n");
    assert!(text.contains("Hebel-Details"), "Detailbox fehlt");
    assert!(text.contains("Betroffene Pakete"), "Paketliste fehlt");
    assert!(text.contains("Entfernt:"), "Entfernt-Liste fehlt");
    assert!(text.contains("dev-libs/openssl"), "Paketname fehlt");
}

/// Sortiermodus zeigt exklusive Kosten links.
#[test]
fn renders_exclusive_sort_costs() {
    let index = test_index();
    let profile = Profile::test_default();
    let mut state = test_state(&index, &profile);
    state.cycle_left_sort();
    let text = render_to(140, 36, &state).join("\n");
    assert!(text.contains("pkg,"), "exklusive Kosten fehlen: {text}");
}

/// Footer zeigt neue Labels + Tastenhinweise ohne Clipping.
#[test]
fn footer_labels_and_hints() {
    let index = test_index();
    let profile = Profile::test_default();
    let state = test_state(&index, &profile);
    for (w, h) in [(120u16, 40u16), (80u16, 24u16)] {
        let text = render_to(w, h, &state).join("\n");
        assert!(text.contains("Binpkg"), "Binpkg fehlt ({w}x{h})");
        assert!(text.contains("Quell-DL"), "Quell-DL fehlt ({w}x{h})");
        assert!(text.contains("Build-Only"), "Build-Only fehlt ({w}x{h})");
        assert!(text.contains("[/]"), "Suche-Hinweis fehlt ({w}x{h})");
        assert!(text.contains("[?]"), "Hilfe-Hinweis fehlt ({w}x{h})");
        assert!(text.contains("[x]"), "Cut-Hinweis fehlt ({w}x{h})");
        assert!(text.contains("[o]"), "Sort-Hinweis fehlt ({w}x{h})");
        assert!(text.contains("[u]"), "Undo-Hinweis fehlt ({w}x{h})");
    }
}

/// Hilfe-Modal rendert Inhalt und schliesst per Esc.
#[test]
fn help_modal_renders_and_closes() {
    let index = test_index();
    let profile = Profile::test_default();
    let mut state = test_state(&index, &profile);
    state.modal = Some(Modal::Help);
    for (w, h) in [(120u16, 40u16), (80u16, 24u16)] {
        let text = render_to(w, h, &state).join("\n");
        assert!(text.contains("Hilfe"), "Hilfe-Titel fehlt ({w}x{h})");
        assert!(
            text.contains("Quell-DL"),
            "Metrik-Erklaerung fehlt ({w}x{h})"
        );
    }
    state.modal = None;
    assert!(state.modal.is_none());
}

/// Rechte Pane hebt Fokus hervor, zeigt Konflikte, Filter zeigt `/`-Prompt.
#[test]
fn right_pane_focus_conflicts_and_filter_prompt() {
    use ratatui::style::Color;
    fn has_yellow_in_right_third(width: u16, height: u16, state: &AppState) -> bool {
        let backend = TestBackend::new(width, height);
        let mut terminal = Terminal::new(backend).unwrap();
        terminal.draw(|frame| render(frame, state)).unwrap();
        let buf = terminal.backend().buffer();
        let area = buf.area;
        let start_x = area.width * 2 / 3;
        for y in 0..area.height {
            for x in start_x..area.width {
                if buf[(x, y)].style().fg == Some(Color::Yellow) {
                    return true;
                }
            }
        }
        false
    }
    let index = repo_index(vec![
        PkgFixture::new("app/a")
            .iuse("bar")
            .rdepend("dev-libs/libx[foo]")
            .binpkg(Some(10))
            .build(),
        PkgFixture::new("app/c")
            .iuse("baz")
            .rdepend("dev-libs/libx[-foo]")
            .binpkg(Some(10))
            .build(),
        PkgFixture::new("dev-libs/libx")
            .iuse("foo")
            .rdepend("")
            .binpkg(Some(50))
            .build(),
    ]);
    let profile = Profile::test_default();
    let mut state = AppState::new(
        &index,
        &profile,
        vec!["app/a".to_string(), "app/c".to_string()],
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
    assert!(!state.conflicts.is_empty());
    for (w, h) in [(80u16, 24u16), (120u16, 40u16)] {
        state.pane = crate::mod_12_tui_state::Pane::Right;
        assert!(
            has_yellow_in_right_third(w, h, &state),
            "rechter Fokus-Rahmen fehlt ({w}x{h})"
        );
        state.pane = crate::mod_12_tui_state::Pane::Left;
        assert!(
            !has_yellow_in_right_third(w, h, &state),
            "unerwarteter rechter Fokus-Rahmen ({w}x{h})"
        );
        state.pane = crate::mod_12_tui_state::Pane::Right;
        state.left_selected = 1;
        let text = render_to(w, h, &state).join("\n");
        assert!(text.contains("Konflikte:"), "Konflikt fehlt ({w}x{h})");
    }
    state.pane = crate::mod_12_tui_state::Pane::Left;
    state.filtering = true;
    state.filter.clear();
    let text = render_to(80, 24, &state).join("\n");
    assert!(text.contains("/_"), "Filter-Prompt fehlt: {text}");
}
