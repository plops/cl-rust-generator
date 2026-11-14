use super::*;
use crate::mod_02_profile::Profile;
use crate::mod_05_repo_index::RepoIndex;
use crate::mod_09_plan_toml::{ChoiceOption, Plan, PlanChoice};
use crate::mod_12_tui_state::AppState;
use crate::test_helpers::{PkgFixture, repo_index};
use ratatui::Terminal;
use ratatui::backend::TestBackend;

fn test_state<'x>(index: &'x RepoIndex, profile: &'x Profile, plan: Plan) -> AppState<'x> {
    AppState::new(index, profile, vec!["app/a".to_string()], plan, None).unwrap()
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

/// Scrolling (T06): Selektion 75 von 100 bleibt sichtbar, Eintrag 0 scrollt raus.
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
    let index = repo_index(vec![
        PkgFixture::new("app-misc/big")
            .iuse(&flags.join(" "))
            .binpkg(Some(100))
            .build(),
    ]);
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
    assert!(!state.conflicts.is_empty());
    for (w, h) in [(80u16, 24u16), (120u16, 40u16)] {
        // Fokus rechts: gelber Rahmen im rechten Drittel.
        state.pane = crate::mod_12_tui_state::Pane::Right;
        assert!(
            has_yellow_in_right_third(w, h, &state),
            "rechter Fokus-Rahmen fehlt ({w}x{h})"
        );
        // Fokus links: kein Gelb rechts.
        state.pane = crate::mod_12_tui_state::Pane::Left;
        assert!(
            !has_yellow_in_right_third(w, h, &state),
            "unerwarteter rechter Fokus-Rahmen ({w}x{h})"
        );
        // Konfliktabschnitt in der rechten Pane (Konflikt haengt an app/c).
        state.pane = crate::mod_12_tui_state::Pane::Right;
        state.left_selected = 1;
        let text = render_to(w, h, &state).join("\n");
        assert!(text.contains("Konflikte:"), "Konflikt fehlt ({w}x{h})");
        assert!(text.contains("verlangt"), "Konflikttext fehlt ({w}x{h})");
    }
    // Filtermodus zeigt `/`-Prompt — auch bei leerem Filter.
    state.pane = crate::mod_12_tui_state::Pane::Left;
    state.filtering = true;
    state.filter.clear();
    let text = render_to(80, 24, &state).join("\n");
    assert!(text.contains("/_"), "Filter-Prompt fehlt: {text}");
}
