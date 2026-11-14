use super::*;
use crate::mod_01_types::QuickSwitch;
use crate::test_helpers::{PkgFixture, repo_index};

fn fixture<'x>(index: &'x RepoIndex, profile: &'x Profile) -> AppState<'x> {
    AppState::new(index, profile, vec!["app/a".to_string()]).unwrap()
}

fn test_index() -> RepoIndex {
    repo_index(vec![
        PkgFixture::new("app/a")
            .iuse("+ssl")
            .rdepend("ssl? ( dev-libs/openssl )")
            .binpkg(Some(100))
            .build(),
        PkgFixture::new("dev-libs/openssl")
            .iuse("+ssl")
            .rdepend("")
            .binpkg(Some(800))
            .build(),
    ])
}

#[test]
fn tab_switch_and_pane_cycle() {
    let index = test_index();
    let profile = Profile::test_default();
    let mut st = fixture(&index, &profile);
    assert_eq!(st.tab, Tab::Packages);
    st.set_tab(Tab::BigLevers).unwrap();
    assert_eq!(st.tab, Tab::BigLevers);
    st.cycle_pane();
    assert_eq!(st.pane, Pane::Middle);
    st.cycle_pane();
    assert_eq!(st.pane, Pane::Right);
}

#[test]
fn toggle_updates_deltas_and_dirty() {
    let index = test_index();
    let profile = Profile::test_default();
    let mut st = fixture(&index, &profile);
    assert!(st.graph.nodes.contains_key("dev-libs/openssl"));
    st.pane = Pane::Middle;
    assert!(st.toggle_selected_flag().unwrap());
    assert!(st.dirty);
    assert!(!st.graph.nodes.contains_key("dev-libs/openssl"));
    assert!(st.toggle_selected_flag().unwrap());
    assert!(st.graph.nodes.contains_key("dev-libs/openssl"));
}

#[test]
fn undo_restores_overrides() {
    let index = test_index();
    let profile = Profile::test_default();
    let mut st = fixture(&index, &profile);
    st.pane = Pane::Middle;
    assert!(st.toggle_selected_flag().unwrap());
    assert!(st.dirty);
    assert!(st.pop_undo().unwrap());
    assert!(st.graph.nodes.contains_key("dev-libs/openssl"));
}

#[test]
fn quick_switch_resolves_conflict() {
    let versions = vec![
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
    ];
    let index = repo_index(versions);
    let profile = Profile::test_default();
    let mut st = AppState::new(
        &index,
        &profile,
        vec!["app/a".to_string(), "app/c".to_string()],
    )
    .unwrap();
    st.engine
        .overrides
        .insert("app/a".into(), [("bar".into(), true)].into_iter().collect());
    st.engine
        .overrides
        .insert("app/c".into(), [("baz".into(), true)].into_iter().collect());
    st.refresh().unwrap();
    assert!(!st.conflicts.is_empty());
    let option = st.conflicts[0].options[0].clone();
    st.apply_quick_switch(&option).unwrap();
    assert!(st.modal.is_none());
}

#[test]
fn package_use_export_writes_overrides() {
    let index = test_index();
    let profile = Profile::test_default();
    let mut st = fixture(&index, &profile);
    st.pane = Pane::Middle;
    st.toggle_selected_flag().unwrap();
    let d = tempfile::tempdir().unwrap();
    let path = d.path().join("package.use");
    st.export_package_use(&path).unwrap();
    let text = std::fs::read_to_string(&path).unwrap();
    assert!(text.contains("app/a"));
    assert!(text.contains("-ssl"));
    assert!(st.status.contains("package.use"));
}

#[test]
fn atom_cpn_strips_version_slot_and_use() {
    use crate::mod_01_types::atom_cpn;
    assert_eq!(atom_cpn(">=dev-libs/openssl-3.0"), "dev-libs/openssl");
    assert_eq!(atom_cpn("app-misc/jq"), "app-misc/jq");
    assert_eq!(atom_cpn("app-editors/emacs:0"), "app-editors/emacs");
    assert_eq!(atom_cpn("dev-libs/libx[foo]"), "dev-libs/libx");
}

#[test]
fn filter_switch_selection_reloads_middle() {
    let index = test_index();
    let profile = Profile::test_default();
    let mut st = fixture(&index, &profile);
    assert_eq!(st.selected_package().as_deref(), Some("app/a"));
    assert!(st.flag_deltas.iter().any(|d| d.package == "app/a"));
    st.filter = "openssl".to_string();
    st.apply_filter_only();
    assert_eq!(st.selected_package().as_deref(), Some("dev-libs/openssl"));
    st.ensure_deltas().unwrap();
    assert!(!st.deltas_loading);
    assert!(!st.flag_deltas.is_empty());
    assert!(
        st.flag_deltas
            .iter()
            .all(|d| d.package == "dev-libs/openssl")
    );
    st.filter.clear();
    st.apply_filter_only();
    st.ensure_deltas().unwrap();
    assert_eq!(st.selected_package().as_deref(), Some("app/a"));
}

#[test]
fn right_scroll_resets_on_package_change() {
    let index = test_index();
    let profile = Profile::test_default();
    let mut st = fixture(&index, &profile);
    st.right_scroll = 7;
    st.select_left_fast(1);
    assert_eq!(st.right_scroll, 0);
}

#[test]
fn big_lever_toggle_disables_globally() {
    let index = test_index();
    let profile = Profile::test_default();
    let mut st = fixture(&index, &profile);
    st.set_tab(Tab::BigLevers).unwrap();
    assert!(!st.levers.is_empty());
    assert!(st.levers.iter().all(|l| !l.turn_on));
    let pos = st.levers.iter().position(|l| l.flag == "ssl").unwrap();
    st.lever_selected = pos;
    st.lever_table_state.select(Some(pos));
    assert!(st.toggle_big_lever().unwrap());
    assert!(!st.graph.nodes.contains_key("dev-libs/openssl"));
}

#[test]
fn or_decision_cycles() {
    let versions = vec![
        PkgFixture::new("app/a")
            .iuse("")
            .rdepend("|| ( net-misc/openssh net-misc/dropbear )")
            .binpkg(Some(100))
            .build(),
        PkgFixture::new("net-misc/openssh")
            .binpkg(Some(200))
            .build(),
        PkgFixture::new("net-misc/dropbear")
            .binpkg(Some(50))
            .build(),
    ];
    let index = repo_index(versions);
    let profile = Profile::test_default();
    let mut st = AppState::new(&index, &profile, vec!["app/a".to_string()]).unwrap();
    assert!(st.selected_or_group().is_none());
    let idx = st
        .left
        .iter()
        .position(|e| matches!(e, LeftEntry::OrDecision(_)))
        .unwrap();
    st.left_selected = idx;
    assert!(st.selected_or_group().is_some());
    assert!(st.cycle_or_decision().unwrap());
}

#[test]
fn sort_cycles_without_resolve() {
    let index = test_index();
    let profile = Profile::test_default();
    let mut st = fixture(&index, &profile);
    let before = st.graph.nodes.len();
    assert_eq!(st.left_sort, LeftSort::Default);
    st.cycle_left_sort();
    assert_eq!(st.left_sort, LeftSort::ExclusivePackages);
    st.cycle_left_sort();
    assert_eq!(st.left_sort, LeftSort::ExclusiveSize);
    st.cycle_left_sort();
    assert_eq!(st.left_sort, LeftSort::Default);
    assert_eq!(st.graph.nodes.len(), before);
}

#[test]
fn cut_plan_and_apply_and_undo() {
    let index = test_index();
    let profile = Profile::test_default();
    let mut st = fixture(&index, &profile);
    assert!(st.compute_cut_plan().unwrap());
    let target = st.selected_package().unwrap();
    assert_eq!(st.cut_plan.as_ref().unwrap().target, target);
    assert!(!st.pop_undo().unwrap());
}

#[test]
fn cut_apply_sets_flag_blockers_and_undo_restores() {
    // Diamond: openssl wird von a/b via ssl und von c unbedingt gehalten.
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
    let mut st = AppState::new(
        &index,
        &profile,
        vec![
            "app/a".to_string(),
            "app/b".to_string(),
            "app/c".to_string(),
        ],
    )
    .unwrap();
    let idx = st
        .left
        .iter()
        .position(|e| *e == LeftEntry::Transitive("dev-libs/openssl".into()))
        .unwrap();
    st.left_selected = idx;
    assert!(st.compute_cut_plan().unwrap());
    let plan = st.cut_plan.clone().unwrap();
    assert_eq!(
        plan.flag_blockers
            .iter()
            .map(|b| b.package.clone())
            .collect::<Vec<_>>(),
        vec!["app/a".to_string(), "app/b".to_string()]
    );
    assert!(st.apply_cut_blockers().unwrap());
    assert_eq!(
        st.engine.overrides.get("app/a").unwrap().get("ssl"),
        Some(&false)
    );
    assert!(st.graph.nodes.contains_key("dev-libs/openssl"));
    assert!(st.pop_undo().unwrap());
    assert!(st.engine.overrides.is_empty());
}

#[test]
fn toggle_trigger_cleans_cascade_effects() {
    let index = repo_index(vec![
        PkgFixture::new("gui/app")
            .iuse("svt-av1 aom")
            .required_use("svt-av1? ( aom )")
            .binpkg(Some(100))
            .build(),
    ]);
    let profile = Profile::test_default();
    let mut st = AppState::new(&index, &profile, vec!["gui/app".to_string()]).unwrap();
    st.pane = Pane::Middle;
    let pos = st
        .flag_deltas
        .iter()
        .position(|d| d.flag == "svt-av1")
        .unwrap();
    st.middle_selected = pos;
    assert!(st.toggle_selected_flag().unwrap());
    assert!(st.graph.nodes["gui/app"].use_flags.contains("aom"));
    let user = st.engine.overrides.get("gui/app").unwrap();
    assert_eq!(user.get("svt-av1"), Some(&true));
    assert!(!user.contains_key("aom"));
    let pos = st
        .flag_deltas
        .iter()
        .position(|d| d.flag == "svt-av1")
        .unwrap();
    st.middle_selected = pos;
    assert!(st.toggle_selected_flag().unwrap());
    assert!(!st.graph.nodes["gui/app"].use_flags.contains("aom"));
    assert!(!st.engine.overrides["gui/app"].contains_key("aom"));
}

#[test]
fn space_on_conflict_flag_opens_modal() {
    let index = test_index();
    let profile = Profile::test_default();
    let mut st = fixture(&index, &profile);
    st.pane = Pane::Middle;
    st.middle_selected = 0;
    st.flag_deltas[0].conflict = Some(ConflictInfo {
        package: "app/a".into(),
        message: "simulierter Widerspruch".into(),
        caused_by: vec!["app/a".into()],
        options: vec![QuickSwitch {
            label: "uebernehmen".into(),
            changes: vec![("app/a".into(), "ssl".into(), true)],
        }],
    });
    assert!(!st.toggle_selected_flag().unwrap());
    assert!(matches!(st.modal, Some(Modal::SimulatedConflict { .. })));
    let opt = st.modal_selected_option().unwrap();
    st.apply_quick_switch(&opt).unwrap();
    assert!(st.modal.is_none());
    assert!(st.dirty);
}

#[test]
fn empty_quick_switch_sets_status_not_dirty() {
    let index = test_index();
    let profile = Profile::test_default();
    let mut st = fixture(&index, &profile);
    let opt = QuickSwitch {
        label: "leer".into(),
        changes: vec![],
    };
    st.apply_quick_switch(&opt).unwrap();
    assert!(!st.dirty);
    assert_eq!(st.status, "Keine automatische Aenderung verfuegbar");
    assert!(st.modal.is_none());
}

#[test]
fn filter_only_does_not_resolve() {
    let index = test_index();
    let profile = Profile::test_default();
    let mut st = fixture(&index, &profile);
    let before = st.graph.nodes.len();
    st.filtering = true;
    st.filter.push('a');
    st.apply_filter_only();
    assert_eq!(st.graph.nodes.len(), before);
    assert!(!st.filter.is_empty());
}
