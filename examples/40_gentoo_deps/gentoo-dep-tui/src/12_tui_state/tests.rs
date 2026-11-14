use super::*;
use crate::mod_01_types::QuickSwitch;
use crate::test_helpers::{PkgFixture, repo_index};

fn fixture<'x>(index: &'x RepoIndex, profile: &'x Profile) -> AppState<'x> {
    AppState::new(
        index,
        profile,
        vec!["app/a".to_string()],
        Plan::default(),
        None,
    )
    .unwrap()
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
    // Zurueckschalten stellt wieder her.
    assert!(st.toggle_selected_flag().unwrap());
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
        Plan::default(),
        None,
    )
    .unwrap();
    // Overrides fuer bar/baz setzen (sonst keine use-dep-Constraints aktiv).
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
fn save_resets_dirty() {
    let index = test_index();
    let profile = Profile::test_default();
    let mut st = fixture(&index, &profile);
    let d = tempfile::tempdir().unwrap();
    st.plan_path = Some(d.path().join("plan.toml"));
    st.pane = Pane::Middle;
    st.toggle_selected_flag().unwrap();
    assert!(st.dirty);
    st.save().unwrap();
    assert!(!st.dirty);
    let back = Plan::load(&d.path().join("plan.toml")).unwrap();
    assert!(!back.package.is_empty());
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
    // Review3-Bug: vor `/`-Filter war A selektiert, nach Filter steht nur
    // noch B in der Liste — die Mitte muss B-Flags zeigen, keine A-Flags.
    let index = test_index();
    let profile = Profile::test_default();
    let mut st = fixture(&index, &profile);
    assert_eq!(st.selected_package().as_deref(), Some("app/a"));
    assert!(st.flag_deltas.iter().any(|d| d.package == "app/a"));
    st.filter = "openssl".to_string();
    st.apply_filter_only();
    assert_eq!(st.selected_package().as_deref(), Some("dev-libs/openssl"));
    // Cache-Miss → Idle-Recompute laden, dann B-Flags pruefen.
    st.ensure_deltas().unwrap();
    assert!(!st.deltas_loading);
    assert!(!st.flag_deltas.is_empty());
    assert!(
        st.flag_deltas
            .iter()
            .all(|d| d.package == "dev-libs/openssl")
    );
    // Filter leeren stellt A wieder her.
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
    let pos = st.levers.iter().position(|l| l.flag == "ssl").unwrap();
    st.lever_selected = pos;
    st.lever_table_state.select(Some(pos));
    assert!(st.toggle_big_lever().unwrap());
    // ssl ueberall aus → openssl faellt aus dem Graphen.
    assert!(!st.graph.nodes.contains_key("dev-libs/openssl"));
    // Zurueck: wieder einschalten stellt openssl wieder her.
    st.set_tab(Tab::BigLevers).unwrap();
    let pos = st.levers.iter().position(|l| l.flag == "ssl").unwrap();
    st.lever_selected = pos;
    assert!(st.toggle_big_lever().unwrap());
    assert!(st.graph.nodes.contains_key("dev-libs/openssl"));
}

#[test]
fn choice_switch_updates_world_without_baseline() {
    use crate::mod_09_plan_toml::{ChoiceOption, PlanChoice};
    let versions = vec![
        PkgFixture::new("app/a")
            .iuse("+ssl")
            .rdepend("dev-libs/openssl")
            .binpkg(Some(100))
            .build(),
        PkgFixture::new("net-misc/openssh")
            .iuse("")
            .rdepend("")
            .binpkg(Some(200))
            .build(),
        PkgFixture::new("net-misc/dropbear")
            .iuse("")
            .rdepend("")
            .binpkg(Some(50))
            .build(),
        PkgFixture::new("dev-libs/openssl")
            .iuse("+ssl")
            .rdepend("")
            .binpkg(Some(800))
            .build(),
    ];
    let index = repo_index(versions);
    let profile = Profile::test_default();
    let mut plan = Plan::default();
    // Baseline ohne Choice-Atome (T07-Konvention): Optionen kommen aus `choice`.
    plan.baseline.world = vec!["app/a".to_string()];
    plan.choice.push(PlanChoice {
        id: "ssh".into(),
        required: true,
        description: "ssh".into(),
        selected: 0,
        option: vec![
            ChoiceOption {
                atom: "net-misc/openssh".into(),
                pros: "voll".into(),
                cons: "gross".into(),
                suggested_use: vec![],
            },
            ChoiceOption {
                atom: "net-misc/dropbear".into(),
                pros: "klein".into(),
                cons: "weniger".into(),
                suggested_use: vec![],
            },
        ],
    });
    let mut st = AppState::new(&index, &profile, plan.effective_world(), plan, None).unwrap();
    assert!(
        st.left
            .iter()
            .any(|e| *e == LeftEntry::PlanChoice("ssh".into()))
    );
    let idx = st
        .left
        .iter()
        .position(|e| *e == LeftEntry::PlanChoice("ssh".into()))
        .unwrap();
    st.left_selected = idx;
    assert!(st.cycle_choice_option().unwrap());
    assert!(st.world.contains(&"net-misc/dropbear".to_string()));
    assert!(!st.world.contains(&"net-misc/openssh".to_string()));
    // Baseline darf das Choice-Atom nicht enthalten (T07).
    sync_plan_from_state(&mut st.plan, &st.engine, &st.world);
    assert!(
        !st.plan
            .baseline
            .world
            .contains(&"net-misc/dropbear".to_string())
    );
    assert!(st.plan.baseline.world.contains(&"app/a".to_string()));
}

#[test]
fn toggle_preserves_cursor_on_same_flag() {
    // Cursor bleibt auf demselben Flag (kein Sprung auf 0 je Toggle).
    let versions = vec![
        PkgFixture::new("app/a")
            .iuse("+ssl foo")
            .rdepend("ssl? ( dev-libs/openssl )")
            .binpkg(Some(100))
            .build(),
        PkgFixture::new("dev-libs/openssl")
            .iuse("+ssl")
            .rdepend("")
            .binpkg(Some(800))
            .build(),
    ];
    let index = repo_index(versions);
    let profile = Profile::test_default();
    let mut st = fixture(&index, &profile);
    st.pane = Pane::Middle;
    // Deltas sortiert: foo, ssl → ssl an Index 1.
    assert_eq!(st.flag_deltas[1].flag, "ssl");
    st.middle_selected = 1;
    assert!(st.toggle_selected_flag().unwrap());
    assert_eq!(st.middle_selected, 1);
    assert_eq!(st.flag_deltas[1].flag, "ssl");
}

#[test]
fn toggle_trigger_cleans_cascade_effects() {
    // Trigger an → Kaskade wirkt, User-Overrides bleiben sauber;
    // Trigger aus → Kaskade zieht sich zurueck (auch in plan.toml).
    let index = repo_index(vec![
        PkgFixture::new("gui/app")
            .iuse("svt-av1 aom")
            .required_use("svt-av1? ( aom )")
            .binpkg(Some(100))
            .build(),
    ]);
    let profile = Profile::test_default();
    let mut st = AppState::new(
        &index,
        &profile,
        vec!["gui/app".to_string()],
        Plan::default(),
        None,
    )
    .unwrap();
    st.pane = Pane::Middle;
    let pos = st
        .flag_deltas
        .iter()
        .position(|d| d.flag == "svt-av1")
        .unwrap();
    st.middle_selected = pos;
    assert!(st.toggle_selected_flag().unwrap());
    // Kaskade aktiv, aber User-Overrides enthalten kein `aom`.
    assert!(st.graph.nodes["gui/app"].use_flags.contains("aom"));
    let user = st.engine.overrides.get("gui/app").unwrap();
    assert_eq!(user.get("svt-av1"), Some(&true));
    assert!(!user.contains_key("aom"));
    // Trigger wieder aus: Kaskade weg, Overrides sauber.
    let pos = st
        .flag_deltas
        .iter()
        .position(|d| d.flag == "svt-av1")
        .unwrap();
    st.middle_selected = pos;
    assert!(st.toggle_selected_flag().unwrap());
    assert!(!st.graph.nodes["gui/app"].use_flags.contains("aom"));
    assert!(!st.engine.overrides["gui/app"].contains_key("aom"));
    // plan.toml enthaelt nur den expliziten Intent.
    let d = tempfile::tempdir().unwrap();
    st.plan_path = Some(d.path().join("plan.toml"));
    st.save().unwrap();
    let back = Plan::load(&d.path().join("plan.toml")).unwrap();
    let entry = back.package.iter().find(|p| p.atom == "gui/app").unwrap();
    assert!(!entry.use_flags.iter().any(|t| t.contains("aom")));
}

#[test]
fn space_on_conflict_flag_opens_modal() {
    // `Space` auf CONFLICT-Flag oeffnet ein Modal statt still zu scheitern.
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
    // Leere Changes: Hinweis, kein Dirty, kein Re-Resolve.
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
    // Kein Resolve: Graph unveraendert, Levers unangetastet.
    assert_eq!(st.graph.nodes.len(), before);
    assert!(!st.filter.is_empty());
}
