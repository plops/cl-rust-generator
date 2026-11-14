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
    assert!(!st.dirty);
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
fn sort_toggle_keeps_selection_on_same_package() {
    // `app/big` faellt nach dem Ausschalten von `ssl` in der
    // Exklusiv-Sortierung hinter `app/small` — der Cursor bleibt trotzdem
    // auf `app/big`.
    let index = repo_index(vec![
        PkgFixture::new("app/big")
            .iuse("+ssl")
            .rdepend("ssl? ( dev-libs/openssl )")
            .binpkg(Some(100))
            .build(),
        PkgFixture::new("app/small")
            .iuse("+x")
            .rdepend("x? ( dev-libs/smalldep )")
            .binpkg(Some(50))
            .build(),
        PkgFixture::new("dev-libs/openssl")
            .src_uri("http://x/o.tar.gz")
            .manifest("DIST o.tar.gz 8000\n")
            .binpkg(Some(800))
            .build(),
        PkgFixture::new("dev-libs/smalldep")
            .binpkg(Some(700))
            .build(),
    ]);
    let profile = Profile::test_default();
    let mut st = AppState::new(
        &index,
        &profile,
        vec!["app/big".to_string(), "app/small".to_string()],
    )
    .unwrap();
    st.cycle_left_sort();
    assert_eq!(st.left_sort, LeftSort::ExclusivePackages);
    // big (2 exklusive Pakete, grosse DL) steht vor small.
    assert_eq!(st.selected_package().as_deref(), Some("app/big"));
    st.pane = Pane::Middle;
    let pos = st
        .flag_deltas
        .iter()
        .position(|d| d.flag == "ssl")
        .unwrap();
    st.middle_selected = pos;
    assert!(st.toggle_selected_flag().unwrap());
    // Nach dem Toggle ist small (2 Pakete) vor big (1 Paket) sortiert ...
    assert_eq!(
        st.left.first(),
        Some(&LeftEntry::WorldPkg("app/small".into()))
    );
    // ... der Cursor bleibt trotzdem auf app/big.
    assert_eq!(st.selected_package().as_deref(), Some("app/big"));
}

#[test]
fn duplicate_world_atoms_are_deduplicated() {
    let index = repo_index(vec![
        PkgFixture::new("sys-kernel/gentoo-sources")
            .version("1.0")
            .slot("1.0")
            .binpkg(Some(100))
            .build(),
        PkgFixture::new("sys-kernel/gentoo-sources")
            .version("2.0")
            .slot("2.0")
            .binpkg(Some(100))
            .build(),
        PkgFixture::new("app/a").binpkg(Some(10)).build(),
    ]);
    let profile = Profile::test_default();
    let st = AppState::new(
        &index,
        &profile,
        vec![
            "sys-kernel/gentoo-sources:1.0".to_string(),
            "sys-kernel/gentoo-sources:2.0".to_string(),
            "app/a".to_string(),
        ],
    )
    .unwrap();
    let count = st
        .left
        .iter()
        .filter(|e| *e == &LeftEntry::WorldPkg("sys-kernel/gentoo-sources".into()))
        .count();
    assert_eq!(count, 1);
}

#[test]
fn export_merges_valid_package_use_cleans_dead_and_clears_dirty() {
    // `package.use`: dbus (Profil an), sound-server gueltig, alsa/udev tot
    // (nicht in IUSE). TUI schaltet `extra` an -> `[S]` schreibt exakt
    // `extra sound-server`, setzt `dirty == false`.
    let index = repo_index(vec![
        PkgFixture::new("media-sound/bla")
            .iuse("dbus sound-server extra")
            .binpkg(Some(100))
            .build(),
    ]);
    let mut profile = Profile::test_default();
    profile.global_use.insert("dbus".to_string());
    profile.package_use.insert(
        "media-sound/bla".into(),
        vec![
            "alsa".into(),
            "dbus".into(),
            "sound-server".into(),
            "udev".into(),
        ],
    );
    profile.sync_pattern_caches();
    let mut st = AppState::new(
        &index,
        &profile,
        vec!["media-sound/bla".to_string()],
    )
    .unwrap();
    st.pane = Pane::Middle;
    let pos = st
        .flag_deltas
        .iter()
        .position(|d| d.flag == "extra")
        .unwrap();
    st.middle_selected = pos;
    assert!(st.toggle_selected_flag().unwrap());
    assert!(st.dirty);
    let d = tempfile::tempdir().unwrap();
    let path = d.path().join("package.use");
    st.export_package_use(&path).unwrap();
    let text = std::fs::read_to_string(&path).unwrap();
    assert_eq!(text, "media-sound/bla extra sound-server\n");
    assert!(!st.dirty);
    assert!(st.status.contains("package.use geschrieben:"));
    // Naechster Toggle loescht den `package.use geschrieben:`-Status.
    let pos = st
        .flag_deltas
        .iter()
        .position(|d| d.flag == "extra")
        .unwrap();
    st.middle_selected = pos;
    assert!(st.toggle_selected_flag().unwrap());
    assert!(!st.status.contains("package.use geschrieben:"));
}

#[test]
fn right_pane_hover_updates_cut_plan_automatically() {
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
    assert!(!st.right_targets.is_empty());
    assert!(st.cut_plan.is_none());
    st.cycle_pane();
    assert_eq!(st.pane, Pane::Middle);
    st.cycle_pane();
    assert_eq!(st.pane, Pane::Right);
    assert_eq!(st.right_target_selected, Some(0));
    st.move_selection(1);
    assert_eq!(st.right_target_selected, Some(1));
    st.ensure_deltas().unwrap();
    let target = st.right_targets[1].clone();
    assert_eq!(st.cut_plan.as_ref().unwrap().target, target);
}

#[test]
fn unrelated_conflict_does_not_poison_other_flags() {
    // Konflikt zwischen app/a und app/c; app/b ist unbeteiligt.
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
            .binpkg(Some(50))
            .build(),
        PkgFixture::new("app/b")
            .iuse("+ssl doc")
            .rdepend("ssl? ( dev-libs/openssl )")
            .binpkg(Some(10))
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
            "app/c".to_string(),
            "app/b".to_string(),
        ],
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
    let idx = st
        .left
        .iter()
        .position(|e| *e == LeftEntry::WorldPkg("app/b".into()))
        .unwrap();
    st.left_selected = idx;
    st.rebuild_middle().unwrap();
    assert!(!st.flag_deltas.is_empty());
    for d in &st.flag_deltas {
        assert!(
            d.conflict.is_none(),
            "fremder Konflikt auf unbeteiligtem Flag {}: {:?}",
            d.flag,
            d.conflict
        );
    }
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
