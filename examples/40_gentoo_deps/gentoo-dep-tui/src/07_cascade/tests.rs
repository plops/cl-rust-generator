use super::*;
use crate::mod_02_profile::Profile;
use crate::test_helpers::{PkgFixture, repo_index};

#[test]
fn required_use_forces_flag() {
    // wayland? ( egl ): wayland an → egl wird erzwungen.
    let index = repo_index(vec![
        PkgFixture::new("gui/app")
            .iuse("wayland egl")
            .required_use("wayland? ( egl )")
            .build(),
    ]);
    let profile = Profile::test_default();
    let mut engine = Engine::new(&index, &profile);
    engine.overrides.insert(
        "gui/app".into(),
        [("wayland".to_string(), true)].into_iter().collect(),
    );
    let outcome = resolve_with_cascade(&mut engine, &["gui/app".into()]).unwrap();
    assert!(outcome.conflicts.is_empty());
    let node = &outcome.graph.nodes["gui/app"];
    assert!(node.use_flags.contains("egl"));
}

#[test]
fn cascade_does_not_pollute_user_overrides() {
    // User an: `svt-av1` → Kaskade erzwingt `aom`, aber User-Overrides
    // bleiben sauber; User aus: kein Kaskaden-Rest in Overrides/Flags.
    let index = repo_index(vec![
        PkgFixture::new("gui/app")
            .iuse("svt-av1 aom")
            .required_use("svt-av1? ( aom )")
            .build(),
    ]);
    let profile = Profile::test_default();
    let mut engine = Engine::new(&index, &profile);
    engine.overrides.insert(
        "gui/app".into(),
        [("svt-av1".to_string(), true)].into_iter().collect(),
    );
    let user_before = engine.overrides.clone();
    let outcome = resolve_with_cascade(&mut engine, &["gui/app".into()]).unwrap();
    assert!(outcome.conflicts.is_empty());
    assert!(outcome.graph.nodes["gui/app"].use_flags.contains("aom"));
    assert_eq!(engine.overrides, user_before);
    assert!(
        outcome
            .effective_overrides
            .get("gui/app")
            .is_some_and(|m| m.get("aom") == Some(&true))
    );
    let mut engine2 = Engine::new(&index, &profile);
    engine2.overrides.insert(
        "gui/app".into(),
        [("svt-av1".to_string(), false)].into_iter().collect(),
    );
    let outcome2 = resolve_with_cascade(&mut engine2, &["gui/app".into()]).unwrap();
    assert!(!outcome2.graph.nodes["gui/app"].use_flags.contains("aom"));
    assert!(
        !outcome2
            .effective_overrides
            .get("gui/app")
            .is_some_and(|m| m.contains_key("aom"))
    );
}

#[test]
fn any_of_skips_masked_and_user_off() {
    // `|| ( masked user_off valid )`: maskiertes und user-gesperrtes
    // Flag ueberspringen, gueltiges waehlen.
    let index = repo_index(vec![
        PkgFixture::new("gui/app")
            .iuse("masked_flag valid_flag user_flag")
            .required_use("|| ( masked_flag user_flag valid_flag )")
            .build(),
    ]);
    let mut profile = Profile::test_default();
    profile.use_mask.insert("masked_flag".into());
    let mut engine = Engine::new(&index, &profile);
    engine.overrides.insert(
        "gui/app".into(),
        [("user_flag".to_string(), false)].into_iter().collect(),
    );
    let outcome = resolve_with_cascade(&mut engine, &["gui/app".into()]).unwrap();
    assert!(outcome.conflicts.is_empty());
    assert!(
        outcome.graph.nodes["gui/app"]
            .use_flags
            .contains("valid_flag")
    );
}

#[test]
fn exactly_one_quickswitch_resolves() {
    // `^^ ( a b )` beide an → Konflikt mit echten Changes, die nach
    // Anwendung den Konflikt aufloesen.
    let index = repo_index(vec![
        PkgFixture::new("gui/app")
            .iuse("a b")
            .rdepend("")
            .required_use("^^ ( a b )")
            .build(),
    ]);
    let profile = Profile::test_default();
    let mut engine = Engine::new(&index, &profile);
    engine.overrides.insert(
        "gui/app".into(),
        [("a".to_string(), true), ("b".to_string(), true)]
            .into_iter()
            .collect(),
    );
    let outcome = resolve_with_cascade(&mut engine, &["gui/app".into()]).unwrap();
    assert_eq!(outcome.conflicts.len(), 1);
    let opt = &outcome.conflicts[0].options[0];
    assert!(!opt.changes.is_empty());
    let mut engine2 = Engine::new(&index, &profile);
    let mut ov = engine.overrides.clone();
    for (p, f, v) in &opt.changes {
        ov.entry(p.clone()).or_default().insert(f.clone(), *v);
    }
    engine2.overrides = ov;
    let outcome2 = resolve_with_cascade(&mut engine2, &["gui/app".into()]).unwrap();
    assert!(outcome2.conflicts.is_empty());
}

#[test]
fn use_dep_missing_flag_with_default_not_injected() {
    // `libx[missing(+)]`, `missing` nicht in IUSE: Default `+` gilt,
    // kein Override injiziert, kein Konflikt.
    let index = repo_index(vec![
        PkgFixture::new("app/a")
            .iuse("bar")
            .rdepend("dev-libs/libx[missing(+)]")
            .required_use("")
            .build(),
        PkgFixture::new("dev-libs/libx")
            .iuse("")
            .rdepend("")
            .required_use("")
            .build(),
    ]);
    let profile = Profile::test_default();
    let mut engine = Engine::new(&index, &profile);
    engine
        .overrides
        .insert("app/a".into(), [("bar".into(), true)].into_iter().collect());
    let outcome = resolve_with_cascade(&mut engine, &["app/a".into()]).unwrap();
    assert!(outcome.conflicts.is_empty());
    assert!(
        !outcome
            .effective_overrides
            .get("dev-libs/libx")
            .is_some_and(|m| m.contains_key("missing"))
    );
}

#[test]
fn use_dep_missing_flag_without_default_conflicts() {
    // `libx[missing]` ohne Default und ohne IUSE: Konflikt statt Injektion.
    let index = repo_index(vec![
        PkgFixture::new("app/a")
            .iuse("bar")
            .rdepend("dev-libs/libx[missing]")
            .required_use("")
            .build(),
        PkgFixture::new("dev-libs/libx")
            .iuse("")
            .rdepend("")
            .required_use("")
            .build(),
    ]);
    let profile = Profile::test_default();
    let mut engine = Engine::new(&index, &profile);
    engine
        .overrides
        .insert("app/a".into(), [("bar".into(), true)].into_iter().collect());
    let outcome = resolve_with_cascade(&mut engine, &["app/a".into()]).unwrap();
    assert!(!outcome.conflicts.is_empty());
    assert!(outcome.conflicts.iter().all(|c| !c.options.is_empty()));
    assert!(
        !outcome
            .effective_overrides
            .get("dev-libs/libx")
            .is_some_and(|m| m.contains_key("missing"))
    );
}

#[test]
fn use_dep_conflict_detected_with_options() {
    // A braucht libX[foo], C braucht libX[-foo] → Konflikt.
    let index = repo_index(vec![
        PkgFixture::new("app/a")
            .iuse("bar")
            .rdepend("dev-libs/libx[foo]")
            .required_use("")
            .build(),
        PkgFixture::new("app/c")
            .iuse("baz")
            .rdepend("dev-libs/libx[-foo]")
            .required_use("")
            .build(),
        PkgFixture::new("dev-libs/libx")
            .iuse("foo")
            .rdepend("")
            .required_use("")
            .build(),
    ]);
    let profile = Profile::test_default();
    let mut engine = Engine::new(&index, &profile);
    engine
        .overrides
        .insert("app/a".into(), [("bar".into(), true)].into_iter().collect());
    engine
        .overrides
        .insert("app/c".into(), [("baz".into(), true)].into_iter().collect());
    let outcome = resolve_with_cascade(&mut engine, &["app/a".into(), "app/c".into()]).unwrap();
    assert!(!outcome.conflicts.is_empty());
    assert!(outcome.conflicts.iter().all(|c| !c.options.is_empty()));
}

#[test]
fn shared_dep_delta_zero_documented() {
    // Vorstufe fuer T10: beide brauchen openssl — Abschalten bei nur
    // einem Paket spart nichts (wird in 08_delta exakt gerechnet).
    let index = repo_index(vec![
        PkgFixture::new("app/a")
            .iuse("ssl")
            .rdepend("dev-libs/openssl")
            .required_use("")
            .build(),
        PkgFixture::new("app/b")
            .iuse("ssl")
            .rdepend("dev-libs/openssl")
            .required_use("")
            .build(),
        PkgFixture::new("dev-libs/openssl")
            .iuse("ssl")
            .rdepend("")
            .required_use("")
            .build(),
    ]);
    let profile = Profile::test_default();
    let mut engine = Engine::new(&index, &profile);
    let outcome = resolve_with_cascade(&mut engine, &["app/a".into(), "app/b".into()]).unwrap();
    assert!(outcome.conflicts.is_empty());
    assert_eq!(outcome.graph.refcounts.get("dev-libs/openssl"), Some(&2));
}
