use super::*;
use crate::mod_02_profile::Profile;
use crate::test_helpers::{PkgFixture, repo_index};

fn test_index() -> RepoIndex {
    // Diamond: app -> {libb, libc}, beide -> libshared.
    // app[ssl?] -> openssl ; app hat BDEPEND auf build-tool.
    let versions = vec![
        PkgFixture::new("app-misc/app")
            .version("1.0")
            .keywords("amd64")
            .iuse("ssl")
            .rdepend("dev-libs/libb dev-libs/libc ssl? ( dev-libs/openssl )")
            .bdepend("dev-build/tool")
            .src_uri("http://x/a.tar.gz")
            .manifest("DIST a.tar.gz 1000\n")
            .binpkg(Some(100))
            .build(),
        PkgFixture::new("dev-libs/libb")
            .version("1.0")
            .keywords("amd64")
            .iuse("")
            .rdepend("dev-libs/libshared")
            .bdepend("")
            .src_uri("http://x/a.tar.gz")
            .manifest("DIST a.tar.gz 1000\n")
            .binpkg(Some(50))
            .build(),
        PkgFixture::new("dev-libs/libc")
            .version("1.0")
            .keywords("amd64")
            .iuse("")
            .rdepend("dev-libs/libshared")
            .bdepend("")
            .src_uri("http://x/a.tar.gz")
            .manifest("DIST a.tar.gz 1000\n")
            .binpkg(Some(60))
            .build(),
        PkgFixture::new("dev-libs/libshared")
            .version("1.0")
            .keywords("amd64")
            .iuse("")
            .rdepend("")
            .bdepend("")
            .src_uri("http://x/a.tar.gz")
            .manifest("DIST a.tar.gz 1000\n")
            .binpkg(Some(70))
            .build(),
        PkgFixture::new("dev-libs/openssl")
            .version("3.0")
            .keywords("amd64")
            .iuse("")
            .rdepend("")
            .bdepend("")
            .src_uri("http://x/a.tar.gz")
            .manifest("DIST a.tar.gz 1000\n")
            .binpkg(Some(80))
            .build(),
        PkgFixture::new("dev-build/tool")
            .version("1.0")
            .keywords("amd64")
            .iuse("")
            .rdepend("")
            .bdepend("")
            .src_uri("http://x/a.tar.gz")
            .manifest("DIST a.tar.gz 1000\n")
            .binpkg(Some(900))
            .build(),
    ];
    repo_index(versions)
}

#[test]
fn shared_dep_counted_once_with_refcount_two() {
    let index = test_index();
    let profile = Profile::test_default();
    let engine = Engine::new(&index, &profile);
    let graph = resolve(&engine, &["app-misc/app".into()]).unwrap();
    // app, libb, libc, libshared (ssl aus → kein openssl).
    assert_eq!(graph.nodes.len(), 4);
    assert_eq!(graph.refcounts.get("dev-libs/libshared"), Some(&2));
    let total = graph.totals();
    assert_eq!(total.binpkg_bytes, 100 + 50 + 60 + 70);
    // Build-Tool landet im Extra, nicht im Runtime-Total.
    assert_eq!(graph.build_extra.binpkg_bytes, 900);
    assert!(!graph.nodes.contains_key("dev-build/tool"));
}

#[test]
fn use_toggle_pulls_optional_dep() {
    let index = test_index();
    let profile = Profile::test_default();
    let mut engine = Engine::new(&index, &profile);
    engine.overrides.insert(
        "app-misc/app".into(),
        [("ssl".to_string(), true)].into_iter().collect(),
    );
    let graph = resolve(&engine, &["app-misc/app".into()]).unwrap();
    assert!(graph.nodes.contains_key("dev-libs/openssl"));
    assert_eq!(graph.refcounts.get("dev-libs/openssl"), Some(&1));
}

#[test]
fn cycle_terminates() {
    let versions = vec![
        PkgFixture::new("cat/a")
            .version("1.0")
            .keywords("amd64")
            .iuse("")
            .rdepend("cat/b")
            .bdepend("")
            .src_uri("http://x/a.tar.gz")
            .manifest("DIST a.tar.gz 1000\n")
            .binpkg(Some(10))
            .build(),
        PkgFixture::new("cat/b")
            .version("1.0")
            .keywords("amd64")
            .iuse("")
            .rdepend("cat/a")
            .bdepend("")
            .src_uri("http://x/a.tar.gz")
            .manifest("DIST a.tar.gz 1000\n")
            .binpkg(Some(20))
            .build(),
    ];
    let index = repo_index(versions);
    let profile = Profile::test_default();
    let engine = Engine::new(&index, &profile);
    let graph = resolve(&engine, &["cat/a".into()]).unwrap();
    assert_eq!(graph.nodes.len(), 2);
}

#[test]
fn transitive_versioned_dep_selects_matching() {
    // `<dev-libs/lib-2.0` muss 1.0 waehlen, nicht die neueste 2.0.
    let versions = vec![
        PkgFixture::new("app-misc/app")
            .version("1.0")
            .keywords("amd64")
            .iuse("")
            .rdepend("<dev-libs/lib-2.0")
            .bdepend("")
            .src_uri("http://x/a.tar.gz")
            .manifest("DIST a.tar.gz 1000\n")
            .binpkg(Some(100))
            .build(),
        PkgFixture::new("dev-libs/lib")
            .version("1.0")
            .keywords("amd64")
            .iuse("")
            .rdepend("")
            .bdepend("")
            .src_uri("http://x/a.tar.gz")
            .manifest("DIST a.tar.gz 1000\n")
            .binpkg(Some(50))
            .build(),
        PkgFixture::new("dev-libs/lib")
            .version("2.0")
            .keywords("amd64")
            .iuse("")
            .rdepend("")
            .bdepend("")
            .src_uri("http://x/a.tar.gz")
            .manifest("DIST a.tar.gz 1000\n")
            .binpkg(Some(60))
            .build(),
    ];
    let index = repo_index(versions);
    let profile = Profile::test_default();
    let engine = Engine::new(&index, &profile);
    let graph = resolve(&engine, &["app-misc/app".into()]).unwrap();
    assert_eq!(graph.nodes["dev-libs/lib"].cpv, "dev-libs/lib-1.0");
}

#[test]
fn pdepend_is_runtime() {
    // PDEPEND landet im Runtime-Graphen (nicht nur RDEPEND).
    let index = repo_index(vec![
        PkgFixture::new("app-misc/app")
            .pdepend("dev-libs/post")
            .manifest("DIST a.tar.gz 1000\n")
            .binpkg(Some(10))
            .build(),
        PkgFixture::new("dev-libs/post")
            .manifest("DIST a.tar.gz 1000\n")
            .binpkg(Some(10))
            .build(),
    ]);
    let profile = Profile::test_default();
    let engine = Engine::new(&index, &profile);
    let graph = resolve(&engine, &["app-misc/app".into()]).unwrap();
    assert!(graph.nodes.contains_key("dev-libs/post"));
}

#[test]
fn distfile_mirrors_count_once() {
    // Zwei Spiegel derselben Datei zaehlen nur einmal.
    let versions = vec![
        PkgFixture::new("app-misc/app")
            .version("1.0")
            .keywords("amd64")
            .iuse("")
            .rdepend("")
            .bdepend("")
            .src_uri("http://a/a.tar.gz http://b/a.tar.gz")
            .manifest("DIST a.tar.gz 1000\n")
            .binpkg(Some(100))
            .build(),
    ];
    let index = repo_index(versions);
    let profile = Profile::test_default();
    let engine = Engine::new(&index, &profile);
    let graph = resolve(&engine, &["app-misc/app".into()]).unwrap();
    assert_eq!(graph.nodes["app-misc/app"].download, 1000);
}

#[test]
fn nested_conditional_keeps_deepest_flag() {
    // `foo? ( bar? ( dep ) )`: tiefste Bedingung gewinnt.
    let versions = vec![
        PkgFixture::new("app-misc/app")
            .version("1.0")
            .keywords("amd64")
            .iuse("foo bar")
            .rdepend("foo? ( bar? ( dev-libs/dep ) )")
            .bdepend("")
            .src_uri("http://x/a.tar.gz")
            .manifest("DIST a.tar.gz 1000\n")
            .binpkg(Some(100))
            .build(),
        PkgFixture::new("dev-libs/dep")
            .version("1.0")
            .keywords("amd64")
            .iuse("")
            .rdepend("")
            .bdepend("")
            .src_uri("http://x/a.tar.gz")
            .manifest("DIST a.tar.gz 1000\n")
            .binpkg(Some(10))
            .build(),
    ];
    let index = repo_index(versions);
    let profile = Profile::test_default();
    let mut engine = Engine::new(&index, &profile);
    engine.overrides.insert(
        "app-misc/app".into(),
        [("foo".to_string(), true), ("bar".to_string(), true)]
            .into_iter()
            .collect(),
    );
    let graph = resolve(&engine, &["app-misc/app".into()]).unwrap();
    assert!(graph.nodes.contains_key("dev-libs/dep"));
    let parents = &graph.reverse["dev-libs/dep"];
    assert!(
        parents
            .iter()
            .any(|(p, v)| p == "app-misc/app" && *v == Some("bar".to_string())),
        "tiefstes via erwartet: {parents:?}"
    );
}

#[test]
fn unconditional_beats_conditional_regardless_of_order() {
    // Unbedingt gewinnt — egal ob vor oder nach der Bedingung.
    for rdepend in [
        "dev-libs/dep foo? ( dev-libs/dep )",
        "foo? ( dev-libs/dep ) dev-libs/dep",
    ] {
        let versions = vec![
            PkgFixture::new("app-misc/app")
                .version("1.0")
                .keywords("amd64")
                .iuse("foo")
                .rdepend(rdepend)
                .bdepend("")
                .src_uri("http://x/a.tar.gz")
                .manifest("DIST a.tar.gz 1000\n")
                .binpkg(Some(100))
                .build(),
            PkgFixture::new("dev-libs/dep")
                .version("1.0")
                .keywords("amd64")
                .iuse("")
                .rdepend("")
                .bdepend("")
                .src_uri("http://x/a.tar.gz")
                .manifest("DIST a.tar.gz 1000\n")
                .binpkg(Some(10))
                .build(),
        ];
        let index = repo_index(versions);
        let profile = Profile::test_default();
        let mut engine = Engine::new(&index, &profile);
        engine.overrides.insert(
            "app-misc/app".into(),
            [("foo".to_string(), true)].into_iter().collect(),
        );
        let graph = resolve(&engine, &["app-misc/app".into()]).unwrap();
        let parents = &graph.reverse["dev-libs/dep"];
        assert!(
            parents
                .iter()
                .any(|(p, v)| p == "app-misc/app" && v.is_none()),
            "via None erwartet fuer {rdepend}: {parents:?}"
        );
    }
}

#[test]
fn negated_condition_formats_bang() {
    // `!foo? ( dep )` bei ausgeschaltetem foo → via `"!foo"`.
    let versions = vec![
        PkgFixture::new("app-misc/app")
            .version("1.0")
            .keywords("amd64")
            .iuse("foo")
            .rdepend("!foo? ( dev-libs/dep )")
            .bdepend("")
            .src_uri("http://x/a.tar.gz")
            .manifest("DIST a.tar.gz 1000\n")
            .binpkg(Some(100))
            .build(),
        PkgFixture::new("dev-libs/dep")
            .version("1.0")
            .keywords("amd64")
            .iuse("")
            .rdepend("")
            .bdepend("")
            .src_uri("http://x/a.tar.gz")
            .manifest("DIST a.tar.gz 1000\n")
            .binpkg(Some(10))
            .build(),
    ];
    let index = repo_index(versions);
    let profile = Profile::test_default();
    let engine = Engine::new(&index, &profile);
    let graph = resolve(&engine, &["app-misc/app".into()]).unwrap();
    assert!(graph.nodes.contains_key("dev-libs/dep"));
    let parents = &graph.reverse["dev-libs/dep"];
    assert!(
        parents
            .iter()
            .any(|(p, v)| p == "app-misc/app" && *v == Some("!foo".to_string())),
        "{parents:?}"
    );
}

#[test]
fn world_cycle_blame_nonempty() {
    // Zyklus zwischen zwei Welt-Paketen liefert echte Ketten statt `[]`.
    let versions = vec![
        PkgFixture::new("cat/a")
            .version("1.0")
            .keywords("amd64")
            .iuse("")
            .rdepend("cat/b")
            .bdepend("")
            .src_uri("http://x/a.tar.gz")
            .manifest("DIST a.tar.gz 1000\n")
            .binpkg(Some(10))
            .build(),
        PkgFixture::new("cat/b")
            .version("1.0")
            .keywords("amd64")
            .iuse("")
            .rdepend("cat/a")
            .bdepend("")
            .src_uri("http://x/a.tar.gz")
            .manifest("DIST a.tar.gz 1000\n")
            .binpkg(Some(20))
            .build(),
    ];
    let index = repo_index(versions);
    let profile = Profile::test_default();
    let engine = Engine::new(&index, &profile);
    let graph = resolve(&engine, &["cat/a".into(), "cat/b".into()]).unwrap();
    let chains = graph.blame("cat/a");
    assert!(!chains.is_empty());
    assert!(chains.iter().all(|c| !c.is_empty()));
}

#[test]
fn any_of_records_alternative() {
    let versions = vec![
        PkgFixture::new("app-misc/app")
            .version("1.0")
            .keywords("amd64")
            .iuse("")
            .rdepend("|| ( net-misc/curl net-misc/wget )")
            .bdepend("")
            .src_uri("http://x/a.tar.gz")
            .manifest("DIST a.tar.gz 1000\n")
            .binpkg(Some(100))
            .build(),
        PkgFixture::new("net-misc/curl")
            .version("1.0")
            .keywords("amd64")
            .iuse("")
            .rdepend("")
            .bdepend("")
            .src_uri("http://x/a.tar.gz")
            .manifest("DIST a.tar.gz 1000\n")
            .binpkg(Some(30))
            .build(),
        PkgFixture::new("net-misc/wget")
            .version("1.0")
            .keywords("amd64")
            .iuse("")
            .rdepend("")
            .bdepend("")
            .src_uri("http://x/a.tar.gz")
            .manifest("DIST a.tar.gz 1000\n")
            .binpkg(Some(40))
            .build(),
    ];
    let index = repo_index(versions);
    let profile = Profile::test_default();
    let engine = Engine::new(&index, &profile);
    let graph = resolve(&engine, &["app-misc/app".into()]).unwrap();
    assert!(graph.nodes.contains_key("net-misc/curl"));
    assert_eq!(graph.alternatives.len(), 1);
    assert_eq!(graph.alternatives[0].candidates.len(), 2);
}
