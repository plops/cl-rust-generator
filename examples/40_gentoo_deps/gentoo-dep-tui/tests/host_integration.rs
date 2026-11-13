//! Host-Integrationstests auf synthetischen `tempfile`-Repos.
//!
//! Deckt T04/T07/T10/T13 ab: Index-Filter, Graph/Blame, Deltas/Kaskaden,
//! plan.toml-Roundtrip und Export/Verify-Mocks.

use std::collections::HashMap;
use std::io::Write;

use gentoo_dep_tui::mod_02_profile::{self, Profile};
use gentoo_dep_tui::mod_03_manifest::parse_manifest;
use gentoo_dep_tui::mod_05_repo_index::RepoIndex;
use gentoo_dep_tui::mod_06_graph::{Engine, resolve};
use gentoo_dep_tui::mod_08_delta::{big_levers, flag_delta};
use gentoo_dep_tui::mod_09_plan_toml::Plan;
use gentoo_dep_tui::mod_10_export_verify::{export_portage, parse_emerge_pretend};
use gentoo_dep_tui::mod_11_cli::export_plan;

/// Mini-Repo bauen: md5-cache + Manifest + profiles + world.
struct Fixture {
    _dir: tempfile::TempDir,
    repo: std::path::PathBuf,
    profile: Profile,
}

fn entry(keywords: &str, iuse: &str, rdepend: &str, required_use: &str, src_uri: &str) -> String {
    format!(
        "EAPI=8\nDESCRIPTION=t\nSLOT=0\nKEYWORDS={keywords}\nIUSE={iuse}\n\
         RDEPEND={rdepend}\nREQUIRED_USE={required_use}\nSRC_URI={src_uri}\n"
    )
}

fn write_pkg(repo: &std::path::Path, cpn: &str, version: &str, body: &str) {
    // Echtes md5-cache-Layout: `<kat>/<paket-version>` (zweistufig).
    let (cat, pkg) = cpn.split_once('/').unwrap();
    let dir = repo.join("metadata/md5-cache").join(cat);
    std::fs::create_dir_all(&dir).unwrap();
    let mut f = std::fs::File::create(dir.join(format!("{pkg}-{version}"))).unwrap();
    f.write_all(body.as_bytes()).unwrap();
}

fn fixture() -> Fixture {
    let dir = tempfile::tempdir().unwrap();
    let repo = dir.path().join("repo");
    // Diamond + SSL-Flag + 9999-Live + maskierte Version.
    write_pkg(
        &repo,
        "app-misc/app",
        "1.0",
        &entry(
            "amd64",
            "+ssl",
            "dev-libs/b dev-libs/c ssl? ( dev-libs/openssl )",
            "",
            "http://x/app.tar.gz",
        ),
    );
    write_pkg(
        &repo,
        "dev-libs/b",
        "1.0",
        &entry("amd64", "", "dev-libs/shared", "", "http://x/b.tar.gz"),
    );
    write_pkg(
        &repo,
        "dev-libs/c",
        "1.0",
        &entry("amd64", "", "dev-libs/shared", "", "http://x/c.tar.gz"),
    );
    write_pkg(
        &repo,
        "dev-libs/shared",
        "1.0",
        &entry("amd64", "", "", "", "http://x/s.tar.gz"),
    );
    write_pkg(
        &repo,
        "dev-libs/openssl",
        "3.0",
        &entry("amd64", "+ssl", "", "", "http://x/o.tar.gz"),
    );
    write_pkg(
        &repo,
        "app-misc/app",
        "9999",
        &entry("amd64", "", "", "", "http://x/live.tar.gz"),
    );
    let manifest_dir = repo.join("dev-libs/openssl");
    std::fs::create_dir_all(&manifest_dir).unwrap();
    std::fs::write(manifest_dir.join("Manifest"), "DIST o.tar.gz 8000\n").unwrap();
    let profiles = repo.join("profiles");
    std::fs::create_dir_all(&profiles).unwrap();
    std::fs::write(profiles.join("package.mask"), ">=dev-libs/openssl-99\n").unwrap();
    let mut profile = Profile::test_default();
    profile.package_mask = vec![">=dev-libs/openssl-99".into()];
    Fixture {
        _dir: dir,
        repo,
        profile,
    }
}

#[test]
fn index_skips_live_and_masked() {
    let fx = fixture();
    let idx = RepoIndex::load(&fx.repo, None, &fx.profile).unwrap();
    // 9999 ignoriert → hoechste stabile Version gewinnt.
    let sel = idx.select("app-misc/app").unwrap();
    assert_eq!(sel.version_str, "1.0");
    // Manifest-Summe exakt.
    assert_eq!(
        idx.select("dev-libs/openssl")
            .unwrap()
            .manifest
            .size_of("o.tar.gz"),
        Some(8000)
    );
}

#[test]
fn diamond_graph_counts_shared_once_with_blame() {
    let fx = fixture();
    let idx = RepoIndex::load(&fx.repo, None, &fx.profile).unwrap();
    let engine = Engine::new(&idx, &fx.profile);
    let graph = resolve(&engine, &["app-misc/app".into()]).unwrap();
    // app, b, c, shared + openssl (IUSE `+ssl` ist per Default an).
    assert_eq!(graph.nodes.len(), 5);
    assert_eq!(graph.refcounts.get("dev-libs/shared"), Some(&2));
    assert_eq!(graph.refcounts.get("dev-libs/openssl"), Some(&1));
    let chains = graph.blame("dev-libs/shared");
    assert!(!chains.is_empty());
    // Pfad endet in der Welt.
    assert!(
        chains
            .iter()
            .any(|c| c.iter().any(|l| l.required_by == "app-misc/app"))
    );
}

#[test]
fn bdepend_does_not_inflate_runtime() {
    let fx = fixture();
    write_pkg(
        &fx.repo,
        "dev-libs/b",
        "1.0",
        &entry("amd64", "", "dev-libs/shared", "", "http://x/b.tar.gz"),
    );
    // BDEPEND separat propagieren: eigener Eintrag mit BDEPEND auf Tool.
    let bdep = "EAPI=8\nDESCRIPTION=t\nSLOT=0\nKEYWORDS=amd64\nIUSE=\nRDEPEND=dev-libs/shared\nBDEPEND=dev-build/tool\nSRC_URI=http://x/b.tar.gz\n";
    write_pkg(&fx.repo, "dev-libs/b", "1.0", bdep);
    write_pkg(
        &fx.repo,
        "dev-build/tool",
        "1.0",
        &entry("amd64", "", "", "", "http://x/tool.tar.gz"),
    );
    let idx = RepoIndex::load(&fx.repo, None, &fx.profile).unwrap();
    let engine = Engine::new(&idx, &fx.profile);
    let graph = resolve(&engine, &["app-misc/app".into()]).unwrap();
    assert!(!graph.nodes.contains_key("dev-build/tool"));
    assert!(graph.build_extra.binpkg_bytes >= 0);
}

#[test]
fn shared_ssl_delta_zero_until_last_user() {
    let fx = fixture();
    let idx = RepoIndex::load(&fx.repo, None, &fx.profile).unwrap();
    let engine = Engine::new(&idx, &fx.profile);
    let world = vec!["app-misc/app".to_string()];
    let with_ssl = {
        let mut e = engine.fork();
        e.overrides.insert(
            "app-misc/app".into(),
            [("ssl".to_string(), true)].into_iter().collect(),
        );
        e
    };
    let d = flag_delta(&with_ssl, &world, "app-misc/app", "ssl").unwrap();
    // Nur in der Welt, kein Binhost: Binpkg-Delta 0, Download-Delta negativ
    // (Manifest `o.tar.gz` mit 8000 B faellt weg).
    assert_eq!(d.delta.binpkg_bytes, 0);
    assert!(d.delta.download_bytes < 0);
    let levers = big_levers(&engine, &world).unwrap();
    assert!(!levers.is_empty());
}

#[test]
fn plan_roundtrip_and_export_syntax() {
    let fx = fixture();
    let world_file = fx._dir.path().join("world");
    std::fs::write(&world_file, "app-misc/app\n").unwrap();
    let mut plan = Plan::init_from_world(&world_file, "test").unwrap();
    plan.package
        .push(gentoo_dep_tui::mod_09_plan_toml::PlanPackage {
            atom: "app-misc/app".into(),
            use_flags: vec!["ssl".into(), "-doc".into()],
        });
    let path = fx._dir.path().join("plan.toml");
    plan.save(&path).unwrap();
    let back = Plan::load(&path).unwrap();
    assert_eq!(back.package[0].use_flags, vec!["ssl", "-doc"]);
    let out = fx._dir.path().join("export");
    let exported = export_plan(&back, &out).unwrap();
    assert_eq!(exported.package_use, vec!["app-misc/app ssl -doc"]);
    // Portage-Syntax: world + package.use Dateien.
    let world_text = std::fs::read_to_string(out.join("world")).unwrap();
    assert!(world_text.contains("app-misc/app"));
}

#[test]
fn verify_parser_handles_profiles_and_manifest_behaviour() {
    // Profil-Vererbung (parent) wendet use.mask/use.force an.
    let d = tempfile::tempdir().unwrap();
    let profiles = d.path().join("profiles");
    let base = profiles.join("base");
    let child = profiles.join("child");
    std::fs::create_dir_all(&base).unwrap();
    std::fs::create_dir_all(&child).unwrap();
    std::fs::write(base.join("make.defaults"), "ARCH=amd64\n").unwrap();
    std::fs::write(base.join("use.mask"), "wayland\n").unwrap();
    std::fs::write(child.join("parent"), "../base\n").unwrap();
    let prof = mod_02_profile::load_profile_stack(&child, &profiles).unwrap();
    assert_eq!(prof.arch, "amd64");
    let mask = mod_02_profile::read_flag_list(&base.join("use.mask")).unwrap();
    assert!(mask.contains("wayland"));
    // Verify-Parser: sauber vs. REQUIRED_USE-Fehler.
    let ok = parse_emerge_pretend("[ebuild   R   ] dev-libs/openssl-3.3.2\n");
    assert!(ok.success);
    let bad = parse_emerge_pretend("!!! REQUIRED_USE unsatisfied: wayland? ( egl )\n");
    assert!(!bad.success);
    // Manifest-Edge-Cases (leere Felder).
    let m = parse_manifest("DIST \nDIST a.tar.gz notanumber\nDIST b.tar.gz 42\n");
    assert_eq!(m.size_of("b.tar.gz"), Some(42));
    assert_eq!(m.size_of("a.tar.gz"), None);
    // Export gegen Baseline-Differenz.
    let mut cur = HashMap::new();
    cur.insert("a/b".to_string(), ["x".into()].into_iter().collect());
    let exp = export_portage(
        &["a/b".into()],
        &cur,
        &HashMap::new(),
        &d.path().join("exp"),
    )
    .unwrap();
    assert_eq!(exp.package_use, vec!["a/b x"]);
}
