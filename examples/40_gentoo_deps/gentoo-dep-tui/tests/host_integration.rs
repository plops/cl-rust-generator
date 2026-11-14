//! Host-Integrationstests auf synthetischen `tempfile`-Repos.

use std::io::Write;

use gentoo_dep_tui::mod_02_profile::{self, Profile};
use gentoo_dep_tui::mod_03_manifest::parse_manifest;
use gentoo_dep_tui::mod_05_repo_index::RepoIndex;
use gentoo_dep_tui::mod_06_graph::{Engine, resolve};
use gentoo_dep_tui::mod_07_cascade::resolve_with_cascade_opts;
use gentoo_dep_tui::mod_08_delta::{big_levers, cut_package, flag_delta};

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
    let (cat, pkg) = cpn.split_once('/').unwrap();
    let dir = repo.join("metadata/md5-cache").join(cat);
    std::fs::create_dir_all(&dir).unwrap();
    let mut f = std::fs::File::create(dir.join(format!("{pkg}-{version}"))).unwrap();
    f.write_all(body.as_bytes()).unwrap();
}

fn fixture() -> Fixture {
    let dir = tempfile::tempdir().unwrap();
    let repo = dir.path().join("repo");
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
    profile.sync_pattern_caches();
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
    let sel = idx.select("app-misc/app").unwrap();
    assert_eq!(sel.version_str, "1.0");
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
    assert_eq!(graph.nodes.len(), 5);
    assert_eq!(graph.refcounts.get("dev-libs/shared"), Some(&2));
    assert_eq!(graph.refcounts.get("dev-libs/openssl"), Some(&1));
    let chains = graph.blame("dev-libs/shared");
    assert!(!chains.is_empty());
    assert!(
        chains
            .iter()
            .any(|c| c.iter().any(|l| l.required_by == "app-misc/app"))
    );
}

#[test]
fn world_package_blame_is_clean() {
    let fx = fixture();
    let idx = RepoIndex::load(&fx.repo, None, &fx.profile).unwrap();
    let engine = Engine::new(&idx, &fx.profile);
    let graph = resolve(&engine, &["app-misc/app".into()]).unwrap();
    assert!(graph.blame("app-misc/app").is_empty());
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
    assert_eq!(d.delta.binpkg_bytes, 0);
    assert!(d.delta.download_bytes < 0);
    let levers = big_levers(&engine, &world).unwrap();
    assert!(!levers.is_empty());
}

#[test]
fn cut_plan_finds_real_holders() {
    let fx = fixture();
    let idx = RepoIndex::load(&fx.repo, None, &fx.profile).unwrap();
    let engine = Engine::new(&idx, &fx.profile);
    let world = vec!["app-misc/app".to_string()];
    let base = resolve_with_cascade_opts(&mut engine.fork(), &world, false, true).unwrap();
    let plan = cut_package(&engine, &world, &base, "dev-libs/openssl")
        .unwrap()
        .expect("Cut-Plan erwartet");
    assert!(plan.flag_blockers.iter().any(|b| b.package == "app-misc/app"));
    assert!(plan.world_blockers.is_empty());
    assert!(!plan.without_flag_blockers.removed_packages.is_empty());
}

#[test]
fn big_levers_sorted_by_pkg_delta_without_binhost() {
    let fx = fixture();
    let idx = RepoIndex::load(&fx.repo, None, &fx.profile).unwrap();
    let engine = Engine::new(&idx, &fx.profile);
    let world = vec!["app-misc/app".to_string()];
    let levers = big_levers(&engine, &world).unwrap();
    assert!(!levers.is_empty());
    assert!(levers.iter().all(|l| !l.turn_on));
    let mut sorted = levers.clone();
    sorted.sort_by(|a, b| {
        a.net_pkg_delta
            .cmp(&b.net_pkg_delta)
            .then_with(|| a.total.download_bytes.cmp(&b.total.download_bytes))
            .then_with(|| a.flag.cmp(&b.flag))
    });
    let flags: Vec<&str> = levers.iter().map(|l| l.flag.as_str()).collect();
    let expected: Vec<&str> = sorted.iter().map(|l| l.flag.as_str()).collect();
    assert_eq!(flags, expected);
}

#[test]
fn node_costs_diamond() {
    let fx = fixture();
    let idx = RepoIndex::load(&fx.repo, None, &fx.profile).unwrap();
    let engine = Engine::new(&idx, &fx.profile);
    let graph = resolve(&engine, &["app-misc/app".into()]).unwrap();
    let costs = graph.node_costs();
    let app = costs.get("app-misc/app").unwrap();
    assert_eq!(app.exclusive_packages, graph.nodes.len());
    let shared = costs.get("dev-libs/shared").unwrap();
    assert!(shared.exclusive_packages < app.exclusive_packages);
}

#[test]
fn verify_parser_profiles_and_manifest_behaviour() {
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
    let m = parse_manifest("DIST \nDIST a.tar.gz notanumber\nDIST b.tar.gz 42\n");
    assert_eq!(m.size_of("b.tar.gz"), Some(42));
    assert_eq!(m.size_of("a.tar.gz"), None);
}

#[test]
fn missing_world_is_an_error_with_hint() {
    use gentoo_dep_tui::mod_11_cli::{CommonArgs, load_world};
    let args = CommonArgs {
        repo: std::path::PathBuf::from("/nonexistent-repo"),
        profile_dir: None,
        profile: None,
        world: std::path::PathBuf::from("/nonexistent-world-12345"),
        binhost: None,
        make_conf: None,
        package_use: None,
        portage_dir: std::path::PathBuf::from("/nonexistent-portage"),
        snapshot: None,
        json: false,
        perf_report: false,
    };
    let err = load_world(&args).unwrap_err();
    let msg = format!("{err:#}");
    assert!(msg.contains("--world"), "Hinweis fehlt: {msg}");
}

#[test]
fn ffmpeg_svt_av1_delta_lists_package() {
    use gentoo_dep_tui::mod_06_graph::Engine;
    use gentoo_dep_tui::mod_08_delta::package_deltas;
    let text_ffmpeg = entry(
        "amd64",
        "svt-av1",
        "svt-av1? ( media-libs/svt-av1 )",
        "",
        "http://x/ffmpeg.tar.gz",
    );
    let text_lib = entry("amd64", "", "", "", "http://x/svt.tar.gz");
    let dir = tempfile::tempdir().unwrap();
    let repo = dir.path().join("repo");
    write_pkg(&repo, "media-video/ffmpeg", "7.0", &text_ffmpeg);
    write_pkg(&repo, "media-libs/svt-av1", "2.0", &text_lib);
    let profile = Profile::test_default();
    let index = RepoIndex::load(&repo, None, &profile).unwrap();
    let engine = Engine::new(&index, &profile);
    let world = vec!["media-video/ffmpeg".to_string()];
    let deltas = package_deltas(&engine, &world, "media-video/ffmpeg").unwrap();
    let svt = deltas.iter().find(|d| d.flag == "svt-av1").unwrap();
    assert_eq!(svt.added_packages, vec!["media-libs/svt-av1".to_string()]);
}
