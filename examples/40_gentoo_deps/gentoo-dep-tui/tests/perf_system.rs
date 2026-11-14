//! Instrumentierter System- und Performance-Test (T01/T02).
//!
//! Baut ein deterministisches synthetisches Repo (libs + Diamond-Ketten +
//! `media-video/ffmpeg`-Simulator mit 60 USE-Flags) und misst:
//! - `RepoIndex::load`-Dauer,
//! - `package_deltas`-Dauer + `resolve_calls`/`build_extra_calls`,
//! - `big_levers`-Dauer (1 Thread vs. alle Kerne, Ergebnis-identisch),
//! - TUI-Workflow (Cursor, Filter, Toggle, Tab-Wechsel) mit Phasen-Zeiten.
//!
//! Baseline (gemessen bei Einfuehrung, siehe Commit):
//! - `sys_perf_index_load`: Repo mit 122 Cache-Dateien.
//! - `sys_perf_package_deltas`: 1 Baseline + 60 parallele Flag-Simulationen,
//!   `build_extra_calls == 0` (Runtime-only).
//! - `sys_perf_big_levers`: identische Ergebnisse 1 Thread vs. Pool.
//! - `sys_perf_tui_workflow`: Filter-Tippen und Cursor ohne Resolve.
//!
//! Laufen mit: `cargo test --release --test perf_system -- --nocapture`.

use std::io::Write;
use std::time::Instant;

use gentoo_dep_tui::mod_01_types::perf_snapshot_and_reset;
use gentoo_dep_tui::mod_02_profile::Profile;
use gentoo_dep_tui::mod_05_repo_index::RepoIndex;
use gentoo_dep_tui::mod_06_graph::Engine;
use gentoo_dep_tui::mod_07_cascade::resolve_with_cascade;
use gentoo_dep_tui::mod_08_delta::{big_levers, package_deltas};
use gentoo_dep_tui::mod_09_plan_toml::Plan;
use gentoo_dep_tui::mod_12_tui_state::{AppState, Tab};

/// Perf-Zaehler sind prozessglobal → counter-sensitive Tests serialisieren.
static SERIAL: std::sync::Mutex<()> = std::sync::Mutex::new(());

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

/// Synthetisches Repo: 100 Blatt-Libs, 20 App-Pakete (Diamond auf Libs),
/// ffmpeg-Simulator mit 60 USE-Flags, svt-av1/aom/dav1d ziehen echte Deps.
fn build_repo(dir: &tempfile::TempDir) -> std::path::PathBuf {
    let repo = dir.path().join("repo");
    for i in 0..100 {
        let cpn = format!("dev-libs/lib{i:03}");
        write_pkg(
            &repo,
            &cpn,
            "1.0",
            &entry("amd64", "ssl", "", "", "http://x/lib.tar.gz"),
        );
    }
    for i in 0..20 {
        let cpn = format!("app-misc/app{i:02}");
        // Diamond: jede App haengt an zwei Libs + shared openssl.
        let dep = format!(
            "dev-libs/lib{i:03} dev-libs/lib{:03} ssl? ( dev-libs/openssl )",
            (i + 1) % 100
        );
        write_pkg(
            &repo,
            &cpn,
            "1.0",
            &entry("amd64", "+ssl", &dep, "", "http://x/app.tar.gz"),
        );
    }
    write_pkg(
        &repo,
        "dev-libs/openssl",
        "3.0",
        &entry("amd64", "+ssl", "", "", "http://x/o.tar.gz"),
    );
    // ffmpeg-Simulator: 60 Flags, drei echte Encoder-Deps + REQUIRED_USE-Kette.
    let mut flags: Vec<String> = (0..57).map(|i| format!("f{i:02}")).collect();
    flags.extend(["svt-av1".into(), "aom".into(), "dav1d".into()]);
    let iuse = flags.join(" ");
    let mut deps: Vec<String> = (0..57)
        .map(|i| format!("f{i:02}? ( dev-libs/lib{i:03} )"))
        .collect();
    deps.extend([
        "svt-av1? ( media-libs/svt-av1 )".into(),
        "aom? ( media-libs/aom )".into(),
        "dav1d? ( media-libs/dav1d )".into(),
    ]);
    write_pkg(
        &repo,
        "media-video/ffmpeg",
        "7.0",
        &entry(
            "amd64",
            &iuse,
            &deps.join(" "),
            "svt-av1? ( aom )",
            "http://x/ffmpeg.tar.gz",
        ),
    );
    for (cpn, ver) in [
        ("media-libs/svt-av1", "2.0"),
        ("media-libs/aom", "3.0"),
        ("media-libs/dav1d", "1.0"),
    ] {
        write_pkg(
            &repo,
            cpn,
            ver,
            &entry("amd64", "", "", "", "http://x/enc.tar.gz"),
        );
    }
    // Manifests mit echten DIST-Groessen (sonst waeren alle dl-Deltas 0).
    for (cat, pkg, dist, size) in [
        ("media-video", "ffmpeg", "ffmpeg.tar.gz", 5_000),
        ("media-libs", "svt-av1", "enc.tar.gz", 8_000),
        ("media-libs", "aom", "enc.tar.gz", 9_000),
        ("media-libs", "dav1d", "enc.tar.gz", 4_000),
        ("dev-libs", "openssl", "o.tar.gz", 7_000),
    ] {
        let dir = repo.join(cat).join(pkg);
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join("Manifest"), format!("DIST {dist} {size}\n")).unwrap();
    }
    // profiles-Verzeichnis (leer, Default-Profil reicht).
    std::fs::create_dir_all(repo.join("profiles")).unwrap();
    repo
}

fn fixture_repo() -> (tempfile::TempDir, std::path::PathBuf, Profile) {
    let dir = tempfile::tempdir().unwrap();
    let repo = build_repo(&dir);
    (dir, repo, Profile::test_default())
}

#[test]
fn sys_perf_index_load() {
    let _guard = SERIAL.lock().unwrap_or_else(|e| e.into_inner());
    let (_dir, repo, profile) = fixture_repo();
    let _ = perf_snapshot_and_reset();
    let start = Instant::now();
    let index = RepoIndex::load(&repo, None, &profile).unwrap();
    let ms = start.elapsed().as_millis();
    let stats = perf_snapshot_and_reset();
    println!(
        "sys_perf_index_load: {ms} ms, cache_files={}, resolve_calls={}",
        index.cache_files, stats.resolve_calls
    );
    assert!(index.packages() >= 120, "Pakete: {}", index.packages());
}

#[test]
fn sys_perf_package_deltas() {
    let _guard = SERIAL.lock().unwrap_or_else(|e| e.into_inner());
    let (_dir, repo, profile) = fixture_repo();
    let index = RepoIndex::load(&repo, None, &profile).unwrap();
    let engine = Engine::new(&index, &profile);
    let world = vec!["media-video/ffmpeg".to_string()];
    let _ = perf_snapshot_and_reset();
    let start = Instant::now();
    let deltas = package_deltas(&engine, &world, "media-video/ffmpeg").unwrap();
    let ms = start.elapsed().as_millis();
    let stats = perf_snapshot_and_reset();
    println!(
        "sys_perf_package_deltas: {ms} ms, flags={}, resolve_calls={}, build_extra_calls={}, delta_runs={}",
        deltas.len(),
        stats.resolve_calls,
        stats.build_extra_calls,
        stats.delta_runs
    );
    assert_eq!(deltas.len(), 60);
    // Simulationen sind Runtime-only: kein BDEPEND-Lauf.
    assert_eq!(stats.build_extra_calls, 0);
    // svt-av1 zieht svt-av1 (REQUIRED_USE-Kaskade `svt-av1? ( aom )`
    // schaltet aom mit ein → auch media-libs/aom).
    let svt = deltas.iter().find(|d| d.flag == "svt-av1").unwrap();
    assert!(
        svt.added_packages
            .contains(&"media-libs/svt-av1".to_string())
    );
    assert!(svt.added_packages.contains(&"media-libs/aom".to_string()));
}

#[test]
fn sys_perf_big_levers() {
    let _guard = SERIAL.lock().unwrap_or_else(|e| e.into_inner());
    let (_dir, repo, profile) = fixture_repo();
    let index = RepoIndex::load(&repo, None, &profile).unwrap();
    let engine = Engine::new(&index, &profile);
    let mut world: Vec<String> = (0..20).map(|i| format!("app-misc/app{i:02}")).collect();
    world.push("media-video/ffmpeg".to_string());
    let single = rayon::ThreadPoolBuilder::new()
        .num_threads(1)
        .build()
        .unwrap();
    let _ = perf_snapshot_and_reset();
    let t1 = Instant::now();
    let one = single.install(|| big_levers(&engine, &world).unwrap());
    let ms1 = t1.elapsed().as_millis();
    let calls1 = perf_snapshot_and_reset();
    let t2 = Instant::now();
    let all = big_levers(&engine, &world).unwrap();
    let ms_all = t2.elapsed().as_millis();
    let calls_all = perf_snapshot_and_reset();
    println!(
        "sys_perf_big_levers: 1 Thread {ms1} ms (resolves={}), Pool {ms_all} ms (resolves={}), levers={}",
        calls1.resolve_calls,
        calls_all.resolve_calls,
        all.len()
    );
    assert_eq!(one, all, "1 Thread vs. Pool muessen identisch sein");
    assert!(!all.is_empty());
}

#[test]
fn sys_perf_tui_workflow() {
    let _guard = SERIAL.lock().unwrap_or_else(|e| e.into_inner());
    let (_dir, repo, profile) = fixture_repo();
    let index = RepoIndex::load(&repo, None, &profile).unwrap();
    let world = vec!["media-video/ffmpeg".to_string()];
    let _ = perf_snapshot_and_reset();
    // Leak fuer 'static (Test-Prozess, einmalig).
    let index: &'static RepoIndex = Box::leak(Box::new(index));
    let profile: &'static Profile = Box::leak(Box::new(profile));
    let t0 = Instant::now();
    let mut state = AppState::new(index, profile, world, Plan::default(), None).unwrap();
    let init_ms = t0.elapsed().as_millis();
    // 10x Cursor: reine Selektion (kein Resolve).
    let t1 = Instant::now();
    for _ in 0..10 {
        state.move_selection(1);
    }
    let moves_ms = t1.elapsed().as_millis();
    // Nachladen (erster Besuch: parallel; Rest aus Cache).
    let t2 = Instant::now();
    state.ensure_deltas().unwrap();
    let ensure_ms = t2.elapsed().as_millis();
    // 5x Filter-Tippen: nur Liste filtern.
    let t3 = Instant::now();
    state.filtering = true;
    for c in ['f', 'f', 'm', 'p', 'e'] {
        state.filter.push(c);
        state.apply_filter_only();
    }
    let filter_ms = t3.elapsed().as_millis();
    state.filtering = false;
    // 3x Toggle: je ein Refresh (Resolve + Deltas des Pakets).
    let t4 = Instant::now();
    state.pane = gentoo_dep_tui::mod_12_tui_state::Pane::Middle;
    for _ in 0..3 {
        if state.flag_deltas.is_empty() {
            state.ensure_deltas().unwrap();
        }
        if !state.toggle_selected_flag().unwrap() {
            state.middle_selected = (state.middle_selected + 1).max(1);
        }
    }
    let toggle_ms = t4.elapsed().as_millis();
    // Tab-Wechsel: lazy Big Levers.
    let t5 = Instant::now();
    state.set_tab(Tab::BigLevers).unwrap();
    let levers_ms = t5.elapsed().as_millis();
    let stats = perf_snapshot_and_reset();
    println!(
        "sys_perf_tui_workflow: init={init_ms}ms moves10={moves_ms}ms ensure={ensure_ms}ms filter5={filter_ms}ms toggle3={toggle_ms}ms levers={levers_ms}ms levers_n={} resolves={}",
        state.levers.len(),
        stats.resolve_calls
    );
    // Harte Budgets: Navigation + Filter duerfen nie blockieren.
    assert!(moves_ms < 1000, "10x Cursor zu langsam: {moves_ms}ms");
    assert!(filter_ms < 1000, "5x Filter zu langsam: {filter_ms}ms");
    assert!(!state.levers.is_empty());
}

/// Optionaler HIL-Snapshot (`GENTOO_HIL_REPO=/tmp/gentoo-hil`), sonst Skip.
#[test]
fn sys_perf_hil_snapshot() {
    let Some(repo) = std::env::var("GENTOO_HIL_REPO")
        .ok()
        .filter(|s| !s.is_empty())
    else {
        eprintln!("sys_perf_hil_snapshot: SKIP (GENTOO_HIL_REPO nicht gesetzt)");
        return;
    };
    let repo = std::path::PathBuf::from(repo);
    if !repo.is_dir() {
        eprintln!(
            "sys_perf_hil_snapshot: SKIP (kein Verzeichnis: {})",
            repo.display()
        );
        return;
    }
    // Dasselbe Basis-Layout wie `hil_regression` akzeptieren (`<base>/repo`).
    let nested = repo.join("repo");
    let repo = if nested.join("metadata").join("md5-cache").is_dir() {
        nested
    } else {
        repo
    };
    let profile = Profile::test_default();
    let start = Instant::now();
    let index = RepoIndex::load(&repo, None, &profile).unwrap();
    println!(
        "sys_perf_hil_snapshot: {} Pakete in {} ms",
        index.packages(),
        start.elapsed().as_millis()
    );
    let outcome = resolve_with_cascade(
        &mut Engine::new(&index, &profile),
        &["media-video/ffmpeg".to_string()],
    );
    match outcome {
        Ok(o) => println!(
            "  ffmpeg-Graph: {} Knoten, {} Konflikte",
            o.graph.nodes.len(),
            o.conflicts.len()
        ),
        Err(e) => println!("  ffmpeg-Graph: Fehler {e:#}"),
    }
}
