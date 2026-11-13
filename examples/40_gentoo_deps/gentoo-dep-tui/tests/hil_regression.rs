//! HIL-Regressionstests auf echten Gentoo-Daten.
//!
//! Aktiv nur mit `GENTOO_HIL_REPO=/tmp/gentoo-hil` (siehe T02-Snapshot);
//! ohne Datensatz sauber uebersprungen, sodass `cargo test` immer gruen ist.

use std::path::PathBuf;

fn hil_repo() -> Option<PathBuf> {
    std::env::var("GENTOO_HIL_REPO").ok().map(PathBuf::from)
}

fn world_of(repo: &std::path::Path) -> Option<Vec<String>> {
    let candidates = [repo.join("world"), repo.join("var/lib/portage/world")];
    for world in &candidates {
        if let Ok(text) = std::fs::read_to_string(world) {
            let atoms: Vec<String> = text
                .lines()
                .map(str::trim)
                .filter(|l| !l.is_empty() && !l.starts_with('#'))
                .map(str::to_string)
                .collect();
            if !atoms.is_empty() {
                return Some(atoms);
            }
        }
    }
    None
}

/// Repo-Root im Snapshot (`<base>/repo`, Config liegt auf `<base>`).
fn repo_root(base: &std::path::Path) -> PathBuf {
    base.join("repo")
}

/// Vollstaendiges Profil zum Snapshot (Stapel + make.conf + package.use).
fn hil_full_setup(
    base: &std::path::Path,
) -> Option<(
    gentoo_dep_tui::mod_02_profile::Profile,
    gentoo_dep_tui::mod_05_repo_index::RepoIndex,
    Vec<String>,
)> {
    let world = world_of(base)?;
    let root = repo_root(base);
    let profiles = root.join("profiles");
    let mut profile = gentoo_dep_tui::mod_02_profile::load_profile_stack(
        &profiles.join("default/linux/amd64/23.0/no-multilib"),
        &profiles,
    )
    .ok()?;
    profile.profile_name = "default/linux/amd64/23.0/no-multilib".into();
    if profile.arch.is_empty() {
        profile.arch = "amd64".into();
    }
    if profile.accept_keywords.is_empty() {
        profile.accept_keywords.insert(profile.arch.clone());
    }
    let make_conf = base.join("make.conf");
    if let Ok(kv) = gentoo_dep_tui::mod_02_profile::read_key_value(&make_conf) {
        if let Some(u) = kv.get("USE") {
            profile.global_use = gentoo_dep_tui::mod_02_profile::parse_use_tokens(u);
        }
        if let Some(k) = kv.get("ACCEPT_KEYWORDS") {
            profile
                .accept_keywords
                .extend(gentoo_dep_tui::mod_02_profile::parse_use_tokens(k));
        }
        let vars: Vec<String> = profile.expand_defaults.keys().cloned().collect();
        for var in vars {
            if let Some(line) = kv.get(&var) {
                profile
                    .expand_defaults
                    .insert(var, line.split_whitespace().map(str::to_string).collect());
            }
        }
    }
    profile.package_use =
        gentoo_dep_tui::mod_02_profile::read_package_use(&base.join("package.use"))
            .unwrap_or_default();
    let index = gentoo_dep_tui::mod_05_repo_index::RepoIndex::load(&root, None, &profile).unwrap();
    Some((profile, index, world))
}

#[test]
fn hil_index_loads_real_tree() {
    let Some(repo) = hil_repo() else {
        eprintln!("SKIP: GENTOO_HIL_REPO nicht gesetzt");
        return;
    };
    let profile = gentoo_dep_tui::mod_02_profile::Profile {
        arch: "amd64".into(),
        accept_keywords: ["amd64".to_string()].into_iter().collect(),
        ..Default::default()
    };
    let index =
        gentoo_dep_tui::mod_05_repo_index::RepoIndex::load(&repo_root(&repo), None, &profile)
            .unwrap();
    assert!(
        index.packages() > 1000,
        "zu wenige Pakete: {}",
        index.packages()
    );
    eprintln!(
        "HIL index: {} Kategorien, {} Pakete in {} ms",
        index.categories(),
        index.packages(),
        index.load_ms
    );
}

#[test]
fn hil_graph_resolves_world() {
    let Some(repo) = hil_repo() else {
        eprintln!("SKIP: GENTOO_HIL_REPO nicht gesetzt");
        return;
    };
    let Some(world) = world_of(&repo) else {
        eprintln!("SKIP: keine world im Snapshot");
        return;
    };
    let profile = gentoo_dep_tui::mod_02_profile::Profile {
        arch: "amd64".into(),
        accept_keywords: ["amd64".to_string()].into_iter().collect(),
        ..Default::default()
    };
    let index =
        gentoo_dep_tui::mod_05_repo_index::RepoIndex::load(&repo_root(&repo), None, &profile)
            .unwrap();
    let engine = gentoo_dep_tui::mod_06_graph::Engine::new(&index, &profile);
    let graph = gentoo_dep_tui::mod_06_graph::resolve(&engine, &world).unwrap();
    assert!(!graph.nodes.is_empty());
    eprintln!(
        "HIL graph: {} Pakete, Binpkg {} MB",
        graph.nodes.len(),
        graph.totals().binpkg_bytes / 1_048_576
    );
}

#[test]
fn hil_deltas_and_levers_sane() {
    let Some(repo) = hil_repo() else {
        eprintln!("SKIP: GENTOO_HIL_REPO nicht gesetzt");
        return;
    };
    let Some(world) = world_of(&repo) else {
        eprintln!("SKIP: keine world im Snapshot");
        return;
    };
    let profile = gentoo_dep_tui::mod_02_profile::Profile {
        arch: "amd64".into(),
        accept_keywords: ["amd64".to_string()].into_iter().collect(),
        ..Default::default()
    };
    let index =
        gentoo_dep_tui::mod_05_repo_index::RepoIndex::load(&repo_root(&repo), None, &profile)
            .unwrap();
    let engine = gentoo_dep_tui::mod_06_graph::Engine::new(&index, &profile);
    let levers = gentoo_dep_tui::mod_08_delta::big_levers(&engine, &world).unwrap();
    assert!(!levers.is_empty());
    for lever in levers.iter().take(5) {
        eprintln!(
            "HIL lever: {:<24} Pakete {:>4} Delta {} B Konflikte {}",
            lever.flag, lever.packages, lever.total.binpkg_bytes, lever.conflicts
        );
    }
}

#[test]
fn hil_tui_core_actions_on_snapshot() {
    use gentoo_dep_tui::mod_09_plan_toml::Plan;
    use gentoo_dep_tui::mod_12_tui_state::{AppState, LeftEntry, Pane, Tab};

    let Some(base) = hil_repo() else {
        eprintln!("SKIP: GENTOO_HIL_REPO nicht gesetzt");
        return;
    };
    let Some((profile, index, world)) = hil_full_setup(&base) else {
        eprintln!("SKIP: Snapshot unvollstaendig");
        return;
    };
    let mut state = AppState::new(&index, &profile, world, Plan::default(), None).unwrap();
    assert!(!state.left.is_empty(), "linke Liste leer");
    eprintln!(
        "HIL tui: {} Knoten, {} Eintraege links, {} Konflikte",
        state.graph.nodes.len(),
        state.left.len(),
        state.conflicts.len()
    );

    // Tabs + Panes schalten (reine State-Uebergaenge).
    state.set_tab(Tab::BigLevers);
    assert!(!state.levers.is_empty(), "keine Big Levers");
    state.set_tab(Tab::Packages);
    state.cycle_pane();
    assert_eq!(state.pane, Pane::Middle);
    state.cycle_pane();
    state.cycle_pane();
    assert_eq!(state.pane, Pane::Left);

    // Erstes Paket mit schaltbaren Flags suchen, toggeln, speichern.
    let mut acted = false;
    for i in 0..state.left.len() {
        let is_pkg = matches!(
            state.left[i],
            LeftEntry::WorldPkg(_) | LeftEntry::Transitive(_)
        );
        if !is_pkg {
            continue;
        }
        state.select_left(i).unwrap();
        if state.flag_deltas.is_empty() {
            continue;
        }
        state.pane = Pane::Middle;
        state.middle_selected = 0;
        let flag = state.flag_deltas[0].flag.clone();
        let pkg = state.selected_package().unwrap();
        state.toggle_selected_flag().unwrap();
        // Entweder umgeschaltet (dirty) oder Konflikt-Modal mit Optionen.
        if state.modal.is_some() {
            eprintln!("HIL tui: Konflikt-Modal bei {pkg}[{}]", flag);
            break;
        }
        assert!(state.dirty, "toggle setzte kein dirty");
        acted = true;
        eprintln!("HIL tui: Toggle {pkg}[{flag}] ok");
        break;
    }
    assert!(
        acted || state.modal.is_some(),
        "keine Toggle-Aktion moeglich"
    );

    // Speichern + Exportieren (Portage-Syntax lesbar).
    let dir = tempfile::tempdir().unwrap();
    state.plan_path = Some(dir.path().join("plan.toml"));
    let saved = state.save().unwrap();
    let back = Plan::load(&saved).unwrap();
    assert!(!back.package.is_empty() || state.modal.is_some());
    let out = dir.path().join("export");
    let exported = gentoo_dep_tui::mod_11_cli::export_plan(&back, &out).unwrap();
    assert!(!exported.world.is_empty());
    assert!(out.join("world").is_file());
    assert!(out.join("package.use").is_file());
    eprintln!(
        "HIL tui: plan + export ok ({} world, {} package.use)",
        exported.world.len(),
        exported.package_use.len()
    );
}
