//! Skriptbare TUI-Tests mit Screencapture-Export (review3 Task 6.2).
//!
//! Genau eine Zustaendigkeit: Tastatur-Skripte gegen `TestBackend` fahren und
//! jeden Frame als Text-Screenshot sichern. Derselbe `dispatch_key` wie die
//! echte TUI (kein Sonderpfad), dazu Assertions auf die Review-Befunde aus
//! `plan/20261114_01_review3` (Filter-Stale, `Pkg-Quell-DL`-Label,
//! Shared-Blocker, Phantom-Flags, Hilfe, Konflikt-Modal).
//!
//! Laufen mit: `cargo test --test tui_screencaps`.
//! Ablage: `target/screencaps/review_screencaps.md`, oder wohin
//! `GENTOO_SCREENCAP_OUT=<pfad>` zeigt.

use std::io::Write;
use std::path::{Path, PathBuf};

use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use ratatui::Terminal;
use ratatui::backend::TestBackend;

use gentoo_dep_tui::mod_02_profile::Profile;
use gentoo_dep_tui::mod_05_repo_index::RepoIndex;
use gentoo_dep_tui::mod_12_tui_state::AppState;
use gentoo_dep_tui::mod_13_tui_view::render;
use gentoo_dep_tui::mod_14_tui_runner::{RunnerContext, dispatch_key, step_idle};

/// Ein Skript-Schritt (Tastatur, Idle-Recompute oder Capture).
enum Step {
    /// Einzelne Taste (identischer Dispatch wie echte TUI).
    Key(KeyCode),
    /// Zeichenkette tippen (Filter-Eingabe).
    Type(&'static str),
    /// Idle-Recompute bis zur Ruhe (Deltas + Shared-Analyse).
    Idle,
    /// Frame als Text-Screenshot sichern.
    Capture(&'static str),
}

/// Treiber: State + Tasten-Dispatch + TestBackend-Rendering.
struct Driver<'x> {
    state: AppState<'x>,
    args: gentoo_dep_tui::mod_11_cli::TuiArgs,
    width: u16,
    height: u16,
    frames: Vec<(String, String)>,
}

impl<'x> Driver<'x> {
    fn key(&mut self, code: KeyCode) {
        let key = KeyEvent::new(code, KeyModifiers::NONE);
        let mut ctx = RunnerContext {
            args: &self.args,
        };
        let _ = dispatch_key(&mut self.state, key, &mut ctx).unwrap();
    }

    fn idle(&mut self) {
        for _ in 0..50 {
            if !self.state.deltas_loading {
                break;
            }
            step_idle(&mut self.state).unwrap();
        }
        // Shared-Analyse + Cache-Endzustand sichern.
        step_idle(&mut self.state).unwrap();
    }

    fn capture(&mut self, name: &'static str) {
        let backend = TestBackend::new(self.width, self.height);
        let mut terminal = Terminal::new(backend).unwrap();
        terminal.draw(|frame| render(frame, &self.state)).unwrap();
        let buf = terminal.backend().buffer().clone();
        let area = buf.area;
        let mut rows = Vec::new();
        for y in 0..area.height {
            let mut row = String::new();
            for x in 0..area.width {
                row.push_str(buf[(x, y)].symbol());
            }
            rows.push(row.trim_end().to_string());
        }
        // Leerzeilen am Ende kuerzen (stabile Diffs).
        while rows.last().is_some_and(|l| l.is_empty()) {
            rows.pop();
        }
        self.frames.push((name.to_string(), rows.join("\n")));
    }

    fn run(&mut self, steps: &[Step]) {
        for step in steps {
            match step {
                Step::Key(code) => self.key(*code),
                Step::Type(text) => {
                    for c in text.chars() {
                        self.key(KeyCode::Char(c));
                    }
                }
                Step::Idle => self.idle(),
                Step::Capture(name) => self.capture(name),
            }
        }
    }

    fn frame(&self, name: &str) -> &str {
        &self
            .frames
            .iter()
            .find(|(n, _)| n == name)
            .unwrap_or_else(|| panic!("Frame fehlt: {name}"))
            .1
    }
}

/// Cache-Eintrag schreiben (`<kat>/<paket-version>` unter md5-cache).
fn write_cache(repo: &Path, cpn: &str, version: &str, body: &str) {
    let (cat, pkg) = cpn.split_once('/').unwrap();
    let dir = repo.join("metadata/md5-cache").join(cat);
    std::fs::create_dir_all(&dir).unwrap();
    let mut f = std::fs::File::create(dir.join(format!("{pkg}-{version}"))).unwrap();
    f.write_all(body.as_bytes()).unwrap();
}

fn entry(keywords: &str, iuse: &str, rdepend: &str, required_use: &str, src_uri: &str) -> String {
    format!(
        "EAPI=8\nDESCRIPTION=t\nSLOT=0\nKEYWORDS={keywords}\nIUSE={iuse}\n\
         RDEPEND={rdepend}\nREQUIRED_USE={required_use}\nSRC_URI={src_uri}\n"
    )
}

/// Manifest schreiben (`<kat>/<pkg>/Manifest`).
fn write_manifest(repo: &Path, cpn: &str, dist: &str, size: u64) {
    let (cat, pkg) = cpn.split_once('/').unwrap();
    let dir = repo.join(cat).join(pkg);
    std::fs::create_dir_all(&dir).unwrap();
    std::fs::write(dir.join("Manifest"), format!("DIST {dist} {size}\n")).unwrap();
}

/// Realitaetsnahes synthetisches Repo aus dem Review-Szenario:
/// `gentoo-sources` (`!build?`), `ffmpeg` (`svt-av1` + REQUIRED_USE),
/// `emacs` (`gui`/`Xaw3d`/`gtk`) + `firefox-bin` als Shared-Blocker,
/// Konflikt-Paar `app/a`/`app/c` ueber `libx[foo]`.
fn build_repo(dir: &tempfile::TempDir) -> PathBuf {
    let repo = dir.path().join("repo");
    // Kernel mit invertierter Bedingung (Review Screenshot 1).
    write_cache(
        &repo,
        "sys-kernel/gentoo-sources",
        "6.6.7",
        &entry(
            "amd64",
            "build vanilla test-rust",
            "!build? ( sys-apps/bc )",
            "",
            "http://x/linux.tar.gz",
        ),
    );
    write_cache(
        &repo,
        "sys-apps/bc",
        "1.07",
        &entry("amd64", "", "", "", "http://x/bc.tar.gz"),
    );
    // ffmpeg mit Encoder-Flags + REQUIRED_USE-Kette.
    write_cache(
        &repo,
        "media-video/ffmpeg",
        "7.0",
        &entry(
            "amd64",
            "svt-av1 aom dav1d",
            "svt-av1? ( media-libs/svt-av1 )",
            "svt-av1? ( aom )",
            "http://x/ffmpeg.tar.gz",
        ),
    );
    for (cpn, ver) in [
        ("media-libs/svt-av1", "2.0"),
        ("media-libs/aom", "3.0"),
        ("media-libs/dav1d", "1.0"),
    ] {
        write_cache(
            &repo,
            cpn,
            ver,
            &entry("amd64", "", "", "", "http://x/enc.tar.gz"),
        );
    }
    // emacs: Toolkit-Fallback Kette + Shared-Blocker firefox-bin.
    write_cache(
        &repo,
        "app-editors/emacs",
        "30.1",
        &entry(
            "amd64",
            "+gui +Xaw3d +gtk",
            "Xaw3d? ( x11-libs/libXaw3d ) gtk? ( x11-libs/gtk+ )",
            "",
            "http://x/emacs.tar.gz",
        ),
    );
    write_cache(
        &repo,
        "x11-libs/libXaw3d",
        "1.6",
        &entry("amd64", "", "", "", "http://x/xaw3d.tar.gz"),
    );
    write_cache(
        &repo,
        "x11-libs/gtk+",
        "3.24",
        &entry("amd64", "", "", "", "http://x/gtk.tar.gz"),
    );
    write_cache(
        &repo,
        "www-client/firefox-bin",
        "130.0",
        &entry("amd64", "", "x11-libs/gtk+", "", "http://x/ff.tar.gz"),
    );
    // Konflikt-Paar: gegenlaeufige `use_deps` auf libx.
    write_cache(
        &repo,
        "app-a/a",
        "1.0",
        &entry("amd64", "", "dev-libs/libx[foo]", "", "http://x/a.tar.gz"),
    );
    write_cache(
        &repo,
        "app-c/c",
        "1.0",
        &entry("amd64", "", "dev-libs/libx[-foo]", "", "http://x/c.tar.gz"),
    );
    write_cache(
        &repo,
        "dev-libs/libx",
        "1.0",
        &entry("amd64", "foo", "", "", "http://x/libx.tar.gz"),
    );
    // Manifests mit echten DIST-Groessen (sonst waeren alle dl-Deltas 0;
    // fehlende Manifeste wuerden das Paket sogar herausfiltern).
    for (cpn, dist, size) in [
        ("sys-kernel/gentoo-sources", "linux.tar.gz", 5_000),
        ("sys-apps/bc", "bc.tar.gz", 700),
        ("media-video/ffmpeg", "ffmpeg.tar.gz", 11_200),
        ("media-libs/svt-av1", "enc.tar.gz", 8_000),
        ("media-libs/aom", "enc.tar.gz", 8_000),
        ("media-libs/dav1d", "enc.tar.gz", 8_000),
        ("app-editors/emacs", "emacs.tar.gz", 9_000),
        ("x11-libs/libXaw3d", "xaw3d.tar.gz", 220),
        ("x11-libs/gtk+", "gtk.tar.gz", 5_000),
        ("www-client/firefox-bin", "ff.tar.gz", 90_000),
        ("app-a/a", "a.tar.gz", 100),
        ("app-c/c", "c.tar.gz", 100),
        ("dev-libs/libx", "libx.tar.gz", 500),
    ] {
        write_manifest(&repo, cpn, dist, size);
    }
    std::fs::create_dir_all(repo.join("profiles")).unwrap();
    repo
}

/// Standard-Welt zum Szenario.
fn scenario_world() -> Vec<String> {
    vec![
        "sys-kernel/gentoo-sources".to_string(),
        "media-video/ffmpeg".to_string(),
        "app-editors/emacs".to_string(),
        "www-client/firefox-bin".to_string(),
        "app-a/a".to_string(),
        "app-c/c".to_string(),
    ]
}


/// Frames als lesbares Review-Artefakt sichern (Markdown).
fn save_screencaps(frames: &[(String, String)], size: &str, out: &Path) {
    if let Some(parent) = out.parent()
        && !parent.as_os_str().is_empty()
    {
        std::fs::create_dir_all(parent).unwrap();
    }
    let mut text = format!("# TUI-Screencaps ({size}, Review-Szenario)\n\n");
    for (name, frame) in frames {
        text.push_str(&format!("## {name}\n\n```text\n{frame}\n```\n\n"));
    }
    std::fs::write(out, text).unwrap();
}

fn screencap_out(default: &str) -> PathBuf {
    std::env::var("GENTOO_SCREENCAP_OUT")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from(default))
}

/// Vollstaendiges Review-Skript fahren (ein Treiber je Groesse).
fn run_scenario(width: u16, height: u16) -> Vec<(String, String)> {
    let dir = tempfile::tempdir().unwrap();
    let repo = build_repo(&dir);
    let mut profile = Profile::test_default();
    // Phantom-Flag global setzen (darf nirgends auftauchen, review3 1.1).
    profile.global_use.insert("openmp".to_string());
    profile.sync_pattern_caches();
    let index = RepoIndex::load(&repo, None, &profile).unwrap();
    let world = scenario_world();
    let mut state = AppState::new(&index, &profile, world).unwrap();
    // Offline-Flag-Doku einspeisen (wie `use.local.desc`).
    let desc_dir = repo.join("profiles");
    std::fs::write(
        desc_dir.join("use.local.desc"),
        "app-editors/emacs:gtk - GTK-Toolkit\n",
    )
    .unwrap();
    state.use_descriptions = gentoo_dep_tui::mod_02_profile::UseDescriptions::load(&desc_dir);

    let args = gentoo_dep_tui::mod_11_cli::TuiArgs {
        common: gentoo_dep_tui::mod_11_cli::CommonArgs {
            repo: repo.clone(),
            profile_dir: None,
            profile: None,
            world: dir.path().join("world"),
            binhost: None,
            make_conf: Some(dir.path().join("make.conf")),
            package_use: Some(dir.path().join("package.use")),
            portage_dir: dir.path().join("portage"),
            snapshot: None,
            json: false,
            perf_report: false,
        },
        out_use: dir.path().join("package.use"),
    };
    let mut driver = Driver {
        state,
        args,
        width,
        height,
        frames: Vec::new(),
    };
    use Step::{Capture, Idle, Key, Type};
    driver.run(&[
        Capture("01_start"),
        // Suche `/ffm`: nur ffmpeg bleibt — keine gentoo-sources-Flags,
        // Label `Pkg-Quell-DL` statt `Subgraph-DL`.
        Type("/ffm"),
        Key(KeyCode::Enter),
        Idle,
        Capture("02_ffmpeg_after_filter"),
        // Sortierung `o`: exklusive Kosten erscheinen links.
        Key(KeyCode::Char('o')),
        Capture("02b_sorted"),
        Key(KeyCode::Char('o')),
        Key(KeyCode::Char('o')),
        // Suche `/emacs`, auf gtk navigieren: Shared-Blocker + Doku.
        Key(KeyCode::Char('/')),
        Type("emacs"),
        Key(KeyCode::Enter),
        Idle,
        Key(KeyCode::Tab),
        Key(KeyCode::Down),
        Idle,
        Capture("03_emacs_shared_blocker"),
        // Tab in Pane::Right: Auto-Cut, dann `a` (apply) und `u` (undo).
        Key(KeyCode::Tab),
        Idle,
        Capture("03b_cut_plan"),
        Key(KeyCode::Char('a')),
        Idle,
        Capture("03c_cut_applied"),
        Key(KeyCode::Char('u')),
        Idle,
        Capture("03d_cut_undone"),
        // Tab 2: keine Phantom-Flags, Detailbox sichtbar.
        Key(KeyCode::Char('2')),
        Idle,
        Capture("04_big_levers"),
        // Hilfe-Overlay.
        Key(KeyCode::Char('?')),
        Capture("05_help_modal"),
        Key(KeyCode::Esc),
        // Konflikt-Modal ueber libx (Filter ohne libXaw3d-Treffer).
        Key(KeyCode::Char('1')),
        Key(KeyCode::Char('/')),
        Type("v-libs/libx"),
        Key(KeyCode::Enter),
        Idle,
        Key(KeyCode::Enter),
        Capture("06_conflict_modal"),
    ]);
    driver.frames
}

/// Review-Szenario: Skript fahren, Befunde per Assertion pruefen,
///
/// Frames sichern.
#[test]
fn tui_review_scenario_screencaps() {
    for (width, height) in [(140u16, 36u16), (80u16, 24u16)] {
        let frames = run_scenario(width, height);
        let size = format!("{width}x{height}");

        // 01: Startansicht mit allen Welt-Paketen (schmal: gekuerzt).
        let start = frames
            .iter()
            .find(|(n, _)| n == "01_start")
            .unwrap()
            .1
            .clone();
        let names: &[&str] = if width >= 100 {
            &["gentoo-sources", "ffmpeg", "emacs", "firefox-bin"]
        } else {
            &["gentoo-sources", "ffmpeg", "emacs", "firefox"]
        };
        for pkg in names {
            assert!(start.contains(pkg), "{pkg} fehlt in 01_start ({size})");
        }

        // 02: Filter stellt Mitte auf ffmpeg um (keine stale Flags).
        let ffmpeg = frames
            .iter()
            .find(|(n, _)| n == "02_ffmpeg_after_filter")
            .unwrap()
            .1
            .clone();
        if width >= 100 {
            assert!(
                ffmpeg.contains("Pkg-Quell-DL"),
                "Pkg-Quell-DL-Label fehlt ({size}):\n{ffmpeg}"
            );
        } else {
            // Schmal: Kopfzeile wird abgeschnitten (kein Wrap in der Mitte);
            // Label-Pruefung laeuft ueber Footertest + Breitsicht.
            assert!(
                ffmpeg.contains("media-video/ffmpeg"),
                "Paketkopf fehlt ({size})"
            );
        }
        assert!(!ffmpeg.contains("Subgraph-DL"), "altes Label ({size})");
        assert!(
            !ffmpeg.contains("vanilla") && !ffmpeg.contains("test-rust"),
            "gentoo-sources-Flags stehen noch ({size}):\n{ffmpeg}"
        );
        assert!(ffmpeg.contains("svt-av1"), "ffmpeg-Flag fehlt ({size})");

        // 03: Geteilte Abhaengigkeiten direkt in der rechten Pane.
        let emacs = frames
            .iter()
            .find(|(n, _)| n == "03_emacs_shared_blocker")
            .unwrap()
            .1
            .clone();
        assert!(
            !emacs.contains("Voll-Ersparnis"),
            "altes Voll-Ersparnis-Rauschen ({size}):\n{emacs}"
        );
        // Geteilte Abhaengigkeiten: auf breitem Terminal voll sichtbar,
        // schmal nur angerissen (rechte Pane ist dort 24 Spalten schmal).
        if width >= 100 {
            assert!(
                emacs.contains("Geteilte Abhaengigkeiten"),
                "Geteilte Abhaengigkeiten fehlen ({size}):\n{emacs}"
            );
            assert!(emacs.contains("firefox-bin"), "Blocker fehlt ({size})");
        }
        // 03b: Cut-Analyse per Auto-Cut in Pane::Right (breit: voll sichtbar).
        let cut = frames
            .iter()
            .find(|(n, _)| n == "03b_cut_plan")
            .unwrap()
            .1
            .clone();
        if width >= 100 {
            assert!(
                cut.contains("Cut-Analyse"),
                "Cut-Plan fehlt ({size}):\n{cut}"
            );
        }
        // 02b: Sortierung `o` zeigt exklusive Kosten.
        let sorted = frames
            .iter()
            .find(|(n, _)| n == "02b_sorted")
            .unwrap()
            .1
            .clone();
        if width >= 100 {
            assert!(
                sorted.contains("pkg,"),
                "exklusive Kosten fehlen ({size}):\n{sorted}"
            );
        } else {
            // Schmal: Suffix angerissen (`media-video/ffmpeg (`).
            assert!(
                sorted.contains("ffmpeg ("),
                "Sort-Suffix fehlt ({size}):\n{sorted}"
            );
        }
        // Doku: nur auf breitem Terminal oberhalb des Falzes sichtbar.
        if width >= 100 {
            assert!(
                emacs.contains("gtk: GTK-Toolkit"),
                "Flag-Doku fehlt ({size}):\n{emacs}"
            );
        }

        // 04: Big Levers ohne Phantom-Flags, mit Detailbox.
        let levers = frames
            .iter()
            .find(|(n, _)| n == "04_big_levers")
            .unwrap()
            .1
            .clone();
        assert!(!levers.contains("openmp"), "Phantom-Flag openmp ({size})");
        assert!(
            levers.contains("Hebel-Details"),
            "Detailbox fehlt ({size}):\n{levers}"
        );
        assert!(
            levers.contains("Betroffene Pakete"),
            "Paketliste fehlt ({size})"
        );

        // 05: Hilfe-Overlay.
        let help = frames
            .iter()
            .find(|(n, _)| n == "05_help_modal")
            .unwrap()
            .1
            .clone();
        assert!(help.contains("Hilfe"), "Hilfe-Titel fehlt ({size})");
        assert!(
            help.contains("[Esc]/[?]/[q]"),
            "Schliess-Hinweis fehlt ({size})"
        );

        // 06: Konflikt-Modal.
        let conflict = frames
            .iter()
            .find(|(n, _)| n == "06_conflict_modal")
            .unwrap()
            .1
            .clone();
        assert!(
            conflict.contains("Quick-Switch"),
            "Konflikt-Modal fehlt ({size}):\n{conflict}"
        );

        if width == 140 {
            save_screencaps(
                &frames,
                &size,
                &screencap_out("target/screencaps/review_screencaps.md"),
            );
        } else {
            save_screencaps(
                &frames,
                &size,
                &PathBuf::from("target/screencaps/review_screencaps_80x24.md"),
            );
        }
    }
}

/// Optionaler HIL-Lauf auf echtem Gentoo-Snapshot (nur mit
/// `GENTOO_HIL_REPO`, sonst Skip): Start, Leer-Filter, Filter-loeschen per
/// `Esc`, Hilfe — als `hil_screencaps.md`.
#[test]
fn hil_screencaps_on_snapshot() {
    let Some(base) = std::env::var("GENTOO_HIL_REPO")
        .ok()
        .filter(|s| !s.is_empty())
    else {
        eprintln!("SKIP: GENTOO_HIL_REPO nicht gesetzt");
        return;
    };
    let base = PathBuf::from(base);
    let nested = base.join("repo");
    let repo = if nested.join("metadata").join("md5-cache").is_dir() {
        nested
    } else {
        base.clone()
    };
    let world_file = if base.join("world").is_file() {
        base.join("world")
    } else {
        base.join("var/lib/portage/world")
    };
    let text = std::fs::read_to_string(&world_file).unwrap();
    let world: Vec<String> = text
        .lines()
        .map(str::trim)
        .filter(|l| !l.is_empty() && !l.starts_with('#'))
        .map(str::to_string)
        .collect();
    assert!(!world.is_empty(), "HIL-World leer");
    let profile = Profile::test_default();
    let index = RepoIndex::load(&repo, None, &profile).unwrap();
    let state = AppState::new(&index, &profile, world).unwrap();
    let args = gentoo_dep_tui::mod_11_cli::TuiArgs {
        common: gentoo_dep_tui::mod_11_cli::CommonArgs {
            repo: repo.clone(),
            profile_dir: None,
            profile: None,
            world: world_file,
            binhost: None,
            make_conf: Some(base.join("make.conf")),
            package_use: Some(base.join("package.use")),
            portage_dir: base.join("etc/portage"),
            snapshot: None,
            json: false,
            perf_report: false,
        },
        out_use: base.join("package.use"),
    };
    let mut driver = Driver {
        state,
        args,
        width: 140,
        height: 36,
        frames: Vec::new(),
    };
    use Step::{Capture, Idle, Key, Type};
    driver.run(&[
        Capture("hil_01_start"),
        // Trefferloser Filter: leere Liste, leere Mitte (kein Crash).
        Type("/zzznomatch"),
        Key(KeyCode::Enter),
        Idle,
        Capture("hil_02_empty_filter"),
        // Esc loescht den Filter zuerst (review3 Task 1.2).
        Key(KeyCode::Esc),
        Idle,
        Capture("hil_03_filter_cleared"),
        Key(KeyCode::Char('?')),
        Capture("hil_04_help"),
    ]);
    let empty = driver.frame("hil_02_empty_filter");
    assert!(empty.contains("(leer)"), "Leer-Filter ohne Anzeige");
    // Esc hat den Filter geloeschst (Task 1.2) und die Liste kehrt zurueck.
    assert!(driver.state.filter.is_empty(), "Esc loescht Filter nicht");
    assert!(!driver.state.left.is_empty(), "Liste kehrt nicht zurueck");
    let cleared = driver.frame("hil_03_filter_cleared");
    assert!(cleared.contains("Details"), "Mitte fehlt nach Clear");
    assert!(driver.frame("hil_04_help").contains("Hilfe"));
    let out = std::env::var("GENTOO_HIL_SCREENCAP_OUT")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from("target/screencaps/hil_screencaps.md"));
    save_screencaps(&driver.frames, "140x36", &out);
}

