//! 11_cli/args — CLI-Argumente (clap).

use std::path::PathBuf;

use clap::{Parser, Subcommand};

#[derive(Debug, Clone, Parser)]
pub struct CommonArgs {
    /// Repo-Root (enthaelt `metadata/md5-cache`, `profiles`, Paketdirs).
    #[arg(long, default_value = "/var/db/repos/gentoo")]
    pub repo: PathBuf,
    /// Profil-Verzeichnis (Default: aus `--repo/profiles` + `make.profile`).
    #[arg(long)]
    pub profile_dir: Option<PathBuf>,
    /// Aktives Profil (relativ zu profiles).
    #[arg(long)]
    pub profile: Option<String>,
    /// Welt-Datei.
    #[arg(long, default_value = "/var/lib/portage/world")]
    pub world: PathBuf,
    /// Binhost-`Packages`-Datei (optional, sonst keine Binpkg-Groessen).
    #[arg(long, value_name = "PACKAGES-DATEI")]
    pub binhost: Option<PathBuf>,
    /// `make.conf` (fuer globale USE/ACCEPT_KEYWORDS). Explizit > Portage-Dir.
    #[arg(long)]
    pub make_conf: Option<PathBuf>,
    /// `package.use` (Datei oder Verzeichnis). Explizit > Portage-Dir.
    #[arg(long)]
    pub package_use: Option<PathBuf>,
    /// Portage-Verzeichnis (Default `/etc/portage`): liefert make.conf,
    /// package.use/mask/unmask/accept_keywords und make.profile.
    #[arg(long, default_value = "/etc/portage")]
    pub portage_dir: PathBuf,
    /// Snapshot-Root: `<PFAD>/repo`, `<PFAD>/world`, `<PFAD>/etc/portage`.
    #[arg(long)]
    pub snapshot: Option<PathBuf>,
    /// JSON-Ausgabe auf stdout (Perf-Report dann nach stderr).
    #[arg(long, default_value_t = false)]
    pub json: bool,
    /// Phasen-Report der Perf-Metriken ausgeben (braucht `--features perf-metrics`).
    #[arg(long, default_value_t = false)]
    pub perf_report: bool,
}

impl CommonArgs {
    /// Repo-Pfad unter Beruecksichtigung von `--snapshot`.
    pub fn eff_repo(&self) -> PathBuf {
        if let Some(snap) = &self.snapshot
            && snap.join("repo").is_dir()
        {
            return snap.join("repo");
        }
        self.repo.clone()
    }

    /// Welt-Pfad unter Beruecksichtigung von `--snapshot`.
    pub fn eff_world(&self) -> PathBuf {
        if let Some(snap) = &self.snapshot {
            let w = snap.join("world");
            if w.is_file() {
                return w;
            }
        }
        self.world.clone()
    }

    /// Portage-Dir unter Beruecksichtigung von `--snapshot`.
    pub fn eff_portage_dir(&self) -> PathBuf {
        if let Some(snap) = &self.snapshot {
            let p = snap.join("etc/portage");
            if p.is_dir() {
                return p;
            }
        }
        self.portage_dir.clone()
    }

    /// make.conf-Pfad (explizit > snapshot > portage-dir).
    pub fn eff_make_conf(&self) -> PathBuf {
        if let Some(p) = &self.make_conf {
            return p.clone();
        }
        if let Some(snap) = &self.snapshot {
            for cand in [snap.join("make.conf"), snap.join("etc/portage/make.conf")] {
                if cand.is_file() {
                    return cand;
                }
            }
        }
        self.eff_portage_dir().join("make.conf")
    }

    /// package.use-Pfad (explizit > snapshot > portage-dir).
    pub fn eff_package_use(&self) -> PathBuf {
        if let Some(p) = &self.package_use {
            return p.clone();
        }
        if let Some(snap) = &self.snapshot {
            for cand in [
                snap.join("package.use"),
                snap.join("etc/portage/package.use"),
            ] {
                if cand.exists() {
                    return cand;
                }
            }
        }
        self.eff_portage_dir().join("package.use")
    }
}

/// CLI-Wurzel.
#[derive(Debug, Parser)]
#[command(name = "gentoo-dep-tui", about = "ncdu fuer Gentoo-USE-Flags")]
pub struct Cli {
    #[command(subcommand)]
    pub command: Option<Commands>,
}

/// Alle Subcommands.
#[derive(Debug, Subcommand)]
pub enum Commands {
    /// Repo-Index laden und Statistik ausgeben.
    IndexCheck(CommonArgs),
    /// Runtime-Graph aufloesen und Groessen ausgeben.
    Graph(GraphArgs),
    /// Blame-Ketten fuer ein Paket ausgeben.
    Blame(BlameArgs),
    /// USE-Flag-Deltas eines Pakets ausgeben.
    Deltas(DeltasArgs),
    /// Paket-Eliminierungs-Analyse.
    Cut(CutArgs),
    /// Globale Big Levers ausgeben.
    BigLevers(BigLeversArgs),
    /// Einmalige Override-Simulation als JSON.
    Simulate(SimulateArgs),
    /// JSON-Anfragen zeilenweise von stdin (Index einmal laden).
    Batch(CommonArgs),
    /// Interaktive TUI starten.
    Tui(TuiArgs),
}

/// `graph`-Argumente.
#[derive(Debug, Clone, Parser)]
pub struct GraphArgs {
    #[command(flatten)]
    pub common: CommonArgs,
}

/// `blame`-Argumente.
#[derive(Debug, Clone, Parser)]
pub struct BlameArgs {
    #[command(flatten)]
    pub common: CommonArgs,
    /// Zielpaket (`cat/pkg`).
    pub package: String,
}

/// `deltas`-Argumente.
#[derive(Debug, Clone, Parser)]
pub struct DeltasArgs {
    #[command(flatten)]
    pub common: CommonArgs,
    /// Zielpaket (`cat/pkg`).
    pub package: String,
}

/// `cut`-Argumente.
#[derive(Debug, Clone, Parser)]
pub struct CutArgs {
    #[command(flatten)]
    pub common: CommonArgs,
    /// Zielpaket (`cat/pkg`).
    pub package: String,
}

/// `big-levers`-Argumente.
#[derive(Debug, Clone, Parser)]
pub struct BigLeversArgs {
    #[command(flatten)]
    pub common: CommonArgs,
    /// Auch wieder einschaltbare Flags liefern (sonst nur Abschalten).
    #[arg(long, default_value_t = false)]
    pub include_enable: bool,
}

/// `simulate`-Argumente.
#[derive(Debug, Clone, Parser)]
pub struct SimulateArgs {
    #[command(flatten)]
    pub common: CommonArgs,
    /// Override `--set cat/pkg=flag1,-flag2` (mehrfach angebbar).
    #[arg(long = "set")]
    pub set: Vec<String>,
    /// Welt-Paket entfernen `--drop cat/pkg` (mehrfach angebbar).
    #[arg(long)]
    pub drop: Vec<String>,
}

/// `tui`-Argumente.
#[derive(Debug, Clone, Parser)]
pub struct TuiArgs {
    #[command(flatten)]
    pub common: CommonArgs,
    /// Ziel fuer `[S]` package.use-Export.
    #[arg(long, default_value = "/tmp/gentoo-dep-tui.package.use")]
    pub out_use: PathBuf,
}
