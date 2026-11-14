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
    /// Aktives Profil (relativ zu profiles, Default: `default/linux/amd64/23.0`).
    #[arg(long)]
    pub profile: Option<String>,
    /// Welt-Datei.
    #[arg(long, default_value = "/var/lib/portage/world")]
    pub world: PathBuf,
    /// Binhost-`Packages`-Datei (optional, sonst keine Binpkg-Groessen).
    ///
    /// Empfohlener Default (offizieller Gentoo-Binhost, amd64/23.0):
    /// `https://distfiles.gentoo.org/releases/amd64/binpackages/23.0/x86-64/Packages`
    /// als Datei laden, z. B. `wget -O /tmp/Packages <URL>`, und hier per
    /// `--binhost /tmp/Packages` uebergeben. `index-check` zeigt die Trefferquote.
    #[arg(long, value_name = "PACKAGES-DATEI")]
    pub binhost: Option<PathBuf>,
    /// `make.conf` (fuer globale USE/ACCEPT_KEYWORDS).
    #[arg(long, default_value = "/etc/portage/make.conf")]
    pub make_conf: PathBuf,
    /// `package.use` (Datei oder Verzeichnis).
    #[arg(long, default_value = "/etc/portage/package.use")]
    pub package_use: PathBuf,
    /// Phasen-Report der Perf-Metriken ausgeben (braucht `--features perf-metrics`).
    #[arg(long, default_value_t = false)]
    pub perf_report: bool,
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
    Graph(CommonArgs),
    /// Blame-Ketten fuer ein Paket ausgeben.
    Blame(BlameArgs),
    /// USE-Flag-Deltas eines Pakets ausgeben.
    Deltas(DeltasArgs),
    /// Globale Big Levers ausgeben.
    BigLevers(CommonArgs),
    /// `world` → `plan.toml` konvertieren.
    Init(InitArgs),
    /// `plan.toml` → Portage-Dateien exportieren.
    Export(ExportArgs),
    /// Export gegen `emerge --pretend` verifizieren.
    Verify(VerifyArgs),
    /// Interaktive TUI starten.
    Tui(TuiArgs),
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
    /// Zusaetzlich Shared-Blocker-/Voll-Ersparnis-Analyse je Flag
    /// (teuer: je Flag bis zu drei Kaskadenlaeufe, review3 Task 4.1).
    #[arg(long, default_value_t = false)]
    pub deep: bool,
}

/// `init`-Argumente.
#[derive(Debug, Clone, Parser)]
pub struct InitArgs {
    /// Welt-Datei.
    #[arg(long, default_value = "/var/lib/portage/world")]
    pub world: PathBuf,
    /// Ziel-`plan.toml`.
    #[arg(long, default_value = "plan.toml")]
    pub plan: PathBuf,
    /// Profilname (informativ).
    #[arg(long, default_value = "")]
    pub profile: String,
}

/// `export`-Argumente.
#[derive(Debug, Clone, Parser)]
pub struct ExportArgs {
    /// Quell-`plan.toml`.
    #[arg(long, default_value = "plan.toml")]
    pub plan: PathBuf,
    /// Zielverzeichnis (`world`, `package.use`).
    #[arg(long, default_value = "/tmp/gentoo-export")]
    pub out: PathBuf,
}

/// `verify`-Argumente.
#[derive(Debug, Clone, Parser)]
pub struct VerifyArgs {
    /// Configroot (exportierte Portage-Dateien als `/etc/portage`-Ersatz).
    #[arg(long, default_value = "/tmp/gentoo-export")]
    pub configroot: PathBuf,
    /// Befehlstemplate (Default: lokales `emerge -p`; HIL: ssh-Prefix).
    #[arg(long)]
    pub command: Option<String>,
}

/// `tui`-Argumente.
#[derive(Debug, Clone, Parser)]
pub struct TuiArgs {
    #[command(flatten)]
    pub common: CommonArgs,
    /// Plan-Datei (optional; ohne Plan: direkt aus Welt).
    #[arg(long)]
    pub plan: Option<PathBuf>,
}
