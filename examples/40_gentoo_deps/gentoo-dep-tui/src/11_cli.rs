//! 11_cli — CLI-Definition (clap) und Ausfuehrung aller Nicht-TUI-Modi.
//!
//! Genau eine Zustaendigkeit: Argumente → Engine-Verdrahtung → Ausgabe.
//! Modi: `index-check`, `graph`, `blame`, `deltas`, `big-levers`, `init`,
//! `export`, `verify`. (TUI-Start: `tui`, siehe `14_tui_runner`.)

use std::path::PathBuf;

use anyhow::Context;
use clap::{Parser, Subcommand};

use crate::mod_02_profile::{self, Profile};
use crate::mod_05_repo_index::RepoIndex;
use crate::mod_06_graph::{Engine, resolve};
use crate::mod_07_cascade::resolve_with_cascade;
use crate::mod_08_delta::{big_levers, package_deltas};
use crate::mod_09_plan_toml::Plan;
use crate::mod_10_export_verify::{Exported, VerifyRunner};

/// Gemeinsame Pfad-Argumente (Repo, Profile, Welt, Binhost).
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
    #[arg(long)]
    pub binhost: Option<PathBuf>,
    /// `make.conf` (fuer globale USE/ACCEPT_KEYWORDS).
    #[arg(long, default_value = "/etc/portage/make.conf")]
    pub make_conf: PathBuf,
    /// `package.use` (Datei oder Verzeichnis).
    #[arg(long, default_value = "/etc/portage/package.use")]
    pub package_use: PathBuf,
}

/// CLI-Wurzel.
#[derive(Debug, Parser)]
#[command(name = "gentoo-dep-tui", about = "ncdu fuer Gentoo-USE-Flags")]
pub struct Cli {
    #[command(subcommand)]
    pub command: Commands,
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

/// Einstieg (aus `main.rs`): parsen und dispatchen.
pub fn run() -> anyhow::Result<()> {
    let cli = Cli::parse();
    match cli.command {
        Commands::IndexCheck(a) => cmd_index_check(&a),
        Commands::Graph(a) => cmd_graph(&a),
        Commands::Blame(a) => cmd_blame(&a),
        Commands::Deltas(a) => cmd_deltas(&a),
        Commands::BigLevers(a) => cmd_big_levers(&a),
        Commands::Init(a) => cmd_init(&a),
        Commands::Export(a) => cmd_export(&a),
        Commands::Verify(a) => cmd_verify(&a),
        Commands::Tui(a) => crate::mod_14_tui_runner::run_tui(&a),
    }
}

/// Profil aus Common-Argumenten laden (mit make.conf-Anreicherung).
pub fn load_profile(args: &CommonArgs) -> anyhow::Result<Profile> {
    let profiles_dir = args
        .profile_dir
        .clone()
        .unwrap_or_else(|| args.repo.join("profiles"));
    let profile_name = args
        .profile
        .clone()
        .unwrap_or_else(|| "default/linux/amd64/23.0".to_string());
    let mut profile =
        mod_02_profile::load_profile_stack(&profiles_dir.join(&profile_name), &profiles_dir)
            .with_context(|| "Profilstapel laden")?;
    profile.profile_name = profile_name;
    if profile.arch.is_empty() {
        profile.arch = "amd64".into();
    }
    if profile.accept_keywords.is_empty() {
        profile.accept_keywords.insert(profile.arch.clone());
    }
    // make.conf: USE + ACCEPT_KEYWORDS einmischen.
    if let Ok(kv) = mod_02_profile::read_key_value(&args.make_conf) {
        if let Some(use_line) = kv.get("USE") {
            profile.global_use = mod_02_profile::parse_use_tokens(use_line);
        }
        if let Some(keywords) = kv.get("ACCEPT_KEYWORDS") {
            profile
                .accept_keywords
                .extend(mod_02_profile::parse_use_tokens(keywords));
        }
        if let Some(arch) = kv.get("ARCH") {
            profile.arch = arch.clone();
        }
        // USE_EXPAND-Werte aus make.conf haben Vorrang vor Profil-Defaults.
        let expand_vars: Vec<String> = profile.expand_defaults.keys().cloned().collect();
        for var in expand_vars {
            if let Some(line) = kv.get(&var) {
                profile
                    .expand_defaults
                    .insert(var, line.split_whitespace().map(str::to_string).collect());
            }
        }
    }
    // use.mask/use.force/use zu separaten Dateien? Profilstapel kennt nur
    // package.mask — Mask-Flags aus Profil-Verzeichnissen nachladen.
    let mask =
        mod_02_profile::read_flag_list(&profiles_dir.join(&profile.profile_name).join("use.mask"))
            .unwrap_or_default();
    profile.use_mask.extend(mask);
    let force =
        mod_02_profile::read_flag_list(&profiles_dir.join(&profile.profile_name).join("use.force"))
            .unwrap_or_default();
    profile.use_force.extend(force);
    profile.package_use = mod_02_profile::read_package_use(&args.package_use).unwrap_or_default();
    Ok(profile)
}

/// Welt lesen (leere Welt ist erlaubt, ergibt leeren Graphen).
pub fn load_world(args: &CommonArgs) -> anyhow::Result<Vec<String>> {
    Ok(mod_02_profile::read_world(&args.world).unwrap_or_default())
}

/// Engine aus Common-Argumenten aufbauen.
pub fn load_engine(
    args: &CommonArgs,
) -> anyhow::Result<(Engine<'static>, Vec<String>, ProfileLease)> {
    let profile = load_profile(args)?;
    let world = load_world(args)?;
    let index = Box::leak(Box::new(
        RepoIndex::load(&args.repo, args.binhost.as_deref(), &profile)
            .with_context(|| "Repo-Index laden")?,
    ));
    let profile = Box::leak(Box::new(profile));
    let engine = Engine::new(index, profile);
    Ok((engine, world, ProfileLease))
}

/// Halter (stellt sicher, dass geleakte Boxen dokumentiert sind).
pub struct ProfileLease;

fn cmd_index_check(args: &CommonArgs) -> anyhow::Result<()> {
    let profile = load_profile(args)?;
    let index = RepoIndex::load(&args.repo, args.binhost.as_deref(), &profile)?;
    println!("Kategorien : {}", index.categories());
    println!("Pakete     : {}", index.packages());
    println!("Cache-Dateien: {}", index.cache_files);
    println!("Ladezeit   : {} ms", index.load_ms);
    println!("Binhost-Treffer: {:.1} %", index.binhost_hit_rate() * 100.0);
    Ok(())
}

fn cmd_graph(args: &CommonArgs) -> anyhow::Result<()> {
    let (mut engine, world, _lease) = load_engine(args)?;
    let outcome = resolve_with_cascade(&mut engine, &world)?;
    let totals = outcome.graph.totals();
    println!("Pakete     : {}", outcome.graph.nodes.len());
    println!("Binpkg     : {} MB", totals.binpkg_bytes / 1_048_576);
    println!("Download   : {} MB", totals.download_bytes / 1_048_576);
    println!(
        "BDEPEND-extra: {} MB",
        outcome.graph.build_extra.binpkg_bytes / 1_048_576
    );
    println!("Konflikte  : {}", outcome.conflicts.len());
    println!("Uebersprungen: {}", outcome.graph.skipped.len());
    for s in outcome.graph.skipped.iter().take(10) {
        println!("SKIP {} (von {}): {}", s.atom, s.required_by, s.reason);
    }
    for c in &outcome.conflicts {
        println!("CONFLICT {}: {}", c.package, c.message);
    }
    let mut cpvs: Vec<String> = outcome
        .graph
        .nodes
        .values()
        .map(|n| n.cpv.clone())
        .collect();
    cpvs.sort();
    for cpv in cpvs {
        println!("  {cpv}");
    }
    Ok(())
}

fn cmd_blame(args: &BlameArgs) -> anyhow::Result<()> {
    let (engine, world, _lease) = load_engine(&args.common)?;
    let graph = resolve(&engine, &world)?;
    let chains = graph.blame(&args.package);
    if chains.is_empty() {
        println!("Keine Kette fuer {} (nicht im Graphen?)", args.package);
        return Ok(());
    }
    for (i, chain) in chains.iter().enumerate() {
        println!("Kette {}:", i + 1);
        for link in chain {
            match &link.via_flag {
                Some(f) => println!("  {} -> {} [+{}]", link.required_by, link.package, f),
                None => println!("  {} -> {}", link.required_by, link.package),
            }
        }
    }
    Ok(())
}

fn cmd_deltas(args: &DeltasArgs) -> anyhow::Result<()> {
    let (engine, world, _lease) = load_engine(&args.common)?;
    for d in package_deltas(&engine, &world, &args.package)? {
        let sign = if d.delta.binpkg_bytes <= 0 { "" } else { "+" };
        println!(
            "  [{}] {}  ({}{} B bin / {} B dl){}",
            if d.current { "x" } else { " " },
            d.flag,
            sign,
            d.delta.binpkg_bytes,
            d.delta.download_bytes,
            d.conflict.map(|_| " CONFLICT").unwrap_or("")
        );
    }
    Ok(())
}

fn cmd_big_levers(args: &CommonArgs) -> anyhow::Result<()> {
    let (engine, world, _lease) = load_engine(args)?;
    println!(
        "{:<28} {:>8} {:>12} {:>10}",
        "Flag", "Pakete", "Bin-Delta", "Konflikte"
    );
    for lever in big_levers(&engine, &world)? {
        println!(
            "{:<28} {:>8} {:>12} {:>10}",
            lever.flag, lever.packages, lever.total.binpkg_bytes, lever.conflicts
        );
    }
    Ok(())
}

fn cmd_init(args: &InitArgs) -> anyhow::Result<()> {
    let plan = Plan::init_from_world(&args.world, &args.profile)?;
    plan.save(&args.plan)?;
    println!(
        "plan.toml geschrieben: {} Welt-Eintraege",
        plan.baseline.world.len()
    );
    Ok(())
}

fn cmd_export(args: &ExportArgs) -> anyhow::Result<()> {
    let plan = Plan::load(&args.plan)?;
    let exported = export_plan(&plan, &args.out)?;
    println!(
        "Exportiert: {} world, {} package.use",
        exported.world.len(),
        exported.package_use.len()
    );
    Ok(())
}

fn cmd_verify(args: &VerifyArgs) -> anyhow::Result<()> {
    let runner = match &args.command {
        Some(cmd) => VerifyRunner {
            command: cmd.clone(),
            workdir: args.configroot.clone(),
        },
        None => VerifyRunner::against_configroot(&args.configroot),
    };
    let outcome = runner.run()?;
    println!("Erfolg: {}", outcome.success);
    println!("Pakete: {}", outcome.packages.len());
    for p in &outcome.problems {
        println!("PROBLEM: {p}");
    }
    Ok(())
}

/// `plan.toml` → Portage-Dateien (Tokens wortwoertlich, `-flag` bleibt).
pub fn export_plan(plan: &Plan, out_dir: &std::path::Path) -> anyhow::Result<Exported> {
    let world = plan.effective_world();
    let mut use_lines: Vec<String> = Vec::new();
    for pkg in &plan.package {
        if !pkg.use_flags.is_empty() {
            use_lines.push(format!("{} {}", pkg.atom, pkg.use_flags.join(" ")));
        }
    }
    for choice in &plan.choice {
        if let Some(opt) = choice.option.get(choice.selected)
            && !opt.suggested_use.is_empty()
            && !plan.package.iter().any(|p| p.atom == opt.atom)
        {
            use_lines.push(format!("{} {}", opt.atom, opt.suggested_use.join(" ")));
        }
    }
    std::fs::create_dir_all(out_dir)?;
    std::fs::write(out_dir.join("world"), world.join("\n") + "\n")?;
    std::fs::write(out_dir.join("package.use"), use_lines.join("\n") + "\n")?;
    Ok(Exported {
        world,
        package_use: use_lines,
        accept_keywords: Vec::new(),
    })
}
