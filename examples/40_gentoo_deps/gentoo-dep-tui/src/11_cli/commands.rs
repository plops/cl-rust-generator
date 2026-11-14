//! 11_cli/commands — Nicht-TUI-Subcommands und plan.toml-Export.

use crate::mod_05_repo_index::RepoIndex;
use crate::mod_07_cascade::resolve_with_cascade;
use crate::mod_08_delta::{big_levers, package_deltas};
use crate::mod_09_plan_toml::Plan;
use crate::mod_10_export_verify::{Exported, VerifyRunner};

use super::args::{BlameArgs, CommonArgs, DeltasArgs, ExportArgs, InitArgs, VerifyArgs};
use super::{load_context, load_profile};

pub(super) fn cmd_index_check(args: &CommonArgs) -> anyhow::Result<()> {
    let profile = load_profile(args)?;
    let index = RepoIndex::load(&args.repo, args.binhost.as_deref(), &profile)?;
    println!("Kategorien : {}", index.categories());
    println!("Pakete     : {}", index.packages());
    println!("Cache-Dateien: {}", index.cache_files);
    println!("Ladezeit   : {} ms", index.load_ms);
    println!("Binhost-Treffer: {:.1} %", index.binhost_hit_rate() * 100.0);
    Ok(())
}

pub(super) fn cmd_graph(args: &CommonArgs) -> anyhow::Result<()> {
    let ctx = load_context(args)?;
    let mut engine = ctx.engine();
    let outcome = resolve_with_cascade(&mut engine, &ctx.world)?;
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

pub(super) fn cmd_blame(args: &BlameArgs) -> anyhow::Result<()> {
    let ctx = load_context(&args.common)?;
    let mut engine = ctx.engine();
    // Mit Kaskade aufloesen: kaskaden-induzierte Deps gehoeren zur Blame.
    let outcome = resolve_with_cascade(&mut engine, &ctx.world)?;
    let graph = outcome.graph;
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

pub(super) fn cmd_deltas(args: &DeltasArgs) -> anyhow::Result<()> {
    let ctx = load_context(&args.common)?;
    let engine = ctx.engine();
    let any_binhost = engine.index.has_any_binhost();
    for d in package_deltas(&engine, &ctx.world, &args.package)? {
        let sign = if d.delta.binpkg_bytes <= 0 { "" } else { "+" };
        let bin = if any_binhost {
            format!("{sign}{} B bin", d.delta.binpkg_bytes)
        } else {
            "n/a (ohne --binhost)".to_string()
        };
        let mut extra = String::new();
        if !d.added_packages.is_empty() || !d.removed_packages.is_empty() {
            extra.push_str(&format!(
                " (+{}/-{} Pakete",
                d.added_packages.len(),
                d.removed_packages.len()
            ));
            let mut names: Vec<&String> = d
                .added_packages
                .iter()
                .chain(d.removed_packages.iter())
                .collect();
            names.sort();
            names.dedup();
            let shown: Vec<&str> = names.iter().take(3).map(|s| s.as_str()).collect();
            extra.push_str(&format!(": {}", shown.join(", ")));
            if names.len() > 3 {
                extra.push_str(", ...");
            }
            extra.push(')');
        }
        println!(
            "  [{}] {}  ({bin} / {} B dl{}{}){}",
            if d.current { "x" } else { " " },
            d.flag,
            d.delta.download_bytes,
            extra,
            if d.cascade_note.is_some() {
                " KASKADE"
            } else {
                ""
            },
            d.conflict.map(|_| " CONFLICT").unwrap_or("")
        );
    }
    Ok(())
}

pub(super) fn cmd_big_levers(args: &CommonArgs) -> anyhow::Result<()> {
    let ctx = load_context(args)?;
    let engine = ctx.engine();
    println!(
        "{:<28} {:>8} {:>12} {:>12} {:>10}",
        "Flag", "Pakete", "Bin-Delta", "DL-Delta", "Konflikte"
    );
    for lever in big_levers(&engine, &ctx.world)? {
        println!(
            "{:<28} {:>8} {:>12} {:>12} {:>10}",
            lever.flag,
            lever.packages,
            lever.total.binpkg_bytes,
            lever.total.download_bytes,
            lever.conflicts
        );
    }
    Ok(())
}

pub(super) fn cmd_init(args: &InitArgs) -> anyhow::Result<()> {
    let plan = Plan::init_from_world(&args.world, &args.profile)?;
    plan.save(&args.plan)?;
    println!(
        "plan.toml geschrieben: {} Welt-Eintraege",
        plan.baseline.world.len()
    );
    Ok(())
}

pub(super) fn cmd_export(args: &ExportArgs) -> anyhow::Result<()> {
    let plan = Plan::load(&args.plan)?;
    let exported = export_plan(&plan, &args.out)?;
    println!(
        "Exportiert: {} world, {} package.use",
        exported.world.len(),
        exported.package_use.len()
    );
    Ok(())
}

pub(super) fn cmd_verify(args: &VerifyArgs) -> anyhow::Result<()> {
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
///
/// Schreibt menschenlesbare Top-Level-Dateien (`world`, `package.use`) und
/// zusaetzlich das Portage-Standardlayout (`var/lib/portage/world`,
/// `etc/portage/package.use`), sodass `VerifyRunner::against_configroot`
/// direkt mit `emerge --root … --config-root … @world` funktioniert.
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
    crate::mod_10_export_verify::write_export_files(out_dir, world, use_lines)
}
