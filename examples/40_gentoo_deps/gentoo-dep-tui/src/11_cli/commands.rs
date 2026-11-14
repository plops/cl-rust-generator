//! 11_cli/commands — Nicht-TUI-Subcommands (Text oder JSON).

use std::collections::HashMap;

use serde::Serialize;

use crate::mod_01_types::atom_cpn;
use crate::mod_05_repo_index::RepoIndex;
use crate::mod_07_cascade::{resolve_with_cascade, resolve_with_cascade_opts};
use crate::mod_08_delta::{
    big_levers_with_opts, cut_package, package_deltas, shared_removal_impact,
};

use super::args::{
    BigLeversArgs, BlameArgs, CommonArgs, CutArgs, DeltasArgs, GraphArgs, SimulateArgs,
};
use super::{load_context, load_profile};

/// `--perf-report` ausgeben (stdout bei Text, stderr bei `--json`).
fn maybe_perf_report(args: &CommonArgs) {
    if args.perf_report {
        let report = crate::mod_00_perf::perf_report();
        if args.json {
            eprintln!("{report}");
        } else {
            println!("{report}");
        }
    }
}

fn print_json(value: &impl Serialize) -> anyhow::Result<()> {
    println!("{}", serde_json::to_string_pretty(value)?);
    Ok(())
}

pub(super) fn cmd_index_check(args: &CommonArgs) -> anyhow::Result<()> {
    let profile = load_profile(args)?;
    let index = RepoIndex::load(&args.eff_repo(), args.binhost.as_deref(), &profile)?;
    if args.json {
        #[derive(Serialize)]
        struct Out {
            categories: usize,
            packages: usize,
            cache_files: usize,
            load_ms: u128,
            binhost_hit_rate: f64,
        }
        print_json(&Out {
            categories: index.categories(),
            packages: index.packages(),
            cache_files: index.cache_files,
            load_ms: index.load_ms,
            binhost_hit_rate: index.binhost_hit_rate(),
        })?;
    } else {
        println!("Kategorien : {}", index.categories());
        println!("Pakete     : {}", index.packages());
        println!("Cache-Dateien: {}", index.cache_files);
        println!("Ladezeit   : {} ms", index.load_ms);
        println!("Binhost-Treffer: {:.1} %", index.binhost_hit_rate() * 100.0);
    }
    maybe_perf_report(args);
    Ok(())
}

pub(super) fn cmd_graph(args: &GraphArgs) -> anyhow::Result<()> {
    let ctx = load_context(&args.common)?;
    let mut engine = ctx.engine();
    let outcome = resolve_with_cascade(&mut engine, &ctx.world)?;
    let totals = outcome.graph.totals();
    if args.common.json {
        let costs = outcome.graph.node_costs();
        #[derive(Serialize)]
        struct NodeOut {
            cpn: String,
            cpv: String,
            slot: String,
            binpkg: u64,
            download: u64,
            has_binpkg: bool,
            use_flags: Vec<String>,
            exclusive_packages: usize,
            exclusive_download_bytes: i64,
            exclusive_binpkg_bytes: i64,
        }
        #[derive(Serialize)]
        struct TotalsOut {
            packages: usize,
            binpkg_bytes: i64,
            download_bytes: i64,
            has_binhost: bool,
            missing_binhost_count: usize,
            build_extra: crate::mod_01_types::SizeMetrics,
        }
        let mut nodes: Vec<NodeOut> = outcome
            .graph
            .nodes
            .iter()
            .map(|(cpn, n)| {
                let cost = costs.get(cpn);
                let mut flags: Vec<String> = n.use_flags.iter().cloned().collect();
                flags.sort();
                NodeOut {
                    cpn: cpn.clone(),
                    cpv: n.cpv.clone(),
                    slot: n.slot.clone(),
                    binpkg: n.binpkg,
                    download: n.download,
                    has_binpkg: n.has_binpkg,
                    use_flags: flags,
                    exclusive_packages: cost.map(|c| c.exclusive_packages).unwrap_or(0),
                    exclusive_download_bytes: cost
                        .map(|c| c.exclusive_metrics.download_bytes)
                        .unwrap_or(0),
                    exclusive_binpkg_bytes: cost
                        .map(|c| c.exclusive_metrics.binpkg_bytes)
                        .unwrap_or(0),
                }
            })
            .collect();
        nodes.sort_by(|a, b| a.cpn.cmp(&b.cpn));
        #[derive(Serialize)]
        struct Out {
            totals: TotalsOut,
            nodes: Vec<NodeOut>,
            conflicts: Vec<crate::mod_01_types::ConflictInfo>,
            skipped: Vec<crate::mod_01_types::SkippedDep>,
        }
        print_json(&Out {
            totals: TotalsOut {
                packages: outcome.graph.nodes.len(),
                binpkg_bytes: totals.binpkg_bytes,
                download_bytes: totals.download_bytes,
                has_binhost: ctx.index.has_any_binhost(),
                missing_binhost_count: outcome
                    .graph
                    .nodes
                    .values()
                    .filter(|n| !n.has_binpkg)
                    .count(),
                build_extra: outcome.graph.build_extra,
            },
            nodes,
            conflicts: outcome.conflicts,
            skipped: outcome.graph.skipped,
        })?;
    } else {
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
    }
    maybe_perf_report(&args.common);
    Ok(())
}

pub(super) fn cmd_blame(args: &BlameArgs) -> anyhow::Result<()> {
    let ctx = load_context(&args.common)?;
    let mut engine = ctx.engine();
    let outcome = resolve_with_cascade(&mut engine, &ctx.world)?;
    let graph = outcome.graph;
    let chains = graph.blame(&args.package);
    if args.common.json {
        print_json(&chains)?;
    } else if chains.is_empty() {
        println!(
            "(kein Pfad — Welt-Paket oder leer) [{}]",
            args.package
        );
    } else {
        for (i, chain) in chains.iter().enumerate() {
            println!("Kette {}:", i + 1);
            for link in chain {
                match &link.via_flag {
                    Some(f) => println!("  {} -> {} [+{}]", link.required_by, link.package, f),
                    None => println!("  {} -> {}", link.required_by, link.package),
                }
            }
        }
    }
    maybe_perf_report(&args.common);
    Ok(())
}

pub(super) fn cmd_deltas(args: &DeltasArgs) -> anyhow::Result<()> {
    let ctx = load_context(&args.common)?;
    let mut engine = ctx.engine();
    let deltas = package_deltas(&engine, &ctx.world, &args.package)?;
    if args.common.json {
        #[derive(Serialize)]
        struct Out<'a> {
            package: &'a str,
            has_binhost: bool,
            flags: &'a [crate::mod_01_types::FlagDelta],
        }
        print_json(&Out {
            package: &args.package,
            has_binhost: engine.index.has_any_binhost(),
            flags: &deltas,
        })?;
    } else {
        let any_binhost = engine.index.has_any_binhost();
        for d in &deltas {
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
            let conflict = match &d.conflict {
                Some(c) => format!(" CONFLICT ({})", c.message),
                None => d.conflict.as_ref().map(|_| " CONFLICT").unwrap_or("").to_string(),
            };
            println!(
                "  [{}] {}  ({bin} / {} B dl{}{}{})",
                if d.current { "x" } else { " " },
                d.flag,
                d.delta.download_bytes,
                extra,
                if d.cascade_note.is_some() {
                    " KASKADE"
                } else {
                    ""
                },
                conflict,
            );
        }
    }
    if args.deep {
        cmd_deltas_deep(
            &mut engine,
            &ctx.world,
            &args.package,
            args.common.json,
        )?;
    }
    maybe_perf_report(&args.common);
    Ok(())
}

/// Shared-Blocker-Bericht je Flag (`deltas --deep`).
fn cmd_deltas_deep(
    engine: &mut crate::mod_06_graph::Engine<'_>,
    world: &[String],
    package: &str,
    as_json: bool,
) -> anyhow::Result<()> {
    let base = resolve_with_cascade(engine, world)?;
    if as_json {
        let mut out = Vec::new();
        for d in package_deltas(engine, world, package)? {
            if let Some(impact) = shared_removal_impact(engine, world, &base, package, &d.flag)?
                && (!impact.flag_blockers.is_empty() || !impact.world_blockers.is_empty())
            {
                out.push(impact);
            }
        }
        print_json(&out)?;
        return Ok(());
    }
    for d in package_deltas(engine, world, package)? {
        let impact = shared_removal_impact(engine, world, &base, package, &d.flag)?;
        let Some(impact) = impact else { continue };
        if impact.flag_blockers.is_empty() && impact.world_blockers.is_empty() {
            continue;
        }
        println!("  deep {} [{}]:", impact.flag, impact.package);
        println!(
            "    ohne Flag-Blocker: {} B bin / {} B dl (-{} pkg)",
            impact.without_flag_blockers.metrics.binpkg_bytes,
            impact.without_flag_blockers.metrics.download_bytes,
            impact.without_flag_blockers.removed_packages.len()
        );
        if !impact.flag_blockers.is_empty() {
            let blockers: Vec<String> = impact
                .flag_blockers
                .iter()
                .map(|b| {
                    if b.want_on {
                        format!("{} [+{}]", b.package, b.flag)
                    } else {
                        format!("{} [-{}]", b.package, b.flag)
                    }
                })
                .collect();
            println!("    Flag aus bei: {}", blockers.join(", "));
        }
        println!(
            "    inkl. world-Entfernung: {} B bin (-{} pkg)",
            impact.without_all_blockers.metrics.binpkg_bytes,
            impact.without_all_blockers.removed_packages.len()
        );
        if !impact.world_blockers.is_empty() {
            println!("    world entfernen: {}", impact.world_blockers.join(", "));
        }
    }
    Ok(())
}

pub(super) fn cmd_cut(args: &CutArgs) -> anyhow::Result<()> {
    let ctx = load_context(&args.common)?;
    let mut engine = ctx.engine();
    let base = resolve_with_cascade(&mut engine, &ctx.world)?;
    let plan = cut_package(&engine, &ctx.world, &base, &args.package)?;
    if args.common.json {
        match &plan {
            Some(p) => print_json(p)?,
            None => print_json(&serde_json::json!({"target": args.package, "absent": true}))?,
        }
    } else {
        match &plan {
            None => println!("{} nicht im Graphen", args.package),
            Some(p) => {
                println!("Cut-Analyse fuer {}:", p.target);
                if p.flag_blockers.is_empty() {
                    println!("  keine Flag-Blocker");
                } else {
                    println!("  Flag-Blocker:");
                    for b in &p.flag_blockers {
                        println!(
                            "    {} [{}]",
                            b.package,
                            if b.want_on {
                                format!("+{}", b.flag)
                            } else {
                                format!("-{}", b.flag)
                            }
                        );
                    }
                }
                if p.world_blockers.is_empty() {
                    println!("  keine harten world-Blocker");
                } else {
                    println!("  Harte world-Blocker: {}", p.world_blockers.join(", "));
                }
                println!(
                    "  Ohne Flag-Blocker: {} bin / {} dl (-{} pkg)",
                    p.without_flag_blockers.metrics.binpkg_bytes,
                    p.without_flag_blockers.metrics.download_bytes,
                    p.without_flag_blockers.removed_packages.len()
                );
                println!(
                    "  Ohne alle Blocker: {} bin / {} dl (-{} pkg)",
                    p.without_all_blockers.metrics.binpkg_bytes,
                    p.without_all_blockers.metrics.download_bytes,
                    p.without_all_blockers.removed_packages.len()
                );
            }
        }
    }
    maybe_perf_report(&args.common);
    Ok(())
}

pub(super) fn cmd_big_levers(args: &BigLeversArgs) -> anyhow::Result<()> {
    let ctx = load_context(&args.common)?;
    let engine = ctx.engine();
    let base = resolve_with_cascade_opts(&mut engine.fork(), &ctx.world, false, false)?;
    let levers = big_levers_with_opts(&engine, &ctx.world, &base, args.include_enable)?;
    if args.common.json {
        print_json(&levers)?;
    } else {
        println!(
            "{:<28} {:>4} {:>8} {:>9} {:>12} {:>12} {:>10}",
            "Flag", "An?", "Pakete", "Pkg-Delta", "Bin-Delta", "DL-Delta", "Konflikte"
        );
        for lever in &levers {
            println!(
                "{:<28} {:>4} {:>8} {:>+9} {:>12} {:>12} {:>10}",
                lever.flag,
                if lever.turn_on { "[AN]" } else { "[AUS]" },
                lever.packages,
                lever.net_pkg_delta,
                lever.total.binpkg_bytes,
                lever.total.download_bytes,
                lever.conflicts
            );
        }
    }
    maybe_perf_report(&args.common);
    Ok(())
}

/// `--set cat/pkg=flag1,-flag2` parsen.
fn parse_set_arg(arg: &str) -> anyhow::Result<(String, HashMap<String, bool>)> {
    let (cpn, flags) = arg.split_once('=').ok_or_else(|| {
        anyhow::anyhow!("--set erwartet FORM `cat/pkg=flag1,-flag2`, erhalten: {arg}")
    })?;
    let mut map = HashMap::new();
    for token in flags.split(',') {
        let token = token.trim();
        if token.is_empty() {
            continue;
        }
        if let Some(flag) = token.strip_prefix('-') {
            map.insert(flag.to_string(), false);
        } else if let Some(flag) = token.strip_prefix('+') {
            map.insert(flag.to_string(), true);
        } else {
            map.insert(token.to_string(), true);
        }
    }
    Ok((atom_cpn(cpn.trim()), map))
}

pub(super) fn cmd_simulate(args: &SimulateArgs) -> anyhow::Result<()> {
    let ctx = load_context(&args.common)?;
    // Baseline VOR den Overrides (sonst waere das Diff immer 0).
    let before = {
        let mut clean = ctx.engine();
        resolve_with_cascade_opts(&mut clean, &ctx.world, false, false)?
    };
    let mut engine = ctx.engine();
    for set in &args.set {
        let (cpn, map) = parse_set_arg(set)?;
        engine.overrides.entry(cpn).or_default().extend(map);
    }
    let mut world = ctx.world.clone();
    if !args.drop.is_empty() {
        let drop_cpns: Vec<String> = args.drop.iter().map(|d| atom_cpn(d)).collect();
        world.retain(|w| !drop_cpns.contains(&atom_cpn(w)));
    }
    let outcome = resolve_with_cascade(&mut engine, &world)?;
    let before_totals = before.graph.totals();
    let after_totals = outcome.graph.totals();
    let mut added: Vec<String> = outcome
        .graph
        .nodes
        .keys()
        .filter(|k| !before.graph.nodes.contains_key(*k))
        .cloned()
        .collect();
    let mut removed: Vec<String> = before
        .graph
        .nodes
        .keys()
        .filter(|k| !outcome.graph.nodes.contains_key(*k))
        .cloned()
        .collect();
    added.sort();
    removed.sort();
    #[derive(Serialize)]
    struct Out {
        totals: crate::mod_01_types::SizeMetrics,
        diff_binpkg: i64,
        diff_download: i64,
        added_packages: Vec<String>,
        removed_packages: Vec<String>,
        conflicts: Vec<crate::mod_01_types::ConflictInfo>,
    }
    let out = Out {
        totals: after_totals,
        diff_binpkg: after_totals.binpkg_bytes - before_totals.binpkg_bytes,
        diff_download: after_totals.download_bytes - before_totals.download_bytes,
        added_packages: added,
        removed_packages: removed,
        conflicts: outcome.conflicts,
    };
    if args.common.json {
        print_json(&out)?;
    } else {
        println!("Pakete     : {}", outcome.graph.nodes.len());
        println!("Binpkg-Delta : {} B", out.diff_binpkg);
        println!("DL-Delta     : {} B", out.diff_download);
        println!("Hinzu        : {}", out.added_packages.join(", "));
        println!("Entfernt     : {}", out.removed_packages.join(", "));
        println!("Konflikte    : {}", out.conflicts.len());
        for c in &out.conflicts {
            println!("CONFLICT {}: {}", c.package, c.message);
        }
    }
    maybe_perf_report(&args.common);
    Ok(())
}

pub(super) fn cmd_batch(args: &CommonArgs) -> anyhow::Result<()> {
    use std::io::BufRead;
    let ctx = load_context(args)?;
    let stdin = std::io::stdin();
    let mut stdout = std::io::stdout();
    for line in stdin.lock().lines() {
        let line = line?;
        if line.trim().is_empty() {
            continue;
        }
        let response = handle_batch_line(&ctx, &line);
        use std::io::Write;
        stdout.write_all(response.as_bytes())?;
        stdout.write_all(b"\n")?;
        stdout.flush()?;
    }
    Ok(())
}

fn handle_batch_line(ctx: &super::LoadedContext, line: &str) -> String {
    #[derive(serde::Deserialize)]
    #[serde(tag = "op", rename_all = "snake_case")]
    enum Req {
        Graph,
        Deltas { package: String },
        Cut { package: String },
        BigLevers { #[serde(default)] include_enable: bool },
        Simulate {
            #[serde(default)]
            set: HashMap<String, Vec<String>>,
            #[serde(default)]
            drop: Vec<String>,
        },
    }
    let req: Result<Req, _> = serde_json::from_str(line);
    let req = match req {
        Ok(r) => r,
        Err(e) => return serde_json::json!({"error": e.to_string()}).to_string(),
    };
    let mut engine = ctx.engine();
    match req {
        Req::Graph => match resolve_with_cascade(&mut engine, &ctx.world) {
            Ok(outcome) => {
                let totals = outcome.graph.totals();
                serde_json::json!({
                    "packages": outcome.graph.nodes.len(),
                    "binpkg_bytes": totals.binpkg_bytes,
                    "download_bytes": totals.download_bytes,
                    "conflicts": outcome.conflicts.len(),
                })
                .to_string()
            }
            Err(e) => serde_json::json!({"error": format!("{e:#}")}).to_string(),
        },
        Req::Deltas { package } => match package_deltas(&engine, &ctx.world, &package) {
            Ok(flags) => serde_json::json!({"package": package, "flags": flags}).to_string(),
            Err(e) => serde_json::json!({"error": format!("{e:#}")}).to_string(),
        },
        Req::Cut { package } => {
            match resolve_with_cascade(&mut engine, &ctx.world)
                .map_err(|e| anyhow::anyhow!("{e:#}"))
                .and_then(|base| {
                    cut_package(&engine, &ctx.world, &base, &package)
                        .map_err(|e| anyhow::anyhow!("{e:#}"))
                }) {
                Ok(plan) => serde_json::json!(plan).to_string(),
                Err(e) => serde_json::json!({"error": format!("{e:#}")}).to_string(),
            }
        }
        Req::BigLevers { include_enable } => {
            match resolve_with_cascade_opts(&mut engine.fork(), &ctx.world, false, false)
                .map_err(|e| anyhow::anyhow!("{e:#}"))
                .and_then(|base| {
                    big_levers_with_opts(&engine, &ctx.world, &base, include_enable)
                        .map_err(|e| anyhow::anyhow!("{e:#}"))
                }) {
                Ok(levers) => serde_json::json!(levers).to_string(),
                Err(e) => serde_json::json!({"error": format!("{e:#}")}).to_string(),
            }
        }
        Req::Simulate { set, drop } => {
            for (cpn, flags) in &set {
                let entry = engine.overrides.entry(atom_cpn(cpn)).or_default();
                for token in flags {
                    if let Some(flag) = token.strip_prefix('-') {
                        entry.insert(flag.to_string(), false);
                    } else if let Some(flag) = token.strip_prefix('+') {
                        entry.insert(flag.to_string(), true);
                    } else {
                        entry.insert(token.clone(), true);
                    }
                }
            }
            let mut world = ctx.world.clone();
            if !drop.is_empty() {
                let drop_cpns: Vec<String> = drop.iter().map(|d| atom_cpn(d)).collect();
                world.retain(|w| !drop_cpns.contains(&atom_cpn(w)));
            }
            match resolve_with_cascade(&mut engine, &world) {
                Ok(outcome) => {
                    let totals = outcome.graph.totals();
                    serde_json::json!({
                        "packages": outcome.graph.nodes.len(),
                        "binpkg_bytes": totals.binpkg_bytes,
                        "download_bytes": totals.download_bytes,
                        "conflicts": outcome.conflicts,
                    })
                    .to_string()
                }
                Err(e) => serde_json::json!({"error": format!("{e:#}")}).to_string(),
            }
        }
    }
}
