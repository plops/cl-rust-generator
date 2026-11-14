//! 11_cli — CLI-Einstieg: Argumente → Engine-Verdrahtung → Ausgabe.
//!
//! Modi: `index-check`, `graph`, `blame`, `deltas`, `cut`, `big-levers`,
//! `simulate`, `batch`. (TUI-Start: `tui`, siehe `14_tui_runner`.)
//! Arg-Structs: `args`; Laden: `context`; Subcommands: `commands`.

use clap::Parser;

#[path = "11_cli/args.rs"]
mod args;
#[path = "11_cli/commands.rs"]
mod commands;
#[path = "11_cli/context.rs"]
mod context;
#[cfg(test)]
#[path = "11_cli/tests.rs"]
mod tests;

pub use args::{
    BigLeversArgs, BlameArgs, Cli, Commands, CommonArgs, CutArgs, DeltasArgs, GraphArgs,
    SimulateArgs, TuiArgs,
};
pub use context::{LoadedContext, load_context, load_profile, load_world};

use commands::{
    cmd_batch, cmd_big_levers, cmd_blame, cmd_cut, cmd_deltas, cmd_graph, cmd_index_check,
    cmd_simulate,
};

/// Einstieg (aus `main.rs`): parsen und dispatchen.
pub fn run() -> anyhow::Result<()> {
    let cli = Cli::parse();
    // Default-Subcommand: ohne Angabe startet die TUI.
    let command = cli.command.unwrap_or_else(|| {
        Commands::Tui(TuiArgs {
            common: CommonArgs {
                repo: std::path::PathBuf::from("/var/db/repos/gentoo"),
                profile_dir: None,
                profile: None,
                world: std::path::PathBuf::from("/var/lib/portage/world"),
                binhost: None,
                make_conf: None,
                package_use: None,
                portage_dir: std::path::PathBuf::from("/etc/portage"),
                snapshot: None,
                json: false,
                perf_report: false,
            },
            out_use: std::path::PathBuf::from("/tmp/gentoo-dep-tui.package.use"),
        })
    });
    match command {
        Commands::IndexCheck(a) => cmd_index_check(&a),
        Commands::Graph(a) => cmd_graph(&a),
        Commands::Blame(a) => cmd_blame(&a),
        Commands::Deltas(a) => cmd_deltas(&a),
        Commands::Cut(a) => cmd_cut(&a),
        Commands::BigLevers(a) => cmd_big_levers(&a),
        Commands::Simulate(a) => cmd_simulate(&a),
        Commands::Batch(a) => cmd_batch(&a),
        Commands::Tui(a) => crate::mod_14_tui_runner::run_tui(&a),
    }
}
