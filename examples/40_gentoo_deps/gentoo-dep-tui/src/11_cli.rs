//! 11_cli — CLI-Einstieg: Argumente → Engine-Verdrahtung → Ausgabe.
//!
//! Modi: `index-check`, `graph`, `blame`, `deltas`, `big-levers`, `init`,
//! `export`, `verify`. (TUI-Start: `tui`, siehe `14_tui_runner`.)
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
    BlameArgs, Cli, Commands, CommonArgs, DeltasArgs, ExportArgs, InitArgs, TuiArgs, VerifyArgs,
};
pub use commands::export_plan;
pub use context::{LoadedContext, load_context, load_profile, load_world};

use commands::{
    cmd_big_levers, cmd_blame, cmd_deltas, cmd_export, cmd_graph, cmd_index_check, cmd_init,
    cmd_verify,
};

/// Einstieg (aus `main.rs`): parsen und dispatchen.
pub fn run() -> anyhow::Result<()> {
    let cli = Cli::parse();
    // Default-Subcommand: ohne Angabe startet die TUI (T07).
    let command = cli.command.unwrap_or_else(|| {
        Commands::Tui(TuiArgs {
            common: CommonArgs {
                repo: std::path::PathBuf::from("/var/db/repos/gentoo"),
                profile_dir: None,
                profile: None,
                world: std::path::PathBuf::from("/var/lib/portage/world"),
                binhost: None,
                make_conf: std::path::PathBuf::from("/etc/portage/make.conf"),
                package_use: std::path::PathBuf::from("/etc/portage/package.use"),
                perf_report: false,
            },
            plan: None,
        })
    });
    match command {
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
