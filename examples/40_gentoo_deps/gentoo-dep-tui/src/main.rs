//! gentoo-dep-tui Binary: parst CLI-Argumente und dispatcht zu Modi/TUI.
//!
//! Nur Verdrahtung (Aufruf in `11_cli`), kein Verhalten.

use gentoo_dep_tui::mod_11_cli::run;

fn main() {
    if let Err(err) = run() {
        eprintln!("gentoo-dep-tui: {err:#}");
        std::process::exit(2);
    }
}
