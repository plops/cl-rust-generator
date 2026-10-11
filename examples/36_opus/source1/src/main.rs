//! `opus-vis`-CLI: nur Verdrahtung (Exit-Codes), Logik in `opus_vis::cli`.
//!
//! Exit-Codes: 0 = ok, 1 = Aufruf-/Umgebungsfehler, 2 = Analyse-/IO-Fehler.

use std::process::ExitCode;

use opus_vis::cli;

fn main() -> ExitCode {
    match cli::run(std::env::args_os()) {
        Ok(()) => ExitCode::SUCCESS,
        Err(err) => {
            eprintln!("error: {err}");
            ExitCode::from(err.exit_code())
        }
    }
}
