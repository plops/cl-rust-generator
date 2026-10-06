//! Binär-Einstieg: verdrahtet nur CLI-Parsen mit Headless- oder GUI-Pfad.

use sph::params::Cli;

fn main() {
    let cli = Cli::parse_args();
    if cli.headless {
        sph::headless::run(cli);
    } else {
        sph::app::run(cli);
    }
}
