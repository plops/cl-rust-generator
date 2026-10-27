//! live-silk — Server-Einstieg (nur Verdrahtung, wird in T6 vervollstaendigt).

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if let Err(e) = live_silk::config::Config::from_args(&args) {
        eprintln!("{e}");
        std::process::exit(2);
    }
}
