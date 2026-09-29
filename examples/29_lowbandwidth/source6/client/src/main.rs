//! `lbw-client` — nur Verdrahtung: Konfiguration → Fenster → App-Schleife.

use lbw_client::app::run;
use lbw_client::config::Config;
use macroquad::window::Conf;

fn config() -> Config {
    Config::parse(std::env::args().skip(1)).unwrap_or_else(|msg| {
        eprintln!("{msg}");
        std::process::exit(2)
    })
}

fn window_conf() -> Conf {
    let c = config();
    Conf {
        window_title: "lbw-client".into(),
        window_width: c.size as i32,
        window_height: c.size as i32,
        window_resizable: false,
        ..Default::default()
    }
}

#[macroquad::main(window_conf)]
async fn main() {
    run(config()).await;
}
