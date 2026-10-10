//! `lbw-client` — nur Verdrahtung: Konfiguration → Fenster → App-Schleife.

use clap::Parser;
use lbw_common::{HEIGHT, WIDTH};
use macroquad::window::Conf;

use lbw_client::app::run;
use lbw_client::config::Config;

fn window_conf() -> Conf {
    Conf {
        window_title: "lbw-client".into(),
        window_width: WIDTH as i32,
        window_height: HEIGHT as i32,
        window_resizable: false,
        ..Default::default()
    }
}

#[macroquad::main(window_conf)]
async fn main() {
    run(Config::parse()).await;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn window_is_fixed_720p() {
        let c = window_conf();
        assert_eq!((c.window_width, c.window_height), (1280, 720));
        assert!(!c.window_resizable);
    }
}
