//! `lbw-client` — nur Verdrahtung: Konfiguration → Fenster → App-Schleife.

use clap::Parser;
use lbw_common::SIZE;
use macroquad::window::Conf;

use lbw_client::app::run;
use lbw_client::config::Config;

fn window_conf() -> Conf {
    Conf {
        window_title: "lbw-client".into(),
        window_width: SIZE as i32,
        window_height: SIZE as i32,
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
    fn window_is_fixed_640() {
        let c = window_conf();
        assert_eq!((c.window_width, c.window_height), (640, 640));
        assert!(!c.window_resizable);
    }
}
