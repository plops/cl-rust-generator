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
    window_conf_for(&config())
}

fn window_conf_for(c: &Config) -> Conf {
    Conf {
        window_title: "lbw-client".into(),
        window_width: (c.size * c.scale()) as i32,
        window_height: (c.size * c.scale()) as i32,
        window_resizable: false,
        ..Default::default()
    }
}

#[macroquad::main(window_conf)]
async fn main() {
    run(config()).await;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn window_dimensions_follow_zoom() {
        let mut c = Config::parse(Vec::new()).unwrap();
        let zoomed = window_conf_for(&c);
        assert_eq!((zoomed.window_width, zoomed.window_height), (1280, 1280));
        c.zoom = false;
        let normal = window_conf_for(&c);
        assert_eq!((normal.window_width, normal.window_height), (640, 640));
    }
}
