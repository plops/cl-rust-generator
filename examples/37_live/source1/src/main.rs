//! live-relay — Raum-Relay fuer den Live-AI-Dolmetscher.
//!
//! Nur Modul-Deklarationen und oberste Verdrahtung (init, serve, join).
//! Alles Verhalten steckt in `01_config` … `04_serve`.

use std::{io::Read, sync::Arc, time::Duration};

use live_relay::{
    config::{Action, Config},
    serve,
    state::Rooms,
};

#[tokio::main]
async fn main() {
    let args: Vec<String> = std::env::args().collect();
    let (cfg, action) = match Config::from_args(&args) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("{e}");
            std::process::exit(2);
        }
    };
    match action {
        Action::Serve => serve(cfg).await,
        Action::Check { base } => check(&base, None),
        Action::Watch { base } => watch(&base),
    }
}

async fn serve(cfg: Config) {
    let rooms = Arc::new(Rooms::new(cfg.max_peers_per_room));
    let app = serve::router(rooms, &cfg.dir);
    let listener = tokio::net::TcpListener::bind(cfg.listen)
        .await
        .expect("bind");
    eprintln!("live-relay hoert auf {}", cfg.listen);
    axum::serve(listener, app).await.expect("serve");
}

/// Einmalabfrage fuer TUI-light: `/healthz` + `/api/rooms` lesen.
/// Nur `std` (kein HTTP-Client): winziges GET ueber `TcpStream`.
fn check(base: &str, only_room: Option<&str>) {
    match fetch_status(base) {
        Ok((health, rooms)) => {
            println!("health: {health}");
            let mut names: Vec<&String> = rooms.keys().collect();
            names.sort();
            if names.is_empty() {
                println!("kein raum aktiv");
            }
            for name in names {
                if only_room.is_some_and(|f| f != name) {
                    continue;
                }
                println!("raum {name}: {} peers", rooms[name]);
            }
        }
        Err(e) => {
            eprintln!("check fehlgeschlagen: {e}");
            std::process::exit(1);
        }
    }
}

fn watch(base: &str) {
    loop {
        check(base, None);
        println!("---");
        std::thread::sleep(Duration::from_secs(2));
    }
}

/// Sehr kleines HTTP-GET (http only), gibt (healthz-Body, rooms-Map).
fn fetch_status(base: &str) -> Result<(String, std::collections::HashMap<String, usize>), String> {
    let health = http_get(base, "/healthz").map_err(|e| e.to_string())?;
    let rooms_body = http_get(base, "/api/rooms").map_err(|e| e.to_string())?;
    let rooms: std::collections::HashMap<String, usize> =
        parse_rooms(&rooms_body).map_err(|e| e.to_string())?;
    Ok((health.trim().to_owned(), rooms))
}

/// Minimaler `{"raum": n, ...}`-Parser ohne serde (TUI bleibt dep-frei).
fn parse_rooms(body: &str) -> Result<std::collections::HashMap<String, usize>, &'static str> {
    let body = body.trim();
    if !body.starts_with('{') || !body.ends_with('}') {
        return Err("kein objekt");
    }
    let mut out = std::collections::HashMap::new();
    let inner = body[1..body.len() - 1].trim();
    if inner.is_empty() {
        return Ok(out);
    }
    for part in inner.split(',') {
        let (k, v) = part.split_once(':').ok_or("kein paar")?;
        let k = k.trim().trim_matches('"').to_owned();
        let v: usize = v.trim().parse().map_err(|_| "keine zahl")?;
        out.insert(k, v);
    }
    Ok(out)
}

/// Blockierendes HTTP/1.0-GET ueber `TcpStream` (nur `http://host:port`).
fn http_get(base: &str, path: &str) -> std::io::Result<String> {
    let host = base.strip_prefix("http://").unwrap_or(base);
    let mut stream = std::net::TcpStream::connect(host)?;
    let req = format!("GET {path} HTTP/1.0\r\nHost: {host}\r\nConnection: close\r\n\r\n");
    std::io::Write::write_all(&mut stream, req.as_bytes())?;
    let mut raw = String::new();
    stream.read_to_string(&mut raw)?;
    Ok(raw.rsplit("\r\n\r\n").next().unwrap_or("").to_owned())
}
