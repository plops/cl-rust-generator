//! 08 — Status-TUI-light: `--check` (einmal) und `--watch` (alle 2 s).
//!
//! Liest `/healthz` und `/api/rooms` eines laufenden Servers ueber ein
//! winziges HTTP/1.0-GET (nur `std`, kein HTTP-Client-Crate) und zeigt die
//! Raeume mit Sprache und Transport je Peer, z. B.:
//! `raum r1: 2 peers [zh-Hans opus, de pcm]`.
//!
//! `/api/rooms` liefert Listen von Peer-Infos (`{lang, transport}`); geparst
//! wird mit `serde_json` (ohnehin Dependency).

use std::io::{Read, Write};
use std::time::Duration;

use crate::config::Transport;
use crate::state::PeerInfo;

/// Raum-Momentaufnahme fuer die Ausgabe: Name → Peer-Infos.
type RoomList = Vec<(String, Vec<PeerInfo>)>;

/// Einmalabfrage: `/healthz` + `/api/rooms` lesen und formatiert ausgeben.
pub fn check(base: &str) {
    match fetch_status(base) {
        Ok((health, rooms)) => {
            println!("health: {health}");
            print!("{}", format_rooms(&rooms));
        }
        Err(e) => {
            eprintln!("check fehlgeschlagen: {e}");
            std::process::exit(1);
        }
    }
}

/// Endlos-Schleife fuer `--watch` (alle 2 s neu abfragen).
pub fn watch(base: &str) {
    loop {
        check(base);
        println!("---");
        std::thread::sleep(Duration::from_secs(2));
    }
}

/// Raum-Momentaufnahme (Name → Peer-Infos) als mehrzeiligen Text.
/// Leere Antwort → `kein raum aktiv`.
pub fn format_rooms(rooms: &[(String, Vec<PeerInfo>)]) -> String {
    if rooms.is_empty() {
        return "kein raum aktiv\n".to_owned();
    }
    let mut names: Vec<&(String, Vec<PeerInfo>)> = rooms.iter().collect();
    names.sort_by(|a, b| a.0.cmp(&b.0));
    let mut out = String::new();
    for (name, peers) in names {
        let parts: Vec<String> = peers
            .iter()
            .map(|p| {
                let t = match p.transport {
                    Transport::Pcm => "pcm",
                    Transport::Opus => "opus",
                };
                format!("{} {}", p.lang, t)
            })
            .collect();
        out.push_str(&format!(
            "raum {name}: {} peers [{}]\n",
            peers.len(),
            parts.join(", ")
        ));
    }
    out
}

/// `/healthz` (getrimmt) und geparste Raum-Liste holen.
fn fetch_status(base: &str) -> Result<(String, RoomList), String> {
    let health = http_get(base, "/healthz").map_err(|e| e.to_string())?;
    let rooms_body = http_get(base, "/api/rooms").map_err(|e| e.to_string())?;
    let rooms = parse_rooms(&rooms_body)?;
    Ok((health.trim().to_owned(), rooms))
}

/// `{"raum": [PeerInfo..], ..}` mit `serde_json` in Paare parsen.
fn parse_rooms(body: &str) -> Result<RoomList, String> {
    let map: std::collections::HashMap<String, Vec<PeerInfoParsed>> =
        serde_json::from_str(body.trim()).map_err(|e| format!("json: {e}"))?;
    Ok(map
        .into_iter()
        .map(|(k, v)| (k, v.into_iter().map(PeerInfoParsed::into_info).collect()))
        .collect())
}

/// Deserialisierbares Gegenstueck zu `PeerInfo` (das selbst nur `Serialize`
/// ist). `transport` kommt als `"pcm"`/`"opus"` (serde lowercase).
#[derive(serde::Deserialize)]
struct PeerInfoParsed {
    lang: String,
    transport: Transport,
}

impl PeerInfoParsed {
    fn into_info(self) -> PeerInfo {
        PeerInfo {
            lang: self.lang,
            transport: self.transport,
        }
    }
}

/// Blockierendes HTTP/1.0-GET ueber `TcpStream` (nur `http://host:port`).
fn http_get(base: &str, path: &str) -> std::io::Result<String> {
    let host = base.strip_prefix("http://").unwrap_or(base);
    let host = host.trim_end_matches('/');
    let mut stream = std::net::TcpStream::connect(host)?;
    let req = format!("GET {path} HTTP/1.0\r\nHost: {host}\r\nConnection: close\r\n\r\n");
    stream.write_all(req.as_bytes())?;
    let mut raw = String::new();
    stream.read_to_string(&mut raw)?;
    Ok(raw.rsplit("\r\n\r\n").next().unwrap_or("").to_owned())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn info(lang: &str, transport: Transport) -> PeerInfo {
        PeerInfo {
            lang: lang.to_owned(),
            transport,
        }
    }

    #[test]
    fn leer_meldet_kein_raum() {
        assert_eq!(format_rooms(&[]), "kein raum aktiv\n");
    }

    #[test]
    fn format_zeigt_sprache_und_transport_sortiert() {
        let rooms = vec![
            (
                "r1".to_owned(),
                vec![info("zh-Hans", Transport::Opus), info("de", Transport::Pcm)],
            ),
            ("a0".to_owned(), vec![info("en", Transport::Pcm)]),
        ];
        let out = format_rooms(&rooms);
        // Raeume alphabetisch: a0 vor r1.
        let idx_a0 = out.find("raum a0").unwrap();
        let idx_r1 = out.find("raum r1").unwrap();
        assert!(idx_a0 < idx_r1);
        assert!(out.contains("raum r1: 2 peers [zh-Hans opus, de pcm]"));
        assert!(out.contains("raum a0: 1 peers [en pcm]"));
    }

    #[test]
    fn parse_rooms_liest_listen() {
        let body = r#"{"r1":[{"lang":"de","transport":"opus"}]}"#;
        let rooms = parse_rooms(body).unwrap();
        assert_eq!(rooms.len(), 1);
        assert_eq!(rooms[0].0, "r1");
        assert_eq!(rooms[0].1[0].transport, Transport::Opus);
    }

    #[test]
    fn parse_rooms_leer() {
        assert!(parse_rooms("{}").unwrap().is_empty());
    }
}
