//! `lbw-client` — nur Verdrahtung: Konfiguration → Tunnel → Fenster.
//!
//! Ohne `--via`: direkt an `connect`. Mit `--via`: russh-Kette über
//! `--jump`-Hops zum Ziel (Plan §2.1), lokaler Forward als Tunnel-Ende.
//! Der Tunnel-Thread (eigene Tokio-Runtime) lebt so lange wie der Client.

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
    let cfg = Config::parse();
    match start_tunnel(&cfg) {
        Ok(addr) => {
            run(Config {
                connect: addr,
                ..cfg
            })
            .await
        }
        Err(e) => eprintln!("lbw-client: Tunnel: {e}"),
    }
}

/// Baut ggf. die SSH-Kette und liefert das lokale Tunnel-Ende
/// (`127.0.0.1:<port>`, wie `ssh -L`).
fn start_tunnel(cfg: &Config) -> Result<String, String> {
    let via = match &cfg.via {
        None => return Ok(cfg.connect.clone()),
        Some(v) => v.clone(),
    };
    let mut hops = Vec::new();
    for (i, j) in cfg.jump.iter().enumerate() {
        hops.push((
            lbw_tunnel::parse_hop(j).map_err(|e| format!("--jump {j}: {e}"))?,
            cfg.jump_identity
                .as_deref()
                .ok_or("--jump-identity fehlt")?,
            cfg.pin.get(i).cloned(),
        ));
    }
    let target = lbw_tunnel::parse_hop(&via).map_err(|e| format!("--via {via}: {e}"))?;
    hops.push((
        target,
        cfg.via_identity.as_deref().ok_or("--via-identity fehlt")?,
        cfg.pin.get(hops.len()).cloned(),
    ));
    let target_sshd = if cfg.jump.is_empty() {
        ("127.0.0.1".to_string(), 22)
    } else {
        let t = cfg
            .target_sshd
            .as_deref()
            .ok_or("--target-sshd fehlt (Jump aktiv)")?;
        let (h, p) = t
            .rsplit_once(':')
            .ok_or("--target-sshd braucht host:port")?;
        (
            h.to_string(),
            p.parse().map_err(|_| "--target-sshd: ungültiger Port")?,
        )
    };
    let lbw_port = cfg.lbw_port;
    let tap_port = cfg.tap.unwrap_or(0);
    // Alles Owned in den Thread (keine Borrow aus `cfg`).
    let hops: Vec<(lbw_tunnel::Hop, String, Option<String>)> = hops
        .into_iter()
        .map(|(hop, idfile, pin)| (hop, idfile.to_string(), pin))
        .collect();
    let (tx, rx) = std::sync::mpsc::channel::<Result<String, String>>();
    // Eigener Thread + Runtime: lebt bis Prozessende, wie ein `ssh -L`.
    std::thread::spawn(move || {
        let rt = match tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
        {
            Ok(rt) => rt,
            Err(e) => {
                let _ = tx.send(Err(e.to_string()));
                return;
            }
        };
        rt.block_on(async {
            let pinned: Vec<lbw_tunnel::PinnedHop> = match hops
                .into_iter()
                .map(|(hop, idfile, pin)| {
                    let identity_openssh =
                        std::fs::read_to_string(&idfile).map_err(|e| format!("{idfile}: {e}"))?;
                    Ok(lbw_tunnel::PinnedHop {
                        hop,
                        identity_openssh,
                        passphrase: None,
                        pin,
                    })
                })
                .collect::<Result<_, String>>()
            {
                Ok(p) => p,
                Err(e) => {
                    let _ = tx.send(Err(e));
                    return;
                }
            };
            let policy = lbw_tunnel::TunnelPolicy::default();
            let mut chain = match lbw_tunnel::connect_chain_retry(
                &policy,
                &pinned,
                (&target_sshd.0, target_sshd.1),
                lbw_port,
            )
            .await
            {
                Ok(c) => c,
                Err(e) => {
                    let _ = tx.send(Err(e));
                    return;
                }
            };
            // Fester Tap-Port (Wireshark sieht den Klartext auf `lo`),
            // sonst ephemer.
            let listen = match tokio::net::TcpListener::bind(("127.0.0.1", tap_port)).await {
                Ok(l) => l,
                Err(e) => {
                    let _ = tx.send(Err(e.to_string()));
                    return;
                }
            };
            let addr = match listen.local_addr() {
                Ok(a) => a.to_string(),
                Err(e) => {
                    let _ = tx.send(Err(e.to_string()));
                    return;
                }
            };
            let _ = tx.send(Ok(addr));
            // Eine LBW-Session nach der anderen über dieselbe Kette.
            loop {
                let Ok((mut sock, _)) = listen.accept().await else {
                    return;
                };
                let _ = tokio::io::copy_bidirectional(&mut chain.stream, &mut sock).await;
            }
        });
    });
    rx.recv().map_err(|_| "Tunnel-Thread weg".to_string())?
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

    #[test]
    fn no_via_means_direct_connect() {
        let cfg = Config::try_parse_from(["lbw-client"]).unwrap();
        assert_eq!(start_tunnel(&cfg).unwrap(), "127.0.0.1:7878");
    }
}
