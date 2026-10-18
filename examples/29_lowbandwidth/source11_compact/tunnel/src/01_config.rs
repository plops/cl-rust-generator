//! `01_config` — Hops: Host/IP + Port + User + Key, alles konfigurierbar.
//!
//! Format: `user@host[:port]`, Default-Port 22. Keys kommen separat
//! (`--identity` je Hop), nie hartcodiert.

/// Ein Hop der SSH-Kette (Jump oder Ziel).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Hop {
    /// Host/IP (frei konfigurierbar, nie hartcodiert).
    pub host: String,
    /// SSH-Port.
    pub port: u16,
    /// Login-User.
    pub user: String,
}

/// Parst `user@host[:port]`.
pub fn parse_hop(s: &str) -> Result<Hop, String> {
    let (user, rest) = s.split_once('@').ok_or("Hop braucht user@host[:port]")?;
    if user.is_empty() {
        return Err("leerer User".into());
    }
    let (host, port) = match rest.rsplit_once(':') {
        Some((h, p)) if !h.is_empty() && !h.contains(':') => {
            let port: u16 = p.parse().map_err(|_| "ungültiger Port")?;
            (h.to_string(), port)
        }
        _ => {
            if rest.is_empty() || rest.contains(':') {
                return Err("leerer/ungültiger Host".into());
            }
            (rest.to_string(), 22)
        }
    };
    Ok(Hop {
        host,
        port,
        user: user.to_string(),
    })
}

/// Betriebs-Politik der Kette (Keepalive/Watchdog/Reconnect).
#[derive(Clone, Debug)]
pub struct TunnelPolicy {
    /// TCP-Connect-Timeout je Hop.
    pub connect_timeout: std::time::Duration,
    /// SSH-Keepalive-Intervall (ServerAliveInterval-Äquivalent).
    pub keepalive: std::time::Duration,
    /// Fehlgeschlagene Keepalives bis zum Abbruch.
    pub keepalive_max: usize,
    /// Wartezeiten zwischen Reconnect-Versuchen.
    pub retry_delays: Vec<std::time::Duration>,
    /// Deckel je Ketten-Versuch (Handshake/Auth/Kanal haben einzeln keinen
    /// Timeout — ohne Deckel hinge ein Versuch ewig und Retry griffe nie).
    pub attempt_timeout: std::time::Duration,
}

impl Default for TunnelPolicy {
    fn default() -> Self {
        Self {
            connect_timeout: std::time::Duration::from_secs(10),
            keepalive: std::time::Duration::from_secs(15),
            keepalive_max: 3,
            retry_delays: vec![
                std::time::Duration::from_millis(500),
                std::time::Duration::from_secs(1),
                std::time::Duration::from_secs(2),
                std::time::Duration::from_secs(5),
            ],
            attempt_timeout: std::time::Duration::from_secs(30),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hop_keeps_configured_address() {
        let h = parse_hop("kiel@51.107.16.14").unwrap();
        assert_eq!(h.host, "51.107.16.14");
        assert_eq!(h.port, 22);
        let h = parse_hop("kiel@127.0.0.1:2332").unwrap();
        assert_eq!((h.host.as_str(), h.port), ("127.0.0.1", 2332));
    }

    #[test]
    fn policy_defaults_match_ssh_practice() {
        let p = TunnelPolicy::default();
        assert_eq!(p.keepalive, std::time::Duration::from_secs(15));
        assert_eq!(p.keepalive_max, 3);
        assert_eq!(p.retry_delays.len(), 4);
    }

    #[test]
    fn hop_rejects_garbage() {
        assert!(parse_hop("nurhost").is_err());
        assert!(parse_hop("@h").is_err());
        assert!(parse_hop("u@:99").is_err());
        assert!(parse_hop("u@h:99999").is_err());
    }
}
