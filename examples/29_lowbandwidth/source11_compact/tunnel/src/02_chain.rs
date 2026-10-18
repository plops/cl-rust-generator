//! `02_chain` — Aufbau der SSH-Kette mit TOFU-Hostkey-Prüfung pro Hop.
//!
//! Ablauf pro Hop: TCP → `russh`-Handshake → Hostkey gegen erwarteten
//! `SHA256:`-Fingerprint (Abbruch **vor** Auth bei Abweichung) →
//! Publickey-Auth (Ed25519 u. a. im OpenSSH-Format). Jump: danach
//! `direct-tcpip`-Channel zum Ziel-sshd, darüber genestete Session — der
//! Jump sieht nur SSH-Chiffrat. Ergebnis ist ein Strom zum LBW-Port.

use std::future::Future;
use std::sync::{Arc, Mutex};

use russh::client::{self, Handle};
use russh::keys::{PublicKeyBase64, PublicKeyOrCertificate};
use ssh_key::{Fingerprint, HashAlg, PublicKey};
use tokio::io::{AsyncRead, AsyncReadExt, AsyncWrite, AsyncWriteExt};

use crate::config::{Hop, TunnelPolicy};

/// Erwarteter Hostkey eines Hops (TOFU-Pin, `SHA256:…` wie `ssh-keygen -lf`).
#[derive(Clone, Debug)]
pub struct PinnedHop {
    pub hop: Hop,
    /// OpenSSH-Private-Key des Hops (Text, inkl. Ed25519).
    pub identity_openssh: String,
    /// Passphrase für verschlüsselte Keys (None = unverschlüsselt).
    pub passphrase: Option<String>,
    /// Erwarteter Hostkey-Fingerprint (None = beim 1. Connect lernen).
    pub pin: Option<String>,
}

/// Offene Kette bis zum LBW-Port (Handles halten die Sessions am Leben).
pub struct ChainStream {
    /// Strom zum LBW-Port durch alle Hops.
    pub stream: russh::ChannelStream<client::Msg>,
    #[allow(dead_code)]
    handles: Vec<Handle<Client>>,
}

pub struct Client {
    /// Erwarteter Fingerprint (None = lernen, nicht prüfen).
    pin: Option<String>,
    /// Gelernte Fingerprints (TOFU).
    learned: Arc<Mutex<Vec<String>>>,
}

impl client::Handler for Client {
    type Error = russh::Error;

    fn check_server_key(
        &mut self,
        server_public_key: &PublicKeyOrCertificate,
    ) -> impl Future<Output = Result<bool, Self::Error>> + Send {
        let pin = self.pin.clone();
        let learned = self.learned.clone();
        let fp = server_fingerprint(server_public_key);
        async move {
            let fp = fp?;
            match pin {
                Some(want) => Ok(fp == want),
                None => {
                    learned.lock().unwrap().push(fp);
                    Ok(true)
                }
            }
        }
    }
}

fn server_fingerprint(k: &PublicKeyOrCertificate) -> Result<String, russh::Error> {
    let blob = match k {
        PublicKeyOrCertificate::PublicKey { key, .. } => key.public_key_bytes(),
        PublicKeyOrCertificate::Certificate(_) => return Err(russh::Error::CouldNotReadKey),
    };
    let key = PublicKey::from_bytes(&blob).map_err(|_| russh::Error::CouldNotReadKey)?;
    Ok(sha256_fingerprint(key.key_data()))
}

/// `SHA256:…`-Fingerprint wie `ssh-keygen -lf`.
#[must_use]
pub fn sha256_fingerprint(key: &ssh_key::public::KeyData) -> String {
    Fingerprint::new(HashAlg::Sha256, key).to_string()
}

fn ssh_config(policy: &TunnelPolicy) -> Arc<client::Config> {
    Arc::new(client::Config {
        // Kleine Eingaben warten nie auf volle Segmente (Latenz-Pfad).
        nodelay: true,
        // Keepalive/Watchdog: tote Hops sterben statt zu hängen.
        keepalive_interval: Some(policy.keepalive),
        keepalive_max: policy.keepalive_max,
        inactivity_timeout: None,
        ..Default::default()
    })
}

async fn session<S>(
    stream: S,
    pin: Option<String>,
    policy: &TunnelPolicy,
) -> Result<(Handle<Client>, Vec<String>), String>
where
    S: AsyncRead + AsyncWrite + Unpin + Send + 'static,
{
    let config = ssh_config(policy);
    let learned = Arc::new(Mutex::new(Vec::<String>::new()));
    let h: Handle<Client> = client::connect_stream(
        config,
        stream,
        Client {
            pin,
            learned: learned.clone(),
        },
    )
    .await
    .map_err(|e| format!("SSH-Handshake: {e}"))?;
    let learned = learned.lock().unwrap().clone();
    Ok((h, learned))
}

async fn auth(h: &mut Handle<Client>, p: &PinnedHop) -> Result<(), String> {
    let key = russh::keys::decode_secret_key(&p.identity_openssh, p.passphrase.as_deref())
        .map_err(|e| format!("Key {}: {e}", p.hop.user))?;
    let key = russh::keys::PrivateKeyWithHashAlg::new(Arc::new(key), None);
    match h
        .authenticate_publickey(p.hop.user.as_str(), key)
        .await
        .map_err(|e| format!("Auth {}: {e}", p.hop.user))?
    {
        client::AuthResult::Success => Ok(()),
        _ => Err(format!("Auth abgelehnt ({})", p.hop.user)),
    }
}

/// Baut die Kette auf (`hops[..n-1]` Jumps, `hops[n-1]` Ziel; auch n = 1
/// ohne Jump), danach `direct-tcpip` auf `lbw_port` am Ziel.
/// `target_sshd` ist der Ziel-sshd aus Sicht des letzten Jumps
/// (Ihre `127.0.0.1:2332`); ohne Jump wird es ignoriert.
pub async fn connect_chain(
    hops: &[PinnedHop],
    target_sshd: (&str, u16),
    lbw_port: u16,
) -> Result<ChainStream, String> {
    connect_chain_with(&TunnelPolicy::default(), hops, target_sshd, lbw_port).await
}

/// Wie `connect_chain`, mit expliziter Betriebs-Politik.
pub async fn connect_chain_with(
    policy: &TunnelPolicy,
    hops: &[PinnedHop],
    target_sshd: (&str, u16),
    lbw_port: u16,
) -> Result<ChainStream, String> {
    connect_chain_inner(policy, hops, target_sshd, lbw_port)
        .await
        .map(|(c, _)| c)
}

/// Wie `connect_chain_with`, liefert zusätzlich die (gelernten oder
/// geprüften) Hostkey-Fingerprints pro Hop — für TOFU-Persistenz.
pub async fn connect_chain_with_pins(
    policy: &TunnelPolicy,
    hops: &[PinnedHop],
    target_sshd: (&str, u16),
    lbw_port: u16,
) -> Result<(ChainStream, Vec<String>), String> {
    connect_chain_inner(policy, hops, target_sshd, lbw_port).await
}

async fn connect_chain_inner(
    policy: &TunnelPolicy,
    hops: &[PinnedHop],
    target_sshd: (&str, u16),
    lbw_port: u16,
) -> Result<(ChainStream, Vec<String>), String> {
    if hops.is_empty() {
        return Err("kein Hop".into());
    }
    let first = &hops[0];
    let tcp = tokio::time::timeout(
        policy.connect_timeout,
        tokio::net::TcpStream::connect((first.hop.host.as_str(), first.hop.port)),
    )
    .await
    .map_err(|_| {
        format!(
            "TCP {}:{}: Timeout nach {:?}",
            first.hop.host, first.hop.port, policy.connect_timeout
        )
    })?
    .map_err(|e| format!("TCP {}:{}: {e}", first.hop.host, first.hop.port))?;
    let (mut h, learned) = session(tcp, first.pin.clone(), policy).await?;
    let mut pins = vec![fingerprint_of(first.pin.as_deref(), &learned)];
    auth(&mut h, first).await?;
    let mut handles = vec![h];

    for (i, p) in hops.iter().enumerate().skip(1) {
        // Über den zuletzt aufgebauten Hop — beim Ziel über dessen
        // sshd-Adresse aus Jump-Sicht (z. B. 127.0.0.1:2332).
        let last = handles.last().expect("mind. ein Handle");
        let (th, tp) = if i == hops.len() - 1 {
            (target_sshd.0, target_sshd.1)
        } else {
            (p.hop.host.as_str(), p.hop.port)
        };
        let ch = last
            .channel_open_direct_tcpip(th, u32::from(tp), "127.0.0.1", 0)
            .await
            .map_err(|e| format!("direct-tcpip {th}:{tp}: {e}"))?;
        let stream = ch.into_stream();
        let (mut h2, learned) = session(stream, p.pin.clone(), policy).await?;
        pins.push(fingerprint_of(p.pin.as_deref(), &learned));
        auth(&mut h2, p).await?;
        handles.push(h2);
    }

    // LBW-Forward am Ziel.
    let last = handles.last().expect("mind. ein Handle");
    let ch = last
        .channel_open_direct_tcpip("127.0.0.1", lbw_port.into(), "127.0.0.1", 0)
        .await
        .map_err(|e| format!("direct-tcpip LBW {lbw_port}: {e}"))?;
    let stream = ch.into_stream();
    Ok((ChainStream { stream, handles }, pins))
}

/// Fingerprint des geprüften Hostkeys, sonst erster gelernter (TOFU).
fn fingerprint_of(pin: Option<&str>, learned: &[String]) -> String {
    match pin {
        Some(p) => p.to_string(),
        None => learned.first().cloned().unwrap_or_default(),
    }
}

/// Baut die Kette mit Reconnect: erster Versuch sofort, danach die
/// `retry_delays` der Politik. Gibt den letzten Fehler zurück, wenn alle
/// Versuche scheitern.
pub async fn connect_chain_retry(
    policy: &TunnelPolicy,
    hops: &[PinnedHop],
    target_sshd: (&str, u16),
    lbw_port: u16,
) -> Result<ChainStream, String> {
    connect_chain_retry_with_pins(policy, hops, target_sshd, lbw_port)
        .await
        .map(|(c, _)| c)
}

/// Wie `connect_chain_retry`, liefert zusätzlich die (gelernten oder
/// geprüften) Hostkey-Fingerprints pro Hop — für TOFU-Persistenz.
pub async fn connect_chain_retry_with_pins(
    policy: &TunnelPolicy,
    hops: &[PinnedHop],
    target_sshd: (&str, u16),
    lbw_port: u16,
) -> Result<(ChainStream, Vec<String>), String> {
    let mut last_err = String::new();
    for (n, wait) in std::iter::once(std::time::Duration::ZERO)
        .chain(policy.retry_delays.iter().copied())
        .enumerate()
    {
        if n > 0 {
            tokio::time::sleep(wait).await;
        }
        match connect_chain_with_pins(policy, hops, target_sshd, lbw_port).await {
            Ok(c) => return Ok(c),
            Err(e) => last_err = e,
        }
    }
    Err(format!("alle Versuche: {last_err}"))
}

/// Kleiner Echo-Selbsttest über den Ketten-Strom (für Tests).
pub async fn echo_check(
    stream: &mut russh::ChannelStream<client::Msg>,
    ping: &[u8],
) -> Result<Vec<u8>, String> {
    stream
        .write_all(ping)
        .await
        .map_err(|e| format!("write: {e}"))?;
    let mut buf = vec![0u8; ping.len()];
    stream
        .read_exact(&mut buf)
        .await
        .map_err(|e| format!("read: {e}"))?;
    Ok(buf)
}
