//! `lbw-tunnel` — SSH-Tunnelkette für Linux- und Android-Client.
//!
//! Kette (Plan §2.1, echte Topologie tinyeu → hpz6o): Session zum Jump →
//! `direct-tcpip` auf dessen Ziel-sshd-Port → **genestete** Session zum
//! Ziel (Hostkey Ende-zu-Ende) → `direct-tcpip` auf `127.0.0.1:7878`.

#[path = "02_chain.rs"]
pub mod chain;
#[path = "01_config.rs"]
pub mod config;

pub use chain::{
    ChainStream, PinnedHop, connect_chain, connect_chain_retry, connect_chain_retry_with_pins,
    connect_chain_with, connect_chain_with_pins, echo_check, sha256_fingerprint,
};
pub use config::TunnelPolicy;
pub use config::{Hop, parse_hop};
