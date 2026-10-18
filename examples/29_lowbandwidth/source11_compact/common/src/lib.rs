//! `lbw-common` — Protokoll v4 für `source11_compact`.
//!
//! Klartext **innerhalb** des SSH-Tunnels (Verschlüsselung = SSH, s.
//! Plan §2.1). Framing: `[u32LE len][bincode-Body]`, `MAX_MSG` schützt
//! vor OOM bei korrupten Längen.

pub mod auth;
#[path = "02_framing.rs"]
pub mod framing;
#[path = "01_types.rs"]
pub mod types;
#[path = "03_yuv.rs"]
pub mod yuv;

pub use auth::{auth_mac, verify_auth};
pub use framing::{FrameReader, Read1, decode_msg, encode_msg, write_msg};
pub use types::*;
