//! Shared protocol types for the STM32G474 instrument (host + firmware).
//!
//! Text mode: ASCII lines (`\n`/`\r` terminated, leading letter).
//! Binary mode: postcard + COBS frames, `0x00` delimited.

#![cfg_attr(not(test), no_std)]

pub mod frame;
pub mod router;
pub mod text;

/// Protocol version, bumped on incompatible changes.
pub const PROTO_VER: u8 = 1;
/// Firmware version string reported by `GET VER` / `DeviceResp::Ver`.
pub const FW_VER: &str = env!("CARGO_PKG_VERSION");

/// Max text line length (bytes, excluding terminator).
pub const MAX_LINE: usize = 64;
/// Max COBS frame buffer on the device.
pub const MAX_FRAME: usize = 128;

use serde::{Deserialize, Serialize};

/// Commands host -> device (binary mode).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum HostCmd {
    Ping,
    GetVer,
    Echo(heapless::Vec<u8, 64>),
}

/// Responses device -> host (binary mode).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum DeviceResp {
    Pong,
    Ver { proto: u8, fw: heapless::String<16> },
    Echo(heapless::Vec<u8, 64>),
    Err { code: u8 },
}

/// Binary error codes.
pub mod err {
    pub const UNKNOWN_CMD: u8 = 1;
    pub const BAD_FRAME: u8 = 2;
    pub const OVERFLOW: u8 = 3;
    pub const PROTO_MISMATCH: u8 = 4;
}
