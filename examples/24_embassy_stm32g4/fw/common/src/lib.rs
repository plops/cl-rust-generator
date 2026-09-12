//! Shared protocol types for the STM32G474 instrument (host + firmware).
//!
//! Text mode: ASCII lines (`\n`/`\r` terminated, leading letter).
//! Binary mode: postcard + COBS frames, `0x00` delimited.

#![cfg_attr(not(test), no_std)]

#[path = "05_blocks.rs"]
pub mod blocks_05;
pub mod frame;
#[path = "04_modes.rs"]
pub mod modes_04;
pub mod router;
pub mod text;

/// Protocol version, bumped on incompatible changes (v2: mode commands + blocks).
pub const PROTO_VER: u8 = 2;
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
    ModeStop,
    SelfTest,
    FreqStart(modes_04::FreqConfig),
    FreqRead,
    ScopeStart(modes_04::ScopeConfig),
    ScopeRead {
        off: u32,
        len: u16,
    },
    AwgLoad {
        off: u32,
        data: heapless::Vec<u8, 64>,
    },
    AwgStart(modes_04::AwgConfig),
    CapStart(modes_04::CapConfig),
    CapRead,
    VnaStart(modes_04::VnaConfig),
    VnaRead {
        off: u32,
        len: u16,
    },
    BlockAck {
        seq: u16,
    },
}

/// Responses device -> host (binary mode).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum DeviceResp {
    Pong,
    Ver {
        proto: u8,
        fw: heapless::String<16>,
    },
    Echo(heapless::Vec<u8, 64>),
    Err {
        code: u8,
    },
    ModeOk {
        mode: u8,
    },
    ModeBusy,
    ModeIdle,
    Freq {
        hz: u32,
        counts: u32,
        gate_ms: u32,
    },
    Block {
        seq: u16,
        total: u16,
        data: heapless::Vec<u8, 96>,
    },
    BlockEnd {
        total: u16,
        crc: u16,
    },
    SelfTestOk {
        bits: u16,
    },
}

/// Binary error codes.
pub mod err {
    pub const UNKNOWN_CMD: u8 = 1;
    pub const BAD_FRAME: u8 = 2;
    pub const OVERFLOW: u8 = 3;
    pub const PROTO_MISMATCH: u8 = 4;
    pub const MODE_BUSY: u8 = 5;
    pub const NO_DATA: u8 = 6;
    pub const BAD_ARG: u8 = 7;
    pub const NOT_IMPL: u8 = 8;
}
