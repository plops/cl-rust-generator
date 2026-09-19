//! Exclusive mode manager (F2, spec §2): exactly one active measurement mode.
//!
//! A new mode command supersedes the previous state (cancellation by drop at
//! the control level). Preemptive abort of a running hardware gate is T2
//! hardening; until then a second measurement request gets `MODE_BUSY`.

use core::sync::atomic::{AtomicU16, Ordering};

use g474_common::modes_04::{id, AwgConfig, FreqConfig, VnaConfig};
use g474_common::{err, DeviceResp, HostCmd, PROTO_VER};

/// Self-test capability bits (`DeviceResp::SelfTestOk::bits`).
pub mod selftest {
    pub const USB: u16 = 0x01;
    pub const UART: u16 = 0x02;
    pub const COMP_DAC: u16 = 0x04;
}

/// Set by `main`/`freq_task` as transports and peripherals come up.
pub static SELFTEST_BITS: AtomicU16 = AtomicU16::new(0);

pub struct Control {
    mode: u8,
    freq_cfg: Option<FreqConfig>,
    freq_last: Option<(u32, u32)>,
    cap_last: Option<(u8, u32, bool)>,
}

/// What the transport must do for one command.
pub enum Action {
    Reply(DeviceResp),
    /// Run a frequency measurement, store it, and reply `Freq`.
    MeasureFreq(FreqConfig),
    /// Start/retune the AWG tone, then reply `ModeOk`.
    AwgStart(AwgConfig),
    /// Measure one discharge timing, then reply `Cap`.
    CapStart(u8),
    /// Acquire a snapshot, then stream `Block`s plus `BlockEnd`.
    ScopeRead {
        off: u32,
        len: u16,
    },
    /// Run a VNA sweep, then reply `ModeOk`.
    VnaStart(VnaConfig),
    /// Download sweep points as `Block`s plus `BlockEnd`.
    VnaRead {
        off: u32,
        len: u16,
    },
}

impl Control {
    pub const fn new() -> Self {
        Self {
            mode: id::NONE,
            freq_cfg: None,
            freq_last: None,
            cap_last: None,
        }
    }

    fn stop(&mut self) -> DeviceResp {
        self.mode = id::NONE;
        self.freq_cfg = None;
        DeviceResp::ModeIdle
    }

    pub fn store_freq(&mut self, counts: u32, gate_ms: u32) {
        self.freq_last = Some((counts, gate_ms));
    }

    pub fn store_cap(&mut self, pin: u8, time_us: u32, timeout: bool) {
        self.cap_last = Some((pin, time_us, timeout));
    }

    pub fn handle_cmd(&mut self, cmd: HostCmd) -> Action {
        let reply = match cmd {
            HostCmd::Ping => DeviceResp::Pong,
            HostCmd::GetVer => DeviceResp::Ver {
                proto: PROTO_VER,
                fw: heapless::String::from(env!("CARGO_PKG_VERSION")),
            },
            HostCmd::Echo(v) => DeviceResp::Echo(v),
            HostCmd::ModeStop => return Action::Reply(self.stop()),
            HostCmd::SelfTest => {
                return Action::Reply(DeviceResp::SelfTestOk {
                    bits: SELFTEST_BITS.load(Ordering::Relaxed),
                })
            }
            HostCmd::FreqStart(cfg) => {
                if cfg.validate().is_err() {
                    return Action::Reply(DeviceResp::Err { code: err::BAD_ARG });
                }
                if cfg.filter != 0 {
                    // TIM2 digital-filter path not implemented yet (E1 uses
                    // COMP edges + time gate); reject instead of mismeasuring.
                    return Action::Reply(DeviceResp::Err { code: err::BAD_ARG });
                }
                self.mode = id::E_FREQ;
                self.freq_cfg = Some(cfg);
                return Action::MeasureFreq(cfg);
            }
            HostCmd::FreqRead => match self.freq_last {
                Some((counts, gate_ms)) => DeviceResp::Freq {
                    hz: FreqConfig::hz_from_counts(counts, gate_ms),
                    counts,
                    gate_ms,
                },
                None => DeviceResp::Err { code: err::NO_DATA },
            },
            HostCmd::AwgStart(cfg) => {
                if cfg.validate().is_err() {
                    return Action::Reply(DeviceResp::Err { code: err::BAD_ARG });
                }
                self.mode = id::C_AWG;
                return Action::AwgStart(cfg);
            }
            HostCmd::ScopeStart(cfg) => {
                if cfg.validate().is_err() {
                    return Action::Reply(DeviceResp::Err { code: err::BAD_ARG });
                }
                if cfg.interleaved != 0 {
                    // 4×ADC interleaving is stage 2.
                    return Action::Reply(DeviceResp::Err {
                        code: err::NOT_IMPL,
                    });
                }
                self.mode = id::A_SCOPE;
                return Action::Reply(DeviceResp::ModeOk { mode: id::A_SCOPE });
            }
            HostCmd::ScopeRead { off, len } => {
                let off = off as usize;
                let len = len as usize;
                if len == 0 || off.saturating_add(len) > crate::scope_07::SCOPE_MAX {
                    return Action::Reply(DeviceResp::Err { code: err::BAD_ARG });
                }
                self.mode = id::A_SCOPE;
                return Action::ScopeRead {
                    off: off as u32,
                    len: len as u16,
                };
            }
            HostCmd::VnaStart(cfg) => {
                if cfg.validate().is_err() {
                    return Action::Reply(DeviceResp::Err { code: err::BAD_ARG });
                }
                self.mode = id::B_VNA;
                return Action::VnaStart(cfg);
            }
            HostCmd::VnaRead { off, len } => {
                let off = off as usize;
                let len = len as usize;
                if len == 0 || off.saturating_add(len) > crate::vna_08::VNA_MAX_POINTS {
                    return Action::Reply(DeviceResp::Err { code: err::BAD_ARG });
                }
                self.mode = id::B_VNA;
                return Action::VnaRead {
                    off: off as u32,
                    len: len as u16,
                };
            }
            HostCmd::CapStart(cfg) => {
                if cfg.validate().is_err() {
                    return Action::Reply(DeviceResp::Err { code: err::BAD_ARG });
                }
                self.mode = id::D_CAP;
                return Action::CapStart(cfg.pin);
            }
            HostCmd::CapRead => match self.cap_last {
                Some((pin, time_us, timeout)) => DeviceResp::Cap {
                    pin,
                    time_us,
                    timeout,
                },
                None => DeviceResp::Err { code: err::NO_DATA },
            },
            // AwgLoad (LUT upload) waits for timer-triggered DAC DMA (stage 2).
            HostCmd::AwgLoad { .. } => DeviceResp::Err {
                code: err::NOT_IMPL,
            },
            HostCmd::BlockAck { .. } => DeviceResp::Err { code: err::NO_DATA },
        };
        Action::Reply(reply)
    }

    /// Text-mode mode commands. Returns a reply when the line is a mode
    /// command; `None` means "fall through to the legacy text parser".
    pub fn handle_text(&mut self, line: &str) -> Option<heapless::String<96>> {
        let t = line.trim();
        let mut out: heapless::String<96> = heapless::String::new();
        if t.eq_ignore_ascii_case("MODE STOP") {
            let _ = out.push_str(match self.stop() {
                DeviceResp::ModeIdle => "OK MODE IDLE",
                _ => "ERR INTERNAL",
            });
            return Some(out);
        }
        if t.eq_ignore_ascii_case("GET FREQ") || t.eq_ignore_ascii_case("G FREQ") {
            match self.freq_last {
                Some((counts, gate_ms)) => {
                    let _ = core::fmt::write(
                        &mut out,
                        format_args!(
                            "OK FREQ hz={} counts={} gate_ms={}",
                            FreqConfig::hz_from_counts(counts, gate_ms),
                            counts,
                            gate_ms
                        ),
                    );
                }
                None => {
                    let _ = out.push_str("ERR NO_DATA");
                }
            }
            return Some(out);
        }
        if t.eq_ignore_ascii_case("GET CAP") || t.eq_ignore_ascii_case("G CAP") {
            match self.cap_last {
                Some((pin, time_us, timeout)) => {
                    let _ = core::fmt::write(
                        &mut out,
                        format_args!(
                            "OK CAP pin={} time_us={} timeout={}",
                            pin, time_us, timeout as u8
                        ),
                    );
                }
                None => {
                    let _ = out.push_str("ERR NO_DATA");
                }
            }
            return Some(out);
        }
        None
    }
}
