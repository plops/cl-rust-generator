//! Exclusive mode manager (F2, spec §2): exactly one active measurement mode.
//!
//! A new mode command supersedes the previous state (cancellation by drop at
//! the control level). Preemptive abort of a running hardware gate is T2
//! hardening; until then a second measurement request gets `MODE_BUSY`.

use core::sync::atomic::{AtomicU16, Ordering};

use g474_common::modes_04::{id, FreqConfig};
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
}

/// What the transport must do for one command.
pub enum Action {
    Reply(DeviceResp),
    /// Run a frequency measurement, store it, and reply `Freq`.
    MeasureFreq(FreqConfig),
}

impl Control {
    pub const fn new() -> Self {
        Self {
            mode: id::NONE,
            freq_cfg: None,
            freq_last: None,
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
            // Modes A–D land here until their tasks exist (task order E→C→A→B→D).
            HostCmd::ScopeStart(_)
            | HostCmd::AwgStart(_)
            | HostCmd::CapStart(_)
            | HostCmd::VnaStart(_)
            | HostCmd::AwgLoad { .. } => DeviceResp::Err {
                code: err::NOT_IMPL,
            },
            HostCmd::ScopeRead { .. } | HostCmd::VnaRead { .. } | HostCmd::BlockAck { .. } => {
                DeviceResp::Err { code: err::NO_DATA }
            }
            HostCmd::CapRead => DeviceResp::Err {
                code: err::NOT_IMPL,
            },
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
        None
    }
}
