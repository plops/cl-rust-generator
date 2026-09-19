//! App state + actions (T1): one screen per mode, space runs the default
//! measurement, results accumulate in the log pane.

use crate::comms_01::{check_proto, Comms};
use g474_common::modes_04::{AwgConfig, CapConfig, FreqConfig, ScopeConfig, VnaConfig};
use g474_common::{DeviceResp, HostCmd};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Screen {
    Status,
    Freq,
    Scope,
    Awg,
    Vna,
    Cap,
}

impl Screen {
    pub fn title(self) -> &'static str {
        match self {
            Screen::Status => "Status",
            Screen::Freq => "Freq counter",
            Screen::Scope => "Scope",
            Screen::Awg => "AWG tone",
            Screen::Vna => "VNA sweep",
            Screen::Cap => "Capacitance",
        }
    }

    pub fn hint(self) -> &'static str {
        match self {
            Screen::Status => "space: re-query VER + SELFTEST",
            Screen::Freq => "space: 500 ms gate @1650 mV",
            Screen::Scope => "space: 256-sample snapshot",
            Screen::Awg => "space: toggle 1 kHz tone",
            Screen::Vna => "space: 500-2000 Hz / 5 pts",
            Screen::Cap => "space: measure ch0-2",
        }
    }
}

pub struct App {
    pub screen: Screen,
    pub log: Vec<String>,
    pub scope: Vec<u16>,
    pub vna: Vec<(u32, u16)>,
    pub awg_on: bool,
    comms: Comms,
}

impl App {
    pub fn new(port: &str) -> Result<Self, String> {
        let mut comms = Comms::open(port)?;
        let ver = check_proto(&mut comms)?;
        let mut app = Self {
            screen: Screen::Status,
            log: vec![format!("connected: {}", ver)],
            scope: Vec::new(),
            vna: Vec::new(),
            awg_on: false,
            comms,
        };
        app.query_status();
        Ok(app)
    }

    fn push(&mut self, line: String) {
        self.log.push(line);
        if self.log.len() > 200 {
            self.log.drain(..self.log.len() - 200);
        }
    }

    fn query_status(&mut self) {
        match self.comms.roundtrip(&HostCmd::SelfTest) {
            Ok(DeviceResp::SelfTestOk { bits }) => {
                self.push(format!("selftest bits={:#06x}", bits))
            }
            Ok(other) => self.push(format!("selftest unexpected {:?}", other)),
            Err(e) => self.push(format!("selftest error {}", e)),
        }
    }

    /// Run the screen's default action (space key).
    pub fn action(&mut self) {
        match self.screen {
            Screen::Status => self.query_status(),
            Screen::Freq => {
                let cfg = FreqConfig {
                    level_mv: 1650,
                    hyst: 2,
                    filter: 0,
                    gate_ms: 500,
                };
                match self.comms.roundtrip(&HostCmd::FreqStart(cfg)) {
                    Ok(DeviceResp::Freq {
                        hz,
                        counts,
                        gate_ms,
                    }) => self.push(format!(
                        "freq {} Hz ({} counts / {} ms)",
                        hz, counts, gate_ms
                    )),
                    Ok(other) => self.push(format!("freq unexpected {:?}", other)),
                    Err(e) => self.push(format!("freq error {}", e)),
                }
            }
            Screen::Scope => {
                let _ = self.comms.roundtrip(&HostCmd::ScopeStart(ScopeConfig {
                    interleaved: 0,
                    level_mv: 100,
                }));
                match self.comms.scope_read(0, 256) {
                    Ok(bytes) => {
                        let mut samples = Vec::new();
                        for c in bytes.as_chunks::<2>().0 {
                            samples.push(u16::from_le_bytes([c[0], c[1]]));
                        }
                        self.push(format!("scope {} samples", samples.len()));
                        self.scope = samples;
                    }
                    Err(e) => self.push(format!("scope error {}", e)),
                }
            }
            Screen::Awg => {
                self.awg_on = !self.awg_on;
                let cmd = if self.awg_on {
                    HostCmd::AwgStart(AwgConfig { freq_hz: 1000 })
                } else {
                    HostCmd::ModeStop
                };
                match self.comms.roundtrip(&cmd) {
                    Ok(r) => self.push(format!("awg on={} -> {:?}", self.awg_on, r)),
                    Err(e) => {
                        self.awg_on = false;
                        self.push(format!("awg error {}", e))
                    }
                }
            }
            Screen::Vna => {
                let cfg = VnaConfig {
                    f0_hz: 500,
                    f1_hz: 2000,
                    points: 5,
                };
                match self.comms.roundtrip(&HostCmd::VnaStart(cfg)) {
                    Ok(DeviceResp::ModeOk { .. }) => match self.comms.vna_read(0, 5) {
                        Ok(pts) => {
                            for (f, mv) in &pts {
                                self.push(format!("vna {} Hz -> {} mVpp", f, mv));
                            }
                            self.vna = pts;
                        }
                        Err(e) => self.push(format!("vna read error {}", e)),
                    },
                    Ok(other) => self.push(format!("vna unexpected {:?}", other)),
                    Err(e) => self.push(format!("vna error {}", e)),
                }
            }
            Screen::Cap => {
                for pin in 0..=2u8 {
                    match self.comms.roundtrip(&HostCmd::CapStart(CapConfig { pin })) {
                        Ok(DeviceResp::Cap {
                            pin,
                            time_us,
                            timeout,
                        }) => self.push(format!(
                            "cap ch{}: {} us{}",
                            pin,
                            time_us,
                            if timeout { " TIMEOUT" } else { "" }
                        )),
                        Ok(other) => self.push(format!("cap unexpected {:?}", other)),
                        Err(e) => self.push(format!("cap error {}", e)),
                    }
                }
            }
        }
    }
}
