//! Mode B VNA stage 1: scalar magnitude sweep (spec §3.2).
//!
//! The AWG tone (mode C task) stimulates the DUT while the scope snapshot
//! (mode A task) measures the response amplitude per frequency point; each
//! point stores `(freq_hz, mvpp)`. Exclusivity falls out of the shared
//! request channels (a concurrent request sees them full → `MODE_BUSY`).
//!
//! Stage-1 limits: square-wave stimulus (harmonics unaccounted), magnitude
//! only (no phase — that needs HRTIM phase-stepped ADC triggers, stage 2).
//! DUT wiring (user): PA5 → DUT → PA0, 1 kΩ series, ≤3.3 V (spec §4).

use embassy_sync::blocking_mutex::raw::CriticalSectionRawMutex;
use embassy_sync::channel::Channel;
use embassy_sync::mutex::Mutex;
use embassy_time::Timer;
use g474_common::blocks_05::BLOCK_DATA_MAX;
use g474_common::modes_04::VnaConfig;

use crate::awg_06::{AwgReq, AWG_ACK, AWG_REQ};
use crate::scope_07::{ScopeReq, SCOPE_REQ, SCOPE_RESP, SCOPE_SNAP};

/// Max sweep points (bounds sweep time to a few seconds).
pub const VNA_MAX_POINTS: usize = 128;
/// Samples per frequency point for the amplitude estimate.
pub const VNA_SAMPLES: usize = 64;
/// Serialized bytes per point (u32 freq + u16 mVpp, LE).
pub const VNA_POINT_BYTES: usize = 6;
/// Points per block (96 B payload).
pub const VNA_PER_BLOCK: usize = BLOCK_DATA_MAX / VNA_POINT_BYTES;
/// Tone settle time per point (ms).
const SETTLE_MS: u64 = 15;

/// Sweep requests from either transport (capacity 1 → MODE_BUSY).
pub static VNA_REQ: Channel<CriticalSectionRawMutex, VnaReq, 1> = Channel::new();
/// Completion notice back to the requesting transport.
pub static VNA_RESP: Channel<CriticalSectionRawMutex, VnaResp, 1> = Channel::new();
/// Latest sweep, shared with the transports for block download.
pub static VNA_SNAP: Mutex<CriticalSectionRawMutex, VnaSnap> = Mutex::new(VnaSnap::new());

/// One sweep command (already validated).
#[derive(Debug, Clone, Copy)]
pub struct VnaReq {
    pub cfg: VnaConfig,
}

/// Sweep result.
#[derive(Debug, Clone, Copy)]
pub struct VnaResp {
    /// Points actually stored.
    pub points: usize,
}

/// Shared sweep: point count and `(freq_hz, mvpp)` pairs.
pub struct VnaSnap {
    pub n: usize,
    pub pts: [(u32, u16); VNA_MAX_POINTS],
}

impl VnaSnap {
    pub const fn new() -> Self {
        Self {
            n: 0,
            pts: [(0, 0); VNA_MAX_POINTS],
        }
    }

    /// Fill `out` with LE bytes of points `[start..end)`.
    pub fn bytes_into(&self, start: usize, end: usize, out: &mut heapless::Vec<u8, 96>) {
        out.clear();
        for (f, mv) in self.pts[start..end.min(self.n)].iter() {
            if out.extend_from_slice(&f.to_le_bytes()).is_err() {
                break;
            }
            if out.extend_from_slice(&mv.to_le_bytes()).is_err() {
                break;
            }
        }
    }
}

/// Sweep coordinator: owns no peripherals, drives the AWG + scope tasks.
#[embassy_executor::task]
pub async fn vna_task(
    req: &'static Channel<CriticalSectionRawMutex, VnaReq, 1>,
    resp: &'static Channel<CriticalSectionRawMutex, VnaResp, 1>,
) {
    defmt::info!("vna: ready");
    loop {
        let VnaReq { cfg } = req.receive().await;
        let points = (cfg.points as usize).clamp(2, VNA_MAX_POINTS);
        defmt::info!("vna: sweep {} pts", points);
        {
            let mut snap = VNA_SNAP.lock().await;
            snap.n = 0;
            for i in 0..points {
                let f = cfg.freq_at(i);
                // Stimulus + settle, then snapshot. Any concurrent user of
                // the AWG/scope channels would have failed earlier with
                // MODE_BUSY (capacity 1), so these rendezvous are ours.
                AWG_REQ.send(AwgReq::Start { freq_hz: f }).await;
                AWG_ACK.receive().await;
                Timer::after_millis(SETTLE_MS).await;
                SCOPE_REQ.send(ScopeReq { n: VNA_SAMPLES }).await;
                let got = SCOPE_RESP.receive().await;
                let mv = {
                    let s = SCOPE_SNAP.lock().await;
                    let m = got.n.min(VNA_SAMPLES);
                    let (mut lo, mut hi) = (u16::MAX, 0u16);
                    for v in s.buf[..m].iter() {
                        lo = lo.min(*v);
                        hi = hi.max(*v);
                    }
                    ((hi.saturating_sub(lo) as u32 * 3300) / 4095) as u16
                };
                snap.pts[i] = (f, mv);
                snap.n = i + 1;
            }
            // Rendezvous (not fire-and-forget): every request pairs with one
            // ack; a stray ack would wedge the AWG task on its next send.
            AWG_REQ.send(AwgReq::Stop).await;
            AWG_ACK.receive().await;
            defmt::info!("vna: done");
        }
        resp.send(VnaResp { points }).await;
    }
}
