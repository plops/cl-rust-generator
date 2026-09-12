//! Mode A oscilloscope stage 1: single-ADC DMA snapshot (spec §3.1).
//!
//! ADC1 samples PA0 (`TT_a`: max 3.6 V, 1 kΩ series resistor for inputs
//! above VDDA range) into a static 4096-sample buffer; the transports
//! download slices as `Block`/`BlockEnd` frames. Stage 2 (pending):
//! 4×ADC interleaving to 16 MSPS and ETS via COMP1 + HRTIM delay.

use embassy_stm32::adc::{Adc, AdcChannel as _, SampleTime};
use embassy_stm32::{peripherals, Peri};
use embassy_sync::blocking_mutex::raw::CriticalSectionRawMutex;
use embassy_sync::channel::Channel;
use embassy_sync::mutex::Mutex;
use g474_common::blocks_05::BLOCK_DATA_MAX;

/// Snapshot capacity in samples (u16).
pub const SCOPE_MAX: usize = 4096;
/// Samples per block (96 B payload, LE u16).
pub const SCOPE_PER_BLOCK: usize = BLOCK_DATA_MAX / 2;

/// Acquisition requests from either transport (capacity 1 → MODE_BUSY).
pub static SCOPE_REQ: Channel<CriticalSectionRawMutex, ScopeReq, 1> = Channel::new();
/// Completion notice back to the requesting transport.
pub static SCOPE_RESP: Channel<CriticalSectionRawMutex, ScopeResp, 1> = Channel::new();
/// Latest snapshot, shared with the transports for block download.
pub static SCOPE_SNAP: Mutex<CriticalSectionRawMutex, ScopeSnap> = Mutex::new(ScopeSnap::new());

/// One acquisition command.
#[derive(Debug, Clone, Copy)]
pub struct ScopeReq {
    /// Samples to acquire (1..=`SCOPE_MAX`).
    pub n: usize,
}

/// Acquisition result.
#[derive(Debug, Clone, Copy)]
pub struct ScopeResp {
    /// Samples actually stored.
    pub n: usize,
}

/// Shared snapshot: length and the samples (the transports CRC the
/// downloaded range while streaming, so no CRC is stored here).
pub struct ScopeSnap {
    pub len: usize,
    pub buf: [u16; SCOPE_MAX],
}

impl ScopeSnap {
    pub const fn new() -> Self {
        Self {
            len: 0,
            buf: [0; SCOPE_MAX],
        }
    }

    /// Fill `out` with LE bytes of samples `[start..end)`.
    pub fn bytes_into(&self, start: usize, end: usize, out: &mut heapless::Vec<u8, 96>) {
        out.clear();
        for s in self.buf[start..end.min(self.len)].iter() {
            if out.extend_from_slice(&s.to_le_bytes()).is_err() {
                break;
            }
        }
    }
}

/// Exclusive owner of ADC1 + PA0 (spec §2).
///
/// Stage 1 polls `blocking_read` (no DMA/interrupt dependency); DMA burst
/// transfer returns as stage 2 together with 4×ADC interleaving.
#[embassy_executor::task]
pub async fn scope_task(
    adc_peri: Peri<'static, peripherals::ADC1>,
    pin: Peri<'static, peripherals::PA0>,
    req: &'static Channel<CriticalSectionRawMutex, ScopeReq, 1>,
    resp: &'static Channel<CriticalSectionRawMutex, ScopeResp, 1>,
) {
    defmt::info!("scope: init adc");
    let mut adc = Adc::new(adc_peri, Default::default());
    defmt::info!("scope: init pin");
    let mut adc_pin = pin.degrade_adc();
    defmt::info!("scope: ready");
    loop {
        let ScopeReq { n } = req.receive().await;
        defmt::info!("scope: acquire {}", n);
        let n = n.clamp(1, SCOPE_MAX);
        {
            let mut snap = SCOPE_SNAP.lock().await;
            for i in 0..n {
                snap.buf[i] = adc.blocking_read(&mut adc_pin, SampleTime::CYCLES47_5);
            }
            snap.len = n;
            defmt::info!("scope: {} samples", n);
        }
        resp.send(ScopeResp { n }).await;
    }
}
