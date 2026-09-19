//! Mode E frequency counter (E1, spec §3.5).
//!
//! Signal path: PA7 → COMP2 non-inverting input; trigger level: DAC1_CH1 →
//! COMP2 `Dac1` inverting input (0–3300 mV, VDDA-referenced). The gate counts
//! COMP rising edges in 10 ms slices against `embassy-time`.
//!
//! Analog limits (spec §4): PA7 is a `TT_a` pin — max 3.6 V (abs. 4.0 V).
//! Feed measurement signals through ~1 kΩ series resistors; 5 V-tolerant
//! `FT` pins are the right choice for digital counter inputs on later spins.
//!
//! E1 limits: TIM2 hardware counter + digital input filter are pending, so
//! `filter != 0` is rejected (see `04_control`) and usable rates are capped
//! by edge-polling at a few kHz. The TIM2 upgrade keeps this wire protocol.

use embassy_futures::select::{select, Either};
use embassy_stm32::comp::{self, Comp, Config, Hysteresis, InvertingInput};
use embassy_stm32::dac::{DacChannel, Value};
use embassy_stm32::{bind_interrupts, peripherals};
use embassy_sync::blocking_mutex::raw::CriticalSectionRawMutex;
use embassy_sync::channel::Channel;
use embassy_time::{Duration, Instant, Timer};
use g474_common::modes_04::{dac12_from_mv, FreqConfig};

use crate::control_04::{selftest, SELFTEST_BITS};
use core::sync::atomic::Ordering;

bind_interrupts!(struct Irqs {
    COMP1_2_3 => comp::InterruptHandler<peripherals::COMP2>;
});

/// Measurement requests from either transport (capacity 1 → MODE_BUSY).
pub static FREQ_REQ: Channel<CriticalSectionRawMutex, FreqConfig, 1> = Channel::new();
/// Completed measurements back to the requesting transport.
pub static FREQ_RESP: Channel<CriticalSectionRawMutex, FreqResult, 1> = Channel::new();

/// One completed gate: raw edge count plus the gate it was counted in.
pub struct FreqResult {
    pub counts: u32,
    pub gate_ms: u32,
}

/// Wire `hyst` step (0..=3) → G4 comparator hysteresis.
fn hyst_from_step(step: u8) -> Hysteresis {
    match step {
        0 => Hysteresis::None,
        1 => Hysteresis::Hyst10M,
        2 => Hysteresis::Hyst30M,
        _ => Hysteresis::Hyst70M,
    }
}

fn comp_config(cfg: &FreqConfig) -> Config {
    Config {
        inverting_input: InvertingInput::Dac1,
        hysteresis: hyst_from_step(cfg.hyst),
        ..Default::default()
    }
}

/// Count COMP2 rising edges until `deadline`, in 10 ms cancellable slices.
async fn run_gate(comp: &mut Comp<'static, peripherals::COMP2>, gate_ms: u32) -> u32 {
    let deadline = Instant::now() + Duration::from_millis(gate_ms as u64);
    let mut counts = 0u32;
    while Instant::now() < deadline {
        let remain = deadline.saturating_duration_since(Instant::now());
        let slice = remain.min(Duration::from_millis(10));
        match select(comp.wait_for_rising_edge(), Timer::after(slice)).await {
            Either::First(_) => counts += 1,
            Either::Second(_) => {}
        }
    }
    counts
}

/// Exclusive owner of COMP2 + DAC1 (spec §2: de-init by drop at task end).
/// Peripherals are re-stolen per request so `Comp` can be rebuilt with the
/// requested hysteresis (the driver exposes no runtime setter).
#[embassy_executor::task]
pub async fn freq_task(
    req: &'static Channel<CriticalSectionRawMutex, FreqConfig, 1>,
    resp: &'static Channel<CriticalSectionRawMutex, FreqResult, 1>,
) {
    loop {
        let cfg = req.receive().await;
        // Peripherals are untouched by the transports, so re-stealing here is
        // single-owner at any time (same pattern as embassy's dac test).
        let p = unsafe { embassy_stm32::Peripherals::steal() };
        let mut dac = DacChannel::new_blocking(p.DAC1, p.PA4);
        dac.set(Value::Bit12Right(dac12_from_mv(cfg.level_mv)));
        let mut comp = Comp::new(p.COMP2, p.PA7, Irqs, comp_config(&cfg));
        comp.enable();
        SELFTEST_BITS.fetch_or(selftest::COMP_DAC, Ordering::Relaxed);
        let counts = run_gate(&mut comp, cfg.gate_ms).await;
        // Drop order matters: `comp` and `dac` de-init here (exclusive HW).
        drop(comp);
        drop(dac);
        resp.send(FreqResult {
            counts,
            gate_ms: cfg.gate_ms,
        })
        .await;
    }
}
