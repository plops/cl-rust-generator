//! Mode D capacitance meter stage 1: GPIO discharge timing (spec §3.4).
//!
//! Per TDM channel: drive the pin push-pull HIGH (charge), switch to input
//! with internal pull-down (discharge), and time the decay past the ~1.2 V
//! Vref threshold with a comparator edge + `embassy-time` stamp.
//!
//! Channels: 0 = PA1/COMP1, 1 = PB1/COMP1, 2 = PB0/COMP4 (`TT_a` pins: max
//! 3.6 V — 1 kΩ series resistors on all test wiring). Peripherals are
//! re-stolen per phase (charge → arm → sense) so one task can TDM all three
//! channels without holding pins across requests (same pattern as mode E).
//!
//! Stage-1 limits: `embassy-time` ticks at 32_768 Hz (~30 µs resolution) and
//! the internal pull-down is a coarse ~40 kΩ, so small caps read ~0 and the
//! firmware reports raw discharge *time* — capacitance conversion (with the
//! actual R) belongs host-side. TIM2 input-capture timestamps are stage 2.

use embassy_stm32::comp::{self, Comp, Config, Hysteresis, InterruptHandler, InvertingInput};
use embassy_stm32::gpio::{Input, Level, Output, Pull, Speed};
use embassy_stm32::interrupt::typelevel::{Binding, COMP1_2_3};
use embassy_stm32::{bind_interrupts, peripherals, Peri};
use embassy_sync::blocking_mutex::raw::CriticalSectionRawMutex;
use embassy_sync::channel::Channel;
use embassy_time::{Duration, Instant, Timer};

// Proper binding: vector COMP4_5_6 is unused elsewhere.
bind_interrupts!(struct Irqs456 {
    COMP4_5_6 => comp::InterruptHandler<peripherals::COMP4>;
});

/// Type-level token for COMP1, whose vector (COMP1_2_3) is already bound to
/// the freq task's COMP2 handler — one ISR per vector, so `bind_interrupts!`
/// cannot be used twice.
///
/// SAFETY: the `Binding` contract asks that `H::on_interrupt()` run whenever
/// `I` fires. Cap never arms COMP1 EXTI (it polls `output_level()`; the EXTI
/// mask stays at reset) and `Comp::new` ignores its `_irq` token at runtime
/// (NVIC unpend/enable only), so no COMP1 interrupt can occur and the claim
/// is vacuous. The installed COMP1_2_3 shim keeps serving COMP2 correctly.
#[derive(Copy, Clone)]
pub struct CapComp1Token;
unsafe impl Binding<COMP1_2_3, InterruptHandler<peripherals::COMP1>> for CapComp1Token {}

/// Charge dwell time: enough for ≤1 µF through the push-pull driver.
const CHARGE_US: u64 = 50;
/// Discharge timeout: bounds every measurement.
const TIMEOUT_MS: u64 = 200;

/// Measure requests from either transport (capacity 1 → MODE_BUSY).
pub static CAP_REQ: Channel<CriticalSectionRawMutex, CapReq, 1> = Channel::new();
/// Completed measurements back to the requesting transport.
pub static CAP_RESP: Channel<CriticalSectionRawMutex, CapResp, 1> = Channel::new();

/// One measurement command (channel already validated 0..=2).
#[derive(Debug, Clone, Copy)]
pub struct CapReq {
    pub pin: u8,
}

/// One completed discharge timing.
#[derive(Debug, Clone, Copy)]
pub struct CapResp {
    pub pin: u8,
    pub time_us: u32,
    pub timeout: bool,
}

fn comp_config() -> Config {
    Config {
        inverting_input: InvertingInput::Vref,
        hysteresis: Hysteresis::None,
        ..Default::default()
    }
}

/// Charge one pin HIGH through the push-pull driver.
async fn charge<P: embassy_stm32::gpio::Pin>(pin: Peri<'static, P>) {
    let _out = Output::new(pin, Level::High, Speed::Low);
    Timer::after_micros(CHARGE_US).await;
    // Drop restores analog mode; PUPDR is (re-)set in the arm phase.
}

/// Switch one pin to input with pull-down (discharge path).
fn arm_pull<P: embassy_stm32::gpio::Pin>(pin: Peri<'static, P>) {
    let inp = Input::new(pin, Pull::Down);
    // Drop keeps PUPDR (Down); Comp::new then only flips MODER to analog.
    // If a future embassy changes Drop to clear PUPDR, HIL (timeout on a
    // known capacitor) will catch it — see task D2.
    drop(inp);
}

/// Time the decay past Vref on one comparator input by polling.
///
/// Polling (instead of EXTI waits) keeps COMP1 off the shared COMP1_2_3
/// vector entirely; 25 µs poll period is ample for ms-scale discharges.
async fn time_decay<C, P>(
    comp_peri: Peri<'static, C>,
    pin_peri: Peri<'static, P>,
    irq: impl Binding<C::Interrupt, InterruptHandler<C>>,
) -> (u32, bool)
where
    C: comp::Instance,
    P: comp::InputPlusPin<C> + embassy_stm32::gpio::Pin,
{
    let mut comp = Comp::new(comp_peri, pin_peri, irq, comp_config());
    comp.enable();
    let t0 = Instant::now();
    let limit = Duration::from_millis(TIMEOUT_MS);
    loop {
        if !comp.output_level() {
            let us = t0.elapsed().as_micros().min(u32::MAX as u64) as u32;
            drop(comp);
            return (us, false);
        }
        if t0.elapsed() >= limit {
            drop(comp);
            return (TIMEOUT_MS as u32 * 1000, true);
        }
        Timer::after_micros(25).await;
    }
}

/// Exclusive channel sequencer (spec §2): exactly one TDM measurement at a
/// time; peripherals are dropped (de-init) at the end of every request.
#[embassy_executor::task]
pub async fn cap_task(
    req: &'static Channel<CriticalSectionRawMutex, CapReq, 1>,
    resp: &'static Channel<CriticalSectionRawMutex, CapResp, 1>,
) {
    defmt::info!("cap: ready");
    loop {
        let CapReq { pin } = req.receive().await;
        defmt::info!("cap: measure ch{}", pin);
        // Charge + arm use fresh steals; the drivers are dropped before the
        // sense phase so exactly one owner exists at any time.
        match pin {
            0 => {
                let p = unsafe { embassy_stm32::Peripherals::steal() };
                charge(p.PA1).await;
                let p = unsafe { embassy_stm32::Peripherals::steal() };
                arm_pull(p.PA1);
                let p = unsafe { embassy_stm32::Peripherals::steal() };
                let (t, to) = time_decay(p.COMP1, p.PA1, CapComp1Token).await;
                resp.send(CapResp {
                    pin,
                    time_us: t,
                    timeout: to,
                })
                .await;
            }
            1 => {
                let p = unsafe { embassy_stm32::Peripherals::steal() };
                charge(p.PB1).await;
                let p = unsafe { embassy_stm32::Peripherals::steal() };
                arm_pull(p.PB1);
                let p = unsafe { embassy_stm32::Peripherals::steal() };
                let (t, to) = time_decay(p.COMP1, p.PB1, CapComp1Token).await;
                resp.send(CapResp {
                    pin,
                    time_us: t,
                    timeout: to,
                })
                .await;
            }
            _ => {
                let p = unsafe { embassy_stm32::Peripherals::steal() };
                charge(p.PB0).await;
                let p = unsafe { embassy_stm32::Peripherals::steal() };
                arm_pull(p.PB0);
                let p = unsafe { embassy_stm32::Peripherals::steal() };
                let (t, to) = time_decay(p.COMP4, p.PB0, Irqs456).await;
                resp.send(CapResp {
                    pin,
                    time_us: t,
                    timeout: to,
                })
                .await;
            }
        }
        defmt::info!("cap: done");
    }
}
