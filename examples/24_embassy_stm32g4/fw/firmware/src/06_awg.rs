//! Mode C AWG stage 1: square-wave tone source (spec §3.3).
//!
//! TIM2_CH1 drives PA5 (push-pull, 0–3.3 V, 50 % duty) at the requested
//! frequency. This is the calibration/reference source for the counter
//! (mode E), scope (mode A) and VNA (mode B) loopbacks: one jumper wire
//! PA5 → measurement input closes each loop.
//!
//! Stage 2 (pending): SRAM1 LUT → DMA → DAC1 → OPAMP1 arbitrary playback for
//! `AwgLoad`. It needs timer-triggered DAC DMA, which embassy-stm32 0.6.0
//! does not expose (only a SOFTWARE trigger exists); `AwgLoad` answers
//! `NOT_IMPL` until then instead of pretending.

use embassy_stm32::gpio::OutputType;
use embassy_stm32::time::Hertz;
use embassy_stm32::timer::low_level::CountingMode;
use embassy_stm32::timer::simple_pwm::{PwmPin, SimplePwm};
use embassy_stm32::{peripherals, Peri};
use embassy_sync::blocking_mutex::raw::CriticalSectionRawMutex;
use embassy_sync::channel::Channel;

/// Waveform requests from either transport (capacity 1 → MODE_BUSY).
pub static AWG_REQ: Channel<CriticalSectionRawMutex, AwgReq, 1> = Channel::new();
/// Rendezvous after the request was applied.
pub static AWG_ACK: Channel<CriticalSectionRawMutex, (), 1> = Channel::new();

/// One AWG command.
#[derive(Debug, Clone, Copy)]
pub enum AwgReq {
    /// Start/retune the square wave (Hz, already validated 1..=1_000_000).
    Start { freq_hz: u32 },
    /// Stop the output (pin idles low).
    Stop,
}

/// Exclusive owner of TIM2 + PA5 (spec §2).
#[embassy_executor::task]
pub async fn awg_task(
    tim: Peri<'static, peripherals::TIM2>,
    pin: Peri<'static, peripherals::PA5>,
    req: &'static Channel<CriticalSectionRawMutex, AwgReq, 1>,
    ack: &'static Channel<CriticalSectionRawMutex, (), 1>,
) {
    defmt::info!("awg: building pwm");
    let ch1 = PwmPin::new(pin, OutputType::PushPull);
    let mut pwm = SimplePwm::new(
        tim,
        Some(ch1),
        None,
        None,
        None,
        Hertz::hz(1_000),
        CountingMode::EdgeAlignedUp,
    );
    pwm.ch1().set_duty_cycle_fully_off();
    defmt::info!("awg: ready");
    loop {
        match req.receive().await {
            AwgReq::Start { freq_hz } => {
                defmt::info!("awg: start {} Hz", freq_hz);
                pwm.set_frequency(Hertz::hz(freq_hz));
                let max = pwm.max_duty_cycle();
                pwm.ch1().set_duty_cycle(max / 2);
            }
            AwgReq::Stop => {
                defmt::info!("awg: stop");
                pwm.ch1().set_duty_cycle_fully_off();
            }
        }
        ack.send(()).await;
    }
}
