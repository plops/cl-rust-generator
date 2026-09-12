//! G474 multi-instrument firmware (USB-CDC + UART, exclusive mode tasks).
//!
//! Wiring only — behavior lives in the numbered modules:
//! `01_clock` (RCC), `02_usb` (CDC plumbing), `03_transport` (serve tasks),
//! `04_control` (exclusive mode state), `05_mode_freq` (counter task).

#![no_std]
#![no_main]

use defmt::info;
use defmt_rtt as _;
use embassy_stm32::usart::{Config as UsartConfig, Uart};
use embassy_stm32::usb::{self, Driver};
use embassy_stm32::{bind_interrupts, dma, peripherals, usart, Config};
use embassy_usb::class::cdc_acm::{CdcAcmClass, State};
use embassy_usb::Builder;
use panic_probe as _;

use core::sync::atomic::Ordering;

#[path = "06_awg.rs"]
mod awg_06;
#[path = "01_clock.rs"]
mod clock_01;
#[path = "04_control.rs"]
mod control_04;
#[path = "05_mode_freq.rs"]
mod mode_freq_05;
#[path = "03_transport.rs"]
mod transport_03;
#[path = "02_usb.rs"]
mod usb_02;

bind_interrupts!(struct Irqs {
    USB_LP => usb::InterruptHandler<peripherals::USB>;
    USART1 => usart::InterruptHandler<peripherals::USART1>;
    DMA1_CHANNEL1 => dma::InterruptHandler<peripherals::DMA1_CH1>;
    DMA1_CHANNEL2 => dma::InterruptHandler<peripherals::DMA1_CH2>;
});

#[embassy_executor::main]
async fn main(spawner: embassy_executor::Spawner) {
    let mut config = Config::default();
    clock_01::apply_clock(&mut config);
    let p = embassy_stm32::init(config);
    info!("g474 instrument starting");

    let driver = Driver::new(p.USB, Irqs, p.PA12, p.PA11);

    let mut config = embassy_usb::Config::new(0xc0de, 0xcafe);
    config.manufacturer = Some("cl-rust-generator");
    config.product = Some("G474 dual-mode proto");
    config.serial_number = Some("000001");

    static CONFIG_DESC: static_cell::StaticCell<[u8; 256]> = static_cell::StaticCell::new();
    static BOS_DESC: static_cell::StaticCell<[u8; 256]> = static_cell::StaticCell::new();
    static CONTROL_BUF: static_cell::StaticCell<[u8; 64]> = static_cell::StaticCell::new();
    static CDC_STATE: static_cell::StaticCell<State> = static_cell::StaticCell::new();
    let config_desc = CONFIG_DESC.init([0; 256]);
    let bos_desc = BOS_DESC.init([0; 256]);
    let control_buf = CONTROL_BUF.init([0; 64]);
    let cdc_state = CDC_STATE.init(State::new());

    let mut builder = Builder::new(
        driver,
        config,
        config_desc,
        bos_desc,
        &mut [], // no MSOS descriptors
        control_buf,
    );
    let class = CdcAcmClass::new(&mut builder, cdc_state, 64);
    let usb = builder.build();
    control_04::SELFTEST_BITS.fetch_or(control_04::selftest::USB, Ordering::Relaxed);

    // Second transport: USART1 on PA9 (TX) / PA10 (RX), 115200 8N1.
    // Same protocol as USB (shared Router), reachable via STLink VCP.
    let mut uc = UsartConfig::default();
    uc.baudrate = 115_200;
    let uart = Uart::new(p.USART1, p.PA10, p.PA9, p.DMA1_CH1, p.DMA1_CH2, Irqs, uc).unwrap();
    info!("UART ready on PA9/PA10 @115200");
    control_04::SELFTEST_BITS.fetch_or(control_04::selftest::UART, Ordering::Relaxed);

    spawner.spawn(transport_03::usb_run_task(usb).unwrap());
    spawner.spawn(transport_03::usb_serve_task(class).unwrap());
    spawner.spawn(transport_03::uart_serve_task(uart).unwrap());
    spawner
        .spawn(mode_freq_05::freq_task(&mode_freq_05::FREQ_REQ, &mode_freq_05::FREQ_RESP).unwrap());
    spawner.spawn(awg_06::awg_task(p.TIM2, p.PA5, &awg_06::AWG_REQ, &awg_06::AWG_ACK).unwrap());

    core::future::pending::<()>().await;
}
