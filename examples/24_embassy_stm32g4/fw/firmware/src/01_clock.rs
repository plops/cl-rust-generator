//! System clocks (F0): HSI48 trimmed by USB SOF via CRS feeds the USB domain.
//! No HSE dependency — crystal population unverified (see 01_init walkthrough).

use embassy_stm32::Config;

/// Apply the validated clock tree to a fresh `Config`.
pub fn apply_clock(config: &mut Config) {
    use embassy_stm32::rcc::*;
    config.rcc.hsi48 = Some(Hsi48Config {
        sync_from_usb: true,
    });
    config.rcc.mux.clk48sel = mux::Clk48sel::HSI48;
}
