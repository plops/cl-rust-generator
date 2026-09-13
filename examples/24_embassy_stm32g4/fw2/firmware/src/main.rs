#![no_std]
#![no_main]

use defmt::info;
use defmt_rtt as _; // Leitet Log-Ausgaben über den ST-Link (Debugger) an den PC
use panic_probe as _; // Wenn das Programm abstürzt, wird der Fehler geloggt

use embassy_executor::Spawner;
use embassy_stm32::adc::{Adc, AdcChannel as _, SampleTime};
use embassy_stm32::dac::{DacChannel, Value};
use embassy_stm32::{bind_interrupts, dma, peripherals, Config, Peri};
use embassy_time::{Duration, Ticker, Timer};

// 1. INTERRUPTS VERKNÜPFEN
// Für ADC-Messungen via DMA wird in Embassy nur der DMA-Interrupt benötigt.
bind_interrupts!(struct Irqs {
    // Unser DMA-Kanal braucht diesen Interrupt, um das Ende der Übertragung zu melden
    DMA1_CHANNEL1 => dma::InterruptHandler<peripherals::DMA1_CH1>;
});

// ==============================================================================
// TASK 1: DIGITAL-ANALOG-WANDLER (DAC) - Sägezahn
// ==============================================================================
#[embassy_executor::task]
async fn dac_task(
    dac_peri: Peri<'static, peripherals::DAC1>,
    pin: Peri<'static, peripherals::PA4>,
) {
    info!("DAC Task gestartet (Sägezahn auf PA4)");

    // Initialisiere den DAC
    let mut dac = DacChannel::new_blocking(dac_peri, pin);
    dac.enable();

    // Ein Ticker feuert präzise: 100 Mikrosekunden = 10 kHz Update-Rate
    let mut ticker = Ticker::every(Duration::from_micros(100));
    let mut value: u16 = 0;

    loop {
        // DAC ist 12-Bit (Werte von 0 bis 4095). 0 = 0V, 4095 = 3.3V
        dac.set(Value::Bit12Right(value));

        // Wert um 64 erhöhen. Wenn 4096 erreicht wird, fängt es wieder bei 0 an (Modulo)
        value = (value + 64) % 4096;

        // Pausiere diesen Task, bis die 100 Mikrosekunden um sind.
        ticker.next().await;
    }
}

// ==============================================================================
// TASK 2: ANALOG-DIGITAL-WANDLER (ADC) mit DMA
// ==============================================================================
#[embassy_executor::task]
async fn adc_task(
    adc_peri: Peri<'static, peripherals::ADC1>,
    pin: Peri<'static, peripherals::PA0>,
    mut dma_ch: Peri<'static, peripherals::DMA1_CH1>,
) {
    info!("ADC Task gestartet (Messen auf PA0)");

    let mut adc = Adc::new(adc_peri, Default::default());

    // Pin in typunabhängigen ADC-Kanal (AnyAdcChannel) umwandeln
    let mut channel = pin.degrade_adc();

    // Puffer im Arbeitsspeicher (SRAM) für die DMA-Werte
    let mut buf = [0u16; 128];

    loop {
        // Asynchrones Auslesen über DMA:
        // .reborrow() leiht das DMA-Peripheral für diesen Aufruf aus, ohne es zu konsumieren
        adc.read(
            dma_ch.reborrow(),
            Irqs,
            [(&mut channel, SampleTime::CYCLES47_5)].into_iter(),
            &mut buf,
        )
        .await;

        // Durchschnittswert der 128 Samples berechnen
        let sum: u32 = buf.iter().map(|&v| v as u32).sum();
        let avg = sum / buf.len() as u32;

        // Umrechnung in Millivolt (Referenzspannung ca. 3300 mV bei 12-Bit)
        let mv = (avg * 3300) / 4095;

        info!("ADC DMA Block fertig! Durchschnitt: {} (ca. {} mV)", avg, mv);

        Timer::after_millis(500).await;
    }
}

// ==============================================================================
// HAUPTPROGRAMM (Einstiegspunkt)
// ==============================================================================
#[embassy_executor::main]
async fn main(spawner: Spawner) {
    let mut config = Config::default();

    // ZWINGEND beim STM32G4:
    // Der interne ADC benötigt einen Takt-Multiplexer-Eintrag im RCC.
    config.rcc.mux.adc12sel = embassy_stm32::rcc::mux::Adcsel::SYS;

    let p = embassy_stm32::init(config);

    info!("=== Minimales DAC / ADC-DMA Setup ===");

    // In embassy-executor 0.10.0 gehört .unwrap() an den Task-Aufruf:
    spawner.spawn(dac_task(p.DAC1, p.PA4).unwrap());
    spawner.spawn(adc_task(p.ADC1, p.PA0, p.DMA1_CH1).unwrap());

    core::future::pending::<()>().await;
}
