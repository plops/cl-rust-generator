#![no_std]
#![no_main]

use defmt::info;
use defmt_rtt as _;
use panic_probe as _;

use embassy_executor::Spawner;
use embassy_stm32::adc::{Adc, AdcChannel as _, SampleTime};
use embassy_stm32::dac::{DacChannel, Value};
use embassy_stm32::{bind_interrupts, dma, peripherals, Config, Peri};
use embassy_time::{Duration, Ticker, Timer};

// ============================================================================
// 1. INTERRUPT-BINDUNG
// ============================================================================
// Embassy verlangt, dass Interrupts typensicher deklariert werden.
// Für ADC-Messungen via DMA wird in Embassy nur der DMA-Interrupt benötigt.
// Wenn der DMA-Controller Datenblöcke fertig übertragen hat, löst er einen
// Interrupt aus, der von den Embassy-Treibern intern abgefangen wird.
bind_interrupts!(struct Irqs {
    DMA1_CHANNEL1 => dma::InterruptHandler<peripherals::DMA1_CH1>;
});

// ============================================================================
// 2. WELLENFORM-DEFINITION (SÄGEZAHN)
// ============================================================================
// Der 12-Bit-DAC des STM32G4 akzeptiert Werte von 0 (0,0 V) bis 4095 (3,3 V).
// Wir berechnen hier eine Lookup-Table (LUT) mit 64 Stufen zur Kompilierzeit
// (const eval), sodass zur Laufzeit kein Speicher oder Rechenzeit benötigt wird.
const SAWTOOTH_SAMPLES: usize = 64;
const SAWTOOTH_TABLE: [u16; SAWTOOTH_SAMPLES] = {
    let mut table = [0u16; SAWTOOTH_SAMPLES];
    let mut i = 0;
    while i < SAWTOOTH_SAMPLES {
        // Linearer Anstieg: 0 -> 4095
        table[i] = ((i as u32 * 4095) / (SAWTOOTH_SAMPLES as u32 - 1)) as u16;
        i += 1;
    }
    table
};

// ============================================================================
// 3. DAC-TASK (Signalquelle via zirkulärem DMA)
// ============================================================================
#[embassy_executor::task]
async fn dac_task(
    dac_peri: Peri<'static, peripherals::DAC1>,
    pin: Peri<'static, peripherals::PA4>,
) {
    info!("DAC: Initialisiere DAC1_OUT1 an PA4...");

    // Initialisiere den DAC
    let mut dac = DacChannel::new_blocking(dac_peri, pin);
    dac.enable();

    info!("DAC: Starte Sägezahn-Ausgabe...");

    // Ein Ticker feuert präzise: 100 Mikrosekunden = 10 kHz Update-Rate
    let mut ticker = Ticker::every(Duration::from_micros(100));
    let mut index: usize = 0;

    loop {
        // DAC ist 12-Bit (Werte von 0 bis 4095). 0 = 0V, 4095 = 3.3V
        dac.set(Value::Bit12Right(SAWTOOTH_TABLE[index]));

        // Zum nächsten Wert in der Lookup-Table springen
        index = (index + 1) % SAWTOOTH_SAMPLES;

        // Pausiere diesen Task, bis die 100 Mikrosekunden um sind.
        ticker.next().await;
    }
}

// ============================================================================
// 4. ADC-TASK (Messwerterfassung via DMA)
// ============================================================================
#[embassy_executor::task]
async fn adc_task(
    adc_peri: Peri<'static, peripherals::ADC1>,
    pin: Peri<'static, peripherals::PA0>,
    mut dma_ch: Peri<'static, peripherals::DMA1_CH1>,
) {
    info!("ADC: Initialisiere ADC1 an PA0 mit DMA1_CH1...");

    // ADC initialisieren. Der Standard-Konstruktor kalibriert den Wandler intern.
    let mut adc = Adc::new(adc_peri, Default::default());

    // Pin in einen generischen ADC-Kanal umwandeln
    let mut channel = pin.degrade_adc();

    // Puffer für 128 aufeinanderfolgende Messwerte
    let mut buffer = [0u16; 128];

    loop {
        // ADC-Wandlung über DMA ausführen:
        // Der DMA-Kanal füllt den gesamten Puffer mit 128 Abtastungen.
        // Der Aufruf blockiert asynchron (.await), bis der DMA-Transfer fertig ist.
        // .reborrow() leiht das DMA-Peripheral für diesen Aufruf aus, ohne es zu konsumieren
        adc.read(
            dma_ch.reborrow(),
            Irqs,
            [(&mut channel, SampleTime::CYCLES47_5)].into_iter(),
            &mut buffer,
        )
        .await;

        // Statistische Auswertung des Puffers (Mittelwert, Min, Max)
        let mut sum: u32 = 0;
        let mut min_val: u16 = u16::MAX;
        let mut max_val: u16 = 0;

        for &sample in buffer.iter() {
            sum += sample as u32;
            min_val = min_val.min(sample);
            max_val = max_val.max(sample);
        }

        let avg_raw = (sum / buffer.len() as u32) as u16;
        // Umrechnung von 12-Bit Rohwert (0..4095) in Millivolt (bei Vdda = 3300 mV)
        let avg_mv = (avg_raw as u32 * 3300) / 4095;
        let min_mv = (min_val as u32 * 3300) / 4095;
        let max_mv = (max_val as u32 * 3300) / 4095;

        info!(
            "ADC: Mittelwert = {} mV (Raw: {}), Min = {} mV, Max = {} mV",
            avg_mv, avg_raw, min_mv, max_mv
        );

        // 500 ms Pause bis zur nächsten Messung
        Timer::after_millis(500).await;
    }
}

// ============================================================================
// 5. HAUPTPROGRAMM (System-Boot & Task-Start)
// ============================================================================
#[embassy_executor::main]
async fn main(spawner: Spawner) {
    let mut config = Config::default();

    // WICHTIG beim STM32G4:
    // Der interne ADC benötigt zwingend einen Takt-Multiplexer-Eintrag im RCC.
    // Ohne diese Zeile schlägt Adc::new mit einem Panic fehl!
    config.rcc.mux.adc12sel = embassy_stm32::rcc::mux::Adcsel::SYS;

    // Hardware initialisieren
    let p = embassy_stm32::init(config);
    info!("=== STM32G474 Minimal DMA Demo gestartet ===");

    // Beide Tasks starten: Sie laufen kooperativ nebeneinander auf dem Executor.
    // In embassy-executor 0.10.0 gehört .unwrap() an den Task-Aufruf:
    spawner.spawn(dac_task(p.DAC1, p.PA4).unwrap());
    spawner.spawn(adc_task(p.ADC1, p.PA0, p.DMA1_CH1).unwrap());

    // Der main-Task hat seine Arbeit erledigt und wartet ewig
    core::future::pending::<()>().await;
}
