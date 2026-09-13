Die Compiler-Fehler entstehen durch API-Änderungen in den verwendeten Versionen **`embassy-stm32 0.6.0`** und **`embassy-executor 0.10.0`**. 

Hier ist die genaue Erklärung der Fehlerursachen und wie sie behoben werden:

---

### Die Ursachen im Detail

1. **`adc::InterruptHandler` existiert nicht** (Fehler `E0425`):
   * Auf dem STM32G4 stellt das Modul `adc` keinen eigenen `InterruptHandler` bereit. Für ADC-Leseoperationen über DMA wird ausschließlich der DMA-Interrupt benötigt (`dma::InterruptHandler<peripherals::DMA1_CH1>`). Die Zeile `ADC1_2 => adc::InterruptHandler<...>;` muss aus `bind_interrupts!` entfernt werden.

2. **Peripherie-Typen sind jetzt `Peri<'static, T>`** (Fehler `E0308`):
   * In `embassy-stm32 0.6.0` sind die Peripherie-Singletons in `p` (z. B. `p.ADC1`, `p.DAC1`, `p.PA0`) vom Typ `Peri<'static, T>` und nicht mehr reine `peripherals::T`.
   * Entsprechend müssen die Task-Signaturen `Peri<'static, peripherals::...>` als Parametertyp erwarten.

3. **Kanal-Typ für `adc.read`** (Fehler `E0271`):
   * `adc.read()` erwartet in der Sequenz keine rohen Pins, sondern den vereinheitlichten Kanaltyp `AnyAdcChannel`.
   * Durch `use embassy_stm32::adc::AdcChannel as _;` erhält man die Methode `.degrade_adc()`. Man wandelt den Pin vor der Schleife mit `let mut channel = pin.degrade_adc();` um und übergibt in der Sequenz `(&mut channel, SampleTime::CYCLES47_5)`.

4. **DMA-Kanal Übergabe mit `.reborrow()`** (Fehler `E0308` an Line 78):
   * `adc.read()` erwartet ein `Peri<'_, DMA1_CH1>`. Da der DMA-Kanal in einer Endlosschleife wiederverwendet wird, übergibt man ihn mit `dma_ch.reborrow()`.

5. **Geänderte Task-Spawn-Syntax in `embassy-executor 0.10`** (Fehler `E0308` und `E0599`):
   * In Embassy-Executor 0.10 liefert der Aufruf der Task-Funktion selbst ein `Result<SpawnToken, SpawnError>`.
   * `spawner.spawn(...)` nimmt direkt das `SpawnToken` entgegen und liefert `()` zurück.
   * **Falsch:** `spawner.spawn(task(...)).unwrap();`
   * **Richtig:** `spawner.spawn(task(...).unwrap());`

6. **Wichtiger Laufzeit-Hinweis (STM32G4 ADC Clock):**
   * Im alten Code (`main.rs_old`) war `config.rcc.mux.adc12sel = embassy_stm32::rcc::mux::Adcsel::SYS;` gesetzt. Im neuen `main.rs` wurde nur `Config::default()` übergeben. Auf dem STM32G4 führt das zwingend zu einem **Laufzeit-Panic** in `Adc::new`, weil der ADC-Takt nicht konfiguriert ist. Diese Zeile muss wieder in `main()` ergänzt werden.

---

### Die korrigierte `src/main.rs`

Ersetze den Inhalt von `src/main.rs` vollständig durch folgenden Code:

```rust
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
```

Ein Blick in dein größeres, funktionierendes Projekt **`fw`** bestätigt genau die Punkte, die in **`fw2`** zu den Compiler-Fehlern geführt haben. 

Hier ist der direkte Vergleich der beiden Projekte – und warum `fw` sauber durch den Compiler läuft:

---

### 1. Task-Spawning: Die Klammerung von `.unwrap()`
* **In `fw2` (Fehler):**
  ```rust
  spawner.spawn(dac_task(p.DAC1, p.PA4)).unwrap();
  //                                    ^^^^^^^^ FEHLER: unwrap() auf ()
  ```
* **In `fw` (`main.rs`, Zeilen 98–105) (Korrekt):**
  ```rust
  spawner.spawn(scope_07::scope_task(p.ADC1, p.PA0, &scope_07::SCOPE_REQ, &scope_07::SCOPE_RESP).unwrap());
  //                                                                                              ^^^^^^^^
  ```
  In `fw` steht das `.unwrap()` **innerhalb** der `spawner.spawn(...)`-Klammer, weil der Task-Aufruf ein `Result<SpawnToken, SpawnError>` zurückgibt, `spawn` aber direkt das `SpawnToken` erwartet.

---

### 2. Parametertypen: `Peri<'static, ...>` statt `peripherals::...`
* **In `fw2` (Fehler):**
  ```rust
  async fn adc_task(adc_peri: peripherals::ADC1, mut pin: peripherals::PA0, ...)
  ```
  Führte zu Typkonflikten (`expected Peri, found ADC1`), da `p.ADC1` in Embassy 0.6 den Typ `Peri<'static, ADC1>` hat.
* **In `fw` (`06_awg.rs` & `07_scope.rs`) (Korrekt):**
  ```rust
  // in 06_awg.rs:
  pub async fn awg_task(
      tim: Peri<'static, peripherals::TIM2>,
      pin: Peri<'static, peripherals::PA5>,
      ...
  )

  // in 07_scope.rs:
  pub async fn scope_task(
      adc_peri: Peri<'static, peripherals::ADC1>,
      pin: Peri<'static, peripherals::PA0>,
      ...
  )
  ```
  In `fw` werden die Peripherie-Typen überall mit `Peri<'static, ...>` deklariert.

---

### 3. Warum `fw` keinen `adc::InterruptHandler` braucht
* **In `fw2` (Fehler):**
  ```rust
  bind_interrupts!(struct Irqs {
      ADC1_2 => adc::InterruptHandler<peripherals::ADC1>; // Existiert nicht in embassy-stm32
  });
  ```
* **In `fw` (`main.rs` & `07_scope.rs`):**
  In `fw` gibt es gar keinen ADC-Interrupt in `bind_interrupts!`. In `07_scope.rs` liest das Projekt den ADC blockierend aus:
  ```rust
  let mut adc_pin = pin.degrade_adc();
  ...
  snap.buf[i] = adc.blocking_read(&mut adc_pin, SampleTime::CYCLES47_5);
  ```
  *(Hinweis im Code von `07_scope.rs`: „Stage 1 polls blocking_read (no DMA/interrupt dependency)“)*.

  Wenn du in `fw2` asynchron über DMA lesen willst (`adc.read`), brauchst du dafür ausschließlich den DMA-Interrupt-Handler (`DMA1_CHANNEL1 => dma::InterruptHandler<peripherals::DMA1_CH1>;`), aber **keinen** ADC-Interrupt.

---

### 4. `pin.degrade_adc()`
* **In `fw2` (Fehler):**
  In `fw2` wurde `&mut pin` direkt an die Sequenz übergeben, was zu `expected AnyAdcChannel, found PA0` führte.
* **In `fw` (`07_scope.rs`, Zeile 88) (Korrekt):**
  ```rust
  let mut adc_pin = pin.degrade_adc();
  ```
  Auch `fw` wandelt den Pin zuerst über `degrade_adc()` in den generischen Typ `AnyAdcChannel` um.

---

### 5. ADC Clock Multiplexer (`adc12sel`)
* **In `fw` (`01_clock.rs`, Zeilen 13–15):**
  ```rust
  // ADC12 needs an explicit kernel clock (Adc::new panics without one);
  // sysclk (HSI 16 MHz) keeps the ADC in spec without a PLL.
  config.rcc.mux.adc12sel = mux::Adcsel::SYS;
  ```
  Hier ist in `fw` genau der Kommentar hinterlegt: Ohne diese Zeile stürzt `Adc::new` zur Laufzeit mit einem Panic ab.

---

### 6. Unterschiede in `Cargo.toml`
* **`features = [..., "exti"]`:** `fw` hat das Feature `"exti"` in `embassy-stm32` aktiviert. Das wird für `comp.wait_for_rising_edge()` in `05_mode_freq.rs` benötigt.
* **Edition:** `fw` nutzt `edition = "2021"`, `fw2` nutzt `edition = "2024"`. Für die Embassy-APIs macht das keinen Unterschied, aber die API-Typen (`Peri`, `SpawnToken`, `degrade_adc`) müssen exakt wie in `fw` gehandhabt werden.
