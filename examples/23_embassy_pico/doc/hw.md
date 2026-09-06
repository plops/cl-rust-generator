what are the peripherals of the pi pico and pi pico 2. which one of these can i control with rust embassy? is there anything analog? if not is it possible to measure the time to discharge pins (capacitive measurement)?

Peripherals Comparison
The Raspberry Pi Pico
(powered by the RP2040 chip) and the Raspberry Pi Pico 2
(powered by the RP2350 chip) share a highly compatible footprint but feature notable differences in their hardware capabilities: 
Peripheral	Raspberry Pi Pico (RP2040)
	Raspberry Pi Pico 2 (RP2350)
CPU Cores	2 × ARM Cortex-M0+ @ 133MHz	2 × ARM Cortex-M33 OR 2 × RISC-V Hazard3 @ 150MHz
SRAM	264 KB	520 KB
UART	2 × Controllers	2 × Controllers
SPI	2 × Controllers	2 × Controllers
I2C	2 × Controllers	2 × Controllers
PWM	16 Channels (8 blocks)	24 Channels (12 blocks)
PIO (Programmable IO)	2 blocks (8 state machines total)	3 blocks (12 state machines total)
HSTX (High-Speed TX)	❌ None	1 × Output-only controller (for digital video/VGA)
ADC (Analog-to-Digital)	4-channel, 12-bit 500ksps	4-channel, 12-bit 500ksps
Hardware Security	❌ None	ARM TrustZone, OTP memory, SHA-256, Secure Boot
Control via Rust Embassy
Yes, you can control both microcontrollers using Rust Embassy.
The embassy-rp Hardware Abstraction Layer (HAL) provides full, production-ready support for the RP2040
and extensive support for the newer RP2350
. 
Through embassy-rp, you can control almost all core peripherals asynchronously or synchronously, including:

    GPIO / PWM (Asynchronous pin reading, writing, and hardware PWM dimming).
    UART, SPI, and I2C (Using Rust futures to safely await bus transfers without wasting CPU cycles).
    PIO Blocks (Loading custom state-machine bytecode via async drivers).
    ADC (Reading analog voltages asynchronously). 

Is there anything Analog?
Yes, both boards have an Analog-to-Digital Converter (ADC). 

    They feature a 12-bit, 500 ksps Successive Approximation Register (SAR) ADC.
    The Pi Pico
    and Pico 2
    route 3 external ADC pins to the board headers (GPIO 26, 27, and 28).
    A 4th internal channel is bound to a built-in temperature sensor on both chips.
    (Note: Neither board features a native Digital-to-Analog Converter (DAC). For analog output, you must emulate it via smoothed PWM or an external DAC over I2C/SPI). 

Measuring Pin Discharge Time (Capacitive Sensing)
Yes, it is entirely possible to perform capacitive touch or distance measurements using digital pins, even without using the ADC. This technique involves timing a Resistor-Capacitor (RC) network discharge cycle.
How to implement it in Rust Embassy:

    Charge the Capacitor: Configure a GPIO pin as a digital output (FlexPin or Output in Embassy) and drive it High to fully charge your capacitive surface.
    Switch to Input: Instantly reconfigure the pin as an input (Input) with no internal pull-up or pull-down resistors.
    Time the Discharge: Start a high-resolution timer (the RP2040
    /RP2350
    hardware timer increments precisely every 1 microsecond). Wait in a tight loop until the pin transitions to Low.
    Calculate: Read the timer again. The microsecond delta directly reflects the capacitance of the pin/sensor. 

The Pro-Tip: Use PIO for ultra-high precision
While doing this in Rust code works, CPU execution fluctuations can introduce jitter. Because both Pico boards feature PIO (Programmable IO) blocks, you can write a tiny 3-line assembly block to handle this deterministically: 

    The PIO can switch the pin to input and decrement a counter register every single clock cycle (running at 133MHz or 150MHz) until the pin reads 0.
    This provides nanosecond-level precision instead of microsecond-level precision, yielding incredibly stable capacitive readings.
